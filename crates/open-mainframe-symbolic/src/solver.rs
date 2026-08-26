//! Pure-Rust constraint solver for symbolic execution.
//!
//! [`SymbolicSolver`] combines direct bound propagation with a bounded,
//! deterministic model search. It proves contradictions for supported
//! integer and Boolean constraints, produces concrete assignments when it can,
//! and returns [`CheckResult::Unknown`] for expressions outside that subset.

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::time::{Duration, Instant};

use crate::path::PathCondition;
use crate::sort::Sort;
use crate::value::{ExprOp, SymbolicValue};

const DEFAULT_TIMEOUT_MS: u64 = 10_000;
const MAX_MODEL_CANDIDATES: usize = 100_000;
const MAX_VALUES_PER_INTEGER: usize = 32;

/// Result of a satisfiability check without model extraction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SatResult {
    /// A concrete satisfying assignment was found.
    Sat,
    /// The supported constraint subset proves that no assignment exists.
    Unsat,
    /// The bounded solver could neither find a model nor prove a contradiction.
    Unknown,
}

/// Result of a satisfiability check, enriched with concrete assignments.
#[derive(Debug)]
pub enum CheckResult {
    /// The constraints are satisfiable. `assignments` maps symbolic variable
    /// names to concrete values found by the solver.
    Sat {
        assignments: HashMap<String, SymbolicValue>,
    },
    /// The constraints are unsatisfiable within the supported theory.
    Unsat,
    /// The expression is unsupported, or the bounded search was inconclusive.
    Unknown,
}

/// A concrete model returned for a satisfiable path condition.
#[derive(Debug, Clone)]
pub struct SolverModel {
    assignments: HashMap<String, SymbolicValue>,
}

impl SolverModel {
    /// Borrow the concrete assignments in this model.
    pub fn assignments(&self) -> &HashMap<String, SymbolicValue> {
        &self.assignments
    }

    /// Consume the model and return its assignments.
    pub fn into_assignments(self) -> HashMap<String, SymbolicValue> {
        self.assignments
    }
}

/// Lightweight, dependency-free solver for the symbolic domain.
pub struct SymbolicSolver {
    timeout: Duration,
    max_candidates: usize,
}

impl SymbolicSolver {
    /// Create a solver with the default per-query timeout.
    pub fn new() -> Self {
        Self::with_timeout(DEFAULT_TIMEOUT_MS)
    }

    /// Create a solver with a custom timeout in milliseconds.
    pub fn with_timeout(timeout_ms: u64) -> Self {
        Self {
            timeout: Duration::from_millis(timeout_ms),
            max_candidates: MAX_MODEL_CANDIDATES,
        }
    }

    /// Check satisfiability of a path condition.
    pub fn check_sat(&self, path: &PathCondition) -> SatResult {
        match self.check(path) {
            CheckResult::Sat { .. } => SatResult::Sat,
            CheckResult::Unsat => SatResult::Unsat,
            CheckResult::Unknown => SatResult::Unknown,
        }
    }

    /// Check satisfiability and return concrete assignments when available.
    pub fn check(&self, path: &PathCondition) -> CheckResult {
        let conditions: Vec<&SymbolicValue> = path
            .constraints()
            .iter()
            .map(|constraint| &constraint.condition)
            .collect();
        self.solve(&conditions)
    }

    /// Return a concrete model for a satisfiable path condition.
    pub fn get_model(&self, path: &PathCondition) -> Option<SolverModel> {
        match self.check(path) {
            CheckResult::Sat { assignments } => Some(SolverModel { assignments }),
            CheckResult::Unsat | CheckResult::Unknown => None,
        }
    }

    /// Check whether adding `extra` could still be satisfiable.
    ///
    /// An inconclusive result is treated as feasible so exploration does not
    /// silently discard a potentially valid program path.
    pub fn is_feasible(&self, path: &PathCondition, extra: &SymbolicValue) -> bool {
        let mut conditions: Vec<&SymbolicValue> = path
            .constraints()
            .iter()
            .map(|constraint| &constraint.condition)
            .collect();
        conditions.push(extra);
        !matches!(self.solve(&conditions), CheckResult::Unsat)
    }

    fn solve(&self, conditions: &[&SymbolicValue]) -> CheckResult {
        let mut variables = BTreeMap::new();
        let mut conflicting_sorts = false;
        let mut constants = BTreeSet::new();

        for condition in conditions {
            collect_symbolic_names(condition, &mut variables, &mut conflicting_sorts);
            collect_integer_constants(condition, &mut constants);
        }

        if conflicting_sorts {
            return CheckResult::Unknown;
        }

        let mut domains = initialize_domains(&variables);
        if domains.values().any(VariableDomain::is_unsupported) {
            return CheckResult::Unknown;
        }

        let mut has_unsupported_constraint = false;
        for condition in conditions {
            match apply_required_condition(condition, true, &mut domains) {
                ConstraintEffect::Contradiction => return CheckResult::Unsat,
                ConstraintEffect::Unsupported => has_unsupported_constraint = true,
                ConstraintEffect::Applied => {}
            }
        }

        if domains.values().any(VariableDomain::is_impossible) {
            return CheckResult::Unsat;
        }

        let empty_assignment = HashMap::new();
        for condition in conditions {
            if matches!(evaluate_bool(condition, &empty_assignment), Some(false)) {
                return CheckResult::Unsat;
            }
        }

        let candidates = match build_candidates(&variables, &domains, &constants) {
            Some(candidates) => candidates,
            None => return CheckResult::Unsat,
        };

        let complete_search = !has_unsupported_constraint
            && variables.keys().all(|name| {
                domains
                    .get(name)
                    .is_some_and(VariableDomain::has_finite_complete_domain)
            });
        let started = Instant::now();
        let mut checked = 0usize;
        let mut assignments = HashMap::new();
        let search = search_assignments(
            0,
            &candidates,
            conditions,
            &mut assignments,
            &mut checked,
            self.max_candidates,
            started,
            self.timeout,
        );

        match search {
            SearchResult::Found(assignments) => CheckResult::Sat { assignments },
            SearchResult::Exhausted if complete_search => CheckResult::Unsat,
            SearchResult::Exhausted | SearchResult::Limited => CheckResult::Unknown,
        }
    }
}

impl Default for SymbolicSolver {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
enum ConcreteValue {
    Int(i64),
    Bool(bool),
    Str(String),
}

impl ConcreteValue {
    fn as_bool(&self) -> Option<bool> {
        match self {
            ConcreteValue::Bool(value) => Some(*value),
            ConcreteValue::Int(value) => Some(*value != 0),
            ConcreteValue::Str(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Comparison {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Comparison {
    fn from_expr(op: &ExprOp) -> Option<Self> {
        match op {
            ExprOp::Eq => Some(Self::Eq),
            ExprOp::Ne => Some(Self::Ne),
            ExprOp::Lt => Some(Self::Lt),
            ExprOp::Le => Some(Self::Le),
            ExprOp::Gt => Some(Self::Gt),
            ExprOp::Ge => Some(Self::Ge),
            _ => None,
        }
    }

    fn negate(self) -> Self {
        match self {
            Self::Eq => Self::Ne,
            Self::Ne => Self::Eq,
            Self::Lt => Self::Ge,
            Self::Le => Self::Gt,
            Self::Gt => Self::Le,
            Self::Ge => Self::Lt,
        }
    }

    fn reverse(self) -> Self {
        match self {
            Self::Eq => Self::Eq,
            Self::Ne => Self::Ne,
            Self::Lt => Self::Gt,
            Self::Le => Self::Ge,
            Self::Gt => Self::Lt,
            Self::Ge => Self::Le,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct IntDomain {
    min: Option<i64>,
    max: Option<i64>,
    equal: Option<i64>,
    excluded: BTreeSet<i64>,
    impossible: bool,
}

impl IntDomain {
    fn signed_bit_vector(width: u32) -> Self {
        if width == 0 {
            Self {
                impossible: true,
                ..Self::default()
            }
        } else if width >= 64 {
            Self::default()
        } else {
            let magnitude = 1_i64 << (width - 1);
            Self {
                min: Some(-magnitude),
                max: Some(magnitude - 1),
                ..Self::default()
            }
        }
    }

    fn apply(&mut self, comparison: Comparison, constant: i64) -> ConstraintEffect {
        match comparison {
            Comparison::Eq => {
                if self.equal.is_some_and(|current| current != constant) {
                    self.impossible = true;
                }
                self.equal = Some(constant);
            }
            Comparison::Ne => {
                self.excluded.insert(constant);
            }
            Comparison::Lt => match constant.checked_sub(1) {
                Some(maximum) => self.set_max(maximum),
                None => self.impossible = true,
            },
            Comparison::Le => self.set_max(constant),
            Comparison::Gt => match constant.checked_add(1) {
                Some(minimum) => self.set_min(minimum),
                None => self.impossible = true,
            },
            Comparison::Ge => self.set_min(constant),
        }
        self.validate();
        if self.impossible {
            ConstraintEffect::Contradiction
        } else {
            ConstraintEffect::Applied
        }
    }

    fn set_min(&mut self, minimum: i64) {
        self.min = Some(self.min.map_or(minimum, |current| current.max(minimum)));
    }

    fn set_max(&mut self, maximum: i64) {
        self.max = Some(self.max.map_or(maximum, |current| current.min(maximum)));
    }

    fn validate(&mut self) {
        if matches!((self.min, self.max), (Some(minimum), Some(maximum)) if minimum > maximum) {
            self.impossible = true;
        }
        if let Some(value) = self.equal {
            if !self.allows(value) {
                self.impossible = true;
            }
        }
        if let (Some(minimum), Some(maximum)) = (self.min, self.max) {
            if minimum == maximum && self.excluded.contains(&minimum) {
                self.impossible = true;
            }
        }
    }

    fn allows(&self, value: i64) -> bool {
        !self.impossible
            && self.min.is_none_or(|minimum| value >= minimum)
            && self.max.is_none_or(|maximum| value <= maximum)
            && self.equal.is_none_or(|equal| value == equal)
            && !self.excluded.contains(&value)
    }

    fn has_finite_complete_domain(&self) -> bool {
        self.equal.is_some() || matches!((self.min, self.max), (Some(a), Some(b)) if a == b)
    }

    fn seed_values(&self) -> Vec<i64> {
        if let Some(equal) = self.equal {
            return vec![equal];
        }
        let mut values = Vec::new();
        if let Some(minimum) = self.min {
            values.push(minimum);
            if let Some(next) = minimum.checked_add(1) {
                values.push(next);
            }
        }
        if let Some(maximum) = self.max {
            values.push(maximum);
            if let Some(previous) = maximum.checked_sub(1) {
                values.push(previous);
            }
        }
        values.extend([0, 1, -1]);
        values
    }
}

#[derive(Debug, Clone, Copy)]
struct BoolDomain {
    allow_false: bool,
    allow_true: bool,
}

impl Default for BoolDomain {
    fn default() -> Self {
        Self {
            allow_false: true,
            allow_true: true,
        }
    }
}

impl BoolDomain {
    fn require(&mut self, value: bool) -> ConstraintEffect {
        if value {
            self.allow_false = false;
        } else {
            self.allow_true = false;
        }
        if self.allow_false || self.allow_true {
            ConstraintEffect::Applied
        } else {
            ConstraintEffect::Contradiction
        }
    }
}

#[derive(Debug, Clone)]
enum VariableDomain {
    Int(IntDomain),
    Bool(BoolDomain),
    Unsupported,
}

impl VariableDomain {
    fn is_unsupported(&self) -> bool {
        matches!(self, Self::Unsupported)
    }

    fn is_impossible(&self) -> bool {
        match self {
            Self::Int(domain) => domain.impossible,
            Self::Bool(domain) => !domain.allow_false && !domain.allow_true,
            Self::Unsupported => false,
        }
    }

    fn has_finite_complete_domain(&self) -> bool {
        match self {
            Self::Int(domain) => domain.has_finite_complete_domain(),
            Self::Bool(_) => true,
            Self::Unsupported => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConstraintEffect {
    Applied,
    Unsupported,
    Contradiction,
}

#[derive(Debug)]
enum SearchResult {
    Found(HashMap<String, SymbolicValue>),
    Exhausted,
    Limited,
}

fn initialize_domains(variables: &BTreeMap<String, Sort>) -> HashMap<String, VariableDomain> {
    variables
        .iter()
        .map(|(name, sort)| {
            let domain = match sort {
                Sort::Int => VariableDomain::Int(IntDomain::default()),
                Sort::Bool => VariableDomain::Bool(BoolDomain::default()),
                Sort::BitVec(width) => VariableDomain::Int(IntDomain::signed_bit_vector(*width)),
                Sort::Array { .. } => VariableDomain::Unsupported,
            };
            (name.clone(), domain)
        })
        .collect()
}

fn apply_required_condition(
    value: &SymbolicValue,
    required: bool,
    domains: &mut HashMap<String, VariableDomain>,
) -> ConstraintEffect {
    match value {
        SymbolicValue::ConcreteBool(actual) => {
            if *actual == required {
                ConstraintEffect::Applied
            } else {
                ConstraintEffect::Contradiction
            }
        }
        SymbolicValue::Concrete(actual) => {
            if (*actual != 0) == required {
                ConstraintEffect::Applied
            } else {
                ConstraintEffect::Contradiction
            }
        }
        SymbolicValue::ConcreteStr(_) | SymbolicValue::Unknown => ConstraintEffect::Unsupported,
        SymbolicValue::Symbolic { name, sort } => match (sort, domains.get_mut(name)) {
            (Sort::Bool, Some(VariableDomain::Bool(domain))) => domain.require(required),
            (Sort::Int | Sort::BitVec(_), Some(VariableDomain::Int(domain))) => domain.apply(
                if required {
                    Comparison::Ne
                } else {
                    Comparison::Eq
                },
                0,
            ),
            _ => ConstraintEffect::Unsupported,
        },
        SymbolicValue::Expression { op, args } => match op {
            ExprOp::Not if args.len() == 1 => {
                apply_required_condition(&args[0], !required, domains)
            }
            ExprOp::And if required && args.len() == 2 => combine_effects(
                apply_required_condition(&args[0], true, domains),
                apply_required_condition(&args[1], true, domains),
            ),
            ExprOp::Or if !required && args.len() == 2 => combine_effects(
                apply_required_condition(&args[0], false, domains),
                apply_required_condition(&args[1], false, domains),
            ),
            _ => Comparison::from_expr(op).map_or(ConstraintEffect::Unsupported, |comparison| {
                if args.len() != 2 {
                    return ConstraintEffect::Unsupported;
                }
                apply_comparison(
                    if required {
                        comparison
                    } else {
                        comparison.negate()
                    },
                    &args[0],
                    &args[1],
                    domains,
                )
            }),
        },
    }
}

fn combine_effects(left: ConstraintEffect, right: ConstraintEffect) -> ConstraintEffect {
    match (left, right) {
        (ConstraintEffect::Contradiction, _) | (_, ConstraintEffect::Contradiction) => {
            ConstraintEffect::Contradiction
        }
        (ConstraintEffect::Applied, ConstraintEffect::Applied) => ConstraintEffect::Applied,
        _ => ConstraintEffect::Unsupported,
    }
}

fn apply_comparison(
    comparison: Comparison,
    left: &SymbolicValue,
    right: &SymbolicValue,
    domains: &mut HashMap<String, VariableDomain>,
) -> ConstraintEffect {
    if left == right {
        return match comparison {
            Comparison::Eq | Comparison::Le | Comparison::Ge => ConstraintEffect::Applied,
            Comparison::Ne | Comparison::Lt | Comparison::Gt => ConstraintEffect::Contradiction,
        };
    }

    if let Some((name, constant)) = symbolic_integer_and_constant(left, right) {
        return match domains.get_mut(name) {
            Some(VariableDomain::Int(domain)) => domain.apply(comparison, constant),
            _ => ConstraintEffect::Unsupported,
        };
    }
    if let Some((name, constant)) = symbolic_integer_and_constant(right, left) {
        return match domains.get_mut(name) {
            Some(VariableDomain::Int(domain)) => domain.apply(comparison.reverse(), constant),
            _ => ConstraintEffect::Unsupported,
        };
    }

    if let Some((name, constant)) = symbolic_bool_and_constant(left, right) {
        return apply_bool_comparison(name, constant, comparison, domains);
    }
    if let Some((name, constant)) = symbolic_bool_and_constant(right, left) {
        return apply_bool_comparison(name, constant, comparison.reverse(), domains);
    }

    ConstraintEffect::Unsupported
}

fn symbolic_integer_and_constant<'a>(
    symbolic: &'a SymbolicValue,
    constant: &SymbolicValue,
) -> Option<(&'a str, i64)> {
    match (symbolic, constant) {
        (
            SymbolicValue::Symbolic {
                name,
                sort: Sort::Int | Sort::BitVec(_),
            },
            SymbolicValue::Concrete(value),
        ) => Some((name, *value)),
        _ => None,
    }
}

fn symbolic_bool_and_constant<'a>(
    symbolic: &'a SymbolicValue,
    constant: &SymbolicValue,
) -> Option<(&'a str, bool)> {
    match (symbolic, constant) {
        (
            SymbolicValue::Symbolic {
                name,
                sort: Sort::Bool,
            },
            SymbolicValue::ConcreteBool(value),
        ) => Some((name, *value)),
        _ => None,
    }
}

fn apply_bool_comparison(
    name: &str,
    constant: bool,
    comparison: Comparison,
    domains: &mut HashMap<String, VariableDomain>,
) -> ConstraintEffect {
    let required = match comparison {
        Comparison::Eq => constant,
        Comparison::Ne => !constant,
        Comparison::Lt | Comparison::Le | Comparison::Gt | Comparison::Ge => {
            return ConstraintEffect::Unsupported;
        }
    };
    match domains.get_mut(name) {
        Some(VariableDomain::Bool(domain)) => domain.require(required),
        _ => ConstraintEffect::Unsupported,
    }
}

fn build_candidates(
    variables: &BTreeMap<String, Sort>,
    domains: &HashMap<String, VariableDomain>,
    constants: &BTreeSet<i64>,
) -> Option<Vec<(String, Vec<SymbolicValue>)>> {
    let mut result = Vec::new();
    for name in variables.keys() {
        let values = match domains.get(name)? {
            VariableDomain::Bool(domain) => {
                let mut values = Vec::new();
                if domain.allow_false {
                    values.push(SymbolicValue::ConcreteBool(false));
                }
                if domain.allow_true {
                    values.push(SymbolicValue::ConcreteBool(true));
                }
                values
            }
            VariableDomain::Int(domain) => {
                let mut raw: BTreeSet<i64> = domain
                    .seed_values()
                    .into_iter()
                    .filter(|value| domain.allows(*value))
                    .collect();
                for constant in constants {
                    for candidate in [
                        Some(*constant),
                        constant.checked_sub(1),
                        constant.checked_add(1),
                    ]
                    .into_iter()
                    .flatten()
                    {
                        if domain.allows(candidate) {
                            raw.insert(candidate);
                        }
                    }
                }
                raw.into_iter()
                    .take(MAX_VALUES_PER_INTEGER)
                    .map(SymbolicValue::Concrete)
                    .collect()
            }
            VariableDomain::Unsupported => return None,
        };
        if values.is_empty() {
            return None;
        }
        result.push((name.clone(), values));
    }
    Some(result)
}

#[allow(clippy::too_many_arguments)]
fn search_assignments(
    index: usize,
    candidates: &[(String, Vec<SymbolicValue>)],
    conditions: &[&SymbolicValue],
    assignments: &mut HashMap<String, SymbolicValue>,
    checked: &mut usize,
    max_candidates: usize,
    started: Instant,
    timeout: Duration,
) -> SearchResult {
    if *checked >= max_candidates || started.elapsed() >= timeout {
        return SearchResult::Limited;
    }

    if index == candidates.len() {
        *checked += 1;
        if conditions
            .iter()
            .all(|condition| matches!(evaluate_bool(condition, assignments), Some(true)))
        {
            return SearchResult::Found(assignments.clone());
        }
        return SearchResult::Exhausted;
    }

    let (name, values) = &candidates[index];
    for value in values {
        assignments.insert(name.clone(), value.clone());
        match search_assignments(
            index + 1,
            candidates,
            conditions,
            assignments,
            checked,
            max_candidates,
            started,
            timeout,
        ) {
            SearchResult::Exhausted => {}
            result @ (SearchResult::Found(_) | SearchResult::Limited) => return result,
        }
    }
    assignments.remove(name);
    SearchResult::Exhausted
}

fn evaluate_bool(
    value: &SymbolicValue,
    assignments: &HashMap<String, SymbolicValue>,
) -> Option<bool> {
    evaluate_value(value, assignments)?.as_bool()
}

fn evaluate_value(
    value: &SymbolicValue,
    assignments: &HashMap<String, SymbolicValue>,
) -> Option<ConcreteValue> {
    match value {
        SymbolicValue::Concrete(value) => Some(ConcreteValue::Int(*value)),
        SymbolicValue::ConcreteBool(value) => Some(ConcreteValue::Bool(*value)),
        SymbolicValue::ConcreteStr(value) => Some(ConcreteValue::Str(value.clone())),
        SymbolicValue::Unknown => None,
        SymbolicValue::Symbolic { name, .. } => evaluate_value(assignments.get(name)?, assignments),
        SymbolicValue::Expression { op, args } => evaluate_expression(op, args, assignments),
    }
}

fn evaluate_expression(
    op: &ExprOp,
    args: &[SymbolicValue],
    assignments: &HashMap<String, SymbolicValue>,
) -> Option<ConcreteValue> {
    match op {
        ExprOp::Add | ExprOp::Sub | ExprOp::Mul | ExprOp::Div | ExprOp::Mod => {
            let [left, right] = args else {
                return None;
            };
            let (ConcreteValue::Int(left), ConcreteValue::Int(right)) = (
                evaluate_value(left, assignments)?,
                evaluate_value(right, assignments)?,
            ) else {
                return None;
            };
            let value = match op {
                ExprOp::Add => left.wrapping_add(right),
                ExprOp::Sub => left.wrapping_sub(right),
                ExprOp::Mul => left.wrapping_mul(right),
                ExprOp::Div => left.checked_div(right)?,
                ExprOp::Mod => left.checked_rem(right)?,
                _ => unreachable!(),
            };
            Some(ConcreteValue::Int(value))
        }
        ExprOp::Neg => {
            let [inner] = args else {
                return None;
            };
            let ConcreteValue::Int(value) = evaluate_value(inner, assignments)? else {
                return None;
            };
            Some(ConcreteValue::Int(value.wrapping_neg()))
        }
        ExprOp::Eq | ExprOp::Ne | ExprOp::Lt | ExprOp::Le | ExprOp::Gt | ExprOp::Ge => {
            let [left, right] = args else {
                return None;
            };
            evaluate_comparison(op, left, right, assignments).map(ConcreteValue::Bool)
        }
        ExprOp::And | ExprOp::Or => {
            let [left, right] = args else {
                return None;
            };
            let left = evaluate_bool(left, assignments)?;
            let right = evaluate_bool(right, assignments)?;
            Some(ConcreteValue::Bool(match op {
                ExprOp::And => left && right,
                ExprOp::Or => left || right,
                _ => unreachable!(),
            }))
        }
        ExprOp::Not => {
            let [inner] = args else {
                return None;
            };
            Some(ConcreteValue::Bool(!evaluate_bool(inner, assignments)?))
        }
        ExprOp::Concat => {
            let [left, right] = args else {
                return None;
            };
            let (ConcreteValue::Str(left), ConcreteValue::Str(right)) = (
                evaluate_value(left, assignments)?,
                evaluate_value(right, assignments)?,
            ) else {
                return None;
            };
            Some(ConcreteValue::Str(format!("{left}{right}")))
        }
        ExprOp::Substring | ExprOp::Select | ExprOp::Store => None,
    }
}

fn evaluate_comparison(
    op: &ExprOp,
    left: &SymbolicValue,
    right: &SymbolicValue,
    assignments: &HashMap<String, SymbolicValue>,
) -> Option<bool> {
    if left == right {
        return match op {
            ExprOp::Eq | ExprOp::Le | ExprOp::Ge => Some(true),
            ExprOp::Ne | ExprOp::Lt | ExprOp::Gt => Some(false),
            _ => None,
        };
    }
    let left = evaluate_value(left, assignments)?;
    let right = evaluate_value(right, assignments)?;
    match (left, right) {
        (ConcreteValue::Int(left), ConcreteValue::Int(right)) => Some(match op {
            ExprOp::Eq => left == right,
            ExprOp::Ne => left != right,
            ExprOp::Lt => left < right,
            ExprOp::Le => left <= right,
            ExprOp::Gt => left > right,
            ExprOp::Ge => left >= right,
            _ => return None,
        }),
        (ConcreteValue::Bool(left), ConcreteValue::Bool(right)) => match op {
            ExprOp::Eq => Some(left == right),
            ExprOp::Ne => Some(left != right),
            _ => None,
        },
        (ConcreteValue::Str(left), ConcreteValue::Str(right)) => match op {
            ExprOp::Eq => Some(left == right),
            ExprOp::Ne => Some(left != right),
            _ => None,
        },
        _ => None,
    }
}

fn collect_symbolic_names(
    value: &SymbolicValue,
    out: &mut BTreeMap<String, Sort>,
    conflicting_sorts: &mut bool,
) {
    match value {
        SymbolicValue::Symbolic { name, sort } => {
            if out.get(name).is_some_and(|existing| existing != sort) {
                *conflicting_sorts = true;
            } else {
                out.entry(name.clone()).or_insert_with(|| sort.clone());
            }
        }
        SymbolicValue::Expression { args, .. } => {
            for arg in args {
                collect_symbolic_names(arg, out, conflicting_sorts);
            }
        }
        _ => {}
    }
}

fn collect_integer_constants(value: &SymbolicValue, out: &mut BTreeSet<i64>) {
    match value {
        SymbolicValue::Concrete(value) => {
            out.insert(*value);
        }
        SymbolicValue::Expression { args, .. } => {
            for arg in args {
                collect_integer_constants(arg, out);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::Constraint;
    use open_mainframe_lang_core::Span;

    fn dummy_span() -> Span {
        Span::default()
    }

    fn add(path: &mut PathCondition, condition: SymbolicValue) {
        path.add(Constraint {
            condition,
            source: dummy_span(),
            description: None,
        });
    }

    #[test]
    fn trivially_sat() {
        let solver = SymbolicSolver::new();
        assert_eq!(solver.check_sat(&PathCondition::new()), SatResult::Sat);
    }

    #[test]
    fn trivially_unsat() {
        let solver = SymbolicSolver::new();
        let mut path = PathCondition::new();
        let x = SymbolicValue::sym_int("X");
        add(&mut path, x.clone().gt(SymbolicValue::int(0)));
        add(&mut path, x.lt(SymbolicValue::int(0)));
        assert_eq!(solver.check_sat(&path), SatResult::Unsat);
    }

    #[test]
    fn sat_with_model() {
        let solver = SymbolicSolver::new();
        let mut path = PathCondition::new();
        add(
            &mut path,
            SymbolicValue::sym_int("X").gt(SymbolicValue::int(5)),
        );

        match solver.check(&path) {
            CheckResult::Sat { assignments } => {
                let value = assignments["X"].as_concrete_int().unwrap();
                assert!(value > 5);
            }
            other => panic!("Expected Sat, got {other:?}"),
        }
    }

    #[test]
    fn feasibility_check() {
        let solver = SymbolicSolver::new();
        let mut path = PathCondition::new();
        let x = SymbolicValue::sym_int("X");
        add(&mut path, x.clone().gt(SymbolicValue::int(10)));

        assert!(solver.is_feasible(&path, &x.clone().gt(SymbolicValue::int(5))));
        assert!(!solver.is_feasible(&path, &x.lt(SymbolicValue::int(0))));
    }

    #[test]
    fn boolean_constraint() {
        let solver = SymbolicSolver::new();
        let mut path = PathCondition::new();
        add(&mut path, SymbolicValue::sym_bool("FLAG"));

        match solver.check(&path) {
            CheckResult::Sat { assignments } => {
                assert_eq!(assignments["FLAG"], SymbolicValue::ConcreteBool(true));
            }
            other => panic!("Expected Sat, got {other:?}"),
        }
    }

    #[test]
    fn boolean_equality_uses_boolean_domain() {
        let solver = SymbolicSolver::new();
        let mut path = PathCondition::new();
        add(
            &mut path,
            SymbolicValue::sym_bool("FLAG").eq(SymbolicValue::bool(false)),
        );

        match solver.check(&path) {
            CheckResult::Sat { assignments } => {
                assert_eq!(assignments["FLAG"], SymbolicValue::ConcreteBool(false));
            }
            other => panic!("Expected Sat, got {other:?}"),
        }
    }

    #[test]
    fn unknown_is_conservatively_feasible() {
        let solver = SymbolicSolver::new();
        let path = PathCondition::new();
        assert!(solver.is_feasible(&path, &SymbolicValue::Unknown));
    }
}
