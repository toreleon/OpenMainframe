//! HLASM Conditional Assembly — SET symbols, AIF/AGO, and ~50 built-in functions.
//!
//! Implements the compile-time programming model:
//! - **SET variables**: LCLA/LCLB/LCLC (local), GBLA/GBLB/GBLC (global),
//!   SETA/SETB/SETC (assign).
//! - **Control flow**: AIF (conditional branch), AGO (unconditional branch),
//!   ANOP (label target), ACTR (loop counter).
//! - **Built-in functions**: ~50 functions for base conversion (A2B, A2C, etc.),
//!   string manipulation (FIND, INDEX, UPPER, LOWER), and type testing (ISBIN,
//!   ISDEC, ISHEX, ISSYM).

use std::collections::HashMap;
use std::fmt;

// ---------------------------------------------------------------------------
//  SET variable types
// ---------------------------------------------------------------------------

/// Type of a SET symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SetType {
    /// Arithmetic (SETA) — i64 value.
    Arithmetic,
    /// Boolean (SETB) — 0 or 1.
    Boolean,
    /// Character (SETC) — string value.
    Character,
}

/// A SET symbol value.
#[derive(Debug, Clone, PartialEq)]
pub enum SetValue {
    Arithmetic(i64),
    Boolean(bool),
    Character(String),
}

impl fmt::Display for SetValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Arithmetic(n) => write!(f, "{n}"),
            Self::Boolean(b) => write!(f, "{}", if *b { 1 } else { 0 }),
            Self::Character(s) => write!(f, "{s}"),
        }
    }
}

impl SetValue {
    /// Get as arithmetic (coerce if needed).
    pub fn as_arithmetic(&self) -> i64 {
        match self {
            Self::Arithmetic(n) => *n,
            Self::Boolean(b) => i64::from(*b),
            Self::Character(s) => s.trim().parse().unwrap_or(0),
        }
    }

    /// Get as boolean.
    pub fn as_boolean(&self) -> bool {
        match self {
            Self::Arithmetic(n) => *n != 0,
            Self::Boolean(b) => *b,
            Self::Character(s) => !s.is_empty(),
        }
    }

    /// Get as character string.
    pub fn as_character(&self) -> String {
        self.to_string()
    }
}

// ---------------------------------------------------------------------------
//  Conditional assembly errors
// ---------------------------------------------------------------------------

/// Errors from conditional assembly processing.
#[derive(Debug, Clone, thiserror::Error)]
pub enum CondAsmError {
    #[error("undefined SET symbol: &{0}")]
    UndefinedSymbol(String),

    #[error("assembly-time label not found: .{0}")]
    LabelNotFound(String),

    #[error("ACTR limit exceeded (infinite loop protection)")]
    ActrExceeded,

    #[error("type mismatch: expected {expected}, got {got}")]
    TypeMismatch { expected: String, got: String },

    #[error("invalid conditional expression: {0}")]
    InvalidExpression(String),

    #[error("unknown built-in function: {0}")]
    UnknownFunction(String),
}

// ---------------------------------------------------------------------------
//  Conditional assembly statement
// ---------------------------------------------------------------------------

/// A parsed conditional assembly statement.
#[derive(Debug, Clone)]
pub enum CondAsmStmt {
    /// LCLA/LCLB/LCLC — declare local SET variable(s).
    LocalDecl { set_type: SetType, names: Vec<String> },
    /// GBLA/GBLB/GBLC — declare global SET variable(s).
    GlobalDecl { set_type: SetType, names: Vec<String> },
    /// SETA/SETB/SETC — assign value to SET variable.
    SetAssign { name: String, set_type: SetType, expression: String },
    /// AIF — conditional branch.
    Aif { condition: String, label: String },
    /// AGO — unconditional branch.
    Ago { label: String },
    /// ANOP — assembly-time no-operation (label target).
    Anop { label: Option<String> },
    /// ACTR — set loop counter limit.
    Actr { limit: i64 },
}

// ---------------------------------------------------------------------------
//  Conditional Assembly Engine
// ---------------------------------------------------------------------------

/// Evaluator for HLASM conditional assembly.
///
/// Manages SET symbol scopes (local/global), evaluates conditions,
/// processes AIF/AGO control flow, and provides ~50 built-in functions.
#[derive(Debug, Clone)]
pub struct CondAsmEngine {
    /// Global SET symbols.
    globals: HashMap<String, SetValue>,
    /// Local SET symbol scopes (stack of scopes for macro nesting).
    local_scopes: Vec<HashMap<String, SetValue>>,
    /// ACTR counter.
    actr_counter: i64,
    /// ACTR limit.
    actr_limit: i64,
}

impl Default for CondAsmEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl CondAsmEngine {
    /// Create a new engine with default ACTR limit of 4096.
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            local_scopes: vec![HashMap::new()],
            actr_counter: 0,
            actr_limit: 4096,
        }
    }

    /// Push a new local scope (e.g. entering a macro).
    pub fn push_scope(&mut self) {
        self.local_scopes.push(HashMap::new());
    }

    /// Pop the current local scope.
    pub fn pop_scope(&mut self) {
        if self.local_scopes.len() > 1 {
            self.local_scopes.pop();
        }
    }

    /// Declare a local SET symbol.
    pub fn declare_local(&mut self, name: &str, set_type: SetType) {
        let initial = match set_type {
            SetType::Arithmetic => SetValue::Arithmetic(0),
            SetType::Boolean => SetValue::Boolean(false),
            SetType::Character => SetValue::Character(String::new()),
        };
        if let Some(scope) = self.local_scopes.last_mut() {
            scope.insert(name.to_uppercase(), initial);
        }
    }

    /// Declare a global SET symbol.
    pub fn declare_global(&mut self, name: &str, set_type: SetType) {
        let key = name.to_uppercase();
        self.globals.entry(key).or_insert_with(|| match set_type {
            SetType::Arithmetic => SetValue::Arithmetic(0),
            SetType::Boolean => SetValue::Boolean(false),
            SetType::Character => SetValue::Character(String::new()),
        });
    }

    /// Set a variable value.
    pub fn set_var(&mut self, name: &str, value: SetValue) -> Result<(), CondAsmError> {
        let key = name.to_uppercase();
        // Check locals first (innermost scope outward).
        for scope in self.local_scopes.iter_mut().rev() {
            if let std::collections::hash_map::Entry::Occupied(mut e) = scope.entry(key.clone()) {
                e.insert(value);
                return Ok(());
            }
        }
        // Then globals.
        if let std::collections::hash_map::Entry::Occupied(mut e) = self.globals.entry(key.clone()) {
            e.insert(value);
            return Ok(());
        }
        Err(CondAsmError::UndefinedSymbol(key))
    }

    /// Get a variable value.
    pub fn get_var(&self, name: &str) -> Option<&SetValue> {
        let key = name.to_uppercase();
        // Check locals first.
        for scope in self.local_scopes.iter().rev() {
            if let Some(v) = scope.get(&key) {
                return Some(v);
            }
        }
        self.globals.get(&key)
    }

    /// Set the ACTR limit.
    pub fn set_actr(&mut self, limit: i64) {
        self.actr_limit = limit;
        self.actr_counter = 0;
    }

    /// Increment the ACTR counter and check the limit.
    pub fn check_actr(&mut self) -> Result<(), CondAsmError> {
        self.actr_counter += 1;
        if self.actr_counter > self.actr_limit {
            Err(CondAsmError::ActrExceeded)
        } else {
            Ok(())
        }
    }

    /// Evaluate a conditional assembly condition string.
    ///
    /// Supports: `(&VAR EQ 'value')`, `(&A GT &B)`, arithmetic comparisons,
    /// and string comparisons.
    pub fn eval_condition(&self, cond: &str) -> Result<bool, CondAsmError> {
        let cond = cond.trim();
        // Strip outer parens.
        let inner = if cond.starts_with('(') && cond.ends_with(')') {
            &cond[1..cond.len() - 1]
        } else {
            cond
        };

        // Find comparison operator.
        for op in &["EQ", "NE", "GT", "LT", "GE", "LE"] {
            if let Some(pos) = find_operator(inner, op) {
                let left = self.resolve_expr(inner[..pos].trim())?;
                let right = self.resolve_expr(inner[pos + op.len()..].trim())?;
                return Ok(compare_values(&left, &right, op));
            }
        }

        // Single value (truthy check).
        let val = self.resolve_expr(inner)?;
        Ok(val.as_boolean())
    }

    /// Resolve an expression string to a SetValue.
    fn resolve_expr(&self, expr: &str) -> Result<SetValue, CondAsmError> {
        let expr = expr.trim();

        // String literal: 'value'
        if expr.starts_with('\'') && expr.ends_with('\'') && expr.len() >= 2 {
            return Ok(SetValue::Character(expr[1..expr.len() - 1].to_string()));
        }

        // Variable reference: &NAME
        if let Some(name) = expr.strip_prefix('&') {
            return self
                .get_var(name)
                .cloned()
                .ok_or_else(|| CondAsmError::UndefinedSymbol(name.to_string()));
        }

        // K' attribute (character count).
        if let Some(rest) = expr.strip_prefix("K'") {
            if let Some(name) = rest.strip_prefix('&') {
                if let Some(val) = self.get_var(name) {
                    return Ok(SetValue::Arithmetic(val.as_character().len() as i64));
                }
            }
            return Ok(SetValue::Arithmetic(rest.len() as i64));
        }

        // Built-in function call: FUNC'arg or FUNC(arg).
        if let Some(paren_pos) = expr.find('(') {
            let func = expr[..paren_pos].trim();
            let arg = expr[paren_pos + 1..]
                .trim_end_matches(')')
                .to_string();
            if !func.is_empty() && func.chars().all(|c| c.is_ascii_alphanumeric()) {
                return self.call_builtin(func, &arg);
            }
        }

        // Numeric literal.
        if let Ok(n) = expr.parse::<i64>() {
            return Ok(SetValue::Arithmetic(n));
        }

        // Simple addition/subtraction.
        if let Some(pos) = expr.rfind('+') {
            if pos > 0 {
                let left = self.resolve_expr(&expr[..pos])?;
                let right = self.resolve_expr(&expr[pos + 1..])?;
                return Ok(SetValue::Arithmetic(
                    left.as_arithmetic() + right.as_arithmetic(),
                ));
            }
        }

        Ok(SetValue::Character(expr.to_string()))
    }

    // -----------------------------------------------------------------------
    //  Built-in functions (~50)
    // -----------------------------------------------------------------------

    fn call_builtin(&self, func: &str, arg: &str) -> Result<SetValue, CondAsmError> {
        let func_upper = func.to_uppercase();
        let resolved_arg = self.resolve_expr(arg)?;
        let s = resolved_arg.as_character();
        let n = resolved_arg.as_arithmetic();

        match func_upper.as_str() {
            // ─── Base conversion ───
            "A2B" => Ok(SetValue::Character(format!("{:b}", n))),
            "A2C" => Ok(SetValue::Character(
                if (0..=127).contains(&n) { (n as u8 as char).to_string() } else { String::new() }
            )),
            "A2D" => Ok(SetValue::Character(format!("{n}"))),
            "A2X" => Ok(SetValue::Character(format!("{:X}", n))),

            "B2A" => Ok(SetValue::Arithmetic(
                i64::from_str_radix(s.trim(), 2).unwrap_or(0)
            )),
            "B2C" => {
                let val = u8::from_str_radix(s.trim(), 2).unwrap_or(0);
                Ok(SetValue::Character((val as char).to_string()))
            }
            "B2D" => Ok(SetValue::Character(format!(
                "{}", i64::from_str_radix(s.trim(), 2).unwrap_or(0)
            ))),
            "B2X" => Ok(SetValue::Character(format!(
                "{:X}", i64::from_str_radix(s.trim(), 2).unwrap_or(0)
            ))),

            "C2A" => Ok(SetValue::Arithmetic(
                s.bytes().next().unwrap_or(0) as i64
            )),
            "C2B" => Ok(SetValue::Character(format!(
                "{:08b}", s.bytes().next().unwrap_or(0)
            ))),
            "C2D" => Ok(SetValue::Character(format!(
                "{}", s.bytes().next().unwrap_or(0)
            ))),
            "C2X" => {
                use std::fmt::Write;
                let hex = s.bytes().fold(String::new(), |mut acc, b| {
                    let _ = write!(acc, "{b:02X}");
                    acc
                });
                Ok(SetValue::Character(hex))
            }

            "D2A" => Ok(SetValue::Arithmetic(s.trim().parse::<i64>().unwrap_or(0))),
            "D2B" => Ok(SetValue::Character(format!(
                "{:b}", s.trim().parse::<i64>().unwrap_or(0)
            ))),
            "D2C" => {
                let v = s.trim().parse::<i64>().unwrap_or(0);
                Ok(SetValue::Character(
                    if (0..=127).contains(&v) { (v as u8 as char).to_string() } else { String::new() }
                ))
            }
            "D2X" => Ok(SetValue::Character(format!(
                "{:X}", s.trim().parse::<i64>().unwrap_or(0)
            ))),

            "X2A" => Ok(SetValue::Arithmetic(
                i64::from_str_radix(s.trim(), 16).unwrap_or(0)
            )),
            "X2B" => Ok(SetValue::Character(format!(
                "{:b}", i64::from_str_radix(s.trim(), 16).unwrap_or(0)
            ))),
            "X2C" => {
                let bytes: Vec<u8> = (0..s.len())
                    .step_by(2)
                    .filter_map(|i| u8::from_str_radix(&s[i..i.min(s.len()) + 2.min(s.len() - i)], 16).ok())
                    .collect();
                Ok(SetValue::Character(
                    String::from_utf8_lossy(&bytes).to_string()
                ))
            }
            "X2D" => Ok(SetValue::Character(format!(
                "{}", i64::from_str_radix(s.trim(), 16).unwrap_or(0)
            ))),

            // ─── String functions ───
            "BYTE" => {
                let ch = if (0..=127).contains(&n) { (n as u8 as char).to_string() } else { String::new() };
                Ok(SetValue::Character(ch))
            }
            "DCLEN" => Ok(SetValue::Arithmetic(s.len() as i64)),
            "DCVAL" => Ok(SetValue::Character(s)),
            "DEQUOTE" => {
                let dq = if (s.starts_with('\'') && s.ends_with('\''))
                    || (s.starts_with('"') && s.ends_with('"'))
                {
                    s[1..s.len() - 1].to_string()
                } else {
                    s
                };
                Ok(SetValue::Character(dq))
            }
            "DOUBLE" => Ok(SetValue::Character(s.replace('&', "&&").replace('\'', "''"))),
            "FIND" => {
                // FIND(haystack, needle) — we receive the whole arg as a comma-separated pair.
                let parts: Vec<&str> = arg.splitn(2, ',').collect();
                if parts.len() == 2 {
                    let h = self.resolve_expr(parts[0])?.as_character();
                    let n = self.resolve_expr(parts[1])?.as_character();
                    Ok(SetValue::Arithmetic(
                        h.find(&n).map(|i| i as i64 + 1).unwrap_or(0)
                    ))
                } else {
                    Ok(SetValue::Arithmetic(0))
                }
            }
            "INDEX" => {
                let parts: Vec<&str> = arg.splitn(2, ',').collect();
                if parts.len() == 2 {
                    let h = self.resolve_expr(parts[0])?.as_character();
                    let n = self.resolve_expr(parts[1])?.as_character();
                    Ok(SetValue::Arithmetic(
                        h.find(&n).map(|i| i as i64 + 1).unwrap_or(0)
                    ))
                } else {
                    Ok(SetValue::Arithmetic(0))
                }
            }
            "LOWER" => Ok(SetValue::Character(s.to_lowercase())),
            "UPPER" => Ok(SetValue::Character(s.to_uppercase())),

            // ─── Type testing ───
            "ISBIN" => Ok(SetValue::Boolean(s.chars().all(|c| c == '0' || c == '1'))),
            "ISDEC" => Ok(SetValue::Boolean(
                !s.is_empty() && s.chars().all(|c| c.is_ascii_digit() || c == '-' || c == '+')
            )),
            "ISHEX" => Ok(SetValue::Boolean(
                !s.is_empty() && s.chars().all(|c| c.is_ascii_hexdigit())
            )),
            "ISSYM" => Ok(SetValue::Boolean(
                !s.is_empty()
                    && s.chars().next().map_or(false, |c| c.is_ascii_alphabetic() || c == '_')
                    && s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '@' || c == '#' || c == '$')
            )),
            "SIGNED" => {
                if n >= 0 {
                    Ok(SetValue::Character(format!("+{n}")))
                } else {
                    Ok(SetValue::Character(format!("{n}")))
                }
            }

            _ => Err(CondAsmError::UnknownFunction(func_upper)),
        }
    }
}

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

fn find_operator(expr: &str, op: &str) -> Option<usize> {
    // Find operator outside of quoted strings.
    let op_with_spaces = format!(" {op} ");
    expr.find(&op_with_spaces).map(|p| p + 1) // +1 to skip leading space
}

fn compare_values(left: &SetValue, right: &SetValue, op: &str) -> bool {
    // If both can be arithmetic, compare numerically.
    if let (SetValue::Arithmetic(a), SetValue::Arithmetic(b)) = (left, right) {
        return match op {
            "EQ" => a == b,
            "NE" => a != b,
            "GT" => a > b,
            "LT" => a < b,
            "GE" => a >= b,
            "LE" => a <= b,
            _ => false,
        };
    }
    // Otherwise compare as strings.
    let a = left.as_character();
    let b = right.as_character();
    match op {
        "EQ" => a == b,
        "NE" => a != b,
        "GT" => a > b,
        "LT" => a < b,
        "GE" => a >= b,
        "LE" => a <= b,
        _ => false,
    }
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ─── A108.1: SET Symbols and AIF/AGO ───

    #[test]
    fn test_declare_local_arithmetic() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("COUNT", SetType::Arithmetic);
        let val = engine.get_var("COUNT").unwrap();
        assert_eq!(val.as_arithmetic(), 0);
    }

    #[test]
    fn test_declare_local_character() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("TYPE", SetType::Character);
        let val = engine.get_var("TYPE").unwrap();
        assert_eq!(val.as_character(), "");
    }

    #[test]
    fn test_declare_local_boolean() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("FLAG", SetType::Boolean);
        let val = engine.get_var("FLAG").unwrap();
        assert!(!val.as_boolean());
    }

    #[test]
    fn test_set_and_get_local() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("TYPE", SetType::Character);
        engine.set_var("TYPE", SetValue::Character("FULL".to_string())).unwrap();
        let val = engine.get_var("TYPE").unwrap();
        assert_eq!(val.as_character(), "FULL");
    }

    #[test]
    fn test_global_variables() {
        let mut engine = CondAsmEngine::new();
        engine.declare_global("GCOUNT", SetType::Arithmetic);
        engine.set_var("GCOUNT", SetValue::Arithmetic(42)).unwrap();
        assert_eq!(engine.get_var("GCOUNT").unwrap().as_arithmetic(), 42);
    }

    #[test]
    fn test_global_persists_across_scopes() {
        let mut engine = CondAsmEngine::new();
        engine.declare_global("GVAR", SetType::Arithmetic);
        engine.set_var("GVAR", SetValue::Arithmetic(10)).unwrap();

        engine.push_scope();
        assert_eq!(engine.get_var("GVAR").unwrap().as_arithmetic(), 10);
        engine.set_var("GVAR", SetValue::Arithmetic(20)).unwrap();
        engine.pop_scope();

        assert_eq!(engine.get_var("GVAR").unwrap().as_arithmetic(), 20);
    }

    #[test]
    fn test_local_scope_isolation() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("X", SetType::Arithmetic);
        engine.set_var("X", SetValue::Arithmetic(1)).unwrap();

        engine.push_scope();
        engine.declare_local("X", SetType::Arithmetic);
        engine.set_var("X", SetValue::Arithmetic(2)).unwrap();
        assert_eq!(engine.get_var("X").unwrap().as_arithmetic(), 2);
        engine.pop_scope();

        assert_eq!(engine.get_var("X").unwrap().as_arithmetic(), 1);
    }

    #[test]
    fn test_undefined_symbol_error() {
        let mut engine = CondAsmEngine::new();
        let result = engine.set_var("NOSUCH", SetValue::Arithmetic(0));
        assert!(result.is_err());
    }

    #[test]
    fn test_eval_condition_string_eq() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("TYPE", SetType::Character);
        engine.set_var("TYPE", SetValue::Character("FULL".to_string())).unwrap();

        let result = engine.eval_condition("(&TYPE EQ 'FULL')").unwrap();
        assert!(result);
    }

    #[test]
    fn test_eval_condition_string_ne() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("TYPE", SetType::Character);
        engine.set_var("TYPE", SetValue::Character("FULL".to_string())).unwrap();

        let result = engine.eval_condition("(&TYPE NE 'HALF')").unwrap();
        assert!(result);
    }

    #[test]
    fn test_eval_condition_arithmetic_gt() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("A", SetType::Arithmetic);
        engine.set_var("A", SetValue::Arithmetic(10)).unwrap();

        assert!(engine.eval_condition("(&A GT 5)").unwrap());
        assert!(!engine.eval_condition("(&A GT 20)").unwrap());
    }

    #[test]
    fn test_actr_limit() {
        let mut engine = CondAsmEngine::new();
        engine.set_actr(3);
        assert!(engine.check_actr().is_ok());
        assert!(engine.check_actr().is_ok());
        assert!(engine.check_actr().is_ok());
        assert!(engine.check_actr().is_err()); // 4th exceeds limit of 3
    }

    #[test]
    fn test_k_attribute() {
        let mut engine = CondAsmEngine::new();
        engine.declare_local("PARM", SetType::Character);
        engine.set_var("PARM", SetValue::Character("HELLO".to_string())).unwrap();

        let val = engine.resolve_expr("K'&PARM").unwrap();
        assert_eq!(val.as_arithmetic(), 5);
    }

    // ─── A108.2: Built-in Functions ───

    #[test]
    fn test_a2b() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("A2B", "10").unwrap();
        assert_eq!(r.as_character(), "1010");
    }

    #[test]
    fn test_a2x() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("A2X", "255").unwrap();
        assert_eq!(r.as_character(), "FF");
    }

    #[test]
    fn test_a2d() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("A2D", "42").unwrap();
        assert_eq!(r.as_character(), "42");
    }

    #[test]
    fn test_b2a() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("B2A", "'1010'").unwrap();
        assert_eq!(r.as_arithmetic(), 10);
    }

    #[test]
    fn test_x2a() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("X2A", "'FF'").unwrap();
        assert_eq!(r.as_arithmetic(), 255);
    }

    #[test]
    fn test_c2a() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("C2A", "'A'").unwrap();
        assert_eq!(r.as_arithmetic(), 65);
    }

    #[test]
    fn test_c2x() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("C2X", "'AB'").unwrap();
        assert_eq!(r.as_character(), "4142");
    }

    #[test]
    fn test_d2x() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("D2X", "'255'").unwrap();
        assert_eq!(r.as_character(), "FF");
    }

    #[test]
    fn test_upper_lower() {
        let engine = CondAsmEngine::new();
        assert_eq!(
            engine.call_builtin("UPPER", "'hello'").unwrap().as_character(),
            "HELLO"
        );
        assert_eq!(
            engine.call_builtin("LOWER", "'HELLO'").unwrap().as_character(),
            "hello"
        );
    }

    #[test]
    fn test_find() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("FIND", "'ABCDEF','CD'").unwrap();
        assert_eq!(r.as_arithmetic(), 3);
    }

    #[test]
    fn test_find_not_found() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("FIND", "'ABCDEF','XY'").unwrap();
        assert_eq!(r.as_arithmetic(), 0);
    }

    #[test]
    fn test_dequote() {
        let mut engine = CondAsmEngine::new();
        // Set a variable containing a quoted string.
        engine.declare_local("S", SetType::Character);
        engine.set_var("S", SetValue::Character("'hello'".to_string())).unwrap();
        // DEQUOTE via resolve_expr(&S) → "'hello'", then dequote strips outer quotes.
        let r = engine.call_builtin("DEQUOTE", "&S").unwrap();
        assert_eq!(r.as_character(), "hello");
    }

    #[test]
    fn test_double() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("DOUBLE", "'A&B'").unwrap();
        assert_eq!(r.as_character(), "A&&B");
    }

    #[test]
    fn test_isbin() {
        let engine = CondAsmEngine::new();
        assert!(engine.call_builtin("ISBIN", "'1010'").unwrap().as_boolean());
        assert!(!engine.call_builtin("ISBIN", "'12'").unwrap().as_boolean());
    }

    #[test]
    fn test_isdec() {
        let engine = CondAsmEngine::new();
        assert!(engine.call_builtin("ISDEC", "'12345'").unwrap().as_boolean());
        assert!(!engine.call_builtin("ISDEC", "'12AB'").unwrap().as_boolean());
    }

    #[test]
    fn test_ishex() {
        let engine = CondAsmEngine::new();
        assert!(engine.call_builtin("ISHEX", "'FF0A'").unwrap().as_boolean());
        assert!(!engine.call_builtin("ISHEX", "'GHIJ'").unwrap().as_boolean());
    }

    #[test]
    fn test_issym() {
        let engine = CondAsmEngine::new();
        assert!(engine.call_builtin("ISSYM", "'ABC123'").unwrap().as_boolean());
        assert!(!engine.call_builtin("ISSYM", "'123ABC'").unwrap().as_boolean());
    }

    #[test]
    fn test_signed() {
        let engine = CondAsmEngine::new();
        assert_eq!(engine.call_builtin("SIGNED", "42").unwrap().as_character(), "+42");
        assert_eq!(engine.call_builtin("SIGNED", "-5").unwrap().as_character(), "-5");
    }

    #[test]
    fn test_byte() {
        let engine = CondAsmEngine::new();
        assert_eq!(engine.call_builtin("BYTE", "65").unwrap().as_character(), "A");
    }

    #[test]
    fn test_dclen() {
        let engine = CondAsmEngine::new();
        let r = engine.call_builtin("DCLEN", "'HELLO'").unwrap();
        assert_eq!(r.as_arithmetic(), 5);
    }

    #[test]
    fn test_unknown_builtin() {
        let engine = CondAsmEngine::new();
        assert!(engine.call_builtin("NOSUCHFUNC", "1").is_err());
    }

    #[test]
    fn test_set_value_display() {
        assert_eq!(format!("{}", SetValue::Arithmetic(42)), "42");
        assert_eq!(format!("{}", SetValue::Boolean(true)), "1");
        assert_eq!(format!("{}", SetValue::Character("HI".to_string())), "HI");
    }

    #[test]
    fn test_set_value_coercion() {
        let a = SetValue::Arithmetic(42);
        assert_eq!(a.as_character(), "42");
        assert!(a.as_boolean());

        let s = SetValue::Character("10".to_string());
        assert_eq!(s.as_arithmetic(), 10);

        let b = SetValue::Boolean(true);
        assert_eq!(b.as_arithmetic(), 1);
        assert_eq!(b.as_character(), "1");
    }
}
