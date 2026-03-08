//! Property specification for formal verification.
//!
//! Properties describe what a COBOL program *should* do, and the model
//! checker verifies them against the symbolic execution results.
//! Properties can be written in YAML files or embedded as COBOL
//! structured comments (`*> @INVARIANT: ...`).

use crate::value::{ExprOp, SymbolicValue};

/// A verification property to be checked against execution paths.
#[derive(Debug, Clone)]
pub enum Property {
    /// An invariant that must hold at the specified location on every path.
    Invariant {
        name: String,
        condition: SymbolicValue,
        location: PropertyLocation,
    },

    /// An implication: if `precondition` holds at entry, then
    /// `postcondition` must hold at exit.
    Implication {
        name: String,
        precondition: SymbolicValue,
        postcondition: SymbolicValue,
    },

    /// A safety property: the `violation_condition` must never be true.
    Safety {
        name: String,
        violation_condition: SymbolicValue,
    },

    /// Equivalence: two expressions (from different program versions)
    /// must produce the same value.  An optional tolerance allows
    /// approximate equality for numeric migration.
    Equivalence {
        name: String,
        program_a_output: SymbolicValue,
        program_b_output: SymbolicValue,
        tolerance: Option<f64>,
    },
}

impl Property {
    /// The human-readable name of this property.
    pub fn name(&self) -> &str {
        match self {
            Property::Invariant { name, .. }
            | Property::Implication { name, .. }
            | Property::Safety { name, .. }
            | Property::Equivalence { name, .. } => name,
        }
    }
}

/// Where to check a property relative to the program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PropertyLocation {
    /// At program entry (before the first statement).
    Entry,
    /// At program exit (after `STOP RUN` / `GOBACK`).
    Exit,
    /// At a specific source line.
    Source { line: usize, column: usize },
    /// At a named paragraph or section.
    Paragraph(String),
    /// Everywhere (checked on all paths at every step).
    Global,
}

// ---------------------------------------------------------------------------
// Parsing from COBOL annotations
// ---------------------------------------------------------------------------

/// Parse inline COBOL annotations of the form:
///
/// ```text
/// *> @INVARIANT: BALANCE >= 0
/// *> @PRE: AMOUNT > 0
/// *> @POST: NEW-BALANCE = OLD-BALANCE + AMOUNT
/// *> @SAFETY: DIVIDE-BY-ZERO-FLAG = 0
/// ```
pub fn parse_annotations(source: &str) -> Vec<Property> {
    let mut properties = Vec::new();

    for line in source.lines() {
        let trimmed = line.trim();

        // Look for structured comment annotations.
        let comment_body = if let Some(rest) = trimmed.strip_prefix("*>") {
            rest.trim()
        } else {
            continue;
        };

        if let Some(rest) = comment_body.strip_prefix("@INVARIANT:") {
            let rest = rest.trim();
            if let Some(prop) = parse_simple_condition(rest) {
                properties.push(Property::Invariant {
                    name: format!("invariant: {rest}"),
                    condition: prop,
                    location: PropertyLocation::Global,
                });
            }
        } else if let Some(rest) = comment_body.strip_prefix("@PRE:") {
            let rest = rest.trim();
            if let Some(cond) = parse_simple_condition(rest) {
                // Store as an implication with the postcondition TBD
                // (will be paired with the next @POST).
                properties.push(Property::Implication {
                    name: format!("pre: {rest}"),
                    precondition: cond,
                    postcondition: SymbolicValue::ConcreteBool(true),
                });
            }
        } else if let Some(rest) = comment_body.strip_prefix("@POST:") {
            let rest = rest.trim();
            if let Some(cond) = parse_simple_condition(rest) {
                // Try to pair with the most recent @PRE.
                if let Some(Property::Implication {
                    postcondition,
                    name,
                    ..
                }) = properties.last_mut()
                {
                    *postcondition = cond;
                    *name = format!("{name} => post: {rest}");
                } else {
                    properties.push(Property::Invariant {
                        name: format!("post: {rest}"),
                        condition: cond,
                        location: PropertyLocation::Exit,
                    });
                }
            }
        } else if let Some(rest) = comment_body.strip_prefix("@SAFETY:") {
            let rest = rest.trim();
            if let Some(cond) = parse_simple_condition(rest) {
                properties.push(Property::Safety {
                    name: format!("safety: {rest}"),
                    violation_condition: cond,
                });
            }
        }
    }

    properties
}

/// Parse a simple condition of the form `VAR op VALUE` where `op` is one
/// of `>=`, `<=`, `>`, `<`, `=`, `<>`.
///
/// Returns `None` if the condition cannot be parsed.
fn parse_simple_condition(text: &str) -> Option<SymbolicValue> {
    // Try each operator (longest first to avoid prefix conflicts).
    for (token, op) in &[
        (">=", ExprOp::Ge),
        ("<=", ExprOp::Le),
        ("<>", ExprOp::Ne),
        (">", ExprOp::Gt),
        ("<", ExprOp::Lt),
        ("=", ExprOp::Eq),
    ] {
        if let Some(pos) = text.find(token) {
            let lhs_text = text[..pos].trim();
            let rhs_text = text[pos + token.len()..].trim();

            let lhs = parse_operand(lhs_text);
            let rhs = parse_operand(rhs_text);

            return Some(SymbolicValue::Expression {
                op: op.clone(),
                args: vec![lhs, rhs],
            });
        }
    }
    None
}

/// Parse a single operand: integer literal or symbolic variable name.
fn parse_operand(text: &str) -> SymbolicValue {
    // Try integer.
    if let Ok(n) = text.parse::<i64>() {
        return SymbolicValue::Concrete(n);
    }
    // Otherwise treat as a symbolic variable.
    SymbolicValue::sym_int(text)
}

/// Parse properties from a YAML string.
///
/// Expected format:
/// ```yaml
/// properties:
///   - name: "balance_non_negative"
///     type: invariant
///     condition: "BALANCE >= 0"
///     location: exit
///   - name: "amount_positive"
///     type: safety
///     condition: "AMOUNT = 0"
/// ```
pub fn parse_properties_yaml(yaml: &str) -> Result<Vec<Property>, String> {
    // Lightweight YAML-ish parser (avoids pulling in a full YAML dep).
    let mut properties = Vec::new();
    let mut current_name: Option<String> = None;
    let mut current_type: Option<String> = None;
    let mut current_condition: Option<String> = None;
    let mut current_location: Option<String> = None;

    let flush = |name: &Option<String>,
                 typ: &Option<String>,
                 cond: &Option<String>,
                 loc: &Option<String>,
                 out: &mut Vec<Property>|
     -> Result<(), String> {
        let Some(name) = name else { return Ok(()) };
        let Some(typ) = typ else {
            return Err(format!("property '{name}' missing type"));
        };
        let Some(cond_str) = cond else {
            return Err(format!("property '{name}' missing condition"));
        };

        let condition = parse_simple_condition(cond_str)
            .ok_or_else(|| format!("cannot parse condition: {cond_str}"))?;

        let location = match loc.as_deref() {
            Some("entry") => PropertyLocation::Entry,
            Some("exit") => PropertyLocation::Exit,
            Some("global") | None => PropertyLocation::Global,
            Some(other) => PropertyLocation::Paragraph(other.to_string()),
        };

        match typ.as_str() {
            "invariant" => {
                out.push(Property::Invariant {
                    name: name.clone(),
                    condition,
                    location,
                });
            }
            "safety" => {
                out.push(Property::Safety {
                    name: name.clone(),
                    violation_condition: condition,
                });
            }
            other => {
                return Err(format!("unknown property type: {other}"));
            }
        }
        Ok(())
    };

    for line in yaml.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("- name:") {
            // Flush previous property.
            flush(
                &current_name,
                &current_type,
                &current_condition,
                &current_location,
                &mut properties,
            )?;
            current_name = Some(extract_yaml_value(trimmed, "name:"));
            current_type = None;
            current_condition = None;
            current_location = None;
        } else if trimmed.starts_with("type:") {
            current_type = Some(extract_yaml_value(trimmed, "type:"));
        } else if trimmed.starts_with("condition:") {
            current_condition = Some(extract_yaml_value(trimmed, "condition:"));
        } else if trimmed.starts_with("location:") {
            current_location = Some(extract_yaml_value(trimmed, "location:"));
        }
    }

    // Flush last property.
    flush(
        &current_name,
        &current_type,
        &current_condition,
        &current_location,
        &mut properties,
    )?;

    Ok(properties)
}

/// Extract the value from a "key: value" or 'key: "value"' YAML line.
fn extract_yaml_value(line: &str, key: &str) -> String {
    let rest = line.split_once(key).map(|(_, v)| v.trim()).unwrap_or("");
    // Strip quotes.
    rest.trim_matches('"').trim_matches('\'').to_string()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_invariant_annotation() {
        let source = r#"
       *> @INVARIANT: BALANCE >= 0
       *> @SAFETY: DIVISOR = 0
        "#;
        let props = parse_annotations(source);
        assert_eq!(props.len(), 2);
        assert!(matches!(&props[0], Property::Invariant { .. }));
        assert!(matches!(&props[1], Property::Safety { .. }));
    }

    #[test]
    fn parse_pre_post_pair() {
        let source = r#"
       *> @PRE: AMOUNT > 0
       *> @POST: NEW-BALANCE >= 0
        "#;
        let props = parse_annotations(source);
        assert_eq!(props.len(), 1);
        assert!(matches!(&props[0], Property::Implication { .. }));
        assert!(props[0].name().contains("post:"));
    }

    #[test]
    fn parse_simple_condition_eq() {
        let cond = parse_simple_condition("X = 42").unwrap();
        assert!(matches!(
            cond,
            SymbolicValue::Expression { op: ExprOp::Eq, .. }
        ));
    }

    #[test]
    fn parse_simple_condition_ge() {
        let cond = parse_simple_condition("BALANCE >= 0").unwrap();
        assert!(matches!(
            cond,
            SymbolicValue::Expression { op: ExprOp::Ge, .. }
        ));
    }

    #[test]
    fn parse_operand_int() {
        assert_eq!(parse_operand("42"), SymbolicValue::Concrete(42));
        assert_eq!(parse_operand("-5"), SymbolicValue::Concrete(-5));
    }

    #[test]
    fn parse_operand_variable() {
        assert_eq!(parse_operand("AMOUNT"), SymbolicValue::sym_int("AMOUNT"));
    }

    #[test]
    fn parse_yaml_basic() {
        let yaml = r#"
properties:
  - name: "balance_ok"
    type: invariant
    condition: "BALANCE >= 0"
    location: exit
  - name: "no_div_zero"
    type: safety
    condition: "DIVISOR = 0"
        "#;
        let props = parse_properties_yaml(yaml).unwrap();
        assert_eq!(props.len(), 2);
        assert!(matches!(&props[0], Property::Invariant { .. }));
        assert!(matches!(&props[1], Property::Safety { .. }));
    }

    #[test]
    fn property_name() {
        let p = Property::Safety {
            name: "test".into(),
            violation_condition: SymbolicValue::bool(false),
        };
        assert_eq!(p.name(), "test");
    }
}
