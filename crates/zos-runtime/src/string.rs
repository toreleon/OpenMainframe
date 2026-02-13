//! String manipulation functions for COBOL runtime.
//!
//! This module implements STRING, UNSTRING, and INSPECT verbs.

use crate::error::RuntimeError;

/// Delimiter type for STRING operation.
#[derive(Debug, Clone)]
pub enum StringDelimiter {
    /// Use the entire field (SIZE).
    Size,
    /// Use up to a specific delimiter string.
    Value(String),
}

/// Source for STRING operation.
#[derive(Debug, Clone)]
pub struct StringSource {
    /// The source value.
    pub value: String,
    /// How to delimit the source.
    pub delimiter: StringDelimiter,
}

/// Execute a STRING statement.
///
/// Concatenates source strings into target, stopping at delimiters.
pub fn string_concat(
    sources: &[StringSource],
    target_size: usize,
    initial_pointer: usize,
) -> Result<(String, usize, bool), RuntimeError> {
    let mut result = String::with_capacity(target_size);
    let mut pointer = initial_pointer.saturating_sub(1); // COBOL uses 1-based indexing
    let mut overflow = false;

    for source in sources {
        let text = match &source.delimiter {
            StringDelimiter::Size => source.value.clone(),
            StringDelimiter::Value(delim) => {
                // Take characters up to (but not including) the delimiter
                if let Some(pos) = source.value.find(delim.as_str()) {
                    source.value[..pos].to_string()
                } else {
                    source.value.clone()
                }
            }
        };

        for ch in text.chars() {
            if pointer >= target_size {
                overflow = true;
                break;
            }
            result.push(ch);
            pointer += 1;
        }

        if overflow {
            break;
        }
    }

    // Pad with spaces if needed
    while result.len() < target_size {
        result.push(' ');
    }

    // Truncate if overflow
    if result.len() > target_size {
        result.truncate(target_size);
    }

    Ok((result, pointer + 1, overflow)) // Return 1-based pointer
}

/// Delimiter for UNSTRING operation.
#[derive(Debug, Clone)]
pub struct UnstringDelimiter {
    /// Whether to treat multiple consecutive delimiters as one (ALL).
    pub all: bool,
    /// The delimiter string.
    pub value: String,
}

/// Target field for UNSTRING operation.
#[derive(Debug, Clone)]
pub struct UnstringTarget {
    /// Size of the target field.
    pub size: usize,
    /// Whether to capture the delimiter that ended this field.
    pub capture_delimiter: bool,
    /// Whether to capture the count of characters.
    pub capture_count: bool,
}

/// Result of UNSTRING operation for one field.
#[derive(Debug, Clone)]
pub struct UnstringFieldResult {
    /// The extracted value.
    pub value: String,
    /// The delimiter that ended this field (if captured).
    pub delimiter: Option<String>,
    /// The count of characters (if captured).
    pub count: Option<usize>,
}

/// Execute an UNSTRING statement.
///
/// Splits source string at delimiters into target fields.
pub fn unstring(
    source: &str,
    delimiters: &[UnstringDelimiter],
    targets: &[UnstringTarget],
    initial_pointer: usize,
) -> Result<(Vec<UnstringFieldResult>, usize, usize, bool), RuntimeError> {
    let mut results = Vec::with_capacity(targets.len());
    let mut pointer = initial_pointer.saturating_sub(1); // COBOL uses 1-based indexing
    let mut tally = 0;
    let mut overflow = false;

    let chars: Vec<char> = source.chars().collect();

    for target in targets {
        if pointer >= chars.len() {
            // Source exhausted
            overflow = true;
            results.push(UnstringFieldResult {
                value: String::new(),
                delimiter: None,
                count: if target.capture_count { Some(0) } else { None },
            });
            continue;
        }

        let mut field_value = String::new();
        let mut found_delimiter: Option<String> = None;

        // Scan for delimiter
        'scan: while pointer < chars.len() {
            // Check for delimiter match
            for delim in delimiters {
                let delim_chars: Vec<char> = delim.value.chars().collect();
                if pointer + delim_chars.len() <= chars.len() {
                    let slice: String =
                        chars[pointer..pointer + delim_chars.len()].iter().collect();
                    if slice == delim.value {
                        found_delimiter = Some(delim.value.clone());
                        pointer += delim_chars.len();

                        // Handle ALL keyword
                        if delim.all {
                            while pointer + delim_chars.len() <= chars.len() {
                                let next_slice: String =
                                    chars[pointer..pointer + delim_chars.len()].iter().collect();
                                if next_slice == delim.value {
                                    pointer += delim_chars.len();
                                } else {
                                    break;
                                }
                            }
                        }

                        break 'scan;
                    }
                }
            }

            // No delimiter found, add character to field
            if field_value.len() < target.size {
                field_value.push(chars[pointer]);
            }
            pointer += 1;
        }

        let count = field_value.len();
        tally += 1;

        // Pad with spaces if needed
        while field_value.len() < target.size {
            field_value.push(' ');
        }

        results.push(UnstringFieldResult {
            value: field_value,
            delimiter: if target.capture_delimiter {
                found_delimiter
            } else {
                None
            },
            count: if target.capture_count {
                Some(count)
            } else {
                None
            },
        });
    }

    Ok((results, pointer + 1, tally, overflow)) // Return 1-based pointer
}

/// Mode for INSPECT TALLYING.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InspectMode {
    /// Count all characters.
    Characters,
    /// Count all occurrences.
    All,
    /// Count leading occurrences.
    Leading,
    /// Count first occurrence.
    First,
}

/// INSPECT TALLYING clause.
#[derive(Debug, Clone)]
pub struct TallyingClause {
    /// What to count.
    pub mode: InspectMode,
    /// Pattern to match (None for CHARACTERS).
    pub pattern: Option<String>,
    /// BEFORE phrase pattern.
    pub before: Option<String>,
    /// AFTER phrase pattern.
    pub after: Option<String>,
}

/// Execute INSPECT TALLYING.
pub fn inspect_tallying(source: &str, clauses: &[TallyingClause]) -> usize {
    let mut count = 0;

    for clause in clauses {
        // Determine search range based on BEFORE/AFTER
        let search_text = apply_before_after(source, &clause.before, &clause.after);

        match clause.mode {
            InspectMode::Characters => {
                count += search_text.len();
            }
            InspectMode::All => {
                if let Some(ref pattern) = clause.pattern {
                    count += search_text.matches(pattern).count();
                }
            }
            InspectMode::Leading => {
                if let Some(ref pattern) = clause.pattern {
                    let chars: Vec<char> = search_text.chars().collect();
                    let pattern_chars: Vec<char> = pattern.chars().collect();
                    let mut i = 0;
                    while i + pattern_chars.len() <= chars.len() {
                        let slice: String = chars[i..i + pattern_chars.len()].iter().collect();
                        if slice == *pattern {
                            count += 1;
                            i += pattern_chars.len();
                        } else {
                            break;
                        }
                    }
                }
            }
            InspectMode::First => {
                if let Some(ref pattern) = clause.pattern {
                    if search_text.contains(pattern) {
                        count += 1;
                    }
                }
            }
        }
    }

    count
}

/// INSPECT REPLACING clause.
#[derive(Debug, Clone)]
pub struct ReplacingClause {
    /// What to replace.
    pub mode: InspectMode,
    /// Pattern to match (None for CHARACTERS).
    pub pattern: Option<String>,
    /// Replacement string.
    pub replacement: String,
    /// BEFORE phrase pattern.
    pub before: Option<String>,
    /// AFTER phrase pattern.
    pub after: Option<String>,
}

/// Execute INSPECT REPLACING.
pub fn inspect_replacing(source: &str, clauses: &[ReplacingClause]) -> String {
    let mut result = source.to_string();

    for clause in clauses {
        match clause.mode {
            InspectMode::Characters => {
                // Replace each character with replacement (character by character)
                let replacement_chars: Vec<char> = clause.replacement.chars().collect();
                if !replacement_chars.is_empty() {
                    let rep_char = replacement_chars[0];
                    let (prefix, search_text, suffix) =
                        split_before_after(&result, &clause.before, &clause.after);
                    let replaced: String = search_text.chars().map(|_| rep_char).collect();
                    result = format!("{}{}{}", prefix, replaced, suffix);
                }
            }
            InspectMode::All => {
                if let Some(ref pattern) = clause.pattern {
                    let (prefix, search_text, suffix) =
                        split_before_after(&result, &clause.before, &clause.after);
                    let replaced = search_text.replace(pattern, &clause.replacement);
                    result = format!("{}{}{}", prefix, replaced, suffix);
                }
            }
            InspectMode::Leading => {
                if let Some(ref pattern) = clause.pattern {
                    let (prefix, search_text, suffix) =
                        split_before_after(&result, &clause.before, &clause.after);

                    let chars: Vec<char> = search_text.chars().collect();
                    let pattern_chars: Vec<char> = pattern.chars().collect();
                    let replacement_chars: Vec<char> = clause.replacement.chars().collect();

                    let mut i = 0;
                    let mut replaced_chars: Vec<char> = Vec::new();

                    while i + pattern_chars.len() <= chars.len() {
                        let slice: String = chars[i..i + pattern_chars.len()].iter().collect();
                        if slice == *pattern {
                            replaced_chars.extend(replacement_chars.iter());
                            i += pattern_chars.len();
                        } else {
                            break;
                        }
                    }

                    replaced_chars.extend(chars[i..].iter());
                    let replaced: String = replaced_chars.into_iter().collect();
                    result = format!("{}{}{}", prefix, replaced, suffix);
                }
            }
            InspectMode::First => {
                if let Some(ref pattern) = clause.pattern {
                    let (prefix, search_text, suffix) =
                        split_before_after(&result, &clause.before, &clause.after);
                    let replaced = search_text.replacen(pattern, &clause.replacement, 1);
                    result = format!("{}{}{}", prefix, replaced, suffix);
                }
            }
        }
    }

    result
}

/// INSPECT CONVERTING clause.
pub fn inspect_converting(
    source: &str,
    from: &str,
    to: &str,
    before: Option<&str>,
    after: Option<&str>,
) -> String {
    let (prefix, search_text, suffix) = split_before_after(
        source,
        &before.map(|s| s.to_string()),
        &after.map(|s| s.to_string()),
    );

    let from_chars: Vec<char> = from.chars().collect();
    let to_chars: Vec<char> = to.chars().collect();

    let converted: String = search_text
        .chars()
        .map(|ch| {
            if let Some(pos) = from_chars.iter().position(|&c| c == ch) {
                if pos < to_chars.len() {
                    to_chars[pos]
                } else {
                    ch
                }
            } else {
                ch
            }
        })
        .collect();

    format!("{}{}{}", prefix, converted, suffix)
}

/// Apply BEFORE/AFTER phrases to get search range.
fn apply_before_after(source: &str, before: &Option<String>, after: &Option<String>) -> String {
    let mut start = 0;
    let mut end = source.len();

    if let Some(ref after_pattern) = after {
        if let Some(pos) = source.find(after_pattern) {
            start = pos + after_pattern.len();
        }
    }

    if let Some(ref before_pattern) = before {
        if let Some(pos) = source[start..].find(before_pattern) {
            end = start + pos;
        }
    }

    source[start..end].to_string()
}

/// Split source into prefix, searchable text, and suffix based on BEFORE/AFTER.
fn split_before_after(
    source: &str,
    before: &Option<String>,
    after: &Option<String>,
) -> (String, String, String) {
    let mut start = 0;
    let mut end = source.len();

    if let Some(ref after_pattern) = after {
        if let Some(pos) = source.find(after_pattern) {
            start = pos + after_pattern.len();
        }
    }

    if let Some(ref before_pattern) = before {
        if let Some(pos) = source[start..].find(before_pattern) {
            end = start + pos;
        }
    }

    (
        source[..start].to_string(),
        source[start..end].to_string(),
        source[end..].to_string(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_concat_basic() {
        let sources = vec![
            StringSource {
                value: "HELLO".to_string(),
                delimiter: StringDelimiter::Size,
            },
            StringSource {
                value: " WORLD".to_string(),
                delimiter: StringDelimiter::Size,
            },
        ];

        let (result, pointer, overflow) = string_concat(&sources, 20, 1).unwrap();

        assert_eq!(result.trim(), "HELLO WORLD");
        assert_eq!(pointer, 12);
        assert!(!overflow);
    }

    #[test]
    fn test_string_concat_with_delimiter() {
        let sources = vec![
            StringSource {
                value: "ABC,DEF".to_string(),
                delimiter: StringDelimiter::Value(",".to_string()),
            },
            StringSource {
                value: "XYZ".to_string(),
                delimiter: StringDelimiter::Size,
            },
        ];

        let (result, _, _) = string_concat(&sources, 10, 1).unwrap();

        assert_eq!(result.trim(), "ABCXYZ");
    }

    #[test]
    fn test_string_concat_overflow() {
        let sources = vec![StringSource {
            value: "HELLO WORLD".to_string(),
            delimiter: StringDelimiter::Size,
        }];

        let (result, _, overflow) = string_concat(&sources, 5, 1).unwrap();

        assert_eq!(result, "HELLO");
        assert!(overflow);
    }

    #[test]
    fn test_unstring_basic() {
        let source = "A,B,C";
        let delimiters = vec![UnstringDelimiter {
            all: false,
            value: ",".to_string(),
        }];
        let targets = vec![
            UnstringTarget {
                size: 5,
                capture_delimiter: false,
                capture_count: true,
            },
            UnstringTarget {
                size: 5,
                capture_delimiter: false,
                capture_count: true,
            },
            UnstringTarget {
                size: 5,
                capture_delimiter: false,
                capture_count: true,
            },
        ];

        let (results, _, tally, _) = unstring(source, &delimiters, &targets, 1).unwrap();

        assert_eq!(results.len(), 3);
        assert_eq!(results[0].value.trim(), "A");
        assert_eq!(results[1].value.trim(), "B");
        assert_eq!(results[2].value.trim(), "C");
        assert_eq!(tally, 3);
    }

    #[test]
    fn test_unstring_all_delimiter() {
        let source = "A,,B";
        let delimiters = vec![UnstringDelimiter {
            all: true,
            value: ",".to_string(),
        }];
        let targets = vec![
            UnstringTarget {
                size: 5,
                capture_delimiter: false,
                capture_count: false,
            },
            UnstringTarget {
                size: 5,
                capture_delimiter: false,
                capture_count: false,
            },
        ];

        let (results, _, _, _) = unstring(source, &delimiters, &targets, 1).unwrap();

        assert_eq!(results[0].value.trim(), "A");
        assert_eq!(results[1].value.trim(), "B");
    }

    #[test]
    fn test_inspect_tallying_all() {
        let source = "BANANA";
        let clauses = vec![TallyingClause {
            mode: InspectMode::All,
            pattern: Some("A".to_string()),
            before: None,
            after: None,
        }];

        let count = inspect_tallying(source, &clauses);
        assert_eq!(count, 3);
    }

    #[test]
    fn test_inspect_tallying_leading() {
        let source = "AAA123";
        let clauses = vec![TallyingClause {
            mode: InspectMode::Leading,
            pattern: Some("A".to_string()),
            before: None,
            after: None,
        }];

        let count = inspect_tallying(source, &clauses);
        assert_eq!(count, 3);
    }

    #[test]
    fn test_inspect_tallying_characters() {
        let source = "HELLO";
        let clauses = vec![TallyingClause {
            mode: InspectMode::Characters,
            pattern: None,
            before: None,
            after: None,
        }];

        let count = inspect_tallying(source, &clauses);
        assert_eq!(count, 5);
    }

    #[test]
    fn test_inspect_replacing_all() {
        let source = "BANANA";
        let clauses = vec![ReplacingClause {
            mode: InspectMode::All,
            pattern: Some("A".to_string()),
            replacement: "X".to_string(),
            before: None,
            after: None,
        }];

        let result = inspect_replacing(source, &clauses);
        assert_eq!(result, "BXNXNX");
    }

    #[test]
    fn test_inspect_replacing_first() {
        let source = "BANANA";
        let clauses = vec![ReplacingClause {
            mode: InspectMode::First,
            pattern: Some("A".to_string()),
            replacement: "X".to_string(),
            before: None,
            after: None,
        }];

        let result = inspect_replacing(source, &clauses);
        assert_eq!(result, "BXNANA");
    }

    #[test]
    fn test_inspect_converting() {
        let source = "ABC123";
        let result = inspect_converting(source, "ABC", "XYZ", None, None);
        assert_eq!(result, "XYZ123");
    }

    #[test]
    fn test_inspect_with_before() {
        let source = "AABAA";
        let clauses = vec![TallyingClause {
            mode: InspectMode::All,
            pattern: Some("A".to_string()),
            before: Some("B".to_string()),
            after: None,
        }];

        let count = inspect_tallying(source, &clauses);
        assert_eq!(count, 2); // Only count A's before B
    }

    #[test]
    fn test_inspect_with_after() {
        let source = "AABAA";
        let clauses = vec![TallyingClause {
            mode: InspectMode::All,
            pattern: Some("A".to_string()),
            before: None,
            after: Some("B".to_string()),
        }];

        let count = inspect_tallying(source, &clauses);
        assert_eq!(count, 2); // Only count A's after B
    }
}
