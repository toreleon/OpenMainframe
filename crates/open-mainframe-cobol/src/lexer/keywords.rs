//! Keyword recognition for COBOL reserved words.
//!
//! COBOL has many reserved words. This module provides efficient
//! lookup to distinguish keywords from user-defined identifiers.
//!
//! The keyword map is generated from the master keyword table in `macros.rs`.
//! To add a new keyword, add one line there — the HashMap entry is generated
//! automatically.

use crate::lexer::token::Keyword;
use std::collections::HashMap;
use std::sync::LazyLock;

/// Map of uppercase keyword strings to Keyword enum values.
///
/// Only `@primary` keywords from `for_all_keywords!` are included.
/// `@alias` keywords are contextual and constructed by the parser.
macro_rules! gen_keyword_map {
    (
        @primary { $($pvar:ident => $pstr:literal),* $(,)? }
        @alias   { $($avar:ident => $astr:literal),* $(,)? }
    ) => {
        static KEYWORDS: LazyLock<HashMap<&'static str, Keyword>> = LazyLock::new(|| {
            let mut map = HashMap::new();
            $(map.insert($pstr, Keyword::$pvar);)*
            map
        });
    };
}
for_all_keywords!(gen_keyword_map);

/// Look up a word to see if it's a reserved keyword.
///
/// The input is normalized to uppercase before lookup.
pub fn lookup_keyword(word: &str) -> Option<Keyword> {
    KEYWORDS.get(word.to_uppercase().as_str()).copied()
}

/// Check if a word is a reserved keyword.
pub fn is_keyword(word: &str) -> bool {
    lookup_keyword(word).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_lookup() {
        assert_eq!(
            lookup_keyword("IDENTIFICATION"),
            Some(Keyword::Identification)
        );
        assert_eq!(
            lookup_keyword("identification"),
            Some(Keyword::Identification)
        );
        assert_eq!(
            lookup_keyword("Identification"),
            Some(Keyword::Identification)
        );
        assert_eq!(lookup_keyword("PROGRAM-ID"), Some(Keyword::ProgramId));
        assert_eq!(lookup_keyword("DISPLAY"), Some(Keyword::Display));
    }

    #[test]
    fn test_non_keyword() {
        assert_eq!(lookup_keyword("MY-VARIABLE"), None);
        assert_eq!(lookup_keyword("CUSTOMER-NAME"), None);
    }

    #[test]
    fn test_is_keyword() {
        assert!(is_keyword("MOVE"));
        assert!(is_keyword("move"));
        assert!(!is_keyword("CUSTOMER"));
    }
}
