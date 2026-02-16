//! Code page registry for CCSID and name-based lookup.

use super::extended_tables::*;
use super::tables::{CodePage, CP037, CP1047, CP500};
use crate::error::EncodingError;

/// All registered code pages in the system.
const ALL_CODE_PAGES: &[&CodePage] = &[
    // Base code pages
    &CP037, &CP500, &CP1047,
    // National code pages
    &CP273, &CP277, &CP278, &CP280, &CP284, &CP285, &CP297, &CP871,
    // Euro-enabled code pages
    &CP1140, &CP1141, &CP1142, &CP1143, &CP1144, &CP1145, &CP1146, &CP1147, &CP1148, &CP1149,
];

/// Registry for looking up EBCDIC code pages by CCSID number or name.
///
/// The registry contains all built-in code pages. Lookup is performed
/// by linear search over the static table (21 entries), which is
/// efficient for this small set.
pub struct CodePageRegistry;

impl CodePageRegistry {
    /// Look up a code page by CCSID number.
    ///
    /// # Examples
    /// ```
    /// use open_mainframe_encoding::ebcdic::CodePageRegistry;
    /// let cp = CodePageRegistry::from_ccsid(37).unwrap();
    /// assert_eq!(cp.name, "CP037");
    /// ```
    ///
    /// # Errors
    /// Returns `EncodingError::InvalidCodePage` if the CCSID is not recognized.
    pub fn from_ccsid(ccsid: u16) -> Result<&'static CodePage, EncodingError> {
        ALL_CODE_PAGES
            .iter()
            .find(|cp| cp.ccsid == ccsid)
            .copied()
            .ok_or_else(|| EncodingError::InvalidCodePage(format!("CCSID {}", ccsid)))
    }

    /// Look up a code page by name.
    ///
    /// Accepts names in multiple formats:
    /// - `"CP037"`, `"CP1047"` (standard format)
    /// - `"IBM-037"`, `"IBM-1047"` (IBM format with dash)
    /// - `"IBM037"`, `"IBM1047"` (IBM format without dash)
    /// - `"EBCDIC-037"` (EBCDIC prefix format)
    ///
    /// Name matching is case-insensitive.
    ///
    /// # Examples
    /// ```
    /// use open_mainframe_encoding::ebcdic::CodePageRegistry;
    /// let cp = CodePageRegistry::by_name("IBM-1047").unwrap();
    /// assert_eq!(cp.ccsid, 1047);
    /// ```
    ///
    /// # Errors
    /// Returns `EncodingError::InvalidCodePage` if the name is not recognized.
    pub fn by_name(name: &str) -> Result<&'static CodePage, EncodingError> {
        let upper = name.to_uppercase();

        // Extract the numeric part from various prefixes
        let num_str = if let Some(rest) = upper.strip_prefix("CP") {
            rest
        } else if let Some(rest) = upper.strip_prefix("IBM-") {
            rest
        } else if let Some(rest) = upper.strip_prefix("IBM") {
            rest
        } else if let Some(rest) = upper.strip_prefix("EBCDIC-") {
            rest
        } else {
            &upper
        };

        if let Ok(ccsid) = num_str.parse::<u16>() {
            Self::from_ccsid(ccsid)
        } else {
            Err(EncodingError::InvalidCodePage(name.to_string()))
        }
    }

    /// Returns a slice of all registered code pages.
    pub fn all() -> &'static [&'static CodePage] {
        ALL_CODE_PAGES
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_ccsid_base_pages() {
        let cp = CodePageRegistry::from_ccsid(37).unwrap();
        assert_eq!(cp.name, "CP037");
        assert_eq!(cp.ccsid, 37);

        let cp = CodePageRegistry::from_ccsid(1047).unwrap();
        assert_eq!(cp.name, "CP1047");

        let cp = CodePageRegistry::from_ccsid(500).unwrap();
        assert_eq!(cp.name, "CP500");
    }

    #[test]
    fn test_from_ccsid_national_pages() {
        let cp = CodePageRegistry::from_ccsid(273).unwrap();
        assert_eq!(cp.name, "CP273");

        let cp = CodePageRegistry::from_ccsid(277).unwrap();
        assert_eq!(cp.name, "CP277");

        let cp = CodePageRegistry::from_ccsid(871).unwrap();
        assert_eq!(cp.name, "CP871");
    }

    #[test]
    fn test_from_ccsid_euro_pages() {
        for ccsid in 1140..=1149 {
            let cp = CodePageRegistry::from_ccsid(ccsid).unwrap();
            assert_eq!(cp.ccsid, ccsid);
            assert!(!cp.special_chars.is_empty(), "Euro page CP{} should have special_chars", ccsid);
            assert_eq!(cp.special_chars[0], (0x9F, '€'));
        }
    }

    #[test]
    fn test_from_ccsid_unknown() {
        let err = CodePageRegistry::from_ccsid(65535).unwrap_err();
        assert!(matches!(err, EncodingError::InvalidCodePage(_)));
    }

    #[test]
    fn test_by_name_cp_format() {
        let cp = CodePageRegistry::by_name("CP037").unwrap();
        assert_eq!(cp.ccsid, 37);

        let cp = CodePageRegistry::by_name("CP1047").unwrap();
        assert_eq!(cp.ccsid, 1047);
    }

    #[test]
    fn test_by_name_ibm_format() {
        let cp = CodePageRegistry::by_name("IBM-1047").unwrap();
        assert_eq!(cp.ccsid, 1047);

        let cp = CodePageRegistry::by_name("IBM037").unwrap();
        assert_eq!(cp.ccsid, 37);
    }

    #[test]
    fn test_by_name_ebcdic_format() {
        let cp = CodePageRegistry::by_name("EBCDIC-500").unwrap();
        assert_eq!(cp.ccsid, 500);
    }

    #[test]
    fn test_by_name_case_insensitive() {
        let cp = CodePageRegistry::by_name("cp037").unwrap();
        assert_eq!(cp.ccsid, 37);

        let cp = CodePageRegistry::by_name("ibm-1047").unwrap();
        assert_eq!(cp.ccsid, 1047);
    }

    #[test]
    fn test_by_name_unknown() {
        let err = CodePageRegistry::by_name("CP65535").unwrap_err();
        assert!(matches!(err, EncodingError::InvalidCodePage(_)));
    }

    #[test]
    fn test_all_code_pages() {
        let all = CodePageRegistry::all();
        assert_eq!(all.len(), 21); // 3 base + 8 national + 10 euro
    }
}
