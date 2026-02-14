//! Copybook (COPY statement) resolution.
//!
//! This module handles finding and including copybook files during
//! lexical analysis. It supports:
//! - Multiple copybook search paths
//! - Nested copybook inclusion
//! - Circular inclusion detection
//! - REPLACING clause text substitution

use std::path::PathBuf;

use crate::error::CobolError;
use crate::lexer::source::{SourceFile, SourceFormat};
use crate::lexer::span::FileId;

/// Result type for copybook operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// Configuration for copybook resolution.
#[derive(Debug, Clone, Default)]
pub struct CopybookConfig {
    /// Search paths for copybook files (searched in order).
    pub search_paths: Vec<PathBuf>,
    /// File extensions to try when resolving copybook names.
    pub extensions: Vec<String>,
}

impl CopybookConfig {
    /// Create a new copybook configuration with default extensions.
    pub fn new() -> Self {
        Self {
            search_paths: Vec::new(),
            extensions: vec![
                String::new(), // Try exact name first
                ".cpy".to_string(),
                ".cbl".to_string(),
                ".cob".to_string(),
                ".CPY".to_string(),
                ".CBL".to_string(),
                ".COB".to_string(),
            ],
        }
    }

    /// Add a search path.
    pub fn add_path(&mut self, path: impl Into<PathBuf>) {
        self.search_paths.push(path.into());
    }

    /// Add multiple search paths.
    pub fn add_paths(&mut self, paths: impl IntoIterator<Item = impl Into<PathBuf>>) {
        for path in paths {
            self.search_paths.push(path.into());
        }
    }
}

/// Copybook resolver.
///
/// Manages the resolution of COPY statements to actual files,
/// tracking inclusion to detect circular dependencies.
#[derive(Debug)]
pub struct CopybookResolver {
    /// Configuration for resolution.
    config: CopybookConfig,
    /// Source format to use when loading copybooks.
    format: SourceFormat,
    /// Stack of currently included copybooks (for circular detection).
    include_stack: Vec<String>,
    /// All resolved copybooks (name -> path).
    resolved: std::collections::HashMap<String, PathBuf>,
}

impl CopybookResolver {
    /// Create a new copybook resolver.
    pub fn new(config: CopybookConfig, format: SourceFormat) -> Self {
        Self {
            config,
            format,
            include_stack: Vec::new(),
            resolved: std::collections::HashMap::new(),
        }
    }

    /// Resolve a copybook name to a file path.
    ///
    /// Searches the configured paths for a file matching the copybook name.
    pub fn resolve(&mut self, name: &str) -> Result<PathBuf> {
        // Check cache first
        if let Some(path) = self.resolved.get(name) {
            return Ok(path.clone());
        }

        // Check for circular inclusion
        if self.include_stack.contains(&name.to_uppercase()) {
            let mut chain = self.include_stack.clone();
            chain.push(name.to_uppercase());
            return Err(CobolError::CircularCopybook {
                chain: chain.join(" -> "),
            });
        }

        // Search for the copybook
        let path = self.find_copybook(name)?;

        // Cache and return
        self.resolved.insert(name.to_uppercase(), path.clone());
        Ok(path)
    }

    /// Find a copybook file.
    fn find_copybook(&self, name: &str) -> Result<PathBuf> {
        let mut searched_paths = Vec::new();

        for search_path in &self.config.search_paths {
            for ext in &self.config.extensions {
                let file_name = if ext.is_empty() {
                    name.to_string()
                } else {
                    format!("{}{}", name, ext)
                };

                let full_path = search_path.join(&file_name);
                searched_paths.push(full_path.display().to_string());

                if full_path.exists() && full_path.is_file() {
                    return Ok(full_path);
                }
            }
        }

        // Also try current directory if not in search paths
        for ext in &self.config.extensions {
            let file_name = if ext.is_empty() {
                name.to_string()
            } else {
                format!("{}{}", name, ext)
            };

            let path = PathBuf::from(&file_name);
            searched_paths.push(path.display().to_string());

            if path.exists() && path.is_file() {
                return Ok(path);
            }
        }

        Err(CobolError::CopybookNotFound {
            name: name.to_string(),
            searched_paths: searched_paths.join(", "),
        })
    }

    /// Load a copybook file.
    ///
    /// Loads the copybook content and returns a SourceFile.
    /// Updates the include stack for circular detection.
    pub fn load(&mut self, name: &str, file_id: FileId) -> Result<SourceFile> {
        let path = self.resolve(name)?;

        // Push onto include stack
        self.include_stack.push(name.to_uppercase());

        let result = SourceFile::from_path(file_id, &path, self.format);

        // Pop from include stack (regardless of result)
        self.include_stack.pop();

        result
    }

    /// Push a copybook onto the include stack (for external tracking).
    pub fn push_include(&mut self, name: &str) -> Result<()> {
        let upper = name.to_uppercase();
        if self.include_stack.contains(&upper) {
            let mut chain = self.include_stack.clone();
            chain.push(upper);
            return Err(CobolError::CircularCopybook {
                chain: chain.join(" -> "),
            });
        }
        self.include_stack.push(upper);
        Ok(())
    }

    /// Pop a copybook from the include stack.
    pub fn pop_include(&mut self) {
        self.include_stack.pop();
    }

    /// Get the current include depth.
    pub fn include_depth(&self) -> usize {
        self.include_stack.len()
    }
}

/// A REPLACING clause specification.
#[derive(Debug, Clone)]
pub struct Replacement {
    /// The text to be replaced (pattern).
    pub from: String,
    /// The replacement text.
    pub to: String,
}

impl Replacement {
    /// Create a new replacement.
    pub fn new(from: impl Into<String>, to: impl Into<String>) -> Self {
        Self {
            from: from.into(),
            to: to.into(),
        }
    }
}

/// Apply REPLACING substitutions to text.
///
/// Handles the COBOL REPLACING syntax where patterns are delimited by ==.
pub fn apply_replacements(text: &str, replacements: &[Replacement]) -> String {
    let mut result = text.to_string();

    for replacement in replacements {
        // Handle both forms:
        // 1. ==:PREFIX:== BY ==WS-==
        // 2. Simple word replacement
        let from = replacement
            .from
            .trim_start_matches("==")
            .trim_end_matches("==");
        let to = replacement
            .to
            .trim_start_matches("==")
            .trim_end_matches("==");

        result = result.replace(from, to);
    }

    result
}

/// Parse REPLACING clause from tokens.
///
/// Expected format: REPLACING ==pattern1== BY ==replacement1== ==pattern2== BY ==replacement2==...
pub fn parse_replacing_clause(text: &str) -> Vec<Replacement> {
    let mut replacements = Vec::new();

    // Split by "BY" (case insensitive)
    let parts: Vec<&str> = text.split_whitespace().collect();
    let mut i = 0;

    while i < parts.len() {
        // Look for pattern
        if parts[i].starts_with("==") || parts[i].starts_with(":") {
            // Collect the "from" pattern
            let mut from = String::new();
            while i < parts.len() && !parts[i].eq_ignore_ascii_case("BY") {
                if !from.is_empty() {
                    from.push(' ');
                }
                from.push_str(parts[i]);
                i += 1;
            }

            // Skip "BY"
            if i < parts.len() && parts[i].eq_ignore_ascii_case("BY") {
                i += 1;
            }

            // Collect the "to" pattern
            let mut to = String::new();
            while i < parts.len()
                && !parts[i].starts_with("==")
                && !parts[i].starts_with(":")
                && !parts[i].eq_ignore_ascii_case("REPLACING")
            {
                if !to.is_empty() {
                    to.push(' ');
                }
                to.push_str(parts[i]);
                i += 1;
            }

            if !from.is_empty() {
                replacements.push(Replacement::new(from, to));
            }
        } else {
            i += 1;
        }
    }

    replacements
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copybook_config() {
        let mut config = CopybookConfig::new();
        config.add_path("/path/to/copybooks");
        assert_eq!(config.search_paths.len(), 1);
    }

    #[test]
    fn test_copybook_not_found() {
        let config = CopybookConfig::new();
        let resolver = CopybookResolver::new(config, SourceFormat::Free);
        let result = resolver.find_copybook("NONEXISTENT_XYZ_COPYBOOK");
        assert!(result.is_err());
        assert!(matches!(result, Err(CobolError::CopybookNotFound { .. })));
    }

    #[test]
    fn test_circular_detection() {
        let config = CopybookConfig::new();
        let mut resolver = CopybookResolver::new(config, SourceFormat::Free);

        resolver.push_include("A").unwrap();
        resolver.push_include("B").unwrap();

        let result = resolver.push_include("A");
        assert!(result.is_err());
        assert!(matches!(result, Err(CobolError::CircularCopybook { .. })));
    }

    #[test]
    fn test_include_depth() {
        let config = CopybookConfig::new();
        let mut resolver = CopybookResolver::new(config, SourceFormat::Free);

        assert_eq!(resolver.include_depth(), 0);
        resolver.push_include("A").unwrap();
        assert_eq!(resolver.include_depth(), 1);
        resolver.push_include("B").unwrap();
        assert_eq!(resolver.include_depth(), 2);
        resolver.pop_include();
        assert_eq!(resolver.include_depth(), 1);
    }

    #[test]
    fn test_apply_replacements() {
        let text = "01 :PREFIX:-FIELD PIC X.";
        let replacements = vec![Replacement::new(":PREFIX:", "WS")];
        let result = apply_replacements(text, &replacements);
        assert_eq!(result, "01 WS-FIELD PIC X.");
    }

    #[test]
    fn test_apply_replacements_with_delimiters() {
        let text = "01 :TAG:-NAME PIC X.";
        let replacements = vec![Replacement::new("==:TAG:==", "==CUST==")];
        let result = apply_replacements(text, &replacements);
        assert_eq!(result, "01 CUST-NAME PIC X.");
    }

    #[test]
    fn test_multiple_replacements() {
        let text = "01 :A:-:B:-:C:.";
        let replacements = vec![
            Replacement::new(":A:", "X"),
            Replacement::new(":B:", "Y"),
            Replacement::new(":C:", "Z"),
        ];
        let result = apply_replacements(text, &replacements);
        assert_eq!(result, "01 X-Y-Z.");
    }

    #[test]
    fn test_parse_replacing_clause() {
        // Simple replacing clause parsing test
        let clause = "==:PREFIX:== BY ==WS-==";
        let replacements = parse_replacing_clause(clause);
        assert!(!replacements.is_empty());
        // Verify first replacement captures the pattern
        assert!(replacements[0].from.contains(":PREFIX:"));
    }
}
