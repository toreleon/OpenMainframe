//! File and directory scanning for batch COBOL assessment.
//!
//! Discovers COBOL source files in a directory tree, resolves COPY copybook
//! includes from configurable search paths, and runs analysis on each file.
//! Results are aggregated into a single [`Report`].

use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use crate::analyzer::AnalysisResult;
use crate::ast_analyzer::AstAnalyzer;
use crate::report::Report;
use crate::{AssessError, AssessResult};

/// Default COBOL source file extensions.
const DEFAULT_EXTENSIONS: &[&str] = &["cbl", "cob", "CBL", "COB"];

/// Default copybook extensions.
const COPYBOOK_EXTENSIONS: &[&str] = &["cpy", "CPY", "copy", "COPY"];

/// Configuration for directory scanning.
#[derive(Debug, Clone)]
pub struct ScanConfig {
    /// Root directory to scan.
    pub root: PathBuf,
    /// Optional glob-style patterns to include (e.g., `*.cbl`).
    /// Only files matching at least one pattern are included.
    pub include_patterns: Vec<String>,
    /// File extensions to treat as COBOL source (default: cbl, cob).
    pub source_extensions: Vec<String>,
    /// Include paths for copybook resolution.
    pub copybook_paths: Vec<PathBuf>,
    /// Whether to recurse into subdirectories.
    pub recursive: bool,
}

impl ScanConfig {
    /// Create a new scan configuration for the given root directory.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self {
            root: root.into(),
            include_patterns: Vec::new(),
            source_extensions: DEFAULT_EXTENSIONS.iter().map(|s| s.to_string()).collect(),
            copybook_paths: Vec::new(),
            recursive: true,
        }
    }

    /// Add an include glob pattern (e.g., `src/**/*.cbl`).
    pub fn with_pattern(mut self, pattern: &str) -> Self {
        self.include_patterns.push(pattern.to_string());
        self
    }

    /// Add a copybook search path.
    pub fn with_copybook_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.copybook_paths.push(path.into());
        self
    }

    /// Set whether to recurse into subdirectories.
    pub fn with_recursive(mut self, recursive: bool) -> Self {
        self.recursive = recursive;
        self
    }
}

/// Batch COBOL codebase scanner.
///
/// Discovers COBOL files in a directory tree, optionally resolves copybook
/// COPY statements, and produces a combined assessment report.
pub struct Scanner {
    config: ScanConfig,
    analyzer: AstAnalyzer,
}

/// Result of scanning a single file.
#[derive(Debug)]
pub struct ScanEntry {
    /// Relative path from the scan root.
    pub relative_path: String,
    /// Absolute path to the file.
    pub absolute_path: PathBuf,
}

/// Aggregated scan results.
#[derive(Debug)]
pub struct ScanResult {
    /// Number of files discovered.
    pub files_discovered: usize,
    /// Number of files successfully analyzed.
    pub files_analyzed: usize,
    /// Number of files that failed analysis.
    pub files_failed: usize,
    /// Warnings from scanning/analysis.
    pub warnings: Vec<String>,
    /// The generated report.
    pub report: Report,
}

impl Scanner {
    /// Create a new scanner with the given configuration.
    pub fn new(config: ScanConfig) -> Self {
        Self {
            config,
            analyzer: AstAnalyzer::new(),
        }
    }

    /// Scan the configured directory and produce an assessment report.
    pub fn scan(&self) -> AssessResult<ScanResult> {
        let root = &self.config.root;
        if !root.exists() {
            return Err(AssessError::FileNotFound(
                root.to_string_lossy().to_string(),
            ));
        }

        // Discover files
        let entries = self.discover_files()?;
        let files_discovered = entries.len();

        let mut results: Vec<AnalysisResult> = Vec::new();
        let mut warnings: Vec<String> = Vec::new();
        let mut files_failed = 0;

        for entry in &entries {
            match self.analyze_file(entry) {
                Ok(result) => results.push(result),
                Err(e) => {
                    warnings.push(format!(
                        "Failed to analyze {}: {}",
                        entry.relative_path, e
                    ));
                    files_failed += 1;
                }
            }
        }

        let files_analyzed = results.len();
        let report = Report::new("Migration Assessment Report", results);

        Ok(ScanResult {
            files_discovered,
            files_analyzed,
            files_failed,
            warnings,
            report,
        })
    }

    /// Discover COBOL source files in the configured directory.
    pub fn discover_files(&self) -> AssessResult<Vec<ScanEntry>> {
        let mut entries = Vec::new();
        self.walk_dir(&self.config.root, &mut entries)?;

        // Apply include patterns if specified
        if !self.config.include_patterns.is_empty() {
            entries.retain(|e| {
                self.config
                    .include_patterns
                    .iter()
                    .any(|p| matches_glob_pattern(p, &e.relative_path))
            });
        }

        entries.sort_by(|a, b| a.relative_path.cmp(&b.relative_path));
        Ok(entries)
    }

    /// Recursively walk a directory collecting COBOL source files.
    fn walk_dir(&self, dir: &Path, entries: &mut Vec<ScanEntry>) -> AssessResult<()> {
        let read_dir = fs::read_dir(dir).map_err(|e| {
            AssessError::Io(std::io::Error::new(
                e.kind(),
                format!("{}: {}", dir.display(), e),
            ))
        })?;

        for entry in read_dir {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                if self.config.recursive {
                    self.walk_dir(&path, entries)?;
                }
            } else if self.is_cobol_source(&path) {
                let relative = path
                    .strip_prefix(&self.config.root)
                    .unwrap_or(&path)
                    .to_string_lossy()
                    .to_string();

                entries.push(ScanEntry {
                    relative_path: relative,
                    absolute_path: path,
                });
            }
        }

        Ok(())
    }

    /// Check if a file has a COBOL source extension.
    fn is_cobol_source(&self, path: &Path) -> bool {
        path.extension()
            .and_then(|e| e.to_str())
            .map(|ext| {
                self.config
                    .source_extensions
                    .iter()
                    .any(|se| se == ext)
            })
            .unwrap_or(false)
    }

    /// Analyze a single discovered file.
    fn analyze_file(&self, entry: &ScanEntry) -> AssessResult<AnalysisResult> {
        let source = fs::read_to_string(&entry.absolute_path).map_err(|e| {
            AssessError::Io(std::io::Error::new(
                e.kind(),
                format!("{}: {}", entry.relative_path, e),
            ))
        })?;

        // Resolve copybooks (inline COPY expansions)
        let source = self.resolve_copybooks(&source);

        self.analyzer.analyze(&source, &entry.relative_path)
    }

    /// Resolve COPY statements by inlining copybook content.
    ///
    /// Searches for `COPY <name>.` statements and replaces them with
    /// the contents of the matching copybook file from the configured
    /// include paths. Missing copybooks are left as-is with a comment.
    fn resolve_copybooks(&self, source: &str) -> String {
        if self.config.copybook_paths.is_empty() {
            return source.to_string();
        }

        let mut result = String::with_capacity(source.len());
        let mut resolved: HashSet<String> = HashSet::new();

        for line in source.lines() {
            let trimmed = line.trim().to_uppercase();
            if trimmed.starts_with("COPY ") {
                // Extract copybook name: COPY <name>.
                let name = trimmed
                    .trim_start_matches("COPY ")
                    .trim_end_matches('.')
                    .trim()
                    .to_string();

                if !name.is_empty() && !resolved.contains(&name) {
                    if let Some(content) = self.find_copybook(&name) {
                        resolved.insert(name);
                        result.push_str(&content);
                        result.push('\n');
                        continue;
                    }
                }
            }
            result.push_str(line);
            result.push('\n');
        }

        result
    }

    /// Search include paths for a copybook file.
    fn find_copybook(&self, name: &str) -> Option<String> {
        for include_path in &self.config.copybook_paths {
            // Try with each copybook extension
            for ext in COPYBOOK_EXTENSIONS {
                let path = include_path.join(format!("{}.{}", name, ext));
                if path.exists() {
                    return fs::read_to_string(&path).ok();
                }
            }

            // Try with source extensions
            for ext in DEFAULT_EXTENSIONS {
                let path = include_path.join(format!("{}.{}", name, ext));
                if path.exists() {
                    return fs::read_to_string(&path).ok();
                }
            }

            // Try without extension
            let path = include_path.join(name);
            if path.exists() {
                return fs::read_to_string(&path).ok();
            }
        }

        None
    }
}

/// Simple glob pattern matching.
///
/// Supports `*` (matches any characters in a single path segment) and
/// `**` (matches any number of path segments). Case-insensitive.
fn matches_glob_pattern(pattern: &str, path: &str) -> bool {
    let pattern = pattern.replace('\\', "/");
    let path = path.replace('\\', "/");

    // Simple extension-only pattern like `*.cbl`
    if pattern.starts_with("*.") && !pattern.contains('/') {
        let ext = &pattern[1..]; // ".cbl"
        return path.ends_with(ext);
    }

    // Pattern with ** wildcard
    if pattern.contains("**") {
        let parts: Vec<&str> = pattern.split("**").collect();
        if parts.len() == 2 {
            let prefix = parts[0].trim_end_matches('/');
            let suffix = parts[1].trim_start_matches('/');

            let prefix_ok = prefix.is_empty() || path.starts_with(prefix);
            let suffix_ok = suffix.is_empty() || {
                // suffix is like "*.cbl"
                if suffix.starts_with("*.") {
                    let ext = &suffix[1..];
                    path.ends_with(ext)
                } else {
                    path.ends_with(suffix)
                }
            };

            return prefix_ok && suffix_ok;
        }
    }

    // Exact match (case-insensitive)
    pattern.eq_ignore_ascii_case(&path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static TEST_COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn create_test_dir() -> PathBuf {
        let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        let dir = std::env::temp_dir().join(format!(
            "assess_scanner_test_{}_{}",
            std::process::id(),
            id
        ));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn cleanup_test_dir(dir: &Path) {
        let _ = fs::remove_dir_all(dir);
    }

    fn write_cobol(dir: &Path, name: &str, program_id: &str) {
        let content = format!(
            "       IDENTIFICATION DIVISION.\n\
             \x20      PROGRAM-ID. {}.\n\
             \x20      PROCEDURE DIVISION.\n\
             \x20      MAIN-PARA.\n\
             \x20          DISPLAY \"HELLO\".\n\
             \x20          STOP RUN.\n",
            program_id
        );
        fs::write(dir.join(name), content).unwrap();
    }

    #[test]
    fn test_discover_cobol_files() {
        let dir = create_test_dir();
        write_cobol(&dir, "PROG1.cbl", "PROG1");
        write_cobol(&dir, "PROG2.cob", "PROG2");
        fs::write(dir.join("readme.txt"), "not cobol").unwrap();

        let config = ScanConfig::new(&dir);
        let scanner = Scanner::new(config);
        let entries = scanner.discover_files().unwrap();

        assert_eq!(entries.len(), 2);
        let names: Vec<&str> = entries.iter().map(|e| e.relative_path.as_str()).collect();
        assert!(names.contains(&"PROG1.cbl"));
        assert!(names.contains(&"PROG2.cob"));

        cleanup_test_dir(&dir);
    }

    #[test]
    fn test_recursive_scanning() {
        let dir = create_test_dir();
        let sub = dir.join("sub");
        fs::create_dir_all(&sub).unwrap();
        write_cobol(&dir, "ROOT.cbl", "ROOT");
        write_cobol(&sub, "SUB.cbl", "SUB");

        let config = ScanConfig::new(&dir);
        let scanner = Scanner::new(config);
        let entries = scanner.discover_files().unwrap();

        assert_eq!(entries.len(), 2);

        cleanup_test_dir(&dir);
    }

    #[test]
    fn test_non_recursive_scanning() {
        let dir = create_test_dir();
        let sub = dir.join("sub");
        fs::create_dir_all(&sub).unwrap();
        write_cobol(&dir, "ROOT.cbl", "ROOT");
        write_cobol(&sub, "SUB.cbl", "SUB");

        let config = ScanConfig::new(&dir).with_recursive(false);
        let scanner = Scanner::new(config);
        let entries = scanner.discover_files().unwrap();

        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].relative_path, "ROOT.cbl");

        cleanup_test_dir(&dir);
    }

    #[test]
    fn test_glob_pattern_filter() {
        let dir = create_test_dir();
        let src = dir.join("src");
        let lib = dir.join("lib");
        fs::create_dir_all(&src).unwrap();
        fs::create_dir_all(&lib).unwrap();
        write_cobol(&src, "PROG.cbl", "PROG");
        write_cobol(&lib, "LIB.cbl", "LIB");

        let config = ScanConfig::new(&dir).with_pattern("src/**/*.cbl");
        let scanner = Scanner::new(config);
        let entries = scanner.discover_files().unwrap();

        assert_eq!(entries.len(), 1);
        assert!(entries[0].relative_path.contains("PROG.cbl"));

        cleanup_test_dir(&dir);
    }

    #[test]
    fn test_full_scan() {
        let dir = create_test_dir();
        write_cobol(&dir, "PROG1.cbl", "PROG1");
        write_cobol(&dir, "PROG2.cbl", "PROG2");

        let config = ScanConfig::new(&dir);
        let scanner = Scanner::new(config);
        let result = scanner.scan().unwrap();

        assert_eq!(result.files_discovered, 2);
        assert_eq!(result.files_analyzed, 2);
        assert_eq!(result.files_failed, 0);
        assert_eq!(result.report.results.len(), 2);

        cleanup_test_dir(&dir);
    }

    #[test]
    fn test_scan_nonexistent_directory() {
        let config = ScanConfig::new("/tmp/does_not_exist_assess_test");
        let scanner = Scanner::new(config);
        let result = scanner.scan();

        assert!(result.is_err());
    }

    #[test]
    fn test_copybook_resolution() {
        let dir = create_test_dir();
        let copybooks = dir.join("copybooks");
        fs::create_dir_all(&copybooks).unwrap();

        // Create a copybook
        fs::write(
            copybooks.join("CUSTOMER-REC.cpy"),
            "       01  WS-CUSTOMER-NAME  PIC X(30).\n",
        )
        .unwrap();

        // Create a COBOL source that uses COPY
        let source = "\
       IDENTIFICATION DIVISION.\n\
       PROGRAM-ID. COPYPROG.\n\
       DATA DIVISION.\n\
       WORKING-STORAGE SECTION.\n\
           COPY CUSTOMER-REC.\n\
       PROCEDURE DIVISION.\n\
       MAIN-PARA.\n\
           DISPLAY \"HELLO\".\n\
           STOP RUN.\n";
        fs::write(dir.join("COPYPROG.cbl"), source).unwrap();

        let config = ScanConfig::new(&dir).with_copybook_path(&copybooks);
        let scanner = Scanner::new(config);
        let result = scanner.scan().unwrap();

        assert_eq!(result.files_analyzed, 1);
        assert_eq!(result.files_failed, 0);

        cleanup_test_dir(&dir);
    }

    #[test]
    fn test_missing_copybook_continues() {
        let dir = create_test_dir();
        let copybooks = dir.join("copybooks");
        fs::create_dir_all(&copybooks).unwrap();

        // COBOL source with a COPY for a non-existent copybook
        let source = "\
       IDENTIFICATION DIVISION.\n\
       PROGRAM-ID. MISSING.\n\
       DATA DIVISION.\n\
       WORKING-STORAGE SECTION.\n\
           COPY NONEXISTENT.\n\
       PROCEDURE DIVISION.\n\
       MAIN-PARA.\n\
           DISPLAY \"HELLO\".\n\
           STOP RUN.\n";
        fs::write(dir.join("MISSING.cbl"), source).unwrap();

        let config = ScanConfig::new(&dir).with_copybook_path(&copybooks);
        let scanner = Scanner::new(config);
        let result = scanner.scan().unwrap();

        // Should still analyze (COPY line left as-is)
        assert_eq!(result.files_analyzed, 1);

        cleanup_test_dir(&dir);
    }

    #[test]
    fn test_glob_matches() {
        assert!(matches_glob_pattern("*.cbl", "PROG.cbl"));
        assert!(!matches_glob_pattern("*.cbl", "PROG.cob"));
        assert!(matches_glob_pattern("src/**/*.cbl", "src/main/PROG.cbl"));
        assert!(matches_glob_pattern("**/*.cbl", "any/path/PROG.cbl"));
        assert!(!matches_glob_pattern("src/**/*.cbl", "lib/PROG.cbl"));
    }

    #[test]
    fn test_scan_with_analysis_results() {
        let dir = create_test_dir();

        // Write a program with DB2
        let db2_source = "\
       IDENTIFICATION DIVISION.\n\
       PROGRAM-ID. DB2PROG.\n\
       PROCEDURE DIVISION.\n\
       MAIN-PARA.\n\
           EXEC SQL SELECT * FROM TABLE END-EXEC.\n\
           STOP RUN.\n";
        fs::write(dir.join("DB2PROG.cbl"), db2_source).unwrap();

        let config = ScanConfig::new(&dir);
        let scanner = Scanner::new(config);
        let result = scanner.scan().unwrap();

        assert_eq!(result.files_analyzed, 1);
        let analysis = &result.report.results[0];
        assert!(analysis.features.iter().any(|f| f.name == "DB2"));

        cleanup_test_dir(&dir);
    }
}
