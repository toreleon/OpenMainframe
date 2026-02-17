//! AST-based COBOL analysis.
//!
//! Provides accurate feature detection and metrics by parsing COBOL source
//! into an AST and walking the tree, rather than relying on text pattern matching.
//! Falls back to text-based analysis for unparseable source.

use std::collections::{HashMap, HashSet};

use open_mainframe_cobol::ast::{
    ProcedureBody, Program, Statement,
};
use open_mainframe_cobol::lexer::{SourceFile, SourceFormat, FileId};
use open_mainframe_cobol::{scan, Parser};

use crate::analyzer::{AnalysisResult, Feature};
use crate::compatibility::CompatibilityChecker;
use crate::metrics::CodeMetrics;
use crate::{AssessResult, FeatureCategory, MigrationComplexity};

/// AST-based COBOL analyzer.
///
/// Uses the `open-mainframe-cobol` parser to build an AST from source code,
/// then walks the AST to extract features, metrics, and compatibility issues.
/// Falls back to text-based analysis for unparseable code with a warning.
pub struct AstAnalyzer {
    compatibility: CompatibilityChecker,
}

impl AstAnalyzer {
    /// Create a new AST-based analyzer.
    pub fn new() -> Self {
        Self {
            compatibility: CompatibilityChecker::new(),
        }
    }

    /// Analyze COBOL source using AST-based analysis.
    ///
    /// Attempts to parse the source into an AST. If parsing succeeds,
    /// walks the AST for accurate feature/metric extraction. If parsing
    /// fails, falls back to text-based analysis with a warning.
    pub fn analyze(&self, source: &str, file_name: &str) -> AssessResult<AnalysisResult> {
        // Try to parse the source
        let source_file = SourceFile::from_text(
            FileId::MAIN,
            source.to_string(),
            SourceFormat::Free,
        );
        let (tokens, _scan_errors) = scan(&source_file);
        let parser = Parser::new(tokens);
        let (program, parse_errors) = parser.parse_program();

        match program {
            Some(ref prog) => {
                let mut result = self.analyze_ast(prog, source, file_name);

                // Add parse error warnings
                if !parse_errors.is_empty() {
                    result.recommendations.push(format!(
                        "Warning: {} parse error(s) encountered — partial AST analysis",
                        parse_errors.len()
                    ));
                }

                Ok(result)
            }
            None => {
                // Fall back to text-based analysis
                let text_analyzer = crate::analyzer::Analyzer::new();
                let mut result = text_analyzer.analyze(source, file_name)?;

                result.recommendations.insert(
                    0,
                    format!(
                        "Warning: COBOL parsing failed ({} error(s)) — using text-based analysis",
                        parse_errors.len()
                    ),
                );

                Ok(result)
            }
        }
    }

    /// Analyze a parsed COBOL AST.
    fn analyze_ast(
        &self,
        program: &Program,
        source: &str,
        file_name: &str,
    ) -> AnalysisResult {
        // Extract program ID
        let program_id = Some(program.identification.program_id.name.clone());

        // Calculate metrics from AST
        let lines: Vec<&str> = source.lines().collect();
        let mut metrics = self.calculate_ast_metrics(program, &lines);

        // Detect features from AST
        let features = self.detect_ast_features(program, source);

        // Check compatibility (still text-based — catches platform-specific patterns)
        let issues = self.compatibility.check(source);

        // Calculate complexity
        let complexity = self.calculate_complexity(&metrics, &features, &issues);

        // Generate recommendations
        let recommendations = self.generate_recommendations(&features, &issues);

        // Ensure basic line metrics are set
        if metrics.total_lines == 0 {
            metrics.total_lines = lines.len();
        }

        AnalysisResult {
            file_name: file_name.to_string(),
            program_id,
            metrics,
            features,
            issues,
            complexity,
            recommendations,
        }
    }

    /// Calculate code metrics from the AST.
    fn calculate_ast_metrics(&self, program: &Program, lines: &[&str]) -> CodeMetrics {
        let mut metrics = CodeMetrics::default();

        // Line counts from source
        metrics.total_lines = lines.len();
        for line in lines {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                metrics.blank_lines += 1;
            } else if trimmed.starts_with('*')
                || (line.len() > 6 && line.chars().nth(6) == Some('*'))
            {
                metrics.comment_lines += 1;
            } else {
                metrics.code_lines += 1;
            }
        }

        // Division presence
        metrics.has_identification = true; // always present if parsed
        metrics.has_environment = program.environment.is_some();
        metrics.has_data = program.data.is_some();
        metrics.has_procedure = program.procedure.is_some();

        // Data items from AST
        if let Some(ref data) = program.data {
            metrics.data_items = count_data_items(&data.working_storage)
                + count_data_items(&data.local_storage)
                + count_data_items(&data.linkage);
        }

        // Procedure metrics from AST
        if let Some(ref proc_div) = program.procedure {
            let statements = collect_all_statements(&proc_div.body);

            metrics.executable_statements = statements.len();

            // Count paragraphs
            match &proc_div.body {
                ProcedureBody::Sections(sections) => {
                    for section in sections {
                        metrics.paragraph_count += section.paragraphs.len();
                    }
                }
                ProcedureBody::Paragraphs(paragraphs) => {
                    metrics.paragraph_count = paragraphs.len();
                }
                ProcedureBody::Statements(_) => {
                    // No explicit paragraphs
                }
            }

            // Cyclomatic complexity from decision points in AST
            metrics.cyclomatic_complexity = 1; // base
            for stmt in &statements {
                match stmt {
                    Statement::If(_) => metrics.cyclomatic_complexity += 1,
                    Statement::Evaluate(ev) => {
                        // Each WHEN clause adds a decision path
                        metrics.cyclomatic_complexity += ev.when_clauses.len();
                    }
                    Statement::Perform(p) => {
                        if p.until.is_some() || p.varying.is_some() {
                            metrics.cyclomatic_complexity += 1;
                        }
                    }
                    Statement::Search(_) => metrics.cyclomatic_complexity += 1,
                    _ => {}
                }
            }
        }

        metrics
    }

    /// Detect features from the AST.
    fn detect_ast_features(&self, program: &Program, source: &str) -> HashSet<Feature> {
        let mut features: HashMap<String, Feature> = HashMap::new();

        // Check for file handling in environment division
        if let Some(ref env) = program.environment {
            if let Some(ref io) = env.input_output {
                for fc in &io.file_control {
                    let org_name = match fc.organization {
                        open_mainframe_cobol::ast::FileOrganization::Indexed => "VSAM",
                        open_mainframe_cobol::ast::FileOrganization::Sequential => "Sequential Files",
                        open_mainframe_cobol::ast::FileOrganization::Relative => "Relative Files",
                        _ => "Sequential Files",
                    };
                    let category = FeatureCategory::FileHandling;
                    let feature = features
                        .entry(org_name.to_string())
                        .or_insert_with(|| Feature::new(org_name, category));
                    feature.add_occurrence(0); // AST doesn't have line numbers per-SELECT
                }
            }
        }

        // Walk procedure division for statement-based features
        if let Some(ref proc_div) = program.procedure {
            let statements = collect_all_statements(&proc_div.body);

            for stmt in &statements {
                match stmt {
                    Statement::ExecSql(_) => {
                        let f = features
                            .entry("DB2".to_string())
                            .or_insert_with(|| Feature::new("DB2", FeatureCategory::Database));
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    Statement::ExecCics(_) => {
                        let f = features
                            .entry("CICS".to_string())
                            .or_insert_with(|| Feature::new("CICS", FeatureCategory::Transaction));
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    Statement::Call(_) => {
                        let f = features
                            .entry("Subprogram Calls".to_string())
                            .or_insert_with(|| {
                                Feature::new("Subprogram Calls", FeatureCategory::Interoperability)
                            });
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    Statement::Display(_) => {
                        let f = features
                            .entry("DISPLAY".to_string())
                            .or_insert_with(|| Feature::new("DISPLAY", FeatureCategory::CoreLanguage));
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    Statement::Accept(_) => {
                        let f = features
                            .entry("ACCEPT".to_string())
                            .or_insert_with(|| Feature::new("ACCEPT", FeatureCategory::CoreLanguage));
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    Statement::String(_) | Statement::Unstring(_) => {
                        let f = features
                            .entry("STRING/UNSTRING".to_string())
                            .or_insert_with(|| {
                                Feature::new("STRING/UNSTRING", FeatureCategory::CoreLanguage)
                            });
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    Statement::Inspect(_) => {
                        let f = features
                            .entry("INSPECT".to_string())
                            .or_insert_with(|| Feature::new("INSPECT", FeatureCategory::CoreLanguage));
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    Statement::Compute(_) => {
                        let f = features
                            .entry("COMPUTE".to_string())
                            .or_insert_with(|| Feature::new("COMPUTE", FeatureCategory::CoreLanguage));
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    Statement::Sort(_) | Statement::Merge(_) => {
                        let f = features
                            .entry("SORT/MERGE".to_string())
                            .or_insert_with(|| Feature::new("SORT/MERGE", FeatureCategory::Batch));
                        f.add_occurrence(byte_offset_to_line(source, stmt.span().start));
                    }
                    _ => {}
                }
            }
        }

        features.into_values().collect()
    }

    fn calculate_complexity(
        &self,
        metrics: &CodeMetrics,
        features: &HashSet<Feature>,
        issues: &[crate::compatibility::CompatibilityIssue],
    ) -> MigrationComplexity {
        let mut score = 0;

        if metrics.total_lines > 5000 {
            score += 3;
        } else if metrics.total_lines > 2000 {
            score += 2;
        } else if metrics.total_lines > 500 {
            score += 1;
        }

        if metrics.cyclomatic_complexity > 50 {
            score += 3;
        } else if metrics.cyclomatic_complexity > 20 {
            score += 2;
        } else if metrics.cyclomatic_complexity > 10 {
            score += 1;
        }

        for feature in features {
            match feature.category {
                FeatureCategory::Database => score += 2,
                FeatureCategory::Transaction => score += 2,
                FeatureCategory::PlatformSpecific => score += 3,
                FeatureCategory::FileHandling => score += 1,
                _ => {}
            }
        }

        let critical = issues
            .iter()
            .filter(|i| i.severity == crate::compatibility::Severity::Critical)
            .count();
        let high = issues
            .iter()
            .filter(|i| i.severity == crate::compatibility::Severity::High)
            .count();

        score += critical * 3;
        score += high * 2;

        match score {
            0..=3 => MigrationComplexity::Low,
            4..=7 => MigrationComplexity::Medium,
            8..=12 => MigrationComplexity::High,
            _ => MigrationComplexity::VeryHigh,
        }
    }

    fn generate_recommendations(
        &self,
        features: &HashSet<Feature>,
        issues: &[crate::compatibility::CompatibilityIssue],
    ) -> Vec<String> {
        let mut recommendations = Vec::new();

        for feature in features {
            match (feature.category, feature.name.as_str()) {
                (FeatureCategory::Database, "DB2") => {
                    recommendations.push(
                        "Consider using PostgreSQL with the open-mainframe-db2 compatibility layer"
                            .to_string(),
                    );
                }
                (FeatureCategory::Transaction, "CICS") => {
                    recommendations.push(
                        "CICS commands can be emulated using the open-mainframe-cics runtime"
                            .to_string(),
                    );
                }
                (FeatureCategory::FileHandling, "VSAM") => {
                    recommendations.push(
                        "VSAM files can be migrated to the open-mainframe-dataset VSAM emulation"
                            .to_string(),
                    );
                }
                _ => {}
            }
        }

        for issue in issues {
            if !issue.recommendation.is_empty() && !recommendations.contains(&issue.recommendation)
            {
                recommendations.push(issue.recommendation.clone());
            }
        }

        recommendations
    }
}

impl Default for AstAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert a byte offset into a 1-indexed line number.
fn byte_offset_to_line(source: &str, offset: u32) -> usize {
    let offset = offset as usize;
    source[..offset.min(source.len())]
        .bytes()
        .filter(|&b| b == b'\n')
        .count()
        + 1
}

/// Count data items recursively.
fn count_data_items(items: &[open_mainframe_cobol::ast::data::DataItem]) -> usize {
    let mut count = 0;
    for item in items {
        count += 1;
        count += count_data_items(&item.children);
    }
    count
}

/// Collect all statements from a procedure body (recursively descending into
/// sections, paragraphs, and nested statement blocks).
fn collect_all_statements(body: &ProcedureBody) -> Vec<&Statement> {
    let mut stmts = Vec::new();

    match body {
        ProcedureBody::Sections(sections) => {
            for section in sections {
                for paragraph in &section.paragraphs {
                    for stmt in &paragraph.statements {
                        collect_nested_statements(stmt, &mut stmts);
                    }
                }
            }
        }
        ProcedureBody::Paragraphs(paragraphs) => {
            for paragraph in paragraphs {
                for stmt in &paragraph.statements {
                    collect_nested_statements(stmt, &mut stmts);
                }
            }
        }
        ProcedureBody::Statements(statements) => {
            for stmt in statements {
                collect_nested_statements(stmt, &mut stmts);
            }
        }
    }

    stmts
}

/// Recursively collect a statement and any nested statements.
fn collect_nested_statements<'a>(stmt: &'a Statement, out: &mut Vec<&'a Statement>) {
    out.push(stmt);

    match stmt {
        Statement::If(if_stmt) => {
            for s in &if_stmt.then_branch {
                collect_nested_statements(s, out);
            }
            if let Some(ref else_branch) = if_stmt.else_branch {
                for s in else_branch {
                    collect_nested_statements(s, out);
                }
            }
        }
        Statement::Evaluate(eval) => {
            for when in &eval.when_clauses {
                for s in &when.statements {
                    collect_nested_statements(s, out);
                }
            }
            if let Some(ref other) = eval.when_other {
                for s in other {
                    collect_nested_statements(s, out);
                }
            }
        }
        Statement::Perform(p) => {
            if let Some(ref inline) = p.inline {
                for s in inline {
                    collect_nested_statements(s, out);
                }
            }
        }
        Statement::Compute(c) => {
            if let Some(ref on_err) = c.on_size_error {
                for s in on_err {
                    collect_nested_statements(s, out);
                }
            }
            if let Some(ref not_err) = c.not_on_size_error {
                for s in not_err {
                    collect_nested_statements(s, out);
                }
            }
        }
        Statement::Read(r) => {
            if let Some(ref at_end) = r.at_end {
                for s in at_end {
                    collect_nested_statements(s, out);
                }
            }
            if let Some(ref not_end) = r.not_at_end {
                for s in not_end {
                    collect_nested_statements(s, out);
                }
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SIMPLE_COBOL: &str = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "HELLO".
           STOP RUN.
"#;

    #[test]
    fn test_ast_analyzer_basic() {
        let analyzer = AstAnalyzer::new();
        let result = analyzer.analyze(SIMPLE_COBOL, "test.cbl").unwrap();

        assert_eq!(result.program_id, Some("TESTPROG".to_string()));
        assert!(result.metrics.has_identification);
        assert!(result.metrics.has_procedure);
        assert!(result.metrics.executable_statements >= 2);
    }

    #[test]
    fn test_ast_no_false_positive_from_comment() {
        // EXEC SQL in a comment should NOT be detected
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. NODB2.
       PROCEDURE DIVISION.
       MAIN-PARA.
      *> EXEC SQL SELECT * FROM TABLE END-EXEC.
           DISPLAY "NO DB2 HERE".
           STOP RUN.
"#;

        let analyzer = AstAnalyzer::new();
        let result = analyzer.analyze(source, "nodb2.cbl").unwrap();

        // Should NOT detect DB2 since it's in a comment
        assert!(
            !result.features.iter().any(|f| f.name == "DB2"),
            "DB2 should not be detected in comments"
        );
    }

    #[test]
    fn test_ast_detects_display_feature() {
        let analyzer = AstAnalyzer::new();
        let result = analyzer.analyze(SIMPLE_COBOL, "test.cbl").unwrap();

        assert!(
            result.features.iter().any(|f| f.name == "DISPLAY"),
            "DISPLAY feature should be detected"
        );
    }

    #[test]
    fn test_ast_cyclomatic_complexity() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPLEX.
       PROCEDURE DIVISION.
       MAIN-PARA.
           IF A = B
               DISPLAY "EQUAL"
           ELSE
               DISPLAY "NOT EQUAL"
           END-IF.
           IF C = D
               DISPLAY "CD"
           END-IF.
           STOP RUN.
"#;

        let analyzer = AstAnalyzer::new();
        let result = analyzer.analyze(source, "test.cbl").unwrap();

        // Base (1) + 2 IF statements = 3
        assert!(
            result.metrics.cyclomatic_complexity >= 3,
            "Expected CC >= 3, got {}",
            result.metrics.cyclomatic_complexity,
        );
    }

    #[test]
    fn test_ast_paragraph_count() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARAS.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "MAIN".
           PERFORM SUB-PARA.
           STOP RUN.
       SUB-PARA.
           DISPLAY "SUB".
"#;

        let analyzer = AstAnalyzer::new();
        let result = analyzer.analyze(source, "test.cbl").unwrap();

        assert_eq!(result.metrics.paragraph_count, 2);
    }

    #[test]
    fn test_ast_fallback_on_parse_failure() {
        // Intentionally broken COBOL (no proper structure)
        let source = "THIS IS NOT VALID COBOL AT ALL";

        let analyzer = AstAnalyzer::new();
        let result = analyzer.analyze(source, "broken.cbl").unwrap();

        // Should still produce a result (via fallback)
        assert!(result
            .recommendations
            .iter()
            .any(|r| r.contains("Warning")));
    }
}
