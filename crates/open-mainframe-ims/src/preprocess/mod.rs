//! EXEC DLI preprocessing.
//!
//! Scans COBOL source for `EXEC DLI ... END-EXEC` blocks, parses the
//! DL/I command within, and replaces it with a `CALL 'CBLTDLI'` statement.
//! Also generates the DIB (DL/I Interface Block) copybook and SSA working
//! storage layouts.

// -----------------------------------------------------------------------
// Story 400.1 — EXEC DLI Block Extraction
// -----------------------------------------------------------------------

/// A parsed EXEC DLI block extracted from COBOL source.
#[derive(Debug, Clone, PartialEq)]
pub struct ExecDliBlock {
    /// Starting line number in source (1-based).
    pub start_line: usize,
    /// Ending line number in source (1-based).
    pub end_line: usize,
    /// The DL/I function code (GU, GN, ISRT, SCHD, CHKP, etc.).
    pub function: String,
    /// PCB index (from `USING PCB(n)`).
    pub pcb_index: Option<usize>,
    /// Target segment name (from `SEGMENT(name)`).
    pub segment: Option<String>,
    /// I/O area host variable (from `INTO(var)` or `FROM(var)`).
    pub io_area: Option<String>,
    /// Qualification (from `WHERE(field op :var)`).
    pub qualification: Option<DliQualification>,
    /// SSA strings parsed from the block.
    pub ssas: Vec<String>,
    /// PSB name (for SCHD command).
    pub psb_name: Option<String>,
    /// Checkpoint ID variable (for CHKP command).
    pub chkp_id: Option<String>,
    /// Raw text of the block.
    pub raw_text: String,
}

/// A parsed qualification from a WHERE clause.
#[derive(Debug, Clone, PartialEq)]
pub struct DliQualification {
    /// Field name.
    pub field: String,
    /// Operator (=, >=, <=, >, <, !=).
    pub operator: String,
    /// Host variable name (with leading colon stripped).
    pub host_var: String,
}

/// Scanner for extracting EXEC DLI blocks from COBOL source.
pub struct DliScanner {
    /// Extracted blocks.
    blocks: Vec<ExecDliBlock>,
}

impl DliScanner {
    /// Create a new scanner.
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
        }
    }

    /// Scan COBOL source and extract all EXEC DLI blocks.
    pub fn scan(&mut self, source: &str) -> Vec<ExecDliBlock> {
        self.blocks.clear();
        let lines: Vec<&str> = source.lines().collect();

        let mut i = 0;
        while i < lines.len() {
            let line = lines[i];
            let content = cobol_content(line);

            // Look for EXEC DLI (case-insensitive)
            if let Some(start_pos) = find_exec_dli(content) {
                let start_line = i + 1; // 1-based
                let mut block_text = String::new();

                // Collect until END-EXEC
                let mut found_end = false;
                let remaining = &content[start_pos..];
                block_text.push_str(remaining.trim());

                // Check if END-EXEC is on the same line
                if contains_end_exec(remaining) {
                    let end_line = i + 1;
                    let raw = strip_end_exec(&block_text);
                    if let Some(block) = parse_dli_block(&raw, start_line, end_line) {
                        self.blocks.push(block);
                    }
                    i += 1;
                    continue;
                }

                // Multi-line: continue collecting
                let mut j = i + 1;
                while j < lines.len() {
                    let next_content = cobol_content(lines[j]);
                    block_text.push(' ');
                    block_text.push_str(next_content.trim());

                    if contains_end_exec(next_content) {
                        found_end = true;
                        break;
                    }
                    j += 1;
                }

                if found_end {
                    let end_line = j + 1;
                    let raw = strip_end_exec(&block_text);
                    if let Some(block) = parse_dli_block(&raw, start_line, end_line) {
                        self.blocks.push(block);
                    }
                    i = j + 1;
                } else {
                    // Unterminated EXEC DLI — skip
                    i += 1;
                }
            } else {
                i += 1;
            }
        }

        self.blocks.clone()
    }

    /// Get the last set of extracted blocks.
    pub fn blocks(&self) -> &[ExecDliBlock] {
        &self.blocks
    }
}

impl Default for DliScanner {
    fn default() -> Self {
        Self::new()
    }
}

/// Extract the content area from a COBOL line.
///
/// For lines >= 7 chars, extracts columns 7-72 (skipping sequence area).
/// For shorter lines, returns the full trimmed line (free-form or test input).
/// Comment lines (indicator `*` or `/` in column 7) return empty.
fn cobol_content(line: &str) -> &str {
    if line.len() <= 6 {
        // Short line — treat as-is (may be free-form or test input)
        return line;
    }
    // Skip column 7 if it's an indicator (comment, continuation)
    let ch7 = line.as_bytes().get(6).copied().unwrap_or(b' ');
    if ch7 == b'*' || ch7 == b'/' {
        return "";
    }
    let start = 6; // column 7 (0-based index 6)
    let end = line.len().min(72);
    &line[start..end]
}

/// Check if content contains `EXEC DLI` and return offset.
fn find_exec_dli(content: &str) -> Option<usize> {
    let upper = content.to_uppercase();
    // Match "EXEC DLI" with flexible whitespace
    if let Some(pos) = upper.find("EXEC") {
        let after_exec = &upper[pos + 4..];
        let trimmed = after_exec.trim_start();
        if trimmed.starts_with("DLI") {
            // Return the position right after "DLI"
            let dli_offset = after_exec.len() - trimmed.len() + 3;
            return Some(pos + 4 + dli_offset);
        }
    }
    None
}

/// Check if content contains `END-EXEC`.
fn contains_end_exec(content: &str) -> bool {
    content.to_uppercase().contains("END-EXEC")
}

/// Strip everything from END-EXEC onward (and the EXEC DLI prefix).
fn strip_end_exec(text: &str) -> String {
    let upper = text.to_uppercase();
    if let Some(pos) = upper.find("END-EXEC") {
        text[..pos].trim().to_string()
    } else {
        text.trim().to_string()
    }
}

/// Parse the inner content of an EXEC DLI block into a structured block.
fn parse_dli_block(raw: &str, start_line: usize, end_line: usize) -> Option<ExecDliBlock> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }

    // Tokenize: the first token is the function code
    let tokens = tokenize(text);
    if tokens.is_empty() {
        return None;
    }

    let function = tokens[0].to_uppercase();

    let mut block = ExecDliBlock {
        start_line,
        end_line,
        function: function.clone(),
        pcb_index: None,
        segment: None,
        io_area: None,
        qualification: None,
        ssas: Vec::new(),
        psb_name: None,
        chkp_id: None,
        raw_text: raw.to_string(),
    };

    // Parse keyword parameters.
    // Tokens may be bare keywords ("SEGMENT") or keyword+paren ("SEGMENT(CUSTOMER)").
    let mut i = 1;
    while i < tokens.len() {
        let token = &tokens[i];

        if try_extract(token, "USING", |_| { /* handled by next token */ }) {
            // USING PCB(n) — next token should be PCB(n)
            i += 1;
            if i < tokens.len() {
                if let Some(val) = extract_paren_value(&tokens[i], "PCB") {
                    block.pcb_index = val.parse().ok();
                }
            }
        } else if let Some(val) = extract_paren_value(token, "SEGMENT") {
            block.segment = Some(val);
        } else if is_bare_keyword(token, "SEGMENT") {
            i += 1;
            if i < tokens.len() {
                block.segment = Some(strip_parens(&tokens[i]));
            }
        } else if let Some(val) = extract_paren_value(token, "INTO") {
            block.io_area = Some(val);
        } else if is_bare_keyword(token, "INTO") {
            i += 1;
            if i < tokens.len() {
                block.io_area = Some(strip_parens(&tokens[i]));
            }
        } else if let Some(val) = extract_paren_value(token, "FROM") {
            block.io_area = Some(val);
        } else if is_bare_keyword(token, "FROM") {
            i += 1;
            if i < tokens.len() {
                block.io_area = Some(strip_parens(&tokens[i]));
            }
        } else if let Some(val) = extract_paren_value(token, "WHERE") {
            block.qualification = parse_qualification(&val);
        } else if is_bare_keyword(token, "WHERE") {
            i += 1;
            if i < tokens.len() {
                let q = strip_parens(&tokens[i]);
                block.qualification = parse_qualification(&q);
            }
        } else if let Some(val) = extract_paren_value(token, "PSB") {
            block.psb_name = Some(val);
        } else if is_bare_keyword(token, "PSB") {
            i += 1;
            if i < tokens.len() {
                block.psb_name = Some(strip_parens(&tokens[i]));
            }
        } else if let Some(val) = extract_paren_value(token, "CHKPID") {
            block.chkp_id = Some(val);
        } else if is_bare_keyword(token, "CHKPID") {
            i += 1;
            if i < tokens.len() {
                block.chkp_id = Some(strip_parens(&tokens[i]));
            }
        } else if let Some(val) = extract_paren_value(token, "PCB") {
            // PCB(n) without USING prefix
            block.pcb_index = val.parse().ok();
        }

        i += 1;
    }

    // Build SSA from segment + qualification
    if let Some(ref seg) = block.segment {
        if let Some(ref qual) = block.qualification {
            let ssa = format!(
                "{}({} {} :{})",
                seg, qual.field, qual.operator, qual.host_var
            );
            block.ssas.push(ssa);
        } else {
            block.ssas.push(seg.clone());
        }
    }

    Some(block)
}

/// Tokenize text respecting parenthesized groups.
fn tokenize(text: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut paren_depth = 0;

    for ch in text.chars() {
        match ch {
            '(' => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' => {
                paren_depth -= 1;
                current.push(ch);
                if paren_depth == 0 {
                    // Complete token
                    let t = current.trim().to_string();
                    if !t.is_empty() {
                        tokens.push(t);
                    }
                    current.clear();
                }
            }
            ' ' | '\t' if paren_depth == 0 => {
                let t = current.trim().to_string();
                if !t.is_empty() {
                    tokens.push(t);
                }
                current.clear();
            }
            ',' if paren_depth == 0 => {
                let t = current.trim().to_string();
                if !t.is_empty() {
                    tokens.push(t);
                }
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
    }

    let t = current.trim().to_string();
    if !t.is_empty() {
        tokens.push(t);
    }

    tokens
}

/// Check if a token is exactly the bare keyword (no parentheses).
fn is_bare_keyword(token: &str, keyword: &str) -> bool {
    token.to_uppercase() == keyword.to_uppercase()
}

/// Helper to check if a token matches a bare keyword; invokes a callback.
fn try_extract<F: FnOnce(&str)>(token: &str, keyword: &str, _f: F) -> bool {
    is_bare_keyword(token, keyword)
}

/// Extract value from `KEYWORD(value)` pattern.
fn extract_paren_value(token: &str, keyword: &str) -> Option<String> {
    let upper = token.to_uppercase();
    let kw_upper = keyword.to_uppercase();
    if upper.starts_with(&format!("{}(", kw_upper)) && upper.ends_with(')') {
        let start = keyword.len() + 1;
        let end = token.len() - 1;
        if start < end {
            return Some(token[start..end].trim().to_string());
        }
    }
    None
}

/// Strip surrounding parentheses from a token.
fn strip_parens(token: &str) -> String {
    let t = token.trim();
    if t.starts_with('(') && t.ends_with(')') {
        t[1..t.len() - 1].trim().to_string()
    } else {
        t.to_string()
    }
}

/// Parse a qualification string like `CUSTNO = :WS-KEY`.
fn parse_qualification(text: &str) -> Option<DliQualification> {
    let operators = [">=", "<=", "!=", "=", ">", "<"];

    for op in &operators {
        if let Some(pos) = text.find(op) {
            let field = text[..pos].trim().to_string();
            let value = text[pos + op.len()..].trim().to_string();

            // Strip leading colon from host variable
            let host_var = if let Some(stripped) = value.strip_prefix(':') {
                stripped.to_string()
            } else {
                value.clone()
            };

            if !field.is_empty() && !host_var.is_empty() {
                return Some(DliQualification {
                    field,
                    operator: op.to_string(),
                    host_var,
                });
            }
        }
    }
    None
}

// -----------------------------------------------------------------------
// Story 400.2 — CBLTDLI Call Generation
// -----------------------------------------------------------------------

/// Counter for generating unique SSA layout names.
static SSA_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

/// Generate a CBLTDLI CALL statement from an EXEC DLI block.
///
/// Returns `(procedure_division_code, working_storage_code)`.
pub fn generate_cbltdli_call(block: &ExecDliBlock) -> (String, String) {
    let mut proc_lines = Vec::new();
    let mut ws_lines = Vec::new();

    match block.function.as_str() {
        "SCHD" => {
            // Schedule PSB: CALL 'CBLTDLI' USING DLI-SCHD, IO-PCB-MASK, PSB-NAME
            let psb_name = block.psb_name.as_deref().unwrap_or("UNKNOWN");
            let psb_ws_name = format!("DLI-PSB-{}", psb_name);

            ws_lines.push(format!(
                "       01  {:<30} PIC X(8) VALUE '{}'.",
                psb_ws_name, psb_name
            ));
            ws_lines.push(
                "       01  DLI-SCHD                       PIC X(4) VALUE 'PCB '.".to_string(),
            );

            proc_lines.push(
                "           CALL 'CBLTDLI' USING DLI-SCHD".to_string(),
            );
            proc_lines.push(format!(
                "               IO-PCB-MASK {}",
                psb_ws_name
            ));
        }
        "CHKP" => {
            // Checkpoint: CALL 'CBLTDLI' USING DLI-CHKP, IO-PCB-MASK, CHKP-ID
            let chkp_id = block.chkp_id.as_deref().unwrap_or("WS-CHKP-ID");

            ws_lines.push(
                "       01  DLI-CHKP                       PIC X(4) VALUE 'CHKP'.".to_string(),
            );

            proc_lines.push("           CALL 'CBLTDLI' USING DLI-CHKP".to_string());
            proc_lines.push(format!(
                "               IO-PCB-MASK {}",
                chkp_id
            ));
        }
        "ROLB" => {
            ws_lines.push(
                "       01  DLI-ROLB                       PIC X(4) VALUE 'ROLB'.".to_string(),
            );
            proc_lines.push("           CALL 'CBLTDLI' USING DLI-ROLB".to_string());
            proc_lines.push("               IO-PCB-MASK".to_string());
        }
        _ => {
            // Database DL/I call: GU, GN, GNP, GHU, GHN, GHNP, ISRT, DLET, REPL
            let func = &block.function;
            let func_ws = format!("DLI-{}", func);
            let func_code = dli_function_code(func);
            let pcb_index = block.pcb_index.unwrap_or(1);
            let pcb_mask = format!("PCB-{:03}", pcb_index);
            let io_area = block.io_area.as_deref().unwrap_or("IO-AREA");

            // Function code WS definition
            ws_lines.push(format!(
                "       01  {:<30} PIC X(4) VALUE '{}'.",
                func_ws, func_code
            ));

            // SSA WS definitions
            let ssa_refs: Vec<String> = if !block.ssas.is_empty() {
                let mut refs = Vec::new();
                for ssa_str in &block.ssas {
                    let counter = SSA_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1;
                    let ssa_name = format!("SSA-{:04}", counter);
                    let ssa_layout = generate_ssa_layout(&ssa_name, ssa_str);
                    ws_lines.push(ssa_layout);
                    refs.push(ssa_name);
                }
                refs
            } else {
                Vec::new()
            };

            // Build CALL statement
            proc_lines.push(format!(
                "           CALL 'CBLTDLI' USING {}",
                func_ws
            ));
            proc_lines.push(format!("               {}", pcb_mask));
            proc_lines.push(format!("               {}", io_area));
            for ssa_ref in &ssa_refs {
                proc_lines.push(format!("               {}", ssa_ref));
            }
        }
    }

    // Terminate the last line with a period
    if let Some(last) = proc_lines.last_mut() {
        last.push('.');
    }

    (proc_lines.join("\n"), ws_lines.join("\n"))
}

/// Map a DL/I function name to its 4-character function code.
fn dli_function_code(func: &str) -> &'static str {
    match func.to_uppercase().as_str() {
        "GU" => "GU  ",
        "GN" => "GN  ",
        "GNP" => "GNP ",
        "GHU" => "GHU ",
        "GHN" => "GHN ",
        "GHNP" => "GHNP",
        "ISRT" => "ISRT",
        "DLET" => "DLET",
        "REPL" => "REPL",
        "CHKP" => "CHKP",
        "ROLB" => "ROLB",
        "SYNC" => "SYNC",
        "LOG" => "LOG ",
        "STAT" => "STAT",
        "SCHD" | "PCB" => "PCB ",
        "TERM" => "TERM",
        _ => "    ",
    }
}

/// Generate SSA layout for WORKING-STORAGE.
fn generate_ssa_layout(ssa_name: &str, ssa_str: &str) -> String {
    // Parse the SSA string to determine if qualified
    // Unqualified: just segment name (8 chars + space)
    // Qualified: segment name + '(' + field + op + value + ')'
    let upper = ssa_str.to_uppercase();

    if upper.contains('(') {
        // Qualified SSA — generate structured layout
        let seg_end = ssa_str.find('(').unwrap();
        let segment = ssa_str[..seg_end].trim();

        format!(
            "       01  {:<30} PIC X(80) VALUE\n           '{:<8}({}'.",
            ssa_name,
            segment,
            &ssa_str[seg_end + 1..]
        )
    } else {
        // Unqualified SSA — just segment name padded to 8
        format!(
            "       01  {:<30} PIC X(9) VALUE '{:<8} '.",
            ssa_name, ssa_str.trim()
        )
    }
}

// -----------------------------------------------------------------------
// Story 400.3 — DIB (DL/I Interface Block) Generation
// -----------------------------------------------------------------------

/// Generate the DIB (DL/I Interface Block) copybook.
///
/// The DIB is the EXEC DLI equivalent of SQLCA. It provides fields that
/// the program can check after each DL/I call:
///
/// - `DIBSTAT` — 2-character status code
/// - `DIBSEGM` — segment name from last call
/// - `DIBKFBL` — key feedback area length
/// - `DIBDBDNM` — database name
/// - `DIBSEGNO` — segment level number
pub fn generate_dib() -> String {
    let lines = vec![
        "      *-------------------------------------------------------",
        "      * DIB - DL/I INTERFACE BLOCK FOR EXEC DLI",
        "      *-------------------------------------------------------",
        "       01  DIB.",
        "           05  DIBSTAT    PIC XX.",
        "               88  DIBSTAT-OK       VALUE SPACES.",
        "               88  DIBSTAT-GE       VALUE 'GE'.",
        "               88  DIBSTAT-GB       VALUE 'GB'.",
        "               88  DIBSTAT-GA       VALUE 'GA'.",
        "               88  DIBSTAT-GK       VALUE 'GK'.",
        "               88  DIBSTAT-II       VALUE 'II'.",
        "               88  DIBSTAT-DJ       VALUE 'DJ'.",
        "               88  DIBSTAT-RX       VALUE 'RX'.",
        "               88  DIBSTAT-ERROR    VALUE 'AD' 'AI' 'AK' 'AP'.",
        "           05  DIBSEGM   PIC X(8).",
        "           05  DIBKFBL   PIC S9(4) COMP.",
        "           05  DIBDBDNM  PIC X(8).",
        "           05  DIBSEGNO  PIC S9(4) COMP.",
        "           05  DIBSEGLV  PIC XX.",
        "           05  DIBPCBNM  PIC X(8).",
        "           05  FILLER    PIC X(26).",
    ];
    lines.join("\n")
}

// -----------------------------------------------------------------------
// Preprocessor — orchestrates scanning + code generation
// -----------------------------------------------------------------------

/// The EXEC DLI preprocessor.
///
/// Scans COBOL source, replaces `EXEC DLI ... END-EXEC` blocks with
/// `CALL 'CBLTDLI'` statements, and generates WORKING-STORAGE items
/// for SSA layouts and the DIB copybook.
pub struct DliPreprocessor {
    /// Generated working-storage additions.
    working_storage: Vec<String>,
    /// Whether DIB has been generated.
    dib_generated: bool,
}

impl DliPreprocessor {
    /// Create a new preprocessor.
    pub fn new() -> Self {
        Self {
            working_storage: Vec::new(),
            dib_generated: false,
        }
    }

    /// Process COBOL source and return transformed source.
    ///
    /// Returns `(transformed_source, working_storage_additions)`.
    pub fn process(&mut self, source: &str) -> (String, String) {
        let mut scanner = DliScanner::new();
        let blocks = scanner.scan(source);

        if blocks.is_empty() {
            return (source.to_string(), String::new());
        }

        // Generate DIB if not already done
        if !self.dib_generated {
            self.working_storage.push(generate_dib());
            self.dib_generated = true;
        }

        // Process each block
        let lines: Vec<&str> = source.lines().collect();
        let mut output_lines: Vec<String> = Vec::new();
        let mut line_idx = 0;
        let mut block_idx = 0;

        while line_idx < lines.len() {
            if block_idx < blocks.len() {
                let block = &blocks[block_idx];
                let start = block.start_line - 1; // 0-based
                let end = block.end_line; // 1-based end, so end-1 is last line (0-based)

                if line_idx == start {
                    // Replace block with CBLTDLI call
                    let (proc_code, ws_code) = generate_cbltdli_call(block);

                    // Comment out original lines
                    for orig_idx in start..end {
                        if orig_idx < lines.len() {
                            let orig = lines[orig_idx];
                            let commented = format!("      *DLI {}", &orig[6.min(orig.len())..]);
                            output_lines.push(commented);
                        }
                    }

                    // Insert generated CALL
                    for call_line in proc_code.lines() {
                        output_lines.push(call_line.to_string());
                    }

                    if !ws_code.is_empty() {
                        self.working_storage.push(ws_code);
                    }

                    line_idx = end;
                    block_idx += 1;
                    continue;
                }
            }

            output_lines.push(lines[line_idx].to_string());
            line_idx += 1;
        }

        let ws_output = self.working_storage.join("\n");
        (output_lines.join("\n"), ws_output)
    }

    /// Get the working-storage additions.
    pub fn working_storage(&self) -> &[String] {
        &self.working_storage
    }
}

impl Default for DliPreprocessor {
    fn default() -> Self {
        Self::new()
    }
}

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // --- Story 400.1: EXEC DLI Block Extraction Tests ---

    #[test]
    fn test_scan_single_exec_dli() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROCEDURE DIVISION.
           EXEC DLI GU USING PCB(1) SEGMENT(CUSTOMER)
               INTO(WS-CUST) WHERE(CUSTNO = :WS-KEY)
           END-EXEC.
           DISPLAY 'DONE'.
"#;
        let mut scanner = DliScanner::new();
        let blocks = scanner.scan(source);

        assert_eq!(blocks.len(), 1);
        let block = &blocks[0];
        assert_eq!(block.function, "GU");
        assert_eq!(block.pcb_index, Some(1));
        assert_eq!(block.segment.as_deref(), Some("CUSTOMER"));
        assert_eq!(block.io_area.as_deref(), Some("WS-CUST"));
        assert!(block.qualification.is_some());

        let qual = block.qualification.as_ref().unwrap();
        assert_eq!(qual.field, "CUSTNO");
        assert_eq!(qual.operator, "=");
        assert_eq!(qual.host_var, "WS-KEY");
    }

    #[test]
    fn test_scan_multiple_blocks() {
        let source = r#"
       PROCEDURE DIVISION.
           EXEC DLI GU USING PCB(1)
               SEGMENT(CUSTOMER) INTO(WS-CUST)
           END-EXEC.
           EXEC DLI GN USING PCB(1)
               SEGMENT(ORDER) INTO(WS-ORD)
           END-EXEC.
"#;
        let mut scanner = DliScanner::new();
        let blocks = scanner.scan(source);

        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0].function, "GU");
        assert_eq!(blocks[1].function, "GN");
        assert_eq!(blocks[0].segment.as_deref(), Some("CUSTOMER"));
        assert_eq!(blocks[1].segment.as_deref(), Some("ORDER"));
    }

    #[test]
    fn test_scan_schd_block() {
        let source = r#"
       PROCEDURE DIVISION.
           EXEC DLI SCHD PSB(MYPSB) END-EXEC.
"#;
        let mut scanner = DliScanner::new();
        let blocks = scanner.scan(source);

        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].function, "SCHD");
        assert_eq!(blocks[0].psb_name.as_deref(), Some("MYPSB"));
    }

    #[test]
    fn test_scan_isrt_block() {
        let source = r#"
       PROCEDURE DIVISION.
           EXEC DLI ISRT USING PCB(1)
               SEGMENT(CUSTOMER) FROM(WS-CUST)
           END-EXEC.
"#;
        let mut scanner = DliScanner::new();
        let blocks = scanner.scan(source);

        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].function, "ISRT");
        assert_eq!(blocks[0].io_area.as_deref(), Some("WS-CUST"));
    }

    #[test]
    fn test_scan_chkp_block() {
        let source = r#"
       PROCEDURE DIVISION.
           EXEC DLI CHKP CHKPID(WS-CHKP-ID) END-EXEC.
"#;
        let mut scanner = DliScanner::new();
        let blocks = scanner.scan(source);

        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].function, "CHKP");
        assert_eq!(blocks[0].chkp_id.as_deref(), Some("WS-CHKP-ID"));
    }

    #[test]
    fn test_scan_with_correct_line_numbers() {
        let source = "       IDENTIFICATION DIVISION.\n       PROCEDURE DIVISION.\n           EXEC DLI GU USING PCB(1)\n               SEGMENT(CUSTOMER) INTO(WS-CUST)\n           END-EXEC.\n";
        let mut scanner = DliScanner::new();
        let blocks = scanner.scan(source);

        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].start_line, 3);
        assert_eq!(blocks[0].end_line, 5);
    }

    #[test]
    fn test_scan_no_exec_dli() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO'.
           STOP RUN.
"#;
        let mut scanner = DliScanner::new();
        let blocks = scanner.scan(source);

        assert!(blocks.is_empty());
    }

    // --- Story 400.2: CBLTDLI Call Generation Tests ---

    #[test]
    fn test_generate_cbltdli_gu() {
        let block = ExecDliBlock {
            start_line: 1,
            end_line: 3,
            function: "GU".to_string(),
            pcb_index: Some(1),
            segment: Some("CUSTOMER".to_string()),
            io_area: Some("WS-CUST".to_string()),
            qualification: Some(DliQualification {
                field: "CUSTNO".to_string(),
                operator: "=".to_string(),
                host_var: "WS-KEY".to_string(),
            }),
            ssas: vec!["CUSTOMER(CUSTNO = :WS-KEY)".to_string()],
            psb_name: None,
            chkp_id: None,
            raw_text: String::new(),
        };

        let (proc_code, ws_code) = generate_cbltdli_call(&block);

        assert!(proc_code.contains("CALL 'CBLTDLI'"));
        assert!(proc_code.contains("DLI-GU"));
        assert!(proc_code.contains("PCB-001"));
        assert!(proc_code.contains("WS-CUST"));
        assert!(proc_code.contains("SSA-")); // SSA reference
        assert!(ws_code.contains("DLI-GU"));
        assert!(ws_code.contains("GU  ")); // 4-char function code
        assert!(ws_code.contains("SSA-")); // SSA layout
    }

    #[test]
    fn test_generate_cbltdli_schd() {
        let block = ExecDliBlock {
            start_line: 1,
            end_line: 1,
            function: "SCHD".to_string(),
            pcb_index: None,
            segment: None,
            io_area: None,
            qualification: None,
            ssas: Vec::new(),
            psb_name: Some("MYPSB".to_string()),
            chkp_id: None,
            raw_text: String::new(),
        };

        let (proc_code, ws_code) = generate_cbltdli_call(&block);

        assert!(proc_code.contains("CALL 'CBLTDLI'"));
        assert!(proc_code.contains("DLI-SCHD"));
        assert!(proc_code.contains("IO-PCB-MASK"));
        assert!(proc_code.contains("DLI-PSB-MYPSB"));
        assert!(ws_code.contains("MYPSB"));
    }

    #[test]
    fn test_generate_cbltdli_chkp() {
        let block = ExecDliBlock {
            start_line: 1,
            end_line: 1,
            function: "CHKP".to_string(),
            pcb_index: None,
            segment: None,
            io_area: None,
            qualification: None,
            ssas: Vec::new(),
            psb_name: None,
            chkp_id: Some("WS-CHKP-ID".to_string()),
            raw_text: String::new(),
        };

        let (proc_code, ws_code) = generate_cbltdli_call(&block);

        assert!(proc_code.contains("CALL 'CBLTDLI'"));
        assert!(proc_code.contains("DLI-CHKP"));
        assert!(proc_code.contains("IO-PCB-MASK"));
        assert!(proc_code.contains("WS-CHKP-ID"));
        assert!(ws_code.contains("DLI-CHKP"));
    }

    #[test]
    fn test_generate_unqualified_ssa_layout() {
        let layout = generate_ssa_layout("SSA-TEST", "CUSTOMER");
        assert!(layout.contains("SSA-TEST"));
        assert!(layout.contains("CUSTOMER"));
        assert!(layout.contains("PIC X(9)"));
    }

    #[test]
    fn test_generate_qualified_ssa_layout() {
        let layout = generate_ssa_layout("SSA-TEST", "CUSTOMER(CUSTNO = :WS-KEY)");
        assert!(layout.contains("SSA-TEST"));
        assert!(layout.contains("CUSTOMER"));
        assert!(layout.contains("PIC X(80)"));
    }

    // --- Story 400.3: DIB Generation Tests ---

    #[test]
    fn test_generate_dib() {
        let dib = generate_dib();

        assert!(dib.contains("DIB"));
        assert!(dib.contains("DIBSTAT"));
        assert!(dib.contains("DIBSEGM"));
        assert!(dib.contains("DIBKFBL"));
        assert!(dib.contains("DIBDBDNM"));
        assert!(dib.contains("DIBSEGNO"));
        assert!(dib.contains("DIBSEGLV"));
        assert!(dib.contains("DIBPCBNM"));
        // Check 88-level conditions
        assert!(dib.contains("DIBSTAT-OK"));
        assert!(dib.contains("DIBSTAT-GE"));
        assert!(dib.contains("DIBSTAT-GB"));
        assert!(dib.contains("DIBSTAT-II"));
        assert!(dib.contains("DIBSTAT-ERROR"));
    }

    // --- Preprocessor Integration Tests ---

    #[test]
    fn test_preprocessor_replaces_exec_dli() {
        let source = "       IDENTIFICATION DIVISION.\n       PROCEDURE DIVISION.\n           EXEC DLI GU USING PCB(1)\n               SEGMENT(CUSTOMER) INTO(WS-CUST)\n           END-EXEC.\n           DISPLAY 'DONE'.\n";

        let mut preprocessor = DliPreprocessor::new();
        let (transformed, ws) = preprocessor.process(source);

        // Original EXEC DLI lines should be commented out
        assert!(transformed.contains("*DLI"));
        // CALL 'CBLTDLI' should be present
        assert!(transformed.contains("CALL 'CBLTDLI'"));
        // DIB should be in working storage
        assert!(ws.contains("DIBSTAT"));
        // DISPLAY line should be preserved
        assert!(transformed.contains("DISPLAY 'DONE'"));
    }

    #[test]
    fn test_preprocessor_no_exec_dli_passthrough() {
        let source = "       IDENTIFICATION DIVISION.\n       PROCEDURE DIVISION.\n           DISPLAY 'HELLO'.\n";

        let mut preprocessor = DliPreprocessor::new();
        let (transformed, ws) = preprocessor.process(source);

        assert_eq!(transformed, source);
        assert!(ws.is_empty());
    }

    #[test]
    fn test_preprocessor_generates_dib_once() {
        let source = "       PROCEDURE DIVISION.\n           EXEC DLI GU USING PCB(1) SEGMENT(A) INTO(X) END-EXEC.\n           EXEC DLI GN USING PCB(1) SEGMENT(B) INTO(Y) END-EXEC.\n";

        let mut preprocessor = DliPreprocessor::new();
        let (_, ws) = preprocessor.process(source);

        // DIB should appear exactly once
        let dib_count = ws.matches("01  DIB.").count();
        assert_eq!(dib_count, 1);
    }

    // --- Parsing edge case tests ---

    #[test]
    fn test_parse_qualification_equals() {
        let qual = parse_qualification("CUSTNO = :WS-KEY").unwrap();
        assert_eq!(qual.field, "CUSTNO");
        assert_eq!(qual.operator, "=");
        assert_eq!(qual.host_var, "WS-KEY");
    }

    #[test]
    fn test_parse_qualification_gte() {
        let qual = parse_qualification("AMOUNT >= :WS-MIN").unwrap();
        assert_eq!(qual.field, "AMOUNT");
        assert_eq!(qual.operator, ">=");
        assert_eq!(qual.host_var, "WS-MIN");
    }

    #[test]
    fn test_tokenize_simple() {
        let tokens = tokenize("GU USING PCB(1) SEGMENT(CUSTOMER)");
        assert_eq!(tokens, vec!["GU", "USING", "PCB(1)", "SEGMENT(CUSTOMER)"]);
    }

    #[test]
    fn test_tokenize_with_where() {
        let tokens = tokenize("GU USING PCB(1) WHERE(CUSTNO = :WS-KEY)");
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], "GU");
        assert_eq!(tokens[1], "USING");
        assert_eq!(tokens[2], "PCB(1)");
        assert_eq!(tokens[3], "WHERE(CUSTNO = :WS-KEY)");
    }

    #[test]
    fn test_cobol_content_comment() {
        assert_eq!(cobol_content("      * This is a comment"), "");
    }

    #[test]
    fn test_cobol_content_normal() {
        let line = "           DISPLAY 'HELLO'.";
        let content = cobol_content(line);
        assert!(content.contains("DISPLAY"));
    }

    #[test]
    fn test_extract_paren_value() {
        assert_eq!(extract_paren_value("PCB(1)", "PCB"), Some("1".to_string()));
        assert_eq!(
            extract_paren_value("SEGMENT(CUSTOMER)", "SEGMENT"),
            Some("CUSTOMER".to_string())
        );
        assert_eq!(extract_paren_value("NOMATCH(1)", "PCB"), None);
    }

    #[test]
    fn test_dli_function_codes() {
        assert_eq!(dli_function_code("GU"), "GU  ");
        assert_eq!(dli_function_code("ISRT"), "ISRT");
        assert_eq!(dli_function_code("GHNP"), "GHNP");
        assert_eq!(dli_function_code("CHKP"), "CHKP");
    }

    #[test]
    fn test_rolb_generation() {
        let block = ExecDliBlock {
            start_line: 1,
            end_line: 1,
            function: "ROLB".to_string(),
            pcb_index: None,
            segment: None,
            io_area: None,
            qualification: None,
            ssas: Vec::new(),
            psb_name: None,
            chkp_id: None,
            raw_text: String::new(),
        };

        let (proc_code, ws_code) = generate_cbltdli_call(&block);

        assert!(proc_code.contains("CALL 'CBLTDLI'"));
        assert!(proc_code.contains("DLI-ROLB"));
        assert!(proc_code.contains("IO-PCB-MASK"));
        assert!(ws_code.contains("ROLB"));
    }
}
