//! HLASM macro language engine.
//!
//! - MACRO/MEND definition with positional and keyword parameters
//! - System variable symbols (&SYSNDX, &SYSECT, &SYSDATE, &SYSTIME, etc.)
//! - Macro expansion with parameter substitution and &SYSLIST
//! - MNOTE (assembly-time messages with severity)
//! - MEXIT (early exit from macro expansion)
//! - COPY member inclusion
//! - Inner macro calls (macros calling macros)

use std::collections::HashMap;

// ---------------------------------------------------------------------------
//  Macro definition
// ---------------------------------------------------------------------------

/// A single macro parameter.
#[derive(Debug, Clone)]
pub struct MacroParam {
    /// Parameter name (including & prefix, e.g. "&P1").
    pub name: String,
    /// Whether this is a keyword parameter (has default value).
    pub keyword: bool,
    /// Default value for keyword parameters.
    pub default: String,
}

/// A macro definition parsed from MACRO/MEND.
#[derive(Debug, Clone)]
pub struct MacroDef {
    /// Macro name (opcode position).
    pub name: String,
    /// Label parameter (e.g. "&LABEL" from the prototype).
    pub label_param: Option<String>,
    /// Positional and keyword parameters in declaration order.
    pub params: Vec<MacroParam>,
    /// Body lines (between prototype and MEND, raw text).
    pub body: Vec<String>,
}

impl MacroDef {
    /// Find a parameter by name (case-insensitive, with & prefix).
    pub fn find_param(&self, name: &str) -> Option<&MacroParam> {
        let upper = name.to_uppercase();
        self.params.iter().find(|p| p.name.to_uppercase() == upper)
    }
}

// ---------------------------------------------------------------------------
//  Macro expansion context
// ---------------------------------------------------------------------------

/// System variable symbols available during macro expansion.
#[derive(Debug, Clone)]
pub struct SystemVars {
    /// &SYSNDX — unique macro invocation counter.
    pub sysndx: u32,
    /// &SYSECT — current control section name.
    pub sysect: String,
    /// &SYSLOC — current location counter name.
    pub sysloc: String,
    /// &SYSDATE — assembly date (MM/DD/YY).
    pub sysdate: String,
    /// &SYSTIME — assembly time (HH.MM).
    pub systime: String,
    /// &SYSPARM — assembly parameter string.
    pub sysparm: String,
    /// &SYSASM — assembler identification.
    pub sysasm: String,
    /// &SYSSTMT — current statement number.
    pub sysstmt: u32,
    /// &SYSTEM_ID — system identification.
    pub system_id: String,
    /// &SYSVER — assembler version.
    pub sysver: String,
}

impl Default for SystemVars {
    fn default() -> Self {
        Self {
            sysndx: 0,
            sysect: String::new(),
            sysloc: String::new(),
            sysdate: "01/01/00".to_string(),
            systime: "00.00".to_string(),
            sysparm: String::new(),
            sysasm: "OPEN MAINFRAME HLASM".to_string(),
            sysstmt: 0,
            system_id: "OMVS".to_string(),
            sysver: "1.6.0".to_string(),
        }
    }
}

impl SystemVars {
    /// Resolve a system variable name to its value.
    pub fn resolve(&self, name: &str) -> Option<String> {
        match name.to_uppercase().as_str() {
            "&SYSNDX" => Some(format!("{:04}", self.sysndx)),
            "&SYSECT" => Some(self.sysect.clone()),
            "&SYSLOC" => Some(self.sysloc.clone()),
            "&SYSDATE" => Some(self.sysdate.clone()),
            "&SYSTIME" => Some(self.systime.clone()),
            "&SYSPARM" => Some(self.sysparm.clone()),
            "&SYSASM" => Some(self.sysasm.clone()),
            "&SYSSTMT" => Some(format!("{:08}", self.sysstmt)),
            "&SYSTEM_ID" => Some(self.system_id.clone()),
            "&SYSVER" => Some(self.sysver.clone()),
            _ => None,
        }
    }
}

/// Context for a single macro expansion.
#[derive(Debug)]
struct ExpansionContext {
    /// The macro definition being expanded.
    def: MacroDef,
    /// Bound parameter values (name → value).
    param_values: HashMap<String, String>,
    /// Positional parameter list for &SYSLIST access.
    positional: Vec<String>,
}

// ---------------------------------------------------------------------------
//  MNOTE
// ---------------------------------------------------------------------------

/// An MNOTE message produced during macro expansion.
#[derive(Debug, Clone)]
pub struct Mnote {
    /// Severity (0..255, or * for assembler error).
    pub severity: u8,
    /// Message text.
    pub message: String,
}

// ---------------------------------------------------------------------------
//  COPY library
// ---------------------------------------------------------------------------

/// A library of source members available for COPY inclusion.
#[derive(Debug, Default)]
pub struct CopyLibrary {
    members: HashMap<String, Vec<String>>,
}

impl CopyLibrary {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a member to the library.
    pub fn add_member(&mut self, name: &str, lines: Vec<String>) {
        self.members.insert(name.to_uppercase(), lines);
    }

    /// Retrieve a member's lines.
    pub fn get_member(&self, name: &str) -> Option<&[String]> {
        self.members.get(&name.to_uppercase()).map(|v| v.as_slice())
    }
}

// ---------------------------------------------------------------------------
//  Macro engine
// ---------------------------------------------------------------------------

/// The macro engine: parses definitions, stores them, and expands invocations.
#[derive(Debug)]
pub struct MacroEngine {
    /// Registered macro definitions by name.
    macros: HashMap<String, MacroDef>,
    /// System variables.
    pub system_vars: SystemVars,
    /// COPY library.
    pub copy_lib: CopyLibrary,
    /// MNOTE messages accumulated during expansion.
    pub mnotes: Vec<Mnote>,
    /// Global SYSNDX counter.
    sysndx_counter: u32,
    /// Expansion depth limit.
    max_depth: usize,
}

impl Default for MacroEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl MacroEngine {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            system_vars: SystemVars::default(),
            copy_lib: CopyLibrary::new(),
            mnotes: Vec::new(),
            sysndx_counter: 0,
            max_depth: 100,
        }
    }

    /// Parse a MACRO/MEND block from source lines.
    ///
    /// `lines` should start with the line *after* MACRO and include everything
    /// up to and including MEND. Returns the parsed definition.
    pub fn parse_macro(&self, lines: &[String]) -> Result<MacroDef, MacroError> {
        if lines.is_empty() {
            return Err(MacroError::InvalidDefinition("Empty macro body".to_string()));
        }

        // First line is the prototype statement.
        let proto = &lines[0];
        let (label_param, name, params) = parse_prototype(proto)?;

        // Collect body lines (everything between prototype and MEND).
        let mut body = Vec::new();
        let mut found_mend = false;
        for line in &lines[1..] {
            let trimmed = line.trim().to_uppercase();
            if trimmed == "MEND" || trimmed.ends_with(" MEND") {
                found_mend = true;
                break;
            }
            body.push(line.clone());
        }

        if !found_mend {
            return Err(MacroError::MissingMend);
        }

        Ok(MacroDef {
            name: name.to_uppercase(),
            label_param,
            params,
            body,
        })
    }

    /// Register a macro definition.
    pub fn define_macro(&mut self, def: MacroDef) {
        self.macros.insert(def.name.to_uppercase(), def);
    }

    /// Check if a mnemonic is a defined macro.
    pub fn is_macro(&self, name: &str) -> bool {
        self.macros.contains_key(&name.to_uppercase())
    }

    /// Look up a macro definition.
    pub fn lookup(&self, name: &str) -> Option<&MacroDef> {
        self.macros.get(&name.to_uppercase())
    }

    /// Expand a macro invocation.
    ///
    /// `label` is the label from column 1 (if any).
    /// `name` is the macro name (opcode field).
    /// `operands` is the operand string (positional and keyword args).
    ///
    /// Returns the expanded source lines.
    pub fn expand(
        &mut self,
        label: &str,
        name: &str,
        operands: &str,
    ) -> Result<Vec<String>, MacroError> {
        self.expand_inner(label, name, operands, 0)
    }

    fn expand_inner(
        &mut self,
        label: &str,
        name: &str,
        operands: &str,
        depth: usize,
    ) -> Result<Vec<String>, MacroError> {
        if depth >= self.max_depth {
            return Err(MacroError::TooDeep);
        }

        let def = self
            .macros
            .get(&name.to_uppercase())
            .ok_or_else(|| MacroError::Undefined(name.to_string()))?
            .clone();

        // Increment SYSNDX.
        self.sysndx_counter += 1;
        self.system_vars.sysndx = self.sysndx_counter;

        // Parse arguments.
        let (positional, keyword_args) = parse_arguments(operands);

        // Bind parameters.
        let mut param_values: HashMap<String, String> = HashMap::new();

        // Bind positional parameters.
        for (i, param) in def.params.iter().enumerate() {
            if !param.keyword {
                let value = positional.get(i).cloned().unwrap_or_default();
                param_values.insert(param.name.to_uppercase(), value);
            }
        }

        // Bind keyword parameters (defaults first, then overrides).
        for param in &def.params {
            if param.keyword {
                let key_upper = param.name.to_uppercase();
                let value = keyword_args
                    .get(&strip_ampersand(&key_upper))
                    .cloned()
                    .unwrap_or_else(|| param.default.clone());
                param_values.insert(key_upper, value);
            }
        }

        // Bind label parameter.
        if let Some(ref lp) = def.label_param {
            param_values.insert(lp.to_uppercase(), label.to_string());
        }

        let ctx = ExpansionContext {
            def: def.clone(),
            param_values,
            positional: positional.clone(),
        };

        // Process body lines.
        let mut output = Vec::new();
        let mut i = 0;
        while i < ctx.def.body.len() {
            let line = &ctx.def.body[i];
            let trimmed = line.trim();

            // Skip macro comments (.* lines).
            if trimmed.starts_with(".*") {
                i += 1;
                continue;
            }

            // Check for MEXIT.
            if trimmed.to_uppercase() == "MEXIT"
                || trimmed.to_uppercase().ends_with(" MEXIT")
            {
                break;
            }

            // Check for MNOTE.
            let upper = trimmed.to_uppercase();
            if upper.starts_with("MNOTE ") || upper.contains(" MNOTE ") {
                if let Some(mnote) = self.parse_mnote(trimmed) {
                    self.mnotes.push(mnote);
                }
                i += 1;
                continue;
            }

            // Check for COPY.
            if upper.starts_with("COPY ") || upper.contains(" COPY ") {
                let member_name = extract_copy_member(trimmed);
                if let Some(copy_lines) = self.copy_lib.get_member(&member_name) {
                    for cl in copy_lines {
                        output.push(substitute_line(cl, &ctx, &self.system_vars));
                    }
                }
                i += 1;
                continue;
            }

            // Check for inner macro call.
            let expanded_line = substitute_line(line, &ctx, &self.system_vars);
            let parsed = crate::lexer::parse_source_line(&expanded_line);
            if let crate::lexer::SourceLine::Instruction(ref insn) = parsed {
                if self.is_macro(&insn.opcode) {
                    let inner_label = insn.label.as_deref().unwrap_or("");
                    let inner_ops = &insn.operands;
                    let inner_result =
                        self.expand_inner(inner_label, &insn.opcode, inner_ops, depth + 1)?;
                    output.extend(inner_result);
                    i += 1;
                    continue;
                }
            }

            output.push(expanded_line);
            i += 1;
        }

        Ok(output)
    }

    fn parse_mnote(&self, line: &str) -> Option<Mnote> {
        // MNOTE severity,'message'  or  MNOTE *,'message'
        let mnote_pos = line.to_uppercase().find("MNOTE")?;
        let rest = line[mnote_pos + 5..].trim();
        let (sev_str, msg) = if let Some(comma_pos) = rest.find(',') {
            (rest[..comma_pos].trim(), rest[comma_pos + 1..].trim())
        } else {
            ("0", rest)
        };

        let severity = if sev_str == "*" {
            255
        } else {
            sev_str.parse::<u8>().unwrap_or(0)
        };

        // Strip quotes from message.
        let message = msg.trim_matches('\'').to_string();
        Some(Mnote { severity, message })
    }

    /// Number of registered macros.
    pub fn len(&self) -> usize {
        self.macros.len()
    }

    /// Whether the engine has no macros.
    pub fn is_empty(&self) -> bool {
        self.macros.is_empty()
    }

    /// Get accumulated MNOTE messages.
    pub fn mnotes(&self) -> &[Mnote] {
        &self.mnotes
    }

    /// Clear accumulated MNOTE messages.
    pub fn clear_mnotes(&mut self) {
        self.mnotes.clear();
    }
}

// ---------------------------------------------------------------------------
//  Macro error
// ---------------------------------------------------------------------------

/// Error from the macro engine.
#[derive(Debug, Clone, thiserror::Error)]
pub enum MacroError {
    #[error("Invalid macro definition: {0}")]
    InvalidDefinition(String),
    #[error("Missing MEND")]
    MissingMend,
    #[error("Undefined macro: {0}")]
    Undefined(String),
    #[error("Macro expansion too deep (infinite recursion?)")]
    TooDeep,
}

// ---------------------------------------------------------------------------
//  Parsing helpers
// ---------------------------------------------------------------------------

/// Parse a macro prototype statement.
/// Format: `&LABEL  MACNAME  &P1,&P2,&KEY=DEFAULT`
fn parse_prototype(proto: &str) -> Result<(Option<String>, String, Vec<MacroParam>), MacroError> {
    let trimmed = proto.trim();
    if trimmed.is_empty() {
        return Err(MacroError::InvalidDefinition("Empty prototype".to_string()));
    }

    let tokens: Vec<&str> = trimmed.split_whitespace().collect();
    if tokens.is_empty() {
        return Err(MacroError::InvalidDefinition("Empty prototype".to_string()));
    }

    let (label_param, name_idx) = if tokens[0].starts_with('&') {
        (Some(tokens[0].to_string()), 1)
    } else {
        (None, 0)
    };

    if name_idx >= tokens.len() {
        return Err(MacroError::InvalidDefinition(
            "No macro name in prototype".to_string(),
        ));
    }

    let name = tokens[name_idx].to_string();

    // Parse parameters from remaining tokens.
    let param_str = if name_idx + 1 < tokens.len() {
        tokens[name_idx + 1..].join(" ")
    } else {
        String::new()
    };

    let params = parse_param_list(&param_str);
    Ok((label_param, name, params))
}

/// Parse a parameter list like `&P1,&P2,&KEY=DEFAULT`.
fn parse_param_list(s: &str) -> Vec<MacroParam> {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return Vec::new();
    }

    let mut params = Vec::new();
    for part in split_params(trimmed) {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        if let Some(eq_pos) = part.find('=') {
            let name = part[..eq_pos].trim().to_string();
            let default = part[eq_pos + 1..].trim().to_string();
            params.push(MacroParam {
                name,
                keyword: true,
                default,
            });
        } else {
            params.push(MacroParam {
                name: part.to_string(),
                keyword: false,
                default: String::new(),
            });
        }
    }
    params
}

/// Split parameter/argument list on commas, respecting parentheses and quotes.
fn split_params(s: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_quotes = false;
    let mut paren_depth: i32 = 0;

    for ch in s.chars() {
        match ch {
            '\'' => {
                in_quotes = !in_quotes;
                current.push(ch);
            }
            '(' if !in_quotes => {
                paren_depth += 1;
                current.push(ch);
            }
            ')' if !in_quotes => {
                paren_depth -= 1;
                current.push(ch);
            }
            ',' if !in_quotes && paren_depth == 0 => {
                parts.push(current.clone());
                current.clear();
            }
            _ => {
                current.push(ch);
            }
        }
    }
    if !current.is_empty() {
        parts.push(current);
    }
    parts
}

/// Parse macro invocation arguments into positional and keyword args.
fn parse_arguments(operands: &str) -> (Vec<String>, HashMap<String, String>) {
    let trimmed = operands.trim();
    if trimmed.is_empty() {
        return (Vec::new(), HashMap::new());
    }

    let parts = split_params(trimmed);
    let mut positional = Vec::new();
    let mut keyword = HashMap::new();

    for part in parts {
        let part = part.trim().to_string();
        // Check for keyword argument: NAME=VALUE (no & prefix in the call).
        if let Some(eq_pos) = part.find('=') {
            let key = part[..eq_pos].trim().to_uppercase();
            let value = part[eq_pos + 1..].trim().to_string();
            keyword.insert(key, value);
        } else {
            positional.push(part);
        }
    }

    (positional, keyword)
}

/// Strip leading & from a parameter name.
fn strip_ampersand(name: &str) -> String {
    if let Some(stripped) = name.strip_prefix('&') {
        stripped.to_string()
    } else {
        name.to_string()
    }
}

/// Extract the COPY member name from a COPY line.
fn extract_copy_member(line: &str) -> String {
    let upper = line.to_uppercase();
    if let Some(pos) = upper.find("COPY") {
        line[pos + 4..].trim().to_string()
    } else {
        String::new()
    }
}

/// Perform variable substitution on a single line.
///
/// Replaces &VAR references with their values from the expansion context.
/// Handles:
/// - &PARAM → parameter value
/// - &SYSNDX, &SYSECT, etc. → system variable values
/// - &SYSLIST(n) → positional parameter by index (1-based)
/// - Period after variable name terminates the variable: `&VAR.SUFFIX`
fn substitute_line(line: &str, ctx: &ExpansionContext, sys: &SystemVars) -> String {
    let mut result = String::with_capacity(line.len());
    let chars: Vec<char> = line.chars().collect();
    let len = chars.len();
    let mut i = 0;

    while i < len {
        if chars[i] == '&' && i + 1 < len && (chars[i + 1].is_alphabetic() || chars[i + 1] == '&') {
            // Ampersand doubling: && → &
            if chars[i + 1] == '&' {
                result.push('&');
                i += 2;
                continue;
            }

            // Collect variable name.
            let start = i;
            i += 1; // skip &
            let mut var_name = String::from("&");
            while i < len && (chars[i].is_alphanumeric() || chars[i] == '_') {
                var_name.push(chars[i]);
                i += 1;
            }

            // Check for &SYSLIST(n).
            if var_name.to_uppercase() == "&SYSLIST" && i < len && chars[i] == '(' {
                i += 1; // skip (
                let mut idx_str = String::new();
                while i < len && chars[i] != ')' {
                    idx_str.push(chars[i]);
                    i += 1;
                }
                if i < len {
                    i += 1; // skip )
                }
                // Consume trailing period.
                if i < len && chars[i] == '.' {
                    i += 1;
                }
                if let Ok(idx) = idx_str.parse::<usize>() {
                    if idx > 0 && idx <= ctx.positional.len() {
                        result.push_str(&ctx.positional[idx - 1]);
                    }
                }
                continue;
            }

            // Try to resolve.
            let var_upper = var_name.to_uppercase();
            let value = ctx
                .param_values
                .get(&var_upper)
                .cloned()
                .or_else(|| sys.resolve(&var_upper))
                .unwrap_or_else(|| {
                    // Unresolved — keep original.
                    line[start..i].to_string()
                });

            result.push_str(&value);

            // Consume trailing period (concatenation terminator).
            if i < len && chars[i] == '.' {
                i += 1;
            }
        } else {
            result.push(chars[i]);
            i += 1;
        }
    }

    result
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn make_engine() -> MacroEngine {
        MacroEngine::new()
    }

    #[test]
    fn test_parse_prototype_simple() {
        let (label, name, params) = parse_prototype("         MYMAC &P1,&P2").unwrap();
        assert!(label.is_none());
        assert_eq!(name, "MYMAC");
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "&P1");
        assert_eq!(params[1].name, "&P2");
        assert!(!params[0].keyword);
    }

    #[test]
    fn test_parse_prototype_with_label() {
        let (label, name, params) = parse_prototype("&LBL     MYMAC &P1,&KEY=DEFAULT").unwrap();
        assert_eq!(label, Some("&LBL".to_string()));
        assert_eq!(name, "MYMAC");
        assert_eq!(params.len(), 2);
        assert!(!params[0].keyword);
        assert!(params[1].keyword);
        assert_eq!(params[1].default, "DEFAULT");
    }

    #[test]
    fn test_parse_macro_definition() {
        let engine = make_engine();
        let lines = vec![
            "         SAVE  &P1".to_string(),
            "         STM   14,12,12(13)".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        assert_eq!(def.name, "SAVE");
        assert_eq!(def.params.len(), 1);
        assert_eq!(def.body.len(), 1);
        assert!(def.body[0].contains("STM"));
    }

    #[test]
    fn test_parse_macro_missing_mend() {
        let engine = make_engine();
        let lines = vec![
            "         SAVE  &P1".to_string(),
            "         STM   14,12,12(13)".to_string(),
        ];
        assert!(engine.parse_macro(&lines).is_err());
    }

    #[test]
    fn test_simple_expansion() {
        let mut engine = make_engine();
        let lines = vec![
            "         INIT  &REG".to_string(),
            "         SR    &REG,&REG".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("", "INIT", "3").unwrap();
        assert_eq!(result.len(), 1);
        assert!(result[0].contains("3,3"));
    }

    #[test]
    fn test_keyword_parameter() {
        let mut engine = make_engine();
        let lines = vec![
            "         SETUP &REG,&BASE=13".to_string(),
            "         LR    &REG,&BASE".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        // Use default.
        let result = engine.expand("", "SETUP", "3").unwrap();
        assert!(result[0].contains("3,13"));

        // Override.
        let result = engine.expand("", "SETUP", "3,BASE=12").unwrap();
        assert!(result[0].contains("3,12"));
    }

    #[test]
    fn test_label_parameter() {
        let mut engine = make_engine();
        let lines = vec![
            "&LBL     ENTRY &REG".to_string(),
            "&LBL     DS    0H".to_string(),
            "         STM   14,&REG,12(13)".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("MYSUB", "ENTRY", "12").unwrap();
        assert_eq!(result.len(), 2);
        assert!(result[0].contains("MYSUB"));
    }

    #[test]
    fn test_sysndx_substitution() {
        let mut engine = make_engine();
        let lines = vec![
            "         GEN".to_string(),
            "LBL&SYSNDX DS 0H".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result1 = engine.expand("", "GEN", "").unwrap();
        assert!(result1[0].contains("0001"), "Got: {}", result1[0]);

        let result2 = engine.expand("", "GEN", "").unwrap();
        assert!(result2[0].contains("0002"), "Got: {}", result2[0]);
    }

    #[test]
    fn test_syslist_access() {
        let mut engine = make_engine();
        let lines = vec![
            "         MULTI &A,&B,&C".to_string(),
            "         DC    C'&SYSLIST(1)'".to_string(),
            "         DC    C'&SYSLIST(3)'".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("", "MULTI", "ALPHA,BETA,GAMMA").unwrap();
        assert!(result[0].contains("ALPHA"), "Got: {}", result[0]);
        assert!(result[1].contains("GAMMA"), "Got: {}", result[1]);
    }

    #[test]
    fn test_mexit() {
        let mut engine = make_engine();
        let lines = vec![
            "         EARLY &P1".to_string(),
            "         LR    1,&P1".to_string(),
            "         MEXIT".to_string(),
            "         LR    2,&P1".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("", "EARLY", "5").unwrap();
        assert_eq!(result.len(), 1);
        assert!(result[0].contains("1,5"));
    }

    #[test]
    fn test_mnote() {
        let mut engine = make_engine();
        let lines = vec![
            "         WARN &P1".to_string(),
            "         MNOTE 4,'Missing parameter'".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        engine.expand("", "WARN", "").unwrap();
        assert_eq!(engine.mnotes().len(), 1);
        assert_eq!(engine.mnotes()[0].severity, 4);
        assert_eq!(engine.mnotes()[0].message, "Missing parameter");
    }

    #[test]
    fn test_copy_inclusion() {
        let mut engine = make_engine();
        engine
            .copy_lib
            .add_member("REGS", vec!["R0       EQU   0".to_string(), "R1       EQU   1".to_string()]);

        let lines = vec![
            "         EQUREGS".to_string(),
            "         COPY REGS".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("", "EQUREGS", "").unwrap();
        assert_eq!(result.len(), 2);
        assert!(result[0].contains("R0"));
        assert!(result[1].contains("R1"));
    }

    #[test]
    fn test_inner_macro() {
        let mut engine = make_engine();

        // Inner macro.
        let inner = vec![
            "         INNER &R".to_string(),
            "         SR    &R,&R".to_string(),
            "         MEND".to_string(),
        ];
        let def_inner = engine.parse_macro(&inner).unwrap();
        engine.define_macro(def_inner);

        // Outer macro calling inner.
        let outer = vec![
            "         OUTER &R".to_string(),
            "         INNER &R".to_string(),
            "         LR    &R,&R".to_string(),
            "         MEND".to_string(),
        ];
        let def_outer = engine.parse_macro(&outer).unwrap();
        engine.define_macro(def_outer);

        let result = engine.expand("", "OUTER", "5").unwrap();
        assert_eq!(result.len(), 2);
        assert!(result[0].contains("SR") && result[0].contains("5,5"));
        assert!(result[1].contains("LR") && result[1].contains("5,5"));
    }

    #[test]
    fn test_macro_comment_skipped() {
        let mut engine = make_engine();
        let lines = vec![
            "         MYDEF &P1".to_string(),
            ".* This is a macro comment".to_string(),
            "         LR    1,&P1".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("", "MYDEF", "5").unwrap();
        assert_eq!(result.len(), 1);
        assert!(result[0].contains("1,5"));
    }

    #[test]
    fn test_ampersand_doubling() {
        let mut engine = make_engine();
        let lines = vec![
            "         ADDR".to_string(),
            "         DC    C'A&&B'".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("", "ADDR", "").unwrap();
        assert!(result[0].contains("A&B"), "Got: {}", result[0]);
    }

    #[test]
    fn test_period_concatenation() {
        let mut engine = make_engine();
        let lines = vec![
            "         PREFIX &PFX".to_string(),
            "&PFX.DATA DC  F'0'".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("", "PREFIX", "MY").unwrap();
        assert!(result[0].starts_with("MYDATA"), "Got: {}", result[0]);
    }

    #[test]
    fn test_system_vars_resolve() {
        let sys = SystemVars::default();
        assert_eq!(sys.resolve("&SYSASM"), Some("OPEN MAINFRAME HLASM".to_string()));
        assert_eq!(sys.resolve("&SYSVER"), Some("1.6.0".to_string()));
        assert!(sys.resolve("&NOSUCH").is_none());
    }

    #[test]
    fn test_engine_len() {
        let mut engine = make_engine();
        assert!(engine.is_empty());
        let lines = vec![
            "         TEST".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);
        assert_eq!(engine.len(), 1);
    }

    #[test]
    fn test_no_params() {
        let mut engine = make_engine();
        let lines = vec![
            "         NOPARM".to_string(),
            "         NOP".to_string(),
            "         MEND".to_string(),
        ];
        let def = engine.parse_macro(&lines).unwrap();
        engine.define_macro(def);

        let result = engine.expand("", "NOPARM", "").unwrap();
        assert_eq!(result.len(), 1);
        assert!(result[0].contains("NOP"));
    }
}
