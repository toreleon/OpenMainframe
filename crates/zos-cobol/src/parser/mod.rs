//! COBOL recursive descent parser.
//!
//! This module implements a hand-written recursive descent parser for COBOL.
//! It consumes tokens produced by the lexer and builds an AST.
//!
//! The parser supports error recovery to report multiple errors per compilation.

use crate::ast::*;
use crate::error::CobolError;
use crate::lexer::{Keyword, Span, Token, TokenKind};

/// Result type for parser operations.
pub type Result<T> = std::result::Result<T, CobolError>;

/// The COBOL parser.
pub struct Parser {
    /// Token stream.
    tokens: Vec<Token>,
    /// Current position in the token stream.
    current: usize,
    /// Accumulated errors (for error recovery).
    errors: Vec<CobolError>,
}

impl Parser {
    /// Create a new parser from a token stream.
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    /// Parse a complete COBOL program.
    pub fn parse_program(mut self) -> (Option<Program>, Vec<CobolError>) {
        match self.parse_program_inner() {
            Ok(program) => (Some(program), self.errors),
            Err(e) => {
                self.errors.push(e);
                (None, self.errors)
            }
        }
    }

    fn parse_program_inner(&mut self) -> Result<Program> {
        let start = self.current_span();

        // IDENTIFICATION DIVISION is required
        let identification = self.parse_identification_division()?;

        // ENVIRONMENT DIVISION is optional
        let environment = if self.check_keyword(Keyword::Environment) {
            Some(self.parse_environment_division()?)
        } else {
            None
        };

        // DATA DIVISION is optional
        let data = if self.check_keyword(Keyword::Data) {
            Some(self.parse_data_division()?)
        } else {
            None
        };

        // PROCEDURE DIVISION is optional (for copybooks)
        let procedure = if self.check_keyword(Keyword::Procedure) {
            Some(self.parse_procedure_division()?)
        } else {
            None
        };

        // Check for END PROGRAM
        if self.check_keyword(Keyword::EndProgram) {
            self.advance(); // END
            self.expect_keyword(Keyword::Program)?;
            // Optional program name
            if self.check_identifier() {
                self.advance();
            }
            self.expect(TokenKind::Period)?;
        }

        let end = self.previous_span();

        Ok(Program {
            identification,
            environment,
            data,
            procedure,
            span: start.extend(end),
        })
    }

    // ========================================================================
    // IDENTIFICATION DIVISION
    // ========================================================================

    fn parse_identification_division(&mut self) -> Result<IdentificationDivision> {
        let start = self.current_span();

        // IDENTIFICATION DIVISION.
        self.expect_keyword(Keyword::Identification)?;
        self.expect_keyword(Keyword::Division)?;
        self.expect(TokenKind::Period)?;

        // PROGRAM-ID. name.
        self.expect_keyword(Keyword::ProgramId)?;
        self.expect(TokenKind::Period)?;

        let program_name = self.expect_identifier()?;
        let mut is_common = false;
        let mut is_initial = false;

        // Check for IS COMMON/INITIAL
        if self.check_keyword(Keyword::Is) {
            self.advance();
            if self.check_keyword(Keyword::Common) {
                self.advance();
                is_common = true;
            }
            if self.check_keyword(Keyword::Initial) {
                self.advance();
                is_initial = true;
            }
            if self.check_keyword(Keyword::Program) {
                self.advance();
            }
        }

        self.expect(TokenKind::Period)?;

        let program_id = ProgramId {
            name: program_name.clone(),
            is_common,
            is_initial,
            span: start.extend(self.previous_span()),
        };

        // Parse optional paragraphs
        let mut author = None;
        let mut installation = None;
        let mut date_written = None;
        let mut date_compiled = None;
        let mut security = None;

        while !self.is_at_division_start() && !self.is_at_end() {
            if self.check_keyword(Keyword::Author) {
                self.advance();
                self.skip_if(TokenKind::Period);
                author = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::Installation) {
                self.advance();
                self.skip_if(TokenKind::Period);
                installation = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::DateWritten) {
                self.advance();
                self.skip_if(TokenKind::Period);
                date_written = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::DateCompiled) {
                self.advance();
                self.skip_if(TokenKind::Period);
                date_compiled = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::Security) {
                self.advance();
                self.skip_if(TokenKind::Period);
                security = Some(self.consume_until_period());
            } else {
                // Unknown paragraph, skip to next period
                self.advance_to_next_sentence();
            }
        }

        let end = self.previous_span();

        Ok(IdentificationDivision {
            program_id,
            author,
            installation,
            date_written,
            date_compiled,
            security,
            span: start.extend(end),
        })
    }

    // ========================================================================
    // ENVIRONMENT DIVISION
    // ========================================================================

    fn parse_environment_division(&mut self) -> Result<EnvironmentDivision> {
        let start = self.current_span();

        // ENVIRONMENT DIVISION.
        self.expect_keyword(Keyword::Environment)?;
        self.expect_keyword(Keyword::Division)?;
        self.expect(TokenKind::Period)?;

        let mut configuration = None;
        let mut input_output = None;

        // CONFIGURATION SECTION
        if self.check_keyword(Keyword::Configuration) {
            configuration = Some(self.parse_configuration_section()?);
        }

        // INPUT-OUTPUT SECTION
        if self.check_keyword(Keyword::InputOutput) {
            input_output = Some(self.parse_input_output_section()?);
        }

        let end = self.previous_span();

        Ok(EnvironmentDivision {
            configuration,
            input_output,
            span: start.extend(end),
        })
    }

    fn parse_configuration_section(&mut self) -> Result<ConfigurationSection> {
        let start = self.current_span();

        // CONFIGURATION SECTION.
        self.expect_keyword(Keyword::Configuration)?;
        self.expect_keyword(Keyword::Section)?;
        self.expect(TokenKind::Period)?;

        let mut source_computer = None;
        let mut object_computer = None;
        let special_names = Vec::new();

        while !self.is_at_section_start() && !self.is_at_division_start() && !self.is_at_end() {
            if self.check_keyword(Keyword::SourceComputer) {
                self.advance();
                self.skip_if(TokenKind::Period);
                source_computer = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::ObjectComputer) {
                self.advance();
                self.skip_if(TokenKind::Period);
                object_computer = Some(self.consume_until_period());
            } else if self.check_keyword(Keyword::SpecialNames) {
                self.advance();
                self.skip_if(TokenKind::Period);
                // TODO: Parse special-names entries
                self.advance_to_next_sentence();
            } else {
                self.advance_to_next_sentence();
            }
        }

        let end = self.previous_span();

        Ok(ConfigurationSection {
            source_computer,
            object_computer,
            special_names,
            span: start.extend(end),
        })
    }

    fn parse_input_output_section(&mut self) -> Result<InputOutputSection> {
        let start = self.current_span();

        // INPUT-OUTPUT SECTION.
        self.expect_keyword(Keyword::InputOutput)?;
        self.expect_keyword(Keyword::Section)?;
        self.expect(TokenKind::Period)?;

        let mut file_control = Vec::new();

        // FILE-CONTROL.
        if self.check_keyword(Keyword::FileControl) {
            self.advance();
            self.expect(TokenKind::Period)?;

            // Parse SELECT statements
            while self.check_keyword(Keyword::Select) {
                file_control.push(self.parse_file_control_entry()?);
            }
        }

        let end = self.previous_span();

        Ok(InputOutputSection {
            file_control,
            span: start.extend(end),
        })
    }

    fn parse_file_control_entry(&mut self) -> Result<FileControlEntry> {
        let start = self.current_span();

        // SELECT file-name
        self.expect_keyword(Keyword::Select)?;
        let file_name = self.expect_identifier()?;

        // ASSIGN TO external-name
        self.expect_keyword(Keyword::Assign)?;
        if self.check_keyword(Keyword::To) {
            self.advance();
        }
        let assign_to = self.expect_identifier_or_string()?;

        let mut organization = FileOrganization::Sequential;
        let mut access_mode = AccessMode::Sequential;
        let mut record_key = None;
        let mut file_status = None;

        // Parse optional clauses until period
        while !self.check(TokenKind::Period) && !self.is_at_end() {
            if self.check_keyword(Keyword::Organization) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                organization = self.parse_file_organization()?;
            } else if self.check_keyword(Keyword::AccessMode) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                access_mode = self.parse_access_mode()?;
            } else if self.check_keyword(Keyword::RecordKey) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                record_key = Some(self.parse_qualified_name()?);
            } else if self.check_keyword(Keyword::FileStatus) || self.check_keyword(Keyword::File) {
                if self.check_keyword(Keyword::File) {
                    self.advance(); // FILE
                    self.expect_keyword(Keyword::Storage)?; // STATUS is sometimes "FILE STATUS"
                }
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                file_status = Some(self.parse_qualified_name()?);
            } else {
                // Skip unknown clause
                self.advance();
            }
        }

        self.expect(TokenKind::Period)?;

        let end = self.previous_span();

        Ok(FileControlEntry {
            file_name,
            assign_to,
            organization,
            access_mode,
            record_key,
            file_status,
            span: start.extend(end),
        })
    }

    fn parse_file_organization(&mut self) -> Result<FileOrganization> {
        if self.check_keyword(Keyword::Sequential) {
            self.advance();
            Ok(FileOrganization::Sequential)
        } else if self.check_keyword(Keyword::Indexed) {
            self.advance();
            Ok(FileOrganization::Indexed)
        } else if self.check_keyword(Keyword::Relative) {
            self.advance();
            Ok(FileOrganization::Relative)
        } else {
            Ok(FileOrganization::Sequential)
        }
    }

    fn parse_access_mode(&mut self) -> Result<AccessMode> {
        if self.check_keyword(Keyword::Sequential) {
            self.advance();
            Ok(AccessMode::Sequential)
        } else if self.check_keyword(Keyword::Random) {
            self.advance();
            Ok(AccessMode::Random)
        } else if self.check_keyword(Keyword::Dynamic) {
            self.advance();
            Ok(AccessMode::Dynamic)
        } else {
            Ok(AccessMode::Sequential)
        }
    }

    // ========================================================================
    // DATA DIVISION
    // ========================================================================

    fn parse_data_division(&mut self) -> Result<DataDivision> {
        let start = self.current_span();

        // DATA DIVISION.
        self.expect_keyword(Keyword::Data)?;
        self.expect_keyword(Keyword::Division)?;
        self.expect(TokenKind::Period)?;

        let mut file_section = Vec::new();
        let mut working_storage = Vec::new();
        let mut local_storage = Vec::new();
        let mut linkage = Vec::new();

        // Parse sections
        while !self.is_at_division_start() && !self.is_at_end() {
            if self.check_keyword(Keyword::File) {
                self.advance();
                self.expect_keyword(Keyword::Section)?;
                self.expect(TokenKind::Period)?;
                file_section = self.parse_file_section()?;
            } else if self.check_keyword(Keyword::WorkingStorage) {
                self.advance(); // WORKING-STORAGE
                self.expect_keyword(Keyword::Section)?;
                self.expect(TokenKind::Period)?;
                working_storage = self.parse_data_items()?;
            } else if self.check_keyword(Keyword::LocalStorage) {
                self.advance();
                self.expect_keyword(Keyword::Section)?;
                self.expect(TokenKind::Period)?;
                local_storage = self.parse_data_items()?;
            } else if self.check_keyword(Keyword::Linkage) {
                self.advance();
                self.expect_keyword(Keyword::Section)?;
                self.expect(TokenKind::Period)?;
                linkage = self.parse_data_items()?;
            } else {
                self.advance_to_next_sentence();
            }
        }

        let end = self.previous_span();

        Ok(DataDivision {
            file_section,
            working_storage,
            local_storage,
            linkage,
            span: start.extend(end),
        })
    }

    fn parse_file_section(&mut self) -> Result<Vec<FileDescription>> {
        let mut files = Vec::new();

        while self.check_keyword(Keyword::Fd) || self.check_keyword(Keyword::Sd) {
            files.push(self.parse_file_description()?);
        }

        Ok(files)
    }

    fn parse_file_description(&mut self) -> Result<FileDescription> {
        let start = self.current_span();

        let is_sort_file = self.check_keyword(Keyword::Sd);
        self.advance(); // FD or SD

        let name = self.expect_identifier()?;

        // Parse FD clauses until period
        let record_contains = None;
        let block_contains = None;

        while !self.check(TokenKind::Period) && !self.is_at_end() {
            if self.check_keyword(Keyword::Record) {
                self.advance();
                // RECORD CONTAINS clause
                // Skip for now
                while !self.check(TokenKind::Period) && !self.is_at_end() {
                    self.advance();
                }
            } else {
                self.advance();
            }
        }

        self.expect(TokenKind::Period)?;

        // Parse record descriptions
        let records = self.parse_data_items()?;

        let end = self.previous_span();

        Ok(FileDescription {
            name,
            is_sort_file,
            records,
            record_contains,
            block_contains,
            span: start.extend(end),
        })
    }

    fn parse_data_items(&mut self) -> Result<Vec<DataItem>> {
        let mut items = Vec::new();

        while self.check_level_number() {
            items.push(self.parse_data_item()?);
        }

        Ok(items)
    }

    fn parse_data_item(&mut self) -> Result<DataItem> {
        let start = self.current_span();

        // Level number
        let level = self.expect_level_number()?;

        // Data name or FILLER
        let name = if self.check_keyword(Keyword::Filler) {
            self.advance();
            DataItemName::Filler
        } else if self.check_identifier() {
            DataItemName::Named(self.expect_identifier()?)
        } else {
            DataItemName::Filler // Implicit FILLER
        };

        let mut picture = None;
        let mut usage = None;
        let mut value = None;
        let mut occurs = None;
        let mut redefines = None;
        let mut sign = None;
        let mut justified = false;
        let mut blank_when_zero = false;

        // Parse clauses until period
        while !self.check(TokenKind::Period) && !self.is_at_end() {
            if self.check_keyword(Keyword::Pic) || self.check_keyword(Keyword::Picture) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                picture = Some(self.parse_picture_clause()?);
            } else if self.check_keyword(Keyword::Usage) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                usage = Some(self.parse_usage()?);
            } else if self.check_keyword(Keyword::Value) {
                self.advance();
                if self.check_keyword(Keyword::Is) {
                    self.advance();
                }
                value = Some(self.parse_literal()?);
            } else if self.check_keyword(Keyword::Occurs) {
                self.advance();
                occurs = Some(self.parse_occurs_clause()?);
            } else if self.check_keyword(Keyword::Redefines) {
                self.advance();
                redefines = Some(self.parse_qualified_name()?);
            } else if self.check_keyword(Keyword::Sign) {
                self.advance();
                sign = Some(self.parse_sign_clause()?);
            } else if self.check_keyword(Keyword::Justified) || self.check_keyword(Keyword::Just) {
                self.advance();
                if self.check_keyword(Keyword::Right) {
                    self.advance();
                }
                justified = true;
            } else if self.check_keyword(Keyword::Blank) {
                self.advance();
                if self.check_keyword(Keyword::When) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Zero) {
                    self.advance();
                }
                blank_when_zero = true;
            } else if self.is_usage_keyword() {
                // Implicit USAGE
                usage = Some(self.parse_usage()?);
            } else {
                // Unknown clause, skip
                self.advance();
            }
        }

        self.expect(TokenKind::Period)?;

        // Parse subordinate items (for group items)
        let mut children = Vec::new();
        let mut condition_values = Vec::new();

        // If this is a group item (no PIC), parse children
        if picture.is_none() && level < 77 && level != 66 && level != 88 {
            while self.check_level_number() {
                let child_level = self.peek_level_number();
                if child_level > level || child_level == 88 {
                    let child = self.parse_data_item()?;
                    if child.level == 88 {
                        condition_values.push(ConditionValue {
                            name: child.name.as_str().unwrap_or("").to_string(),
                            values: vec![], // TODO: Parse condition values
                            span: child.span,
                        });
                    } else {
                        children.push(child);
                    }
                } else {
                    break;
                }
            }
        }

        let end = self.previous_span();

        Ok(DataItem {
            level,
            name,
            picture,
            usage,
            value,
            occurs,
            redefines,
            sign,
            justified,
            blank_when_zero,
            children,
            condition_values,
            span: start.extend(end),
        })
    }

    fn parse_picture_clause(&mut self) -> Result<PictureClause> {
        let start = self.current_span();

        let picture = if let TokenKind::PictureString(s) = &self.current().kind {
            let pic = s.clone();
            self.advance();
            pic
        } else {
            // Fallback: collect tokens until next clause
            let mut pic = String::new();
            while !self.is_data_clause_start()
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
            {
                if let TokenKind::Identifier(s) = &self.current().kind {
                    pic.push_str(s);
                } else if let TokenKind::IntegerLiteral(n) = &self.current().kind {
                    pic.push_str(&n.to_string());
                } else if let TokenKind::LeftParen = &self.current().kind {
                    pic.push('(');
                } else if let TokenKind::RightParen = &self.current().kind {
                    pic.push(')');
                }
                self.advance();
            }
            pic
        };

        // Analyze picture to determine category and size
        let (category, size, decimal_positions) = analyze_picture(&picture);

        let end = self.previous_span();

        Ok(PictureClause {
            picture,
            category,
            size,
            decimal_positions,
            span: start.extend(end),
        })
    }

    fn parse_usage(&mut self) -> Result<Usage> {
        if self.check_keyword(Keyword::Display) {
            self.advance();
            Ok(Usage::Display)
        } else if self.check_keyword(Keyword::Binary)
            || self.check_keyword(Keyword::Comp)
            || self.check_keyword(Keyword::Comp4)
            || self.check_keyword(Keyword::Computational)
            || self.check_keyword(Keyword::Computational4)
        {
            self.advance();
            Ok(Usage::Binary)
        } else if self.check_keyword(Keyword::Comp1) || self.check_keyword(Keyword::Computational1)
        {
            self.advance();
            Ok(Usage::Comp1)
        } else if self.check_keyword(Keyword::Comp2) || self.check_keyword(Keyword::Computational2)
        {
            self.advance();
            Ok(Usage::Comp2)
        } else if self.check_keyword(Keyword::Comp3)
            || self.check_keyword(Keyword::Computational3)
            || self.check_keyword(Keyword::PackedDecimal)
        {
            self.advance();
            Ok(Usage::PackedDecimal)
        } else if self.check_keyword(Keyword::Comp5) || self.check_keyword(Keyword::Computational5)
        {
            self.advance();
            Ok(Usage::Comp5)
        } else if self.check_keyword(Keyword::Pointer) {
            self.advance();
            Ok(Usage::Pointer)
        } else if self.check_keyword(Keyword::Indexed) {
            self.advance();
            Ok(Usage::Index)
        } else {
            Ok(Usage::Display)
        }
    }

    fn parse_occurs_clause(&mut self) -> Result<OccursClause> {
        let start = self.current_span();

        // n TIMES or n TO m TIMES
        let times = self.expect_integer()? as u32;
        let mut max_times = None;
        let mut depending_on = None;
        let mut indexed_by = Vec::new();
        let mut keys = Vec::new();

        if self.check_keyword(Keyword::To) {
            self.advance();
            max_times = Some(self.expect_integer()? as u32);
        }

        if self.check_keyword(Keyword::Times) {
            self.advance();
        }

        // DEPENDING ON
        if self.check_keyword(Keyword::Depending) {
            self.advance();
            if self.check_keyword(Keyword::On) {
                self.advance();
            }
            depending_on = Some(self.parse_qualified_name()?);
        }

        // ASCENDING/DESCENDING KEY
        while self.check_keyword(Keyword::Ascending) || self.check_keyword(Keyword::Descending) {
            let ascending = self.check_keyword(Keyword::Ascending);
            self.advance();
            if self.check_keyword(Keyword::Key) {
                self.advance();
            }
            if self.check_keyword(Keyword::Is) {
                self.advance();
            }
            let key_name = self.parse_qualified_name()?;
            keys.push(OccursKey {
                name: key_name.clone(),
                ascending,
                span: key_name.span,
            });
        }

        // INDEXED BY
        if self.check_keyword(Keyword::Indexed) {
            self.advance();
            if self.check_keyword(Keyword::By) {
                self.advance();
            }
            while self.check_identifier() {
                indexed_by.push(self.expect_identifier()?);
            }
        }

        let end = self.previous_span();

        Ok(OccursClause {
            times,
            max_times,
            depending_on,
            indexed_by,
            keys,
            span: start.extend(end),
        })
    }

    fn parse_sign_clause(&mut self) -> Result<SignClause> {
        let mut leading = true;
        let mut separate = false;

        if self.check_keyword(Keyword::Is) {
            self.advance();
        }

        if self.check_keyword(Keyword::Leading) {
            self.advance();
            leading = true;
        } else if self.check_keyword(Keyword::Trailing) {
            self.advance();
            leading = false;
        }

        if self.check_keyword(Keyword::Separate) {
            self.advance();
            separate = true;
            if self.check_keyword(Keyword::Character) {
                self.advance();
            }
        }

        Ok(SignClause { leading, separate })
    }

    // ========================================================================
    // PROCEDURE DIVISION (Stub - to be expanded)
    // ========================================================================

    fn parse_procedure_division(&mut self) -> Result<ProcedureDivision> {
        let start = self.current_span();

        // PROCEDURE DIVISION
        self.expect_keyword(Keyword::Procedure)?;
        self.expect_keyword(Keyword::Division)?;

        let mut using = Vec::new();
        let mut returning = None;

        // USING clause
        if self.check_keyword(Keyword::Using) {
            self.advance();
            while self.check_identifier()
                || self.check_keyword(Keyword::Reference)
                || self.check_keyword(Keyword::Content)
                || self.check_keyword(Keyword::Value)
            {
                let mode = if self.check_keyword(Keyword::By) {
                    self.advance();
                    if self.check_keyword(Keyword::Reference) {
                        self.advance();
                        ParameterMode::Reference
                    } else if self.check_keyword(Keyword::Content) {
                        self.advance();
                        ParameterMode::Content
                    } else if self.check_keyword(Keyword::Value) {
                        self.advance();
                        ParameterMode::Value
                    } else {
                        ParameterMode::Reference
                    }
                } else if self.check_keyword(Keyword::Reference)
                    || self.check_keyword(Keyword::Content)
                    || self.check_keyword(Keyword::Value)
                {
                    if self.check_keyword(Keyword::Reference) {
                        self.advance();
                        ParameterMode::Reference
                    } else if self.check_keyword(Keyword::Content) {
                        self.advance();
                        ParameterMode::Content
                    } else {
                        self.advance();
                        ParameterMode::Value
                    }
                } else {
                    ParameterMode::Reference
                };

                if self.check_identifier() {
                    let name = self.parse_qualified_name()?;
                    using.push(UsingParameter {
                        name: name.clone(),
                        mode,
                        span: name.span,
                    });
                }
            }
        }

        // RETURNING clause
        if self.check_keyword(Keyword::Return) {
            self.advance();
            returning = Some(self.parse_qualified_name()?);
        }

        self.expect(TokenKind::Period)?;

        // Parse procedure body
        let body = self.parse_procedure_body()?;

        let end = self.previous_span();

        Ok(ProcedureDivision {
            using,
            returning,
            body,
            span: start.extend(end),
        })
    }

    fn parse_procedure_body(&mut self) -> Result<ProcedureBody> {
        let mut sections = Vec::new();
        let mut paragraphs = Vec::new();
        let mut statements = Vec::new();

        while !self.is_at_end() && !self.check_keyword(Keyword::EndProgram) {
            // Check for section header
            if self.check_identifier() && self.peek_keyword(Keyword::Section) {
                let section = self.parse_section()?;
                sections.push(section);
            }
            // Check for paragraph header
            else if self.check_identifier() && self.peek(TokenKind::Period) {
                let para = self.parse_paragraph()?;
                paragraphs.push(para);
            }
            // Otherwise parse statement
            else if !self.check(TokenKind::Eof) {
                match self.parse_statement() {
                    Ok(stmt) => statements.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        self.advance_to_next_sentence();
                    }
                }
            } else {
                break;
            }
        }

        if !sections.is_empty() {
            Ok(ProcedureBody::Sections(sections))
        } else if !paragraphs.is_empty() {
            Ok(ProcedureBody::Paragraphs(paragraphs))
        } else {
            Ok(ProcedureBody::Statements(statements))
        }
    }

    fn parse_section(&mut self) -> Result<Section> {
        let start = self.current_span();
        let name = self.expect_identifier()?;
        self.expect_keyword(Keyword::Section)?;
        self.expect(TokenKind::Period)?;

        let mut paragraphs = Vec::new();

        while !(self.is_at_end()
            || self.check_keyword(Keyword::EndProgram)
            || (self.check_identifier() && self.peek_keyword(Keyword::Section)))
        {
            if self.check_identifier() && self.peek(TokenKind::Period) {
                paragraphs.push(self.parse_paragraph()?);
            } else if !self.check(TokenKind::Eof) {
                // Inline statement in section
                break;
            } else {
                break;
            }
        }

        let end = self.previous_span();

        Ok(Section {
            name,
            paragraphs,
            span: start.extend(end),
        })
    }

    fn parse_paragraph(&mut self) -> Result<Paragraph> {
        let start = self.current_span();
        let name = self.expect_identifier()?;
        self.expect(TokenKind::Period)?;

        let mut statements = Vec::new();

        while !(self.is_at_end()
            || self.check_keyword(Keyword::EndProgram)
            || (self.check_identifier() && self.peek_keyword(Keyword::Section))
            || (self.check_identifier() && self.peek(TokenKind::Period)))
        {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.advance_to_next_sentence();
                }
            }
        }

        let end = self.previous_span();

        Ok(Paragraph {
            name,
            statements,
            span: start.extend(end),
        })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        if self.check_keyword(Keyword::Move) {
            self.parse_move_statement()
        } else if self.check_keyword(Keyword::Display) {
            self.parse_display_statement()
        } else if self.check_keyword(Keyword::Stop) {
            self.parse_stop_statement()
        } else if self.check_keyword(Keyword::Perform) {
            self.parse_perform_statement()
        } else if self.check_keyword(Keyword::If) {
            self.parse_if_statement()
        } else if self.check_keyword(Keyword::Continue) {
            self.parse_continue_statement()
        } else if self.check_keyword(Keyword::Add) {
            self.parse_add_statement()
        } else if self.check_keyword(Keyword::Subtract) {
            self.parse_subtract_statement()
        } else if self.check_keyword(Keyword::Multiply) {
            self.parse_multiply_statement()
        } else if self.check_keyword(Keyword::Divide) {
            self.parse_divide_statement()
        } else if self.check_keyword(Keyword::Compute) {
            self.parse_compute_statement()
        } else if self.check_keyword(Keyword::String) {
            self.parse_string_statement()
        } else if self.check_keyword(Keyword::Unstring) {
            self.parse_unstring_statement()
        } else if self.check_keyword(Keyword::Call) {
            self.parse_call_statement()
        } else if self.check_keyword(Keyword::GoTo) || self.check_keyword(Keyword::Go) {
            self.parse_goto_statement()
        } else if self.check_keyword(Keyword::Exit) {
            self.parse_exit_statement()
        } else {
            // Skip unknown statement
            let start = self.current_span();
            self.advance_to_next_sentence();
            Ok(Statement::Continue(ContinueStatement { span: start }))
        }
    }

    fn parse_move_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // MOVE

        let corresponding =
            if self.check_keyword(Keyword::Corresponding) || self.check_keyword(Keyword::Corr) {
                self.advance();
                true
            } else {
                false
            };

        let from = self.parse_expression()?;

        self.expect_keyword(Keyword::To)?;

        let mut to = Vec::new();
        while !self.check(TokenKind::Period) && !self.is_at_end() && !self.is_statement_start() {
            to.push(self.parse_qualified_name()?);
        }

        // Optional period
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::Move(MoveStatement {
            from,
            to,
            corresponding,
            span: start.extend(end),
        }))
    }

    fn parse_display_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // DISPLAY

        let mut items = Vec::new();
        let mut upon = None;
        let mut no_advancing = false;

        while !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
            && !self.check_keyword(Keyword::Upon)
            && !self.check_keyword(Keyword::With)
        {
            items.push(self.parse_expression()?);
        }

        if self.check_keyword(Keyword::Upon) {
            self.advance();
            upon = Some(self.expect_identifier()?);
        }

        if self.check_keyword(Keyword::With) {
            self.advance();
            if self.check_keyword(Keyword::Not) {
                self.advance();
            }
            // NO ADVANCING
            self.advance(); // Skip to next
            no_advancing = true;
        }

        // Optional period
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::Display(DisplayStatement {
            items,
            upon,
            no_advancing,
            span: start.extend(end),
        }))
    }

    fn parse_stop_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // STOP

        self.expect_keyword(Keyword::Run)?;

        let return_code = if !self.check(TokenKind::Period) && !self.is_statement_start() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::StopRun(StopRunStatement {
            return_code,
            span: start.extend(end),
        }))
    }

    fn parse_perform_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // PERFORM

        // Check for inline PERFORM
        if self.is_statement_start()
            || self.check_keyword(Keyword::Until)
            || self.check_keyword(Keyword::Varying)
        {
            // Inline PERFORM
            let mut until = None;
            let varying = None;
            let test_before = true;

            if self.check_keyword(Keyword::Until) {
                self.advance();
                until = Some(self.parse_condition()?);
            }

            // TODO: Parse VARYING clause

            // Parse inline statements until END-PERFORM
            let mut inline = Vec::new();
            while !self.check_keyword(Keyword::EndPerform) && !self.is_at_end() {
                match self.parse_statement() {
                    Ok(stmt) => inline.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        self.advance_to_next_sentence();
                        break;
                    }
                }
            }

            if self.check_keyword(Keyword::EndPerform) {
                self.advance();
            }

            let end = self.previous_span();

            return Ok(Statement::Perform(PerformStatement {
                target: None,
                thru: None,
                inline: Some(inline),
                times: None,
                until,
                varying,
                test_before,
                span: start.extend(end),
            }));
        }

        // Out-of-line PERFORM
        let target_name = self.expect_identifier()?;
        let target = Some(PerformTarget {
            name: target_name,
            span: self.previous_span(),
        });

        let mut thru = None;
        if self.check_keyword(Keyword::Thru) || self.check_keyword(Keyword::Through) {
            self.advance();
            thru = Some(self.expect_identifier()?);
        }

        let mut times = None;
        let mut until = None;

        if !self.check(TokenKind::Period) && !self.is_statement_start() {
            if self.check_keyword(Keyword::Times) {
                // n TIMES - but the number should have been parsed
                self.advance();
            } else if self.check_keyword(Keyword::Until) {
                self.advance();
                until = Some(self.parse_condition()?);
            } else if let Ok(expr) = self.parse_expression() {
                // Could be TIMES
                if self.check_keyword(Keyword::Times) {
                    self.advance();
                    times = Some(expr);
                }
            }
        }

        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::Perform(PerformStatement {
            target,
            thru,
            inline: None,
            times,
            until,
            varying: None,
            test_before: true,
            span: start.extend(end),
        }))
    }

    fn parse_if_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // IF

        let condition = self.parse_condition()?;

        // Optional THEN
        if self.check_keyword(Keyword::Then) {
            self.advance();
        }

        // Parse THEN branch
        let mut then_branch = Vec::new();
        while !self.check_keyword(Keyword::Else)
            && !self.check_keyword(Keyword::EndIf)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
        {
            match self.parse_statement() {
                Ok(stmt) => then_branch.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    break;
                }
            }
        }

        // Parse ELSE branch
        let else_branch = if self.check_keyword(Keyword::Else) {
            self.advance();
            let mut stmts = Vec::new();
            while !self.check_keyword(Keyword::EndIf)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
            {
                match self.parse_statement() {
                    Ok(stmt) => stmts.push(stmt),
                    Err(e) => {
                        self.errors.push(e);
                        break;
                    }
                }
            }
            Some(stmts)
        } else {
            None
        };

        // END-IF or period
        if self.check_keyword(Keyword::EndIf) {
            self.advance();
        }
        self.skip_if(TokenKind::Period);

        let end = self.previous_span();

        Ok(Statement::If(IfStatement {
            condition,
            then_branch,
            else_branch,
            span: start.extend(end),
        }))
    }

    fn parse_continue_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // CONTINUE
        self.skip_if(TokenKind::Period);
        Ok(Statement::Continue(ContinueStatement { span: start }))
    }

    // ========================================================================
    // ARITHMETIC STATEMENTS
    // ========================================================================

    /// Parse ADD statement.
    /// ADD operand... TO target... [GIVING target...]
    /// ADD operand... TO operand GIVING target...
    fn parse_add_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // ADD

        // Parse operands (values to add)
        let mut operands = Vec::new();
        while !self.check_keyword(Keyword::To)
            && !self.check_keyword(Keyword::Giving)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
        {
            operands.push(self.parse_expression()?);
        }

        // TO clause
        let mut to = Vec::new();
        if self.check_keyword(Keyword::To) {
            self.advance();
            while !self.check_keyword(Keyword::Giving)
                && !self.check_keyword(Keyword::OnSizeError)
                && !self.check_keyword(Keyword::NotOnSizeError)
                && !self.check_keyword(Keyword::EndAdd)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
            {
                let name = self.parse_qualified_name()?;
                let rounded = if self.check_keyword(Keyword::Rounded) {
                    self.advance();
                    true
                } else {
                    false
                };
                to.push(AddTarget { name, rounded });
            }
        }

        // GIVING clause
        let mut giving = Vec::new();
        if self.check_keyword(Keyword::Giving) {
            self.advance();
            giving = self.parse_compute_targets()?;
        }

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-ADD
        if self.check_keyword(Keyword::EndAdd) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Add(AddStatement {
            operands,
            to,
            giving,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse SUBTRACT statement.
    /// SUBTRACT operand... FROM target... [GIVING target...]
    fn parse_subtract_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // SUBTRACT

        // Parse operands
        let mut operands = Vec::new();
        while !self.check_keyword(Keyword::From)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
        {
            operands.push(self.parse_expression()?);
        }

        // FROM clause
        let mut from = Vec::new();
        if self.check_keyword(Keyword::From) {
            self.advance();
            while !self.check_keyword(Keyword::Giving)
                && !self.check_keyword(Keyword::OnSizeError)
                && !self.check_keyword(Keyword::NotOnSizeError)
                && !self.check_keyword(Keyword::EndSubtract)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
            {
                let name = self.parse_qualified_name()?;
                let rounded = if self.check_keyword(Keyword::Rounded) {
                    self.advance();
                    true
                } else {
                    false
                };
                from.push(AddTarget { name, rounded });
            }
        }

        // GIVING clause
        let mut giving = Vec::new();
        if self.check_keyword(Keyword::Giving) {
            self.advance();
            giving = self.parse_compute_targets()?;
        }

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-SUBTRACT
        if self.check_keyword(Keyword::EndSubtract) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Subtract(SubtractStatement {
            operands,
            from,
            giving,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse MULTIPLY statement.
    /// MULTIPLY operand BY operand [GIVING target...]
    fn parse_multiply_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // MULTIPLY

        let operand = self.parse_expression()?;

        self.expect_keyword(Keyword::By)?;

        let by = self.parse_expression()?;

        // GIVING clause
        let mut giving = Vec::new();
        if self.check_keyword(Keyword::Giving) {
            self.advance();
            giving = self.parse_compute_targets()?;
        }

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-MULTIPLY
        if self.check_keyword(Keyword::EndMultiply) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Multiply(MultiplyStatement {
            operand,
            by,
            giving,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse DIVIDE statement.
    /// DIVIDE operand INTO operand [GIVING target...] [REMAINDER target]
    /// DIVIDE operand BY operand GIVING target... [REMAINDER target]
    fn parse_divide_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // DIVIDE

        let operand = self.parse_expression()?;

        let (into_or_by, is_into) = if self.check_keyword(Keyword::Into) {
            self.advance();
            (self.parse_expression()?, true)
        } else if self.check_keyword(Keyword::By) {
            self.advance();
            (self.parse_expression()?, false)
        } else {
            return Err(CobolError::ParseError {
                message: "Expected INTO or BY in DIVIDE statement".to_string(),
            });
        };

        // GIVING clause
        let mut giving = Vec::new();
        if self.check_keyword(Keyword::Giving) {
            self.advance();
            giving = self.parse_compute_targets()?;
        }

        // REMAINDER clause
        let remainder = if self.check_keyword(Keyword::Remainder) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-DIVIDE
        if self.check_keyword(Keyword::EndDivide) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Divide(DivideStatement {
            operand,
            into_or_by,
            is_into,
            giving,
            remainder,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse COMPUTE statement.
    /// COMPUTE target... = expression
    fn parse_compute_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // COMPUTE

        let targets = self.parse_compute_targets()?;

        // Expect = sign
        self.expect(TokenKind::Equals)?;

        let expression = self.parse_expression()?;

        // ON SIZE ERROR / NOT ON SIZE ERROR
        let (on_size_error, not_on_size_error) = self.parse_size_error_handlers()?;

        // END-COMPUTE
        if self.check_keyword(Keyword::EndCompute) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Compute(ComputeStatement {
            targets,
            expression,
            on_size_error,
            not_on_size_error,
            span: start.extend(end),
        }))
    }

    /// Parse COMPUTE targets (variable names with optional ROUNDED).
    fn parse_compute_targets(&mut self) -> Result<Vec<ComputeTarget>> {
        let mut targets = Vec::new();

        while !self.check(TokenKind::Equals)
            && !self.check_keyword(Keyword::OnSizeError)
            && !self.check_keyword(Keyword::NotOnSizeError)
            && !self.check_keyword(Keyword::Remainder)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
            && !self.is_statement_start()
            && self.check_identifier()
        {
            let name = self.parse_qualified_name()?;
            let rounded = if self.check_keyword(Keyword::Rounded) {
                self.advance();
                true
            } else {
                false
            };
            targets.push(ComputeTarget { name, rounded });
        }

        Ok(targets)
    }

    /// Parse ON SIZE ERROR and NOT ON SIZE ERROR handlers.
    #[allow(clippy::type_complexity)]
    fn parse_size_error_handlers(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<Vec<Statement>>)> {
        let mut on_size_error = None;
        let mut not_on_size_error = None;

        if self.check_keyword(Keyword::OnSizeError) {
            self.advance();
            on_size_error = Some(self.parse_imperative_statements()?);
        }

        if self.check_keyword(Keyword::NotOnSizeError) {
            self.advance();
            not_on_size_error = Some(self.parse_imperative_statements()?);
        }

        Ok((on_size_error, not_on_size_error))
    }

    /// Parse imperative statements for error handlers.
    fn parse_imperative_statements(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();

        while !self.check_keyword(Keyword::EndAdd)
            && !self.check_keyword(Keyword::EndSubtract)
            && !self.check_keyword(Keyword::EndMultiply)
            && !self.check_keyword(Keyword::EndDivide)
            && !self.check_keyword(Keyword::EndCompute)
            && !self.check_keyword(Keyword::EndString)
            && !self.check_keyword(Keyword::EndUnstring)
            && !self.check_keyword(Keyword::NotOnSizeError)
            && !self.check_keyword(Keyword::NotOnOverflow)
            && !self.check(TokenKind::Period)
            && !self.is_at_end()
        {
            if self.is_statement_start() {
                statements.push(self.parse_statement()?);
            } else {
                break;
            }
        }

        Ok(statements)
    }

    // ========================================================================
    // STRING STATEMENTS
    // ========================================================================

    /// Parse STRING statement.
    /// STRING source... INTO target [WITH POINTER pointer] [ON OVERFLOW...]
    fn parse_string_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // STRING

        // Parse sources
        let mut sources = Vec::new();
        while !self.check_keyword(Keyword::Into) && !self.is_at_end() {
            let value = self.parse_expression()?;

            let delimited_by = if self.check_keyword(Keyword::Delimited) {
                self.advance();
                if self.check_keyword(Keyword::By) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Size) {
                    self.advance();
                    StringDelimiter::Size
                } else {
                    StringDelimiter::Value(self.parse_expression()?)
                }
            } else {
                StringDelimiter::Size
            };

            sources.push(StringSource {
                value,
                delimited_by,
            });
        }

        // INTO clause
        self.expect_keyword(Keyword::Into)?;
        let into = self.parse_qualified_name()?;

        // WITH POINTER clause
        let pointer = if self.check_keyword(Keyword::With) {
            self.advance();
            if self.check_keyword(Keyword::Pointer) {
                self.advance();
            }
            Some(self.parse_qualified_name()?)
        } else if self.check_keyword(Keyword::Pointer) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON OVERFLOW / NOT ON OVERFLOW
        let (on_overflow, not_on_overflow) = self.parse_overflow_handlers()?;

        // END-STRING
        if self.check_keyword(Keyword::EndString) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::String(StringStatement {
            sources,
            into,
            pointer,
            on_overflow,
            not_on_overflow,
            span: start.extend(end),
        }))
    }

    /// Parse UNSTRING statement.
    /// UNSTRING source DELIMITED BY delimiter... INTO target... [WITH POINTER...]
    fn parse_unstring_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // UNSTRING

        let source = self.parse_qualified_name()?;

        // DELIMITED BY clause
        let mut delimiters = Vec::new();
        if self.check_keyword(Keyword::Delimited) {
            self.advance();
            if self.check_keyword(Keyword::By) {
                self.advance();
            }

            loop {
                let all = if self.check_keyword(Keyword::All) {
                    self.advance();
                    true
                } else {
                    false
                };
                let value = self.parse_expression()?;
                delimiters.push(UnstringDelimiter { all, value });

                if self.check_keyword(Keyword::Or) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // INTO clause
        let mut into = Vec::new();
        if self.check_keyword(Keyword::Into) {
            self.advance();
            while !self.check_keyword(Keyword::With)
                && !self.check_keyword(Keyword::Pointer)
                && !self.check_keyword(Keyword::Tallying)
                && !self.check_keyword(Keyword::OnOverflow)
                && !self.check_keyword(Keyword::NotOnOverflow)
                && !self.check_keyword(Keyword::EndUnstring)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
                && self.check_identifier()
            {
                let name = self.parse_qualified_name()?;

                let delimiter_in = if self.check_keyword(Keyword::Delimiter) {
                    self.advance();
                    Some(self.parse_qualified_name()?)
                } else {
                    None
                };

                let count_in = if self.check_keyword(Keyword::Count) {
                    self.advance();
                    Some(self.parse_qualified_name()?)
                } else {
                    None
                };

                into.push(UnstringTarget {
                    name,
                    delimiter_in,
                    count_in,
                });
            }
        }

        // WITH POINTER clause
        let pointer = if self.check_keyword(Keyword::With) {
            self.advance();
            if self.check_keyword(Keyword::Pointer) {
                self.advance();
            }
            Some(self.parse_qualified_name()?)
        } else if self.check_keyword(Keyword::Pointer) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // TALLYING clause
        let tallying = if self.check_keyword(Keyword::Tallying) {
            self.advance();
            // Skip IN if present (IN is not a keyword, check identifier)
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON OVERFLOW / NOT ON OVERFLOW
        let (on_overflow, not_on_overflow) = self.parse_overflow_handlers()?;

        // END-UNSTRING
        if self.check_keyword(Keyword::EndUnstring) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Unstring(UnstringStatement {
            source,
            delimiters,
            into,
            pointer,
            tallying,
            on_overflow,
            not_on_overflow,
            span: start.extend(end),
        }))
    }

    /// Parse ON OVERFLOW and NOT ON OVERFLOW handlers.
    #[allow(clippy::type_complexity)]
    fn parse_overflow_handlers(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<Vec<Statement>>)> {
        let mut on_overflow = None;
        let mut not_on_overflow = None;

        if self.check_keyword(Keyword::OnOverflow) {
            self.advance();
            on_overflow = Some(self.parse_imperative_statements()?);
        }

        if self.check_keyword(Keyword::NotOnOverflow) {
            self.advance();
            not_on_overflow = Some(self.parse_imperative_statements()?);
        }

        Ok((on_overflow, not_on_overflow))
    }

    // ========================================================================
    // CALL, GOTO, EXIT STATEMENTS
    // ========================================================================

    /// Parse CALL statement.
    /// CALL program [USING parameter...] [RETURNING var] [ON EXCEPTION...]
    fn parse_call_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // CALL

        // Program name (literal or identifier)
        let program = self.parse_expression()?;

        // USING clause
        let mut using = Vec::new();
        if self.check_keyword(Keyword::Using) {
            self.advance();
            while !self.check_keyword(Keyword::Returning)
                && !self.check_keyword(Keyword::OnException)
                && !self.check_keyword(Keyword::NotOnException)
                && !self.check_keyword(Keyword::EndCall)
                && !self.check(TokenKind::Period)
                && !self.is_at_end()
                && !self.is_statement_start()
            {
                let param_start = self.current_span();

                // Check for BY REFERENCE/CONTENT/VALUE
                let mode = if self.check_keyword(Keyword::By) {
                    self.advance();
                    if self.check_keyword(Keyword::Reference) {
                        self.advance();
                        ParameterMode::Reference
                    } else if self.check_keyword(Keyword::Content) {
                        self.advance();
                        ParameterMode::Content
                    } else if self.check_keyword(Keyword::Value) {
                        self.advance();
                        ParameterMode::Value
                    } else {
                        ParameterMode::Reference
                    }
                } else if self.check_keyword(Keyword::Reference) {
                    self.advance();
                    ParameterMode::Reference
                } else if self.check_keyword(Keyword::Content) {
                    self.advance();
                    ParameterMode::Content
                } else if self.check_keyword(Keyword::Value) {
                    self.advance();
                    ParameterMode::Value
                } else {
                    ParameterMode::Reference
                };

                let value = self.parse_expression()?;
                let param_end = self.previous_span();

                using.push(CallParameter {
                    value,
                    mode,
                    span: param_start.extend(param_end),
                });
            }
        }

        // RETURNING clause
        let returning = if self.check_keyword(Keyword::Returning) {
            self.advance();
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        // ON EXCEPTION / NOT ON EXCEPTION
        let (on_exception, not_on_exception) = self.parse_exception_handlers()?;

        // END-CALL
        if self.check_keyword(Keyword::EndCall) {
            self.advance();
        }

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Call(CallStatement {
            program,
            using,
            returning,
            on_exception,
            not_on_exception,
            span: start.extend(end),
        }))
    }

    /// Parse ON EXCEPTION and NOT ON EXCEPTION handlers.
    #[allow(clippy::type_complexity)]
    fn parse_exception_handlers(
        &mut self,
    ) -> Result<(Option<Vec<Statement>>, Option<Vec<Statement>>)> {
        let mut on_exception = None;
        let mut not_on_exception = None;

        if self.check_keyword(Keyword::OnException) {
            self.advance();
            on_exception = Some(self.parse_imperative_statements()?);
        }

        if self.check_keyword(Keyword::NotOnException) {
            self.advance();
            not_on_exception = Some(self.parse_imperative_statements()?);
        }

        Ok((on_exception, not_on_exception))
    }

    /// Parse GO TO statement.
    /// GO TO paragraph-name [DEPENDING ON identifier]
    fn parse_goto_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();

        // Skip GO or GOTO
        if self.check_keyword(Keyword::Go) {
            self.advance();
            if self.check_keyword(Keyword::To) {
                self.advance();
            }
        } else {
            self.advance(); // GOTO
        }

        // Parse targets
        let mut targets = Vec::new();
        while self.check_identifier()
            && !self.check_keyword(Keyword::Depending)
            && !self.check(TokenKind::Period)
        {
            targets.push(self.expect_identifier()?);
        }

        // DEPENDING ON clause
        let depending = if self.check_keyword(Keyword::Depending) {
            self.advance();
            if self.check_keyword(Keyword::On) {
                self.advance();
            }
            Some(self.parse_qualified_name()?)
        } else {
            None
        };

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::GoTo(GoToStatement {
            targets,
            depending,
            span: start.extend(end),
        }))
    }

    /// Parse EXIT statement.
    /// EXIT [PROGRAM | PERFORM]
    fn parse_exit_statement(&mut self) -> Result<Statement> {
        let start = self.current_span();
        self.advance(); // EXIT

        let mut program = false;
        let mut perform_cycle = false;

        if self.check_keyword(Keyword::Program) {
            self.advance();
            program = true;
        } else if self.check_keyword(Keyword::Perform) {
            self.advance();
            perform_cycle = true;
        }
        // Default is just EXIT (exit paragraph)

        self.skip_if(TokenKind::Period);
        let end = self.previous_span();

        Ok(Statement::Exit(ExitStatement {
            program,
            perform_cycle,
            span: start.extend(end),
        }))
    }

    // ========================================================================
    // EXPRESSIONS
    // ========================================================================

    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_additive_expression()
    }

    fn parse_additive_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_multiplicative_expression()?;

        while self.check(TokenKind::Plus) || self.check(TokenKind::Minus) {
            let op = if self.check(TokenKind::Plus) {
                BinaryOp::Add
            } else {
                BinaryOp::Subtract
            };
            self.advance();
            let right = self.parse_multiplicative_expression()?;
            let span = left.span().extend(right.span());
            left = Expression::Binary(Box::new(BinaryExpr {
                left,
                op,
                right,
                span,
            }));
        }

        Ok(left)
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_power_expression()?;

        while self.check(TokenKind::Star) || self.check(TokenKind::Slash) {
            let op = if self.check(TokenKind::Star) {
                BinaryOp::Multiply
            } else {
                BinaryOp::Divide
            };
            self.advance();
            let right = self.parse_power_expression()?;
            let span = left.span().extend(right.span());
            left = Expression::Binary(Box::new(BinaryExpr {
                left,
                op,
                right,
                span,
            }));
        }

        Ok(left)
    }

    fn parse_power_expression(&mut self) -> Result<Expression> {
        let mut left = self.parse_unary_expression()?;

        if self.check(TokenKind::DoubleStar) {
            self.advance();
            let right = self.parse_power_expression()?; // Right associative
            let span = left.span().extend(right.span());
            left = Expression::Binary(Box::new(BinaryExpr {
                left,
                op: BinaryOp::Power,
                right,
                span,
            }));
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression> {
        if self.check(TokenKind::Plus) {
            let start = self.current_span();
            self.advance();
            let operand = self.parse_primary_expression()?;
            let span = start.extend(operand.span());
            Ok(Expression::Unary(Box::new(UnaryExpr {
                op: UnaryOp::Plus,
                operand,
                span,
            })))
        } else if self.check(TokenKind::Minus) {
            let start = self.current_span();
            self.advance();
            let operand = self.parse_primary_expression()?;
            let span = start.extend(operand.span());
            Ok(Expression::Unary(Box::new(UnaryExpr {
                op: UnaryOp::Minus,
                operand,
                span,
            })))
        } else {
            self.parse_primary_expression()
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Expression> {
        if self.check(TokenKind::LeftParen) {
            self.advance();
            let expr = self.parse_expression()?;
            self.expect(TokenKind::RightParen)?;
            Ok(Expression::Paren(Box::new(expr)))
        } else if self.check_keyword(Keyword::Function) {
            self.parse_function_call()
        } else if self.check_literal() {
            Ok(Expression::Literal(self.parse_literal()?))
        } else if self.check_figurative_constant() {
            Ok(Expression::Literal(self.parse_figurative_constant()?))
        } else if self.check_identifier() {
            let name = self.parse_qualified_name()?;
            // Check for reference modification
            if self.check(TokenKind::LeftParen) {
                // Could be subscript or refmod - for now treat as subscript
            }
            Ok(Expression::Variable(name))
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected expression, found {:?}", self.current().kind),
            })
        }
    }

    fn parse_function_call(&mut self) -> Result<Expression> {
        let start = self.current_span();
        self.expect_keyword(Keyword::Function)?;
        let name = self.expect_identifier()?;

        let mut arguments = Vec::new();
        if self.check(TokenKind::LeftParen) {
            self.advance();
            while !self.check(TokenKind::RightParen) && !self.is_at_end() {
                arguments.push(self.parse_expression()?);
                if self.check(TokenKind::Comma) {
                    self.advance();
                }
            }
            self.expect(TokenKind::RightParen)?;
        }

        let end = self.previous_span();

        Ok(Expression::Function(FunctionCall {
            name,
            arguments,
            span: start.extend(end),
        }))
    }

    fn parse_literal(&mut self) -> Result<Literal> {
        let span = self.current_span();
        let kind = match &self.current().kind {
            TokenKind::IntegerLiteral(n) => {
                let n = *n;
                self.advance();
                LiteralKind::Integer(n)
            }
            TokenKind::DecimalLiteral(s) => {
                let s = s.clone();
                self.advance();
                LiteralKind::Decimal(s)
            }
            TokenKind::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                LiteralKind::String(s)
            }
            TokenKind::HexLiteral(s) => {
                let s = s.clone();
                self.advance();
                LiteralKind::Hex(s)
            }
            _ => {
                return Err(CobolError::ParseError {
                    message: "Expected literal".to_string(),
                })
            }
        };

        Ok(Literal { kind, span })
    }

    fn parse_figurative_constant(&mut self) -> Result<Literal> {
        let span = self.current_span();
        let fc = if self.check_keyword(Keyword::Zero)
            || self.check_keyword(Keyword::Zeros)
            || self.check_keyword(Keyword::Zeroes)
        {
            self.advance();
            FigurativeConstant::Zero
        } else if self.check_keyword(Keyword::Space) || self.check_keyword(Keyword::Spaces) {
            self.advance();
            FigurativeConstant::Space
        } else if self.check_keyword(Keyword::HighValue) || self.check_keyword(Keyword::HighValues)
        {
            self.advance();
            FigurativeConstant::HighValue
        } else if self.check_keyword(Keyword::LowValue) || self.check_keyword(Keyword::LowValues) {
            self.advance();
            FigurativeConstant::LowValue
        } else if self.check_keyword(Keyword::Quote) || self.check_keyword(Keyword::Quotes) {
            self.advance();
            FigurativeConstant::Quote
        } else {
            return Err(CobolError::ParseError {
                message: "Expected figurative constant".to_string(),
            });
        };

        Ok(Literal {
            kind: LiteralKind::Figurative(fc),
            span,
        })
    }

    fn parse_qualified_name(&mut self) -> Result<QualifiedName> {
        let start = self.current_span();
        let name = self.expect_identifier()?;

        let mut qualifiers = Vec::new();
        let mut subscripts = Vec::new();

        // Parse OF/IN qualifications
        while self.check_keyword(Keyword::Of) || self.check_keyword(Keyword::Input) {
            // Note: IN might be confused with INPUT
            if self.check_keyword(Keyword::Of) {
                self.advance();
                qualifiers.push(self.expect_identifier()?);
            } else {
                break;
            }
        }

        // Parse subscripts
        if self.check(TokenKind::LeftParen) {
            self.advance();
            while !self.check(TokenKind::RightParen) && !self.is_at_end() {
                subscripts.push(self.parse_expression()?);
                if self.check(TokenKind::Comma) {
                    self.advance();
                } else if !self.check(TokenKind::RightParen) {
                    // Space-separated subscripts
                }
            }
            self.expect(TokenKind::RightParen)?;
        }

        let end = self.previous_span();

        Ok(QualifiedName {
            name,
            qualifiers,
            subscripts,
            span: start.extend(end),
        })
    }

    // ========================================================================
    // CONDITIONS
    // ========================================================================

    fn parse_condition(&mut self) -> Result<Condition> {
        self.parse_or_condition()
    }

    fn parse_or_condition(&mut self) -> Result<Condition> {
        let mut left = self.parse_and_condition()?;

        while self.check_keyword(Keyword::Or) {
            self.advance();
            let right = self.parse_and_condition()?;
            left = Condition::Or(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_and_condition(&mut self) -> Result<Condition> {
        let mut left = self.parse_not_condition()?;

        while self.check_keyword(Keyword::And) {
            self.advance();
            let right = self.parse_not_condition()?;
            left = Condition::And(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_not_condition(&mut self) -> Result<Condition> {
        if self.check_keyword(Keyword::Not) {
            self.advance();
            let cond = self.parse_primary_condition()?;
            Ok(Condition::Not(Box::new(cond)))
        } else {
            self.parse_primary_condition()
        }
    }

    fn parse_primary_condition(&mut self) -> Result<Condition> {
        if self.check(TokenKind::LeftParen) {
            self.advance();
            let cond = self.parse_condition()?;
            self.expect(TokenKind::RightParen)?;
            return Ok(Condition::Paren(Box::new(cond)));
        }

        // Try to parse as comparison
        let left = self.parse_expression()?;

        // Check for comparison operators
        if self.check_comparison_op() {
            let op = self.parse_comparison_op()?;
            let right = self.parse_expression()?;
            let span = left.span().extend(right.span());
            return Ok(Condition::Comparison(Box::new(Comparison {
                left,
                op,
                right,
                span,
            })));
        }

        // Check for class conditions
        if self.check_keyword(Keyword::Is) {
            self.advance();
            let negated = if self.check_keyword(Keyword::Not) {
                self.advance();
                true
            } else {
                false
            };

            if self.check_keyword(Keyword::Numeric) {
                self.advance();
                return Ok(Condition::Class(Box::new(ClassCondition {
                    operand: left,
                    class: ClassType::Numeric,
                    negated,
                    span: Span::dummy(),
                })));
            } else if self.check_keyword(Keyword::Alphabetic) {
                self.advance();
                return Ok(Condition::Class(Box::new(ClassCondition {
                    operand: left,
                    class: ClassType::Alphabetic,
                    negated,
                    span: Span::dummy(),
                })));
            }
        }

        // Condition name
        if let Expression::Variable(name) = left {
            Ok(Condition::ConditionName(name))
        } else {
            Err(CobolError::ParseError {
                message: "Expected condition".to_string(),
            })
        }
    }

    fn check_comparison_op(&self) -> bool {
        matches!(
            self.current().kind,
            TokenKind::Equals
                | TokenKind::GreaterThan
                | TokenKind::LessThan
                | TokenKind::GreaterEquals
                | TokenKind::LessEquals
                | TokenKind::NotEquals
        ) || self.check_keyword(Keyword::Equal)
            || self.check_keyword(Keyword::Greater)
            || self.check_keyword(Keyword::Less)
            || self.check_keyword(Keyword::Not)
    }

    fn parse_comparison_op(&mut self) -> Result<ComparisonOp> {
        let op = match &self.current().kind {
            TokenKind::Equals => {
                self.advance();
                ComparisonOp::Equal
            }
            TokenKind::GreaterThan => {
                self.advance();
                ComparisonOp::GreaterThan
            }
            TokenKind::LessThan => {
                self.advance();
                ComparisonOp::LessThan
            }
            TokenKind::GreaterEquals => {
                self.advance();
                ComparisonOp::GreaterOrEqual
            }
            TokenKind::LessEquals => {
                self.advance();
                ComparisonOp::LessOrEqual
            }
            TokenKind::NotEquals => {
                self.advance();
                ComparisonOp::NotEqual
            }
            TokenKind::Keyword(Keyword::Equal) => {
                self.advance();
                // Skip TO if present
                if self.check_keyword(Keyword::To) {
                    self.advance();
                }
                ComparisonOp::Equal
            }
            TokenKind::Keyword(Keyword::Greater) => {
                self.advance();
                if self.check_keyword(Keyword::Than) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Or) {
                    self.advance();
                    self.expect_keyword(Keyword::Equal)?;
                    if self.check_keyword(Keyword::To) {
                        self.advance();
                    }
                    ComparisonOp::GreaterOrEqual
                } else {
                    ComparisonOp::GreaterThan
                }
            }
            TokenKind::Keyword(Keyword::Less) => {
                self.advance();
                if self.check_keyword(Keyword::Than) {
                    self.advance();
                }
                if self.check_keyword(Keyword::Or) {
                    self.advance();
                    self.expect_keyword(Keyword::Equal)?;
                    if self.check_keyword(Keyword::To) {
                        self.advance();
                    }
                    ComparisonOp::LessOrEqual
                } else {
                    ComparisonOp::LessThan
                }
            }
            TokenKind::Keyword(Keyword::Not) => {
                self.advance();
                if self.check_keyword(Keyword::Equal) {
                    self.advance();
                    if self.check_keyword(Keyword::To) {
                        self.advance();
                    }
                    ComparisonOp::NotEqual
                } else if self.check(TokenKind::Equals) {
                    self.advance();
                    ComparisonOp::NotEqual
                } else {
                    return Err(CobolError::ParseError {
                        message: "Expected comparison operator".to_string(),
                    });
                }
            }
            _ => {
                return Err(CobolError::ParseError {
                    message: "Expected comparison operator".to_string(),
                })
            }
        };
        Ok(op)
    }

    // ========================================================================
    // UTILITY FUNCTIONS
    // ========================================================================

    fn current(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or_else(|| {
            self.tokens
                .last()
                .expect("Token stream should not be empty")
        })
    }

    fn current_span(&self) -> Span {
        self.current().span
    }

    fn previous_span(&self) -> Span {
        if self.current > 0 {
            self.tokens[self.current - 1].span
        } else {
            self.current_span()
        }
    }

    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::Eof
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.tokens[self.current - 1]
    }

    fn check(&self, kind: TokenKind) -> bool {
        std::mem::discriminant(&self.current().kind) == std::mem::discriminant(&kind)
    }

    fn check_keyword(&self, kw: Keyword) -> bool {
        matches!(&self.current().kind, TokenKind::Keyword(k) if *k == kw)
    }

    fn peek_keyword(&self, kw: Keyword) -> bool {
        if self.current + 1 < self.tokens.len() {
            matches!(&self.tokens[self.current + 1].kind, TokenKind::Keyword(k) if *k == kw)
        } else {
            false
        }
    }

    #[allow(dead_code)]
    fn peek2_keyword(&self, kw: Keyword) -> bool {
        if self.current + 2 < self.tokens.len() {
            matches!(&self.tokens[self.current + 2].kind, TokenKind::Keyword(k) if *k == kw)
        } else {
            false
        }
    }

    fn peek(&self, kind: TokenKind) -> bool {
        if self.current + 1 < self.tokens.len() {
            std::mem::discriminant(&self.tokens[self.current + 1].kind)
                == std::mem::discriminant(&kind)
        } else {
            false
        }
    }

    fn check_identifier(&self) -> bool {
        matches!(&self.current().kind, TokenKind::Identifier(_))
    }

    fn check_literal(&self) -> bool {
        matches!(
            &self.current().kind,
            TokenKind::IntegerLiteral(_)
                | TokenKind::DecimalLiteral(_)
                | TokenKind::StringLiteral(_)
                | TokenKind::HexLiteral(_)
        )
    }

    fn check_figurative_constant(&self) -> bool {
        self.check_keyword(Keyword::Zero)
            || self.check_keyword(Keyword::Zeros)
            || self.check_keyword(Keyword::Zeroes)
            || self.check_keyword(Keyword::Space)
            || self.check_keyword(Keyword::Spaces)
            || self.check_keyword(Keyword::HighValue)
            || self.check_keyword(Keyword::HighValues)
            || self.check_keyword(Keyword::LowValue)
            || self.check_keyword(Keyword::LowValues)
            || self.check_keyword(Keyword::Quote)
            || self.check_keyword(Keyword::Quotes)
    }

    fn check_level_number(&self) -> bool {
        if let TokenKind::IntegerLiteral(n) = &self.current().kind {
            let n = *n;
            (1..=49).contains(&n) || n == 66 || n == 77 || n == 88
        } else {
            false
        }
    }

    fn peek_level_number(&self) -> u8 {
        if let TokenKind::IntegerLiteral(n) = &self.current().kind {
            *n as u8
        } else {
            0
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.check(kind.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected {:?}, found {:?}", kind, self.current().kind),
            })
        }
    }

    fn expect_keyword(&mut self, kw: Keyword) -> Result<()> {
        if self.check_keyword(kw) {
            self.advance();
            Ok(())
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected keyword {:?}, found {:?}", kw, self.current().kind),
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<String> {
        if let TokenKind::Identifier(s) = &self.current().kind {
            let s = s.clone();
            self.advance();
            Ok(s)
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected identifier, found {:?}", self.current().kind),
            })
        }
    }

    fn expect_identifier_or_string(&mut self) -> Result<String> {
        match &self.current().kind {
            TokenKind::Identifier(s) => {
                let s = s.clone();
                self.advance();
                Ok(s)
            }
            TokenKind::StringLiteral(s) => {
                let s = s.clone();
                self.advance();
                Ok(s)
            }
            _ => Err(CobolError::ParseError {
                message: format!(
                    "Expected identifier or string, found {:?}",
                    self.current().kind
                ),
            }),
        }
    }

    fn expect_integer(&mut self) -> Result<i64> {
        if let TokenKind::IntegerLiteral(n) = &self.current().kind {
            let n = *n;
            self.advance();
            Ok(n)
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected integer, found {:?}", self.current().kind),
            })
        }
    }

    fn expect_level_number(&mut self) -> Result<u8> {
        if let TokenKind::IntegerLiteral(n) = &self.current().kind {
            let n = *n;
            if (1..=49).contains(&n) || n == 66 || n == 77 || n == 88 {
                self.advance();
                Ok(n as u8)
            } else {
                Err(CobolError::ParseError {
                    message: format!("Invalid level number: {}", n),
                })
            }
        } else {
            Err(CobolError::ParseError {
                message: format!("Expected level number, found {:?}", self.current().kind),
            })
        }
    }

    fn skip_if(&mut self, kind: TokenKind) {
        if self.check(kind) {
            self.advance();
        }
    }

    fn is_at_division_start(&self) -> bool {
        self.check_keyword(Keyword::Identification)
            || self.check_keyword(Keyword::Environment)
            || self.check_keyword(Keyword::Data)
            || self.check_keyword(Keyword::Procedure)
    }

    fn is_at_section_start(&self) -> bool {
        (self.check_keyword(Keyword::Configuration)
            || self.check_keyword(Keyword::InputOutput)
            || self.check_keyword(Keyword::File)
            || self.check_keyword(Keyword::WorkingStorage)
            || self.check_keyword(Keyword::Working)
            || self.check_keyword(Keyword::LocalStorage)
            || self.check_keyword(Keyword::Linkage))
            && self.peek_keyword(Keyword::Section)
    }

    fn is_statement_start(&self) -> bool {
        self.check_keyword(Keyword::Move)
            || self.check_keyword(Keyword::Add)
            || self.check_keyword(Keyword::Subtract)
            || self.check_keyword(Keyword::Multiply)
            || self.check_keyword(Keyword::Divide)
            || self.check_keyword(Keyword::Compute)
            || self.check_keyword(Keyword::If)
            || self.check_keyword(Keyword::Evaluate)
            || self.check_keyword(Keyword::Perform)
            || self.check_keyword(Keyword::Call)
            || self.check_keyword(Keyword::Display)
            || self.check_keyword(Keyword::Accept)
            || self.check_keyword(Keyword::Open)
            || self.check_keyword(Keyword::Close)
            || self.check_keyword(Keyword::Read)
            || self.check_keyword(Keyword::Write)
            || self.check_keyword(Keyword::Stop)
            || self.check_keyword(Keyword::Exit)
            || self.check_keyword(Keyword::Go)
            || self.check_keyword(Keyword::Initialize)
            || self.check_keyword(Keyword::Inspect)
            || self.check_keyword(Keyword::String)
            || self.check_keyword(Keyword::Unstring)
            || self.check_keyword(Keyword::Set)
            || self.check_keyword(Keyword::Search)
            || self.check_keyword(Keyword::Continue)
    }

    fn is_data_clause_start(&self) -> bool {
        self.check_keyword(Keyword::Pic)
            || self.check_keyword(Keyword::Picture)
            || self.check_keyword(Keyword::Usage)
            || self.check_keyword(Keyword::Value)
            || self.check_keyword(Keyword::Occurs)
            || self.check_keyword(Keyword::Redefines)
            || self.check_keyword(Keyword::Sign)
            || self.check_keyword(Keyword::Justified)
            || self.check_keyword(Keyword::Just)
            || self.check_keyword(Keyword::Blank)
            || self.is_usage_keyword()
    }

    fn is_usage_keyword(&self) -> bool {
        self.check_keyword(Keyword::Display)
            || self.check_keyword(Keyword::Binary)
            || self.check_keyword(Keyword::Comp)
            || self.check_keyword(Keyword::Comp1)
            || self.check_keyword(Keyword::Comp2)
            || self.check_keyword(Keyword::Comp3)
            || self.check_keyword(Keyword::Comp4)
            || self.check_keyword(Keyword::Comp5)
            || self.check_keyword(Keyword::Computational)
            || self.check_keyword(Keyword::Computational1)
            || self.check_keyword(Keyword::Computational2)
            || self.check_keyword(Keyword::Computational3)
            || self.check_keyword(Keyword::Computational4)
            || self.check_keyword(Keyword::Computational5)
            || self.check_keyword(Keyword::PackedDecimal)
            || self.check_keyword(Keyword::Pointer)
    }

    fn consume_until_period(&mut self) -> String {
        let mut result = String::new();
        while !self.check(TokenKind::Period) && !self.is_at_end() {
            match &self.current().kind {
                TokenKind::Identifier(s) => result.push_str(s),
                TokenKind::StringLiteral(s) => result.push_str(s),
                TokenKind::IntegerLiteral(n) => result.push_str(&n.to_string()),
                _ => {}
            }
            result.push(' ');
            self.advance();
        }
        self.skip_if(TokenKind::Period);
        result.trim().to_string()
    }

    fn advance_to_next_sentence(&mut self) {
        while !self.check(TokenKind::Period) && !self.is_at_end() {
            self.advance();
        }
        self.skip_if(TokenKind::Period);
    }
}

/// Analyze a PICTURE string to determine its category and size.
fn analyze_picture(picture: &str) -> (PictureCategory, u32, u32) {
    let upper = picture.to_uppercase();
    let mut size = 0u32;
    let mut decimal_pos = 0u32;
    let mut seen_v = false;
    let mut has_9 = false;
    let mut has_x = false;
    let mut has_a = false;
    let mut has_edit = false;

    let chars: Vec<char> = upper.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let ch = chars[i];
        let count = if i + 1 < chars.len() && chars[i + 1] == '(' {
            // Parse repeat count
            let mut end = i + 2;
            while end < chars.len() && chars[end] != ')' {
                end += 1;
            }
            let count_str: String = chars[i + 2..end].iter().collect();
            let count = count_str.parse::<u32>().unwrap_or(1);
            i = end + 1;
            count
        } else {
            i += 1;
            1
        };

        match ch {
            '9' => {
                has_9 = true;
                size += count;
                if seen_v {
                    decimal_pos += count;
                }
            }
            'X' => {
                has_x = true;
                size += count;
            }
            'A' => {
                has_a = true;
                size += count;
            }
            'S' => { /* Sign, doesn't add to size for DISPLAY */ }
            'V' => {
                seen_v = true;
            }
            'P' => {
                // Assumed decimal position
                if seen_v {
                    decimal_pos += count;
                }
            }
            'Z' | '*' | '+' | '-' | '$' | ',' | '.' | '/' | 'B' | '0' => {
                has_edit = true;
                size += count;
            }
            'C' | 'D' => {
                // CR/DB
                has_edit = true;
                size += 2;
            }
            _ => {}
        }
    }

    let category = if has_edit && has_9 {
        PictureCategory::NumericEdited
    } else if has_9 && !has_x && !has_a {
        PictureCategory::Numeric
    } else if has_x {
        PictureCategory::Alphanumeric
    } else if has_a {
        PictureCategory::Alphabetic
    } else {
        PictureCategory::Alphanumeric
    };

    (category, size, decimal_pos)
}

/// Parse a COBOL program from tokens.
pub fn parse(tokens: Vec<Token>) -> (Option<Program>, Vec<CobolError>) {
    Parser::new(tokens).parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{scan, FileId, SourceFile, SourceFormat};

    fn parse_text(text: &str) -> (Option<Program>, Vec<CobolError>) {
        let source = SourceFile::from_text(FileId::MAIN, text.to_string(), SourceFormat::Free);
        let (tokens, _errors) = scan(&source);
        parse(tokens)
    }

    #[test]
    fn test_parse_minimal_program() {
        let text = r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. HELLO.
            PROCEDURE DIVISION.
                DISPLAY "HELLO, WORLD!".
                STOP RUN.
        "#;

        let (program, errors) = parse_text(text);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
        let program = program.unwrap();
        assert_eq!(program.identification.program_id.name, "HELLO");
        assert!(program.procedure.is_some());
    }

    #[test]
    fn test_parse_with_working_storage() {
        let text = r#"
            IDENTIFICATION DIVISION.
            PROGRAM-ID. TEST.
            DATA DIVISION.
            WORKING-STORAGE SECTION.
            01 WS-NAME PIC X(20).
            01 WS-COUNT PIC 9(5).
            PROCEDURE DIVISION.
                STOP RUN.
        "#;

        let (program, errors) = parse_text(text);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
        let program = program.unwrap();
        assert!(program.data.is_some());
        let data = program.data.as_ref().unwrap();
        assert_eq!(data.working_storage.len(), 2);
    }

    #[test]
    fn test_analyze_picture() {
        let (cat, size, dec) = analyze_picture("X(10)");
        assert_eq!(cat, PictureCategory::Alphanumeric);
        assert_eq!(size, 10);
        assert_eq!(dec, 0);

        let (cat, size, dec) = analyze_picture("9(5)V99");
        assert_eq!(cat, PictureCategory::Numeric);
        assert_eq!(size, 7);
        assert_eq!(dec, 2);

        let (cat, size, dec) = analyze_picture("S9(7)V9(2)");
        assert_eq!(cat, PictureCategory::Numeric);
        assert_eq!(size, 9);
        assert_eq!(dec, 2);
    }
}
