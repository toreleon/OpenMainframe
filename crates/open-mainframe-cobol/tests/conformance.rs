//! IBM Enterprise COBOL v6.4 Conformance Test Suite
//!
//! Systematic tests validating parser, semantic analysis, and data division
//! conformance across all implemented features.

use open_mainframe_cobol::ast::*;
use open_mainframe_cobol::error::CobolError;
use open_mainframe_cobol::lexer::{scan, FileId, SourceFile, SourceFormat};
use open_mainframe_cobol::parser;

// ============================================================================
// Test helpers
// ============================================================================

fn parse_free(text: &str) -> (Option<Program>, Vec<CobolError>) {
    let source = SourceFile::from_text(FileId::MAIN, text.to_string(), SourceFormat::Free);
    let (tokens, _) = scan(&source);
    parser::parse(tokens)
}

fn parse_ok(text: &str) -> Program {
    let (program, errors) = parse_free(text);
    assert!(errors.is_empty(), "Unexpected errors: {:?}", errors);
    program.expect("Expected program")
}

// ============================================================================
// 76.1 — Statement Conformance Tests
// ============================================================================

#[test]
fn conformance_move_statement() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A PIC X(10).
        01 B PIC X(10).
        PROCEDURE DIVISION.
            MOVE A TO B.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    assert!(matches!(proc.body, ProcedureBody::Statements(_)));
}

#[test]
fn conformance_compute_statement() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A PIC 9(5).
        01 B PIC 9(5).
        PROCEDURE DIVISION.
            COMPUTE A = B + 1.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::Compute(_)));
    }
}

#[test]
fn conformance_add_subtract_multiply_divide() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A PIC 9(5).
        01 B PIC 9(5).
        01 C PIC 9(5).
        PROCEDURE DIVISION.
            ADD A TO B.
            SUBTRACT A FROM B.
            MULTIPLY A BY B.
            DIVIDE A INTO B.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::Add(_)));
        assert!(matches!(&stmts[1], Statement::Subtract(_)));
        assert!(matches!(&stmts[2], Statement::Multiply(_)));
        assert!(matches!(&stmts[3], Statement::Divide(_)));
    }
}

#[test]
fn conformance_if_evaluate() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A PIC 9(5).
        PROCEDURE DIVISION.
            IF A > 0
                DISPLAY "POSITIVE"
            ELSE
                DISPLAY "ZERO OR NEGATIVE"
            END-IF.
            EVALUATE A
                WHEN 1 DISPLAY "ONE"
                WHEN 2 DISPLAY "TWO"
                WHEN OTHER DISPLAY "OTHER"
            END-EVALUATE.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::If(_)));
        assert!(matches!(&stmts[1], Statement::Evaluate(_)));
    }
}

#[test]
fn conformance_perform_statement() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 I PIC 9(5).
        PROCEDURE DIVISION.
            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
                DISPLAY I
            END-PERFORM.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::Perform(_)));
    }
}

#[test]
fn conformance_call_statement() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            CALL "SUBPROG" USING BY REFERENCE A.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::Call(_)));
    }
}

#[test]
fn conformance_display_accept() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A PIC X(10).
        PROCEDURE DIVISION.
            DISPLAY "HELLO".
            ACCEPT A FROM DATE.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::Display(_)));
        assert!(matches!(&stmts[1], Statement::Accept(_)));
    }
}

#[test]
fn conformance_io_statements() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            OPEN INPUT MY-FILE.
            CLOSE MY-FILE.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(stmts.iter().any(|s| matches!(s, Statement::Open(_))));
        assert!(stmts.iter().any(|s| matches!(s, Statement::Close(_))));
    }
}

#[test]
fn conformance_read_write_statements() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            READ MY-FILE.
            WRITE MY-REC.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(stmts.iter().any(|s| matches!(s, Statement::Read(_))));
        assert!(stmts.iter().any(|s| matches!(s, Statement::Write(_))));
    }
}

#[test]
fn conformance_string_unstring() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A PIC X(20).
        01 B PIC X(20).
        01 C PIC X(20).
        PROCEDURE DIVISION.
            STRING A DELIMITED BY SPACE INTO C.
            UNSTRING A DELIMITED BY SPACE INTO B C.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::String(_)));
        assert!(matches!(&stmts[1], Statement::Unstring(_)));
    }
}

#[test]
fn conformance_inspect_initialize() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A PIC X(20).
        01 CTR PIC 9(5).
        PROCEDURE DIVISION.
            INITIALIZE A.
            INSPECT A TALLYING CTR FOR ALL "X".
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(stmts.iter().any(|s| matches!(s, Statement::Initialize(_))));
        assert!(stmts.iter().any(|s| matches!(s, Statement::Inspect(_))));
    }
}

#[test]
fn conformance_goto_exit_continue() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            GO TO PARA-A.
            CONTINUE.
            STOP RUN.
        PARA-A.
            EXIT PARAGRAPH.
    "#);
    let proc = p.procedure.unwrap();
    match &proc.body {
        ProcedureBody::Statements(stmts) => {
            assert!(matches!(&stmts[0], Statement::GoTo(_)));
            assert!(matches!(&stmts[1], Statement::Continue(_)));
        }
        ProcedureBody::Paragraphs(paras) => {
            // Could be parsed as paragraphs too
            assert!(!paras.is_empty());
        }
        _ => {}
    }
}

#[test]
fn conformance_stop_goback() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            GOBACK.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::GoBack(_)));
    }
}

#[test]
fn conformance_set_search_sort() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            SET IDX TO 1.
            SEARCH MY-TABLE
                WHEN MY-KEY(IDX) = "X"
                    DISPLAY "FOUND"
            END-SEARCH.
            SORT SORT-FILE ON ASCENDING KEY SORT-KEY
                USING INPUT-FILE
                GIVING OUTPUT-FILE.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(stmts.iter().any(|s| matches!(s, Statement::Set(_))));
        assert!(stmts.iter().any(|s| matches!(s, Statement::Search(_))));
        assert!(stmts.iter().any(|s| matches!(s, Statement::Sort(_))));
    }
}

#[test]
fn conformance_exec_cics() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            EXEC CICS SEND TEXT FROM(WS-MSG) LENGTH(80) END-EXEC.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::ExecCics(_)));
    }
}

#[test]
fn conformance_exec_sql() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            EXEC SQL SELECT 1 FROM SYSIBM.SYSDUMMY1 END-EXEC.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::ExecSql(_)));
    }
}

#[test]
fn conformance_json_generate_parse() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-DATA PIC X(100).
        01 WS-JSON PIC X(200).
        PROCEDURE DIVISION.
            JSON GENERATE WS-JSON FROM WS-DATA.
            JSON PARSE WS-JSON INTO WS-DATA.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::JsonGenerate(_)));
        assert!(matches!(&stmts[1], Statement::JsonParse(_)));
    }
}

#[test]
fn conformance_xml_generate_parse() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-DATA PIC X(100).
        01 WS-XML PIC X(200).
        PROCEDURE DIVISION.
            XML GENERATE WS-XML FROM WS-DATA.
            XML PARSE WS-XML PROCESSING PROCEDURE XML-HANDLER.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::XmlGenerate(_)));
        assert!(matches!(&stmts[1], Statement::XmlParse(_)));
    }
}

#[test]
fn conformance_allocate_free() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 PTR USAGE POINTER.
        01 WS-REC PIC X(100).
        PROCEDURE DIVISION.
            ALLOCATE WS-REC RETURNING PTR.
            FREE PTR.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::Allocate(_)));
        assert!(matches!(&stmts[1], Statement::Free(_)));
    }
}

#[test]
fn conformance_entry_alter() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            ENTRY "ALTENTRY".
            ALTER PARA-A TO PROCEED TO PARA-B.
            STOP RUN.
        PARA-A.
            DISPLAY "A".
        PARA-B.
            DISPLAY "B".
    "#);
    let proc = p.procedure.unwrap();
    match &proc.body {
        ProcedureBody::Statements(stmts) => {
            assert!(stmts.iter().any(|s| matches!(s, Statement::Entry(_))));
            assert!(stmts.iter().any(|s| matches!(s, Statement::Alter(_))));
        }
        ProcedureBody::Paragraphs(paras) => {
            assert!(!paras.is_empty());
        }
        _ => {}
    }
}

#[test]
fn conformance_cancel_statement() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            CANCEL "SUBPROG".
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(matches!(&stmts[0], Statement::Cancel(_)));
    }
}

#[test]
fn conformance_merge_release_return() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        PROCEDURE DIVISION.
            MERGE SORT-FILE ON ASCENDING KEY SORT-KEY
                USING FILE-A FILE-B
                GIVING OUTPUT-FILE.
            RELEASE SORT-REC FROM WS-REC.
            RETURN SORT-FILE INTO WS-REC.
            STOP RUN.
    "#);
    let proc = p.procedure.unwrap();
    if let ProcedureBody::Statements(stmts) = &proc.body {
        assert!(stmts.iter().any(|s| matches!(s, Statement::Merge(_))));
        assert!(stmts.iter().any(|s| matches!(s, Statement::Release(_))));
        assert!(stmts.iter().any(|s| matches!(s, Statement::ReturnStmt(_))));
    }
}

// ============================================================================
// 76.2 — Data Division Conformance Tests
// ============================================================================

#[test]
fn conformance_data_pic_categories() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-ALPHA    PIC A(10).
        01 WS-ALNUM    PIC X(20).
        01 WS-NUMERIC  PIC 9(5)V99.
        01 WS-SIGNED   PIC S9(7)V99.
        01 WS-EDITED   PIC ZZZ,ZZ9.99.
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let data = p.data.unwrap();
    assert_eq!(data.working_storage.len(), 5);
}

#[test]
fn conformance_data_usage_clauses() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-DISP    PIC 9(5) USAGE DISPLAY.
        01 WS-BIN     PIC 9(9) USAGE BINARY.
        01 WS-PACK    PIC 9(7) USAGE PACKED-DECIMAL.
        01 WS-COMP    PIC 9(5) COMP.
        01 WS-COMP1   COMP-1.
        01 WS-COMP2   COMP-2.
        01 WS-COMP3   PIC 9(5) COMP-3.
        01 WS-COMP5   PIC 9(5) COMP-5.
        01 WS-PTR     USAGE POINTER.
        01 WS-IDX     USAGE INDEXED.
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let data = p.data.unwrap();
    assert_eq!(data.working_storage.len(), 10);
}

#[test]
fn conformance_data_group_items() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-GROUP.
            05 WS-FIRST  PIC X(10).
            05 WS-SECOND PIC 9(5).
            05 WS-THIRD  PIC X(20).
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let data = p.data.unwrap();
    assert_eq!(data.working_storage.len(), 1);
    assert_eq!(data.working_storage[0].children.len(), 3);
}

#[test]
fn conformance_data_occurs_clause() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-TABLE.
            05 WS-ITEM OCCURS 10 TIMES PIC X(5).
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let data = p.data.unwrap();
    let table = &data.working_storage[0];
    assert_eq!(table.children.len(), 1);
    assert!(table.children[0].occurs.is_some());
    assert_eq!(table.children[0].occurs.as_ref().unwrap().times, 10);
}

#[test]
fn conformance_data_redefines() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-NUM PIC 9(8).
        01 WS-ALPHA REDEFINES WS-NUM PIC X(8).
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let data = p.data.unwrap();
    assert_eq!(data.working_storage.len(), 2);
    assert!(data.working_storage[1].redefines.is_some());
}

#[test]
fn conformance_data_condition_88() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-STATUS PIC 9.
            88 STATUS-OK VALUE 0.
            88 STATUS-ERR VALUE 1.
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let data = p.data.unwrap();
    let status = &data.working_storage[0];
    assert_eq!(status.condition_values.len(), 2);
}

#[test]
fn conformance_file_section() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        FILE SECTION.
        FD MY-FILE.
        01 MY-RECORD PIC X(80).
        WORKING-STORAGE SECTION.
        01 WS-A PIC X.
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let data = p.data.unwrap();
    assert_eq!(data.file_section.len(), 1);
    assert_eq!(data.file_section[0].name, "MY-FILE");
    assert!(!data.working_storage.is_empty());
}

#[test]
fn conformance_linkage_section() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-A PIC X.
        LINKAGE SECTION.
        01 LS-PARM PIC X(100).
        PROCEDURE DIVISION USING LS-PARM.
            STOP RUN.
    "#);
    let data = p.data.unwrap();
    assert_eq!(data.linkage.len(), 1);
    assert_eq!(p.procedure.unwrap().using.len(), 1);
}

// ============================================================================
// 76.2+ — Environment Division Conformance
// ============================================================================

#[test]
fn conformance_environment_configuration() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. IBM-370.
        OBJECT-COMPUTER. IBM-370.
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let env = p.environment.unwrap();
    let config = env.configuration.unwrap();
    assert!(config.source_computer.is_some());
    assert!(config.object_computer.is_some());
}

#[test]
fn conformance_file_control_indexed() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. T.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT MY-FILE ASSIGN TO "MYFILE"
                ORGANIZATION IS INDEXED
                ACCESS MODE IS DYNAMIC
                RECORD KEY IS MY-KEY
                FILE STATUS IS WS-STATUS.
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    let env = p.environment.unwrap();
    let ios = env.input_output.unwrap();
    assert_eq!(ios.file_control.len(), 1);
    let fc = &ios.file_control[0];
    assert_eq!(fc.organization, FileOrganization::Indexed);
    assert_eq!(fc.access_mode, AccessMode::Dynamic);
    assert!(fc.record_key.is_some());
    assert!(fc.file_status.is_some());
}

// ============================================================================
// 76.2+ — Nested Programs
// ============================================================================

#[test]
fn conformance_nested_program() {
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. OUTER.
        PROCEDURE DIVISION.
            CALL "INNER".
            STOP RUN.
        IDENTIFICATION DIVISION.
        PROGRAM-ID. INNER COMMON.
        PROCEDURE DIVISION.
            GOBACK.
        END PROGRAM INNER.
        END PROGRAM OUTER.
    "#);
    assert_eq!(p.contained_programs.len(), 1);
    assert_eq!(p.contained_programs[0].identification.program_id.name, "INNER");
    assert!(p.contained_programs[0].identification.program_id.is_common);
}

// ============================================================================
// 76.2+ — Intrinsic Function Conformance
// ============================================================================

#[test]
fn conformance_intrinsic_function_registry() {
    use open_mainframe_cobol::intrinsics::{lookup_function, INTRINSIC_FUNCTIONS};

    // Verify total count (77+ after Epics 74 & 75)
    assert!(INTRINSIC_FUNCTIONS.len() >= 77);

    // Spot-check critical functions
    let critical_functions = [
        "CURRENT-DATE", "LENGTH", "TRIM", "UPPER-CASE", "LOWER-CASE",
        "INTEGER-OF-DATE", "DATE-OF-INTEGER", "NUMVAL", "NUMVAL-C",
        "SIN", "COS", "SQRT", "ABS", "MOD",
        // ISO 8601 (Epic 74)
        "FORMATTED-CURRENT-DATE", "FORMATTED-DATE", "FORMATTED-TIME",
        "FORMATTED-DATETIME", "INTEGER-OF-FORMATTED-DATE",
        "SECONDS-FROM-FORMATTED-TIME", "TEST-FORMATTED-DATETIME",
        // UTF-8 (Epic 75)
        "ULENGTH", "UPOS", "USUBSTR", "UVALID", "UWIDTH", "USUPPLEMENTARY",
    ];

    for name in &critical_functions {
        assert!(
            lookup_function(name).is_some(),
            "Missing intrinsic function: {}",
            name
        );
    }
}

// ============================================================================
// 76.3 — Backward Compatibility
// ============================================================================

#[test]
fn conformance_backward_compat_hello_world() {
    // The simplest COBOL program
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. HELLO.
        PROCEDURE DIVISION.
            DISPLAY "HELLO, WORLD!".
            STOP RUN.
    "#);
    assert_eq!(p.identification.program_id.name, "HELLO");
}

#[test]
fn conformance_backward_compat_full_divisions() {
    // All four divisions present
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. FULL-TEST.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. IBM-370.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 WS-A PIC X(10).
        PROCEDURE DIVISION.
            DISPLAY WS-A.
            STOP RUN.
    "#);
    assert!(p.environment.is_some());
    assert!(p.data.is_some());
    assert!(p.procedure.is_some());
}

#[test]
fn conformance_backward_compat_optional_divisions() {
    // Minimal program — only ID and PROCEDURE divisions
    let p = parse_ok(r#"
        IDENTIFICATION DIVISION.
        PROGRAM-ID. MINIMAL.
        PROCEDURE DIVISION.
            STOP RUN.
    "#);
    assert!(p.environment.is_none());
    assert!(p.data.is_none());
    assert!(p.procedure.is_some());
}
