//! End-to-end integration tests for the DB2 crate (Epic 309).
//!
//! These tests exercise the full pipeline:
//!   Preprocess COBOL → DBRM → BIND → Runtime execution
//! as well as DCLGEN round-trip verification.

use open_mainframe_db2::preprocess::{Dbrm, SqlPreprocessor};
use open_mainframe_db2::runtime::{
    CursorManager, RuntimeHostVariable, RuntimeStatement, SqlExecutor, SqlRow,
    SqlTranslator, SqlValue,
};
use open_mainframe_db2::utilities::{
    BindAction, BindOptions, Binder, ColumnInfo, Dclgen, IsolationLevel,
    ReleaseOption, TableInfo, ValidateOption,
};
use open_mainframe_db2::preprocess::{HostVariableUsage, SqlStatementType};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Story 309.1 — Preprocess → DBRM → BIND → Execute Pipeline
// ---------------------------------------------------------------------------

/// A COBOL source fragment containing multiple EXEC SQL operations.
const COBOL_SOURCE: &str = r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CUSTNO  PIC S9(9) COMP.
       01  WS-NAME    PIC X(30).
       01  WS-BALANCE PIC S9(9)V99 COMP-3.
       01  WS-NEWNAME PIC X(30).
       PROCEDURE DIVISION.
           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.
           EXEC SQL
             SELECT NAME, BALANCE
             INTO :WS-NAME, :WS-BALANCE
             FROM CUSTOMER
             WHERE CUSTNO = :WS-CUSTNO
           END-EXEC.
           EXEC SQL
             INSERT INTO CUSTOMER (CUSTNO, NAME, BALANCE)
             VALUES (:WS-CUSTNO, :WS-NAME, :WS-BALANCE)
           END-EXEC.
           EXEC SQL
             DECLARE C1 CURSOR FOR
             SELECT NAME FROM CUSTOMER
             WHERE BALANCE > 0
           END-EXEC.
           EXEC SQL
             OPEN C1
           END-EXEC.
           EXEC SQL
             FETCH C1 INTO :WS-NAME
           END-EXEC.
           EXEC SQL
             CLOSE C1
           END-EXEC.
           STOP RUN.
"#;

#[test]
fn test_preprocess_extracts_all_statements() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    // We expect: INCLUDE, SelectInto, Insert, DeclareCursor, Open, Fetch, Close
    assert!(
        result.sql_statements.len() >= 6,
        "Expected at least 6 SQL statements, got {}",
        result.sql_statements.len()
    );

    // Verify statement types
    let types: Vec<SqlStatementType> = result
        .sql_statements
        .iter()
        .map(|s| s.stmt_type)
        .collect();

    assert!(types.contains(&SqlStatementType::Include));
    assert!(types.contains(&SqlStatementType::SelectInto));
    assert!(types.contains(&SqlStatementType::Insert));
    assert!(types.contains(&SqlStatementType::DeclareCursor));
    assert!(types.contains(&SqlStatementType::Open));
    assert!(types.contains(&SqlStatementType::Fetch));
    assert!(types.contains(&SqlStatementType::Close));
}

#[test]
fn test_preprocess_extracts_host_variables() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    // Check host variables exist
    assert!(
        !result.host_variables.is_empty(),
        "Expected host variables to be extracted"
    );

    // The SELECT INTO statement should have output variables
    let output_vars: Vec<_> = result
        .host_variables
        .iter()
        .filter(|v| v.usage == HostVariableUsage::Output)
        .collect();
    assert!(
        !output_vars.is_empty(),
        "Expected output host variables from SELECT INTO"
    );

    // The SELECT INTO statement should have input variables
    let input_vars: Vec<_> = result
        .host_variables
        .iter()
        .filter(|v| v.usage == HostVariableUsage::Input)
        .collect();
    assert!(
        !input_vars.is_empty(),
        "Expected input host variables from WHERE clause"
    );
}

#[test]
fn test_preprocess_replaces_exec_sql_with_calls() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    // The COBOL source should have EXEC SQL replaced with CALL statements
    assert!(
        !result.cobol_source.contains("EXEC SQL"),
        "EXEC SQL should be replaced in output"
    );

    // Should contain CALL 'SQL' references (the generated CALL statements)
    assert!(
        result.cobol_source.contains("CALL"),
        "Output should contain generated CALL statements"
    );
}

#[test]
fn test_dbrm_generation_from_preprocess() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    let mut dbrm = Dbrm::new("CUSTPROG");
    dbrm.set_collection_id("PRODLIB");
    dbrm.add_statements(&result.sql_statements, &result.host_variables);

    // DBRM should contain all extracted SQL statements
    assert_eq!(
        dbrm.statements.len(),
        result.sql_statements.len(),
        "DBRM should contain all preprocessed statements"
    );

    // Verify listing contains program name and statements
    let listing = dbrm.to_listing();
    assert!(listing.contains("CUSTPROG"));
    assert!(listing.contains("PRODLIB"));
    assert!(listing.contains("STATEMENT 001"));
}

#[test]
fn test_dbrm_file_roundtrip() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    let mut dbrm = Dbrm::new("CUSTPROG");
    dbrm.add_statements(&result.sql_statements, &result.host_variables);

    // Write to temp file
    let temp_dir = std::env::temp_dir();
    let dbrm_path = temp_dir.join("test_integration_dbrm.json");
    dbrm.write_to_file(&dbrm_path).unwrap();

    // Read back
    let loaded = Dbrm::read_from_file(&dbrm_path).unwrap();
    assert_eq!(loaded.program_name, "CUSTPROG");
    assert_eq!(loaded.statements.len(), dbrm.statements.len());

    // Verify each statement matches
    for (orig, loaded) in dbrm.statements.iter().zip(loaded.statements.iter()) {
        assert_eq!(orig.sql, loaded.sql);
        assert_eq!(orig.stmt_type, loaded.stmt_type);
        assert_eq!(orig.number, loaded.number);
    }

    std::fs::remove_file(&dbrm_path).ok();
}

#[test]
fn test_bind_dbrm_produces_package() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    let mut dbrm = Dbrm::new("CUSTPROG");
    dbrm.set_collection_id("PRODLIB");
    dbrm.add_statements(&result.sql_statements, &result.host_variables);

    let options = BindOptions {
        package: Some("CUSTPKG".to_string()),
        collection: "PRODLIB".to_string(),
        owner: "SYSADM".to_string(),
        isolation: IsolationLevel::CursorStability,
        action: BindAction::Replace,
        validate: ValidateOption::Bind,
        release: ReleaseOption::Commit,
    };

    let binder = Binder::with_options(options);
    let package = binder.bind(&dbrm).unwrap();

    assert_eq!(package.name, "CUSTPKG");
    assert_eq!(package.collection, "PRODLIB");
    assert!(
        package.statement_count > 0,
        "Package should contain bound statements"
    );
}

#[test]
fn test_bind_generates_prepare_statements() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    let mut dbrm = Dbrm::new("CUSTPROG");
    dbrm.add_statements(&result.sql_statements, &result.host_variables);

    let binder = Binder::new();
    let prepares = binder.generate_prepares(&dbrm).unwrap();

    // Should generate PREPARE statements for DML
    assert!(
        !prepares.is_empty(),
        "Binder should generate PREPARE statements"
    );
}

#[test]
fn test_full_pipeline_select_into_mock() {
    // 1. Preprocess
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    // 2. DBRM
    let mut dbrm = Dbrm::new("CUSTPROG");
    dbrm.add_statements(&result.sql_statements, &result.host_variables);

    // 3. BIND
    let binder = Binder::new();
    let _package = binder.bind(&dbrm).unwrap();

    // 4. Execute SELECT INTO in mock mode
    let mut executor = SqlExecutor::new();

    // Set up mock result
    let mut row = SqlRow::new();
    row.add_column("NAME", SqlValue::String("Alice".to_string()));
    row.add_column("BALANCE", SqlValue::Float(1000.50));
    executor.set_mock_results(vec![row]);

    // Find the SELECT INTO statement
    let select_stmt = result
        .sql_statements
        .iter()
        .find(|s| s.stmt_type == SqlStatementType::SelectInto)
        .unwrap();

    // Build RuntimeStatement
    let host_vars: Vec<RuntimeHostVariable> = result
        .host_variables
        .iter()
        .filter(|v| v.statement_number == select_stmt.number)
        .map(|v| RuntimeHostVariable {
            name: v.name.clone(),
            indicator: v.indicator.clone(),
            usage: v.usage,
        })
        .collect();

    let runtime_stmt = RuntimeStatement {
        number: select_stmt.number,
        sql: select_stmt.sql.clone(),
        stmt_type: select_stmt.stmt_type,
        host_variables: host_vars,
    };

    let mut input = HashMap::new();
    input.insert("WS-CUSTNO".to_string(), SqlValue::Integer(1001));

    let output = executor.execute_select_into(&runtime_stmt, &input).unwrap();
    assert!(executor.sqlca().is_success());
    assert!(
        !output.is_empty(),
        "SELECT INTO should return output values"
    );
}

#[test]
fn test_full_pipeline_insert_mock() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    let mut dbrm = Dbrm::new("CUSTPROG");
    dbrm.add_statements(&result.sql_statements, &result.host_variables);

    let binder = Binder::new();
    let _package = binder.bind(&dbrm).unwrap();

    let mut executor = SqlExecutor::new();

    let insert_stmt = result
        .sql_statements
        .iter()
        .find(|s| s.stmt_type == SqlStatementType::Insert)
        .unwrap();

    let host_vars: Vec<RuntimeHostVariable> = result
        .host_variables
        .iter()
        .filter(|v| v.statement_number == insert_stmt.number)
        .map(|v| RuntimeHostVariable {
            name: v.name.clone(),
            indicator: v.indicator.clone(),
            usage: v.usage,
        })
        .collect();

    let runtime_stmt = RuntimeStatement {
        number: insert_stmt.number,
        sql: insert_stmt.sql.clone(),
        stmt_type: insert_stmt.stmt_type,
        host_variables: host_vars,
    };

    let mut input = HashMap::new();
    input.insert("WS-CUSTNO".to_string(), SqlValue::Integer(1001));
    input.insert(
        "WS-NAME".to_string(),
        SqlValue::String("Alice".to_string()),
    );
    input.insert("WS-BALANCE".to_string(), SqlValue::Float(1000.50));

    let rows = executor.execute_insert(&runtime_stmt, &input).unwrap();
    assert!(executor.sqlca().is_success());
    assert_eq!(rows, 1);
}

#[test]
fn test_full_pipeline_cursor_mock() {
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(COBOL_SOURCE).unwrap();

    let mut dbrm = Dbrm::new("CUSTPROG");
    dbrm.add_statements(&result.sql_statements, &result.host_variables);

    let binder = Binder::new();
    let _package = binder.bind(&dbrm).unwrap();

    // Set up cursor manager with mock data
    let mut cursor_mgr = CursorManager::new();

    // Find the DECLARE CURSOR statement to get its SQL
    let declare_stmt = result
        .sql_statements
        .iter()
        .find(|s| s.stmt_type == SqlStatementType::DeclareCursor)
        .unwrap();

    // Find the FETCH statement to get host variables
    let fetch_stmt = result
        .sql_statements
        .iter()
        .find(|s| s.stmt_type == SqlStatementType::Fetch)
        .unwrap();

    let fetch_vars: Vec<RuntimeHostVariable> = result
        .host_variables
        .iter()
        .filter(|v| v.statement_number == fetch_stmt.number)
        .map(|v| RuntimeHostVariable {
            name: v.name.clone(),
            indicator: v.indicator.clone(),
            usage: v.usage,
        })
        .collect();

    // Declare cursor
    let cursor = open_mainframe_db2::Cursor::new("C1", &declare_stmt.sql, fetch_vars);
    cursor_mgr.declare(cursor).unwrap();

    // Open cursor
    let input = HashMap::new();
    cursor_mgr.open("C1", &input).unwrap();
    assert!(cursor_mgr.sqlca().is_success());

    // Set mock results
    let mut row1 = SqlRow::new();
    row1.add_column("NAME", SqlValue::String("Alice".to_string()));
    let mut row2 = SqlRow::new();
    row2.add_column("NAME", SqlValue::String("Bob".to_string()));
    cursor_mgr.set_mock_results("C1", vec![row1, row2]);

    // Fetch first row
    let result1 = cursor_mgr.fetch("C1").unwrap();
    assert!(cursor_mgr.sqlca().is_success());
    assert!(!result1.is_empty());

    // Fetch second row
    let result2 = cursor_mgr.fetch("C1").unwrap();
    assert!(cursor_mgr.sqlca().is_success());
    assert!(!result2.is_empty());

    // Fetch past end → SQLCODE 100
    let result3 = cursor_mgr.fetch("C1").unwrap();
    assert!(cursor_mgr.sqlca().is_not_found());
    assert!(result3.is_empty());

    // Close cursor
    cursor_mgr.close("C1").unwrap();
    assert!(cursor_mgr.sqlca().is_success());
}

#[test]
fn test_pipeline_error_bad_table_sqlcode() {
    // Verify that error handling produces correct SQLCA fields
    let mut executor = SqlExecutor::new();

    // Execute a SELECT INTO on a nonexistent table — mock mode returns NOT FOUND
    // when no mock results are provided (simulating the table doesn't exist scenario)
    let stmt = RuntimeStatement {
        number: 1,
        sql: "SELECT NAME INTO :WS-NAME FROM NONEXISTENT WHERE ID = :WS-ID".to_string(),
        stmt_type: SqlStatementType::SelectInto,
        host_variables: vec![
            RuntimeHostVariable {
                name: "WS-NAME".to_string(),
                indicator: None,
                usage: HostVariableUsage::Output,
            },
            RuntimeHostVariable {
                name: "WS-ID".to_string(),
                indicator: None,
                usage: HostVariableUsage::Input,
            },
        ],
    };

    let mut input = HashMap::new();
    input.insert("WS-ID".to_string(), SqlValue::Integer(999));

    // No mock results → NOT FOUND
    let output = executor.execute_select_into(&stmt, &input).unwrap();
    assert_eq!(executor.sqlca().sqlcode(), 100, "Expected SQLCODE 100 for not found");
    assert!(output.is_empty());
}

#[test]
fn test_sql_translation_in_pipeline() {
    // Verify SQL gets translated during the pipeline
    let translator = SqlTranslator::new();

    // Simulated DB2 SQL from preprocessing
    let db2_sql = "SELECT NAME INTO :WS-NAME FROM CUSTOMER WHERE CUSTNO = :WS-CUSTNO FETCH FIRST 1 ROW ONLY WITH UR";
    let pg_sql = translator.translate(db2_sql);

    // Should have LIMIT instead of FETCH FIRST
    assert!(pg_sql.contains("LIMIT 1"));
    assert!(!pg_sql.contains("FETCH FIRST"));

    // Should strip WITH UR
    assert!(!pg_sql.contains("WITH UR"));
}

#[test]
fn test_sqlca_populated_correctly_after_operations() {
    let mut executor = SqlExecutor::new();

    // Successful INSERT
    let stmt = RuntimeStatement {
        number: 1,
        sql: "INSERT INTO T (A) VALUES (:V)".to_string(),
        stmt_type: SqlStatementType::Insert,
        host_variables: vec![RuntimeHostVariable {
            name: "V".to_string(),
            indicator: None,
            usage: HostVariableUsage::Input,
        }],
    };

    let mut input = HashMap::new();
    input.insert("V".to_string(), SqlValue::Integer(42));

    executor.execute_insert(&stmt, &input).unwrap();
    assert_eq!(executor.sqlca().sqlcode(), 0, "Insert should set SQLCODE 0");
    assert_eq!(
        executor.sqlca().rows_affected(),
        1,
        "Insert should set SQLERRD(3) to 1"
    );
}

// ---------------------------------------------------------------------------
// Story 309.2 — DCLGEN Round-Trip Test
// ---------------------------------------------------------------------------

#[test]
fn test_dclgen_generates_valid_cobol_copybook() {
    let table = TableInfo {
        schema: "PUBLIC".to_string(),
        table: "CUSTOMER".to_string(),
        columns: vec![
            ColumnInfo {
                name: "CUSTNO".to_string(),
                data_type: "INTEGER".to_string(),
                max_length: None,
                precision: None,
                scale: None,
                nullable: false,
            },
            ColumnInfo {
                name: "CUST_NAME".to_string(),
                data_type: "VARCHAR".to_string(),
                max_length: Some(30),
                precision: None,
                scale: None,
                nullable: true,
            },
            ColumnInfo {
                name: "BALANCE".to_string(),
                data_type: "DECIMAL".to_string(),
                max_length: None,
                precision: Some(10),
                scale: Some(2),
                nullable: true,
            },
        ],
    };

    let dclgen = Dclgen::new();
    let copybook = dclgen.generate(&table).unwrap();

    // Should contain valid COBOL structure
    assert!(copybook.contains("01  DCLCUSTOMER"));
    assert!(copybook.contains("CUSTNO"));
    assert!(copybook.contains("CUST-NAME"));
    assert!(copybook.contains("BALANCE"));

    // Should contain correct PIC clauses
    assert!(copybook.contains("PIC S9(9) COMP"));     // INTEGER
    assert!(copybook.contains("PIC X(30)"));           // VARCHAR(30)
    assert!(copybook.contains("PIC S9(8)V9(2) COMP-3")); // DECIMAL(10,2)

    // Should contain null indicators for nullable columns
    assert!(copybook.contains("CUST-NAME-IND"));
    assert!(copybook.contains("BALANCE-IND"));
}

#[test]
fn test_dclgen_copybook_host_vars_recognized_by_preprocessor() {
    // Generate DCLGEN copybook
    let table = TableInfo {
        schema: "PUBLIC".to_string(),
        table: "EMPLOYEE".to_string(),
        columns: vec![
            ColumnInfo {
                name: "EMP_ID".to_string(),
                data_type: "INTEGER".to_string(),
                max_length: None,
                precision: None,
                scale: None,
                nullable: false,
            },
            ColumnInfo {
                name: "EMP_NAME".to_string(),
                data_type: "VARCHAR".to_string(),
                max_length: Some(50),
                precision: None,
                scale: None,
                nullable: true,
            },
        ],
    };

    let dclgen = Dclgen::new();
    let copybook = dclgen.generate(&table).unwrap();

    // Verify the generated names match what DB2 EXEC SQL would use
    // DCLGEN converts underscores to hyphens: EMP_ID → EMP-ID
    assert!(copybook.contains("EMP-ID"), "DCLGEN should produce EMP-ID");
    assert!(
        copybook.contains("EMP-NAME"),
        "DCLGEN should produce EMP-NAME"
    );

    // Now write a COBOL program using those host variable names
    let cobol_with_dclgen = format!(
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
{}
       PROCEDURE DIVISION.
           EXEC SQL
             SELECT EMP_ID, EMP_NAME
             INTO :EMP-ID, :EMP-NAME
             FROM EMPLOYEE
             WHERE EMP_ID = :EMP-ID
           END-EXEC.
           STOP RUN.
"#,
        copybook
    );

    // Preprocess the program — host variables from DCLGEN should be recognized
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(&cobol_with_dclgen).unwrap();

    // Should have found host variables
    assert!(
        !result.host_variables.is_empty(),
        "Preprocessor should recognize host variables from DCLGEN copybook"
    );

    // Should have extracted the SELECT INTO statement
    let has_select = result
        .sql_statements
        .iter()
        .any(|s| s.stmt_type == SqlStatementType::SelectInto);
    assert!(has_select, "Should extract SELECT INTO statement");

    // Host variable names from DCLGEN should appear in the extracted variables
    let var_names: Vec<&str> = result
        .host_variables
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert!(
        var_names.contains(&"EMP-ID"),
        "EMP-ID host variable should be extracted; found: {:?}",
        var_names
    );
    assert!(
        var_names.contains(&"EMP-NAME"),
        "EMP-NAME host variable should be extracted; found: {:?}",
        var_names
    );
}

#[test]
fn test_dclgen_write_and_read_file() {
    let table = TableInfo {
        schema: "PUBLIC".to_string(),
        table: "TEST_TABLE".to_string(),
        columns: vec![ColumnInfo {
            name: "COL1".to_string(),
            data_type: "INTEGER".to_string(),
            max_length: None,
            precision: None,
            scale: None,
            nullable: false,
        }],
    };

    let dclgen = Dclgen::new();
    let temp_dir = std::env::temp_dir();
    let path = temp_dir.join("test_dclgen_roundtrip.cpy");

    dclgen.write_to_file(&table, &path).unwrap();

    // Read back and verify contents
    let content = std::fs::read_to_string(&path).unwrap();
    assert!(content.contains("DCLTEST_TABLE"));
    assert!(content.contains("COL1"));

    std::fs::remove_file(&path).ok();
}

#[test]
fn test_full_dclgen_to_runtime_pipeline() {
    // 1. Generate DCLGEN copybook for ACCOUNT table
    let table = TableInfo {
        schema: "BANK".to_string(),
        table: "ACCOUNT".to_string(),
        columns: vec![
            ColumnInfo {
                name: "ACCT_NUM".to_string(),
                data_type: "INTEGER".to_string(),
                max_length: None,
                precision: None,
                scale: None,
                nullable: false,
            },
            ColumnInfo {
                name: "ACCT_NAME".to_string(),
                data_type: "VARCHAR".to_string(),
                max_length: Some(40),
                precision: None,
                scale: None,
                nullable: false,
            },
            ColumnInfo {
                name: "BALANCE".to_string(),
                data_type: "DECIMAL".to_string(),
                max_length: None,
                precision: Some(12),
                scale: Some(2),
                nullable: true,
            },
        ],
    };

    let dclgen = Dclgen::new();
    let copybook = dclgen.generate(&table).unwrap();

    // 2. Build COBOL source with that copybook
    let cobol_source = format!(
        r#"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
{}
       PROCEDURE DIVISION.
           EXEC SQL
             SELECT ACCT_NAME, BALANCE
             INTO :ACCT-NAME, :BALANCE
             FROM ACCOUNT
             WHERE ACCT_NUM = :ACCT-NUM
           END-EXEC.
           STOP RUN.
"#,
        copybook
    );

    // 3. Preprocess
    let mut preprocessor = SqlPreprocessor::new();
    let result = preprocessor.process(&cobol_source).unwrap();

    // 4. DBRM
    let mut dbrm = Dbrm::new("ACCTPROG");
    dbrm.add_statements(&result.sql_statements, &result.host_variables);
    assert!(!dbrm.statements.is_empty());

    // 5. BIND
    let binder = Binder::new();
    let package = binder.bind(&dbrm).unwrap();
    assert!(package.statement_count > 0);

    // 6. Execute in mock mode
    let mut executor = SqlExecutor::new();

    let select_stmt = result
        .sql_statements
        .iter()
        .find(|s| s.stmt_type == SqlStatementType::SelectInto)
        .unwrap();

    let host_vars: Vec<RuntimeHostVariable> = result
        .host_variables
        .iter()
        .filter(|v| v.statement_number == select_stmt.number)
        .map(|v| RuntimeHostVariable {
            name: v.name.clone(),
            indicator: v.indicator.clone(),
            usage: v.usage,
        })
        .collect();

    let runtime_stmt = RuntimeStatement {
        number: select_stmt.number,
        sql: select_stmt.sql.clone(),
        stmt_type: select_stmt.stmt_type,
        host_variables: host_vars,
    };

    // Set mock result
    let mut mock_row = SqlRow::new();
    mock_row.add_column("ACCT_NAME", SqlValue::String("Savings".to_string()));
    mock_row.add_column("BALANCE", SqlValue::Float(5432.10));
    executor.set_mock_results(vec![mock_row]);

    let mut input = HashMap::new();
    input.insert("ACCT-NUM".to_string(), SqlValue::Integer(100));

    let output = executor
        .execute_select_into(&runtime_stmt, &input)
        .unwrap();
    assert!(executor.sqlca().is_success());
    assert!(!output.is_empty(), "Should return query results");
}
