//! Runtime SQLCA (SQL Communication Area) management.
//!
//! Provides the runtime representation of SQLCA for tracking
//! SQL execution status and error information.

/// SQL Communication Area for runtime status tracking.
#[derive(Debug, Clone)]
pub struct Sqlca {
    /// SQL return code
    sqlcode: i32,
    /// Error message length
    sqlerrml: i16,
    /// Error message text
    sqlerrmc: String,
    /// Error state info
    sqlerrd: [i32; 6],
    /// Warning flags
    sqlwarn: [char; 11],
    /// SQL state (5 characters)
    sqlstate: String,
}

impl Sqlca {
    /// Success code.
    pub const SUCCESS: i32 = 0;
    /// Not found code.
    pub const NOT_FOUND: i32 = 100;
    /// Duplicate key code.
    pub const DUPLICATE_KEY: i32 = -803;
    /// Null value without indicator.
    pub const NULL_VALUE: i32 = -305;
    /// Too many rows for SELECT INTO.
    pub const TOO_MANY_ROWS: i32 = -811;
    /// Cursor not open.
    pub const CURSOR_NOT_OPEN: i32 = -501;
    /// Cursor already open.
    pub const CURSOR_ALREADY_OPEN: i32 = -502;
    /// Deadlock or timeout.
    pub const DEADLOCK: i32 = -911;
    /// Connection error.
    pub const CONNECTION_ERROR: i32 = -30081;
    /// Name not found (table, view, etc.).
    pub const NAME_NOT_FOUND: i32 = -204;
    /// Syntax error.
    pub const SYNTAX_ERROR: i32 = -104;
    /// Resource unavailable.
    pub const RESOURCE_UNAVAILABLE: i32 = -904;
    /// Authorization failure.
    pub const AUTHORIZATION_FAILURE: i32 = -551;
    /// Check constraint violation.
    pub const CHECK_VIOLATION: i32 = -545;
    /// Foreign key violation.
    pub const FK_VIOLATION: i32 = -530;
    /// Numeric overflow / data exception.
    pub const DATA_EXCEPTION: i32 = -302;

    // --- Extended SQLCODE constants (Epic 305) ---

    /// Invalid cursor state.
    pub const INVALID_CURSOR_STATE: i32 = -503;
    /// Cursor not declared.
    pub const CURSOR_NOT_DECLARED: i32 = -504;
    /// Positioned UPDATE/DELETE without cursor.
    pub const NO_CURRENT_ROW: i32 = -508;
    /// Cursor not FOR UPDATE.
    pub const NOT_FOR_UPDATE: i32 = -510;
    /// Statement not prepared.
    pub const NOT_PREPARED: i32 = -518;
    /// Invalid column reference.
    pub const INVALID_COLUMN: i32 = -206;
    /// Ambiguous column reference.
    pub const AMBIGUOUS_COLUMN: i32 = -203;
    /// String truncation on assignment.
    pub const STRING_TRUNCATION: i32 = -303;
    /// Null indicator variable required.
    pub const NULL_INDICATOR_REQUIRED: i32 = -305;
    /// Invalid use of NULL.
    pub const INVALID_NULL: i32 = -407;
    /// Parameter count mismatch.
    pub const PARAM_COUNT_MISMATCH: i32 = -313;
    /// Division by zero.
    pub const DIVISION_BY_ZERO: i32 = -801;
    /// Invalid datetime format.
    pub const INVALID_DATETIME: i32 = -180;
    /// Datetime overflow.
    pub const DATETIME_OVERFLOW: i32 = -181;
    /// Invalid SQLSTATE value.
    pub const INVALID_SQLSTATE: i32 = -428;
    /// Table already exists.
    pub const TABLE_EXISTS: i32 = -601;
    /// Column already exists.
    pub const COLUMN_EXISTS: i32 = -612;
    /// Table space full.
    pub const TABLESPACE_FULL: i32 = -289;
    /// Log full.
    pub const LOG_FULL: i32 = -930;
    /// Lock timeout.
    pub const LOCK_TIMEOUT: i32 = -913;
    /// Resource limit exceeded.
    pub const RESOURCE_LIMIT: i32 = -905;
    /// SQL statement too long.
    pub const STMT_TOO_LONG: i32 = -101;
    /// Invalid character in name.
    pub const INVALID_NAME: i32 = -107;
    /// Undefined function.
    pub const UNDEFINED_FUNCTION: i32 = -440;
    /// Invalid data type for operation.
    pub const INVALID_TYPE: i32 = -408;
    /// Savepoint does not exist.
    pub const SAVEPOINT_NOT_FOUND: i32 = -880;
    /// FETCH direction requires SCROLL cursor.
    pub const SCROLL_REQUIRED: i32 = -243;
    /// Invalid FETCH orientation.
    pub const INVALID_FETCH: i32 = -244;
    /// Program not bound.
    pub const NOT_BOUND: i32 = -805;
    /// Package not found.
    pub const PACKAGE_NOT_FOUND: i32 = -818;
    /// Cascade delete rule violation.
    pub const CASCADE_VIOLATION: i32 = -532;
    /// Restrict delete rule violation.
    pub const RESTRICT_VIOLATION: i32 = -531;
    /// Invalid string length.
    pub const INVALID_LENGTH: i32 = -311;
    /// Numeric value out of range.
    pub const NUMERIC_OVERFLOW: i32 = -413;
    /// Invalid escape character.
    pub const INVALID_ESCAPE: i32 = -399;
    /// View cannot be updated.
    pub const VIEW_NOT_UPDATABLE: i32 = -150;
    /// WITH CHECK OPTION violation.
    pub const CHECK_OPTION_VIOLATION: i32 = -161;
    /// Timeout (not deadlock).
    pub const TIMEOUT: i32 = -952;
    /// SQL warning (positive).
    pub const SQL_WARNING: i32 = 1;
    /// Row count warning.
    pub const ROW_COUNT_WARNING: i32 = 12;

    /// Create a new SQLCA with success status.
    pub fn new() -> Self {
        Self {
            sqlcode: 0,
            sqlerrml: 0,
            sqlerrmc: String::new(),
            sqlerrd: [0; 6],
            sqlwarn: [' '; 11],
            sqlstate: "00000".to_string(),
        }
    }

    /// Reset SQLCA to initial state.
    pub fn reset(&mut self) {
        self.sqlcode = 0;
        self.sqlerrml = 0;
        self.sqlerrmc.clear();
        self.sqlerrd = [0; 6];
        self.sqlwarn = [' '; 11];
        self.sqlstate = "00000".to_string();
    }

    /// Get the SQLCODE value.
    pub fn sqlcode(&self) -> i32 {
        self.sqlcode
    }

    /// Set SQLCODE and related fields.
    pub fn set_sqlcode(&mut self, code: i32) {
        self.sqlcode = code;
        self.sqlstate = Self::code_to_state(code);
    }

    /// Set success status.
    pub fn set_success(&mut self) {
        self.set_sqlcode(Self::SUCCESS);
    }

    /// Set not found status.
    pub fn set_not_found(&mut self) {
        self.set_sqlcode(Self::NOT_FOUND);
    }

    /// Set error with message.
    pub fn set_error(&mut self, code: i32, message: &str) {
        self.sqlcode = code;
        self.sqlerrmc = message.chars().take(70).collect();
        self.sqlerrml = self.sqlerrmc.len() as i16;
        self.sqlstate = Self::code_to_state(code);
    }

    /// Get the error message.
    pub fn error_message(&self) -> &str {
        &self.sqlerrmc
    }

    /// Get SQLERRD values.
    pub fn sqlerrd(&self) -> &[i32; 6] {
        &self.sqlerrd
    }

    /// Set SQLERRD[2] (rows affected).
    pub fn set_rows_affected(&mut self, count: i32) {
        self.sqlerrd[2] = count;
    }

    /// Get rows affected (SQLERRD[2]).
    pub fn rows_affected(&self) -> i32 {
        self.sqlerrd[2]
    }

    /// Get SQLSTATE.
    pub fn sqlstate(&self) -> &str {
        &self.sqlstate
    }

    /// Check if last operation was successful.
    pub fn is_success(&self) -> bool {
        self.sqlcode == Self::SUCCESS
    }

    /// Check if no data was found.
    pub fn is_not_found(&self) -> bool {
        self.sqlcode == Self::NOT_FOUND
    }

    /// Check if an error occurred.
    pub fn is_error(&self) -> bool {
        self.sqlcode < 0
    }

    /// Set warning flag.
    pub fn set_warning(&mut self, index: usize, flag: char) {
        if index < 11 {
            self.sqlwarn[index] = flag;
            self.sqlwarn[0] = 'W'; // SQLWARN0 indicates warnings exist
        }
    }

    /// Check if any warnings.
    pub fn has_warnings(&self) -> bool {
        self.sqlwarn[0] == 'W'
    }

    /// Set SQLCA from a PostgreSQL SQLSTATE error code.
    ///
    /// Maps PostgreSQL 5-character SQLSTATE codes to DB2 SQLCODE values.
    pub fn set_from_pg_state(&mut self, pg_state: &str, message: &str) {
        let sqlcode = Self::pg_state_to_sqlcode(pg_state);
        self.set_error(sqlcode, message);
        // Preserve the original PG SQLSTATE
        self.sqlstate = pg_state.to_string();
    }

    /// Map a PostgreSQL SQLSTATE to a DB2 SQLCODE (comprehensive mapping).
    pub fn pg_state_to_sqlcode(pg_state: &str) -> i32 {
        match pg_state {
            // --- Success ---
            "00000" => Self::SUCCESS,

            // --- Warnings (class 01) ---
            "01000" => Self::SQL_WARNING,
            "01003" => Self::SQL_WARNING,    // Null value eliminated in set function
            "01004" => Self::STRING_TRUNCATION, // String data right-truncated (warning)

            // --- No data (class 02) ---
            "02000" => Self::NOT_FOUND,

            // --- Dynamic SQL (class 07) ---
            "07001" => Self::PARAM_COUNT_MISMATCH,
            "07003" => Self::NOT_PREPARED,

            // --- Connection (class 08) ---
            "08000" | "08001" | "08003" | "08004" | "08006" => Self::CONNECTION_ERROR,

            // --- Triggered action (class 09) ---
            "09000" => -723, // Triggered SQL statement failed

            // --- Cardinality (class 21) ---
            "21000" => Self::TOO_MANY_ROWS,

            // --- Data exception (class 22) ---
            "22001" => Self::STRING_TRUNCATION,
            "22002" => Self::NULL_INDICATOR_REQUIRED,
            "22003" => Self::DATA_EXCEPTION,
            "22004" => Self::INVALID_NULL,
            "22007" => Self::INVALID_DATETIME,
            "22008" => Self::DATETIME_OVERFLOW,
            "22012" => Self::DIVISION_BY_ZERO,
            "22019" => Self::INVALID_ESCAPE,
            "22025" => Self::INVALID_ESCAPE,
            "22026" => Self::INVALID_LENGTH,

            // --- Integrity constraints (class 23) ---
            "23000" => Self::FK_VIOLATION,   // General integrity constraint
            "23001" => Self::RESTRICT_VIOLATION,
            "23502" => Self::INVALID_NULL,   // Not-null violation
            "23503" => Self::FK_VIOLATION,
            "23504" => Self::CASCADE_VIOLATION,
            "23505" => Self::DUPLICATE_KEY,
            "23514" => Self::CHECK_VIOLATION,

            // --- Invalid cursor state (class 24) ---
            "24000" | "24501" => Self::CURSOR_NOT_OPEN,
            "24502" => Self::CURSOR_ALREADY_OPEN,
            "24504" => Self::NO_CURRENT_ROW,

            // --- Invalid transaction state (class 25) ---
            "25000" | "25001" | "25P01" | "25P02" => -918, // Transaction state invalid

            // --- Savepoint (class 3B) ---
            "3B000" | "3B001" => Self::SAVEPOINT_NOT_FOUND,

            // --- Cursor not declared (class 34) ---
            "34000" => Self::CURSOR_NOT_DECLARED,

            // --- Serialization/deadlock (class 40) ---
            "40001" => Self::DEADLOCK,
            "40003" => Self::DEADLOCK, // Statement completion unknown
            "40P01" => Self::DEADLOCK,

            // --- Syntax/access rule (class 42) ---
            "42000" => Self::SYNTAX_ERROR,
            "42501" => Self::AUTHORIZATION_FAILURE,
            "42601" => Self::SYNTAX_ERROR,
            "42602" => Self::INVALID_NAME,
            "42702" => Self::AMBIGUOUS_COLUMN,
            "42703" => Self::INVALID_COLUMN,
            "42704" => Self::NAME_NOT_FOUND,
            "42710" => Self::TABLE_EXISTS,
            "42711" => Self::COLUMN_EXISTS,
            "42803" => Self::SYNTAX_ERROR,     // Grouping error
            "42804" => Self::INVALID_TYPE,
            "42807" => Self::VIEW_NOT_UPDATABLE,
            "42821" => Self::INVALID_TYPE,
            "42829" => Self::NOT_FOR_UPDATE,
            "42830" => Self::FK_VIOLATION,     // Invalid FK
            "42846" => Self::INVALID_TYPE,     // Cannot convert type
            "42878" => Self::SCROLL_REQUIRED,
            "42879" => Self::INVALID_FETCH,
            "42883" => Self::UNDEFINED_FUNCTION,
            "42884" => Self::UNDEFINED_FUNCTION,
            "42P01" => Self::NAME_NOT_FOUND,   // Undefined table
            "42P02" => Self::NAME_NOT_FOUND,   // Undefined parameter

            // --- WITH CHECK OPTION (class 44) ---
            "44000" => Self::CHECK_OPTION_VIOLATION,

            // --- Insufficient resources (class 53) ---
            "53000" | "53100" | "53200" | "53300" => Self::RESOURCE_UNAVAILABLE,

            // --- Operator intervention / system error (class 57) ---
            "57011" => Self::RESOURCE_UNAVAILABLE,
            "57014" => Self::TIMEOUT,
            "57015" => Self::LOG_FULL,
            "57033" => Self::LOCK_TIMEOUT,

            // --- Class-level fallbacks ---
            s if s.starts_with("01") => Self::SQL_WARNING,
            s if s.starts_with("02") => Self::NOT_FOUND,
            s if s.starts_with("08") => Self::CONNECTION_ERROR,
            s if s.starts_with("22") => Self::DATA_EXCEPTION,
            s if s.starts_with("23") => Self::FK_VIOLATION,
            s if s.starts_with("24") => Self::CURSOR_NOT_OPEN,
            s if s.starts_with("3B") => Self::SAVEPOINT_NOT_FOUND,
            s if s.starts_with("40") => Self::DEADLOCK,
            s if s.starts_with("42") => Self::SYNTAX_ERROR,
            s if s.starts_with("53") => Self::RESOURCE_UNAVAILABLE,
            s if s.starts_with("57") => Self::RESOURCE_UNAVAILABLE,

            // --- Unknown ---
            _ => -999,
        }
    }

    /// Convert SQLCODE to SQLSTATE (comprehensive bidirectional mapping).
    fn code_to_state(code: i32) -> String {
        match code {
            // --- Success / Warning ---
            0 => "00000",       // Success
            1 => "01000",       // SQL warning
            12 => "01545",      // Row count warning
            100 => "02000",     // Not found

            // --- Syntax / Name Resolution ---
            -101 => "54001",    // Statement too long
            -104 => "42601",    // Syntax error
            -107 => "42602",    // Invalid character in name
            -150 => "42807",    // View not updatable
            -161 => "44000",    // WITH CHECK OPTION violation
            -180 => "22007",    // Invalid datetime format
            -181 => "22008",    // Datetime overflow
            -203 => "42702",    // Ambiguous column
            -204 => "42704",    // Name not found
            -206 => "42703",    // Invalid column reference

            // --- Data Exceptions ---
            -243 => "42878",    // SCROLL required
            -244 => "42879",    // Invalid FETCH orientation
            -289 => "57011",    // Tablespace full
            -302 => "22003",    // Numeric overflow / data exception
            -303 => "22001",    // String truncation
            -305 => "22002",    // Null indicator required
            -311 => "22026",    // Invalid string length
            -313 => "07001",    // Parameter count mismatch
            -399 => "22025",    // Invalid escape character

            // --- NULL / Type Errors ---
            -407 => "23502",    // Invalid use of NULL
            -408 => "42821",    // Invalid data type
            -413 => "22003",    // Numeric value out of range
            -428 => "HY024",    // Invalid SQLSTATE value
            -440 => "42884",    // Undefined function

            // --- Cursor Errors ---
            -501 => "24501",    // Cursor not open
            -502 => "24502",    // Cursor already open
            -503 => "24501",    // Invalid cursor state
            -504 => "34000",    // Cursor not declared
            -508 => "24504",    // No current row for positioned operation
            -510 => "42829",    // Cursor not FOR UPDATE
            -518 => "07003",    // Statement not prepared

            // --- Integrity Constraints ---
            -530 => "23503",    // FK violation
            -531 => "23001",    // Restrict delete rule
            -532 => "23504",    // Cascade rule
            -545 => "23514",    // Check violation
            -551 => "42501",    // Authorization failure

            // --- Object Existence ---
            -601 => "42710",    // Table already exists
            -612 => "42711",    // Column already exists

            // --- Duplicate / Uniqueness ---
            -801 => "22012",    // Division by zero
            -803 => "23505",    // Unique violation
            -805 => "51002",    // Program not bound
            -811 => "21000",    // Cardinality violation
            -818 => "51003",    // Package not found

            // --- Savepoint ---
            -880 => "3B001",    // Savepoint does not exist

            // --- Concurrency / Resources ---
            -904 => "57011",    // Resource unavailable
            -905 => "57014",    // Resource limit exceeded
            -911 => "40001",    // Deadlock / serialization failure
            -913 => "57033",    // Lock timeout
            -930 => "57015",    // Log full
            -952 => "57014",    // Timeout

            // --- Connection ---
            -30081 => "08001",  // Connection error

            // --- Fallback ---
            _ if code < 0 => "HY000", // General error
            _ => "00000",
        }
        .to_string()
    }
}

impl Default for Sqlca {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for SQLCA (for testing and simulation).
pub struct SqlcaBuilder {
    sqlca: Sqlca,
}

impl SqlcaBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            sqlca: Sqlca::new(),
        }
    }

    /// Set SQLCODE.
    pub fn sqlcode(mut self, code: i32) -> Self {
        self.sqlca.set_sqlcode(code);
        self
    }

    /// Set error message.
    pub fn error(mut self, code: i32, message: &str) -> Self {
        self.sqlca.set_error(code, message);
        self
    }

    /// Set rows affected.
    pub fn rows_affected(mut self, count: i32) -> Self {
        self.sqlca.set_rows_affected(count);
        self
    }

    /// Build the SQLCA.
    pub fn build(self) -> Sqlca {
        self.sqlca
    }
}

impl Default for SqlcaBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sqlca_default() {
        let sqlca = Sqlca::new();
        assert_eq!(sqlca.sqlcode(), 0);
        assert!(sqlca.is_success());
        assert!(!sqlca.is_error());
    }

    #[test]
    fn test_sqlca_not_found() {
        let mut sqlca = Sqlca::new();
        sqlca.set_not_found();
        assert_eq!(sqlca.sqlcode(), 100);
        assert!(sqlca.is_not_found());
        assert_eq!(sqlca.sqlstate(), "02000");
    }

    #[test]
    fn test_sqlca_error() {
        let mut sqlca = Sqlca::new();
        sqlca.set_error(-803, "Duplicate key violation");
        assert!(sqlca.is_error());
        assert_eq!(sqlca.sqlcode(), -803);
        assert_eq!(sqlca.error_message(), "Duplicate key violation");
        assert_eq!(sqlca.sqlstate(), "23505");
    }

    #[test]
    fn test_sqlca_rows_affected() {
        let mut sqlca = Sqlca::new();
        sqlca.set_rows_affected(42);
        assert_eq!(sqlca.rows_affected(), 42);
    }

    #[test]
    fn test_sqlca_warnings() {
        let mut sqlca = Sqlca::new();
        assert!(!sqlca.has_warnings());
        sqlca.set_warning(1, 'W');
        assert!(sqlca.has_warnings());
    }

    #[test]
    fn test_sqlca_builder() {
        let sqlca = SqlcaBuilder::new()
            .sqlcode(100)
            .rows_affected(0)
            .build();

        assert!(sqlca.is_not_found());
        assert_eq!(sqlca.rows_affected(), 0);
    }

    #[test]
    fn test_sqlca_reset() {
        let mut sqlca = Sqlca::new();
        sqlca.set_error(-803, "Error");
        sqlca.reset();
        assert!(sqlca.is_success());
        assert!(sqlca.error_message().is_empty());
    }

    // --- PostgreSQL Error Mapping Tests (Story 302.3) ---

    #[test]
    fn test_pg_unique_violation_to_duplicate_key() {
        let code = Sqlca::pg_state_to_sqlcode("23505");
        assert_eq!(code, Sqlca::DUPLICATE_KEY); // -803
    }

    #[test]
    fn test_pg_undefined_table_to_name_not_found() {
        let code = Sqlca::pg_state_to_sqlcode("42P01");
        assert_eq!(code, Sqlca::NAME_NOT_FOUND); // -204
    }

    #[test]
    fn test_pg_undefined_column_to_invalid_column() {
        let code = Sqlca::pg_state_to_sqlcode("42703");
        assert_eq!(code, Sqlca::INVALID_COLUMN); // -206
    }

    #[test]
    fn test_pg_deadlock_to_deadlock() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("40001"), Sqlca::DEADLOCK);
        assert_eq!(Sqlca::pg_state_to_sqlcode("40P01"), Sqlca::DEADLOCK);
    }

    #[test]
    fn test_pg_connection_failure() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("08001"), Sqlca::CONNECTION_ERROR);
        assert_eq!(Sqlca::pg_state_to_sqlcode("08006"), Sqlca::CONNECTION_ERROR);
    }

    #[test]
    fn test_pg_syntax_error() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("42601"), Sqlca::SYNTAX_ERROR);
    }

    #[test]
    fn test_pg_no_data() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("02000"), Sqlca::NOT_FOUND);
    }

    #[test]
    fn test_pg_success() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("00000"), Sqlca::SUCCESS);
    }

    #[test]
    fn test_set_from_pg_state() {
        let mut sqlca = Sqlca::new();
        sqlca.set_from_pg_state("23505", "duplicate key value violates unique constraint");
        assert_eq!(sqlca.sqlcode(), Sqlca::DUPLICATE_KEY);
        assert_eq!(sqlca.sqlstate(), "23505");
        assert!(sqlca.error_message().contains("duplicate key"));
    }

    #[test]
    fn test_pg_check_violation() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("23514"), Sqlca::CHECK_VIOLATION);
    }

    #[test]
    fn test_pg_fk_violation() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("23503"), Sqlca::FK_VIOLATION);
    }

    #[test]
    fn test_pg_data_exception_class() {
        assert_eq!(Sqlca::pg_state_to_sqlcode("22003"), Sqlca::DATA_EXCEPTION);
        // 22001 now maps more precisely to STRING_TRUNCATION
        assert_eq!(Sqlca::pg_state_to_sqlcode("22001"), Sqlca::STRING_TRUNCATION);
        // Unrecognized 22xxx falls back to DATA_EXCEPTION
        assert_eq!(Sqlca::pg_state_to_sqlcode("22999"), Sqlca::DATA_EXCEPTION);
    }

    #[test]
    fn test_code_to_state_new_codes() {
        let mut sqlca = Sqlca::new();
        sqlca.set_sqlcode(Sqlca::NAME_NOT_FOUND);
        assert_eq!(sqlca.sqlstate(), "42704");

        sqlca.set_sqlcode(Sqlca::SYNTAX_ERROR);
        assert_eq!(sqlca.sqlstate(), "42601");

        sqlca.set_sqlcode(Sqlca::CONNECTION_ERROR);
        assert_eq!(sqlca.sqlstate(), "08001");
    }

    // --- Epic 305: Extended SQLCODE/SQLSTATE Mapping Tests ---

    #[test]
    fn test_code_to_state_cursor_errors() {
        let mut sqlca = Sqlca::new();

        sqlca.set_sqlcode(Sqlca::CURSOR_NOT_DECLARED);
        assert_eq!(sqlca.sqlstate(), "34000");

        sqlca.set_sqlcode(Sqlca::NO_CURRENT_ROW);
        assert_eq!(sqlca.sqlstate(), "24504");

        sqlca.set_sqlcode(Sqlca::NOT_FOR_UPDATE);
        assert_eq!(sqlca.sqlstate(), "42829");

        sqlca.set_sqlcode(Sqlca::NOT_PREPARED);
        assert_eq!(sqlca.sqlstate(), "07003");
    }

    #[test]
    fn test_code_to_state_data_errors() {
        let mut sqlca = Sqlca::new();

        sqlca.set_sqlcode(Sqlca::DIVISION_BY_ZERO);
        assert_eq!(sqlca.sqlstate(), "22012");

        sqlca.set_sqlcode(Sqlca::STRING_TRUNCATION);
        assert_eq!(sqlca.sqlstate(), "22001");

        sqlca.set_sqlcode(Sqlca::INVALID_DATETIME);
        assert_eq!(sqlca.sqlstate(), "22007");

        sqlca.set_sqlcode(Sqlca::NUMERIC_OVERFLOW);
        assert_eq!(sqlca.sqlstate(), "22003");
    }

    #[test]
    fn test_code_to_state_constraint_errors() {
        let mut sqlca = Sqlca::new();

        sqlca.set_sqlcode(Sqlca::RESTRICT_VIOLATION);
        assert_eq!(sqlca.sqlstate(), "23001");

        sqlca.set_sqlcode(Sqlca::CASCADE_VIOLATION);
        assert_eq!(sqlca.sqlstate(), "23504");

        sqlca.set_sqlcode(Sqlca::TABLE_EXISTS);
        assert_eq!(sqlca.sqlstate(), "42710");
    }

    #[test]
    fn test_code_to_state_resource_errors() {
        let mut sqlca = Sqlca::new();

        sqlca.set_sqlcode(Sqlca::LOCK_TIMEOUT);
        assert_eq!(sqlca.sqlstate(), "57033");

        sqlca.set_sqlcode(Sqlca::SAVEPOINT_NOT_FOUND);
        assert_eq!(sqlca.sqlstate(), "3B001");

        sqlca.set_sqlcode(Sqlca::SCROLL_REQUIRED);
        assert_eq!(sqlca.sqlstate(), "42878");
    }

    #[test]
    fn test_pg_state_extended_mappings() {
        // Datetime
        assert_eq!(Sqlca::pg_state_to_sqlcode("22007"), Sqlca::INVALID_DATETIME);
        // Division by zero
        assert_eq!(Sqlca::pg_state_to_sqlcode("22012"), Sqlca::DIVISION_BY_ZERO);
        // Undefined function
        assert_eq!(Sqlca::pg_state_to_sqlcode("42883"), Sqlca::UNDEFINED_FUNCTION);
        // Lock timeout
        assert_eq!(Sqlca::pg_state_to_sqlcode("57033"), Sqlca::LOCK_TIMEOUT);
        // Savepoint not found
        assert_eq!(Sqlca::pg_state_to_sqlcode("3B001"), Sqlca::SAVEPOINT_NOT_FOUND);
        // Table exists
        assert_eq!(Sqlca::pg_state_to_sqlcode("42710"), Sqlca::TABLE_EXISTS);
        // NOT FOR UPDATE
        assert_eq!(Sqlca::pg_state_to_sqlcode("42829"), Sqlca::NOT_FOR_UPDATE);
        // Restrict violation
        assert_eq!(Sqlca::pg_state_to_sqlcode("23001"), Sqlca::RESTRICT_VIOLATION);
    }

    #[test]
    fn test_pg_state_class_fallbacks() {
        // Class 01 → warning
        assert_eq!(Sqlca::pg_state_to_sqlcode("01999"), Sqlca::SQL_WARNING);
        // Class 24 → cursor not open
        assert_eq!(Sqlca::pg_state_to_sqlcode("24999"), Sqlca::CURSOR_NOT_OPEN);
        // Class 3B → savepoint
        assert_eq!(Sqlca::pg_state_to_sqlcode("3B999"), Sqlca::SAVEPOINT_NOT_FOUND);
        // Class 40 → deadlock
        assert_eq!(Sqlca::pg_state_to_sqlcode("40999"), Sqlca::DEADLOCK);
        // Class 53 → resource unavailable
        assert_eq!(Sqlca::pg_state_to_sqlcode("53999"), Sqlca::RESOURCE_UNAVAILABLE);
    }

    #[test]
    fn test_bidirectional_sqlcode_sqlstate_roundtrip() {
        // For key codes, verify code→state→code roundtrip via pg mapping
        let codes_and_states = [
            (Sqlca::DUPLICATE_KEY, "23505"),
            (Sqlca::FK_VIOLATION, "23503"),
            (Sqlca::CHECK_VIOLATION, "23514"),
            (Sqlca::DEADLOCK, "40001"),
            (Sqlca::SYNTAX_ERROR, "42601"),
            (Sqlca::NOT_FOUND, "02000"),
            (Sqlca::CONNECTION_ERROR, "08001"),
            (Sqlca::DIVISION_BY_ZERO, "22012"),
        ];

        for (code, state) in &codes_and_states {
            let mut sqlca = Sqlca::new();
            sqlca.set_sqlcode(*code);
            assert_eq!(sqlca.sqlstate(), *state, "code_to_state({code})");
            assert_eq!(Sqlca::pg_state_to_sqlcode(state), *code, "pg_state_to_sqlcode({state})");
        }
    }
}
