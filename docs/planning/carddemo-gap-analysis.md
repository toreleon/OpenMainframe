# CardDemo Gap Analysis — COBOL/CICS Interpreter Fixes

## Context

Running AWS CardDemo (17 CICS programs, 42 total) on OpenMainframe revealed **9 missing interpreter features** blocking full functionality. This document captures the comprehensive audit, v3.0 epic overlap, and implementation plan.

**Already fixed (session 2025-02-17):** REDEFINES shared storage, JUSTIFIED RIGHT, RefMod on MOVE target, VSAM alternate index key position.

**Already working (confirmed by audit):** HANDLE ABEND with LABEL (`cics_bridge.rs:781-795`), WRITEQ TD (dispatcher has "WRITEQ" handler).

---

## Overlap with v3.0 Epics

| Fix | v3.0 Epic | Decision |
|-----|-----------|----------|
| PERFORM THRU | Epic 501 (Story 501.2) | **Implement now** — blocks 10/17 programs |
| STRING DELIMITED BY | *Not in any epic* | **Implement now** — blocks 7/17 programs |
| NUMVAL-C / NUMVAL | Epic 502 (Story 502.2) | **Implement now** — subset needed for CardDemo |
| TRIM | Epic 502 (Story 502.1) | **Implement now** — 2 programs need it |
| INTEGER-OF-DATE / DATE-OF-INTEGER | Epic 502 (Story 502.3) | **Implement now** — 1 program needs it |
| ASKTIME / FORMATTIME | *Not in any epic* | **Implement now** — 1 program needs it |
| INQUIRE PROGRAM | *Not in any epic* | **Implement now** — stub, 2 programs |
| ON SIZE ERROR / ROUNDED | *Not in any epic* | **Defer** — correctness, not blocking |
| COMP-3 runtime | Epic 504 | **Defer** — works via CobolValue abstraction |

---

## CardDemo Program Feature Matrix

| Program | PERFORM THRU | STRING DELIMITED | NUMVAL-C | TRIM | Date Funcs | ASKTIME | INQUIRE |
|---------|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| COSGN00C | | | | | | | |
| COMEN01C | X | X | | | | | X |
| COADM01C | X | X | | | | | X |
| COACTVWC | X | X | | | | | |
| COACTUPC | X | | X | X | | | |
| COCRDLIC | X | | | | | | |
| COCRDSLC | X | | | | | | |
| COCRDUPC | X | | X | X | | | |
| COTRN00C | X | | | | | | |
| COTRN01C | X | | | | | | |
| COTRN02C | X | | X | | | | |
| COBIL00C | | | | | | X | |
| CORPT00C | | | X | | X | | |
| COUSR00C | | | | | | | |
| COUSR01C | | | | | | | |
| COUSR02C | | | | | | | |
| COUSR03C | | | | | | | |
| **Total** | **10** | **3** | **4** | **2** | **1** | **1** | **2** |

---

## Fix 1: PERFORM THRU (10 programs)

**Problem:** `PERFORM X THRU Y` only executes paragraph X. The `thru` field is parsed by the AST (`PerformStatement.thru: Option<String>`) but ignored during lowering. Additionally, paragraph order is lost in `HashMap<String, Vec<SimpleStatement>>`.

**Files to modify:**
- `crates/open-mainframe-runtime/src/interpreter.rs`
- `crates/open-mainframe/src/commands/interpret.rs`

### 1a. Add paragraph ordering to SimpleProgram (`interpreter.rs`)
```rust
pub struct SimpleProgram {
    // ... existing fields ...
    pub paragraph_order: Vec<String>,  // paragraph names in source order
}
```

### 1b. Collect paragraph order during lowering (`interpret.rs`)
In `ProcedureBody::Paragraphs` and `ProcedureBody::Sections` handlers, after inserting into `paragraphs` HashMap:
```rust
paragraph_order.push(para.name.to_uppercase());
```

### 1c. Add `thru` to SimpleStatement::Perform (`interpreter.rs`)
```rust
Perform { target: String, thru: Option<String>, times: Option<u32> }
```

### 1d. Extract THRU in lowering (`interpret.rs`)
```rust
// Statement::Perform handler (~line 1089)
let thru = p.thru.as_ref().cloned();
Ok(Some(SimpleStatement::Perform { target, thru, times }))
```

### 1e. Execute paragraph range (`interpreter.rs`)
```rust
// Perform handler (~line 1047)
if let Some(ref thru_target) = thru {
    let start = target.to_uppercase();
    let end = thru_target.to_uppercase();
    let mut executing = false;
    for para_name in &program.paragraph_order {
        if *para_name == start { executing = true; }
        if executing {
            if let Some(stmts) = program.paragraphs.get(para_name) {
                execute_statements(stmts, program, env)?;
            }
        }
        if *para_name == end { break; }
    }
} else {
    // existing single-paragraph logic
}
```

### 1f. Update all SimpleProgram construction sites
Add `paragraph_order: Vec::new()` to all 5 test instances in `interpreter.rs` and pass collected order in production (`interpret.rs:626`).

---

## Fix 2: STRING DELIMITED BY (7 programs)

**Problem:** Lowering (`interpret.rs:1300-1308`) discards `StringDelimiter` from each `StringSource`. All sources are concatenated in full regardless of delimiter mode.

**CardDemo uses 3 modes:**
- `DELIMITED BY SIZE` — use entire field (10+ instances)
- `DELIMITED BY SPACE` — stop at first space (1 instance, COMEN01C:174)
- `DELIMITED BY '  '` — stop at first occurrence of literal (1 instance, COMEN01C:165)

**AST already parsed correctly:**
```rust
pub enum StringDelimiter {
    Size,                   // DELIMITED BY SIZE
    Value(Expression),      // DELIMITED BY SPACE or literal
}
```

**Files to modify:**
- `crates/open-mainframe-runtime/src/interpreter.rs`
- `crates/open-mainframe/src/commands/interpret.rs`

### 2a. Add delimiter enum to runtime (`interpreter.rs`)
```rust
pub enum SimpleStringDelimiter {
    Size,
    Expr(SimpleExpr),
}
```

### 2b. Update StringConcat variant (`interpreter.rs`)
```rust
StringConcat {
    sources: Vec<(SimpleExpr, SimpleStringDelimiter)>,
    into: String,
}
```

### 2c. Update lowering to preserve delimiters (`interpret.rs`)
```rust
// Statement::String handler (~line 1300)
let sources = s.sources.iter().filter_map(|src| {
    let expr = convert_expr(&src.value).ok()?;
    let delim = match &src.delimited_by {
        StringDelimiter::Size => SimpleStringDelimiter::Size,
        StringDelimiter::Value(e) => {
            SimpleStringDelimiter::Expr(convert_expr(e).unwrap_or(SimpleExpr::String(" ".into())))
        }
    };
    Some((expr, delim))
}).collect();
```

### 2d. Update execution to respect delimiters (`interpreter.rs`)
```rust
// StringConcat handler (~line 1196)
for (src_expr, delim) in sources {
    let val = eval_expr(src_expr, env)?.to_display_string();
    let portion = match delim {
        SimpleStringDelimiter::Size => val,
        SimpleStringDelimiter::Expr(delim_expr) => {
            let delim_str = eval_expr(delim_expr, env)?.to_display_string();
            val.split(&delim_str).next().unwrap_or(&val).to_string()
        }
    };
    result.push_str(&portion);
}
```

---

## Fix 3: FUNCTION NUMVAL-C and NUMVAL (4 programs)

**Problem:** Unknown functions return `CobolValue::from_i64(0)`. COACTUPC, COCRDUPC, COTRN02C, CORPT00C use NUMVAL-C for parsing user input like `"$1,234.56"` into numeric values.

**File to modify:** `crates/open-mainframe-runtime/src/interpreter.rs` — FunctionCall match block (~line 2052)

```rust
"NUMVAL" => {
    let s = arg.to_display_string().trim().to_string();
    parse_numeric_string(&s)  // helper: strip spaces, parse f64
}
"NUMVAL-C" => {
    let s = arg.to_display_string()
        .replace(['$', ',', ' '], "").trim().to_string();
    parse_numeric_string(&s)  // helper: strip currency/commas, parse f64
}
```

Helper function `parse_numeric_string(s: &str) -> Result<CobolValue>`:
- Handle optional sign (+/-)
- Handle optional decimal point
- Handle "CR"/"DB" suffix (credit/debit)
- Return `CobolValue::Numeric(NumericValue)`

---

## Fix 4: FUNCTION TRIM (2 programs)

**Problem:** Returns 0. COACTUPC and COCRDUPC use `FUNCTION UPPER-CASE(FUNCTION TRIM(...))`.

**File to modify:** `crates/open-mainframe-runtime/src/interpreter.rs` — FunctionCall match block

```rust
"TRIM" => {
    let s = arg.to_display_string();
    Ok(CobolValue::Alphanumeric(s.trim().to_string()))
}
```

---

## Fix 5: FUNCTION INTEGER-OF-DATE and DATE-OF-INTEGER (1 program)

**Problem:** Returns 0. CORPT00C uses `DATE-OF-INTEGER(INTEGER-OF-DATE(d) - 1)` to calculate the last day of the previous month.

**File to modify:** `crates/open-mainframe-runtime/src/interpreter.rs`

```rust
"INTEGER-OF-DATE" => {
    // YYYYMMDD -> Lilian day number (days since Oct 15, 1582 = day 1)
    let n = to_numeric(&arg).integer_part() as i64;
    let yyyy = n / 10000;
    let mm = (n % 10000) / 100;
    let dd = n % 100;
    Ok(CobolValue::from_i64(date_to_lilian(yyyy, mm, dd)))
}
"DATE-OF-INTEGER" => {
    // Lilian day number -> YYYYMMDD
    let lilian = to_numeric(&arg).integer_part() as i64;
    let (y, m, d) = lilian_to_date(lilian);
    Ok(CobolValue::from_i64(y * 10000 + m * 100 + d))
}
```

**Lilian day algorithm:** The Lilian calendar starts on October 15, 1582 (Gregorian reform). Implementation uses standard Julian Day Number conversion offset by the Lilian epoch (JDN 2299161).

---

## Fix 6: EXEC CICS ASKTIME / FORMATTIME (1 program)

**Problem:** Unhandled CICS commands fall through to "skipped" log. COBIL00C (`GET-CURRENT-TIMESTAMP` paragraph, lines 249-266) uses:
```cobol
EXEC CICS ASKTIME ABSTIME(WS-ABS-TIME) END-EXEC
EXEC CICS FORMATTIME ABSTIME(WS-ABS-TIME)
    YYYYMMDD(WS-CUR-DATE-X10) DATESEP('-')
    TIME(WS-CUR-TIME-X08) TIMESEP(':')
END-EXEC
```

**File to modify:** `crates/open-mainframe/src/commands/cics_bridge.rs` — command dispatch match block (~line 940)

### 6a. ASKTIME handler
```rust
"ASKTIME" => {
    // Store current epoch seconds as ABSTIME
    let now = chrono::Local::now();
    let abstime = now.timestamp();
    if let Some(var) = Self::get_option_str(options, "ABSTIME") {
        env.set(&var, CobolValue::from_i64(abstime))?;
    }
    self.runtime.eib.set_response(CicsResponse::Normal);
    Ok(())
}
```

### 6b. FORMATTIME handler
```rust
"FORMATTIME" => {
    let now = chrono::Local::now();
    let datesep = Self::get_option_str(options, "DATESEP").unwrap_or("-".into());
    let timesep = Self::get_option_str(options, "TIMESEP").unwrap_or(":".into());
    if let Some(var) = Self::get_option_str(options, "YYYYMMDD") {
        let fmt = format!("%Y{}%m{}%d", datesep, datesep);
        env.set(&var, CobolValue::Alphanumeric(now.format(&fmt).to_string()))?;
    }
    if let Some(var) = Self::get_option_str(options, "TIME") {
        let fmt = format!("%H{}%M{}%S", timesep, timesep);
        env.set(&var, CobolValue::Alphanumeric(now.format(&fmt).to_string()))?;
    }
    self.runtime.eib.set_response(CicsResponse::Normal);
    Ok(())
}
```

Note: `chrono` is already a workspace dependency.

---

## Fix 7: EXEC CICS INQUIRE PROGRAM (2 programs)

**Problem:** Falls through to unknown command handler. COMEN01C and COADM01C use `EXEC CICS INQUIRE PROGRAM(name) NOHANDLE` to check if a program is installed before XCTL. If EIBRESP = NORMAL, proceed with XCTL; otherwise show "not installed" message.

**File to modify:** `crates/open-mainframe/src/commands/cics_bridge.rs`

```rust
"INQUIRE" => {
    // Stub: always return NORMAL (program exists)
    // CardDemo uses this to check if COPAUS0C is installed
    self.runtime.eib.set_response(CicsResponse::Normal);
    Ok(())
}
```

---

## Deferred Items

| Item | v3.0 Epic | Reason to Defer |
|------|-----------|-----------------|
| ON SIZE ERROR | None | Correctness issue, not blocking navigation |
| ROUNDED | None | Financial precision, not blocking navigation |
| COMP-3 pack/unpack | Epic 504 | CobolValue abstraction handles it for now |
| OCCURS DEPENDING ON | None | Linkage section only, COMMAREA works with full length |
| Numeric Editing (PIC Z,$) | Epic 503 | Display formatting, not blocking functionality |

---

## Files Modified (Summary)

| File | Fixes |
|------|-------|
| `crates/open-mainframe-runtime/src/interpreter.rs` | 1 (PERFORM THRU), 2 (STRING), 3 (NUMVAL), 4 (TRIM), 5 (date functions) |
| `crates/open-mainframe/src/commands/interpret.rs` | 1 (paragraph order + THRU lowering), 2 (STRING delimiter lowering) |
| `crates/open-mainframe/src/commands/cics_bridge.rs` | 6 (ASKTIME/FORMATTIME), 7 (INQUIRE) |

---

## Verification

1. **Compile:** `cargo check --workspace` — zero errors
2. **Tests:** `cargo test --workspace` — all 1030+ tests pass
3. **Build:** `cargo build --release`
4. **Smoke test individual fixes:**
   - PERFORM THRU: Test COBOL program with `PERFORM A THRU C` across 3 paragraphs
   - STRING: Test with `DELIMITED BY SIZE`, `DELIMITED BY SPACE`, and literal delimiter
   - Functions: Test `FUNCTION NUMVAL-C("$1,234.56")`, `FUNCTION TRIM("  X  ")`, `FUNCTION INTEGER-OF-DATE(20260217)`
5. **CardDemo end-to-end:**
   - `./carddemo-config/run-carddemo.sh`
   - Sign in -> Main Menu -> Select option 2 (Account Update) -> verify navigation works
   - Select option 1 (Account View) -> verify correct program (COACTVWC, not COBIL00C)
   - Test all 11 menu options reach their target programs
   - Bill Payment (option 10) -> verify ASKTIME/FORMATTIME doesn't crash
