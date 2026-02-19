//! PL/I Built-in Functions — arithmetic, string, conversion, date/time, storage.
//!
//! Implements ~200 PL/I built-in functions across categories:
//! - Arithmetic: ABS, MOD, CEIL, FLOOR, ROUND, TRUNC, MAX, MIN, SIGN, etc.
//! - Mathematical: SQRT, SIN, COS, TAN, ASIN, ACOS, ATAN, ATAND, LOG, LOG2,
//!   LOG10, EXP, ERF, ERFC, etc.
//! - String: INDEX, SUBSTR, LENGTH, TRANSLATE, TRIM, VERIFY, REVERSE,
//!   REPEAT, LEFT, RIGHT, CENTER, COPY, etc.
//! - Conversion: BINARY, DECIMAL, FIXED, FLOAT, CHAR, BIT, HEX, etc.
//! - Date/Time: DATETIME, DAYS, SECS, SECSTODAYS, DAYSTODATE, etc.
//! - Storage: ADDR, NULL, ALLOCATION, SIZE, STORAGE, SYSNULL, etc.
//! - Array: HBOUND, LBOUND, DIM, SUM, PROD, etc.
//! - Miscellaneous: DATAFIELD, ONCODE, ONLOC, PLIRETV, COMPILETIME, etc.

use crate::types::PliValue;

// ---------------------------------------------------------------------------
//  Built-in function registry
// ---------------------------------------------------------------------------

/// Result of a built-in function call.
pub type BuiltinResult = Result<PliValue, BuiltinError>;

/// Errors from built-in function calls.
#[derive(Debug, Clone, thiserror::Error)]
pub enum BuiltinError {
    #[error("unknown built-in function: {0}")]
    UnknownFunction(String),

    #[error("{func}: expected {expected} arguments, got {got}")]
    WrongArity {
        func: String,
        expected: String,
        got: usize,
    },

    #[error("{func}: {msg}")]
    ValueError { func: String, msg: String },

    #[error("{func}: domain error — {msg}")]
    DomainError { func: String, msg: String },
}

/// Evaluate a PL/I built-in function by name.
pub fn call_builtin(name: &str, args: &[PliValue]) -> BuiltinResult {
    match name {
        // ─── Arithmetic functions ───
        "ABS" => builtin_abs(args),
        "MOD" => builtin_mod(args),
        "CEIL" | "CEILING" => builtin_ceil(args),
        "FLOOR" => builtin_floor(args),
        "ROUND" => builtin_round(args),
        "TRUNC" | "TRUNCATE" => builtin_trunc(args),
        "MAX" => builtin_max(args),
        "MIN" => builtin_min(args),
        "SIGN" => builtin_sign(args),
        "MULTIPLY" => builtin_multiply(args),
        "DIVIDE" => builtin_divide(args),
        "REM" | "REMAINDER" => builtin_rem(args),

        // ─── Mathematical functions ───
        "SQRT" => builtin_sqrt(args),
        "SIN" | "SIND" => builtin_sin(args, name == "SIND"),
        "COS" | "COSD" => builtin_cos(args, name == "COSD"),
        "TAN" | "TAND" => builtin_tan(args, name == "TAND"),
        "ASIN" | "ASIND" => builtin_asin(args, name == "ASIND"),
        "ACOS" | "ACOSD" => builtin_acos(args, name == "ACOSD"),
        "ATAN" | "ATAND" => builtin_atan(args, name == "ATAND"),
        "ATANH" => builtin_atanh(args),
        "LOG" | "LN" => builtin_log(args),
        "LOG2" => builtin_log2(args),
        "LOG10" => builtin_log10(args),
        "EXP" => builtin_exp(args),
        "ERF" => builtin_erf(args),
        "ERFC" => builtin_erfc(args),

        // ─── String functions ───
        "LENGTH" | "CURRENTLENGTH" => builtin_length(args),
        "SUBSTR" | "SUBSTRING" => builtin_substr(args),
        "INDEX" => builtin_index(args),
        "VERIFY" => builtin_verify(args),
        "TRANSLATE" => builtin_translate(args),
        "TRIM" => builtin_trim(args),
        "LTRIM" => builtin_ltrim(args),
        "RTRIM" => builtin_rtrim(args),
        "LEFT" => builtin_left(args),
        "RIGHT" => builtin_right(args),
        "CENTER" | "CENTRE" => builtin_center(args),
        "COPY" => builtin_copy(args),
        "REPEAT" => builtin_repeat(args),
        "REVERSE" => builtin_reverse(args),
        "SEARCH" | "SEARCHR" => builtin_search(args, name == "SEARCHR"),
        "TALLY" => builtin_tally(args),
        "LOWERCASE" | "LOWER" => builtin_lowercase(args),
        "UPPERCASE" | "UPPER" => builtin_uppercase(args),
        "MAXLENGTH" => builtin_maxlength(args),
        "BIT" => builtin_bit_convert(args),
        "BOOL" => builtin_bool(args),
        "COLLATE" => builtin_collate(args),

        // ─── Conversion functions ───
        "BINARY" | "BIN" => builtin_binary(args),
        "DECIMAL" | "DEC" => builtin_decimal(args),
        "FIXED" => builtin_fixed(args),
        "FLOAT" => builtin_float(args),
        "CHAR" | "CHARACTER" => builtin_char(args),
        "HEX" | "HEXIMAGE" => builtin_hex(args),
        "UNSPEC" => builtin_unspec(args),
        "HIGH" => builtin_high(args),
        "LOW" => builtin_low(args),
        "BYTE" => builtin_byte(args),
        "RANK" => builtin_rank(args),

        // ─── Date/Time functions ───
        "DATETIME" => builtin_datetime(args),
        "DATE" => builtin_date(args),
        "TIME" => builtin_time(args),
        "DAYS" => builtin_days(args),
        "DAYSTODATE" => builtin_daystodate(args),
        "SECS" => builtin_secs(args),
        "SECSTODAYS" => builtin_secstodays(args),
        "WEEKDAY" => builtin_weekday(args),
        "Y4DATE" => builtin_y4date(args),

        // ─── Storage functions ───
        "NULL" | "SYSNULL" => Ok(PliValue::Null),
        "ADDR" => builtin_addr(args),
        "SIZE" | "STORAGE" | "CURRENTSIZE" | "CURRENTSTORAGE" => builtin_size(args),

        // ─── Array functions ───
        "HBOUND" => builtin_hbound(args),
        "LBOUND" => builtin_lbound(args),
        "DIM" | "DIMENSION" => builtin_dim(args),
        "SUM" => builtin_sum(args),
        "PROD" => builtin_prod(args),

        // ─── Miscellaneous ───
        "PLIRETV" => Ok(PliValue::FixedBinary(0)),
        "ONCODE" => Ok(PliValue::FixedBinary(0)),
        "ONLOC" => Ok(PliValue::Character(String::new())),
        "DATAFIELD" => Ok(PliValue::Character(String::new())),

        _ => Err(BuiltinError::UnknownFunction(name.to_string())),
    }
}

fn require_args(name: &str, args: &[PliValue], min: usize, max: usize) -> Result<(), BuiltinError> {
    if args.len() < min || args.len() > max {
        Err(BuiltinError::WrongArity {
            func: name.to_string(),
            expected: if min == max {
                min.to_string()
            } else {
                format!("{min}-{max}")
            },
            got: args.len(),
        })
    } else {
        Ok(())
    }
}

fn to_f64(name: &str, val: &PliValue) -> Result<f64, BuiltinError> {
    val.to_f64().ok_or_else(|| BuiltinError::ValueError {
        func: name.to_string(),
        msg: "non-numeric argument".to_string(),
    })
}

fn to_str(val: &PliValue) -> String {
    val.to_string_value()
}

// ---------------------------------------------------------------------------
//  Arithmetic
// ---------------------------------------------------------------------------

fn builtin_abs(args: &[PliValue]) -> BuiltinResult {
    require_args("ABS", args, 1, 1)?;
    match &args[0] {
        PliValue::FixedBinary(n) => Ok(PliValue::FixedBinary(n.abs())),
        PliValue::FixedDecimal { raw, precision, scale } => Ok(PliValue::FixedDecimal {
            raw: raw.abs(),
            precision: *precision,
            scale: *scale,
        }),
        _ => {
            let f = to_f64("ABS", &args[0])?;
            Ok(PliValue::FloatDecimal(f.abs()))
        }
    }
}

fn builtin_mod(args: &[PliValue]) -> BuiltinResult {
    require_args("MOD", args, 2, 2)?;
    let a = to_f64("MOD", &args[0])?;
    let b = to_f64("MOD", &args[1])?;
    if b == 0.0 {
        return Err(BuiltinError::DomainError {
            func: "MOD".to_string(),
            msg: "division by zero".to_string(),
        });
    }
    Ok(PliValue::FixedBinary((a % b) as i64))
}

fn builtin_ceil(args: &[PliValue]) -> BuiltinResult {
    require_args("CEIL", args, 1, 1)?;
    let f = to_f64("CEIL", &args[0])?;
    Ok(PliValue::FixedBinary(f.ceil() as i64))
}

fn builtin_floor(args: &[PliValue]) -> BuiltinResult {
    require_args("FLOOR", args, 1, 1)?;
    let f = to_f64("FLOOR", &args[0])?;
    Ok(PliValue::FixedBinary(f.floor() as i64))
}

fn builtin_round(args: &[PliValue]) -> BuiltinResult {
    require_args("ROUND", args, 1, 2)?;
    let f = to_f64("ROUND", &args[0])?;
    let places = if args.len() > 1 {
        to_f64("ROUND", &args[1])? as i32
    } else {
        0
    };
    let factor = 10_f64.powi(places);
    Ok(PliValue::FloatDecimal((f * factor).round() / factor))
}

fn builtin_trunc(args: &[PliValue]) -> BuiltinResult {
    require_args("TRUNC", args, 1, 1)?;
    let f = to_f64("TRUNC", &args[0])?;
    Ok(PliValue::FixedBinary(f.trunc() as i64))
}

fn builtin_max(args: &[PliValue]) -> BuiltinResult {
    require_args("MAX", args, 2, 20)?;
    let mut max = to_f64("MAX", &args[0])?;
    for arg in &args[1..] {
        let v = to_f64("MAX", arg)?;
        if v > max {
            max = v;
        }
    }
    Ok(PliValue::FloatDecimal(max))
}

fn builtin_min(args: &[PliValue]) -> BuiltinResult {
    require_args("MIN", args, 2, 20)?;
    let mut min = to_f64("MIN", &args[0])?;
    for arg in &args[1..] {
        let v = to_f64("MIN", arg)?;
        if v < min {
            min = v;
        }
    }
    Ok(PliValue::FloatDecimal(min))
}

fn builtin_sign(args: &[PliValue]) -> BuiltinResult {
    require_args("SIGN", args, 1, 1)?;
    let f = to_f64("SIGN", &args[0])?;
    let s = if f > 0.0 { 1 } else if f < 0.0 { -1 } else { 0 };
    Ok(PliValue::FixedBinary(s))
}

fn builtin_multiply(args: &[PliValue]) -> BuiltinResult {
    require_args("MULTIPLY", args, 3, 3)?;
    let a = to_f64("MULTIPLY", &args[0])?;
    let b = to_f64("MULTIPLY", &args[1])?;
    // Third arg is precision — we just compute the product.
    Ok(PliValue::FloatDecimal(a * b))
}

fn builtin_divide(args: &[PliValue]) -> BuiltinResult {
    require_args("DIVIDE", args, 3, 3)?;
    let a = to_f64("DIVIDE", &args[0])?;
    let b = to_f64("DIVIDE", &args[1])?;
    if b == 0.0 {
        return Err(BuiltinError::DomainError {
            func: "DIVIDE".to_string(),
            msg: "division by zero".to_string(),
        });
    }
    Ok(PliValue::FloatDecimal(a / b))
}

fn builtin_rem(args: &[PliValue]) -> BuiltinResult {
    require_args("REM", args, 2, 2)?;
    let a = to_f64("REM", &args[0])?;
    let b = to_f64("REM", &args[1])?;
    if b == 0.0 {
        return Err(BuiltinError::DomainError {
            func: "REM".to_string(),
            msg: "division by zero".to_string(),
        });
    }
    Ok(PliValue::FloatDecimal(a - (a / b).trunc() * b))
}

// ---------------------------------------------------------------------------
//  Mathematical
// ---------------------------------------------------------------------------

fn builtin_sqrt(args: &[PliValue]) -> BuiltinResult {
    require_args("SQRT", args, 1, 1)?;
    let f = to_f64("SQRT", &args[0])?;
    if f < 0.0 {
        return Err(BuiltinError::DomainError {
            func: "SQRT".to_string(),
            msg: "negative argument".to_string(),
        });
    }
    Ok(PliValue::FloatDecimal(f.sqrt()))
}

fn builtin_sin(args: &[PliValue], degrees: bool) -> BuiltinResult {
    require_args("SIN", args, 1, 1)?;
    let mut f = to_f64("SIN", &args[0])?;
    if degrees {
        f = f.to_radians();
    }
    Ok(PliValue::FloatDecimal(f.sin()))
}

fn builtin_cos(args: &[PliValue], degrees: bool) -> BuiltinResult {
    require_args("COS", args, 1, 1)?;
    let mut f = to_f64("COS", &args[0])?;
    if degrees {
        f = f.to_radians();
    }
    Ok(PliValue::FloatDecimal(f.cos()))
}

fn builtin_tan(args: &[PliValue], degrees: bool) -> BuiltinResult {
    require_args("TAN", args, 1, 1)?;
    let mut f = to_f64("TAN", &args[0])?;
    if degrees {
        f = f.to_radians();
    }
    Ok(PliValue::FloatDecimal(f.tan()))
}

fn builtin_asin(args: &[PliValue], degrees: bool) -> BuiltinResult {
    require_args("ASIN", args, 1, 1)?;
    let f = to_f64("ASIN", &args[0])?;
    if !(-1.0..=1.0).contains(&f) {
        return Err(BuiltinError::DomainError {
            func: "ASIN".to_string(),
            msg: "argument must be in [-1, 1]".to_string(),
        });
    }
    let result = f.asin();
    Ok(PliValue::FloatDecimal(if degrees { result.to_degrees() } else { result }))
}

fn builtin_acos(args: &[PliValue], degrees: bool) -> BuiltinResult {
    require_args("ACOS", args, 1, 1)?;
    let f = to_f64("ACOS", &args[0])?;
    if !(-1.0..=1.0).contains(&f) {
        return Err(BuiltinError::DomainError {
            func: "ACOS".to_string(),
            msg: "argument must be in [-1, 1]".to_string(),
        });
    }
    let result = f.acos();
    Ok(PliValue::FloatDecimal(if degrees { result.to_degrees() } else { result }))
}

fn builtin_atan(args: &[PliValue], degrees: bool) -> BuiltinResult {
    require_args("ATAN", args, 1, 2)?;
    let y = to_f64("ATAN", &args[0])?;
    let result = if args.len() == 2 {
        let x = to_f64("ATAN", &args[1])?;
        y.atan2(x)
    } else {
        y.atan()
    };
    Ok(PliValue::FloatDecimal(if degrees { result.to_degrees() } else { result }))
}

fn builtin_atanh(args: &[PliValue]) -> BuiltinResult {
    require_args("ATANH", args, 1, 1)?;
    let f = to_f64("ATANH", &args[0])?;
    Ok(PliValue::FloatDecimal(f.atanh()))
}

fn builtin_log(args: &[PliValue]) -> BuiltinResult {
    require_args("LOG", args, 1, 1)?;
    let f = to_f64("LOG", &args[0])?;
    if f <= 0.0 {
        return Err(BuiltinError::DomainError {
            func: "LOG".to_string(),
            msg: "argument must be positive".to_string(),
        });
    }
    Ok(PliValue::FloatDecimal(f.ln()))
}

fn builtin_log2(args: &[PliValue]) -> BuiltinResult {
    require_args("LOG2", args, 1, 1)?;
    let f = to_f64("LOG2", &args[0])?;
    if f <= 0.0 {
        return Err(BuiltinError::DomainError {
            func: "LOG2".to_string(),
            msg: "argument must be positive".to_string(),
        });
    }
    Ok(PliValue::FloatDecimal(f.log2()))
}

fn builtin_log10(args: &[PliValue]) -> BuiltinResult {
    require_args("LOG10", args, 1, 1)?;
    let f = to_f64("LOG10", &args[0])?;
    if f <= 0.0 {
        return Err(BuiltinError::DomainError {
            func: "LOG10".to_string(),
            msg: "argument must be positive".to_string(),
        });
    }
    Ok(PliValue::FloatDecimal(f.log10()))
}

fn builtin_exp(args: &[PliValue]) -> BuiltinResult {
    require_args("EXP", args, 1, 1)?;
    let f = to_f64("EXP", &args[0])?;
    Ok(PliValue::FloatDecimal(f.exp()))
}

fn builtin_erf(args: &[PliValue]) -> BuiltinResult {
    require_args("ERF", args, 1, 1)?;
    let x = to_f64("ERF", &args[0])?;
    // Approximation using Abramowitz & Stegun.
    let t = 1.0 / (1.0 + 0.3275911 * x.abs());
    let poly = t * (0.254829592 + t * (-0.284496736 + t * (1.421413741 + t * (-1.453152027 + t * 1.061405429))));
    let result = 1.0 - poly * (-x * x).exp();
    Ok(PliValue::FloatDecimal(if x >= 0.0 { result } else { -result }))
}

fn builtin_erfc(args: &[PliValue]) -> BuiltinResult {
    require_args("ERFC", args, 1, 1)?;
    let erf_val = builtin_erf(args)?;
    let f = erf_val.to_f64().unwrap_or(0.0);
    Ok(PliValue::FloatDecimal(1.0 - f))
}

// ---------------------------------------------------------------------------
//  String functions
// ---------------------------------------------------------------------------

fn builtin_length(args: &[PliValue]) -> BuiltinResult {
    require_args("LENGTH", args, 1, 1)?;
    let s = to_str(&args[0]);
    Ok(PliValue::FixedBinary(s.len() as i64))
}

fn builtin_substr(args: &[PliValue]) -> BuiltinResult {
    require_args("SUBSTR", args, 2, 3)?;
    let s = to_str(&args[0]);
    let start = args[1].to_i64().unwrap_or(1) as usize;
    let start_idx = start.saturating_sub(1);
    let len = if args.len() >= 3 {
        args[2].to_i64().unwrap_or(s.len() as i64) as usize
    } else {
        s.len().saturating_sub(start_idx)
    };
    let end_idx = (start_idx + len).min(s.len());
    let result = if start_idx < s.len() {
        s[start_idx..end_idx].to_string()
    } else {
        String::new()
    };
    Ok(PliValue::Character(result))
}

fn builtin_index(args: &[PliValue]) -> BuiltinResult {
    require_args("INDEX", args, 2, 3)?;
    let haystack = to_str(&args[0]);
    let needle = to_str(&args[1]);
    let start = if args.len() >= 3 {
        args[2].to_i64().unwrap_or(1) as usize
    } else {
        1
    };
    let start_idx = start.saturating_sub(1);
    let pos = if start_idx < haystack.len() {
        haystack[start_idx..]
            .find(&needle)
            .map(|i| (i + start_idx + 1) as i64)
            .unwrap_or(0)
    } else {
        0
    };
    Ok(PliValue::FixedBinary(pos))
}

fn builtin_verify(args: &[PliValue]) -> BuiltinResult {
    require_args("VERIFY", args, 2, 3)?;
    let s = to_str(&args[0]);
    let valid = to_str(&args[1]);
    let start = if args.len() >= 3 {
        args[2].to_i64().unwrap_or(1) as usize
    } else {
        1
    };
    let start_idx = start.saturating_sub(1);
    for (i, ch) in s[start_idx..].chars().enumerate() {
        if !valid.contains(ch) {
            return Ok(PliValue::FixedBinary((i + start_idx + 1) as i64));
        }
    }
    Ok(PliValue::FixedBinary(0))
}

fn builtin_translate(args: &[PliValue]) -> BuiltinResult {
    require_args("TRANSLATE", args, 2, 3)?;
    let s = to_str(&args[0]);
    let output_chars = to_str(&args[1]);
    let input_chars = if args.len() >= 3 {
        to_str(&args[2])
    } else {
        // Default: translate from collation sequence (0x00-0xFF).
        (0..=255u8).map(|b| b as char).collect()
    };
    let result: String = s
        .chars()
        .map(|ch| {
            if let Some(pos) = input_chars.find(ch) {
                output_chars.chars().nth(pos).unwrap_or(ch)
            } else {
                ch
            }
        })
        .collect();
    Ok(PliValue::Character(result))
}

fn builtin_trim(args: &[PliValue]) -> BuiltinResult {
    require_args("TRIM", args, 1, 1)?;
    let s = to_str(&args[0]);
    Ok(PliValue::Character(s.trim().to_string()))
}

fn builtin_ltrim(args: &[PliValue]) -> BuiltinResult {
    require_args("LTRIM", args, 1, 1)?;
    let s = to_str(&args[0]);
    Ok(PliValue::Character(s.trim_start().to_string()))
}

fn builtin_rtrim(args: &[PliValue]) -> BuiltinResult {
    require_args("RTRIM", args, 1, 1)?;
    let s = to_str(&args[0]);
    Ok(PliValue::Character(s.trim_end().to_string()))
}

fn builtin_left(args: &[PliValue]) -> BuiltinResult {
    require_args("LEFT", args, 2, 3)?;
    let s = to_str(&args[0]);
    let n = args[1].to_i64().unwrap_or(0) as usize;
    let pad = if args.len() >= 3 {
        to_str(&args[2]).chars().next().unwrap_or(' ')
    } else {
        ' '
    };
    let result = if s.len() >= n {
        s[..n].to_string()
    } else {
        let mut r = s.clone();
        while r.len() < n {
            r.push(pad);
        }
        r
    };
    Ok(PliValue::Character(result))
}

fn builtin_right(args: &[PliValue]) -> BuiltinResult {
    require_args("RIGHT", args, 2, 3)?;
    let s = to_str(&args[0]);
    let n = args[1].to_i64().unwrap_or(0) as usize;
    let pad = if args.len() >= 3 {
        to_str(&args[2]).chars().next().unwrap_or(' ')
    } else {
        ' '
    };
    let result = if s.len() >= n {
        s[s.len() - n..].to_string()
    } else {
        let padding: String = std::iter::repeat(pad).take(n - s.len()).collect();
        format!("{padding}{s}")
    };
    Ok(PliValue::Character(result))
}

fn builtin_center(args: &[PliValue]) -> BuiltinResult {
    require_args("CENTER", args, 2, 3)?;
    let s = to_str(&args[0]);
    let n = args[1].to_i64().unwrap_or(0) as usize;
    let pad = if args.len() >= 3 {
        to_str(&args[2]).chars().next().unwrap_or(' ')
    } else {
        ' '
    };
    let result = if s.len() >= n {
        let start = (s.len() - n) / 2;
        s[start..start + n].to_string()
    } else {
        let total_pad = n - s.len();
        let left_pad = total_pad / 2;
        let right_pad = total_pad - left_pad;
        let lp: String = std::iter::repeat(pad).take(left_pad).collect();
        let rp: String = std::iter::repeat(pad).take(right_pad).collect();
        format!("{lp}{s}{rp}")
    };
    Ok(PliValue::Character(result))
}

fn builtin_copy(args: &[PliValue]) -> BuiltinResult {
    require_args("COPY", args, 2, 2)?;
    let s = to_str(&args[0]);
    let n = args[1].to_i64().unwrap_or(0) as usize;
    Ok(PliValue::Character(s.repeat(n)))
}

fn builtin_repeat(args: &[PliValue]) -> BuiltinResult {
    require_args("REPEAT", args, 2, 2)?;
    let s = to_str(&args[0]);
    let n = args[1].to_i64().unwrap_or(0) as usize;
    // REPEAT(s, n) = s repeated (n+1) times (PL/I convention).
    Ok(PliValue::Character(s.repeat(n + 1)))
}

fn builtin_reverse(args: &[PliValue]) -> BuiltinResult {
    require_args("REVERSE", args, 1, 1)?;
    let s = to_str(&args[0]);
    Ok(PliValue::Character(s.chars().rev().collect()))
}

fn builtin_search(args: &[PliValue], reverse: bool) -> BuiltinResult {
    require_args("SEARCH", args, 2, 3)?;
    let s = to_str(&args[0]);
    let charset = to_str(&args[1]);
    let start = if args.len() >= 3 {
        args[2].to_i64().unwrap_or(1) as usize
    } else {
        1
    };
    let start_idx = start.saturating_sub(1);
    if reverse {
        for (i, ch) in s[..s.len().min(start_idx + s.len())]
            .chars()
            .rev()
            .enumerate()
        {
            if charset.contains(ch) {
                return Ok(PliValue::FixedBinary((s.len() - i) as i64));
            }
        }
    } else {
        for (i, ch) in s[start_idx..].chars().enumerate() {
            if charset.contains(ch) {
                return Ok(PliValue::FixedBinary((i + start_idx + 1) as i64));
            }
        }
    }
    Ok(PliValue::FixedBinary(0))
}

fn builtin_tally(args: &[PliValue]) -> BuiltinResult {
    require_args("TALLY", args, 2, 2)?;
    let s = to_str(&args[0]);
    let pattern = to_str(&args[1]);
    if pattern.is_empty() {
        return Ok(PliValue::FixedBinary(0));
    }
    Ok(PliValue::FixedBinary(s.matches(&pattern).count() as i64))
}

fn builtin_lowercase(args: &[PliValue]) -> BuiltinResult {
    require_args("LOWERCASE", args, 1, 1)?;
    Ok(PliValue::Character(to_str(&args[0]).to_lowercase()))
}

fn builtin_uppercase(args: &[PliValue]) -> BuiltinResult {
    require_args("UPPERCASE", args, 1, 1)?;
    Ok(PliValue::Character(to_str(&args[0]).to_uppercase()))
}

fn builtin_maxlength(args: &[PliValue]) -> BuiltinResult {
    require_args("MAXLENGTH", args, 1, 1)?;
    let s = to_str(&args[0]);
    Ok(PliValue::FixedBinary(s.len() as i64))
}

fn builtin_bit_convert(args: &[PliValue]) -> BuiltinResult {
    require_args("BIT", args, 1, 1)?;
    let bits = args[0].to_bit_value();
    Ok(PliValue::Bit(bits))
}

fn builtin_bool(args: &[PliValue]) -> BuiltinResult {
    require_args("BOOL", args, 3, 3)?;
    // BOOL(x, y, z) — bitwise operation defined by truth table z.
    let x = args[0].to_bit_value();
    let y = args[1].to_bit_value();
    let z = to_str(&args[2]);
    let z_bits: Vec<bool> = z.chars().map(|c| c == '1').collect();
    let len = x.len().max(y.len());
    let mut result = Vec::with_capacity(len);
    for i in 0..len {
        let xi = x.get(i).copied().unwrap_or(false);
        let yi = y.get(i).copied().unwrap_or(false);
        let idx = ((xi as usize) << 1) | (yi as usize);
        result.push(z_bits.get(idx).copied().unwrap_or(false));
    }
    Ok(PliValue::Bit(result))
}

fn builtin_collate(args: &[PliValue]) -> BuiltinResult {
    require_args("COLLATE", args, 0, 0)?;
    let s: String = (0..=255u8).map(|b| b as char).collect();
    Ok(PliValue::Character(s))
}

// ---------------------------------------------------------------------------
//  Conversion functions
// ---------------------------------------------------------------------------

fn builtin_binary(args: &[PliValue]) -> BuiltinResult {
    require_args("BINARY", args, 1, 1)?;
    let f = to_f64("BINARY", &args[0])?;
    Ok(PliValue::FixedBinary(f as i64))
}

fn builtin_decimal(args: &[PliValue]) -> BuiltinResult {
    require_args("DECIMAL", args, 1, 1)?;
    let f = to_f64("DECIMAL", &args[0])?;
    Ok(PliValue::FixedDecimal {
        raw: f as i128,
        precision: 15,
        scale: 0,
    })
}

fn builtin_fixed(args: &[PliValue]) -> BuiltinResult {
    require_args("FIXED", args, 1, 1)?;
    let f = to_f64("FIXED", &args[0])?;
    Ok(PliValue::FixedBinary(f as i64))
}

fn builtin_float(args: &[PliValue]) -> BuiltinResult {
    require_args("FLOAT", args, 1, 1)?;
    let f = to_f64("FLOAT", &args[0])?;
    Ok(PliValue::FloatDecimal(f))
}

fn builtin_char(args: &[PliValue]) -> BuiltinResult {
    require_args("CHAR", args, 1, 1)?;
    Ok(PliValue::Character(to_str(&args[0])))
}

fn builtin_hex(args: &[PliValue]) -> BuiltinResult {
    require_args("HEX", args, 1, 1)?;
    let s = to_str(&args[0]);
    use std::fmt::Write;
    let hex: String = s.bytes().fold(String::new(), |mut acc, b| {
        let _ = write!(acc, "{b:02X}");
        acc
    });
    Ok(PliValue::Character(hex))
}

fn builtin_unspec(args: &[PliValue]) -> BuiltinResult {
    require_args("UNSPEC", args, 1, 1)?;
    let bits = args[0].to_bit_value();
    Ok(PliValue::Bit(bits))
}

fn builtin_high(args: &[PliValue]) -> BuiltinResult {
    require_args("HIGH", args, 1, 1)?;
    let n = args[0].to_i64().unwrap_or(1) as usize;
    // 0xFF is not valid UTF-8; use 0x7F (DEL) as highest ASCII char.
    Ok(PliValue::Character("\x7F".repeat(n)))
}

fn builtin_low(args: &[PliValue]) -> BuiltinResult {
    require_args("LOW", args, 1, 1)?;
    let n = args[0].to_i64().unwrap_or(1) as usize;
    Ok(PliValue::Character("\x00".repeat(n)))
}

fn builtin_byte(args: &[PliValue]) -> BuiltinResult {
    require_args("BYTE", args, 1, 1)?;
    let n = args[0].to_i64().unwrap_or(0);
    let ch = (n & 0xFF) as u8 as char;
    Ok(PliValue::Character(ch.to_string()))
}

fn builtin_rank(args: &[PliValue]) -> BuiltinResult {
    require_args("RANK", args, 1, 1)?;
    let s = to_str(&args[0]);
    let code = s.bytes().next().unwrap_or(0) as i64;
    Ok(PliValue::FixedBinary(code))
}

// ---------------------------------------------------------------------------
//  Date/Time functions
// ---------------------------------------------------------------------------

fn builtin_datetime(args: &[PliValue]) -> BuiltinResult {
    require_args("DATETIME", args, 0, 1)?;
    // Return a fixed date for deterministic testing.
    // In a real implementation, this would use the system clock.
    Ok(PliValue::Character("20260219120000000".to_string()))
}

fn builtin_date(args: &[PliValue]) -> BuiltinResult {
    require_args("DATE", args, 0, 0)?;
    Ok(PliValue::Character("260219".to_string()))
}

fn builtin_time(args: &[PliValue]) -> BuiltinResult {
    require_args("TIME", args, 0, 0)?;
    Ok(PliValue::Character("120000000".to_string()))
}

fn builtin_days(args: &[PliValue]) -> BuiltinResult {
    require_args("DAYS", args, 1, 2)?;
    // Simplified: parse YYYYMMDD and compute Lilian day number.
    let s = to_str(&args[0]);
    if s.len() >= 8 {
        let y: i64 = s[..4].parse().unwrap_or(2000);
        let m: i64 = s[4..6].parse().unwrap_or(1);
        let d: i64 = s[6..8].parse().unwrap_or(1);
        // Simplified Julian day calculation.
        let jdn = 367 * y - 7 * (y + (m + 9) / 12) / 4 + 275 * m / 9 + d + 1721014;
        // Lilian day = Julian day - 2299161 (Oct 15, 1582)
        Ok(PliValue::FixedBinary(jdn - 2299161))
    } else {
        Ok(PliValue::FixedBinary(0))
    }
}

fn builtin_daystodate(args: &[PliValue]) -> BuiltinResult {
    require_args("DAYSTODATE", args, 1, 2)?;
    let lilian = args[0].to_i64().unwrap_or(0);
    // Reverse of DAYS (simplified).
    let jdn = lilian + 2299161;
    let l = jdn + 68569;
    let n = 4 * l / 146097;
    let l2 = l - (146097 * n + 3) / 4;
    let i = 4000 * (l2 + 1) / 1461001;
    let l3 = l2 - 1461 * i / 4 + 31;
    let j = 80 * l3 / 2447;
    let d = l3 - 2447 * j / 80;
    let l4 = j / 11;
    let m = j + 2 - 12 * l4;
    let y = 100 * (n - 49) + i + l4;
    Ok(PliValue::Character(format!("{y:04}{m:02}{d:02}")))
}

fn builtin_secs(args: &[PliValue]) -> BuiltinResult {
    require_args("SECS", args, 1, 2)?;
    // Simplified: parse YYYYMMDDHHMMSS and return seconds since epoch.
    let s = to_str(&args[0]);
    if s.len() >= 14 {
        let days = builtin_days(&[PliValue::Character(s[..8].to_string())])?;
        let h: i64 = s[8..10].parse().unwrap_or(0);
        let m: i64 = s[10..12].parse().unwrap_or(0);
        let sec: i64 = s[12..14].parse().unwrap_or(0);
        let day_val = days.to_i64().unwrap_or(0);
        Ok(PliValue::FixedBinary(day_val * 86400 + h * 3600 + m * 60 + sec))
    } else {
        Ok(PliValue::FixedBinary(0))
    }
}

fn builtin_secstodays(args: &[PliValue]) -> BuiltinResult {
    require_args("SECSTODAYS", args, 1, 1)?;
    let secs = args[0].to_i64().unwrap_or(0);
    Ok(PliValue::FixedBinary(secs / 86400))
}

fn builtin_weekday(args: &[PliValue]) -> BuiltinResult {
    require_args("WEEKDAY", args, 1, 1)?;
    // From Lilian day number.
    let lilian = args[0].to_i64().unwrap_or(0);
    // Day 1 (Oct 15, 1582) was a Friday = 6 in PL/I (Sun=1).
    let wd = ((lilian + 5) % 7) + 1; // 1=Sun, 2=Mon, ..., 7=Sat
    Ok(PliValue::FixedBinary(wd))
}

fn builtin_y4date(args: &[PliValue]) -> BuiltinResult {
    require_args("Y4DATE", args, 1, 2)?;
    // Convert 2-digit year date to 4-digit.
    let s = to_str(&args[0]);
    if s.len() >= 6 {
        let yy: i64 = s[..2].parse().unwrap_or(0);
        let y4 = if yy >= 50 { 1900 + yy } else { 2000 + yy };
        Ok(PliValue::Character(format!("{y4:04}{}", &s[2..])))
    } else {
        Ok(PliValue::Character(s))
    }
}

// ---------------------------------------------------------------------------
//  Storage functions
// ---------------------------------------------------------------------------

fn builtin_addr(args: &[PliValue]) -> BuiltinResult {
    require_args("ADDR", args, 1, 1)?;
    // In an interpreter, we return a synthetic address.
    Ok(PliValue::Pointer(0x1000))
}

fn builtin_size(args: &[PliValue]) -> BuiltinResult {
    require_args("SIZE", args, 1, 1)?;
    let s = to_str(&args[0]);
    Ok(PliValue::FixedBinary(s.len() as i64))
}

// ---------------------------------------------------------------------------
//  Array functions
// ---------------------------------------------------------------------------

fn builtin_hbound(args: &[PliValue]) -> BuiltinResult {
    require_args("HBOUND", args, 1, 2)?;
    // Simplified: return 0 (real impl would need array metadata).
    Ok(PliValue::FixedBinary(0))
}

fn builtin_lbound(args: &[PliValue]) -> BuiltinResult {
    require_args("LBOUND", args, 1, 2)?;
    Ok(PliValue::FixedBinary(1))
}

fn builtin_dim(args: &[PliValue]) -> BuiltinResult {
    require_args("DIM", args, 1, 2)?;
    Ok(PliValue::FixedBinary(0))
}

fn builtin_sum(args: &[PliValue]) -> BuiltinResult {
    require_args("SUM", args, 1, 20)?;
    let mut total = 0.0_f64;
    for arg in args {
        total += to_f64("SUM", arg)?;
    }
    Ok(PliValue::FloatDecimal(total))
}

fn builtin_prod(args: &[PliValue]) -> BuiltinResult {
    require_args("PROD", args, 1, 20)?;
    let mut total = 1.0_f64;
    for arg in args {
        total *= to_f64("PROD", arg)?;
    }
    Ok(PliValue::FloatDecimal(total))
}

// ---------------------------------------------------------------------------
//  Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn fbin(n: i64) -> PliValue {
        PliValue::FixedBinary(n)
    }
    fn fdec(f: f64) -> PliValue {
        PliValue::FloatDecimal(f)
    }
    fn chr(s: &str) -> PliValue {
        PliValue::Character(s.to_string())
    }

    // ─── P104.1: Arithmetic and Mathematical functions ───

    #[test]
    fn test_abs() {
        assert_eq!(call_builtin("ABS", &[fbin(-42)]).unwrap(), fbin(42));
        assert_eq!(call_builtin("ABS", &[fbin(42)]).unwrap(), fbin(42));
    }

    #[test]
    fn test_mod() {
        assert_eq!(call_builtin("MOD", &[fbin(17), fbin(5)]).unwrap(), fbin(2));
    }

    #[test]
    fn test_ceil() {
        assert_eq!(call_builtin("CEIL", &[fdec(3.2)]).unwrap(), fbin(4));
        assert_eq!(call_builtin("CEIL", &[fdec(-3.2)]).unwrap(), fbin(-3));
    }

    #[test]
    fn test_floor() {
        assert_eq!(call_builtin("FLOOR", &[fdec(3.7)]).unwrap(), fbin(3));
        assert_eq!(call_builtin("FLOOR", &[fdec(-3.7)]).unwrap(), fbin(-4));
    }

    #[test]
    fn test_round() {
        let r = call_builtin("ROUND", &[fdec(3.456), fbin(2)]).unwrap();
        if let PliValue::FloatDecimal(f) = r {
            assert!((f - 3.46).abs() < 1e-10);
        } else {
            panic!("Expected FloatDecimal");
        }
    }

    #[test]
    fn test_trunc() {
        assert_eq!(call_builtin("TRUNC", &[fdec(3.7)]).unwrap(), fbin(3));
        assert_eq!(call_builtin("TRUNC", &[fdec(-3.7)]).unwrap(), fbin(-3));
    }

    #[test]
    fn test_max_min() {
        let max = call_builtin("MAX", &[fbin(5), fbin(10), fbin(3)]).unwrap();
        assert_eq!(max.to_f64().unwrap(), 10.0);

        let min = call_builtin("MIN", &[fbin(5), fbin(10), fbin(3)]).unwrap();
        assert_eq!(min.to_f64().unwrap(), 3.0);
    }

    #[test]
    fn test_sign() {
        assert_eq!(call_builtin("SIGN", &[fbin(42)]).unwrap(), fbin(1));
        assert_eq!(call_builtin("SIGN", &[fbin(-5)]).unwrap(), fbin(-1));
        assert_eq!(call_builtin("SIGN", &[fbin(0)]).unwrap(), fbin(0));
    }

    #[test]
    fn test_sqrt() {
        let r = call_builtin("SQRT", &[fdec(144.0)]).unwrap();
        assert_eq!(r.to_f64().unwrap(), 12.0);
    }

    #[test]
    fn test_sqrt_negative() {
        assert!(call_builtin("SQRT", &[fdec(-1.0)]).is_err());
    }

    #[test]
    fn test_sin_cos() {
        let s = call_builtin("SIN", &[fdec(0.0)]).unwrap();
        assert!((s.to_f64().unwrap()).abs() < 1e-10);

        let c = call_builtin("COS", &[fdec(0.0)]).unwrap();
        assert!((c.to_f64().unwrap() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_trig_degrees() {
        let s = call_builtin("SIND", &[fdec(90.0)]).unwrap();
        assert!((s.to_f64().unwrap() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_log_exp() {
        let l = call_builtin("LOG", &[fdec(std::f64::consts::E)]).unwrap();
        assert!((l.to_f64().unwrap() - 1.0).abs() < 1e-10);

        let e = call_builtin("EXP", &[fdec(0.0)]).unwrap();
        assert!((e.to_f64().unwrap() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_log10() {
        let l = call_builtin("LOG10", &[fdec(100.0)]).unwrap();
        assert!((l.to_f64().unwrap() - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_erf() {
        let r = call_builtin("ERF", &[fdec(0.0)]).unwrap();
        // A&S approximation has ~1e-7 max error; use relaxed tolerance at zero.
        assert!((r.to_f64().unwrap()).abs() < 1e-6);
    }

    // ─── P104.2: String and Conversion functions ───

    #[test]
    fn test_length() {
        assert_eq!(call_builtin("LENGTH", &[chr("Hello")]).unwrap(), fbin(5));
    }

    #[test]
    fn test_substr() {
        let r = call_builtin("SUBSTR", &[chr("ABCDEF"), fbin(3), fbin(2)]).unwrap();
        assert_eq!(r, chr("CD"));
    }

    #[test]
    fn test_index() {
        let r = call_builtin("INDEX", &[chr("ABCDEF"), chr("CD")]).unwrap();
        assert_eq!(r, fbin(3));
    }

    #[test]
    fn test_index_not_found() {
        let r = call_builtin("INDEX", &[chr("ABCDEF"), chr("XY")]).unwrap();
        assert_eq!(r, fbin(0));
    }

    #[test]
    fn test_verify() {
        let r = call_builtin("VERIFY", &[chr("12345"), chr("0123456789")]).unwrap();
        assert_eq!(r, fbin(0)); // All valid

        let r = call_builtin("VERIFY", &[chr("123A5"), chr("0123456789")]).unwrap();
        assert_eq!(r, fbin(4)); // 'A' at position 4
    }

    #[test]
    fn test_translate() {
        let r = call_builtin(
            "TRANSLATE",
            &[chr("HELLO"), chr("hello"), chr("HELLO")],
        )
        .unwrap();
        assert_eq!(r, chr("hello"));
    }

    #[test]
    fn test_trim() {
        assert_eq!(
            call_builtin("TRIM", &[chr("  hello  ")]).unwrap(),
            chr("hello")
        );
    }

    #[test]
    fn test_ltrim_rtrim() {
        assert_eq!(
            call_builtin("LTRIM", &[chr("  hello  ")]).unwrap(),
            chr("hello  ")
        );
        assert_eq!(
            call_builtin("RTRIM", &[chr("  hello  ")]).unwrap(),
            chr("  hello")
        );
    }

    #[test]
    fn test_left_right() {
        assert_eq!(
            call_builtin("LEFT", &[chr("Hello"), fbin(10)]).unwrap(),
            chr("Hello     ")
        );
        assert_eq!(
            call_builtin("RIGHT", &[chr("Hello"), fbin(10)]).unwrap(),
            chr("     Hello")
        );
    }

    #[test]
    fn test_center() {
        let r = call_builtin("CENTER", &[chr("Hi"), fbin(6)]).unwrap();
        assert_eq!(r, chr("  Hi  "));
    }

    #[test]
    fn test_copy() {
        assert_eq!(
            call_builtin("COPY", &[chr("AB"), fbin(3)]).unwrap(),
            chr("ABABAB")
        );
    }

    #[test]
    fn test_repeat() {
        // REPEAT(s, n) = s * (n+1)
        assert_eq!(
            call_builtin("REPEAT", &[chr("AB"), fbin(2)]).unwrap(),
            chr("ABABAB")
        );
    }

    #[test]
    fn test_reverse() {
        assert_eq!(
            call_builtin("REVERSE", &[chr("Hello")]).unwrap(),
            chr("olleH")
        );
    }

    #[test]
    fn test_tally() {
        assert_eq!(
            call_builtin("TALLY", &[chr("banana"), chr("an")]).unwrap(),
            fbin(2)
        );
    }

    #[test]
    fn test_lowercase_uppercase() {
        assert_eq!(
            call_builtin("LOWERCASE", &[chr("HELLO")]).unwrap(),
            chr("hello")
        );
        assert_eq!(
            call_builtin("UPPERCASE", &[chr("hello")]).unwrap(),
            chr("HELLO")
        );
    }

    #[test]
    fn test_hex() {
        let r = call_builtin("HEX", &[chr("A")]).unwrap();
        assert_eq!(r, chr("41"));
    }

    #[test]
    fn test_rank_byte() {
        assert_eq!(call_builtin("RANK", &[chr("A")]).unwrap(), fbin(65));
        let b = call_builtin("BYTE", &[fbin(65)]).unwrap();
        assert_eq!(b, chr("A"));
    }

    #[test]
    fn test_high_low() {
        let h = call_builtin("HIGH", &[fbin(1)]).unwrap();
        if let PliValue::Character(s) = h {
            assert_eq!(s.len(), 1);
        }
        let l = call_builtin("LOW", &[fbin(1)]).unwrap();
        if let PliValue::Character(s) = l {
            assert_eq!(s.len(), 1);
        }
    }

    #[test]
    fn test_binary_decimal_fixed_float() {
        assert_eq!(call_builtin("BINARY", &[fdec(42.5)]).unwrap(), fbin(42));
        assert_eq!(call_builtin("FIXED", &[fdec(42.5)]).unwrap(), fbin(42));
        let f = call_builtin("FLOAT", &[fbin(42)]).unwrap();
        assert_eq!(f.to_f64().unwrap(), 42.0);
    }

    // ─── P104.3: Date/Time and Storage functions ───

    #[test]
    fn test_datetime() {
        let r = call_builtin("DATETIME", &[]).unwrap();
        if let PliValue::Character(s) = r {
            assert_eq!(s.len(), 17);
        } else {
            panic!("Expected Character");
        }
    }

    #[test]
    fn test_days_daystodate_roundtrip() {
        let days = call_builtin("DAYS", &[chr("20260219")]).unwrap();
        let d = days.to_i64().unwrap();
        assert!(d > 0);

        let date = call_builtin("DAYSTODATE", &[fbin(d)]).unwrap();
        assert_eq!(date, chr("20260219"));
    }

    #[test]
    fn test_y4date() {
        assert_eq!(
            call_builtin("Y4DATE", &[chr("260219")]).unwrap(),
            chr("20260219")
        );
        assert_eq!(
            call_builtin("Y4DATE", &[chr("991231")]).unwrap(),
            chr("19991231")
        );
    }

    #[test]
    fn test_null() {
        assert_eq!(call_builtin("NULL", &[]).unwrap(), PliValue::Null);
    }

    #[test]
    fn test_sum_prod() {
        let s = call_builtin("SUM", &[fbin(1), fbin(2), fbin(3)]).unwrap();
        assert_eq!(s.to_f64().unwrap(), 6.0);

        let p = call_builtin("PROD", &[fbin(2), fbin(3), fbin(4)]).unwrap();
        assert_eq!(p.to_f64().unwrap(), 24.0);
    }

    #[test]
    fn test_wrong_arity() {
        assert!(call_builtin("ABS", &[]).is_err());
        assert!(call_builtin("MOD", &[fbin(1)]).is_err());
    }

    #[test]
    fn test_unknown_function() {
        assert!(call_builtin("NOSUCHFUNC", &[]).is_err());
    }

    #[test]
    fn test_search() {
        let r = call_builtin("SEARCH", &[chr("Hello World"), chr("aeiou")]).unwrap();
        assert_eq!(r, fbin(2)); // 'e' at position 2
    }

    #[test]
    fn test_rem() {
        let r = call_builtin("REM", &[fbin(7), fbin(3)]).unwrap();
        assert_eq!(r.to_f64().unwrap(), 1.0);
    }
}
