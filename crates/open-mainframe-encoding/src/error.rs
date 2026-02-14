//! Error types for the open-mainframe-encoding crate.

use miette::Diagnostic;
use thiserror::Error;

/// Errors that can occur during encoding/decoding operations.
#[derive(Debug, Error, Diagnostic)]
pub enum EncodingError {
    /// Invalid or unsupported code page specified.
    #[error("Invalid code page: {0}")]
    #[diagnostic(
        code(encoding::invalid_code_page),
        help("Supported code pages are: CP037, CP1047, CP500")
    )]
    InvalidCodePage(String),

    /// Conversion between formats failed.
    #[error("Conversion failed: {message}")]
    #[diagnostic(code(encoding::conversion_failed))]
    ConversionFailed {
        /// Description of what went wrong.
        message: String,
    },

    /// Value is out of range for the target format.
    #[error("Value out of range: {value} (expected {min}..{max})")]
    #[diagnostic(
        code(encoding::out_of_range),
        help("Reduce the value or increase the target field size")
    )]
    OutOfRange {
        /// The value that was out of range.
        value: String,
        /// Minimum allowed value.
        min: String,
        /// Maximum allowed value.
        max: String,
    },

    /// Invalid digit found in numeric data.
    #[error("Invalid digit: 0x{byte:02X} at position {position}")]
    #[diagnostic(
        code(encoding::invalid_digit),
        help("Digits must be 0-9 (0x00-0x09 in nibble form)")
    )]
    InvalidDigit {
        /// The invalid byte value.
        byte: u8,
        /// Position in the input where the error occurred.
        position: usize,
    },

    /// Invalid sign nibble in packed or zoned decimal.
    #[error("Invalid sign nibble: 0x{nibble:X}")]
    #[diagnostic(
        code(encoding::invalid_sign),
        help("Valid sign nibbles: 0xC (positive), 0xD (negative), 0xF (unsigned)")
    )]
    InvalidSign {
        /// The invalid nibble value.
        nibble: u8,
    },

    /// Precision loss would occur during conversion.
    #[error("Precision loss: {message}")]
    #[diagnostic(
        code(encoding::precision_loss),
        help("Use a larger target field or accept truncation")
    )]
    PrecisionLoss {
        /// Description of the precision issue.
        message: String,
    },
}
