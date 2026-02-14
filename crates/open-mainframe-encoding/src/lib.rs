//! EBCDIC encoding and decimal arithmetic for OpenMainframe.
//!
//! This crate provides data encoding and conversion facilities for
//! processing mainframe data on Linux systems.
//!
//! # Features
//!
//! - **EBCDIC Conversion**: Support for IBM code pages CP037, CP1047, CP500
//! - **Packed Decimal**: COMP-3 BCD encoding with 18-digit precision
//! - **Zoned Decimal**: DISPLAY format numeric encoding
//! - **Binary Integer**: COMP/COMP-4 big-endian integer encoding
//!
//! # Example
//!
//! ```rust
//! use open_mainframe_encoding::ebcdic::{CP037, CodePage};
//! use open_mainframe_encoding::decimal::{pack_decimal, unpack_decimal};
//! use rust_decimal::Decimal;
//! use std::str::FromStr;
//!
//! // EBCDIC conversion
//! let text = "HELLO";
//! let ebcdic = CP037.encode(text).unwrap();
//! let ascii = CP037.decode(&ebcdic).unwrap();
//! assert_eq!(ascii, text);
//!
//! // Packed decimal
//! let value = Decimal::from_str("12345").unwrap();
//! let packed = pack_decimal(&value, 5, 0, true).unwrap();
//! let (unpacked, _sign) = unpack_decimal(&packed, 0).unwrap();
//! assert_eq!(value, unpacked);
//! ```

pub mod decimal;
pub mod ebcdic;
pub mod error;

// Re-export commonly used types at crate root
pub use decimal::{
    decode_binary, encode_binary, pack_decimal, unpack_decimal, unzone_decimal, zone_decimal,
    BinaryInteger, PackedDecimal, Sign, ZonedDecimal,
};
pub use ebcdic::{CodePage, CP037, CP1047, CP500};
pub use error::EncodingError;

/// Result type for encoding operations.
pub type Result<T> = std::result::Result<T, EncodingError>;
