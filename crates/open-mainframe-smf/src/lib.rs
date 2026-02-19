//! System Management Facilities (SMF).
//!
//! This crate provides:
//!
//! - **SMF Record Format** — standard header and common record structure
//! - **SMF Writer** — writes records to an SMF dataset
//! - **Job/Step Records** — Type 4 (step end), Type 5 (job end), Type 30 (common address space)

pub mod record;
pub mod writer;

pub use record::{
    SmfHeader, SmfRecord, SmfRecordType, SmfType4, SmfType5, SmfType30,
    SmfSubtype30,
};
pub use writer::{SmfTriplet, SmfWriter, SmfWriterConfig, SmfWriterError};
