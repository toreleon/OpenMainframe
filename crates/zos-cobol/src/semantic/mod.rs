//! Semantic analysis for COBOL programs.
//!
//! This module provides:
//! - Symbol table construction and lookup
//! - Type checking and validation
//! - Reference resolution
//! - Diagnostic generation

mod analyzer;
mod symbol_table;
mod types;

pub use analyzer::{analyze, Diagnostic, SemanticAnalyzer, SemanticResult, Severity};
pub use symbol_table::{Scope, Symbol, SymbolKind, SymbolTable};
pub use types::{CobolType, TypeCategory};
