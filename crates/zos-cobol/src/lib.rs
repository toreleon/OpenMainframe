//! COBOL compiler for zOS-clone.
//!
//! This crate provides the COBOL compiler, including:
//! - Lexer: Tokenizes COBOL source code
//! - Parser: Builds an Abstract Syntax Tree (AST)
//! - Semantic Analyzer: Type checking and symbol resolution
//! - Code Generator: LLVM IR generation
//!
//! # Architecture
//!
//! The compiler follows a traditional pipeline:
//! 1. Source files are loaded and preprocessed (copybook resolution)
//! 2. The lexer produces a token stream
//! 3. The parser builds an AST
//! 4. Semantic analysis validates the AST
//! 5. Code generation produces LLVM IR
//! 6. LLVM compiles to native code

pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod semantic;

pub use ast::*;
pub use error::CobolError;
pub use lexer::{
    apply_replacements, parse_replacing_clause, scan, CopybookConfig, CopybookResolver, FileId,
    Indicator, Keyword, Location, Replacement, SourceFile, SourceFormat, SourceLine, SourceManager,
    Span, Token, TokenKind,
};
pub use parser::Parser;
pub use semantic::{
    analyze, CobolType, Diagnostic, SemanticAnalyzer, SemanticResult, Severity, Symbol, SymbolKind,
    SymbolTable, TypeCategory,
};
