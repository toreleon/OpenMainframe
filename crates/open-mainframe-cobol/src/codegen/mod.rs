//! LLVM code generation for COBOL programs.
//!
//! This module transforms the COBOL AST into LLVM IR, which is then
//! compiled to native machine code.
//!
//! # Architecture
//!
//! The code generator works in several phases:
//! 1. Module initialization - set up LLVM context and target
//! 2. Data layout - generate types and global storage for data items
//! 3. Procedure codegen - generate functions for paragraphs/sections
//! 4. Statement codegen - generate IR for each statement
//! 5. Finalization - verify, optimize, and emit object code

mod context;
mod data_layout;
mod types;

pub use context::{CodeGenerator, CodegenOptions, OptimizationLevel, TargetTriple};
pub use data_layout::{DataItemInfo, DataLayout};
pub use types::LlvmType;
