//! LLVM type mappings for COBOL data types.
//!
//! This module provides mappings from COBOL data types to LLVM types.

use inkwell::context::Context;
use inkwell::types::{ArrayType, BasicTypeEnum, IntType, StructType};

use crate::ast::{DataItem, PictureCategory, Usage};

/// LLVM type representation for COBOL data items.
#[derive(Debug, Clone)]
pub enum LlvmType<'ctx> {
    /// Byte array for alphanumeric/display numeric.
    ByteArray(ArrayType<'ctx>),
    /// Integer type for binary data.
    Integer(IntType<'ctx>),
    /// Struct type for group items.
    Struct(StructType<'ctx>),
    /// Array type for OCCURS items.
    Array(ArrayType<'ctx>),
}

impl<'ctx> LlvmType<'ctx> {
    /// Create an LLVM type for a COBOL data item.
    pub fn from_data_item(context: &'ctx Context, item: &DataItem) -> Self {
        // Check if it's a group item
        if item.picture.is_none() && !item.children.is_empty() {
            return Self::create_group_type(context, item);
        }

        // Get the storage size and usage
        let (size, usage) = Self::get_storage_info(item);

        // Handle OCCURS
        if let Some(ref occurs) = item.occurs {
            let element_type = Self::create_elementary_type(context, size, usage);
            let array_type = Self::create_array(context, element_type, occurs.times);
            return LlvmType::Array(array_type);
        }

        Self::create_elementary_type(context, size, usage)
    }

    /// Create a type for an elementary data item.
    fn create_elementary_type(context: &'ctx Context, size: u32, usage: Usage) -> Self {
        match usage {
            Usage::Display | Usage::PackedDecimal => {
                // Stored as byte arrays
                LlvmType::ByteArray(context.i8_type().array_type(size))
            }
            Usage::Binary | Usage::Comp5 => {
                // Binary integers
                let int_type = match size {
                    1 => context.i8_type(),
                    2 => context.i16_type(),
                    4 => context.i32_type(),
                    8 => context.i64_type(),
                    _ => context.i64_type(),
                };
                LlvmType::Integer(int_type)
            }
            Usage::Comp1 => {
                // 32-bit float - store as i32 for now
                LlvmType::Integer(context.i32_type())
            }
            Usage::Comp2 => {
                // 64-bit float - store as i64 for now
                LlvmType::Integer(context.i64_type())
            }
            Usage::Pointer => LlvmType::Integer(context.i64_type()),
            Usage::Index => LlvmType::Integer(context.i32_type()),
        }
    }

    /// Create a type for a group item.
    fn create_group_type(context: &'ctx Context, item: &DataItem) -> Self {
        let field_types: Vec<BasicTypeEnum<'ctx>> = item
            .children
            .iter()
            .map(|child| Self::from_data_item(context, child).to_basic_type(context))
            .collect();

        let struct_type = context.struct_type(&field_types, false);
        LlvmType::Struct(struct_type)
    }

    /// Create an array type.
    fn create_array(
        context: &'ctx Context,
        element: LlvmType<'ctx>,
        count: u32,
    ) -> ArrayType<'ctx> {
        element.to_basic_type(context).array_type(count)
    }

    /// Get storage information for an item.
    fn get_storage_info(item: &DataItem) -> (u32, Usage) {
        let usage = item.usage.unwrap_or(Usage::Display);

        let size = if let Some(ref pic) = item.picture {
            Self::calculate_storage_size(pic.size, usage)
        } else {
            1
        };

        (size, usage)
    }

    /// Calculate storage size based on picture size and usage.
    fn calculate_storage_size(pic_size: u32, usage: Usage) -> u32 {
        match usage {
            Usage::Display => pic_size,
            Usage::PackedDecimal => (pic_size + 2) / 2,
            Usage::Binary | Usage::Comp5 => {
                if pic_size <= 4 {
                    2
                } else if pic_size <= 9 {
                    4
                } else {
                    8
                }
            }
            Usage::Comp1 => 4,
            Usage::Comp2 => 8,
            Usage::Pointer => 8,
            Usage::Index => 4,
        }
    }

    /// Convert to a BasicTypeEnum.
    pub fn to_basic_type(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            LlvmType::ByteArray(arr) => (*arr).into(),
            LlvmType::Integer(int) => (*int).into(),
            LlvmType::Struct(s) => (*s).into(),
            LlvmType::Array(arr) => (*arr).into(),
        }
    }

    /// Get the size in bytes.
    pub fn size_bytes(&self, context: &'ctx Context) -> u64 {
        match self {
            LlvmType::ByteArray(arr) => arr.len() as u64,
            LlvmType::Integer(int) => int.get_bit_width() as u64 / 8,
            LlvmType::Struct(_) => {
                // Would need target data layout to compute accurately
                // For now, approximate
                8
            }
            LlvmType::Array(arr) => arr.len() as u64 * 8, // Approximate
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_elementary_types() {
        let context = Context::create();

        // DISPLAY type - byte array
        let display_type = LlvmType::create_elementary_type(&context, 10, Usage::Display);
        if let LlvmType::ByteArray(arr) = display_type {
            assert_eq!(arr.len(), 10);
        } else {
            panic!("Expected ByteArray");
        }

        // BINARY type - integer
        let binary_type = LlvmType::create_elementary_type(&context, 4, Usage::Binary);
        if let LlvmType::Integer(int) = binary_type {
            assert_eq!(int.get_bit_width(), 32);
        } else {
            panic!("Expected Integer");
        }
    }

    #[test]
    fn test_storage_size_calculation() {
        // DISPLAY: 1 byte per character
        assert_eq!(LlvmType::calculate_storage_size(10, Usage::Display), 10);

        // BINARY: 2/4/8 bytes based on digits
        assert_eq!(LlvmType::calculate_storage_size(4, Usage::Binary), 2);
        assert_eq!(LlvmType::calculate_storage_size(9, Usage::Binary), 4);
        assert_eq!(LlvmType::calculate_storage_size(18, Usage::Binary), 8);

        // PACKED-DECIMAL: (digits + 2) / 2
        assert_eq!(LlvmType::calculate_storage_size(5, Usage::PackedDecimal), 3);
    }
}
