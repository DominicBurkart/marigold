//! Symbol Table for Type Information
//!
//! This module provides a symbol table that tracks type information needed
//! for resolving bounded type expressions.
//!
//! ## Tracked Information
//!
//! - **Enums**: Variant counts for `.len()` and `.cardinality()` operations
//! - **Bounded Fields**: Min/max expressions for struct fields with bounded types
//!
//! ## Usage
//!
//! The symbol table is built from parsed expressions and used by the
//! [`BoundResolver`](crate::bound_resolution::BoundResolver) to evaluate
//! type references.
//!
//! ```ignore
//! use marigold_grammar::symbol_table::SymbolTable;
//!
//! let table = SymbolTable::from_expressions(&expressions);
//!
//! // Check if an enum exists and get its variant count
//! if let Some(len) = table.get_enum_len("Color") {
//!     println!("Color has {} variants", len);
//! }
//! ```

use crate::nodes::{BoundExpr, EnumDeclarationNode, StructDeclarationNode, Type, TypedExpression};
use std::collections::HashMap;

/// Information about an enum type.
#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: String,
    pub variant_count: usize,
}

#[derive(Debug, Clone)]
pub struct BoundedFieldInfo {
    pub struct_name: String,
    pub field_name: String,
    pub min: BoundExpr,
    pub max: BoundExpr,
    pub is_signed: bool,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    enums: HashMap<String, EnumInfo>,
    bounded_fields: Vec<BoundedFieldInfo>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_expressions(exprs: &[TypedExpression]) -> Self {
        let mut table = Self::new();

        for expr in exprs {
            match expr {
                TypedExpression::EnumDeclaration(enum_node) => {
                    table.add_enum(enum_node);
                }
                TypedExpression::StructDeclaration(struct_node) => {
                    table.add_struct_bounded_fields(struct_node);
                }
                _ => {}
            }
        }

        table
    }

    fn add_enum(&mut self, enum_node: &EnumDeclarationNode) {
        let mut variant_count = enum_node.variants.len();
        if enum_node.default_variant.is_some() {
            variant_count += 1;
        }

        self.enums.insert(
            enum_node.name.clone(),
            EnumInfo {
                name: enum_node.name.clone(),
                variant_count,
            },
        );
    }

    fn add_struct_bounded_fields(&mut self, struct_node: &StructDeclarationNode) {
        for (field_name, field_type) in &struct_node.fields {
            match field_type {
                Type::BoundedInt { min, max } => {
                    self.bounded_fields.push(BoundedFieldInfo {
                        struct_name: struct_node.name.clone(),
                        field_name: field_name.clone(),
                        min: min.clone(),
                        max: max.clone(),
                        is_signed: true,
                    });
                }
                Type::BoundedUint { min, max } => {
                    self.bounded_fields.push(BoundedFieldInfo {
                        struct_name: struct_node.name.clone(),
                        field_name: field_name.clone(),
                        min: min.clone(),
                        max: max.clone(),
                        is_signed: false,
                    });
                }
                _ => {}
            }
        }
    }

    pub fn get_enum_len(&self, name: &str) -> Option<usize> {
        self.enums.get(name).map(|info| info.variant_count)
    }

    pub fn get_enum_info(&self, name: &str) -> Option<&EnumInfo> {
        self.enums.get(name)
    }

    pub fn get_bounded_fields(&self) -> &[BoundedFieldInfo] {
        &self.bounded_fields
    }

    pub fn has_bounded_types(&self) -> bool {
        !self.bounded_fields.is_empty()
    }

    pub fn enum_names(&self) -> impl Iterator<Item = &String> {
        self.enums.keys()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nodes::{BoundOp, DefaultEnumVariant};

    fn create_enum_node(
        name: &str,
        variant_count: usize,
        has_default: bool,
    ) -> EnumDeclarationNode {
        let variants: Vec<(String, Option<String>)> = (0..variant_count)
            .map(|i| (format!("Variant{}", i), Some(format!("v{}", i))))
            .collect();

        EnumDeclarationNode {
            name: name.to_string(),
            variants,
            default_variant: if has_default {
                Some(DefaultEnumVariant::WithDefaultValue(
                    "Other".to_string(),
                    "other".to_string(),
                ))
            } else {
                None
            },
        }
    }

    fn create_struct_with_bounded_field(
        name: &str,
        field_name: &str,
        min: BoundExpr,
        max: BoundExpr,
        is_signed: bool,
    ) -> StructDeclarationNode {
        let field_type = if is_signed {
            Type::BoundedInt { min, max }
        } else {
            Type::BoundedUint { min, max }
        };

        StructDeclarationNode {
            name: name.to_string(),
            fields: vec![(field_name.to_string(), field_type)],
        }
    }

    #[test]
    fn test_symbol_table_empty() {
        let table = SymbolTable::new();
        assert!(table.get_enum_len("NonExistent").is_none());
        assert!(!table.has_bounded_types());
    }

    #[test]
    fn test_symbol_table_enum_count() {
        let enum_node = create_enum_node("Color", 3, false);
        let exprs = vec![TypedExpression::EnumDeclaration(enum_node)];
        let table = SymbolTable::from_expressions(&exprs);

        assert_eq!(table.get_enum_len("Color"), Some(3));
    }

    #[test]
    fn test_symbol_table_enum_with_default() {
        let enum_node = create_enum_node("Status", 2, true);
        let exprs = vec![TypedExpression::EnumDeclaration(enum_node)];
        let table = SymbolTable::from_expressions(&exprs);

        assert_eq!(table.get_enum_len("Status"), Some(3));
    }

    #[test]
    fn test_symbol_table_bounded_field_literal() {
        let struct_node = create_struct_with_bounded_field(
            "Pixel",
            "value",
            BoundExpr::Literal(0),
            BoundExpr::Literal(255),
            false,
        );
        let exprs = vec![TypedExpression::StructDeclaration(struct_node)];
        let table = SymbolTable::from_expressions(&exprs);

        assert!(table.has_bounded_types());
        let fields = table.get_bounded_fields();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].struct_name, "Pixel");
        assert_eq!(fields[0].field_name, "value");
        assert!(!fields[0].is_signed);
    }

    #[test]
    fn test_symbol_table_bounded_field_type_reference() {
        let type_ref = arrayvec::ArrayString::from("Color").unwrap();
        let struct_node = create_struct_with_bounded_field(
            "Pixel",
            "color_index",
            BoundExpr::Literal(0),
            BoundExpr::TypeReference(type_ref, BoundOp::Len),
            true,
        );
        let exprs = vec![TypedExpression::StructDeclaration(struct_node)];
        let table = SymbolTable::from_expressions(&exprs);

        assert!(table.has_bounded_types());
        let fields = table.get_bounded_fields();
        assert_eq!(fields.len(), 1);
        assert!(fields[0].is_signed);
    }

    #[test]
    fn test_symbol_table_multiple_enums_and_structs() {
        let enum1 = create_enum_node("Color", 3, false);
        let enum2 = create_enum_node("Size", 4, true);
        let struct1 = create_struct_with_bounded_field(
            "Pixel",
            "value",
            BoundExpr::Literal(0),
            BoundExpr::Literal(100),
            true,
        );

        let exprs = vec![
            TypedExpression::EnumDeclaration(enum1),
            TypedExpression::EnumDeclaration(enum2),
            TypedExpression::StructDeclaration(struct1),
        ];
        let table = SymbolTable::from_expressions(&exprs);

        assert_eq!(table.get_enum_len("Color"), Some(3));
        assert_eq!(table.get_enum_len("Size"), Some(5));
        assert!(table.has_bounded_types());
    }

    #[test]
    fn test_symbol_table_enum_names_iterator() {
        let enum1 = create_enum_node("Alpha", 2, false);
        let enum2 = create_enum_node("Beta", 3, false);

        let exprs = vec![
            TypedExpression::EnumDeclaration(enum1),
            TypedExpression::EnumDeclaration(enum2),
        ];
        let table = SymbolTable::from_expressions(&exprs);

        let names: Vec<&String> = table.enum_names().collect();
        assert_eq!(names.len(), 2);
        assert!(names.iter().any(|&n| n == "Alpha"));
        assert!(names.iter().any(|&n| n == "Beta"));
    }
}
