//! Bound Resolution and Validation
//!
//! This module resolves symbolic bound expressions to concrete values and
//! validates that bounded types are well-formed.
//!
//! ## Key Features
//!
//! - **Symbolic Resolution**: Evaluates type references like `MyEnum.len()`
//! - **Cycle Detection**: Detects and reports circular dependencies between types
//! - **Bounds Validation**: Ensures min <= max for all bounded types
//! - **Unsigned Validation**: Ensures boundedUint types have non-negative bounds
//!
//! ## Error Handling
//!
//! The resolver collects all errors rather than failing on the first one,
//! allowing users to fix multiple issues at once.
//!
//! ## Example
//!
//! ```
//! use marigold_grammar::bound_resolution::BoundResolver;
//! use marigold_grammar::symbol_table::SymbolTable;
//! use marigold_grammar::nodes::{TypedExpression, StructDeclarationNode, Type, BoundExpr};
//!
//! // Create a struct with a bounded field
//! let expressions = vec![
//!     TypedExpression::StructDeclaration(StructDeclarationNode {
//!         name: "Test".to_string(),
//!         fields: vec![("value".to_string(), Type::BoundedInt {
//!             min: BoundExpr::Literal(0),
//!             max: BoundExpr::Literal(100),
//!         })],
//!     }),
//! ];
//!
//! let table = SymbolTable::from_expressions(&expressions);
//! let mut resolver = BoundResolver::new(&table);
//!
//! let resolved = resolver.resolve_all().unwrap();
//! let bound = resolved.get("Test", "value").unwrap();
//! assert_eq!(bound.min, 0);
//! assert_eq!(bound.max, 100);
//! ```

use crate::nodes::{ArithOp, BoundExpr, BoundOp};
use crate::symbol_table::{BoundedFieldInfo, SymbolTable};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// Errors that can occur during bound resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolutionError {
    CyclicDependency {
        cycle: Vec<String>,
    },
    UndefinedType(String),
    InvalidOperation {
        type_name: String,
        operation: String,
    },
    DivisionByZero,
    BoundsViolation {
        field: String,
        message: String,
    },
}

impl fmt::Display for ResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolutionError::CyclicDependency { cycle } => {
                write!(f, "Circular dependency detected: {}", cycle.join(" -> "))
            }
            ResolutionError::UndefinedType(name) => {
                write!(f, "Undefined type: '{}'", name)
            }
            ResolutionError::InvalidOperation {
                type_name,
                operation,
            } => {
                write!(
                    f,
                    "Invalid operation '{}' on type '{}'. Valid operations are: len, min, max, cardinality",
                    operation, type_name
                )
            }
            ResolutionError::DivisionByZero => {
                write!(f, "Division by zero in bound expression")
            }
            ResolutionError::BoundsViolation { field, message } => {
                write!(f, "Bounds violation for field '{}': {}", field, message)
            }
        }
    }
}

impl std::error::Error for ResolutionError {}

#[derive(Debug, Clone)]
pub struct ResolvedBound {
    pub struct_name: String,
    pub field_name: String,
    pub min: i128,
    pub max: i128,
    pub is_signed: bool,
}

impl ResolvedBound {
    pub fn cardinality(&self) -> u128 {
        if self.max >= self.min {
            (self.max - self.min + 1) as u128
        } else {
            0
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ResolvedBounds {
    bounds: HashMap<String, ResolvedBound>,
}

impl ResolvedBounds {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, key: String, bound: ResolvedBound) {
        self.bounds.insert(key, bound);
    }

    pub fn get(&self, struct_name: &str, field_name: &str) -> Option<&ResolvedBound> {
        let key = format!("{}.{}", struct_name, field_name);
        self.bounds.get(&key)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &ResolvedBound)> {
        self.bounds.iter()
    }

    pub fn bounds(&self) -> impl Iterator<Item = &ResolvedBound> {
        self.bounds.values()
    }
}

pub struct BoundResolver<'a> {
    symbol_table: &'a SymbolTable,
    resolved_cache: HashMap<String, i128>,
}

impl<'a> BoundResolver<'a> {
    pub fn new(symbol_table: &'a SymbolTable) -> Self {
        Self {
            symbol_table,
            resolved_cache: HashMap::new(),
        }
    }

    pub fn resolve_all(&mut self) -> Result<ResolvedBounds, Vec<ResolutionError>> {
        let mut resolved = ResolvedBounds::new();
        let mut errors = Vec::new();

        for field_info in self.symbol_table.get_bounded_fields() {
            match self.resolve_field(field_info) {
                Ok(bound) => {
                    let key = format!("{}.{}", bound.struct_name, bound.field_name);
                    resolved.insert(key, bound);
                }
                Err(e) => {
                    errors.push(e);
                }
            }
        }

        if errors.is_empty() {
            Ok(resolved)
        } else {
            Err(errors)
        }
    }

    fn resolve_field(
        &mut self,
        field_info: &BoundedFieldInfo,
    ) -> Result<ResolvedBound, ResolutionError> {
        let context = format!("{}.{}", field_info.struct_name, field_info.field_name);
        let mut visited = HashSet::new();

        let min = self.resolve_expr(&field_info.min, &mut visited, &context)?;
        visited.clear();
        let max = self.resolve_expr(&field_info.max, &mut visited, &context)?;

        if min > max {
            return Err(ResolutionError::BoundsViolation {
                field: context,
                message: format!("min ({}) is greater than max ({})", min, max),
            });
        }

        if !field_info.is_signed && min < 0 {
            return Err(ResolutionError::BoundsViolation {
                field: context,
                message: format!("uint[] cannot have negative min value ({})", min),
            });
        }

        Ok(ResolvedBound {
            struct_name: field_info.struct_name.clone(),
            field_name: field_info.field_name.clone(),
            min,
            max,
            is_signed: field_info.is_signed,
        })
    }

    pub fn resolve_expr(
        &mut self,
        expr: &BoundExpr,
        visited: &mut HashSet<String>,
        context: &str,
    ) -> Result<i128, ResolutionError> {
        match expr {
            BoundExpr::Literal(n) => Ok(*n),

            BoundExpr::TypeReference(type_name, op) => {
                let type_name_str = type_name.as_str();
                let cache_key = format!("{}.{:?}", type_name_str, op);

                if let Some(&cached) = self.resolved_cache.get(&cache_key) {
                    return Ok(cached);
                }

                if visited.contains(type_name_str) {
                    let mut cycle: Vec<String> = visited.iter().cloned().collect();
                    cycle.push(type_name_str.to_string());
                    return Err(ResolutionError::CyclicDependency { cycle });
                }

                visited.insert(type_name_str.to_string());

                let result = self.resolve_type_operation(type_name_str, *op, visited, context)?;

                self.resolved_cache.insert(cache_key, result);

                Ok(result)
            }

            BoundExpr::BinaryOp { left, op, right } => {
                let left_val = self.resolve_expr(left, visited, context)?;
                let right_val = self.resolve_expr(right, visited, context)?;

                match op {
                    ArithOp::Add => Ok(left_val.saturating_add(right_val)),
                    ArithOp::Sub => Ok(left_val.saturating_sub(right_val)),
                    ArithOp::Mul => Ok(left_val.saturating_mul(right_val)),
                    ArithOp::Div => {
                        if right_val == 0 {
                            Err(ResolutionError::DivisionByZero)
                        } else {
                            Ok(left_val / right_val)
                        }
                    }
                }
            }
        }
    }

    fn resolve_type_operation(
        &mut self,
        type_name: &str,
        op: BoundOp,
        visited: &mut HashSet<String>,
        context: &str,
    ) -> Result<i128, ResolutionError> {
        if let Some(len) = self.symbol_table.get_enum_len(type_name) {
            return match op {
                BoundOp::Len => Ok(len as i128),
                BoundOp::Cardinality => Ok(len as i128),
                BoundOp::Min => Ok(0),
                BoundOp::Max => Ok((len - 1) as i128),
            };
        }

        for field_info in self.symbol_table.get_bounded_fields() {
            let field_key = format!("{}.{}", field_info.struct_name, field_info.field_name);
            if field_key == type_name || field_info.field_name == type_name {
                return match op {
                    BoundOp::Min => self.resolve_expr(&field_info.min, visited, context),
                    BoundOp::Max => self.resolve_expr(&field_info.max, visited, context),
                    BoundOp::Len | BoundOp::Cardinality => {
                        let min = self.resolve_expr(&field_info.min, visited, context)?;
                        let max = self.resolve_expr(&field_info.max, visited, context)?;
                        Ok(max - min + 1)
                    }
                };
            }
        }

        Err(ResolutionError::UndefinedType(type_name.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::nodes::{EnumDeclarationNode, StructDeclarationNode, Type, TypedExpression};

    fn create_enum(name: &str, variant_count: usize) -> TypedExpression {
        let variants: Vec<(String, Option<String>)> = (0..variant_count)
            .map(|i| (format!("V{}", i), Some(format!("v{}", i))))
            .collect();

        TypedExpression::EnumDeclaration(EnumDeclarationNode {
            name: name.to_string(),
            variants,
            default_variant: None,
        })
    }

    fn create_struct_bounded(
        name: &str,
        field: &str,
        min: BoundExpr,
        max: BoundExpr,
        signed: bool,
    ) -> TypedExpression {
        let field_type = if signed {
            Type::BoundedInt { min, max }
        } else {
            Type::BoundedUint { min, max }
        };

        TypedExpression::StructDeclaration(StructDeclarationNode {
            name: name.to_string(),
            fields: vec![(field.to_string(), field_type)],
        })
    }

    #[test]
    fn test_resolve_literal() {
        let table = SymbolTable::new();
        let mut resolver = BoundResolver::new(&table);
        let mut visited = HashSet::new();

        let result = resolver.resolve_expr(&BoundExpr::Literal(42), &mut visited, "test");
        assert_eq!(result, Ok(42));
    }

    #[test]
    fn test_resolve_enum_len() {
        let exprs = vec![create_enum("Color", 3)];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);
        let mut visited = HashSet::new();

        let type_ref = arrayvec::ArrayString::from("Color").unwrap();
        let result = resolver.resolve_expr(
            &BoundExpr::TypeReference(type_ref, BoundOp::Len),
            &mut visited,
            "test",
        );
        assert_eq!(result, Ok(3));
    }

    #[test]
    fn test_resolve_enum_max() {
        let exprs = vec![create_enum("Color", 3)];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);
        let mut visited = HashSet::new();

        let type_ref = arrayvec::ArrayString::from("Color").unwrap();
        let result = resolver.resolve_expr(
            &BoundExpr::TypeReference(type_ref, BoundOp::Max),
            &mut visited,
            "test",
        );
        assert_eq!(result, Ok(2));
    }

    #[test]
    fn test_resolve_arithmetic() {
        let exprs = vec![create_enum("Color", 3)];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);
        let mut visited = HashSet::new();

        let type_ref = arrayvec::ArrayString::from("Color").unwrap();
        let expr = BoundExpr::BinaryOp {
            left: Box::new(BoundExpr::TypeReference(type_ref, BoundOp::Len)),
            op: ArithOp::Sub,
            right: Box::new(BoundExpr::Literal(1)),
        };

        let result = resolver.resolve_expr(&expr, &mut visited, "test");
        assert_eq!(result, Ok(2));
    }

    #[test]
    fn test_resolve_all_simple() {
        let exprs = vec![
            create_enum("Color", 3),
            create_struct_bounded(
                "Pixel",
                "value",
                BoundExpr::Literal(0),
                BoundExpr::Literal(100),
                true,
            ),
        ];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_ok());

        let bounds = result.unwrap();
        let pixel_bound = bounds.get("Pixel", "value").unwrap();
        assert_eq!(pixel_bound.min, 0);
        assert_eq!(pixel_bound.max, 100);
        assert_eq!(pixel_bound.cardinality(), 101);
    }

    #[test]
    fn test_resolve_all_with_type_reference() {
        let type_ref = arrayvec::ArrayString::from("Color").unwrap();
        let exprs = vec![
            create_enum("Color", 3),
            create_struct_bounded(
                "Pixel",
                "color_idx",
                BoundExpr::Literal(0),
                BoundExpr::BinaryOp {
                    left: Box::new(BoundExpr::TypeReference(type_ref, BoundOp::Len)),
                    op: ArithOp::Sub,
                    right: Box::new(BoundExpr::Literal(1)),
                },
                true,
            ),
        ];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_ok());

        let bounds = result.unwrap();
        let pixel_bound = bounds.get("Pixel", "color_idx").unwrap();
        assert_eq!(pixel_bound.min, 0);
        assert_eq!(pixel_bound.max, 2);
    }

    #[test]
    fn test_error_undefined_type() {
        let type_ref = arrayvec::ArrayString::from("NonExistent").unwrap();
        let exprs = vec![create_struct_bounded(
            "Test",
            "field",
            BoundExpr::Literal(0),
            BoundExpr::TypeReference(type_ref, BoundOp::Len),
            true,
        )];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            ResolutionError::UndefinedType(name) => assert_eq!(name, "NonExistent"),
            _ => panic!("Expected UndefinedType error"),
        }
    }

    #[test]
    fn test_error_min_greater_than_max() {
        let exprs = vec![create_struct_bounded(
            "Test",
            "field",
            BoundExpr::Literal(100),
            BoundExpr::Literal(10),
            true,
        )];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            ResolutionError::BoundsViolation { field, message } => {
                assert_eq!(field, "Test.field");
                assert!(message.contains("min"));
                assert!(message.contains("max"));
            }
            _ => panic!("Expected BoundsViolation error"),
        }
    }

    #[test]
    fn test_error_negative_uint() {
        let exprs = vec![create_struct_bounded(
            "Test",
            "field",
            BoundExpr::Literal(-1),
            BoundExpr::Literal(10),
            false,
        )];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            ResolutionError::BoundsViolation { field, message } => {
                assert_eq!(field, "Test.field");
                assert!(message.contains("negative"));
            }
            _ => panic!("Expected BoundsViolation error"),
        }
    }

    #[test]
    fn test_error_division_by_zero() {
        let exprs = vec![create_struct_bounded(
            "Test",
            "field",
            BoundExpr::Literal(0),
            BoundExpr::BinaryOp {
                left: Box::new(BoundExpr::Literal(10)),
                op: ArithOp::Div,
                right: Box::new(BoundExpr::Literal(0)),
            },
            true,
        )];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], ResolutionError::DivisionByZero));
    }

    #[test]
    fn test_cycle_detection_direct() {
        let a_ref = arrayvec::ArrayString::from("StructB.fieldB").unwrap();
        let b_ref = arrayvec::ArrayString::from("StructA.fieldA").unwrap();

        let struct_a = TypedExpression::StructDeclaration(StructDeclarationNode {
            name: "StructA".to_string(),
            fields: vec![(
                "fieldA".to_string(),
                Type::BoundedInt {
                    min: BoundExpr::Literal(0),
                    max: BoundExpr::TypeReference(a_ref, BoundOp::Max),
                },
            )],
        });

        let struct_b = TypedExpression::StructDeclaration(StructDeclarationNode {
            name: "StructB".to_string(),
            fields: vec![(
                "fieldB".to_string(),
                Type::BoundedInt {
                    min: BoundExpr::Literal(0),
                    max: BoundExpr::TypeReference(b_ref, BoundOp::Max),
                },
            )],
        });

        let exprs = vec![struct_a, struct_b];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, ResolutionError::CyclicDependency { .. })));
    }

    #[test]
    fn test_cycle_detection_self_reference() {
        let self_ref = arrayvec::ArrayString::from("Test.field").unwrap();

        let struct_test = TypedExpression::StructDeclaration(StructDeclarationNode {
            name: "Test".to_string(),
            fields: vec![(
                "field".to_string(),
                Type::BoundedInt {
                    min: BoundExpr::Literal(0),
                    max: BoundExpr::TypeReference(self_ref, BoundOp::Max),
                },
            )],
        });

        let exprs = vec![struct_test];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_err());

        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, ResolutionError::CyclicDependency { .. })));
    }

    #[test]
    fn test_error_message_cycle() {
        let error = ResolutionError::CyclicDependency {
            cycle: vec!["A".to_string(), "B".to_string(), "A".to_string()],
        };
        let msg = format!("{}", error);
        assert_eq!(msg, "Circular dependency detected: A -> B -> A");
    }

    #[test]
    fn test_resolved_bound_cardinality() {
        let bound = ResolvedBound {
            struct_name: "Test".to_string(),
            field_name: "field".to_string(),
            min: 0,
            max: 10,
            is_signed: true,
        };
        assert_eq!(bound.cardinality(), 11);

        let bound_negative = ResolvedBound {
            struct_name: "Test".to_string(),
            field_name: "field".to_string(),
            min: -5,
            max: 5,
            is_signed: true,
        };
        assert_eq!(bound_negative.cardinality(), 11);
    }

    #[test]
    fn test_cycle_detection_indirect() {
        let b_ref = arrayvec::ArrayString::from("StructB.fieldB").unwrap();
        let c_ref = arrayvec::ArrayString::from("StructC.fieldC").unwrap();
        let a_ref = arrayvec::ArrayString::from("StructA.fieldA").unwrap();

        let struct_a = TypedExpression::StructDeclaration(StructDeclarationNode {
            name: "StructA".to_string(),
            fields: vec![(
                "fieldA".to_string(),
                Type::BoundedInt {
                    min: BoundExpr::Literal(0),
                    max: BoundExpr::TypeReference(b_ref, BoundOp::Max),
                },
            )],
        });

        let struct_b = TypedExpression::StructDeclaration(StructDeclarationNode {
            name: "StructB".to_string(),
            fields: vec![(
                "fieldB".to_string(),
                Type::BoundedInt {
                    min: BoundExpr::Literal(0),
                    max: BoundExpr::TypeReference(c_ref, BoundOp::Max),
                },
            )],
        });

        let struct_c = TypedExpression::StructDeclaration(StructDeclarationNode {
            name: "StructC".to_string(),
            fields: vec![(
                "fieldC".to_string(),
                Type::BoundedInt {
                    min: BoundExpr::Literal(0),
                    max: BoundExpr::TypeReference(a_ref, BoundOp::Max),
                },
            )],
        });

        let exprs = vec![struct_a, struct_b, struct_c];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(
            result.is_err(),
            "Indirect cycle A -> B -> C -> A should be detected"
        );

        let errors = result.unwrap_err();
        assert!(errors
            .iter()
            .any(|e| matches!(e, ResolutionError::CyclicDependency { .. })));
    }

    #[test]
    fn test_multiple_fields_some_valid() {
        let exprs = vec![
            create_enum("Color", 3),
            TypedExpression::StructDeclaration(StructDeclarationNode {
                name: "Test".to_string(),
                fields: vec![
                    (
                        "valid_field".to_string(),
                        Type::BoundedInt {
                            min: BoundExpr::Literal(0),
                            max: BoundExpr::Literal(10),
                        },
                    ),
                    (
                        "also_valid".to_string(),
                        Type::BoundedUint {
                            min: BoundExpr::Literal(0),
                            max: BoundExpr::TypeReference(
                                arrayvec::ArrayString::from("Color").unwrap(),
                                BoundOp::Len,
                            ),
                        },
                    ),
                ],
            }),
        ];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_ok(), "Both fields should resolve successfully");

        let bounds = result.unwrap();
        let valid_bound = bounds.get("Test", "valid_field").unwrap();
        assert_eq!(valid_bound.min, 0);
        assert_eq!(valid_bound.max, 10);

        let also_valid_bound = bounds.get("Test", "also_valid").unwrap();
        assert_eq!(also_valid_bound.min, 0);
        assert_eq!(also_valid_bound.max, 3);
    }

    #[test]
    fn test_complex_arithmetic_bounds() {
        let color_ref = arrayvec::ArrayString::from("Color").unwrap();
        let exprs = vec![
            create_enum("Color", 5),
            create_struct_bounded(
                "Complex",
                "idx",
                BoundExpr::BinaryOp {
                    left: Box::new(BoundExpr::Literal(0)),
                    op: ArithOp::Sub,
                    right: Box::new(BoundExpr::Literal(2)),
                },
                BoundExpr::BinaryOp {
                    left: Box::new(BoundExpr::BinaryOp {
                        left: Box::new(BoundExpr::TypeReference(color_ref, BoundOp::Len)),
                        op: ArithOp::Mul,
                        right: Box::new(BoundExpr::Literal(2)),
                    }),
                    op: ArithOp::Sub,
                    right: Box::new(BoundExpr::Literal(1)),
                },
                true,
            ),
        ];
        let table = SymbolTable::from_expressions(&exprs);
        let mut resolver = BoundResolver::new(&table);

        let result = resolver.resolve_all();
        assert!(result.is_ok());

        let bounds = result.unwrap();
        let bound = bounds.get("Complex", "idx").unwrap();
        assert_eq!(bound.min, -2);
        assert_eq!(bound.max, 9);
    }
}
