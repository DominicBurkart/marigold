#![forbid(unsafe_code)]

//! # Marigold Parser Module
//!
//! This module provides the Marigold parser using the Pest parsing library.
//!
//! ## Usage Examples
//!
//! Parse Marigold code:
//! ```ignore
//! let result = parse_marigold("range(0, 1).return")?;
//! ```
//!
//! Explicitly create a parser instance:
//! ```ignore
//! let parser = PestParser::new();
//! let result = parser.parse("range(0, 1).return")?;
//! ```

use pest::Parser;
use std::fmt;

/// Error type for parser
#[derive(Debug, Clone)]
pub struct MarigoldParseError(pub String);

impl fmt::Display for MarigoldParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse error: {}", self.0)
    }
}

impl std::error::Error for MarigoldParseError {}

/// Parser abstraction trait
pub trait MarigoldParser {
    /// Parse a Marigold program string and return the generated Rust code
    fn parse(&self, input: &str) -> Result<String, MarigoldParseError>;

    /// Get the name of this parser (for debugging/logging purposes)
    fn name(&self) -> &'static str;
}

/// Pest parser backend
pub struct PestParser;

/// Pest-derived parser struct that holds the compiled grammar
#[derive(pest_derive::Parser)]
#[grammar = "marigold.pest"]
pub struct MarigoldPestParser;

impl PestParser {
    /// Create a new Pest parser instance
    pub fn new() -> Self {
        Self
    }

    pub fn analyze(
        input: &str,
    ) -> Result<crate::complexity::ProgramComplexity, MarigoldParseError> {
        let pairs = MarigoldPestParser::parse(Rule::program, input)
            .map_err(|e| MarigoldParseError(format!("{}", e)))?;
        let mut expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)
            .map_err(MarigoldParseError)?;
        Self::resolve_enum_range_counts(&mut expressions).map_err(MarigoldParseError)?;
        Ok(crate::complexity::analyze_program(&expressions))
    }

    /// Internal function: parse input and build AST
    fn parse_input(input: &str) -> Result<String, String> {
        // Stage 1: Parse with Pest grammar
        let pairs =
            MarigoldPestParser::parse(Rule::program, input).map_err(|e| format!("{}", e))?;

        // Stage 2: Build AST from parse tree
        let mut expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)?;

        // Stage 2.5: Validate enum names in range(EnumName) and resolve InputCount::Enum → Known
        Self::resolve_enum_range_counts(&mut expressions)?;

        // Stage 3: Validate bounded types (if any)
        let resolved_bounds = Self::validate_bounded_types(&expressions)?;

        // Stage 4: Generate Rust code (replicating LALRPOP logic)
        Self::generate_rust_code(expressions, resolved_bounds)
    }

    /// Resolve `InputCount::Enum(name)` placeholders produced by `range(EnumName)`.
    ///
    /// For each stream whose input count is `Enum(name)`:
    /// - If `name` is a declared enum in this program, replace with `Known(unit_variant_count)`.
    /// - Otherwise return a user-friendly error instead of silently emitting invalid Rust.
    fn resolve_enum_range_counts(
        expressions: &mut [crate::nodes::TypedExpression],
    ) -> Result<(), String> {
        use crate::nodes::{InputCount, TypedExpression};
        use num_bigint::BigUint;

        // Build name → unit_variant_count from enum declarations in this program.
        let enum_counts: std::collections::HashMap<String, BigUint> = expressions
            .iter()
            .filter_map(|expr| {
                if let TypedExpression::EnumDeclaration(e) = expr {
                    Some((
                        e.name.clone(),
                        BigUint::from(e.unit_variant_count()),
                    ))
                } else {
                    None
                }
            })
            .collect();

        for expr in expressions.iter_mut() {
            let inp = match expr {
                TypedExpression::UnnamedReturningStream(s)
                | TypedExpression::UnnamedNonReturningStream(s) => {
                    Some(&mut s.inp_and_funs.inp)
                }
                TypedExpression::StreamVariable(sv) => Some(&mut sv.inp),
                TypedExpression::NamedReturningStream(_)
                | TypedExpression::NamedNonReturningStream(_)
                | TypedExpression::StreamVariableFromPriorStreamVariable(_)
                | TypedExpression::StructDeclaration(_)
                | TypedExpression::EnumDeclaration(_)
                | TypedExpression::FnDeclaration(_) => None,
            };

            if let Some(inp_node) = inp {
                if let InputCount::Enum(ref enum_name) = inp_node.input_count.clone() {
                    match enum_counts.get(enum_name) {
                        Some(count) => {
                            inp_node.input_count = InputCount::Known(count.clone());
                        }
                        None => {
                            return Err(format!(
                                "range({enum_name}): enum '{enum_name}' is not declared in this program"
                            ));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Validate bounded types in struct fields.
    ///
    /// Validates:
    /// - Type references in bounds point to declared enums
    /// - Min bound ≤ Max bound
    ///
    /// Returns a map from (struct_name, field_name) → ResolvedFieldBounds if successful.
    fn validate_bounded_types(
        expressions: &[crate::nodes::TypedExpression],
    ) -> Result<
        std::collections::HashMap<
            (String, String),
            crate::nodes::ResolvedFieldBounds,
        >,
        String,
    > {
        use crate::nodes::{BoundExpr, BoundOp, ResolvedFieldBounds, Type, TypedExpression};

        // First pass: collect enum cardinalities
        let mut enum_info: std::collections::HashMap<String, EnumInfo> =
            std::collections::HashMap::new();

        for expr in expressions {
            if let TypedExpression::StructDeclaration(s) = expr {
                // Also collect struct info for .min()/.max()/.cardinality() references
                // (handled in second pass below)
                let _ = s; // will process in second pass
            }
            if let TypedExpression::EnumDeclaration(e) = expr {
                enum_info.insert(
                    e.name.clone(),
                    EnumInfo {
                        variant_count: e.unit_variant_count() as i128,
                    },
                );
            }
        }

        // Collect bounded type fields from all struct declarations
        let mut resolved_bounds: std::collections::HashMap<
            (String, String),
            ResolvedFieldBounds,
        > = std::collections::HashMap::new();

        let mut validation_errors: Vec<String> = Vec::new();

        // Also build struct_bounds for cross-struct references (struct.min/max/cardinality)
        let mut struct_bounds: std::collections::HashMap<String, std::collections::HashMap<String, ResolvedFieldBounds>> =
            std::collections::HashMap::new();

        for expr in expressions {
            if let TypedExpression::StructDeclaration(s) = expr {
                let mut field_bounds_for_struct = std::collections::HashMap::new();
                let mut struct_errors: Vec<String> = Vec::new();

                for (field_name, field_type) in &s.fields {
                    let (min_expr, max_expr) = match field_type {
                        Type::BoundedInt { min, max } | Type::BoundedUint { min, max } => {
                            (min.clone(), max.clone())
                        }
                        _ => continue,
                    };

                    // Resolve min expression
                    let resolved_min = resolve_bound_expr(&min_expr, &enum_info, &struct_bounds);
                    let resolved_max = resolve_bound_expr(&max_expr, &enum_info, &struct_bounds);

                    match (resolved_min, resolved_max) {
                        (Ok(min_val), Ok(max_val)) => {
                            if min_val > max_val {
                                struct_errors.push(format!(
                                    "Bounds violation for field '{}.{}': min ({}) is greater than max ({})",
                                    s.name, field_name, min_val, max_val
                                ));
                            } else {
                                let bounds = ResolvedFieldBounds {
                                    min: min_val,
                                    max: max_val,
                                };
                                field_bounds_for_struct
                                    .insert(field_name.clone(), bounds.clone());
                                resolved_bounds
                                    .insert((s.name.clone(), field_name.clone()), bounds);
                            }
                        }
                        (Err(e), _) | (_, Err(e)) => {
                            struct_errors.push(e);
                        }
                    }
                }

                if !struct_errors.is_empty() {
                    validation_errors.extend(struct_errors);
                } else {
                    struct_bounds.insert(s.name.clone(), field_bounds_for_struct);
                }
            }
        }

        if !validation_errors.is_empty() {
            let error_list = validation_errors
                .iter()
                .map(|e| format!("  - {}", e))
                .collect::<Vec<_>>()
                .join("\n");
            return Err(format!("Bounded type validation failed:\n{}", error_list));
        }

        Ok(resolved_bounds)
    }

    /// Generate Rust code from a list of typed expressions.
    fn generate_rust_code(
        expressions: Vec<crate::nodes::TypedExpression>,
        resolved_bounds: std::collections::HashMap<
            (String, String),
            crate::nodes::ResolvedFieldBounds,
        >,
    ) -> Result<String, String> {
        use crate::nodes::TypedExpression;

        let mut code_parts: Vec<String> = Vec::new();
        let mut stream_vars: Vec<String> = Vec::new();
        let mut stream_var_runner_codes: Vec<String> = Vec::new();

        for expression in expressions {
            let code = match expression {
                TypedExpression::UnnamedReturningStream(s) => s.code(),
                TypedExpression::UnnamedNonReturningStream(s) => s.code(),
                TypedExpression::StructDeclaration(s) => {
                    // Gather bounds for this struct
                    let struct_field_bounds: std::collections::HashMap<String, crate::nodes::ResolvedFieldBounds> = resolved_bounds
                        .iter()
                        .filter_map(|((sname, fname), bounds)| {
                            if sname == &s.name {
                                Some((fname.clone(), bounds.clone()))
                            } else {
                                None
                            }
                        })
                        .collect();
                    if struct_field_bounds.is_empty() {
                        s.code()
                    } else {
                        s.code_with_bounds(Some(&struct_field_bounds))
                    }
                }
                TypedExpression::EnumDeclaration(e) => e.code(),
                TypedExpression::FnDeclaration(f) => f.code(),
                TypedExpression::StreamVariable(sv) => {
                    let runner_code = sv.runner_code();
                    let declaration_code = sv.declaration_code();
                    stream_vars.push(declaration_code);
                    stream_var_runner_codes.push(runner_code);
                    continue;
                }
                TypedExpression::StreamVariableFromPriorStreamVariable(sv) => {
                    let runner_code = sv.runner_code();
                    let declaration_code = sv.declaration_code();
                    stream_vars.push(declaration_code);
                    stream_var_runner_codes.push(runner_code);
                    continue;
                }
                TypedExpression::NamedReturningStream(s) => s.code(),
                TypedExpression::NamedNonReturningStream(s) => s.code(),
            };
            code_parts.push(code);
        }

        // Build final program
        // Stream variables need to be declared before stream_var_runner_codes are used,
        // and stream_var_runner_codes produce the final stream output.
        let stream_var_declarations = stream_vars.join("\n");
        let stream_var_runners = if stream_var_runner_codes.is_empty() {
            String::new()
        } else {
            format!(
                "use ::marigold::marigold_impl::*;\n{}",
                stream_var_runner_codes.join("\n")
            )
        };

        // Filter out stream var declarations from code_parts (they're handled separately)
        let main_code = code_parts.join("\n");

        // If we have stream vars, we need to combine everything differently
        if !stream_var_declarations.is_empty() {
            Ok(format!(
                "{stream_var_declarations}\n{stream_var_runners}\n{main_code}"
            ))
        } else {
            Ok(main_code)
        }
    }
}

impl Default for PestParser {
    fn default() -> Self {
        Self::new()
    }
}

impl MarigoldParser for PestParser {
    fn parse(&self, input: &str) -> Result<String, MarigoldParseError> {
        Self::parse_input(input).map_err(MarigoldParseError)
    }

    fn name(&self) -> &'static str {
        "PestParser"
    }
}

/// Helper struct for enum info during bounded type validation
struct EnumInfo {
    variant_count: i128,
}

/// Resolve a bound expression to a concrete i128 value.
fn resolve_bound_expr(
    expr: &crate::nodes::BoundExpr,
    enum_info: &std::collections::HashMap<String, EnumInfo>,
    struct_bounds: &std::collections::HashMap<String, std::collections::HashMap<String, crate::nodes::ResolvedFieldBounds>>,
) -> Result<i128, String> {
    use crate::nodes::{ArithOp, BoundExpr, BoundOp};

    match expr {
        BoundExpr::Literal(v) => Ok(*v),
        BoundExpr::TypeReference(type_name, op) => {
            let name = type_name.as_str();
            match op {
                BoundOp::Len => {
                    if let Some(info) = enum_info.get(name) {
                        Ok(info.variant_count)
                    } else {
                        Err(format!("Undefined type: '{name}'"))
                    }
                }
                BoundOp::Min => {
                    if let Some(fields) = struct_bounds.get(name) {
                        // For struct.min() we return the minimum of all field mins
                        fields
                            .values()
                            .map(|b| b.min)
                            .min()
                            .ok_or_else(|| format!("Struct '{name}' has no bounded fields"))
                    } else {
                        Err(format!("Undefined type: '{name}'"))
                    }
                }
                BoundOp::Max => {
                    if let Some(fields) = struct_bounds.get(name) {
                        fields
                            .values()
                            .map(|b| b.max)
                            .max()
                            .ok_or_else(|| format!("Struct '{name}' has no bounded fields"))
                    } else {
                        Err(format!("Undefined type: '{name}'"))
                    }
                }
                BoundOp::Cardinality => {
                    if let Some(info) = enum_info.get(name) {
                        Ok(info.variant_count)
                    } else if let Some(fields) = struct_bounds.get(name) {
                        // Cardinality of a struct is product of field cardinalities
                        let cardinality: i128 = fields
                            .values()
                            .map(|b| b.max - b.min + 1)
                            .product();
                        Ok(cardinality)
                    } else {
                        Err(format!("Undefined type: '{name}'"))
                    }
                }
            }
        }
        BoundExpr::BinaryOp { left, op, right } => {
            let left_val = resolve_bound_expr(left, enum_info, struct_bounds)?;
            let right_val = resolve_bound_expr(right, enum_info, struct_bounds)?;
            match op {
                ArithOp::Add => Ok(left_val + right_val),
                ArithOp::Sub => Ok(left_val - right_val),
                ArithOp::Mul => Ok(left_val * right_val),
                ArithOp::Div => {
                    if right_val == 0 {
                        Err("Division by zero in bound expression".to_string())
                    } else {
                        Ok(left_val / right_val)
                    }
                }
            }
        }
    }
}

/// Parse a Marigold program string and return the generated Rust code.
///
/// This is the primary public interface for parsing Marigold code.
/// It uses the PestParser internally.
///
/// # Examples
///
/// ```
/// use marigold_grammar::parser::parse_marigold;
/// let result = parse_marigold("range(0, 1).return");
/// assert!(result.is_ok());
/// ```
pub fn parse_marigold(input: &str) -> Result<String, MarigoldParseError> {
    PestParser::new().parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_range_return() {
        let result = parse_marigold("range(0, 1).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_invalid() {
        let result = parse_marigold("bad syntax");
        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        // Should start with exactly one "Parse error: " prefix
        assert!(
            msg.starts_with("Parse error: "),
            "Expected 'Parse error: ...' prefix, got: {msg:?}"
        );
        assert!(
            !msg.contains("Parse error: Parse error:"),
            "Double 'Parse error:' prefix detected: {msg:?}"
        );
    }

    #[test]
    fn test_parse_range_map_return() {
        let result = parse_marigold("range(0, 3).map(double).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_filter() {
        let result = parse_marigold("range(0, 10).filter(is_odd).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_permutations() {
        let result = parse_marigold("range(0, 4).permutations(2).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_combinations() {
        let result = parse_marigold("range(0, 4).combinations(2).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_struct_decl() {
        let result = parse_marigold(
            "struct Point { x: i32, y: i32, } range(0, 1).return",
        );
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_enum_decl() {
        let result = parse_marigold(
            r#"enum Color { Red, Green, Blue, } range(0, 3).return"#,
        );
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_fn_decl() {
        let result = parse_marigold(
            "fn double(x: i32) -> i32 { x * 2 } range(0, 5).map(double).return",
        );
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_inclusive_range() {
        let result = parse_marigold("range(0, =5).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_enum_range() {
        let result = parse_marigold(
            "enum Color { Red, Green, Blue, } range(Color).return",
        );
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_select_all() {
        let result = parse_marigold("select_all(range(0, 2), range(10, 12)).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_analyze_simple() {
        let result = PestParser::analyze("range(0, 5).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_analyze_invalid() {
        let result = PestParser::analyze("bad syntax");
        assert!(result.is_err());
        let err = result.unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.starts_with("Parse error: "),
            "Expected 'Parse error: ...' prefix, got: {msg:?}"
        );
        assert!(
            !msg.contains("Parse error: Parse error:"),
            "Double 'Parse error:' prefix detected: {msg:?}"
        );
    }

    #[test]
    fn test_range_enum_undeclared() {
        let result = parse_marigold("range(UndeclaredEnum).return");
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("not declared"), "Expected 'not declared' error, got: {msg:?}");
    }

    #[test]
    fn test_bounded_type_min_gt_max() {
        let result = parse_marigold(
            "struct Sensor { reading: int[100, 0], } range(0, 1).return",
        );
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("greater than max") || msg.contains("Bounded type"),
            "Expected bounds error, got: {msg:?}");
    }

    #[test]
    fn test_bounded_type_undefined_type_reference() {
        let result = parse_marigold(
            "struct Sensor { reading: int[0, NonExistent.len()], } range(0, 1).return",
        );
        assert!(result.is_err());
        let msg = result.unwrap_err().to_string();
        assert!(msg.contains("Undefined type") || msg.contains("NonExistent"),
            "Expected undefined type error, got: {msg:?}");
    }

    #[test]
    fn test_pest_parser_name() {
        let parser = PestParser::new();
        assert_eq!(parser.name(), "PestParser");
    }

    #[test]
    fn test_pest_parser_default() {
        let parser = PestParser::default();
        let result = parser.parse("range(0, 1).return");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_permutations_with_replacement() {
        let result = parse_marigold("range(0, 3).permutations_with_replacement(2).return");
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_chained_operations() {
        let result = parse_marigold(
            "range(0, 10).filter(is_even).map(double).return",
        );
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parse_keep_first_n() {
        let result = parse_marigold(
            "range(0, 5).permutations(2).keep_first_n(3, sorter).return",
        );
        assert!(result.is_ok(), "Expected Ok but got: {:?}", result);
    }

    #[test]
    fn test_parser_error_no_double_prefix() {
        // This test specifically verifies the fix for the double "Parse error:" bug.
        // Previously, both parse_input() and analyze() added "Parse error: " before
        // wrapping in MarigoldParseError, which itself adds "Parse error: " in Display.
        // The fix: remove the redundant prefix from parse_input() and analyze().
        for input in &["bad syntax", "!!!", "123 invalid", ""] {
            let result = parse_marigold(input);
            if let Err(e) = result {
                let msg = e.to_string();
                // Exactly one "Parse error: " prefix
                assert!(
                    msg.starts_with("Parse error: "),
                    "For input {input:?}: expected single 'Parse error: ' prefix, got: {msg:?}"
                );
                assert!(
                    !msg.contains("Parse error: Parse error:"),
                    "For input {input:?}: double prefix detected: {msg:?}"
                );
            }
        }
    }
}
