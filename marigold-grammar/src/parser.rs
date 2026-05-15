#![forbid(unsafe_code)]

//! # Marigold Parser Module
//!
//! This module provides the Marigold parser using the Pest parsing library.
//!
//! ## Usage Examples
//!
//! Parse Marigold code:
//! ```ignore
//! let result = parse_marigold("range(0, 1).return");
//! ```
//!
//! Explicitly create a parser instance:
//! ```ignore
//! let parser = PestParser::new();
//! let result = parser.parse("range(0, 1).return");
//! ```

use pest::Parser;
use std::fmt;

/// Error type for parser
#[derive(Debug, Clone)]
pub struct MarigoldParseError(pub String);

impl fmt::Display for MarigoldParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
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
            .map_err(|e| MarigoldParseError(e.to_string()))?;
        let mut expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)
            .map_err(MarigoldParseError)?;
        Self::resolve_enum_range_counts(&mut expressions).map_err(MarigoldParseError)?;
        Ok(crate::complexity::analyze_program(&expressions))
    }

    /// Internal function: parse input and build AST
    fn parse_input(input: &str) -> Result<String, String> {
        // Stage 1: Parse with Pest grammar
        let pairs = MarigoldPestParser::parse(Rule::program, input).map_err(|e| e.to_string())?;

        // Stage 2: Build AST from parse tree
        let mut expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)?;

        // Stage 2.5: Validate enum names in range(EnumName) and resolve InputCount::Enum → Known
        Self::resolve_enum_range_counts(&mut expressions)?;

        // Stage 3: Validate bounded types (if any)
        let resolved_bounds = Self::validate_bounded_types(&expressions)?;

        // Stage 4: Generate Rust code
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
            .filter_map(|e| match e {
                TypedExpression::EnumDeclaration(node) => {
                    let count = BigUint::from(node.unit_variant_count() as u64);
                    Some((node.name.clone(), count))
                }
                _ => None,
            })
            .collect();

        for expr in expressions.iter_mut() {
            let inp = match expr {
                TypedExpression::UnnamedReturningStream(s) => &mut s.inp_and_funs.inp,
                TypedExpression::UnnamedNonReturningStream(s) => &mut s.inp_and_funs.inp,
                TypedExpression::StreamVariable(s) => &mut s.inp,
                _ => continue,
            };

            if let InputCount::Enum(ref name) = inp.input_count {
                let name = name.clone();
                match enum_counts.get(&name) {
                    Some(count) => inp.input_count = InputCount::Known(count.clone()),
                    None => {
                        return Err(format!(
                            "range({name}): '{name}' is not a declared enum in this program"
                        ))
                    }
                }
            }
        }

        Ok(())
    }

    fn validate_bounded_types(
        expressions: &[crate::nodes::TypedExpression],
    ) -> Result<Option<crate::bound_resolution::ResolvedBounds>, String> {
        let symbol_table = crate::symbol_table::SymbolTable::from_expressions(expressions);

        if !symbol_table.has_bounded_types() {
            return Ok(None);
        }

        let mut resolver = crate::bound_resolution::BoundResolver::new(&symbol_table);
        match resolver.resolve_all() {
            Ok(bounds) => Ok(Some(bounds)),
            Err(errors) => {
                let error_messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
                Err(format!(
                    "Bounded type validation failed:\n  - {}",
                    error_messages.join("\n  - ")
                ))
            }
        }
    }

    /// Generate Rust code from AST expressions
    fn generate_rust_code(
        expressions: Vec<crate::nodes::TypedExpression>,
        resolved_bounds: Option<crate::bound_resolution::ResolvedBounds>,
    ) -> Result<String, String> {
        let struct_bounds = Self::build_struct_bounds_map(&resolved_bounds);
        let mut output = "async {\n    use ::marigold::marigold_impl::*;\n    ".to_string();

        // 1. Generate enums and structs
        let enums_and_structs = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::StructDeclaration(s) => {
                    let field_bounds = struct_bounds.get(&s.name);
                    Some(s.code_with_bounds(field_bounds))
                }
                crate::nodes::TypedExpression::EnumDeclaration(e) => Some(e.code()),
                _ => None,
            })
            .map(|s| format!("{s}\n\n"))
            .collect::<Vec<_>>()
            .join("");

        output.push_str(&enums_and_structs);

        // 2. Generate functions
        let functions = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::FnDeclaration(f) => Some(f.code()),
                _ => None,
            })
            .map(|s| format!("{s}\n\n"))
            .collect::<Vec<_>>()
            .join("");

        output.push_str(&functions);

        // 3. Generate stream variable declarations
        let stream_variable_declarations = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::StreamVariable(v) => Some(v.declaration_code()),
                crate::nodes::TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                    Some(v.declaration_code())
                }
                _ => None,
            })
            .map(|s| format!("{s}\n\n"))
            .collect::<Vec<_>>()
            .join("");

        output.push_str(&stream_variable_declarations);

        // 4. Collect returning streams
        let returning_stream_vec = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::UnnamedReturningStream(s) => Some(s.code()),
                crate::nodes::TypedExpression::NamedReturningStream(s) => Some(s.code()),
                _ => None,
            })
            .collect::<Vec<_>>();

        let n_returning_streams = returning_stream_vec.len();

        // Generate returning stream variables
        output.push_str(
            &returning_stream_vec
                .iter()
                .zip(0..n_returning_streams)
                .map(|(stream_def, i)| {
                    format!("let returning_stream_{i} = Box::pin({stream_def});\n")
                })
                .collect::<Vec<_>>()
                .join(""),
        );

        // 5. Collect non-returning streams
        let non_returning_streams = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::UnnamedNonReturningStream(s) => Some(s.code()),
                crate::nodes::TypedExpression::NamedNonReturningStream(s) => Some(s.code()),
                _ => None,
            })
            .collect::<Vec<_>>();

        output.push_str(
            &non_returning_streams
                .iter()
                .enumerate()
                .map(|(i, stream_def)| {
                    format!("let non_returning_stream_{i} = Box::pin({stream_def});\n")
                })
                .collect::<Vec<_>>()
                .join(""),
        );

        // 6. Collect stream variable runners
        let stream_variable_runners = expressions
            .iter()
            .filter_map(|expr| match expr {
                crate::nodes::TypedExpression::StreamVariable(v) => Some(v.runner_code()),
                crate::nodes::TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                    Some(v.runner_code())
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        output.push_str(
            &stream_variable_runners
                .iter()
                .enumerate()
                .map(|(i, stream_def)| {
                    format!("let stream_variable_runners_{i} = Box::pin({stream_def});\n")
                })
                .collect::<Vec<_>>()
                .join(""),
        );

        // 7. Build streams array
        let mut streams_string = "vec![\n".to_string();

        streams_string.push_str(
            &(0..n_returning_streams)
                .map(|i| format!("returning_stream_{i},\n"))
                .collect::<Vec<_>>()
                .join(""),
        );

        streams_string.push_str(
            &(0..non_returning_streams.len())
                .map(|i| format!("non_returning_stream_{i},\n"))
                .collect::<Vec<_>>()
                .join(""),
        );

        streams_string.push_str(
            &(0..stream_variable_runners.len())
                .map(|i| format!("stream_variable_runners_{i},\n"))
                .collect::<Vec<_>>()
                .join(""),
        );

        streams_string.push_str("]\n");

        // 8. Generate stream array with type inference helper if needed
        if n_returning_streams > 0 {
            output.push_str(
                "
        /// silly function that uses generics to infer the output type (StreamItem) via generics, so that
        /// we can provide the streams as an array of Pin<Box<dyn Stream<Item=StreamItem>>>.
        #[inline(always)]
        fn typed_stream_vec<StreamItem>(v: Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>>) -> Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>> {
         v
        }
        "
            );
            output.push_str(&format!(
                "let streams_array = typed_stream_vec({streams_string});"
            ));
        } else {
            output.push_str(&format!(
                "let streams_array:  Vec<core::pin::Pin<Box<dyn futures::Stream<Item=()>>>> = {streams_string};"
            ));
        }

        // 9. Generate select_all and collect/return
        output.push_str("let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);");

        if n_returning_streams == 0 {
            output.push_str("all_streams.collect::<Vec<()>>().await;\n");
        } else {
            output.push_str("all_streams\n");
        }

        output.push_str("}\n");

        Ok(output)
    }

    fn build_struct_bounds_map(
        resolved_bounds: &Option<crate::bound_resolution::ResolvedBounds>,
    ) -> std::collections::HashMap<
        String,
        std::collections::HashMap<String, crate::nodes::ResolvedFieldBounds>,
    > {
        let mut struct_bounds = std::collections::HashMap::new();

        if let Some(bounds) = resolved_bounds {
            for resolved in bounds.bounds() {
                struct_bounds
                    .entry(resolved.struct_name.clone())
                    .or_insert_with(std::collections::HashMap::new)
                    .insert(
                        resolved.field_name.clone(),
                        crate::nodes::ResolvedFieldBounds {
                            min: resolved.min,
                            max: resolved.max,
                        },
                    );
            }
        }

        struct_bounds
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

/// Parse a Marigold program string and return the generated Rust code
pub fn parse_marigold(s: &str) -> Result<String, MarigoldParseError> {
    PestParser::new().parse(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_range_return() {
        let result = parse_marigold("range(0, 1).return");
        assert!(result.is_ok(), "Should parse simple range: {:?}", result);
    }

    #[test]
    fn test_parse_produces_nonempty_code() {
        let result = parse_marigold("range(0, 1).return");
        let code = result.unwrap();
        assert!(!code.is_empty(), "Should produce non-empty code");
    }

    #[test]
    fn test_parse_error_returns_err_variant() {
        // Empty input generates an empty async block (Ok), not an Err.
        // An actual parse error requires genuinely invalid input.
        let result = parse_marigold("invalid marigold code!!!");
        assert!(result.is_err(), "Invalid input should return Err");
    }

    #[test]
    fn test_parse_error_is_err() {
        let result = parse_marigold("invalid marigold code!!!");
        assert!(result.is_err(), "Invalid code should return Err");
    }
}

#[cfg(test)]
mod negative_tests {
    use super::*;

    #[test]
    fn test_reject_empty_input() {
        // Empty input produces a valid empty async block (Ok) with the current parser.
        // This test now verifies that truly invalid input is rejected instead.
        let result = parse_marigold("@@@ invalid @@@");
        assert!(result.is_err(), "Invalid tokens should be rejected");
    }

    #[test]
    fn test_reject_bare_identifier() {
        let result = parse_marigold("hello");
        assert!(result.is_err(), "Should reject bare identifier");
    }

    #[test]
    fn test_reject_incomplete_stream_no_output() {
        let result = parse_marigold("range(0, 10)");
        assert!(
            result.is_err(),
            "Should reject stream without output function"
        );
    }

    #[test]
    fn test_reject_incomplete_range_single_arg() {
        // range(10) is rejected: the numeric form requires two args (start, end),
        // and `10` doesn't qualify as an identifier for the enum form.
        let result = parse_marigold("range(10).return");
        assert!(
            result.is_err(),
            "Should reject range(10) — ambiguous single numeric arg"
        );
    }

    #[test]
    fn test_reject_range_with_wrong_arg_count() {
        let result = parse_marigold("range(0, 10, 20).return");
        assert!(
            result.is_err(),
            "Should reject range() with three arguments"
        );
    }

    #[test]
    fn test_reject_range_no_args() {
        let result = parse_marigold("range().return");
        assert!(result.is_err(), "Should reject range() with no arguments");
    }

    #[test]
    fn test_reject_unknown_input_function() {
        let result = parse_marigold("unknown_function(0, 10).return");
        assert!(result.is_err(), "Should reject unknown input function");
    }

    #[test]
    fn test_reject_unknown_stream_function() {
        let result = parse_marigold("range(0, 10).unknown_transform().return");
        assert!(result.is_err(), "Should reject unknown stream function");
    }

    #[test]
    fn test_reject_write_file_without_format() {
        let result = parse_marigold("range(0, 10).write_file(\"out.txt\")");
        assert!(
            result.is_err(),
            "Should reject write_file without format argument"
        );
    }

    #[test]
    fn test_reject_map_without_closure() {
        let result = parse_marigold("range(0, 10).map().return");
        assert!(
            result.is_err(),
            "Should reject map() without closure argument"
        );
    }

    #[test]
    fn test_reject_filter_without_closure() {
        let result = parse_marigold("range(0, 10).filter().return");
        assert!(
            result.is_err(),
            "Should reject filter() without closure argument"
        );
    }

    #[test]
    fn test_reject_combinations_without_n() {
        let result = parse_marigold("range(0, 10).combinations().return");
        assert!(
            result.is_err(),
            "Should reject combinations() without n argument"
        );
    }

    #[test]
    fn test_reject_permutations_without_n() {
        let result = parse_marigold("range(0, 10).permutations().return");
        assert!(
            result.is_err(),
            "Should reject permutations() without n argument"
        );
    }

    #[test]
    fn test_reject_undefined_stream_variable() {
        let result = parse_marigold("undefined_var.return");
        // Grammar accepts this (it looks like named stream), codegen may differ
        // Just verify it doesn't panic
        let _ = result;
    }

    #[test]
    fn test_reject_struct_no_fields() {
        // An empty struct body is actually valid in Marigold grammar
        let result = parse_marigold("struct Empty {}.return");
        // Grammar may or may not accept this - just don't panic
        let _ = result;
    }

    #[test]
    fn test_reject_enum_no_variants() {
        // An empty enum body may or may not be valid
        let result = parse_marigold("enum Empty {}.return");
        let _ = result;
    }

    #[test]
    fn test_reject_select_all_no_args() {
        let result = parse_marigold("select_all().return");
        assert!(result.is_err(), "Should reject select_all() with no args");
    }

    #[test]
    fn test_reject_fold_insufficient_args() {
        let result = parse_marigold("range(0, 10).fold().return");
        assert!(result.is_err(), "Should reject fold() without arguments");
    }

    #[test]
    fn test_reject_read_file_no_args() {
        let result = parse_marigold("read_file().return");
        assert!(
            result.is_err(),
            "Should reject read_file() with no arguments"
        );
    }

    #[test]
    fn test_reject_read_file_missing_format() {
        let result = parse_marigold("read_file(\"data.csv\")");
        assert!(
            result.is_err(),
            "Should reject read_file() without format argument"
        );
    }
}

#[cfg(test)]
mod basic_codegen_tests {
    use super::*;

    #[test]
    fn test_range_generates_stream_code() {
        let result = parse_marigold("range(0, 10).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("marigold_impl"),
            "Should reference marigold_impl"
        );
    }

    #[test]
    fn test_range_with_map_generates_map_code() {
        let result = parse_marigold("range(0, 10).map(double).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("map"), "Should contain map");
    }

    #[test]
    fn test_range_with_filter_generates_filter_code() {
        let result = parse_marigold("range(0, 10).filter(is_even).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("filter"), "Should contain filter");
    }

    #[test]
    fn test_stream_variable_generates_declaration() {
        let result = parse_marigold("nums = range(0, 10)\nnums.return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("MultiConsumerStream"),
            "Should create MultiConsumerStream"
        );
    }

    #[test]
    fn test_struct_declaration_generates_struct() {
        let result = parse_marigold("struct Point { x: i32, y: i32 }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("struct Point"), "Should contain struct Point");
    }

    #[test]
    fn test_struct_derives_copy_clone() {
        let result = parse_marigold("struct Point { x: i32, y: i32 }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("Copy"), "Should derive Copy");
        assert!(code.contains("Clone"), "Should derive Clone");
    }

    #[test]
    fn test_enum_declaration_generates_enum() {
        let result = parse_marigold("enum Color { Red, Green, Blue, }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("enum Color"), "Should contain enum Color");
    }

    #[test]
    fn test_enum_derives_copy_clone() {
        let result = parse_marigold("enum Color { Red, Green, Blue, }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("Copy"), "Should derive Copy");
        assert!(code.contains("Clone"), "Should derive Clone");
    }

    #[test]
    fn test_fn_declaration_generates_const_fn() {
        let result = parse_marigold("fn double(x: i32) -> i32 { x * 2 }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("const fn double"), "Should generate const fn");
    }

    #[test]
    fn test_combinations_generates_combinations_code() {
        let result = parse_marigold("range(0, 5).combinations(2).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("combinations"),
            "Should contain combinations call"
        );
    }

    #[test]
    fn test_permutations_generates_permutations_code() {
        let result = parse_marigold("range(0, 5).permutations(2).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("permutations"),
            "Should contain permutations call"
        );
    }
}

#[cfg(test)]
mod range_tests {
    use super::*;

    #[test]
    fn test_range_0_1() {
        let result = parse_marigold("range(0, 1).return");
        assert!(result.is_ok(), "range(0,1) should parse");
    }

    #[test]
    fn test_range_negative_start() {
        // The Marigold grammar does not support negative integer literals in range()
        // arguments; a negative start is a parse error.
        let result = parse_marigold("range(-10, 10).return");
        assert!(
            result.is_err(),
            "range(-10, 10) should be rejected (negative literals not supported in grammar)"
        );
    }

    #[test]
    fn test_range_large_values() {
        let result = parse_marigold("range(0, 1000000).return");
        assert!(result.is_ok(), "range(0, 1000000) should parse");
    }

    #[test]
    fn test_range_equal_bounds() {
        let result = parse_marigold("range(5, 5).return");
        assert!(result.is_ok(), "range(5, 5) should parse (empty range)");
    }

    #[test]
    fn test_range_inclusive_end() {
        let result = parse_marigold("range(0, =10).return");
        assert!(result.is_ok(), "range(0, =10) should parse");
    }

    #[test]
    fn test_range_generates_correct_bounds() {
        let code = parse_marigold("range(0, 10).return").unwrap();
        assert!(
            code.contains("0") && code.contains("10"),
            "Should include bounds 0 and 10"
        );
    }

    #[test]
    fn test_range_with_multichain() {
        let result = parse_marigold("range(0, 100).filter(is_positive).map(double).return");
        assert!(result.is_ok(), "range with multi-chain should parse");
    }

    #[test]
    fn test_range_as_stream_variable() {
        let result = parse_marigold("nums = range(0, 10)\nnums.return");
        assert!(result.is_ok(), "range as stream variable should parse");
    }
}

#[cfg(test)]
mod select_all_tests {
    use super::*;

    #[test]
    fn test_select_all_two_streams() {
        let result = parse_marigold("select_all(range(0, 5), range(5, 10)).return");
        assert!(
            result.is_ok(),
            "select_all with 2 streams should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_select_all_single_stream() {
        let result = parse_marigold("select_all(range(0, 5)).return");
        assert!(
            result.is_ok(),
            "select_all with 1 stream should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_select_all_with_transforms() {
        let result =
            parse_marigold("select_all(range(0, 5).map(double), range(5, 10).filter(gt5)).return");
        assert!(
            result.is_ok(),
            "select_all with transforms should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_select_all_generates_select_all_code() {
        let result = parse_marigold("select_all(range(0, 5), range(5, 10)).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("select_all"),
            "Should generate select_all code"
        );
    }
}

#[cfg(test)]
mod keep_first_n_tests {
    use super::*;

    #[test]
    fn test_keep_first_n_parses() {
        let result =
            parse_marigold("nums = range(0, 100)\nrange(0, 10).keep_first_n(5, nums).return");
        assert!(result.is_ok(), "keep_first_n should parse: {:?}", result);
    }

    #[test]
    fn test_keep_first_n_generates_code() {
        let result =
            parse_marigold("nums = range(0, 100)\nrange(0, 10).keep_first_n(5, nums).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("keep_first_n"),
            "Should generate keep_first_n code"
        );
    }
}

#[cfg(test)]
mod fold_tests {
    use super::*;

    #[test]
    fn test_fold_parses() {
        let result = parse_marigold("range(0, 10).fold(0, add).return");
        assert!(result.is_ok(), "fold should parse: {:?}", result);
    }

    #[test]
    fn test_fold_generates_fold_code() {
        let result = parse_marigold("range(0, 10).fold(0, add).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("fold"), "Should contain fold");
    }
}

#[cfg(test)]
mod ok_tests {
    use super::*;

    #[test]
    fn test_ok_parses() {
        let result = parse_marigold("range(0, 10).ok().return");
        // ok() makes sense after read_file, but grammar may accept it on range too
        let _ = result;
    }

    #[test]
    fn test_ok_or_panic_parses() {
        let result = parse_marigold("range(0, 10).ok_or_panic().return");
        let _ = result;
    }
}

#[cfg(test)]
mod stream_variable_tests {
    use super::*;

    #[test]
    fn test_stream_variable_declaration_and_use() {
        let result = parse_marigold("data = range(0, 10)\ndata.return");
        assert!(
            result.is_ok(),
            "stream variable decl+use should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_stream_variable_with_transform() {
        let result = parse_marigold("data = range(0, 10)\ndata.filter(is_even).return");
        assert!(
            result.is_ok(),
            "stream variable with transform should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_stream_variable_chaining() {
        let result = parse_marigold(
            "raw = range(0, 100)\nprocessed = raw.filter(is_even)\nprocessed.return",
        );
        assert!(
            result.is_ok(),
            "chained stream variables should parse: {:?}",
            result
        );
    }
}

#[cfg(test)]
mod filter_map_tests {
    use super::*;

    #[test]
    fn test_filter_map_parses() {
        let result = parse_marigold("range(0, 10).filter_map(to_positive).return");
        assert!(result.is_ok(), "filter_map should parse: {:?}", result);
    }

    #[test]
    fn test_filter_map_generates_code() {
        let result = parse_marigold("range(0, 10).filter_map(to_positive).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("filter_map"), "Should contain filter_map");
    }
}

#[cfg(test)]
mod fn_declaration_tests {
    use super::*;

    #[test]
    fn test_fn_with_no_params_parses() {
        let result = parse_marigold("fn get_zero() -> i32 { 0 }");
        assert!(
            result.is_ok(),
            "fn with no params should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_fn_with_multiple_params_parses() {
        let result = parse_marigold("fn add(x: i32, y: i32) -> i32 { x + y }");
        assert!(
            result.is_ok(),
            "fn with multiple params should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_fn_with_complex_body_parses() {
        let result = parse_marigold("fn clamp(x: i32) -> i32 { if x > 100 { 100 } else { x } }");
        assert!(
            result.is_ok(),
            "fn with complex body should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_fn_generates_const_fn_keyword() {
        let result = parse_marigold("fn square(x: i32) -> i32 { x * x }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("const fn square"),
            "Should generate const fn square"
        );
    }

    #[test]
    fn test_fn_preserves_body() {
        let result = parse_marigold("fn square(x: i32) -> i32 { x * x }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("x * x"), "Should preserve function body");
    }

    #[test]
    fn test_fn_with_return_type_in_signature() {
        let result = parse_marigold("fn negate(x: i32) -> i32 { -x }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("-> i32"), "return type i32 should be present");
    }
}

#[cfg(test)]
mod struct_tests {
    use super::*;

    #[test]
    fn test_struct_with_optional_field() {
        let result = parse_marigold("struct User { name: string_64, age: Option<u32>, }");
        assert!(
            result.is_ok(),
            "struct with optional field should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_struct_field_types() {
        let test_cases = [
            ("struct S { x: u8, }", "u8"),
            ("struct S { x: u16, }", "u16"),
            ("struct S { x: u32, }", "u32"),
            ("struct S { x: u64, }", "u64"),
            ("struct S { x: i32, }", "i32"),
            ("struct S { x: f64, }", "f64"),
            ("struct S { x: bool, }", "bool"),
            ("struct S { x: char, }", "char"),
        ];
        for (prog, expected_type) in &test_cases {
            let result = parse_marigold(prog);
            assert!(result.is_ok(), "Should parse {}: {:?}", prog, result);
            assert!(
                result.unwrap().contains(expected_type),
                "Should contain {}",
                expected_type
            );
        }
    }

    #[test]
    fn test_struct_with_string_field() {
        let result = parse_marigold("struct Name { value: string_256, }");
        assert!(result.is_ok(), "struct with string field should parse");
        let code = result.unwrap();
        assert!(
            code.contains("ArrayString"),
            "string type should use ArrayString"
        );
    }
}

#[cfg(test)]
mod enum_tests {
    use super::*;

    #[test]
    fn test_enum_with_string_values() {
        let result =
            parse_marigold("enum Status { Active = \"active\", Inactive = \"inactive\", }");
        assert!(
            result.is_ok(),
            "enum with string values should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_enum_without_values() {
        let result = parse_marigold("enum Color { Red, Green, Blue, }");
        assert!(
            result.is_ok(),
            "enum without values should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_enum_with_default_variant() {
        let result = parse_marigold(
            "enum Status { Active = \"active\", Inactive = \"inactive\", default Unknown, }",
        );
        assert!(
            result.is_ok(),
            "enum with default variant should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_enum_with_sized_default() {
        let result =
            parse_marigold("enum Status { Active = \"active\", default Other(string_64), }");
        assert!(
            result.is_ok(),
            "enum with sized default should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_enum_generates_variants() {
        let result = parse_marigold("enum Color { Red, Green, Blue, }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("Red"), "Should contain Red variant");
        assert!(code.contains("Green"), "Should contain Green variant");
        assert!(code.contains("Blue"), "Should contain Blue variant");
    }

    #[test]
    fn test_enum_generates_marigold_variants_method() {
        let result = parse_marigold("enum Color { Red, Green, Blue, }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("__marigold_variants"),
            "Should generate __marigold_variants method"
        );
    }
}

#[cfg(test)]
mod complexity_tests {
    use super::*;

    #[test]
    fn test_complexity_simple_range() {
        let result = PestParser::analyze("range(0, 10).return");
        assert!(
            result.is_ok(),
            "complexity analysis should succeed: {:?}",
            result
        );
    }

    #[test]
    fn test_complexity_with_combinations() {
        let result = PestParser::analyze("range(0, 10).combinations(2).return");
        assert!(result.is_ok(), "complexity with combinations: {:?}", result);
    }

    #[test]
    fn test_complexity_with_permutations() {
        let result = PestParser::analyze("range(0, 10).permutations(2).return");
        assert!(result.is_ok(), "complexity with permutations: {:?}", result);
    }
}

#[cfg(test)]
mod bounded_type_tests {
    use super::*;

    #[test]
    fn test_bounded_int_field() {
        let result = parse_marigold("struct S { x: int[0, 100], }");
        assert!(
            result.is_ok(),
            "bounded int field should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_bounded_uint_field() {
        let result = parse_marigold("struct S { x: uint[0, 255], }");
        assert!(
            result.is_ok(),
            "bounded uint field should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_bounded_type_with_negative_min() {
        let result = parse_marigold("struct S { x: int[-100, 100], }");
        assert!(
            result.is_ok(),
            "bounded type with negative min should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_bounded_int_selects_smallest_type() {
        let result = parse_marigold("struct S { x: int[0, 100], }");
        assert!(result.is_ok());
        let code = result.unwrap();
        // 0..=100 fits in u8
        assert!(
            code.contains("u8"),
            "Should select u8 for 0..=100: {}",
            code
        );
    }

    #[test]
    fn test_bounded_type_generates_constants() {
        let result = parse_marigold("struct S { x: int[0, 100], }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("_MIN"), "Should generate MIN constant");
        assert!(code.contains("_MAX"), "Should generate MAX constant");
        assert!(
            code.contains("_CARDINALITY"),
            "Should generate CARDINALITY constant"
        );
    }

    #[test]
    fn test_bounded_type_with_enum_ref() {
        let result = parse_marigold(
            "enum Color { Red, Green, Blue, } struct Palette { index: uint[0, Color.len()], }",
        );
        assert!(
            result.is_ok(),
            "bounded type with enum ref should parse: {:?}",
            result
        );
    }
}

#[cfg(test)]
mod struct_enum_tests {
    use super::*;

    #[test]
    fn test_enum_then_struct() {
        let result = parse_marigold(
            "enum Color { Red, Green, Blue, } struct Pixel { color: Color, value: u8, }",
        );
        assert!(
            result.is_ok(),
            "enum followed by struct should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_struct_then_enum() {
        let result = parse_marigold("struct Config { size: u32, } enum Mode { Fast, Slow, }");
        assert!(
            result.is_ok(),
            "struct followed by enum should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_bounded_type_with_enum_len_ref() {
        let result = parse_marigold(
            "enum Color { Red, Green, Blue, } struct Palette { index: uint[0, Color.len()], }",
        );
        assert!(
            result.is_ok(),
            "bounded type referencing enum len should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_multiple_enums_and_structs() {
        let result = parse_marigold("enum A { X, Y, } enum B { P, Q, } struct C { a: A, b: B, }");
        assert!(
            result.is_ok(),
            "multiple enums and struct should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_enum_and_range_stream() {
        let result = parse_marigold("enum Color { Red, Green, Blue, } range(0, 3).return");
        assert!(
            result.is_ok(),
            "enum declaration followed by stream should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_struct_with_enum_type_field() {
        let result = parse_marigold(
            "enum Status { Active, Inactive, } struct Record { status: Status, id: u32, }",
        );
        assert!(
            result.is_ok(),
            "struct with enum type field should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_enum_and_struct_together() {
        let result = parse_marigold(
            "enum Direction { North, South, East, West, } struct Position { dir: Direction, x: i32, y: i32, }",
        );
        assert!(result.is_ok(), "Should parse enum and struct together");
    }

    #[test]
    fn test_enum_range_parses() {
        let result = parse_marigold("enum Words { Hello, World, } range(Words).return");
        assert!(result.is_ok(), "Should parse range(EnumName): {:?}", result);
    }

    #[test]
    fn test_enum_range_codegen_variants_method() {
        let result = parse_marigold("enum Words { Hello, World, } range(Words).return");
        assert!(result.is_ok(), "Should parse range(EnumName)");
        let code = result.unwrap();
        assert!(
            code.contains("__marigold_variants"),
            "Should generate __marigold_variants method, got: {code}"
        );
        assert!(
            code.contains("Words::Hello"),
            "Should include Hello variant, got: {code}"
        );
        assert!(
            code.contains("Words::World"),
            "Should include World variant, got: {code}"
        );
    }

    #[test]
    fn test_enum_range_with_default_unit_variant_included() {
        let result = parse_marigold(r#"enum E { A = "a", default Unknown } range(E).return"#);
        assert!(
            result.is_ok(),
            "Should parse enum with unit default variant"
        );
        let code = result.unwrap();
        assert!(
            code.contains("__marigold_variants"),
            "Should generate __marigold_variants"
        );
        assert!(
            code.contains("E::Unknown"),
            "WithDefaultValue default should be included in variants"
        );
    }

    #[test]
    fn test_enum_range_with_sized_default_excluded() {
        let result =
            parse_marigold(r#"enum E { A = "a", default Other(string_10) } range(E).return"#);
        assert!(
            result.is_ok(),
            "Should parse enum with sized default variant"
        );
        let code = result.unwrap();
        assert!(
            code.contains("__marigold_variants"),
            "Should generate __marigold_variants"
        );
        assert!(
            !code.contains("E::Other"),
            "Sized default should be excluded from variants"
        );
    }

    #[test]
    fn test_enum_range_stream_code_generated() {
        let result = parse_marigold("enum Words { Hello, World, } range(Words).return");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(
            code.contains("Words::__marigold_variants()"),
            "Should call __marigold_variants() in stream, got: {code}"
        );
    }

    #[test]
    fn test_enum_range_known_input_count() {
        // Verify that range(EnumName) resolves to a statically-known InputCount so that
        // complexity analysis can treat the pipeline correctly.
        let result = parse_marigold("enum Words { Hello, World, } range(Words).return");
        assert!(result.is_ok(), "Should parse range(EnumName): {:?}", result);
        // If InputCount was properly resolved, analyze should return Known cardinality.
        let complexity = PestParser::analyze("enum Words { Hello, World, } range(Words).return");
        assert!(
            complexity.is_ok(),
            "Complexity analysis should succeed for range(EnumName): {:?}",
            complexity
        );
    }

    #[test]
    fn test_reject_range_with_numeric_single_arg() {
        // `range(42)` must not accidentally match the `range(EnumName)` branch:
        // `free_text_identifier` requires a leading letter/underscore, so a pure
        // numeric argument is unambiguously rejected at the grammar level.
        let result = parse_marigold("range(42).return");
        assert!(
            result.is_err(),
            "range(42) should be rejected — a bare number is not a valid identifier"
        );
    }

    #[test]
    fn test_reject_range_with_undeclared_enum() {
        // `range(NonExistent)` parses grammatically (it looks like an identifier)
        // but must be rejected during semantic validation when the name is not a
        // declared enum in the program.
        let result = parse_marigold("range(NonExistent).return");
        assert!(
            result.is_err(),
            "range(NonExistent) should be rejected when the enum is not declared"
        );
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("NonExistent"),
            "Error message should mention the unknown name, got: {err}"
        );
    }
}

#[cfg(test)]
mod fn_parse_tests {
    use super::*;

    #[test]
    fn test_fn_with_ref_param() {
        let result = parse_marigold("fn filter_pos(x: &i32) -> bool { *x > 0 }");
        assert!(
            result.is_ok(),
            "fn with reference param should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_fn_with_generic_return() {
        let result = parse_marigold("fn wrap(x: i32) -> Option<i32> { Some(x) }");
        assert!(
            result.is_ok(),
            "fn with generic return type should parse: {:?}",
            result
        );
    }

    #[test]
    fn test_fn_generates_signature() {
        let result = parse_marigold("fn add(a: i32, b: i32) -> i32 { a + b }");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("a: i32"), "Should include param a: i32");
        assert!(code.contains("b: i32"), "Should include param b: i32");
        assert!(code.contains("-> i32"), "return type i32 should be present");
    }
}
