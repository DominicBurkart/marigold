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

    /// Internal function: parse input and build AST
    fn parse_input(input: &str) -> Result<String, String> {
        // Stage 1: Parse with Pest grammar
        let pairs = MarigoldPestParser::parse(Rule::program, input)
            .map_err(|e| format!("Parse error: {}", e))?;

        // Stage 2: Build AST from parse tree
        let expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)?;

        // Stage 3: Validate bounded types (if any)
        let resolved_bounds = Self::validate_bounded_types(&expressions)?;

        // Stage 4: Generate Rust code (replicating LALRPOP logic)
        Self::generate_rust_code(expressions, resolved_bounds)
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
                let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
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
        "Pest"
    }
}

/// Factory function that returns the parser
pub fn get_parser() -> Box<dyn MarigoldParser> {
    Box::new(PestParser::new())
}

/// Convenience function that uses the default parser to parse input
pub fn parse_marigold(input: &str) -> Result<String, MarigoldParseError> {
    let parser = get_parser();
    parser.parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pest_parser_creation() {
        let parser = PestParser::new();
        assert_eq!(parser.name(), "Pest");
    }

    #[test]
    fn test_default_parser_selection() {
        let parser = get_parser();
        assert_eq!(parser.name(), "Pest");
    }

    #[test]
    fn test_parse_marigold_function() {
        let result = parse_marigold("");

        assert!(result.is_ok());

        let output = result.unwrap();
        assert!(output.contains("async"));
    }

    #[test]
    fn test_pest_parser_basic_functionality() {
        let parser = PestParser::new();

        let result = parser.parse("");
        assert!(result.is_ok());

        let output = result.unwrap();
        assert!(output.contains("async"));
    }

    #[test]
    fn test_pest_parser_empty_input() {
        let parser = PestParser::new();
        let result = parser.parse("");

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.contains("async"));
        assert!(output.contains("use ::marigold::marigold_impl::*"));
    }

    #[test]
    fn test_pest_grammar_basic_parsing() {
        let _parser = MarigoldPestParser;
    }

    #[test]
    fn test_parser_empty_input() {
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse("");

        assert!(pest_result.is_ok());
        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("async"));
        assert!(pest_output.contains("use ::marigold::marigold_impl::*"));
    }

    #[test]
    fn test_factory_function_consistency() {
        let parser = get_parser();
        assert_eq!(parser.name(), "Pest");
    }

    #[test]
    fn test_parse_marigold_function_works() {
        let result = parse_marigold("");
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.contains("async"));
    }

    #[test]
    fn test_pest_grammar_rules_validation() {
        let pest_parser = PestParser::new();

        let valid_empty = pest_parser.parse("");
        assert!(valid_empty.is_ok(), "Empty input should be valid");

        let valid_range = pest_parser.parse("range(0, 1).return");
        assert!(valid_range.is_ok(), "range(0, 1).return should be valid");

        let invalid_syntax = pest_parser.parse("invalid syntax here!");
        assert!(invalid_syntax.is_err(), "Invalid syntax should be rejected");

        let invalid_partial = pest_parser.parse("range(0, 1)");
        assert!(
            invalid_partial.is_err(),
            "Incomplete stream should be rejected"
        );
    }

    #[test]
    fn test_parser_read_file_basic() {
        let input = r#"read_file("data.csv", csv, struct=Data).return"#;
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse read_file basic syntax"
        );

        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("csv_async::AsyncDeserializer"));
    }

    #[test]
    fn test_parser_read_file_gzip() {
        let input = r#"read_file("data.csv.gz", csv, struct=Data).return"#;
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse read_file with gzip compression"
        );

        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("GzipDecoder"));
    }

    #[test]
    fn test_parser_read_file_no_compression() {
        let input = r#"read_file("data.csv", csv, struct=Data, infer_compression=false).return"#;
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse read_file with infer_compression=false"
        );

        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("csv_async::AsyncDeserializer"));
        assert!(!pest_output.contains("GzipDecoder"));
    }

    #[test]
    fn test_parser_select_all_single() {
        let input = "select_all(range(0, 10)).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse select_all with single stream"
        );

        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("select_all"));
    }

    #[test]
    fn test_parser_select_all_multiple() {
        let input = "select_all(range(0, 10), range(10, 20)).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse select_all with multiple streams"
        );

        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("select_all"));
    }

    #[test]
    fn test_parser_select_all_with_stream_functions() {
        let input = "select_all(range(0, 10).map(double), range(10, 20)).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse select_all with stream functions"
        );

        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("select_all"));
        assert!(pest_output.contains("map"));
    }

    #[test]
    fn test_bounded_int_literal_bounds() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bounded_int_type, "int[0, 10]");
        assert!(result.is_ok(), "Should parse int[0, 10]");
    }

    #[test]
    fn test_bounded_int_negative_bound() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bounded_int_type, "int[-100, 100]");
        assert!(result.is_ok(), "Should parse int[-100, 100]");
    }

    #[test]
    fn test_bounded_uint_literal_bounds() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bounded_uint_type, "uint[0, 255]");
        assert!(result.is_ok(), "Should parse uint[0, 255]");
    }

    #[test]
    fn test_bounded_int_type_reference() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bounded_int_type, "int[0, MyEnum.len()]");
        assert!(result.is_ok(), "Should parse int[0, MyEnum.len()]");
    }

    #[test]
    fn test_bounded_uint_type_reference() {
        use pest::Parser;
        let result =
            MarigoldPestParser::parse(Rule::bounded_uint_type, "uint[0, Color.cardinality()]");
        assert!(result.is_ok(), "Should parse uint[0, Color.cardinality()]");
    }

    #[test]
    fn test_bounded_int_arithmetic_expression() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bounded_int_type, "int[0, MyEnum.len() - 1]");
        assert!(result.is_ok(), "Should parse int[0, MyEnum.len() - 1]");
    }

    #[test]
    fn test_bounded_int_complex_arithmetic() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bounded_int_type, "int[0, A.len() * 2 + 1]");
        assert!(result.is_ok(), "Should parse int[0, A.len() * 2 + 1]");
    }

    #[test]
    fn test_bound_expr_literal() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bound_expr, "42");
        assert!(result.is_ok(), "Should parse literal 42");
    }

    #[test]
    fn test_bound_expr_negative_literal() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bound_expr, "-5");
        assert!(result.is_ok(), "Should parse negative literal -5");
    }

    #[test]
    fn test_bound_expr_type_ref_len() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bound_expr, "MyEnum.len()");
        assert!(result.is_ok(), "Should parse MyEnum.len()");
    }

    #[test]
    fn test_bound_expr_type_ref_min() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bound_expr, "SomeType.min()");
        assert!(result.is_ok(), "Should parse SomeType.min()");
    }

    #[test]
    fn test_bound_expr_type_ref_max() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bound_expr, "OtherType.max()");
        assert!(result.is_ok(), "Should parse OtherType.max()");
    }

    #[test]
    fn test_bound_expr_parentheses() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(Rule::bound_expr, "(1 + 2) * 3");
        assert!(result.is_ok(), "Should parse (1 + 2) * 3");
    }

    #[test]
    fn test_bounded_type_validation_valid() {
        let input = r#"
            enum Color { Red = "r", Green = "g", Blue = "b" }
            struct Pixel { color_index: int[0, 2] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(
            result.is_ok(),
            "Valid bounded type should parse successfully"
        );
    }

    #[test]
    fn test_bounded_type_validation_with_enum_ref() {
        let input = r#"
            enum Color { Red = "r", Green = "g", Blue = "b" }
            struct Pixel { color_index: int[0, Color.len()] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(
            result.is_ok(),
            "Bounded type with enum reference should parse successfully"
        );
    }

    #[test]
    fn test_bounded_type_validation_min_greater_than_max() {
        let input = r#"
            struct Test { field: int[10, 5] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        eprintln!("Result: {:?}", result);
        assert!(result.is_err(), "min > max should fail validation");
        let err = result.unwrap_err();
        assert!(
            err.0.contains("min") && err.0.contains("max"),
            "Error should mention min and max: {}",
            err.0
        );
    }

    #[test]
    fn test_bounded_type_validation_undefined_type() {
        let input = r#"
            struct Test { field: int[0, NonExistent.len()] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_err(), "Undefined type reference should fail");
        let err = result.unwrap_err();
        assert!(
            err.0.contains("NonExistent"),
            "Error should mention undefined type: {}",
            err.0
        );
    }

    #[test]
    fn test_bounded_uint_negative_min() {
        let input = r#"
            struct Test { field: uint[-1, 10] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_err(), "Negative min for boundedUint should fail");
        let err = result.unwrap_err();
        assert!(
            err.0.contains("negative"),
            "Error should mention negative: {}",
            err.0
        );
    }

    #[test]
    fn test_bounded_type_codegen_nonneg_int_uses_unsigned() {
        let input = r#"
            struct Test { value: int[0, 100] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_ok(), "Should generate code successfully");
        let code = result.unwrap();
        assert!(
            code.contains("value: u8"),
            "int[0, 100] should generate u8 type, got: {}",
            code
        );
    }

    #[test]
    fn test_bounded_type_codegen_i16() {
        let input = r#"
            struct Test { value: int[-1000, 1000] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_ok(), "Should generate code successfully");
        let code = result.unwrap();
        assert!(
            code.contains("value: i16"),
            "int[-1000, 1000] should generate i16 type, got: {}",
            code
        );
    }

    #[test]
    fn test_bounded_type_codegen_u8() {
        let input = r#"
            struct Test { value: uint[0, 200] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_ok(), "Should generate code successfully");
        let code = result.unwrap();
        assert!(
            code.contains("value: u8"),
            "uint[0, 200] should generate u8 type, got: {}",
            code
        );
    }

    #[test]
    fn test_bounded_type_codegen_cardinality_constants() {
        let input = r#"
            struct Test { count: int[0, 10] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_ok(), "Should generate code successfully");
        let code = result.unwrap();
        assert!(
            code.contains("impl Test"),
            "Should generate impl block for Test"
        );
        assert!(
            code.contains("COUNT_MIN: u8 = 0"),
            "Should generate COUNT_MIN constant, got: {}",
            code
        );
        assert!(
            code.contains("COUNT_MAX: u8 = 10"),
            "Should generate COUNT_MAX constant, got: {}",
            code
        );
        assert!(
            code.contains("COUNT_CARDINALITY: u8 = 11"),
            "Should generate COUNT_CARDINALITY constant (11 = 10 - 0 + 1), got: {}",
            code
        );
    }

    #[test]
    fn test_bounded_type_codegen_with_enum_ref() {
        let input = r#"
            enum Color { Red = "r", Green = "g", Blue = "b" }
            struct Pixel { color_index: int[0, Color.len()] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_ok(), "Should generate code successfully");
        let code = result.unwrap();
        assert!(
            code.contains("color_index: u8"),
            "int[0, 3] should generate u8 type, got: {}",
            code
        );
        assert!(
            code.contains("COLOR_INDEX_CARDINALITY: u8 = 4"),
            "Should generate cardinality constant (4 = 3 - 0 + 1), got: {}",
            code
        );
    }

    #[test]
    fn test_bounded_type_struct_reference_error() {
        let input = r#"
            struct A { field: int[0, A.max()] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_err(), "Struct reference A.max() should fail");
        let err = result.unwrap_err();
        assert!(
            err.0.contains("Undefined") || err.0.contains("A"),
            "Error should mention undefined type: {}",
            err.0
        );
    }

    #[test]
    fn test_bounded_type_negative_range_codegen() {
        let input = r#"
            struct Temp { reading: int[-256, -1] }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(
            result.is_ok(),
            "Should generate code successfully, got: {:?}",
            result
        );
        let code = result.unwrap();
        assert!(
            code.contains("reading: i16"),
            "int[-256, -1] should generate i16 type, got: {}",
            code
        );
        assert!(
            code.contains("READING_MIN: i16 = -256"),
            "Should generate READING_MIN with i16 type, got: {}",
            code
        );
        assert!(
            code.contains("READING_MAX: i16 = -1"),
            "Should generate READING_MAX with i16 type, got: {}",
            code
        );
        assert!(
            code.contains("READING_CARDINALITY: u16 = 256"),
            "Should generate READING_CARDINALITY with u16 type, got: {}",
            code
        );
    }

    #[test]
    fn test_bounded_type_multiple_fields_codegen() {
        let input = r#"
            struct Data {
                x: int[-128, 127],
                y: uint[0, 1000]
            }
            range(0, 1).return
        "#;
        let result = parse_marigold(input);
        assert!(result.is_ok(), "Should generate code successfully");
        let code = result.unwrap();
        assert!(
            code.contains("x: i8"),
            "int[-128, 127] should generate i8 type"
        );
        assert!(
            code.contains("y: u16"),
            "uint[0, 1000] should generate u16 type"
        );
        assert!(
            code.contains("X_CARDINALITY: u16 = 256"),
            "Should generate X_CARDINALITY constant"
        );
        assert!(
            code.contains("Y_CARDINALITY: u16 = 1001"),
            "Should generate Y_CARDINALITY constant"
        );
    }
}

#[cfg(test)]
mod negative_tests {
    use super::*;

    // ===== Incomplete streams tests =====

    #[test]
    fn test_reject_incomplete_stream_no_output() {
        let result = parse_marigold("range(0, 100)");
        assert!(
            result.is_err(),
            "Should reject stream without output function (missing .return)"
        );
    }

    #[test]
    fn test_reject_incomplete_stream_output_only() {
        let result = parse_marigold(".return");
        assert!(
            result.is_err(),
            "Should reject output function without input stream"
        );
    }

    #[test]
    fn test_reject_incomplete_stream_function_only() {
        let result = parse_marigold(".map(x)");
        assert!(
            result.is_err(),
            "Should reject stream function without input or output"
        );
    }

    #[test]
    fn test_reject_incomplete_stream_map_without_return() {
        let result = parse_marigold("range(0, 10).map(double)");
        assert!(
            result.is_err(),
            "Should reject stream with map but no output function"
        );
    }

    #[test]
    fn test_reject_incomplete_stream_filter_without_return() {
        let result = parse_marigold("range(0, 10).filter(is_even)");
        assert!(
            result.is_err(),
            "Should reject stream with filter but no output function"
        );
    }

    // ===== Invalid struct/enum declarations tests =====

    #[test]
    fn test_reject_struct_without_name() {
        let result = parse_marigold("struct { x: i32 }");
        assert!(
            result.is_err(),
            "Should reject struct declaration without name"
        );
    }

    #[test]
    fn test_reject_enum_without_name() {
        let result = parse_marigold("enum { A, B }");
        assert!(
            result.is_err(),
            "Should reject enum declaration without name"
        );
    }

    #[test]
    fn test_reject_empty_struct() {
        let result = parse_marigold("struct Foo");
        assert!(
            result.is_err(),
            "Should reject struct without body (missing braces)"
        );
    }

    #[test]
    fn test_reject_empty_enum() {
        let result = parse_marigold("enum Bar");
        assert!(
            result.is_err(),
            "Should reject enum without body (missing braces)"
        );
    }

    // ===== Invalid function declarations tests =====

    #[test]
    fn test_reject_function_without_name() {
        let result = parse_marigold("fn (x: i32) -> i32 { x }");
        assert!(
            result.is_err(),
            "Should reject function declaration without name"
        );
    }

    #[test]
    fn test_reject_function_without_body() {
        let result = parse_marigold("fn foo(x: i32) -> i32");
        assert!(
            result.is_err(),
            "Should reject function declaration without body"
        );
    }

    // ===== Invalid stream operations tests =====

    #[test]
    fn test_reject_map_without_argument() {
        let result = parse_marigold("range(0, 10).map().return");
        assert!(
            result.is_err(),
            "Should reject .map() without transformation function argument"
        );
    }

    #[test]
    fn test_reject_filter_without_argument() {
        let result = parse_marigold("range(0, 10).filter().return");
        assert!(
            result.is_err(),
            "Should reject .filter() without predicate function argument"
        );
    }

    #[test]
    fn test_reject_multiple_consecutive_dots() {
        let result = parse_marigold("range(0, 10)..return");
        assert!(
            result.is_err(),
            "Should reject multiple consecutive dots (..)"
        );
    }

    #[test]
    fn test_reject_stream_ending_with_dot() {
        let result = parse_marigold("range(0, 10).");
        assert!(
            result.is_err(),
            "Should reject stream expression ending with dot"
        );
    }

    // ===== Malformed syntax tests =====

    #[test]
    fn test_reject_unclosed_parentheses() {
        let result = parse_marigold("range(0, 100.return");
        assert!(
            result.is_err(),
            "Should reject unclosed parentheses in function call"
        );
    }

    #[test]
    fn test_reject_missing_comma_in_arguments() {
        let result = parse_marigold("range(0 100).return");
        assert!(
            result.is_err(),
            "Should reject function arguments without comma separator"
        );
    }

    #[test]
    fn test_reject_triple_dot() {
        let result = parse_marigold("range(0, 100)...return");
        assert!(
            result.is_err(),
            "Should reject triple dot (...) as invalid syntax"
        );
    }

    #[test]
    fn test_reject_mismatched_parentheses() {
        let result = parse_marigold("range(0, 100)).return");
        assert!(
            result.is_err(),
            "Should reject mismatched closing parentheses"
        );
    }

    #[test]
    fn test_reject_invalid_characters_at_start() {
        let result = parse_marigold("@range(0, 100).return");
        assert!(
            result.is_err(),
            "Should reject invalid character (@) at start of expression"
        );
    }

    #[test]
    fn test_reject_invalid_characters_in_middle() {
        let result = parse_marigold("range(0, 100).#map(double).return");
        assert!(
            result.is_err(),
            "Should reject invalid character (#) in stream chain"
        );
    }

    // ===== Edge cases and boundary conditions =====

    #[test]
    fn test_reject_incomplete_range_single_arg() {
        let result = parse_marigold("range(10).return");
        assert!(
            result.is_err(),
            "Should reject range() with single argument (grammar requires two)"
        );
    }

    #[test]
    fn test_reject_range_no_args() {
        let result = parse_marigold("range().return");
        assert!(result.is_err(), "Should reject range() with no arguments");
    }

    #[test]
    fn test_reject_return_with_args() {
        let result = parse_marigold("range(0, 10).return(123)");
        assert!(
            result.is_err(),
            "Should reject .return with arguments (not supported)"
        );
    }

    #[test]
    fn test_reject_stream_starting_with_operation() {
        let result = parse_marigold(".map(double).return");
        assert!(
            result.is_err(),
            "Should reject stream starting with operation instead of input"
        );
    }

    #[test]
    fn test_reject_semicolon_in_stream_chain() {
        let result = parse_marigold("range(0, 10); .return");
        assert!(
            result.is_err(),
            "Should reject semicolon separating stream chain"
        );
    }

    // ===== Invalid identifier tests (validates free_text_identifier split) =====

    #[test]
    fn test_reject_function_name_starting_with_digit() {
        let result = parse_marigold("fn 123func(x: i32) -> i32 { x }");
        assert!(
            result.is_err(),
            "Should reject function name starting with digit (free_text_identifier validation)"
        );
    }

    #[test]
    fn test_reject_struct_name_starting_with_digit() {
        let result = parse_marigold("struct 123Point { x: i32 }");
        assert!(
            result.is_err(),
            "Should reject struct name starting with digit"
        );
    }

    #[test]
    fn test_reject_enum_name_starting_with_digit() {
        let result = parse_marigold("enum 456Color { Red, Green }");
        assert!(
            result.is_err(),
            "Should reject enum name starting with digit"
        );
    }

    #[test]
    fn test_reject_map_function_name_starting_with_digit() {
        let result = parse_marigold("range(0, 10).map(123transform).return");
        assert!(
            result.is_err(),
            "Should reject map() function name starting with digit"
        );
    }

    #[test]
    fn test_reject_filter_function_name_starting_with_digit() {
        let result = parse_marigold("range(0, 10).filter(789predicate).return");
        assert!(
            result.is_err(),
            "Should reject filter() function name starting with digit"
        );
    }

    // ===== Valid cases that should pass (regression tests) =====

    #[test]
    fn test_accept_numeric_range_arguments() {
        let result = parse_marigold("range(0, 100).return");
        assert!(
            result.is_ok(),
            "Should accept numeric arguments in range() (free_text_literal validation)"
        );
    }

    #[test]
    fn test_accept_numeric_permutations_argument() {
        let result = parse_marigold("range(0, 10).permutations(3).return");
        assert!(
            result.is_ok(),
            "Should accept numeric argument in permutations()"
        );
    }

    #[test]
    fn test_accept_numeric_combinations_argument() {
        let result = parse_marigold("range(0, 10).combinations(2).return");
        assert!(
            result.is_ok(),
            "Should accept numeric argument in combinations()"
        );
    }

    #[test]
    fn test_accept_valid_function_names() {
        let result = parse_marigold("range(0, 10).map(transform).filter(is_even).return");
        assert!(
            result.is_ok(),
            "Should accept valid identifier function names"
        );
    }

    #[test]
    fn test_accept_underscore_prefix() {
        let result = parse_marigold("range(0, 10).map(_private_func).return");
        assert!(
            result.is_ok(),
            "Should accept identifiers starting with underscore"
        );
    }

    // ===== Invalid read_file tests =====

    #[test]
    fn test_reject_read_file_missing_file_path() {
        let result = parse_marigold(r#"read_file(csv, struct=Data).return"#);
        assert!(result.is_err(), "Should reject read_file without file path");
    }

    #[test]
    fn test_reject_read_file_missing_struct() {
        let result = parse_marigold(r#"read_file("data.csv", csv).return"#);
        assert!(
            result.is_err(),
            "Should reject read_file without struct parameter"
        );
    }

    #[test]
    fn test_reject_read_file_unquoted_path() {
        let result = parse_marigold(r#"read_file(data.csv, csv, struct=Data).return"#);
        assert!(
            result.is_err(),
            "Should reject read_file with unquoted file path"
        );
    }

    #[test]
    fn test_reject_read_file_wrong_format() {
        let result = parse_marigold(r#"read_file("data.csv", json, struct=Data).return"#);
        assert!(
            result.is_err(),
            "Should reject read_file with unsupported format (only csv is supported)"
        );
    }

    #[test]
    fn test_reject_read_file_missing_struct_name() {
        let result = parse_marigold(r#"read_file("data.csv", csv, struct=).return"#);
        assert!(
            result.is_err(),
            "Should reject read_file with empty struct name"
        );
    }

    #[test]
    fn test_reject_read_file_invalid_compression_value() {
        let result = parse_marigold(
            r#"read_file("data.csv", csv, struct=Data, infer_compression=maybe).return"#,
        );
        assert!(
            result.is_err(),
            "Should reject read_file with invalid infer_compression value (must be true or false)"
        );
    }

    #[test]
    fn test_reject_read_file_missing_closing_paren() {
        let result = parse_marigold(r#"read_file("data.csv", csv, struct=Data.return"#);
        assert!(
            result.is_err(),
            "Should reject read_file with missing closing parenthesis"
        );
    }

    // ===== Invalid select_all tests =====

    #[test]
    fn test_reject_select_all_no_args() {
        let result = parse_marigold("select_all().return");
        assert!(
            result.is_err(),
            "Should reject select_all with no arguments"
        );
    }

    #[test]
    fn test_reject_select_all_incomplete_stream() {
        let result = parse_marigold("select_all(range(0, 10), .map(double)).return");
        assert!(
            result.is_err(),
            "Should reject select_all with incomplete stream (missing input)"
        );
    }

    #[test]
    fn test_reject_select_all_trailing_comma() {
        let result = parse_marigold("select_all(range(0, 10),).return");
        assert!(
            result.is_err(),
            "Should reject select_all with trailing comma"
        );
    }

    #[test]
    fn test_reject_select_all_double_comma() {
        let result = parse_marigold("select_all(range(0, 10),, range(10, 20)).return");
        assert!(
            result.is_err(),
            "Should reject select_all with double comma"
        );
    }

    #[test]
    fn test_reject_select_all_missing_closing_paren() {
        let result = parse_marigold("select_all(range(0, 10).return");
        assert!(
            result.is_err(),
            "Should reject select_all with missing closing parenthesis"
        );
    }

    #[test]
    fn test_reject_select_all_invalid_input_function() {
        let result = parse_marigold("select_all(invalid_func(0, 10)).return");
        assert!(
            result.is_err(),
            "Should reject select_all with invalid input function"
        );
    }
}

#[cfg(test)]
mod struct_enum_tests {
    use super::*;

    #[test]
    fn test_struct_simple_fields() {
        let result = parse_marigold("struct Foo { x: i32, y: u64 }");
        assert!(result.is_ok(), "Should parse struct with simple fields");
        let output = result.unwrap();
        assert!(output.contains("struct Foo"));
        assert!(output.contains("x: i32"));
        assert!(output.contains("y: u64"));
    }

    #[test]
    fn test_struct_generic_fields() {
        let result = parse_marigold("struct Bar { data: Vec<String> }");
        assert!(result.is_ok(), "Should parse struct with generic fields");
        let output = result.unwrap();
        assert!(output.contains("struct Bar"));
    }

    #[test]
    fn test_struct_nested_generics() {
        let result = parse_marigold("struct Baz { map: HashMap<String, Vec<i32>> }");
        assert!(
            result.is_ok(),
            "Should parse struct with nested generic fields"
        );
        let output = result.unwrap();
        assert!(output.contains("struct Baz"));
    }

    #[test]
    fn test_struct_string_n_type() {
        let result = parse_marigold("struct Qux { name: string_64 }");
        assert!(result.is_ok(), "Should parse struct with string_N type");
        let output = result.unwrap();
        assert!(output.contains("struct Qux"));
        assert!(output.contains("ArrayString<64>"));
    }

    #[test]
    fn test_struct_option_field() {
        let result = parse_marigold("struct Opt { value: Option<i32> }");
        assert!(result.is_ok(), "Should parse struct with Option field");
        let output = result.unwrap();
        assert!(output.contains("struct Opt"));
        assert!(output.contains("Option<i32>"));
    }

    #[test]
    fn test_struct_trailing_comma() {
        let result = parse_marigold("struct Trail { x: i32, }");
        assert!(result.is_ok(), "Should parse struct with trailing comma");
    }

    #[test]
    fn test_enum_with_values() {
        let result = parse_marigold(r#"enum Status { Active = "active", Inactive = "inactive" }"#);
        assert!(result.is_ok(), "Should parse enum with values");
        let output = result.unwrap();
        assert!(output.contains("enum Status"));
        assert!(output.contains("Active"));
        assert!(output.contains("Inactive"));
    }

    #[test]
    fn test_enum_default_variant() {
        let result = parse_marigold(r#"enum E { A = "a", default Unknown }"#);
        assert!(result.is_ok(), "Should parse enum with default variant");
    }

    #[test]
    fn test_enum_default_with_type() {
        let result = parse_marigold(r#"enum E { A = "a", default Other(string_10) }"#);
        assert!(
            result.is_ok(),
            "Should parse enum with default variant with type: {:?}",
            result
        );
        let output = result.unwrap();
        assert!(output.contains("enum E"));
        assert!(output.contains("Other"));
    }

    #[test]
    fn test_enum_default_with_value() {
        let result = parse_marigold(r#"enum E { A = "a", default Unknown = "unknown" }"#);
        assert!(
            result.is_ok(),
            "Should parse enum with default variant with value"
        );
    }

    #[test]
    fn test_struct_and_stream() {
        let result = parse_marigold(
            r#"struct Ship { class: string_8, hull: string_8 }
            range(0, 10).return"#,
        );
        assert!(result.is_ok(), "Should parse struct followed by stream");
    }

    #[test]
    fn test_enum_and_struct_together() {
        let result = parse_marigold(
            r#"enum Hull { Spherical = "spherical", Split = "split" }
            struct Vaisseau { class: string_8, hull: Hull }"#,
        );
        assert!(result.is_ok(), "Should parse enum and struct together");
    }
}

#[cfg(test)]
mod function_grammar_tests {
    use super::*;

    #[test]
    fn test_fn_simple_body() {
        let result = parse_marigold("fn double(x: i32) -> i32 { x * 2 }");
        assert!(result.is_ok(), "Should parse simple function body");
        let output = result.unwrap();
        assert!(output.contains("const fn double"));
        assert!(output.contains("x * 2"));
    }

    #[test]
    fn test_fn_nested_braces() {
        let result = parse_marigold("fn make_struct(x: i32) -> Foo { Foo { value: x } }");
        assert!(result.is_ok(), "Should parse function with nested braces");
        let output = result.unwrap();
        assert!(output.contains("Foo { value: x }"));
    }

    #[test]
    fn test_fn_deeply_nested_braces() {
        let result = parse_marigold(
            r#"fn complex(x: i32) -> Bar {
                if x > 0 {
                    Bar { inner: Baz { value: x } }
                } else {
                    Bar { inner: Baz { value: 0 } }
                }
            }"#,
        );
        assert!(
            result.is_ok(),
            "Should parse function with deeply nested braces"
        );
    }

    #[test]
    fn test_fn_string_with_braces() {
        let result = parse_marigold(r#"fn get_json(x: i32) -> String { format!("{{ }}", x) }"#);
        assert!(
            result.is_ok(),
            "Should parse function with braces in string literal"
        );
    }

    #[test]
    fn test_fn_char_literal_brace() {
        let result = parse_marigold("fn get_open_brace() -> char { '{' }");
        assert!(
            result.is_ok(),
            "Should parse function with brace char literal"
        );

        let result2 = parse_marigold("fn get_close_brace() -> char { '}' }");
        assert!(
            result2.is_ok(),
            "Should parse function with close brace char literal"
        );
    }

    #[test]
    fn test_fn_with_line_comment() {
        let result = parse_marigold(
            r#"fn commented(x: i32) -> i32 {
                // This { is in a comment
                x + 1
            }"#,
        );
        assert!(
            result.is_ok(),
            "Should parse function with line comment containing brace"
        );
    }

    #[test]
    fn test_fn_with_block_comment() {
        let result = parse_marigold(
            r#"fn block_commented(x: i32) -> i32 {
                /* This { is in a block comment */
                x + 1
            }"#,
        );
        assert!(
            result.is_ok(),
            "Should parse function with block comment containing brace"
        );
    }

    #[test]
    fn test_fn_multiple_functions() {
        let result = parse_marigold(
            r#"fn first(x: i32) -> i32 { x + 1 }
               fn second(y: i32) -> i32 { y * 2 }"#,
        );
        assert!(result.is_ok(), "Should parse multiple functions");
        let output = result.unwrap();
        assert!(output.contains("const fn first"));
        assert!(output.contains("const fn second"));
    }

    #[test]
    fn test_fn_with_reference_params() {
        let result = parse_marigold("fn process(data: &Data) -> bool { data.valid }");
        assert!(
            result.is_ok(),
            "Should parse function with reference parameter"
        );
        let output = result.unwrap();
        assert!(output.contains("data: &Data"));
    }

    #[test]
    fn test_fn_generic_return_type() {
        let result = parse_marigold("fn maybe(x: i32) -> Option<i32> { Some(x) }");
        assert!(
            result.is_ok(),
            "Should parse function with generic return type"
        );
        let output = result.unwrap();
        assert!(output.contains("Option<i32>"));
    }

    #[test]
    fn test_fn_empty_params() {
        let result = parse_marigold("fn zero() -> i32 { 0 }");
        assert!(result.is_ok(), "Should parse function with no parameters");
    }

    #[test]
    fn test_fn_with_escaped_string() {
        let result = parse_marigold(r#"fn escaped() -> String { "hello \"world\"".to_string() }"#);
        assert!(
            result.is_ok(),
            "Should parse function with escaped quotes in string"
        );
    }

    #[test]
    fn test_fn_with_stream() {
        let result = parse_marigold(
            r#"fn double(x: i32) -> i32 { x * 2 }
               range(0, 10).map(double).return"#,
        );
        assert!(
            result.is_ok(),
            "Should parse function declaration with stream"
        );
        let output = result.unwrap();
        assert!(output.contains("const fn double"));
        assert!(output.contains("map") && output.contains("double"));
    }

    #[test]
    fn test_fn_with_string_n_param() {
        let result = parse_marigold("fn greet(name: string_20) -> string_20 { name }");
        assert!(
            result.is_ok(),
            "Should parse function with string_N types: {:?}",
            result
        );
        let output = result.unwrap();
        assert!(
            output.contains("ArrayString<20>"),
            "string_20 should be translated to ArrayString<20>, got: {}",
            output
        );
        assert!(
            !output.contains("string_20"),
            "Raw string_20 should not appear in output, got: {}",
            output
        );
    }

    #[test]
    fn test_fn_with_string_n_ref_param() {
        let result = parse_marigold("fn check(s: &string_64) -> bool { s.len() > 0 }");
        assert!(
            result.is_ok(),
            "Should parse function with &string_N param: {:?}",
            result
        );
        let output = result.unwrap();
        assert!(
            output.contains("&::marigold::marigold_impl::arrayvec::ArrayString<64>"),
            "&string_64 should be translated to &ArrayString<64>, got: {}",
            output
        );
    }
}
