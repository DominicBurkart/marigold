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
            .map_err(|e| MarigoldParseError(format!("Parse error: {e}")))?;
        let expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)
            .map_err(|e| MarigoldParseError(format!("AST build error: {e}")))?;
        Ok(crate::complexity::analyze_program(&expressions))
    }
}

impl Default for PestParser {
    fn default() -> Self {
        Self::new()
    }
}

impl MarigoldParser for PestParser {
    fn parse(&self, input: &str) -> Result<String, MarigoldParseError> {
        let pairs = MarigoldPestParser::parse(Rule::program, input)
            .map_err(|e| MarigoldParseError(format!("{e}")))?;

        let expressions = crate::pest_ast_builder::PestAstBuilder::build_program(pairs)
            .map_err(|e| MarigoldParseError(format!("AST build error: {e}")))?;

        let mut code_parts = Vec::new();
        for expr in &expressions {
            let code = match expr {
                crate::nodes::TypedExpression::UnnamedReturningStream(s) => s.code(),
                crate::nodes::TypedExpression::UnnamedNonReturningStream(s) => s.code(),
                crate::nodes::TypedExpression::StreamVariable(v) => v.declaration_code(),
                crate::nodes::TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                    v.declaration_code()
                }
                crate::nodes::TypedExpression::NamedReturningStream(s) => s.code(),
                crate::nodes::TypedExpression::NamedNonReturningStream(s) => s.code(),
                crate::nodes::TypedExpression::StructDeclaration(s) => s.code(),
                crate::nodes::TypedExpression::EnumDeclaration(e) => e.code(),
                crate::nodes::TypedExpression::FnDeclaration(f) => f.code(),
            };
            code_parts.push(code);
        }

        // Handle multi-expression programs with stream variable runners
        let mut runner_parts = Vec::new();
        for expr in &expressions {
            match expr {
                crate::nodes::TypedExpression::StreamVariable(v) => {
                    runner_parts.push(v.runner_code());
                }
                crate::nodes::TypedExpression::StreamVariableFromPriorStreamVariable(v) => {
                    runner_parts.push(v.runner_code());
                }
                _ => {}
            }
        }

        if runner_parts.is_empty() {
            Ok(code_parts.join("\n"))
        } else {
            // For programs with stream variables, we need to combine declaration and runner code
            let declarations = code_parts.join("\n");
            let runners = runner_parts.join("\n");
            Ok(format!("{declarations}\n{runners}"))
        }
    }

    fn name(&self) -> &'static str {
        "pest"
    }
}

/// Parse a Marigold program string and return the generated Rust code.
///
/// This is a convenience function that creates a `PestParser` and calls `parse`.
pub fn parse_marigold(input: &str) -> Result<String, MarigoldParseError> {
    PestParser::new().parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_range_return() {
        let input = "range(0, 10).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse range return: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_map_fn() {
        let input = "range(0, 3).map(double).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse map: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_filter_fn() {
        let input = "range(0, 10).filter(is_odd).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse filter: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_struct_declaration() {
        let input = r#"struct Cat { name: string_50, age: u8 }"#;
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse struct declaration: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_fn_declaration() {
        let input = "fn double(x: i32) -> i32 { x * 2 }";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse fn declaration: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_permutations() {
        let input = "range(0, 3).permutations(2).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse permutations: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_permutations_with_replacement() {
        let input = "range(0, 3).permutations_with_replacement(2).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse permutations_with_replacement: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_combinations() {
        let input = "range(0, 5).combinations(3).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse combinations: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_keep_first_n() {
        let input = "range(0, 5).keep_first_n(3, my_sort_fn).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse keep_first_n: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_fold() {
        let input = "range(0, 5).fold(0, add).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse fold: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_chained_stream_functions() {
        let input = "range(0, 10).filter(is_odd).map(double).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse chained stream functions: {:?}",
            pest_result.err()
        );
    }

    #[test]
    fn test_parse_map_generates_map_code() {
        let input = "range(0, 3).map(double).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(pest_result.is_ok());
        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("map"));
    }

    #[test]
    fn test_parser_chain_basic() {
        let input = "range(0, 3).chain(range(10, 13)).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse chain: {:?}",
            pest_result.err()
        );

        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("chain"));
    }

    #[test]
    fn test_parser_chain_with_stream_functions() {
        let input = "range(0, 3).chain(range(10, 20).map(double)).return";
        let pest_parser = PestParser::new();
        let pest_result = pest_parser.parse(input);

        assert!(
            pest_result.is_ok(),
            "Pest should parse chain with stream functions in argument: {:?}",
            pest_result.err()
        );

        let pest_output = pest_result.unwrap();
        assert!(pest_output.contains("chain"));
    }

    #[test]
    fn test_parser_chain_no_argument_fails() {
        use pest::Parser;
        // chain() with no argument should fail to parse at the grammar level
        let result = MarigoldPestParser::parse(Rule::chain_fn, "chain()");
        assert!(
            result.is_err(),
            "chain() with no argument should fail to parse"
        );
    }

    #[test]
    fn test_parser_chain_literal_argument_fails() {
        use pest::Parser;
        // chain(42) with a non-stream literal argument should fail to parse
        let result = MarigoldPestParser::parse(Rule::chain_fn, "chain(42)");
        assert!(
            result.is_err(),
            "chain(42) with a non-stream argument should fail to parse"
        );
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
        let result = MarigoldPestParser::parse(
            Rule::bounded_int_type,
            "int[0, MyEnum.len() * OtherEnum.len()]",
        );
        assert!(
            result.is_ok(),
            "Should parse int[0, MyEnum.len() * OtherEnum.len()]"
        );
    }

    #[test]
    fn test_bounded_int_nested_arithmetic() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(
            Rule::bounded_int_type,
            "int[0, (MyEnum.len() + 1) * 2]",
        );
        assert!(
            result.is_ok(),
            "Should parse int[0, (MyEnum.len() + 1) * 2]"
        );
    }

    #[test]
    fn test_bounded_int_min_max_references() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(
            Rule::bounded_int_type,
            "int[MyType.min(), MyType.max()]",
        );
        assert!(
            result.is_ok(),
            "Should parse int[MyType.min(), MyType.max()]"
        );
    }

    #[test]
    fn test_bounded_uint_cardinality_reference() {
        use pest::Parser;
        let result = MarigoldPestParser::parse(
            Rule::bounded_uint_type,
            "uint[0, MyEnum.cardinality()]",
        );
        assert!(
            result.is_ok(),
            "Should parse uint[0, MyEnum.cardinality()]"
        );
    }

    #[test]
    fn test_bounded_int_struct_field() {
        let input = "struct Foo { x: int[0, 10] }";
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(result.is_ok(), "Should parse struct with bounded int field");
    }

    #[test]
    fn test_bounded_uint_struct_field() {
        let input = "struct Foo { x: uint[0, 255] }";
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(result.is_ok(), "Should parse struct with bounded uint field");
    }

    #[test]
    fn test_bounded_int_struct_with_enum_reference() {
        let input = "struct Foo { x: int[0, MyEnum.len()] }";
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(
            result.is_ok(),
            "Should parse struct with bounded int referencing enum"
        );
    }

    #[test]
    fn test_bounded_int_struct_with_arithmetic() {
        let input = "struct Foo { x: int[0, MyEnum.len() * 2] }";
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(
            result.is_ok(),
            "Should parse struct with bounded int with arithmetic"
        );
    }

    #[test]
    fn test_bounded_int_complex_arithmetic_parsing() {
        let input = "struct Foo { x: int[SomeEnum.min(), SomeEnum.max()] }";
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(
            result.is_ok(),
            "Should parse struct with bounded int using min/max references"
        );
    }

    #[test]
    fn test_bounded_uint_with_cardinality_reference() {
        let input = "struct Foo { x: uint[0, MyEnum.cardinality()] }";
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(
            result.is_ok(),
            "Should parse struct with bounded uint using cardinality reference"
        );
    }

    #[test]
    fn test_parse_program_with_fn_and_stream() {
        let input = "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 5).map(double).return";
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(
            result.is_ok(),
            "Should parse program with fn and stream: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_parse_complex_program() {
        let input = r#"fn is_big(x: i32) -> bool { x > 5 }
range(0, 10).filter(is_big).return"#;
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(
            result.is_ok(),
            "Should parse complex program: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_fn_declaration_generates_const_fn() {
        let input = "fn add(a: i32, b: i32) -> i32 { a + b }";
        let pest_parser = PestParser::new();
        let result = pest_parser.parse(input);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("const fn add"), "should generate const fn");
        assert!(code.contains("a: i32"), "param a: i32 should be present");
        assert!(code.contains("b: i32"), "param b: i32 should be present");
        assert!(code.contains("-> i32"), "return type i32 should be present");
    }
}
