#![forbid(unsafe_code)]

//! # Marigold Grammar Library
//!
//! This crate provides the grammar and parser infrastructure for the Marigold DSL.
//!
//! ## Overview
//!
//! Marigold is a domain-specific language for expressing stream processing programs in Rust.
//! This library provides:
//!
//! - **Pest-based parser**: Fast, maintainable PEG parser
//! - **AST definitions**: Complete abstract syntax tree for Marigold programs
//! - **Code generation**: Transforms Marigold programs into valid Rust code
//!
//! ## Quick Start
//!
//! Parse a Marigold program:
//!
//! ```ignore
//! use marigold_grammar::parser::parse_marigold;
//!
//! let code = parse_marigold("range(0, 100).return")?;
//! println!("{}", code);
//! ```
//!
//! ## Architecture
//!
//! ### Parser
//!
//! The [`parser`] module provides the Pest-based parser with a trait abstraction
//! for extensibility. The factory function [`parser::get_parser()`] returns a
//! parser instance.
//!
//! ### Grammar File
//!
//! - **Pest**: `src/marigold.pest` - Defines the complete Marigold language grammar
//!
//! ### AST and Code Generation
//!
//! - [`nodes`]: AST node definitions for all Marigold constructs
//! - Code generation: Internal function that transforms AST to Rust code
//! - [`pest_ast_builder`]: Transforms Pest parse trees into the AST
//!
//! ## Testing & Validation
//!
//! The parser implementation is validated through:
//!
//! - **Unit tests** in each module covering specific functionality
//! - **Negative tests** ensuring invalid syntax is properly rejected
//! - **Integration tests** using real-world example programs
//!
//! ## Feature Flags
//!
//! - `io`: I/O features (available in other crates)
//! - `tokio`: Tokio runtime integration (available in other crates)
//! - `async-std`: async-std runtime integration (available in other crates)
//!
//! ## Performance Characteristics
//!
//! - **Parsing**: Completes in < 1ms for typical programs
//! - **Code generation**: Dominates runtime, scales with program complexity
//! - **Binary size**: Pest parser adds ~40KB to binary size

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Convenience function for parsing Marigold code
///
/// This is an alias for [`parser::parse_marigold`] that uses the appropriate parser
/// based on feature flags. It's the recommended entry point for most use cases.
///
/// # Examples
///
/// ```ignore
/// use marigold_grammar::marigold_parse;
///
/// let code = marigold_parse("range(0, 100).return")?;
/// // code is now valid Rust code ready to compile
/// ```
pub fn marigold_parse(s: &str) -> Result<String, parser::MarigoldParseError> {
    parser::parse_marigold(s)
}

pub fn marigold_analyze(
    s: &str,
) -> Result<complexity::ProgramComplexity, parser::MarigoldParseError> {
    parser::PestParser::analyze(s)
}

#[cfg(test)]
mod tests {
    use super::{marigold_analyze, marigold_parse};
    use crate::complexity::{Cardinality, ComplexityClass};
    use num_bigint::BigUint;

    // -----------------------------------------------------------------------
    // marigold_parse: success cases
    // -----------------------------------------------------------------------

    #[test]
    fn parse_simple_range_return() {
        let result = marigold_parse("range(0, 10).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
        let code = result.unwrap();
        // Generated code should be an async block
        assert!(code.contains("async"), "Expected 'async' in generated code");
    }

    #[test]
    fn parse_range_with_map() {
        let result =
            marigold_parse("fn double(v: i32) -> i32 { v * 2 } range(0, 3).map(double).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
        let code = result.unwrap();
        assert!(code.contains("async"));
    }

    #[test]
    fn parse_permutations() {
        let result = marigold_parse("range(0, 2).permutations(2).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    #[test]
    fn parse_combinations() {
        let result = marigold_parse("range(0, 4).combinations(2).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    #[test]
    fn parse_filter() {
        let result = marigold_parse(
            "fn is_odd(v: i32) -> bool { v % 2 == 1 } range(0, 10).filter(is_odd).return",
        );
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    #[test]
    fn parse_inclusive_range() {
        let result = marigold_parse("range(0, =5).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    #[test]
    fn parse_select_all() {
        let result = marigold_parse("select_all(range(0, 3), range(10, 13)).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    #[test]
    fn parse_enum_range() {
        let result = marigold_parse("enum Words { Hello, World, } range(Words).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    #[test]
    fn parse_fold() {
        let result = marigold_parse(
            "fn add(acc: i32, v: i32) -> i32 { acc + v } range(0, 5).fold(0, add).return",
        );
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    #[test]
    fn parse_chained_map_filter() {
        let result = marigold_parse(
            "fn double(v: i32) -> i32 { v * 2 } \
             fn is_big(v: i32) -> bool { v > 4 } \
             range(0, 5).map(double).filter(is_big).return",
        );
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    #[test]
    fn parse_permutations_with_replacement() {
        let result = marigold_parse("range(0, 3).permutations_with_replacement(2).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    }

    // -----------------------------------------------------------------------
    // marigold_parse: generated code sanity checks
    // -----------------------------------------------------------------------

    #[test]
    fn parse_output_contains_select_all() {
        // The generated code uses select_all to merge streams before returning
        let code = marigold_parse("range(0, 10).return").unwrap();
        assert!(
            code.contains("select_all"),
            "Generated code should contain 'select_all', got: {}",
            code
        );
    }

    #[test]
    fn parse_output_contains_marigold_impl() {
        let code = marigold_parse("range(0, 10).return").unwrap();
        assert!(
            code.contains("marigold"),
            "Generated code should reference marigold"
        );
    }

    #[test]
    fn parse_output_is_nonempty() {
        let code = marigold_parse("range(0, 5).return").unwrap();
        assert!(!code.is_empty(), "Generated code should not be empty");
    }

    // -----------------------------------------------------------------------
    // marigold_parse: failure cases
    // -----------------------------------------------------------------------

    #[test]
    fn parse_invalid_syntax_returns_error() {
        let result = marigold_parse("invalid syntax $$$");
        assert!(result.is_err(), "Expected Err for invalid syntax");
    }

    #[test]
    fn parse_empty_string_succeeds() {
        // An empty marigold program is valid (produces an empty stream program)
        let result = marigold_parse("");
        assert!(
            result.is_ok(),
            "Empty program should parse successfully, got: {:?}",
            result
        );
    }

    #[test]
    fn parse_missing_return_returns_error() {
        // A range without .return is not a complete program
        let result = marigold_parse("range(0, 10)");
        assert!(result.is_err(), "Expected Err for program without .return");
    }

    #[test]
    fn parse_unknown_enum_returns_error() {
        // Reference to an undeclared enum should fail
        let result = marigold_parse("range(UndeclaredEnum).return");
        assert!(
            result.is_err(),
            "Expected Err when referencing undeclared enum"
        );
    }

    // -----------------------------------------------------------------------
    // marigold_analyze: success cases
    // -----------------------------------------------------------------------

    #[test]
    fn analyze_simple_range() {
        let result = marigold_analyze("range(0, 10).return");
        assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
        let complexity = result.unwrap();
        // A simple range should produce exactly 1 stream
        assert_eq!(complexity.streams.len(), 1);
    }

    #[test]
    fn analyze_range_cardinality_is_exact() {
        let complexity = marigold_analyze("range(0, 10).return").unwrap();
        let cardinality = &complexity.streams[0].cardinality;
        match cardinality {
            Cardinality::Exact(n) => {
                assert_eq!(
                    *n,
                    BigUint::from(10u64),
                    "range(0,10) should have cardinality 10"
                )
            }
            _ => panic!(
                "Expected Exact cardinality for range(0,10), got {:?}",
                cardinality
            ),
        }
    }

    #[test]
    fn analyze_range_time_complexity() {
        let complexity = marigold_analyze("range(0, 10).return").unwrap();
        // A simple range without transformations has O(1) time (constant, evaluable)
        assert_eq!(complexity.program_time, ComplexityClass::O1);
    }

    #[test]
    fn analyze_filter_produces_bounded_cardinality() {
        let complexity = marigold_analyze("range(0, 100).filter(is_even).return").unwrap();
        let cardinality = &complexity.streams[0].cardinality;
        // Filter produces bounded cardinality (cannot be exact without running the filter)
        assert!(
            matches!(cardinality, Cardinality::Bounded(_)),
            "Expected Bounded cardinality after filter, got {:?}",
            cardinality
        );
    }

    #[test]
    fn analyze_map_preserves_cardinality() {
        let complexity = marigold_analyze("range(0, 5).map(identity).return").unwrap();
        let cardinality = &complexity.streams[0].cardinality;
        // map preserves cardinality exactly
        match cardinality {
            Cardinality::Exact(n) => {
                assert_eq!(
                    *n,
                    BigUint::from(5u64),
                    "map should preserve cardinality of 5"
                )
            }
            _ => panic!(
                "Expected Exact cardinality after map, got {:?}",
                cardinality
            ),
        }
    }

    #[test]
    fn analyze_combinations_cardinality() {
        // C(4, 2) = 6
        let complexity = marigold_analyze("range(0, 4).combinations(2).return").unwrap();
        let cardinality = &complexity.streams[0].cardinality;
        match cardinality {
            Cardinality::Exact(n) => {
                assert_eq!(*n, BigUint::from(6u64), "C(4,2) should be 6")
            }
            _ => panic!(
                "Expected Exact cardinality for combinations, got {:?}",
                cardinality
            ),
        }
    }

    #[test]
    fn analyze_permutations_cardinality() {
        // P(4, 2) = 4 * 3 = 12
        let complexity = marigold_analyze("range(0, 4).permutations(2).return").unwrap();
        let cardinality = &complexity.streams[0].cardinality;
        match cardinality {
            Cardinality::Exact(n) => {
                assert_eq!(*n, BigUint::from(12u64), "P(4,2) should be 12")
            }
            _ => panic!(
                "Expected Exact cardinality for permutations, got {:?}",
                cardinality
            ),
        }
    }

    #[test]
    fn analyze_fold_cardinality_is_one() {
        let complexity = marigold_analyze("range(0, 100).fold(0, add).return").unwrap();
        let cardinality = &complexity.streams[0].cardinality;
        match cardinality {
            Cardinality::Exact(n) => {
                assert_eq!(
                    *n,
                    BigUint::from(1u64),
                    "fold always produces exactly 1 item"
                )
            }
            _ => panic!(
                "Expected Exact(1) cardinality for fold, got {:?}",
                cardinality
            ),
        }
    }

    #[test]
    fn analyze_inclusive_range_cardinality() {
        // range(0, =4) -> 0,1,2,3,4 -> 5 items
        let complexity = marigold_analyze("range(0, =4).return").unwrap();
        let cardinality = &complexity.streams[0].cardinality;
        match cardinality {
            Cardinality::Exact(n) => {
                assert_eq!(
                    *n,
                    BigUint::from(5u64),
                    "range(0,=4) should have cardinality 5"
                )
            }
            _ => panic!(
                "Expected Exact cardinality for inclusive range, got {:?}",
                cardinality
            ),
        }
    }

    #[test]
    fn analyze_enum_range() {
        let complexity =
            marigold_analyze("enum Words { Hello, World, } range(Words).return").unwrap();
        assert_eq!(complexity.streams.len(), 1);
        let cardinality = &complexity.streams[0].cardinality;
        match cardinality {
            Cardinality::Exact(n) => {
                assert_eq!(
                    *n,
                    BigUint::from(2u64),
                    "enum with 2 variants should have cardinality 2"
                )
            }
            _ => panic!(
                "Expected Exact(2) for 2-variant enum range, got {:?}",
                cardinality
            ),
        }
    }

    #[test]
    fn analyze_program_space_simple_range_is_o1() {
        let complexity = marigold_analyze("range(0, 10).return").unwrap();
        // A simple range with no combinatorial ops uses O(1) space (streaming, no buffering)
        assert_eq!(complexity.program_space, ComplexityClass::O1);
    }

    #[test]
    fn analyze_program_space_combinations_constant_input_is_o1() {
        // When the input is a constant-sized range, combinations have O(1) space
        // because the cardinality can be evaluated at analysis time
        let complexity = marigold_analyze("range(0, 5).combinations(2).return").unwrap();
        // The complexity is O(1) because the range is a constant known at compile time
        assert_eq!(complexity.program_space, ComplexityClass::O1);
    }

    #[test]
    fn analyze_select_all_two_streams() {
        let complexity =
            marigold_analyze("select_all(range(0, 3), range(10, 13)).return").unwrap();
        // select_all merges multiple streams; we expect at least 1 stream in the analysis
        assert!(!complexity.streams.is_empty());
    }

    // -----------------------------------------------------------------------
    // marigold_analyze: failure cases
    // -----------------------------------------------------------------------

    #[test]
    fn analyze_invalid_syntax_returns_error() {
        let result = marigold_analyze("not valid marigold @@");
        assert!(result.is_err(), "Expected Err for invalid syntax");
    }

    #[test]
    fn analyze_empty_string_succeeds() {
        // An empty program is valid and produces a complexity with no streams
        let result = marigold_analyze("");
        assert!(
            result.is_ok(),
            "Empty program should analyze successfully, got: {:?}",
            result
        );
        let complexity = result.unwrap();
        assert!(
            complexity.streams.is_empty(),
            "Empty program should have no streams"
        );
    }

    // -----------------------------------------------------------------------
    // ComplexityClass ordering and display
    // -----------------------------------------------------------------------

    #[test]
    fn complexity_class_ordering() {
        assert!(ComplexityClass::O1 < ComplexityClass::ON);
        assert!(ComplexityClass::ON < ComplexityClass::OPolynomial(2));
        assert!(ComplexityClass::OPolynomial(2) < ComplexityClass::OCombinatorial(2));
        assert!(ComplexityClass::OCombinatorial(2) < ComplexityClass::OPermutational(2));
        assert!(ComplexityClass::OPermutational(2) < ComplexityClass::OFactorial);
        assert!(ComplexityClass::OFactorial < ComplexityClass::Unknown);
    }

    #[test]
    fn complexity_class_display() {
        assert_eq!(ComplexityClass::O1.to_string(), "O(1)");
        assert_eq!(ComplexityClass::ON.to_string(), "O(n)");
        assert_eq!(ComplexityClass::OLogN.to_string(), "O(log(n))");
        assert_eq!(ComplexityClass::ONLogN.to_string(), "O(n*log(n))");
        assert_eq!(ComplexityClass::ONLogK(4).to_string(), "O(n*log(4))");
        assert_eq!(ComplexityClass::OPolynomial(2).to_string(), "O(n^2)");
        assert_eq!(ComplexityClass::OCombinatorial(3).to_string(), "O(C(n,3))");
        assert_eq!(
            ComplexityClass::OPermutational(2).to_string(),
            "O(n!/(n-2)!)"
        );
        assert_eq!(ComplexityClass::OFactorial.to_string(), "O(n!)");
        assert_eq!(ComplexityClass::Unknown.to_string(), "O(?)");
    }

    #[test]
    fn complexity_class_max() {
        assert_eq!(
            ComplexityClass::O1.max(ComplexityClass::ON),
            ComplexityClass::ON
        );
        assert_eq!(
            ComplexityClass::ON.max(ComplexityClass::O1),
            ComplexityClass::ON
        );
        assert_eq!(
            ComplexityClass::OPolynomial(3).max(ComplexityClass::OCombinatorial(2)),
            ComplexityClass::OCombinatorial(2)
        );
    }

    // -----------------------------------------------------------------------
    // ComplexityClass parse (FromStr)
    // -----------------------------------------------------------------------

    #[test]
    fn complexity_class_from_str_o1() {
        use std::str::FromStr;
        assert_eq!(
            ComplexityClass::from_str("O(1)").unwrap(),
            ComplexityClass::O1
        );
    }

    #[test]
    fn complexity_class_from_str_on() {
        use std::str::FromStr;
        assert_eq!(
            ComplexityClass::from_str("O(n)").unwrap(),
            ComplexityClass::ON
        );
    }

    #[test]
    fn complexity_class_from_str_factorial() {
        use std::str::FromStr;
        assert_eq!(
            ComplexityClass::from_str("O(n!)").unwrap(),
            ComplexityClass::OFactorial
        );
    }

    #[test]
    fn complexity_class_from_str_combinatorial() {
        use std::str::FromStr;
        assert_eq!(
            ComplexityClass::from_str("O(C(n,3))").unwrap(),
            ComplexityClass::OCombinatorial(3)
        );
    }

    #[test]
    fn complexity_class_from_str_permutational() {
        use std::str::FromStr;
        assert_eq!(
            ComplexityClass::from_str("O(n!/(n-2)!)").unwrap(),
            ComplexityClass::OPermutational(2)
        );
    }

    #[test]
    fn complexity_class_from_str_polynomial() {
        use std::str::FromStr;
        assert_eq!(
            ComplexityClass::from_str("O(n^3)").unwrap(),
            ComplexityClass::OPolynomial(3)
        );
    }

    #[test]
    fn complexity_class_from_str_invalid_returns_error() {
        use std::str::FromStr;
        assert!(ComplexityClass::from_str("not_a_complexity").is_err());
    }
}
