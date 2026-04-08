#![forbid(unsafe_code)]

//! Grammar and parser infrastructure for the Marigold DSL.
//!
//! Provides a [pest]-based parser, AST definitions, and Rust code generation
//! for Marigold programs.
//!
//! ## Quick Start
//!
//! ```ignore
//! use marigold_grammar::marigold_parse;
//!
//! let code = marigold_parse("range(0, 100).return")?;
//! println!("{}", code);
//! ```
//!
//! ## Modules
//!
//! - [`parser`]: entry point; [`parser::parse_marigold`] returns generated Rust code
//! - [`nodes`]: AST node definitions
//! - [`pest_ast_builder`]: converts Pest parse trees into the AST
//! - Grammar file: `src/marigold.pest`
//!
//! [pest]: https://pest.rs

#[macro_use]
extern crate lazy_static;

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse a Marigold program and return generated Rust code.
///
/// Alias for [`parser::parse_marigold`].
///
/// ```ignore
/// let code = marigold_grammar::marigold_parse("range(0, 100).return")?;
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
    use super::*;

    /// marigold_parse succeeds on valid programs and produces non-empty Rust source.
    #[test]
    fn parse_simple_range_return() {
        let rust = marigold_parse("range(0, 10).return").expect("should parse");
        assert!(!rust.is_empty(), "parsed output should not be empty");
    }

    /// marigold_parse succeeds on a pipeline with map and filter.
    #[test]
    fn parse_map_filter_pipeline() {
        let rust = marigold_parse("range(0, 10).map(f).filter(g).return")
            .expect("map+filter pipeline should parse");
        assert!(!rust.is_empty());
    }

    /// marigold_parse succeeds on a fold program.
    #[test]
    fn parse_fold_program() {
        let rust = marigold_parse("range(0, 5).fold(0, f).return")
            .expect("fold program should parse");
        assert!(!rust.is_empty());
    }

    /// marigold_parse returns an error for completely invalid syntax.
    #[test]
    fn parse_invalid_syntax_returns_error() {
        let result = marigold_parse("this is not valid marigold !!!@#$");
        assert!(result.is_err(), "invalid input should produce a parse error");
    }

    /// marigold_analyze succeeds on a simple program and reports expected stream count.
    #[test]
    fn analyze_simple_range() {
        let complexity = marigold_analyze("range(0, 10).return").expect("should analyze");
        assert_eq!(complexity.streams.len(), 1);
    }

    /// marigold_analyze and marigold_parse both handle the same valid input without panicking.
    #[test]
    fn parse_and_analyze_agree_on_valid_input() {
        let src = "range(0, 100).map(f).return";
        assert!(marigold_parse(src).is_ok());
        assert!(marigold_analyze(src).is_ok());
    }
}
