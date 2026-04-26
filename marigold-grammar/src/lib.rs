#![forbid(unsafe_code)]

//! # Marigold Grammar
//!
//! Grammar, parser, and Rust code generation for the Marigold DSL — a language
//! for expressing stream-processing programs.
//!
//! ## Quick start
//!
//! ```ignore
//! use marigold_grammar::parser::parse_marigold;
//!
//! let code = parse_marigold("range(0, 100).return")?;
//! println!("{}", code);
//! ```
//!
//! ## Modules
//!
//! - [`parser`]: Pest-based parser behind the [`MarigoldParser`](parser::MarigoldParser)
//!   trait; use [`parser::get_parser`] or [`parser::parse_marigold`] as the entry point.
//! - [`nodes`]: AST node definitions.
//! - [`pest_ast_builder`]: Builds the AST from Pest parse trees.
//! - [`complexity`]: Static complexity analysis.
//!
//! The grammar itself lives in `marigold-grammar/src/marigold.pest`.
//!
//! ## Performance
//!
//! - Parsing: < 1ms for typical programs.
//! - Code generation dominates runtime and scales with program size.
//! - Pest adds ~40KB to binary size.

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse Marigold source and return the generated Rust code.
///
/// Recommended entry point; alias for [`parser::parse_marigold`].
///
/// # Examples
///
/// ```ignore
/// use marigold_grammar::marigold_parse;
///
/// let code = marigold_parse("range(0, 100).return")?;
/// // `code` is valid Rust source ready to compile.
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
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
