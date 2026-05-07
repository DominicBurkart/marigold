//! # Marigold Grammar
//!
//! Grammar, parser, and code generation for the Marigold DSL. Marigold is a
//! domain-specific language for expressing stream processing programs in Rust.
//!
//! ## Quick Start
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
//! - [`parser`]: PEG parser built on Pest. Use [`parser::get_parser()`] for an
//!   instance, or [`parser::parse_marigold`] / [`marigold_parse`] for one-shot
//!   parsing.
//! - [`nodes`]: AST node definitions.
//! - [`pest_ast_builder`]: Transforms Pest parse trees into the AST.
//! - The grammar itself lives in `marigold-grammar/src/marigold.pest`.
//!
//! The parser is validated through unit tests, negative tests for invalid
//! syntax, and integration tests using real example programs.
#![forbid(unsafe_code)]

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse a Marigold program and return the generated Rust code.
///
/// Alias for [`parser::parse_marigold`].
///
/// # Examples
///
/// ```ignore
/// use marigold_grammar::marigold_parse;
///
/// let code = marigold_parse("range(0, 100).return")?;
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
