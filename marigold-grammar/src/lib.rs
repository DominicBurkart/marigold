#![forbid(unsafe_code)]

//! # Marigold Grammar Library
//!
//! Grammar and parser infrastructure for the Marigold DSL: a Pest-based PEG
//! parser, AST definitions, and code generation that turns Marigold programs
//! into valid Rust.
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
//! ## Architecture
//!
//! - [`parser`]: Pest-based parser behind a trait abstraction; use
//!   [`parser::get_parser()`] for an instance.
//! - [`nodes`]: AST node definitions for all Marigold constructs.
//! - [`pest_ast_builder`]: Transforms Pest parse trees into the AST.
//! - `src/marigold.pest`: the grammar file.
//!
//! Code generation is performed by an internal function that transforms the
//! AST to Rust source.
//!
//! ## Performance
//!
//! - Parsing: < 1ms for typical programs.
//! - Code generation: dominates runtime; scales with program complexity.
//! - Binary size: Pest parser adds ~40KB.
//!
//! ## Feature Flags
//!
//! `io`, `tokio`, and `async-std` are declared for workspace coordination but
//! gate no functionality in this crate; the corresponding behaviour lives in
//! sibling crates.

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
/// Recommended entry point; thin alias for [`parser::parse_marigold`].
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
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
