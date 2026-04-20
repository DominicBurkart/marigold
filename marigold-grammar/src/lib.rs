#![forbid(unsafe_code)]

//! # marigold-grammar
//!
//! Grammar, parser, and code generation for the Marigold DSL. This crate is
//! used by `marigold-macros` (and in turn the `marigold` crate); most users
//! should depend on `marigold` instead of consuming this crate directly.
//!
//! ## What it provides
//!
//! - A Pest-based PEG parser for Marigold source (`src/marigold.pest`).
//! - AST definitions for all Marigold constructs ([`nodes`]).
//! - A transformer from Pest parse trees to the AST ([`pest_ast_builder`]).
//! - Code generation that lowers Marigold programs to Rust source.
//! - Static complexity analysis ([`complexity`]).
//!
//! ## Quick start
//!
//! Parse a Marigold program into equivalent Rust source:
//!
//! ```ignore
//! use marigold_grammar::marigold_parse;
//!
//! let rust_src = marigold_parse("range(0, 100).return")?;
//! // `rust_src` is Rust code ready to be compiled.
//! ```
//!
//! ## Modules
//!
//! - [`parser`]: Pest-based parser; [`parser::get_parser`] returns a parser
//!   instance, and [`parser::parse_marigold`] is the main entry point.
//! - [`nodes`]: AST node definitions.
//! - [`pest_ast_builder`]: Pest parse tree -> AST.
//! - [`complexity`]: Static complexity analysis.
//! - [`bound_resolution`], [`symbol_table`]: Name and bound resolution.
//!
//! ## Testing
//!
//! The parser is covered by unit tests in each module, negative tests for
//! invalid syntax, and integration tests that parse the example programs in
//! the workspace.
//!
//! ## Feature flags
//!
//! This crate has no feature flags of its own. Runtime (`tokio`, `async-std`)
//! and I/O (`io`) features are exposed by the top-level `marigold` crate and
//! by `marigold-impl`.

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse Marigold source into equivalent Rust source.
///
/// This is a convenience wrapper around [`parser::parse_marigold`] and is the
/// recommended entry point for most use cases.
///
/// # Examples
///
/// ```ignore
/// use marigold_grammar::marigold_parse;
///
/// let rust_src = marigold_parse("range(0, 100).return")?;
/// // `rust_src` is Rust code ready to be compiled.
/// ```
pub fn marigold_parse(s: &str) -> Result<String, parser::MarigoldParseError> {
    parser::parse_marigold(s)
}

/// Statically analyze the complexity of a Marigold program.
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
