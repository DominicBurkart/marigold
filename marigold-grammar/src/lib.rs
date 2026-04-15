#![forbid(unsafe_code)]

//! # Marigold Grammar Library
//!
//! Provides the grammar, parser, and code generator for the Marigold DSL.
//!
//! ## Overview
//!
//! - **Pest-based parser**: PEG parser defined in `src/marigold.pest`
//! - **AST definitions**: [`nodes`] — complete abstract syntax tree
//! - **Code generation**: Transforms Marigold programs into valid Rust code
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
//! ## Architecture
//!
//! - [`parser`]: Pest-based parser; use [`parser::parse_marigold`] as the primary entry point
//! - [`nodes`]: AST node definitions for all Marigold constructs
//! - [`pest_ast_builder`]: Transforms Pest parse trees into the AST

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse a Marigold source string into Rust code.
///
/// This is an alias for [`parser::parse_marigold`] and is the recommended entry point
/// for most use cases.
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

/// Statically analyze a Marigold source string and return its complexity metadata.
pub fn marigold_analyze(
    s: &str,
) -> Result<complexity::ProgramComplexity, parser::MarigoldParseError> {
    parser::PestParser::analyze(s)
}
