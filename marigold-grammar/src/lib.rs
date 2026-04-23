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
//! ```
//! use marigold_grammar::parser::parse_marigold;
//!
//! let code = parse_marigold("range(0, 100).return")
//!     .expect("valid marigold program");
//! assert!(code.contains("async"));
//! ```
//!
//! ## Architecture
//!
//! - [`parser`]: Pest-based parser entry points ([`parser::parse_marigold`],
//!   [`parser::PestParser`]) plus the `MarigoldParser` trait.
//! - [`nodes`]: Typed AST nodes and code-generation helpers.
//! - [`pest_ast_builder`]: Lowers a Pest parse tree into the AST.
//! - [`complexity`]: Static cardinality / time / space analysis.
//! - [`bound_resolution`] and [`symbol_table`]: Validate bounded types and
//!   resolve inter-type references.
//!
//! The Pest grammar lives at `src/marigold.pest`.
//!
//! ## Feature Flags
//!
//! `io`, `tokio`, and `async-std` are forwarded to sibling crates; this crate's
//! public API is feature-flag independent.

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Convenience function for parsing Marigold code.
///
/// This is an alias for [`parser::parse_marigold`]. It is the recommended
/// entry point for callers that do not need to pick a specific parser backend.
///
/// # Examples
///
/// ```
/// use marigold_grammar::marigold_parse;
///
/// let code = marigold_parse("range(0, 100).return").unwrap();
/// assert!(code.contains("async"));
/// ```
pub fn marigold_parse(s: &str) -> Result<String, parser::MarigoldParseError> {
    parser::parse_marigold(s)
}

pub fn marigold_analyze(
    s: &str,
) -> Result<complexity::ProgramComplexity, parser::MarigoldParseError> {
    parser::PestParser::analyze(s)
}
