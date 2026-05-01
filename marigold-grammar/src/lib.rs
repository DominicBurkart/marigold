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
//! - [`parser`]: Pest-based PEG parser entry points. The grammar lives in
//!   `src/marigold.pest`.
//! - [`pest_ast_builder`]: Lowers Pest parse trees into the AST.
//! - [`nodes`]: AST node definitions for all Marigold constructs and the
//!   code-generation logic that emits Rust source.
//! - [`complexity`], [`bound_resolution`], [`symbol_table`]: Static analysis
//!   passes (cardinality/space/time, bounded-type evaluation, type lookups).
//!
//! Validation: per-module unit tests, negative parser tests, and
//! integration tests over the example programs.
//!
//! ## Feature Flags
//!
//! `io`, `tokio`, `async-std`: forwarded by downstream crates that consume
//! the generated code; this crate itself only emits feature-gated source.

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse a Marigold program and return the generated Rust source.
///
/// Thin alias for [`parser::parse_marigold`]; the recommended entry point
/// for most callers.
///
/// # Examples
///
/// ```ignore
/// use marigold_grammar::marigold_parse;
///
/// let code = marigold_parse("range(0, 100).return")?;
/// // code is now valid Rust source ready to compile
/// ```
pub fn marigold_parse(s: &str) -> Result<String, parser::MarigoldParseError> {
    parser::parse_marigold(s)
}

pub fn marigold_analyze(
    s: &str,
) -> Result<complexity::ProgramComplexity, parser::MarigoldParseError> {
    parser::PestParser::analyze(s)
}
