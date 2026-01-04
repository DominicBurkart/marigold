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
//! - **Dual parser support**: LALRPOP (default) and Pest (opt-in via `pest-parser` feature)
//! - **AST definitions**: Complete abstract syntax tree for Marigold programs
//! - **Code generation**: Transforms Marigold programs into valid Rust code
//!
//! ## Quick Start
//!
//! Parse a Marigold program with the default parser:
//!
//! ```ignore
//! use marigold_grammar::parser::parse_marigold;
//!
//! let code = parse_marigold("range(0, 100).return")?;
//! println!("{}", code);
//! ```
//!
//! To use the Pest parser instead, enable the feature in your `Cargo.toml`:
//!
//! ```toml
//! marigold-grammar = { version = "0.1.16", features = ["pest-parser"] }
//! ```
//!
//! ## Architecture
//!
//! ### Parser Abstraction
//!
//! The [`parser`] module provides a trait-based abstraction that allows both parsers to be
//! used interchangeably. The factory function [`parser::get_parser()`] returns the appropriate
//! parser based on compile-time feature flags.
//!
//! ### Grammar Files
//!
//! - **LALRPOP**: `src/ast.lalrpop` (original, used by default)
//! - **Pest**: `src/marigold.pest` (new, enabled with `pest-parser` feature)
//!
//! Both grammar files define the same language but use different parser technologies.
//!
//! ### AST and Code Generation
//!
//! - [`nodes`]: AST node definitions for all Marigold constructs
//! - [`parser::PestParser::generate_rust_code`]: Code generation logic (shared between parsers)
//! - [`pest_ast_builder`]: Transforms Pest parse trees into the shared AST
//!
//! ## Testing & Validation
//!
//! The parser implementations are validated through:
//!
//! - **Unit tests** in each module covering specific functionality
//! - **Equivalence tests** comparing Pest and LALRPOP output for identical inputs
//! - **Integration tests** using real-world example programs
//!
//! ## Feature Flags
//!
//! - `pest-parser`: Enable the Pest-based parser (becomes default when enabled)
//! - `io`: I/O features (available in other crates)
//! - `tokio`: Tokio runtime integration (available in other crates)
//! - `async-std`: async-std runtime integration (available in other crates)
//!
//! ## Performance Characteristics
//!
//! - **Parsing**: Both parsers complete in < 1ms for typical programs
//! - **Code generation**: Dominates runtime, scales with program complexity
//! - **Binary size**: LALRPOP adds ~20KB, Pest feature adds ~40KB
//!
//! ## Migration Guide
//!
//! To use the Pest parser:
//!
//! 1. Add `features = ["pest-parser"]` to your `marigold-grammar` dependency
//! 2. Your code continues to work unchanged (trait-based abstraction handles it)

#[macro_use]
extern crate lalrpop_util;

#[macro_use]
extern crate lazy_static;

use lalrpop_util::ParseError;
extern crate proc_macro;
use crate::ast::Token;

pub use itertools;

pub mod nodes;
pub mod parser;
mod type_aggregation;

#[cfg(feature = "pest-parser")]
pub mod pest_ast_builder;

lalrpop_mod!(#[allow(clippy::all)] pub ast);

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

// Legacy function name for backwards compatibility
#[deprecated(
    since = "0.1.17",
    note = "Use `marigold_parse` instead for the new parser abstraction"
)]
pub fn marigold_parse_legacy<'a>(
    s: &'a str,
) -> Result<String, ParseError<usize, Token<'a>, &'static str>> {
    // Use LALRPOP directly for legacy compatibility
    lazy_static! {
        static ref PARSER: ast::ProgramParser = ast::ProgramParser::new();
    }
    PARSER.parse(s)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
