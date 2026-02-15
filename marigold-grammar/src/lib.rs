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
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
