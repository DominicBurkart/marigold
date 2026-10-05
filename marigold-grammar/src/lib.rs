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

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod diagnostics;
pub mod nodes;
pub mod parser;
mod recovery;
mod resolver;
mod span_index;
pub mod symbol_table;
pub mod symbols;
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

/// Check Marigold source and return every diagnostic found, with byte ranges.
///
/// No diagnostic has [`diagnostics::Severity::Error`] exactly when [`marigold_parse`]
/// succeeds. Warnings (a stream variable read before it is declared) and information
/// (stream variables, functions and structs not declared in the program, which may
/// be Rust items in scope inside `m!()`; a stream variable may be any Rust binding
/// with a `get()` method returning a stream) never affect [`marigold_parse`].
/// At most 50 resolver diagnostics are reported per file.
///
/// A syntax error anywhere in the file hides the resolver diagnostics (warnings and
/// information) for the whole file; only the syntax error is reported until it is fixed.
/// This is intended.
///
/// Input larger than [`diagnostics::MAX_CHECK_INPUT_BYTES`] is rejected with a
/// single `input-too-large` error diagnostic without being parsed.
///
/// ```
/// use marigold_grammar::marigold_check;
/// use marigold_grammar::diagnostics::Severity;
///
/// assert!(marigold_check("range(0, 10).return").is_empty());
///
/// let diags = marigold_check("range(0, 10).retur");
/// assert_eq!(diags[0].code, "syntax-error");
///
/// let diags = marigold_check("range(0, 10).map(double).return");
/// assert_eq!(diags[0].code, "undefined-fn");
/// assert_eq!(diags[0].severity, Severity::Information);
/// assert!(marigold_grammar::marigold_parse("range(0, 10).map(double).return").is_ok());
/// ```
pub fn marigold_check(s: &str) -> Vec<diagnostics::Diagnostic> {
    if s.len() > diagnostics::MAX_CHECK_INPUT_BYTES {
        return vec![diagnostics::Diagnostic::input_too_large(s)];
    }
    parser::PestParser::check(s)
}

/// Index every declaration and reference in Marigold source for navigation.
///
/// Chunks of the source that parse are indexed even when other chunks have syntax
/// errors, so go-to-definition keeps working while a file is being edited.
///
/// ```
/// use marigold_grammar::{marigold_symbols, symbols::SymbolKind};
///
/// let src = "x = range(0, 5)\nx.return";
/// let index = marigold_symbols(src);
/// assert_eq!(index.declarations[0].name, "x");
/// assert_eq!(index.declarations[0].kind, SymbolKind::StreamVariable);
/// assert_eq!(index.references.len(), 1);
///
/// let text = "x = range(0, 5)\nrange(0, 1).retur\nx.return";
/// let broken = marigold_symbols(text);
/// assert_eq!(broken.declarations[0].name, "x");
/// assert_eq!(broken.references[0].range.start, text.rfind("x.return").unwrap());
/// ```
pub fn marigold_symbols(s: &str) -> symbols::SymbolIndex {
    symbols::SymbolIndex::from_source(s)
}

/// Complexity of every variable declaration and output stream, with its name and source range.
///
/// ```
/// use marigold_grammar::marigold_stream_complexities;
///
/// let src = "x = range(0, 5).map(f)\nx.return";
/// let nodes = marigold_stream_complexities(src).unwrap();
/// assert_eq!(nodes.len(), 2);
/// assert_eq!(nodes[0].name.as_deref(), Some("x"));
/// assert_eq!(nodes[1].name, None);
/// assert_eq!(&src[nodes[1].range.start..nodes[1].range.end], "x.return");
/// assert!(marigold_stream_complexities("range(0,").is_err());
/// ```
pub fn marigold_stream_complexities(
    s: &str,
) -> Result<Vec<complexity::NodeComplexity>, parser::MarigoldParseError> {
    parser::PestParser::stream_complexities(s)
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
