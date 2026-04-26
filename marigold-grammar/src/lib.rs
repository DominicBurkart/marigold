#![forbid(unsafe_code)]

//! Grammar and parser infrastructure for the Marigold DSL.
//!
//! Provides a pest-based parser, AST definitions, and Rust code generation
//! for Marigold programs.
//!
//! The main entry points are [`marigold_parse`] (compile a program to Rust)
//! and [`marigold_analyze`] (return static complexity information).
//!
//! ## Feature flags
//!
//! - `io`: enables I/O stream operations
//! - `tokio`: enables the Tokio async runtime
//! - `async-std`: enables the async-std runtime

extern crate proc_macro;

/// Re-exported so downstream macro-generated code can use itertools combinators
/// without declaring their own dependency.
pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse a Marigold program and return the equivalent Rust source code.
///
/// This is the recommended entry point for most use cases.
///
/// # Example
///
/// ```ignore
/// let code = marigold_grammar::marigold_parse("range(0, 100).return")?;
/// ```
pub fn marigold_parse(s: &str) -> Result<String, parser::MarigoldParseError> {
    parser::parse_marigold(s)
}

/// Statically analyze a Marigold program and return its complexity profile.
pub fn marigold_analyze(
    s: &str,
) -> Result<complexity::ProgramComplexity, parser::MarigoldParseError> {
    parser::PestParser::analyze(s)
}
