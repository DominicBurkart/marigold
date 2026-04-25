#![forbid(unsafe_code)]

//! # Marigold Grammar Library
//!
//! Grammar and parser infrastructure for the Marigold DSL: a Pest-based PEG
//! parser, the AST defined in [`nodes`], and code generation that transforms
//! parsed programs into Rust source.
//!
//! ## Quick Start
//!
//! ```ignore
//! use marigold_grammar::parser::parse_marigold;
//!
//! let code = parse_marigold("range(0, 100).return")?;
//! println!("{code}");
//! ```
//!
//! ## Layout
//!
//! - `src/marigold.pest` — PEG grammar.
//! - [`parser`] — entry points [`parser::parse_marigold`] and [`parser::PestParser`].
//! - [`pest_ast_builder`] — builds [`nodes`] AST from the Pest parse tree.
//! - [`bound_resolution`] / [`symbol_table`] — bounded-type resolution.
//! - [`complexity`] — static complexity analysis used by `marigold analyze`.

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Convenience wrapper around [`parser::parse_marigold`] that also runs
/// [`complexity::analyze_program`] on the resolved AST.
pub fn marigold_analyze(
    s: &str,
) -> Result<complexity::ProgramComplexity, parser::MarigoldParseError> {
    parser::PestParser::analyze(s)
}
