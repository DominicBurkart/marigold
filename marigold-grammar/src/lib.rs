#![forbid(unsafe_code)]

//! # Marigold Grammar
//!
//! Parser, AST, and Rust code generation for the Marigold DSL. The grammar
//! itself lives in `src/marigold.pest` and is consumed by a Pest-based parser.
//!
//! ## Example
//!
//! ```ignore
//! use marigold_grammar::marigold_parse;
//!
//! let rust_code = marigold_parse("range(0, 100).return")?;
//! ```
//!
//! ## Modules
//!
//! - [`parser`]: parses Marigold source into Rust code.
//! - [`nodes`]: AST node definitions.
//! - [`pest_ast_builder`]: converts Pest parse trees into AST nodes.
//! - [`complexity`]: static analysis producing time/space/cardinality classes.
//!
//! ## Feature flags
//!
//! - `io`: emits `serde` derives on generated structs (required by I/O ops
//!   like `write_file`/`read_file`).
//! - `tokio`, `async-std`: pass-through flags consumed by sibling crates.

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse Marigold source and return the generated Rust code.
///
/// Recommended entry point; thin wrapper over [`parser::parse_marigold`].
///
/// # Examples
///
/// ```ignore
/// use marigold_grammar::marigold_parse;
///
/// let code = marigold_parse("range(0, 100).return")?;
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
