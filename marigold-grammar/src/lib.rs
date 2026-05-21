//! # Marigold Grammar
//!
//! Pest-based parser and code generator for the Marigold DSL. Parses Marigold
//! source into an AST (see [`nodes`]) and emits the equivalent async Rust code.
//!
//! The grammar itself lives in `src/marigold.pest`.
//!
//! ## Example
//!
//! ```ignore
//! use marigold_grammar::marigold_parse;
//!
//! let code = marigold_parse("range(0, 100).return")?;
//! ```

#![forbid(unsafe_code)]

extern crate proc_macro;

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
/// Convenience wrapper around [`parser::parse_marigold`].
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
