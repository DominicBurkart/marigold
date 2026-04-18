#![forbid(unsafe_code)]

//! Grammar, parser, and code-generation for the Marigold DSL.
//!
//! The main entry point is [`marigold_parse`], which takes a Marigold program string and
//! returns generated Rust code. [`marigold_analyze`] returns static complexity information
//! without generating code.
//!
//! ## Module overview
//!
//! | Module | Purpose |
//! |--------|---------|
//! | [`parser`] | Pest-based PEG parser and `MarigoldParser` trait |
//! | [`nodes`] | AST node types for all Marigold constructs |
//! | [`pest_ast_builder`] | Converts Pest parse trees into AST nodes |
//! | [`complexity`] | Static complexity analysis (time/space/cardinality) |
//! | [`bound_resolution`] | Resolves symbolic bounds in bounded-integer types |
//! | [`symbol_table`] | Tracks declared enums and struct fields for bound resolution |
//!
//! The grammar itself lives in `src/marigold.pest`.

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod nodes;
pub mod parser;
pub mod symbol_table;
mod type_aggregation;

pub mod pest_ast_builder;

/// Parse a Marigold program and return the generated Rust code.
///
/// ```ignore
/// let code = marigold_grammar::marigold_parse("range(0, 100).return")?;
/// // code is async Rust ready to compile
/// ```
pub fn marigold_parse(s: &str) -> Result<String, parser::MarigoldParseError> {
    parser::parse_marigold(s)
}

/// Parse a Marigold program and return its static complexity analysis without generating code.
pub fn marigold_analyze(
    s: &str,
) -> Result<complexity::ProgramComplexity, parser::MarigoldParseError> {
    parser::PestParser::analyze(s)
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse_returns_async_rust() {
        let code = super::marigold_parse("range(0, 5).return").expect("should parse");
        assert!(code.contains("async"));
    }
}
