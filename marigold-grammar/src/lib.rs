#![forbid(unsafe_code)]

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

lalrpop_mod!(#[allow(clippy::all)] pub ast);

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
