#[macro_use]
extern crate lalrpop_util;

#[macro_use]
extern crate lazy_static;

use lalrpop_util::ParseError;
extern crate proc_macro;
use crate::ast::Token;

pub use itertools;

pub mod expressions;

lalrpop_mod!(#[allow(clippy::all)] pub ast);

lazy_static! {
    static ref PARSER: ast::ExprParser = ast::ExprParser::new();
}

pub fn glass_parse<'a>(s: &'a str) -> Result<String, ParseError<usize, Token<'a>, &'static str>> {
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
