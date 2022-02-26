#[macro_use]
extern crate lalrpop_util;
use lalrpop_util::ParseError;
extern crate proc_macro;

lalrpop_mod!(#[allow(clippy::all)] pub ast);

pub fn glass_parse(s: &str) -> Result<String, ParseError<usize, ast::Token<'_>, &'static str>> {
    ast::ExprParser::new().parse(s)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
