#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

fn main() {
    println!("hi");
}

#[test]
fn calculator1() {
    assert!(grammar::TermParser::new().parse("22").is_ok());
    assert!(grammar::TermParser::new().parse("(22)").is_ok());
    assert!(grammar::TermParser::new().parse("((((22))))").is_ok());
    assert!(grammar::TermParser::new().parse("((22)").is_err());
}
