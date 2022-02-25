#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!( #[allow(clippy::all)] pub calculator1); // synthesized by LALRPOP

fn main() {
    println!("hi");
}

#[test]
fn calculator1() {
    assert!(calculator1::TermParser::new().parse("22").is_ok());
    assert!(calculator1::TermParser::new().parse("(22)").is_ok());
    assert!(calculator1::TermParser::new().parse("((((22))))").is_ok());
    assert!(calculator1::TermParser::new().parse("((22)").is_err());
}
