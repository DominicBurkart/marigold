#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub ast);

fn main() {
    println!("hi");
}

// #[test]
// fn calculator1() {
// }
