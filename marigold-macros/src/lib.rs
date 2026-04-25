#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::parser::parse_marigold;
use proc_macro::TokenStream;

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    format!(
        "{{\n{}\n}}\n",
        parse_marigold(&s).expect("marigold parsing error")
    )
    .parse()
    .unwrap()
}
