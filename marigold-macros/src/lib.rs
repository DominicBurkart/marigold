#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::marigold_parse;
use proc_macro::TokenStream;

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    format!(
        "{{\n{}\n}}\n",
        marigold_parse(s.as_str()).expect("marigold parsing error")
    )
    .parse()
    .unwrap()
}
