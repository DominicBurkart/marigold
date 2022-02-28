extern crate proc_macro;
use glass_grammar::glass_parse;
use proc_macro::TokenStream;

#[proc_macro]
pub fn glass(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    glass_parse(s.as_str())
        .expect("glass parsing error")
        .parse()
        .unwrap()
}
