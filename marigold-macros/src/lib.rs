#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::marigold_parse;
use proc_macro::TokenStream;

/// The `m!` macro — compile Marigold DSL into Rust at build time.
///
/// The `.parse().unwrap()` is safe: `marigold_parse` guarantees the returned string is
/// valid Rust token syntax, so parsing it as a `TokenStream` cannot fail.
#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    format!(
        "{{\n{}\n}}\n",
        marigold_parse(&s).expect("marigold parsing error")
    )
    .parse()
    .unwrap()
}
