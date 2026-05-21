#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::marigold_parse;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote_spanned;

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    let generated = match marigold_parse(&s) {
        Ok(code) => code,
        Err(e) => {
            let msg = format!("marigold parsing error: {e}");
            let span = Span::call_site();
            return quote_spanned!(span=> compile_error!(#msg);).into();
        }
    };
    format!("{{\n{generated}\n}}\n")
        .parse()
        .unwrap_or_else(|e| panic!("marigold codegen produced invalid tokens: {e}"))
}
