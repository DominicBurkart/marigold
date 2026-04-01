#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::marigold_parse;
use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    match marigold_parse(&s) {
        Ok(generated) => format!("{{\n{}\n}}\n", generated)
            .parse()
            .expect("generated Rust code failed to parse as a TokenStream"),
        Err(e) => {
            let msg = format!("marigold parse error: {}", e);
            quote! { compile_error!(#msg) }.into()
        }
    }
}
