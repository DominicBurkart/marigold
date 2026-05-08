#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::marigold_parse;
use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    // Note: item.to_string() round-trips through the lexer, losing span fidelity.
    // This means compile_error! can only point at the macro call site, not at the
    // specific token inside the DSL. This is the best achievable on stable Rust today.
    let s = item.to_string();
    match marigold_parse(&s) {
        Ok(generated) => {
            let src = format!(
                "{{
{}
}}
",
                generated
            );
            match src.parse() {
                Ok(ts) => ts,
                Err(e) => {
                    let msg = format!(
                        "marigold internal error: generated code failed to lex: {}. \
                         Please file a bug at https://github.com/DominicBurkart/marigold/issues",
                        e
                    );
                    quote! { compile_error!(#msg) }.into()
                }
            }
        }
        Err(e) => {
            let msg = format!("marigold parse error: {}", e);
            quote! { compile_error!(#msg) }.into()
        }
    }
}
