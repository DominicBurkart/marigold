#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::marigold_parse;
use proc_macro::TokenStream;

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    let generated = format!(
        "{{\n{}\n}}\n",
        marigold_parse(&s).expect("marigold parsing error")
    );
    match generated.parse() {
        Ok(ts) => ts,
        Err(e) => {
            // The generated Rust source failed to lex/parse as a token stream.
            // Emit a compile_error! so the user gets a clear diagnostic instead
            // of a silent panic inside the proc-macro host process.
            let msg = format!(
                "marigold internal error: generated source failed to parse as Rust tokens: {}. \
                 Generated source was: {}",
                e, generated
            );
            format!("compile_error!({:?})", msg)
                .parse()
                .unwrap_or_else(|_| TokenStream::new())
        }
    }
}
