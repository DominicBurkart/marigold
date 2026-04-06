#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::marigold_parse;
use proc_macro::TokenStream;

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    match marigold_parse(&s) {
        Ok(generated) => format!("{{\n{generated}\n}}\n").parse().expect(
            "generated Rust code failed to lex as a TokenStream; this is a bug in marigold",
        ),
        Err(e) => {
            // Emit a compile_error! so the user sees the actual parse failure at the
            // call site rather than a generic panic message.
            let msg = format!("marigold parse error: {e}");
            format!("compile_error!({msg:?})").parse().unwrap()
        }
    }
}
