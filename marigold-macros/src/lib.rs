#![forbid(unsafe_code)]

extern crate proc_macro;
use marigold_grammar::marigold_parse;
use once_cell::sync::Lazy;
use proc_macro::TokenStream;
use regex::Regex;

static TOP_LEVEL_BRACKETS: Lazy<Regex> = Lazy::new(|| {
    Regex::new(
  r"(?P<function_signature>fn[\s]+[\w]+\(.*\)([\s]+->[\s]+[^{]+))(\{)(?P<function_body>[\s\S]*)(\})"
).unwrap()
});

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    let cleaned = TOP_LEVEL_BRACKETS.replace_all(
        s.as_str(),
        "$function_signature %%%MARIGOLD_FUNCTION_START%%% $function_body %%%MARIGOLD_FUNCTION_END%%%"
    );
    format!(
        "{{\n{}\n}}\n",
        marigold_parse(&cleaned).expect("marigold parsing error")
    )
    .parse()
    .unwrap()
}
