#![forbid(unsafe_code)]

extern crate proc_macro;
use proc_macro::TokenStream;

#[cfg(not(feature = "otel"))]
fn generate(source: &str) -> Result<String, marigold_grammar::parser::MarigoldParseError> {
    marigold_grammar::marigold_parse(source)
}

#[cfg(feature = "otel")]
fn generate(source: &str) -> Result<String, marigold_grammar::parser::MarigoldParseError> {
    let program = std::env::var("CARGO_CRATE_NAME").unwrap_or_else(|_| "marigold".to_string());
    marigold_grammar::marigold_parse_instrumented(
        source,
        &marigold_grammar::instrument::CodegenOptions::new(program),
    )
}

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    format!(
        "{{\n{}\n}}\n",
        generate(&s).expect("marigold parsing error")
    )
    .parse()
    .unwrap()
}
