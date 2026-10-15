#![forbid(unsafe_code)]

extern crate proc_macro;
use proc_macro::TokenStream;

#[cfg(not(feature = "otel"))]
fn generate(
    source: &str,
    _site: &str,
) -> Result<String, marigold_grammar::parser::MarigoldParseError> {
    marigold_grammar::marigold_parse(source)
}

#[cfg(feature = "otel")]
static SITES: marigold_grammar::instrument::SiteRegistry =
    marigold_grammar::instrument::SiteRegistry::new();

#[cfg(feature = "otel")]
fn generate(
    source: &str,
    site: &str,
) -> Result<String, marigold_grammar::parser::MarigoldParseError> {
    let program = std::env::var("CARGO_CRATE_NAME").unwrap_or_else(|_| "marigold".to_string());
    marigold_grammar::marigold_parse_instrumented(source, &SITES.options(source, &program, site))
}

fn call_site() -> String {
    let rendered = format!("{:?}", proc_macro::Span::call_site());
    match rendered.find("bytes(") {
        Some(at) => rendered[at..].to_string(),
        None => rendered,
    }
}

#[proc_macro]
pub fn marigold(item: TokenStream) -> TokenStream {
    let s = item.to_string();
    format!(
        "{{\n{}\n}}\n",
        generate(&s, &call_site()).expect("marigold parsing error")
    )
    .parse()
    .unwrap()
}
