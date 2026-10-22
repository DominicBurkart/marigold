#![forbid(unsafe_code)]

//! # Marigold Grammar Library
//!
//! This crate provides the grammar and parser infrastructure for the Marigold DSL.
//!
//! ## Overview
//!
//! Marigold is a domain-specific language for expressing stream processing programs in Rust.
//! This library provides:
//!
//! - **Pest-based parser**: Fast, maintainable PEG parser
//! - **AST definitions**: Complete abstract syntax tree for Marigold programs
//! - **Code generation**: Transforms Marigold programs into valid Rust code
//! - **Source maps**: [`marigold_generate`] returns that code together with a
//!   [`source_map::SourceMap`] from generated Rust back to Marigold source and forward
//! - **Diagnostic remapping**: [`remap::Remapper`] maps rustc's JSON diagnostics
//!   ([`rustc_json`]) and run-time locations in the generated Rust back to Marigold source
//!
//! ## Quick Start
//!
//! Parse a Marigold program:
//!
//! ```ignore
//! use marigold_grammar::parser::parse_marigold;
//!
//! let code = parse_marigold("range(0, 100).return")?;
//! println!("{}", code);
//! ```
//!
//! ## Architecture
//!
//! ### Parser
//!
//! The [`parser`] module provides the Pest-based parser with a trait abstraction
//! for extensibility. The factory function [`parser::get_parser()`] returns a
//! parser instance.
//!
//! ### Grammar File
//!
//! - **Pest**: `src/marigold.pest` - Defines the complete Marigold language grammar
//!
//! ### AST and Code Generation
//!
//! - [`nodes`]: AST node definitions for all Marigold constructs
//! - Code generation: Internal function that transforms AST to Rust code
//! - [`pest_ast_builder`]: Transforms Pest parse trees into the AST
//! - [`source_map`]: Maps the generated Rust back to the Marigold expressions it came from
//! - [`rustc_json`]: Reads the JSON diagnostics of rustc and cargo
//! - [`remap`]: Maps those diagnostics (and panic locations) to Marigold source ranges
//! - [`emit`]: The JSON document (program, generated Rust and source map with line and
//!   column positions) printed by `marigold emit --format json`
//! - [`line_index`]: Converts byte offsets to 1-based line and column positions (bytes,
//!   Unicode scalar values or UTF-16 units) and back
//!
//! ## Testing & Validation
//!
//! The parser implementation is validated through:
//!
//! - **Unit tests** in each module covering specific functionality
//! - **Negative tests** ensuring invalid syntax is properly rejected
//! - **Integration tests** using real-world example programs
//!
//! ## Feature Flags
//!
//! - `io`: I/O features (available in other crates)
//! - `tokio`: Tokio runtime integration (available in other crates)
//! - `async-std`: async-std runtime integration (available in other crates)
//! - `otel`: `marigold_parse_instrumented` generates OpenTelemetry-instrumented code
//!
//! ## Performance Characteristics
//!
//! - **Parsing**: Completes in < 1ms for typical programs
//! - **Code generation**: Dominates runtime, scales with program complexity
//! - **Binary size**: Pest parser adds ~40KB to binary size

extern crate proc_macro;

pub use itertools;

pub mod bound_resolution;
pub mod complexity;
pub mod diagnostics;
pub mod emit;
#[cfg(feature = "otel")]
pub mod instrument;
pub mod line_index;
pub mod node_ids;
pub mod nodes;
pub mod parser;
mod recovery;
pub mod remap;
mod resolver;
pub mod rustc_json;
mod span_index;
mod sub_spans;
pub mod symbol_table;
pub mod symbols;
mod type_aggregation;

pub mod pest_ast_builder;
pub mod source_map;

/// Convenience function for parsing Marigold code
///
/// This is an alias for [`parser::parse_marigold`] that uses the appropriate parser
/// based on feature flags. It's the recommended entry point for most use cases.
///
/// # Examples
///
/// ```ignore
/// use marigold_grammar::marigold_parse;
///
/// let code = marigold_parse("range(0, 100).return")?;
/// // code is now valid Rust code ready to compile
/// ```
pub fn marigold_parse(s: &str) -> Result<String, parser::MarigoldParseError> {
    parser::parse_marigold(s)
}

/// Generate Rust code together with a map from it back to the Marigold source.
///
/// `rust` is byte-for-byte what [`marigold_parse`] returns, and this fails exactly when
/// [`marigold_parse`] fails, with the same message. Unlike [`marigold_check`], input
/// size is not limited, because [`marigold_parse`] has no limit either.
///
/// # What the offsets refer to
///
/// Source offsets in the map are valid only against the exact string passed here, and
/// generated offsets only against the returned `rust`. Inside `m!()` the macro sees
/// rustc's re-printed tokens (comments dropped, whitespace normalized), wraps the
/// generated code in `{\n` ... `\n}\n` and gives every token a call-site span, so rustc
/// diagnostics cannot be mapped through `m!()`. Mapping them requires writing `rust`
/// verbatim to a real file and compiling that. UNTESTED: no test expands `m!()` or
/// compiles `rust`, so this account of rustc's behaviour is unverified. See
/// [`source_map`] for details.
///
/// The map holds one segment per statement-level construct (stream, stream variable and its
/// runner, struct, enum, function) and, nested inside each of them, one segment per part of it:
/// the stream's input, every stream function of the chain, the output, a function's signature
/// and body, a struct's fields, an enum's variants.
///
/// ```
/// use marigold_grammar::source_map::SegmentKind;
/// use marigold_grammar::{marigold_generate, marigold_parse};
///
/// let src = "x = range(0, 5)\nx.return";
/// let program = marigold_generate(src).unwrap();
/// assert_eq!(program.rust, marigold_parse(src).unwrap());
///
/// let kinds: Vec<_> = program.map.segments().iter().map(|s| s.kind).collect();
/// assert_eq!(
///     kinds,
///     [
///         SegmentKind::StreamVariable,
///         SegmentKind::InputRange,
///         SegmentKind::ReturningStream,
///         SegmentKind::InputVariable,
///         SegmentKind::StreamVariableRunner,
///     ]
/// );
///
/// let output = program.map.top_level().nth(1).unwrap();
/// assert_eq!(&src[output.source.start..output.source.end], "x.return");
/// assert_eq!(output.ident.as_deref(), Some("returning_stream_0"));
/// assert!(program.rust[output.generated.start..output.generated.end]
///     .starts_with("let returning_stream_0"));
/// assert_eq!(program.map.source_for_generated(output.generated.start), Some(output));
///
/// // the innermost segment wins: a byte of `x.get()` maps to the variable reference
/// let get = program.rust.find("x.get()").unwrap();
/// let part = program.map.source_for_generated(get).unwrap();
/// assert_eq!(part.kind, SegmentKind::InputVariable);
/// assert_eq!(&src[part.source.start..part.source.end], "x");
///
/// assert!(marigold_generate("range(0,").is_err());
/// ```
pub fn marigold_generate(
    s: &str,
) -> Result<source_map::GeneratedProgram, parser::MarigoldParseError> {
    parser::PestParser::generate_input(s).map_err(parser::MarigoldParseError)
}

/// Generate Rust code in which every input, stream function and output is wrapped with the
/// OpenTelemetry adapters of `marigold_impl::telemetry`.
///
/// Available with the `otel` feature. [`marigold_parse`] is unaffected by the feature. The node
/// ids embedded in the code are the ones [`marigold_node_ids`] returns for
/// [`instrument::CodegenOptions::program`].
///
/// ```
/// use marigold_grammar::instrument::CodegenOptions;
/// use marigold_grammar::{marigold_node_ids, marigold_parse_instrumented};
///
/// let src = "range(0, 5).map(double).return";
/// let code = marigold_parse_instrumented(src, &CodegenOptions::new("demo")).unwrap();
/// for node in marigold_node_ids(src, "demo") {
///     assert!(code.contains(node.id.as_str()));
/// }
/// assert!(code.contains("telemetry::instrument("));
/// ```
#[cfg(feature = "otel")]
pub fn marigold_parse_instrumented(
    s: &str,
    options: &instrument::CodegenOptions,
) -> Result<String, parser::MarigoldParseError> {
    parser::PestParser::parse_instrumented(s, options).map_err(parser::MarigoldParseError)
}

/// Like [`marigold_parse_instrumented`], together with the map back to the Marigold source.
///
/// `rust` is byte-for-byte what [`marigold_parse_instrumented`] returns for the same input and
/// options. Each instrumented statement maps to the expression it was generated from.
///
/// The map only has the statement-level segments (and their [`source_map::Segment::ident`]),
/// not the nested segments [`marigold_generate`] returns for the parts of a statement: the
/// instrumentation wraps the input and every stream function in nested adapter calls, so the
/// parts are not adjacent regions of the generated code.
#[cfg(feature = "otel")]
pub fn marigold_generate_instrumented(
    s: &str,
    options: &instrument::CodegenOptions,
) -> Result<source_map::GeneratedProgram, parser::MarigoldParseError> {
    parser::PestParser::generate_instrumented(s, options).map_err(parser::MarigoldParseError)
}

/// Check Marigold source and return every diagnostic found, with byte ranges.
///
/// No diagnostic has [`diagnostics::Severity::Error`] exactly when [`marigold_parse`]
/// succeeds. Warnings (a stream variable read before it is declared) and information
/// (stream variables, functions and structs not declared in the program, which may
/// be Rust items in scope inside `m!()`; a stream variable may be any Rust binding
/// with a `get()` method returning a stream) never affect [`marigold_parse`].
/// At most 50 resolver diagnostics are reported per file.
///
/// A syntax error anywhere in the file hides the resolver diagnostics (warnings and
/// information) for the whole file; only the syntax error is reported until it is fixed.
/// This is intended.
///
/// Input larger than [`diagnostics::MAX_CHECK_INPUT_BYTES`] is rejected with a
/// single `input-too-large` error diagnostic without being parsed.
///
/// ```
/// use marigold_grammar::marigold_check;
/// use marigold_grammar::diagnostics::Severity;
///
/// assert!(marigold_check("range(0, 10).return").is_empty());
///
/// let diags = marigold_check("range(0, 10).retur");
/// assert_eq!(diags[0].code, "syntax-error");
///
/// let diags = marigold_check("range(0, 10).map(double).return");
/// assert_eq!(diags[0].code, "undefined-fn");
/// assert_eq!(diags[0].severity, Severity::Information);
/// assert!(marigold_grammar::marigold_parse("range(0, 10).map(double).return").is_ok());
/// ```
pub fn marigold_check(s: &str) -> Vec<diagnostics::Diagnostic> {
    if s.len() > diagnostics::MAX_CHECK_INPUT_BYTES {
        return vec![diagnostics::Diagnostic::input_too_large(s)];
    }
    parser::PestParser::check(s)
}

/// Index every declaration and reference in Marigold source for navigation.
///
/// Chunks of the source that parse are indexed even when other chunks have syntax
/// errors, so go-to-definition keeps working while a file is being edited.
///
/// ```
/// use marigold_grammar::{marigold_symbols, symbols::SymbolKind};
///
/// let src = "x = range(0, 5)\nx.return";
/// let index = marigold_symbols(src);
/// assert_eq!(index.declarations[0].name, "x");
/// assert_eq!(index.declarations[0].kind, SymbolKind::StreamVariable);
/// assert_eq!(index.references.len(), 1);
///
/// let text = "x = range(0, 5)\nrange(0, 1).retur\nx.return";
/// let broken = marigold_symbols(text);
/// assert_eq!(broken.declarations[0].name, "x");
/// assert_eq!(broken.references[0].range.start, text.rfind("x.return").unwrap());
/// ```
pub fn marigold_symbols(s: &str) -> symbols::SymbolIndex {
    symbols::SymbolIndex::from_source(s)
}

pub use node_ids::{NodeId, NodeInfo, NodeKind};

/// Stable ids and content hashes for every input, stream function and output of every stream
/// expression, and for every stream variable declaration, in source order.
///
/// `program` namespaces the ids, so the same source in two programs yields different ids. Broken
/// files are recovered chunk by chunk, so intact expressions still get ids. See [`node_ids`] for
/// the hash algorithm and the identity rules.
///
/// ```
/// use marigold_grammar::marigold_node_ids;
///
/// let src = "x = range(0, 5).map(f)\nx.return";
/// let nodes = marigold_node_ids(src, "demo");
/// assert_eq!(nodes.len(), 5);
///
/// let spaced = "x = range(0, 5)\n  .map(f)\nx.return";
/// let again = marigold_node_ids(spaced, "demo");
/// let ids = |n: &[marigold_grammar::NodeInfo]| n.iter().map(|i| i.id.clone()).collect::<Vec<_>>();
/// assert_eq!(ids(&nodes), ids(&again));
/// ```
pub fn marigold_node_ids(s: &str, program: &str) -> Vec<NodeInfo> {
    node_ids::node_infos(s, program)
}

/// Complexity of every variable declaration and output stream, with its name and source range.
///
/// ```
/// use marigold_grammar::marigold_stream_complexities;
///
/// let src = "x = range(0, 5).map(f)\nx.return";
/// let nodes = marigold_stream_complexities(src).unwrap();
/// assert_eq!(nodes.len(), 2);
/// assert_eq!(nodes[0].name.as_deref(), Some("x"));
/// assert_eq!(nodes[1].name, None);
/// assert_eq!(&src[nodes[1].range.start..nodes[1].range.end], "x.return");
/// assert!(marigold_stream_complexities("range(0,").is_err());
/// ```
pub fn marigold_stream_complexities(
    s: &str,
) -> Result<Vec<complexity::NodeComplexity>, parser::MarigoldParseError> {
    parser::PestParser::stream_complexities(s)
}

pub fn marigold_analyze(
    s: &str,
) -> Result<complexity::ProgramComplexity, parser::MarigoldParseError> {
    parser::PestParser::analyze(s)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
