//! Shared helpers for integration tests that need the repository's Marigold program corpus.
//!
//! Not a test target itself (cargo ignores `tests/<dir>/mod.rs`); include with `mod common;`.
#![allow(dead_code)]

use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::parser::{MarigoldPestParser, Rule};
use pest::Parser;
use std::path::PathBuf;

/// Every `.marigold` / `.mg` file in `tests/programs`, `../examples` and the repo root
/// (same search path as `check_diagnostics::corpus`), sorted for determinism.
pub fn corpus() -> Vec<PathBuf> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    for dir in [
        root.join("tests/programs"),
        root.join("../examples"),
        root.join(".."),
    ] {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let ext = path.extension().and_then(|e| e.to_str());
            if path.is_file() && matches!(ext, Some("marigold") | Some("mg")) {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

/// Independent oracle for "the range of top-level expression N": re-derives the
/// convention used by `marigold_stream_complexities` straight from the pest tree
/// (start of the `expr` pair .. start + `as_str().trim_end().len()`).
/// Returns `None` if `src` does not parse.
pub fn expr_ranges(src: &str) -> Option<Vec<ByteRange>> {
    let pairs = MarigoldPestParser::parse(Rule::program, src).ok()?;
    Some(
        pairs
            .flatten()
            .filter(|p| p.as_rule() == Rule::expr)
            .map(|p| ByteRange {
                start: p.as_span().start(),
                end: p.as_span().start() + p.as_str().trim_end().len(),
            })
            .collect(),
    )
}
