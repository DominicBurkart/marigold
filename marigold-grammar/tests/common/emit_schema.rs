//! Strict validator for the `marigold emit --format json` document, schema version 1.
//!
//! Shared by `marigold-grammar/tests/emit_document.rs` and `marigold/tests/emit_cli.rs`
//! (`#[path]`-included; cargo does not treat `tests/common/*.rs` as test targets).
//! Self-contained on purpose: it re-derives everything from the document's own `source` and
//! `rust` strings and does not use `LineIndex`, so it is an independent oracle for it.
//!
//! * `validate_v1`     -- exact key sets and JSON types (fails on unknown or missing fields).
//! * `check_semantics` -- ranges against the embedded texts, ordering and parent links.
#![allow(dead_code)]

use serde_json::Value;
use std::collections::BTreeSet;

pub const TOP_KEYS: [&str; 5] = ["version", "features", "source", "rust", "segments"];
pub const SEGMENT_KEYS: [&str; 6] = [
    "kind",
    "name",
    "expr_index",
    "parent",
    "source",
    "generated",
];
pub const RANGE_KEYS: [&str; 6] = [
    "start",
    "end",
    "start_line",
    "start_column",
    "end_line",
    "end_column",
];

fn keys(v: &Value) -> Result<BTreeSet<&str>, String> {
    v.as_object()
        .map(|o| o.keys().map(String::as_str).collect())
        .ok_or_else(|| format!("expected an object, got {v}"))
}

fn exact_keys(v: &Value, want: &[&str], at: &str) -> Result<(), String> {
    let got = keys(v).map_err(|e| format!("{at}: {e}"))?;
    let want: BTreeSet<&str> = want.iter().copied().collect();
    if let Some(extra) = got.difference(&want).next() {
        return Err(format!("{at}: unknown field `{extra}`"));
    }
    if let Some(missing) = want.difference(&got).next() {
        return Err(format!("{at}: missing field `{missing}`"));
    }
    Ok(())
}

/// A JSON integer that is a non-negative whole number (rejects `1.0`, `-1`, strings).
fn uint(v: &Value, at: &str) -> Result<usize, String> {
    v.as_u64()
        .map(|n| n as usize)
        .ok_or_else(|| format!("{at}: expected an unsigned integer, got {v}"))
}

fn is_snake_case(s: &str) -> bool {
    !s.is_empty()
        && s.bytes().all(|b| b.is_ascii_lowercase() || b == b'_')
        && !s.starts_with('_')
        && !s.ends_with('_')
}

pub fn validate_range(v: &Value, at: &str) -> Result<(), String> {
    exact_keys(v, &RANGE_KEYS, at)?;
    let n = |k: &str| uint(&v[k], &format!("{at}.{k}"));
    let (start, end) = (n("start")?, n("end")?);
    let (sl, sc, el, ec) = (
        n("start_line")?,
        n("start_column")?,
        n("end_line")?,
        n("end_column")?,
    );
    if start > end {
        return Err(format!("{at}: start {start} > end {end}"));
    }
    if sl == 0 || sc == 0 || el == 0 || ec == 0 {
        return Err(format!("{at}: lines and columns are 1-based"));
    }
    if (el, ec) < (sl, sc) {
        return Err(format!("{at}: end position precedes start position"));
    }
    Ok(())
}

pub fn validate_v1(doc: &Value) -> Result<(), String> {
    exact_keys(doc, &TOP_KEYS, "$")?;
    if doc["version"] != serde_json::json!(1) {
        return Err(format!(
            "$.version: expected the integer 1, got {}",
            doc["version"]
        ));
    }
    for k in ["source", "rust"] {
        if !doc[k].is_string() {
            return Err(format!("$.{k}: expected a string"));
        }
    }
    let features = doc["features"]
        .as_array()
        .ok_or("$.features: expected an array")?;
    let mut previous: Option<&str> = None;
    for (i, f) in features.iter().enumerate() {
        let name = f
            .as_str()
            .filter(|n| !n.is_empty() && n.bytes().all(|b| b.is_ascii_lowercase() || b == b'-'))
            .ok_or_else(|| format!("$.features[{i}]: expected a feature name, got {f}"))?;
        if previous.is_some_and(|p| p >= name) {
            return Err(format!("$.features: not sorted and unique at {name:?}"));
        }
        previous = Some(name);
    }
    let segments = doc["segments"]
        .as_array()
        .ok_or("$.segments: expected an array")?;
    for (i, s) in segments.iter().enumerate() {
        let at = format!("$.segments[{i}]");
        exact_keys(s, &SEGMENT_KEYS, &at)?;
        match s["kind"].as_str() {
            Some(k) if is_snake_case(k) => {}
            other => {
                return Err(format!(
                    "{at}.kind: expected snake_case string, got {other:?}"
                ))
            }
        }
        if !(s["name"].is_null() || s["name"].is_string()) {
            return Err(format!("{at}.name: expected string or null"));
        }
        uint(&s["expr_index"], &format!("{at}.expr_index"))?;
        if !s["parent"].is_null() {
            let p = uint(&s["parent"], &format!("{at}.parent"))?;
            if p >= i {
                return Err(format!("{at}.parent: {p} must precede segment {i}"));
            }
        }
        validate_range(&s["source"], &format!("{at}.source"))?;
        validate_range(&s["generated"], &format!("{at}.generated"))?;
    }
    Ok(())
}

/// Independent line/column: 1-based line, 1-based column in Unicode scalar values.
/// One pass over the chars; no index, no ASCII shortcut. O(offset): use for single probes.
pub fn naive_line_col(text: &str, offset: usize) -> (usize, usize) {
    let (mut line, mut col) = (1, 1);
    for (i, c) in text.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Same answers as `naive_line_col`, for validating many offsets of one text: a table of line
/// starts plus a `chars().count()` over the prefix of the line. Still independent of `LineIndex`.
struct Lines<'a> {
    text: &'a str,
    starts: Vec<usize>,
}

impl<'a> Lines<'a> {
    fn new(text: &'a str) -> Self {
        let mut starts = vec![0];
        starts.extend(text.match_indices('\n').map(|(i, _)| i + 1));
        Lines { text, starts }
    }
    fn line_col(&self, offset: usize) -> (usize, usize) {
        let line = self.starts.partition_point(|&s| s <= offset) - 1;
        (
            line + 1,
            self.text[self.starts[line]..offset].chars().count() + 1,
        )
    }
}

fn check_range(v: &Value, lines: &Lines, at: &str) -> Result<(), String> {
    let text = lines.text;
    let n = |k: &str| v[k].as_u64().unwrap() as usize;
    let (start, end) = (n("start"), n("end"));
    if end > text.len() {
        return Err(format!("{at}: end {end} > text length {}", text.len()));
    }
    if start >= end {
        return Err(format!("{at}: empty or inverted range {start}..{end}"));
    }
    if !text.is_char_boundary(start) || !text.is_char_boundary(end) {
        return Err(format!("{at}: {start}..{end} is not on char boundaries"));
    }
    for (off, line, col, what) in [
        (start, "start_line", "start_column", "start"),
        (end, "end_line", "end_column", "end"),
    ] {
        let want = lines.line_col(off);
        let got = (n(line), n(col));
        if got != want {
            return Err(format!(
                "{at}.{what}: line/column {got:?}, expected {want:?} for offset {off}"
            ));
        }
    }
    Ok(())
}

pub fn segment_text<'a>(doc: &'a Value, i: usize, side: &str) -> &'a str {
    let text = doc[if side == "source" { "source" } else { "rust" }]
        .as_str()
        .unwrap();
    let r = &doc["segments"][i][side];
    &text[r["start"].as_u64().unwrap() as usize..r["end"].as_u64().unwrap() as usize]
}

/// Requires `validate_v1(doc)` to have passed. Linear in the number of segments (plus text length).
pub fn check_semantics(doc: &Value) -> Result<(), String> {
    let source = Lines::new(doc["source"].as_str().unwrap());
    let rust = Lines::new(doc["rust"].as_str().unwrap());
    let segments = doc["segments"].as_array().unwrap();
    for (i, s) in segments.iter().enumerate() {
        check_range(&s["source"], &source, &format!("segments[{i}].source"))?;
        check_range(&s["generated"], &rust, &format!("segments[{i}].generated"))?;
    }
    let rng = |i: usize, side: &str| {
        let r = &segments[i][side];
        (
            r["start"].as_u64().unwrap() as usize,
            r["end"].as_u64().unwrap() as usize,
        )
    };
    for i in 1..segments.len() {
        let (a, b) = (rng(i - 1, "generated"), rng(i, "generated"));
        // pre-order of the generated side: start ascending, outer (larger end) first on ties
        if (a.0, std::cmp::Reverse(a.1)) > (b.0, std::cmp::Reverse(b.1)) {
            return Err(format!(
                "segments[{i}] is out of generated pre-order: {a:?} then {b:?}"
            ));
        }
    }
    // `parent` is the innermost earlier segment whose generated range contains the segment;
    // segments are nest-or-disjoint.
    let mut stack: Vec<usize> = Vec::new();
    for (i, segment) in segments.iter().enumerate() {
        let (s, e) = rng(i, "generated");
        while let Some(&top) = stack.last() {
            let (ps, pe) = rng(top, "generated");
            if ps <= s && e <= pe {
                break;
            }
            if s < pe {
                return Err(format!("segments[{top}] and [{i}] partially overlap"));
            }
            stack.pop();
        }
        let want = stack.last().copied();
        let got = segment["parent"].as_u64().map(|p| p as usize);
        if got != want {
            return Err(format!(
                "segments[{i}].parent is {got:?}, expected {want:?}"
            ));
        }
        if let Some(p) = got {
            let (ps, pe) = rng(p, "source");
            let (cs, ce) = rng(i, "source");
            if !(ps <= cs && ce <= pe) {
                return Err(format!(
                    "segments[{i}].source {cs}..{ce} is not inside its parent's source {ps}..{pe}"
                ));
            }
        }
        stack.push(i);
    }
    Ok(())
}
