//! Integration tests for `marigold_grammar::emit::EmitDocument` (issue #400, task T3).
//!
//! `EmitDocument` is the library half of `marigold emit --format json`: it turns
//! `(source text, generated Rust, SourceMap)` into the version-1 JSON document.
//!
//! Oracles used here:
//! * `emit_schema::validate_v1` / `check_semantics` -- strict, self-contained document validator
//!   (exact key sets; ranges re-sliced out of the embedded texts; naive line/column);
//! * `marigold_parse`                         -- `rust` must be byte-identical;
//! * `common::expr_ranges`                    -- independent per-expression source range;
//! * `marigold_generate(..).map`              -- the document must be a lossless view of the map;
//! * hand-built nested `SourceMap`s           -- the only way to reach `parent != null` until T2.

mod common;
#[path = "common/emit_schema.rs"]
mod emit_schema;

use emit_schema::{check_semantics, naive_line_col, segment_text, validate_v1};
use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::emit::{EmitDocument, EmitRange, EMIT_SCHEMA_VERSION};
use marigold_grammar::line_index::LineIndex;
use marigold_grammar::source_map::{Segment, SegmentKind, SourceMap};
use marigold_grammar::{marigold_generate, marigold_parse};
use proptest::prelude::*;
use serde_json::{json, Value};

/// The grammar features this test target is built with, sorted: what `"features"` must list.
fn expected_features() -> Vec<&'static str> {
    let mut features = Vec::new();
    if cfg!(feature = "async-std") {
        features.push("async-std");
    }
    if cfg!(feature = "io") {
        features.push("io");
    }
    if cfg!(feature = "tokio") {
        features.push("tokio");
    }
    features
}

fn doc_for(src: &str) -> EmitDocument {
    let g = marigold_generate(src).unwrap_or_else(|e| panic!("{src:?} should generate: {e}"));
    EmitDocument::new(src, &g.rust, &g.map)
}

fn json_for(src: &str) -> Value {
    serde_json::to_value(doc_for(src)).unwrap()
}

/// Full battery: schema, semantics, losslessness against the map, independent ranges.
fn assert_document_ok(src: &str) {
    let g = marigold_generate(src).unwrap();
    let doc = EmitDocument::new(src, &g.rust, &g.map);
    let v = serde_json::to_value(&doc).unwrap();
    validate_v1(&v).unwrap_or_else(|e| panic!("{e} for {src:?}"));
    check_semantics(&v).unwrap_or_else(|e| panic!("{e} for {src:?}"));

    // header
    assert_eq!(v["version"], json!(1));
    assert_eq!(EMIT_SCHEMA_VERSION, 1);
    assert_eq!(v["features"], json!(expected_features()));
    assert_eq!(v["source"], json!(src));
    assert_eq!(v["rust"], json!(marigold_parse(src).unwrap()));

    // lossless view of the SourceMap, same order, same count
    let segs = g.map.segments();
    let out = v["segments"].as_array().unwrap();
    assert_eq!(out.len(), segs.len(), "{src:?}");
    let ranges = common::expr_ranges(src).unwrap();
    for (i, (s, j)) in segs.iter().zip(out).enumerate() {
        assert_eq!(j["expr_index"], json!(s.expr_index));
        assert_eq!(j["name"], json!(s.name));
        assert_eq!(
            j["kind"],
            serde_json::to_value(s.kind).unwrap(),
            "kind serialises like T1's"
        );
        for (side, r) in [("source", s.source), ("generated", s.generated)] {
            assert_eq!(j[side]["start"], json!(r.start));
            assert_eq!(j[side]["end"], json!(r.end));
        }
        // independent source range oracle (T1 convention) and re-slicing both sides:
        // a top-level segment covers its whole expression, a nested one lies inside it.
        let expr = ranges[s.expr_index];
        if j["parent"].is_null() {
            assert_eq!(segment_text(&v, i, "source"), &src[expr.start..expr.end]);
        } else {
            assert!(expr.start <= s.source.start && s.source.end <= expr.end);
        }
        assert_eq!(
            segment_text(&v, i, "generated"),
            &g.rust[s.generated.start..s.generated.end]
        );
    }
}

/// T1/T3 programs have no nesting. T2 deletes this test on purpose.
#[test]
fn every_segment_is_top_level_until_t2_adds_nesting() {
    for (_, src) in golden_programs() {
        for s in json_for(&src)["segments"].as_array().unwrap() {
            assert!(s["parent"].is_null(), "{src:?}: {s}");
        }
    }
}

// ---------------------------------------------------------------------------------------------
// shape goldens
// ---------------------------------------------------------------------------------------------

#[test]
fn json_shape_and_key_order_are_pinned() {
    let src = "x = range(0, 5)\nx.return";
    let text = serde_json::to_string(&doc_for(src)).unwrap();
    let features = serde_json::to_string(&expected_features()).unwrap();
    assert!(
        text.starts_with(&format!(
            r#"{{"version":1,"features":{features},"source":"x = range(0, 5)\nx.return","rust":"async {{"#
        )),
        "{text}"
    );
    let first_segment = concat!(
        r#"{"kind":"stream_variable","name":"x","expr_index":0,"parent":null,"#,
        r#""source":{"start":0,"end":15,"start_line":1,"start_column":1,"end_line":1,"end_column":16},"#,
        r#""generated":{"start":50,"end":229,"start_line":3,"start_column":5,"end_line":3,"end_column":184}}"#,
    );
    assert!(text.contains(&format!(r#""segments":[{first_segment},{{"kind":"returning_stream","name":null,"expr_index":1,"parent":null,"source":{{"start":16,"end":24,"start_line":2,"start_column":1,"end_line":2,"end_column":9}},"#)), "{text}");
    assert!(text.ends_with("}]}"), "no trailing fields: {text}");
}

#[test]
fn kind_strings_are_snake_case_of_the_t1_variants() {
    let src = "struct S { a: u8 }\nenum E { A }\nfn f(x: i32) -> i32 { x }\nx = range(0, 3).map(f)\nx.return\nx.write_file(\"o.csv\", csv)";
    let v = json_for(src);
    let kinds: Vec<&str> = v["segments"]
        .as_array()
        .unwrap()
        .iter()
        .map(|s| s["kind"].as_str().unwrap())
        .collect();
    assert_eq!(
        kinds,
        [
            "struct",
            "enum",
            "function",
            "stream_variable",
            "returning_stream",
            "non_returning_stream",
            "stream_variable_runner"
        ]
    );
}

#[test]
fn empty_and_whitespace_only_programs_have_no_segments() {
    for src in ["", "   \n\t \r\n "] {
        let v = json_for(src);
        validate_v1(&v).unwrap();
        check_semantics(&v).unwrap();
        assert_eq!(v["segments"], json!([]), "{src:?}");
        assert_eq!(v["source"], json!(src));
        assert_eq!(v["rust"], json!(marigold_parse(src).unwrap()));
    }
}

#[test]
fn multiline_generated_segments_report_end_on_a_later_line() {
    // the stream-variable runner statement spans several generated lines
    let v = json_for("x = range(0, 5)\nx.return");
    let runner = v["segments"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["kind"] == "stream_variable_runner")
        .unwrap();
    assert!(
        runner["generated"]["end_line"].as_u64() > runner["generated"]["start_line"].as_u64(),
        "{runner}"
    );
    assert_eq!(runner["source"]["start_line"], runner["source"]["end_line"]);
}

#[test]
fn multiline_source_segments_report_end_on_a_later_line() {
    let src = "x = range(0, 5)\n  .map(f)\n  .filter(g)\nx\n  .return";
    let v = json_for(src);
    let seg = &v["segments"][0];
    assert_eq!(seg["source"]["start_line"], json!(1));
    assert_eq!(seg["source"]["end_line"], json!(3));
    assert_eq!(
        seg["source"]["end_column"],
        json!("  .filter(g)".chars().count() + 1)
    );
}

#[test]
fn unicode_and_crlf_columns_are_scalar_counts_and_offsets_are_bytes() {
    // 'é' (2 bytes) before the string, an emoji (4 bytes / 2 UTF-16 units) inside the path
    let src = "range(0, 1).write_file(\"é😀.csv\", csv)\r\nrange(2, 3).return\r\n";
    let v = json_for(src);
    validate_v1(&v).unwrap();
    check_semantics(&v).unwrap();
    let by_expr = |n: u64| {
        v["segments"]
            .as_array()
            .unwrap()
            .iter()
            .find(|s| s["expr_index"] == json!(n))
            .unwrap()
    };
    let a = &by_expr(0)["source"];
    assert_eq!(a["start"], json!(0));
    let first_len = "range(0, 1).write_file(\"é😀.csv\", csv)".len();
    assert_eq!(a["end"], json!(first_len), "bytes");
    assert_eq!(
        a["end_column"],
        json!("range(0, 1).write_file(\"é😀.csv\", csv)".chars().count() + 1),
        "scalar values"
    );
    assert!(first_len > "range(0, 1).write_file(\"é😀.csv\", csv)".chars().count());
    let b = &by_expr(1)["source"];
    assert_eq!(b["start"], json!(first_len + 2), "CRLF is two bytes");
    assert_eq!(b["start_line"], json!(2));
    assert_eq!(b["start_column"], json!(1));
}

// ---------------------------------------------------------------------------------------------
// invariants over programs
// ---------------------------------------------------------------------------------------------

fn golden_dir() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../marigold/tests/golden/emit")
}

fn golden_programs() -> Vec<(String, String)> {
    let mut out: Vec<_> = std::fs::read_dir(golden_dir())
        .expect("marigold/tests/golden/emit must exist")
        .flatten()
        .filter(|e| e.path().extension().is_some_and(|x| x == "marigold"))
        .map(|e| {
            (
                e.file_name().to_string_lossy().into_owned(),
                std::fs::read_to_string(e.path()).unwrap(),
            )
        })
        .collect();
    out.sort();
    out
}

#[test]
fn every_golden_program_yields_a_valid_document() {
    let programs = golden_programs();
    assert!(
        programs.len() >= 20,
        "golden inputs missing: {}",
        programs.len()
    );
    for (name, src) in programs {
        assert_document_ok(&src);
        let _ = name;
    }
}

#[test]
fn every_corpus_program_yields_a_valid_document() {
    let files = common::corpus();
    assert!(files.len() >= 30, "corpus too small: {}", files.len());
    for path in files {
        let src = std::fs::read_to_string(&path).unwrap();
        if marigold_parse(&src).is_ok() {
            assert_document_ok(&src);
        }
    }
}

#[test]
fn offset_arithmetic_is_relative_to_the_original_text_not_a_trimmed_copy() {
    // `marigold run` trims; `emit` must not. Surrounding ASCII whitespace changes only the
    // source side of the document, shifted by exactly the left padding in bytes.
    let program = "x = range(0, 5)\nx.return";
    let base = json_for(program);
    for (left, right) in [
        ("", ""),
        ("\n", ""),
        ("  \t", "\n\n"),
        ("\r\n\r\n  ", " \r\n"),
        ("\n\n\n", "   "),
    ] {
        let padded = format!("{left}{program}{right}");
        let v = json_for(&padded);
        validate_v1(&v).unwrap();
        check_semantics(&v).unwrap();
        assert_eq!(
            v["rust"], base["rust"],
            "padding must not change the Rust ({left:?}, {right:?})"
        );
        assert_eq!(
            v["rust"],
            json!(marigold_parse(padded.trim()).unwrap()),
            "emit(untrimmed) == what `run` would compile (trimmed)"
        );
        for (a, b) in base["segments"]
            .as_array()
            .unwrap()
            .iter()
            .zip(v["segments"].as_array().unwrap())
        {
            assert_eq!(a["generated"], b["generated"]);
            assert_eq!(
                (&a["kind"], &a["name"], &a["expr_index"]),
                (&b["kind"], &b["name"], &b["expr_index"])
            );
            assert_eq!(
                b["source"]["start"].as_u64().unwrap(),
                a["source"]["start"].as_u64().unwrap() + left.len() as u64
            );
            assert_eq!(
                b["source"]["end"].as_u64().unwrap(),
                a["source"]["end"].as_u64().unwrap() + left.len() as u64
            );
        }
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(300))]

    #[test]
    fn padded_programs_keep_rust_and_shift_source_offsets(
        left in "[ \t\r\n]{0,6}",
        right in "[ \t\r\n]{0,6}",
        which in 0usize..26,
    ) {
        let programs = golden_programs();
        let (_, src) = &programs[which % programs.len()];
        let trimmed = src.trim_matches(|c| matches!(c, ' ' | '\t' | '\r' | '\n'));
        let base = json_for(trimmed);
        let padded = format!("{left}{trimmed}{right}");
        let v = json_for(&padded);
        prop_assert_eq!(&v["rust"], &base["rust"]);
        validate_v1(&v).unwrap();
        check_semantics(&v).unwrap();
        for (a, b) in base["segments"].as_array().unwrap().iter().zip(v["segments"].as_array().unwrap()) {
            prop_assert_eq!(&a["generated"], &b["generated"]);
            prop_assert_eq!(b["source"]["start"].as_u64().unwrap(), a["source"]["start"].as_u64().unwrap() + left.len() as u64);
        }
        prop_assert_eq!(v["segments"].as_array().unwrap().len(), base["segments"].as_array().unwrap().len());
    }
}

// ---------------------------------------------------------------------------------------------
// hand-built maps: the only way to exercise `parent` before T2, and garbage-in robustness
// ---------------------------------------------------------------------------------------------

fn r(start: usize, end: usize) -> ByteRange {
    ByteRange { start, end }
}

fn seg(
    generated: (usize, usize),
    source: (usize, usize),
    kind: SegmentKind,
    expr: usize,
) -> Segment {
    Segment::new(
        r(generated.0, generated.1),
        r(source.0, source.1),
        kind,
        expr,
        None,
    )
}

#[test]
fn nested_segments_get_flat_pre_order_with_parent_indices() {
    let source = "aaaaaaaaaa\nbbbbbbbbbb\ncccccccccc";
    let rust = "0123456789\n0123456789\n0123456789\n0123456789\n";
    let map = SourceMap::new(vec![
        seg((12, 18), (14, 17), SegmentKind::StreamVariableRunner, 0), // inside the second
        seg((0, 40), (0, 31), SegmentKind::ReturningStream, 0),        // outermost
        seg((11, 30), (11, 20), SegmentKind::StreamVariable, 0),       // middle
        seg((5, 8), (2, 4), SegmentKind::Struct, 1),                   // inside outermost, sibling
        seg((41, 43), (21, 30), SegmentKind::Enum, 2),                 // disjoint, top level
    ]);
    let v = serde_json::to_value(EmitDocument::new(source, rust, &map)).unwrap();
    validate_v1(&v).unwrap();
    check_semantics(&v).unwrap();
    let parents: Vec<Value> = v["segments"]
        .as_array()
        .unwrap()
        .iter()
        .map(|s| s["parent"].clone())
        .collect();
    let starts: Vec<u64> = v["segments"]
        .as_array()
        .unwrap()
        .iter()
        .map(|s| s["generated"]["start"].as_u64().unwrap())
        .collect();
    assert_eq!(
        starts,
        [0, 5, 11, 12, 41],
        "generated pre-order, outer first"
    );
    assert_eq!(
        parents,
        [json!(null), json!(0), json!(0), json!(2), json!(null)]
    );
}

#[test]
fn equal_generated_ranges_nest_in_map_order() {
    let map = SourceMap::new(vec![
        seg((2, 6), (0, 4), SegmentKind::ReturningStream, 0),
        seg((2, 6), (1, 3), SegmentKind::Function, 0),
    ]);
    let v = serde_json::to_value(EmitDocument::new("abcdef", "abcdefgh", &map)).unwrap();
    assert_eq!(v["segments"][0]["parent"], json!(null));
    assert_eq!(v["segments"][1]["parent"], json!(0));
}

#[test]
fn partially_overlapping_segments_are_siblings_not_parent_and_child() {
    // A malformed map (T1/T2 never produce one): neither contains the other, so no parent link.
    let map = SourceMap::new(vec![
        seg((0, 10), (0, 3), SegmentKind::Struct, 0),
        seg((5, 15), (4, 8), SegmentKind::Enum, 1),
        seg((6, 9), (5, 6), SegmentKind::Function, 2),
    ]);
    let doc = EmitDocument::new("abcdefghij", "0123456789abcdef", &map);
    let parents: Vec<_> = doc.segments.iter().map(|s| s.parent).collect();
    assert_eq!(parents, [None, None, Some(1)]);
}

#[test]
fn ranges_outside_the_texts_or_inside_a_char_never_panic() {
    let source = "é😀";
    let rust = "日本";
    let map = SourceMap::new(vec![
        seg((1, 2), (1, 2), SegmentKind::Struct, 0), // inside a char on both sides
        seg((0, 1000), (0, usize::MAX), SegmentKind::Enum, 1), // far past the end
        seg(
            (usize::MAX - 1, usize::MAX),
            (3, 5),
            SegmentKind::Function,
            2,
        ),
    ]);
    let doc = EmitDocument::new(source, rust, &map);
    assert_eq!(doc.segments.len(), 3);
    let text = serde_json::to_string(&doc).unwrap();
    assert!(text.starts_with(r#"{"version":1"#));
    // clamped, not wrapped: the far-past-the-end range reports the end of the text
    let far = doc.segments.iter().find(|s| s.expr_index == 1).unwrap();
    assert_eq!((far.source.end_line, far.source.end_column), (1, 3));
    assert_eq!((far.generated.end_line, far.generated.end_column), (1, 3));
}

#[test]
fn an_empty_map_makes_a_header_only_document() {
    let v =
        serde_json::to_value(EmitDocument::new("src", "rust", &SourceMap::new(vec![]))).unwrap();
    validate_v1(&v).unwrap();
    assert_eq!(
        v,
        json!({"version": 1, "features": expected_features(), "source": "src", "rust": "rust", "segments": []})
    );
}

// ---------------------------------------------------------------------------------------------
// the validator must be able to fail (otherwise every schema test above is vacuous)
// ---------------------------------------------------------------------------------------------

type Corrupt = Box<dyn Fn(&mut Value)>;

fn corruptions() -> Vec<(&'static str, Corrupt)> {
    let mut c: Vec<(&'static str, Corrupt)> = vec![
        ("version 2", Box::new(|v| v["version"] = json!(2))),
        ("version as string", Box::new(|v| v["version"] = json!("1"))),
        ("version as float", Box::new(|v| v["version"] = json!(1.0))),
        ("unknown top-level key", Box::new(|v| v["extra"] = json!(1))),
        (
            "features missing",
            Box::new(|v| {
                v.as_object_mut().unwrap().remove("features");
            }),
        ),
        (
            "features not an array",
            Box::new(|v| v["features"] = json!("io")),
        ),
        (
            "feature not a string",
            Box::new(|v| v["features"] = json!([1])),
        ),
        (
            "feature name in the wrong case",
            Box::new(|v| v["features"] = json!(["Io"])),
        ),
        (
            "features unsorted",
            Box::new(|v| v["features"] = json!(["tokio", "io"])),
        ),
        (
            "features repeated",
            Box::new(|v| v["features"] = json!(["io", "io"])),
        ),
        (
            "unknown segment key",
            Box::new(|v| v["segments"][0]["extra"] = json!(1)),
        ),
        (
            "unknown range key",
            Box::new(|v| v["segments"][0]["source"]["extra"] = json!(1)),
        ),
        (
            "unknown generated range key",
            Box::new(|v| v["segments"][0]["generated"]["extra"] = json!(1)),
        ),
        (
            "name wrong type",
            Box::new(|v| v["segments"][0]["name"] = json!(3)),
        ),
        (
            "kind not snake_case",
            Box::new(|v| v["segments"][0]["kind"] = json!("StreamVariable")),
        ),
        (
            "kind wrong type",
            Box::new(|v| v["segments"][0]["kind"] = json!(null)),
        ),
        (
            "negative offset",
            Box::new(|v| v["segments"][0]["source"]["start"] = json!(-1)),
        ),
        (
            "float offset",
            Box::new(|v| v["segments"][0]["source"]["end"] = json!(3.0)),
        ),
        (
            "zero line",
            Box::new(|v| v["segments"][0]["source"]["start_line"] = json!(0)),
        ),
        (
            "zero column",
            Box::new(|v| v["segments"][0]["generated"]["end_column"] = json!(0)),
        ),
        (
            "inverted range",
            Box::new(|v| v["segments"][0]["source"]["start"] = json!(9999)),
        ),
        (
            "parent not preceding",
            Box::new(|v| v["segments"][0]["parent"] = json!(0)),
        ),
        (
            "segments not an array",
            Box::new(|v| v["segments"] = json!({})),
        ),
        (
            "source not a string",
            Box::new(|v| v["source"] = json!(["x"])),
        ),
    ];
    for k in ["version", "source", "rust", "segments"] {
        c.push((
            Box::leak(format!("missing top-level {k}").into_boxed_str()),
            Box::new(move |v| {
                v.as_object_mut().unwrap().remove(k);
            }),
        ));
    }
    for k in [
        "kind",
        "name",
        "expr_index",
        "parent",
        "source",
        "generated",
    ] {
        c.push((
            Box::leak(format!("missing segment {k}").into_boxed_str()),
            Box::new(move |v| {
                v["segments"][0].as_object_mut().unwrap().remove(k);
            }),
        ));
    }
    for k in [
        "start",
        "end",
        "start_line",
        "start_column",
        "end_line",
        "end_column",
    ] {
        c.push((
            Box::leak(format!("missing range {k}").into_boxed_str()),
            Box::new(move |v| {
                v["segments"][0]["source"]
                    .as_object_mut()
                    .unwrap()
                    .remove(k);
            }),
        ));
        c.push((
            Box::leak(format!("missing generated {k}").into_boxed_str()),
            Box::new(move |v| {
                v["segments"][1]["generated"]
                    .as_object_mut()
                    .unwrap()
                    .remove(k);
            }),
        ));
    }
    c
}

#[test]
fn validate_v1_rejects_every_structural_corruption() {
    let good = json_for("x = range(0, 5)\nx.return");
    validate_v1(&good).unwrap();
    for (what, corrupt) in corruptions() {
        let mut bad = good.clone();
        corrupt(&mut bad);
        assert!(
            validate_v1(&bad).is_err(),
            "validator accepted a document with: {what}"
        );
    }
}

#[test]
fn check_semantics_rejects_every_semantic_corruption() {
    let good = json_for("x = range(0, 5)\ny = x.map(f)\ny.return");
    check_semantics(&good).unwrap();
    let cases: Vec<(&str, Corrupt)> = vec![
        (
            "source end shifted",
            Box::new(|v| v["segments"][0]["source"]["end"] = json!(14)),
        ),
        (
            "generated start shifted",
            Box::new(|v| v["segments"][0]["generated"]["start"] = json!(51)),
        ),
        (
            "wrong source line",
            Box::new(|v| v["segments"][1]["source"]["start_line"] = json!(1)),
        ),
        (
            "wrong source column",
            Box::new(|v| v["segments"][1]["source"]["end_column"] = json!(99)),
        ),
        (
            "wrong generated column",
            Box::new(|v| v["segments"][1]["generated"]["start_column"] = json!(2)),
        ),
        (
            "range past the text",
            Box::new(|v| v["segments"][0]["source"]["end"] = json!(10_000)),
        ),
        (
            "segments reordered",
            Box::new(|v| {
                let a = v["segments"][0].clone();
                v["segments"][0] = v["segments"][1].clone();
                v["segments"][1] = a;
            }),
        ),
        (
            "bogus parent",
            Box::new(|v| v["segments"][1]["parent"] = json!(0)),
        ),
        (
            "range inside a multibyte char",
            Box::new(|v| v["source"] = json!("é = range(0, 5)\ny = x.map(f)\ny.return")),
        ),
    ];
    for (what, corrupt) in cases {
        let mut bad = good.clone();
        corrupt(&mut bad);
        // structural validity is not the point: keep `validate_v1` happy where possible
        assert!(
            validate_v1(&bad).is_err() || check_semantics(&bad).is_err(),
            "validators accepted a document with: {what}"
        );
        if validate_v1(&bad).is_ok() {
            assert!(
                check_semantics(&bad).is_err(),
                "check_semantics accepted: {what}"
            );
        }
    }
}

// ---------------------------------------------------------------------------------------------
// properties on generated programs
// ---------------------------------------------------------------------------------------------

fn arb_chunk(i: usize) -> impl Strategy<Value = String> {
    prop_oneof![
        Just(format!("v{i} = range(0, {i})")),
        Just(format!("v{i} = range(0, 3)\n  .map(f{i})")),
        Just(format!("range(0, {i}).return")),
        Just(format!("range(0, {i}).write_file(\"é{i}😀.csv\", csv)")),
        Just(format!("struct S{i} {{ a: u8 }}")),
        Just(format!("enum E{i} {{ A, B = \"日本\" }}")),
        Just(format!("fn g{i}(x: i32) -> i32 {{ /* é😀 */ x }}")),
    ]
}

fn arb_program() -> impl Strategy<Value = String> {
    (0usize..10)
        .prop_flat_map(|n| {
            let chunks: Vec<_> = (0..n).map(arb_chunk).collect();
            (
                chunks,
                proptest::collection::vec(
                    prop_oneof![
                        Just("\n"),
                        Just("\r\n"),
                        Just(" "),
                        Just("\n\n\t "),
                        Just("  \r\n \r\n")
                    ],
                    n + 1,
                ),
            )
        })
        .prop_map(|(chunks, seps)| {
            let mut out = seps[0].to_string();
            for (c, s) in chunks.iter().zip(&seps[1..]) {
                out.push_str(c);
                out.push_str(s);
            }
            out
        })
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(300))]

    #[test]
    fn generated_programs_give_valid_documents(src in arb_program()) {
        prop_assert!(marigold_parse(&src).is_ok(), "generator produced an invalid program: {:?}", src);
        assert_document_ok(&src);
    }

    #[test]
    fn mutated_programs_either_fail_like_parse_or_give_valid_documents(
        src in arb_program(),
        at in any::<prop::sample::Index>(),
        insert in prop::option::of("[\\PC]"),
    ) {
        let boundaries: Vec<usize> = (0..=src.len()).filter(|i| src.is_char_boundary(*i)).collect();
        let pos = boundaries[at.index(boundaries.len())];
        let mut mutated = src.clone();
        match insert {
            Some(s) => mutated.insert_str(pos, &s),
            None => { if pos < mutated.len() { mutated.remove(pos); } }
        }
        match marigold_generate(&mutated) {
            Ok(_) => assert_document_ok(&mutated),
            Err(e) => prop_assert_eq!(e.0, marigold_parse(&mutated).unwrap_err().0),
        }
    }
}

#[test]
fn naive_line_col_helper_agrees_with_a_hand_computed_case() {
    // guards the validator's own oracle
    let t = "é😀\nxy";
    assert_eq!(naive_line_col(t, 0), (1, 1));
    assert_eq!(naive_line_col(t, 2), (1, 2));
    assert_eq!(naive_line_col(t, 6), (1, 3));
    assert_eq!(naive_line_col(t, 7), (2, 1));
    assert_eq!(naive_line_col(t, 9), (2, 3));
}

// ---------------------------------------------------------------------------------------------
// deep nesting and validator meta-tests with hand-built documents
// ---------------------------------------------------------------------------------------------

fn nested(g: (usize, usize), s: (usize, usize), kind: SegmentKind) -> Segment {
    seg(g, s, kind, 0)
}

#[test]
fn six_levels_deep_chain_of_parents() {
    let map = SourceMap::new(vec![
        nested((0, 100), (0, 50), SegmentKind::ReturningStream),
        nested((10, 90), (1, 49), SegmentKind::StreamVariable),
        nested((20, 80), (2, 48), SegmentKind::Function),
        nested((30, 70), (3, 47), SegmentKind::Struct),
        nested((40, 60), (4, 46), SegmentKind::Enum),
        nested((45, 55), (5, 45), SegmentKind::Enum),
    ]);
    let doc = EmitDocument::new(&"s".repeat(60), &"r".repeat(120), &map);
    let parents: Vec<_> = doc.segments.iter().map(|s| s.parent).collect();
    assert_eq!(parents, [None, Some(0), Some(1), Some(2), Some(3), Some(4)]);
}

#[test]
fn the_features_are_the_ones_this_crate_was_built_with() {
    let doc = doc_for("range(0, 3).return");
    assert_eq!(doc.features, expected_features());
    let mut sorted = doc.features.clone();
    sorted.sort();
    assert_eq!(doc.features, sorted, "sorted, as the schema promises");
}

fn hand_built(source: &str, rust: &str, segments: Vec<Segment>) -> Value {
    serde_json::to_value(EmitDocument::new(source, rust, &SourceMap::new(segments))).unwrap()
}

const TWO_LINES: &str = "aaaaaaaaaa\nbbbbbbbbbb";
const THREE_LINES: &str = "0123456789\n0123456789\n0123456789";

#[test]
fn semantic_validator_accepts_a_nested_document() {
    let v = hand_built(
        TWO_LINES,
        THREE_LINES,
        vec![
            seg((0, 25), (0, 15), SegmentKind::ReturningStream, 0),
            seg((5, 10), (2, 6), SegmentKind::Struct, 0),
        ],
    );
    validate_v1(&v).unwrap();
    check_semantics(&v).unwrap();
}

#[test]
fn semantic_validator_rejects_an_ancestor_that_is_not_the_innermost_parent() {
    let mut v = hand_built(
        TWO_LINES,
        THREE_LINES,
        vec![
            seg((0, 25), (0, 15), SegmentKind::ReturningStream, 0),
            seg((5, 20), (2, 14), SegmentKind::StreamVariable, 0),
            seg((8, 10), (3, 6), SegmentKind::Struct, 0),
        ],
    );
    check_semantics(&v).unwrap();
    assert_eq!(v["segments"][2]["parent"], json!(1));
    v["segments"][2]["parent"] = json!(0); // an ancestor, but not the innermost one
    assert!(check_semantics(&v).is_err());
    v["segments"][2]["parent"] = json!(null); // nested, but claims to be top level
    assert!(check_semantics(&v).is_err());
}

#[test]
fn semantic_validator_rejects_an_empty_range() {
    let mut v = hand_built(
        TWO_LINES,
        "0123456789\n0123456789",
        vec![seg((0, 15), (2, 6), SegmentKind::Struct, 0)],
    );
    v["segments"][0]["source"]["end"] = json!(2);
    v["segments"][0]["source"]["end_line"] = json!(1);
    v["segments"][0]["source"]["end_column"] = json!(3);
    validate_v1(&v).unwrap(); // structurally fine
    assert!(
        check_semantics(&v).is_err(),
        "start == end must be rejected"
    );
}

#[test]
fn semantic_validator_rejects_partially_overlapping_generated_ranges() {
    let v = hand_built(
        "abcdefghij",
        "0123456789abcdef",
        vec![
            seg((0, 10), (0, 3), SegmentKind::Struct, 0),
            seg((5, 15), (4, 8), SegmentKind::Enum, 1),
        ],
    );
    validate_v1(&v).unwrap();
    assert!(check_semantics(&v).is_err(), "{v}");
}

#[test]
fn semantic_validator_rejects_a_child_source_outside_its_parents_source() {
    let v = hand_built(
        TWO_LINES,
        THREE_LINES,
        vec![
            seg((0, 25), (0, 5), SegmentKind::ReturningStream, 0),
            seg((5, 10), (6, 9), SegmentKind::Struct, 0),
        ],
    );
    validate_v1(&v).unwrap();
    assert!(check_semantics(&v).is_err(), "{v}");
}

#[test]
fn semantic_validator_rejects_offsets_inside_a_character_without_panicking() {
    let v = hand_built(
        "éa\nb",
        "0123456789",
        vec![seg((0, 5), (1, 3), SegmentKind::Struct, 0)],
    );
    validate_v1(&v).unwrap();
    let verdict = std::panic::catch_unwind(|| check_semantics(&v));
    assert!(
        matches!(verdict, Ok(Err(_))),
        "must be a clean Err, got {verdict:?}"
    );
}

#[test]
fn emit_range_converts_any_byte_range_with_a_line_index() {
    let text = "ab\n\u{e9}d";
    let index = LineIndex::new(text);
    let range = EmitRange::new(
        ByteRange {
            start: 1,
            end: text.len(),
        },
        &index,
    );
    assert_eq!(
        (range.start, range.end, range.start_line, range.start_column),
        (1, 6, 1, 2)
    );
    assert_eq!((range.end_line, range.end_column), (2, 3));
}
