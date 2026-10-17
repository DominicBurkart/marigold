//! Integration tests for `marigold_grammar::remap` and `marigold_grammar::rustc_json` (issue #400, T4).
//!
//! Ground truth: `tests/fixtures/remap/*` were produced by REALLY compiling Marigold-generated Rust with
//! cargo/rustc 1.97 (`--message-format=json`); each directory is a frozen, self-consistent set
//! (Marigold program, generated Rust, its source map, rustc's JSON, the file layout). Tests never
//! regenerate them, so later codegen changes (T2) cannot invalidate them.
mod common;

use common::corpus;
use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::marigold_generate;
use marigold_grammar::remap::*;
use marigold_grammar::rustc_json::*;
use marigold_grammar::source_map::*;
use proptest::prelude::*;
use serde_json::{json, Value};
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------------------------

fn fixtures_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/remap")
}

fn read(name: &str, file: &str) -> String {
    std::fs::read_to_string(fixtures_root().join(name).join(file))
        .unwrap_or_else(|e| panic!("{name}/{file}: {e}"))
}

fn kind_of(s: &str) -> SegmentKind {
    match s {
        "struct" => SegmentKind::Struct,
        "enum" => SegmentKind::Enum,
        "function" => SegmentKind::Function,
        "stream_variable" => SegmentKind::StreamVariable,
        "stream_variable_runner" => SegmentKind::StreamVariableRunner,
        "returning_stream" => SegmentKind::ReturningStream,
        "non_returning_stream" => SegmentKind::NonReturningStream,
        other => panic!("unknown segment kind {other}"),
    }
}

fn range_of(v: &Value) -> ByteRange {
    ByteRange {
        start: v["start"].as_u64().unwrap() as usize,
        end: v["end"].as_u64().unwrap() as usize,
    }
}

fn segments_from_json(map: &Value) -> Vec<Segment> {
    map["segments"]
        .as_array()
        .unwrap()
        .iter()
        .map(|s| {
            Segment::new(
                range_of(&s["generated"]),
                range_of(&s["source"]),
                kind_of(s["kind"].as_str().unwrap()),
                s["expr_index"].as_u64().unwrap() as usize,
                s["name"].as_str().map(str::to_string),
            )
        })
        .collect()
}

/// A frozen fixture: the Marigold program, its generated Rust + map, rustc's bare diagnostics.
struct Fx {
    name: String,
    src: String,
    program: GeneratedProgram,
    /// `file_name` rustc used for the file that holds the generated Rust.
    generated_file: String,
    /// Bytes of the generated file before `program.rust`.
    prefix: String,
    diags: Vec<RustcDiagnostic>,
}

fn load(name: &str) -> Fx {
    let layout: Value = serde_json::from_str(&read(name, "layout.json")).unwrap();
    let map: Value = serde_json::from_str(&read(name, "map.json")).unwrap();
    let parsed = parse_messages(&read(name, "bare.jsonl"));
    assert!(parsed.errors.is_empty(), "{name}: {:?}", parsed.errors);
    Fx {
        name: name.to_string(),
        src: read(name, "program.marigold"),
        program: GeneratedProgram::new(
            read(name, "generated.rs.txt"),
            SourceMap::new(segments_from_json(&map)),
        ),
        generated_file: layout["generated_file"].as_str().unwrap().to_string(),
        prefix: layout["prefix"].as_str().unwrap().to_string(),
        diags: parsed.messages.into_iter().map(|m| m.diagnostic).collect(),
    }
}

/// Binds `$r` to a `Remapper` for fixture `$fx` (file test = rustc's `file_name` equals the layout's).
macro_rules! remapper {
    ($fx:expr, $r:ident) => {
        let __is_gen = |f: &str| f == $fx.generated_file;
        let $r = Remapper::new(&$fx.program, &$fx.src, &__is_gen).with_prefix(&$fx.prefix);
    };
}

fn find<'a>(fx: &'a Fx, needle: &str) -> &'a RustcDiagnostic {
    let hits: Vec<_> = fx
        .diags
        .iter()
        .filter(|d| d.message.contains(needle))
        .collect();
    assert_eq!(
        hits.len(),
        1,
        "{}: {needle:?} matched {}",
        fx.name,
        hits.len()
    );
    hits[0]
}

fn slice<'a>(src: &'a str, s: &MappedSpan) -> &'a str {
    let r = s.marigold.as_ref().expect("mapped").range;
    &src[r.start..r.end]
}

/// Expected: precision, Marigold slice, 1-based start (line, column).
fn assert_mapped(src: &str, s: &MappedSpan, p: Precision, text: &str, at: (usize, usize)) {
    assert_eq!(s.precision, p, "{s:?}");
    assert_eq!(slice(src, s), text, "{s:?}");
    let loc = s.marigold.as_ref().unwrap();
    assert_eq!((loc.start.line, loc.start.column), at, "{s:?}");
}

fn assert_unmapped(s: &MappedSpan, reason: UnmappedReason) {
    assert_eq!(s.precision, Precision::Unmapped { reason }, "{s:?}");
    assert!(
        s.marigold.is_none(),
        "unmapped span must carry no range: {s:?}"
    );
}

/// Invariants that hold for EVERY mapped span, whatever the input (used by goldens and properties).
fn assert_span_invariants(src: &str, rust: &str, s: &MappedSpan) {
    match (&s.precision, &s.marigold) {
        (Precision::Unmapped { .. }, None) => {}
        (Precision::Unmapped { .. }, Some(_)) => panic!("unmapped but has a range: {s:?}"),
        (_, None) => panic!("mapped precision without a range: {s:?}"),
        (p, Some(loc)) => {
            let r = loc.range;
            assert!(
                r.start <= r.end && r.end <= src.len(),
                "range outside source: {s:?}"
            );
            assert!(
                src.is_char_boundary(r.start) && src.is_char_boundary(r.end),
                "{s:?}"
            );
            // line/column are an independent recomputation from the range
            let lc = |off: usize| {
                let before = &src[..off];
                let line = before.matches('\n').count() + 1;
                let ls = before.rfind('\n').map_or(0, |i| i + 1);
                (line, before[ls..].chars().count() + 1)
            };
            assert_eq!(lc(r.start), (loc.start.line, loc.start.column), "{s:?}");
            assert_eq!(lc(r.end), (loc.end.line, loc.end.column), "{s:?}");
            assert!(s.segment.is_some(), "mapped span names its segment: {s:?}");
            let _ = (p, rust);
        }
    }
}

fn all_spans(d: &MappedDiagnostic) -> Vec<&MappedSpan> {
    let mut v: Vec<&MappedSpan> = d.primary.iter().chain(d.secondary.iter()).collect();
    for c in &d.children {
        v.extend(all_spans(c));
    }
    v
}

/// Builds a rustc-shaped span for `[bs, be)` of `file_text` (line/column/text computed independently
/// of the code under test: scalar-value columns, `\n` as the only line break, CRLF stripped from `text`).
fn rustc_span(file: &str, file_text: &str, bs: usize, be: usize, primary: bool) -> RustcSpan {
    let lc = |off: usize| {
        let before = &file_text[..off];
        let line = before.matches('\n').count() + 1;
        let ls = before.rfind('\n').map_or(0, |i| i + 1);
        (line, before[ls..].chars().count() + 1)
    };
    let (l1, c1) = lc(bs);
    let (l2, c2) = lc(be);
    let lines: Vec<&str> = file_text.split('\n').collect();
    let text: Vec<Value> = (l1..=l2)
        .map(|l| {
            let t = lines[l - 1].strip_suffix('\r').unwrap_or(lines[l - 1]);
            let n = t.chars().count();
            let hs = if l == l1 { c1 } else { 1 };
            let he = if l == l2 { c2 } else { n + 1 };
            json!({"text": t, "highlight_start": hs, "highlight_end": he})
        })
        .collect();
    serde_json::from_value(json!({
        "file_name": file, "byte_start": bs, "byte_end": be,
        "line_start": l1, "line_end": l2, "column_start": c1, "column_end": c2,
        "is_primary": primary, "label": null, "text": text,
        "suggested_replacement": null, "suggestion_applicability": null, "expansion": null
    }))
    .unwrap()
}

fn with_expansion(mut s: RustcSpan, call_site: RustcSpan, name: &str) -> RustcSpan {
    let mut e = RustcExpansion::default();
    e.span = call_site;
    e.macro_decl_name = name.to_string();
    s.expansion = Some(Box::new(e));
    s
}

fn span_to_json(s: &RustcSpan) -> Value {
    json!({
        "file_name": s.file_name, "byte_start": s.byte_start, "byte_end": s.byte_end,
        "line_start": s.line_start, "line_end": s.line_end,
        "column_start": s.column_start, "column_end": s.column_end,
        "is_primary": s.is_primary, "label": s.label,
        "text": s.text.iter().map(|t| json!({"text": t.text, "highlight_start": t.highlight_start, "highlight_end": t.highlight_end})).collect::<Vec<_>>(),
        "suggested_replacement": s.suggested_replacement, "suggestion_applicability": s.suggestion_applicability,
        "expansion": s.expansion.as_ref().map(|e| json!({"span": span_to_json(&e.span), "macro_decl_name": e.macro_decl_name, "def_site_span": null})),
    })
}

fn diag(spans: Vec<RustcSpan>) -> RustcDiagnostic {
    let spans: Vec<Value> = spans.iter().map(span_to_json).collect();
    serde_json::from_value(json!({
        "message": "m", "code": null, "level": "error", "spans": spans, "children": [], "rendered": null
    }))
    .unwrap()
}

/// A tiny program generated by the real pipeline (offsets are derived, never hard-coded).
const MINI_SRC: &str = "fn f(x: i32) -> i32 { x + 1 }\nxs = range(0, 3).map(f)\nxs.write_file(\"o.csv\", csv)\nrange(0, 2).write_file(\"p.csv\", csv)";

fn mini() -> (String, GeneratedProgram) {
    (MINI_SRC.to_string(), marigold_generate(MINI_SRC).unwrap())
}

fn seg_of(p: &GeneratedProgram, kind: SegmentKind, expr: usize) -> Segment {
    p.map
        .segments()
        .iter()
        .find(|s| s.kind == kind && s.expr_index == expr)
        .unwrap_or_else(|| panic!("no {kind:?} {expr}"))
        .clone()
}

fn map_one(src: &str, program: &GeneratedProgram, prefix: &str, span: &RustcSpan) -> MappedSpan {
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(program, src, &is_gen).with_prefix(prefix);
    r.map_span(span)
}

// ---------------------------------------------------------------------------------------------
// A. rustc / cargo JSON reader
// ---------------------------------------------------------------------------------------------

#[test]
fn reads_every_real_cargo_sample_and_every_bare_fixture() {
    for entry in std::fs::read_dir(fixtures_root().join("research")).unwrap() {
        let p = entry.unwrap().path();
        if p.extension().and_then(|e| e.to_str()) != Some("json") {
            continue;
        }
        // the research samples are pretty-printed cargo wrappers: one JSON document, many lines
        let text = std::fs::read_to_string(&p).unwrap();
        let m = parse_message_line(&text)
            .unwrap_or_else(|e| panic!("{p:?}: {e}"))
            .unwrap_or_else(|| panic!("{p:?}: not recognised as a diagnostic"));
        assert!(m.package_id.is_some() && m.manifest_path.is_some(), "{p:?}");
        assert!(!m.diagnostic.message.is_empty());
    }
    for dir in std::fs::read_dir(fixtures_root()).unwrap() {
        let dir = dir.unwrap().path();
        if !dir.join("bare.jsonl").exists() {
            continue;
        }
        let name = dir.file_name().unwrap().to_str().unwrap().to_string();
        let bare = parse_messages(&read(&name, "bare.jsonl"));
        let cargo = parse_messages(&read(&name, "cargo.jsonl"));
        assert!(bare.errors.is_empty() && cargo.errors.is_empty(), "{name}");
        assert_eq!(bare.messages.len(), cargo.messages.len(), "{name}");
        assert!(!bare.messages.is_empty(), "{name}");
        for (b, c) in bare.messages.iter().zip(&cargo.messages) {
            // the cargo wrapper carries exactly the bare diagnostic
            assert_eq!(b.diagnostic, c.diagnostic, "{name}");
            assert!(b.package_id.is_none() && b.manifest_path.is_none());
            assert_eq!(
                c.manifest_path.as_deref(),
                Some("/work/fx/Cargo.toml"),
                "{name}"
            );
        }
    }
}

#[test]
fn span_fields_round_trip_the_real_rustc_units() {
    // E0308 on a line with multibyte text: byte_* are bytes, column_* are scalar values.
    let m = parse_message_line(&read("research", "E0308_unicode_line.json"))
        .unwrap()
        .unwrap();
    let d = m.diagnostic;
    assert_eq!(d.level, Level::Error);
    assert_eq!(d.code.as_ref().unwrap().code, "E0308");
    let p = d.spans.iter().find(|s| s.is_primary).unwrap();
    assert_eq!((p.byte_start, p.byte_end), (255, 258));
    assert_eq!((p.line_start, p.column_start, p.column_end), (7, 38, 41));
    assert_eq!(p.label.as_deref(), Some("expected `u32`, found `&str`"));
    assert_eq!(p.text.len(), 1);
    assert_eq!(
        (p.text[0].highlight_start, p.text[0].highlight_end),
        (38, 41)
    );
    // the label line holds `é→` (2 + 3 bytes for 2 scalars): bytes and scalars really differ here
    let line_start = p.byte_start
        - p.text[0]
            .text
            .char_indices()
            .nth(p.column_start - 1)
            .unwrap()
            .0;
    assert_eq!(
        p.byte_start - line_start,
        p.column_start - 1 + 3,
        "3 extra bytes before the span on its line"
    );
}

#[test]
fn expansion_chains_nest_and_children_keep_their_spans() {
    let m = parse_message_line(&read("research", "E0382_borrow_moved.json"))
        .unwrap()
        .unwrap();
    let primary = m.diagnostic.spans.iter().find(|s| s.is_primary).unwrap();
    let mut names = vec![];
    let mut cur = primary.expansion.as_deref();
    while let Some(e) = cur {
        names.push(e.macro_decl_name.clone());
        cur = e.span.expansion.as_deref();
    }
    assert_eq!(
        names,
        [
            "desugaring of format string literal",
            "$crate::format_args_nl!",
            "println!"
        ]
    );
    let help = &m.diagnostic.children[0];
    assert_eq!(help.level, Level::Help);
    assert_eq!(
        help.spans[0].suggested_replacement.as_deref(),
        Some(".clone()")
    );
    assert_eq!(
        (help.spans[0].byte_start, help.spans[0].byte_end),
        (146, 146)
    );
}

#[test]
fn level_strings_are_preserved_even_when_unknown() {
    for (s, l) in [
        ("error", Level::Error),
        ("warning", Level::Warning),
        ("note", Level::Note),
        ("help", Level::Help),
        ("failure-note", Level::FailureNote),
        ("error: internal compiler error", Level::Ice),
        ("future-level", Level::Other("future-level".into())),
    ] {
        let line =
            json!({"$message_type":"diagnostic","message":"m","level":s,"spans":[],"children":[]})
                .to_string();
        let d = parse_message_line(&line).unwrap().unwrap().diagnostic;
        assert_eq!(d.level, l);
        assert_eq!(d.level.as_str(), s);
        assert_eq!(serde_json::to_value(&d.level).unwrap(), json!(s));
    }
}

#[test]
fn unknown_fields_are_ignored_and_missing_optional_fields_default() {
    let line = r#"{"$message_type":"diagnostic","message":"x","level":"warning","brand_new":{"a":[1,2]},
        "spans":[{"file_name":"a.rs","byte_start":1,"byte_end":2,"zzz":true}],"children":[{"message":"c","level":"note"}]}"#;
    let d = parse_message_line(line).unwrap().unwrap().diagnostic;
    assert_eq!(d.spans[0].file_name, "a.rs");
    assert_eq!((d.spans[0].byte_start, d.spans[0].byte_end), (1, 2));
    assert!(!d.spans[0].is_primary && d.spans[0].label.is_none() && d.spans[0].text.is_empty());
    assert!(d.spans[0].expansion.is_none());
    assert!(d.code.is_none() && d.rendered.is_none());
    assert_eq!(d.children[0].message, "c");
    assert!(d.children[0].spans.is_empty());
    // rustc before `$message_type` existed: recognised by shape
    let old =
        r#"{"message":"x","code":null,"level":"error","spans":[],"children":[],"rendered":"r"}"#;
    assert_eq!(
        parse_message_line(old)
            .unwrap()
            .unwrap()
            .diagnostic
            .rendered
            .as_deref(),
        Some("r")
    );
}

#[test]
fn non_diagnostic_lines_are_skipped_not_errors() {
    let text = [
        r#"{"reason":"compiler-artifact","package_id":"p","target":{},"filenames":[]}"#,
        r#"{"reason":"build-script-executed","package_id":"p"}"#,
        r#"{"reason":"build-finished","success":false}"#,
        r#"{"$message_type":"artifact","artifact":"x","emit":"metadata"}"#,
        r#"{"$message_type":"future_incompat","future_incompat_report":[]}"#,
        "",
        "   ",
    ]
    .join("\n");
    let p = parse_messages(&text);
    assert!(p.messages.is_empty() && p.errors.is_empty(), "{p:?}");
    assert_eq!(p.skipped, 5);
}

#[test]
fn garbage_lines_are_reported_with_their_line_number_and_do_not_stop_the_stream() {
    let good =
        r#"{"$message_type":"diagnostic","message":"ok","level":"error","spans":[],"children":[]}"#;
    let text = format!(
        "{good}\nerror: could not compile `fx` (bin \"fx\") due to 1 previous error\n{{\"reason\":\n[1,2]\n42\n{good}\n"
    );
    let p = parse_messages(&text);
    assert_eq!(p.messages.len(), 2);
    let lines: Vec<usize> = p.errors.iter().map(|e| e.line).collect();
    assert_eq!(lines, [2, 3, 4, 5]);
    assert!(parse_message_line("{").is_err());
    assert!(parse_message_line("null").is_err());
    assert!(parse_message_line(r#"{"reason":"compiler-message"}"#).is_err());
    assert!(parse_message_line(r#"{"reason":"compiler-message","message":"str"}"#).is_err());
    assert!(parse_message_line(
        &json!({"$message_type":"diagnostic","message":5,"level":"error","spans":[]}).to_string()
    )
    .is_err());
    assert!(parse_message_line(&json!({"$message_type":"diagnostic","message":"m","level":"error","spans":[{"byte_start":-1}]}).to_string()).is_err());
}

#[test]
fn pathologically_nested_json_is_an_error_not_a_stack_overflow() {
    let deep = format!("{}1{}", "[".repeat(100_000), "]".repeat(100_000));
    assert!(parse_message_line(&deep).is_err());
    let mut s =
        String::from(r#"{"$message_type":"diagnostic","message":"m","level":"error","spans":["#);
    let n = 5_000;
    for _ in 0..n {
        s.push_str(r#"{"file_name":"a","expansion":{"macro_decl_name":"m","span":"#);
    }
    s.push_str("null");
    for _ in 0..n {
        s.push_str("}}");
    }
    s.push_str("]}");
    assert!(parse_message_line(&s).is_err());
}

// ---------------------------------------------------------------------------------------------
// B. goldens against REAL rustc output (expected Marigold slices verified by hand, see TESTSPEC-T4.md)
// ---------------------------------------------------------------------------------------------

use Precision::{Exact, Expression, Segment as Seg};

#[test]
fn g01_undefined_fn_in_a_stream_maps_to_the_whole_expression() {
    let fx = load("f01_undefined_fn");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "cannot find value `undefined_fn`"));
    assert_eq!(m.level, Level::Error);
    assert_eq!(m.code.as_deref(), Some("E0425"));
    let p = m.primary.as_ref().unwrap();
    assert_mapped(
        &fx.src,
        p,
        Seg,
        "range(0, 10).map(undefined_fn).write_file(\"out.csv\", csv)",
        (1, 1),
    );
    let sg = p.segment.as_ref().unwrap();
    assert_eq!(
        (sg.kind, sg.expr_index, sg.name.as_deref()),
        (SegmentKind::NonReturningStream, 0, None)
    );
    assert_eq!(p.label.as_deref(), Some("not found in this scope"));
    assert!(
        m.rendered.as_ref().unwrap().contains("src/gen.rs:35:72"),
        "rendered text still talks about generated code"
    );
    let raw = p.raw.as_ref().unwrap();
    assert_eq!(
        (
            raw.file_name.as_str(),
            raw.byte_start,
            raw.byte_end,
            raw.line_start,
            raw.column_start
        ),
        ("src/gen.rs", 1549, 1561, 35, 72)
    );
    // the trailing "For more information..." record has no span at all
    let last = r.remap(fx.diags.last().unwrap());
    assert_eq!(last.level, Level::FailureNote);
    assert!(last.primary.is_none() && last.secondary.is_empty() && last.anchor().is_none());
}

#[test]
fn g02_error_in_second_stage_still_maps_to_its_own_expression_not_the_function() {
    let fx = load("f02_second_stage_undefined");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "also_missing"));
    let p = m.primary.as_ref().unwrap();
    assert_mapped(
        &fx.src,
        p,
        Seg,
        "range(0, 10).map(double).filter(also_missing).write_file(\"out.csv\", csv)",
        (2, 1),
    );
    assert_eq!(p.segment.as_ref().unwrap().expr_index, 1);
}

#[test]
fn g03_type_error_inside_fn_body_is_exact_and_byte_identical() {
    let fx = load("f03_type_error_in_fn_body");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "cannot add `&str` to `i32`"));
    let p = m.primary.as_ref().unwrap();
    assert_mapped(&fx.src, p, Exact, "+", (2, 7));
    let sg = p.segment.as_ref().unwrap();
    assert_eq!(
        (sg.kind, sg.name.as_deref()),
        (SegmentKind::Function, Some("is_big"))
    );
    // children: the trait-impl notes point into libcore, which is not the generated file
    let foreign: Vec<_> = m.children.iter().flat_map(|c| all_spans(c)).collect();
    assert_eq!(foreign.len(), 4);
    for s in foreign {
        assert_unmapped(s, UnmappedReason::ForeignFile);
    }
    // the diagnostic is anchored on its primary span
    assert_eq!(
        m.anchor().unwrap().marigold.as_ref().unwrap().range.start,
        fx.src.find('+').unwrap()
    );
}

#[test]
fn g04_lints_on_generated_glue_are_unmapped_or_segment_level_and_flagged_as_noise() {
    let fx = load("f03_type_error_in_fn_body");
    remapper!(fx, r);
    let warns: Vec<MappedDiagnostic> = fx
        .diags
        .iter()
        .filter(|d| d.level == Level::Warning)
        .map(|d| r.remap(d))
        .collect();
    assert_eq!(
        warns.len(),
        3,
        "three `unused_mut` lints from compiler-generated code"
    );
    let mut kinds = vec![];
    for w in &warns {
        assert_eq!(w.code.as_deref(), Some("unused_mut"));
        assert!(w.likely_generated_noise(), "{w:?}");
        kinds.push(w.primary.as_ref().unwrap().precision);
    }
    kinds.sort_by_key(|p| format!("{p:?}"));
    assert_eq!(
        kinds,
        [
            Seg,
            Seg,
            Precision::Unmapped {
                reason: UnmappedReason::Glue
            }
        ]
    );
    // the same lint inside user-written text is NOT noise (and its suggestion is applicable to Marigold)
    let fx = load("f09_unused_warning_in_fn");
    remapper!(fx, r);
    let w = r.remap(find(&fx, "unused variable: `unused`"));
    assert!(!w.likely_generated_noise());
    assert_mapped(
        &fx.src,
        w.primary.as_ref().unwrap(),
        Exact,
        "unused",
        (2, 27),
    );
    let help = &w.children[1];
    let sug = help.primary.as_ref().unwrap().suggestion.as_ref().unwrap();
    assert_eq!(sug.replacement, "_unused");
    assert!(sug.applicable_to_marigold);
    assert_mapped(
        &fx.src,
        help.primary.as_ref().unwrap(),
        Exact,
        "unused",
        (2, 27),
    );
}

#[test]
fn g05_multibyte_line_keeps_columns_in_scalar_values() {
    let fx = load("f04_multibyte_fn");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "mismatched types"));
    // line 2 of the fn is `let s = "héllo → 日本語";` (multibyte); the error is on line 3
    assert_mapped(
        &fx.src,
        m.primary.as_ref().unwrap(),
        Exact,
        "\"bad\"",
        (3, 18),
    );
    assert_mapped(&fx.src, &m.secondary[0], Exact, "u32", (3, 12));
    assert_eq!(
        m.secondary[0].label.as_deref(),
        Some("expected due to this")
    );
}

#[test]
fn g06_crlf_function_maps_exactly_and_offsets_count_the_carriage_return() {
    let fx = load("f05_crlf_fn");
    assert!(fx.src.contains("\r\n") && fx.program.rust.contains("\r\n"));
    remapper!(fx, r);
    let m = r.remap(find(&fx, "mismatched types"));
    assert_mapped(&fx.src, m.primary.as_ref().unwrap(), Exact, "x", (2, 17));
    assert_mapped(&fx.src, &m.secondary[0], Exact, "bool", (2, 10));
}

#[test]
fn g07_macro_expansion_in_a_library_file_follows_the_call_site_into_the_function() {
    let fx = load("f06_mismatched_variable_types");
    remapper!(fx, r);
    // `format!` in a `const fn`: rustc's primary span is inside liballoc; its expansion chain ends at
    // the `format!(...)` call in the generated file, which is inside the user's function.
    let m = r.remap(find(&fx, "cannot call non-const function `format`"));
    let p = m.primary.as_ref().unwrap();
    assert_mapped(&fx.src, p, Exact, "format!(\"n{x}\")", (2, 29));
    assert_eq!(p.via_macro.as_deref(), Some("format!"));
    let raw = p.raw.as_ref().unwrap();
    assert!(
        raw.file_name.contains("/library/alloc/src/macros.rs"),
        "raw keeps what rustc said"
    );
    // and the desugaring variant (two hops)
    let m2 = r.remap(find(&fx, "non-const formatting macro"));
    assert_mapped(
        &fx.src,
        m2.primary.as_ref().unwrap(),
        Exact,
        "format!(\"n{x}\")",
        (2, 29),
    );
    // a note whose span is in another crate and has no expansion stays unmapped
    let e0277 = r.remap(find(&fx, "the trait bound `String: std::marker::Copy`"));
    assert_mapped(
        &fx.src,
        e0277.primary.as_ref().unwrap(),
        Seg,
        "ys = xs.map(name)",
        (4, 1),
    );
    let sg = e0277.primary.as_ref().unwrap().segment.as_ref().unwrap();
    assert_eq!(
        (sg.kind, sg.name.as_deref()),
        (SegmentKind::StreamVariable, Some("ys"))
    );
    for s in all_spans(&e0277.children[0]) {
        assert_unmapped(s, UnmappedReason::ForeignFile);
    }
}

#[test]
fn g08_decl_and_runner_share_one_expression_and_methods_on_the_variable_map_to_the_user_of_it() {
    let fx = load("f06_mismatched_variable_types");
    remapper!(fx, r);
    let get = r.remap(find(&fx, "the method `get` exists"));
    assert_mapped(
        &fx.src,
        get.primary.as_ref().unwrap(),
        Seg,
        "ys.write_file(\"b.csv\", csv)",
        (6, 1),
    );
    let run = r.remap(find(&fx, "the method `run` exists"));
    let p = run.primary.as_ref().unwrap();
    assert_mapped(&fx.src, p, Seg, "ys = xs.map(name)", (4, 1));
    assert_eq!(
        p.segment.as_ref().unwrap().kind,
        SegmentKind::StreamVariableRunner
    );
}

#[test]
fn g09_primary_is_the_flagged_span_and_secondary_spans_keep_their_order() {
    let fx = load("f08_struct_then_undefined");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "cannot find value `mk`"));
    // rustc lists the libcore span FIRST (non-primary); the primary is the second span.
    let p = m.primary.as_ref().unwrap();
    assert!(p.is_primary);
    assert_mapped(
        &fx.src,
        p,
        Seg,
        "range(0, 3).map(mk).write_file(\"o.csv\", csv)",
        (2, 1),
    );
    assert_eq!(m.secondary.len(), 1);
    assert_unmapped(&m.secondary[0], UnmappedReason::ForeignFile);
    // rustc's "did you mean `Ok`" targets generated text: carried through, never offered for Marigold
    let help = m.children[0].primary.as_ref().unwrap();
    let sug = help.suggestion.as_ref().unwrap();
    assert_eq!(sug.replacement, "Ok");
    assert!(!sug.applicable_to_marigold);
    assert_eq!(help.precision, Seg);
}

#[test]
fn g10_variable_declared_with_select_all_maps_to_the_declaration() {
    let fx = load("f11_multi_var");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "cannot find value `nope`"));
    assert_mapped(
        &fx.src,
        m.primary.as_ref().unwrap(),
        Seg,
        "ys = select_all(range(5, 8), range(1, 2)).map(nope)",
        (2, 1),
    );
}

#[test]
fn g11_error_after_a_variable_maps_to_the_consumer_expression() {
    let fx = load("f07_undefined_after_var");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "not_defined"));
    assert_mapped(
        &fx.src,
        m.primary.as_ref().unwrap(),
        Seg,
        "xs.map(not_defined).write_file(\"a.csv\", csv)",
        (3, 1),
    );
}

#[test]
fn g12_last_byte_of_a_function_segment_is_still_exact() {
    let fx = load("f12_moved_value_clone_suggestion");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "destructor of `String`"));
    assert_mapped(&fx.src, m.primary.as_ref().unwrap(), Exact, "t", (3, 9));
    // "value is dropped here" is the closing brace: the final byte of the segment
    assert_mapped(&fx.src, &m.secondary[0], Exact, "}", (5, 1));
    let seg = seg_in(&fx, "dup");
    assert_eq!(
        m.secondary[0].raw.as_ref().unwrap().byte_end,
        seg.generated.end
    );
}

fn seg_in(fx: &Fx, name: &str) -> Segment {
    fx.program
        .map
        .segments()
        .iter()
        .find(|s| s.name.as_deref() == Some(name))
        .unwrap()
        .clone()
}

#[test]
fn g13_return_type_in_the_function_header_is_exact_too() {
    let fx = load("f13_into_suggestion");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "mismatched types"));
    assert_mapped(&fx.src, m.primary.as_ref().unwrap(), Exact, "x", (2, 5));
    assert_mapped(&fx.src, &m.secondary[0], Exact, "i64", (1, 21));
    // the generated header is `const fn widen(...)`: `const ` has no Marigold counterpart
    let seg = seg_in(&fx, "widen");
    assert!(fx.program.rust[seg.generated.start..].starts_with("const fn widen"));
}

#[test]
fn g14_std_macro_arguments_written_by_the_user_are_exact() {
    let fx = load("f14_std_macros_in_fn");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "cannot find value `undefined_thing`"));
    assert_mapped(
        &fx.src,
        m.primary.as_ref().unwrap(),
        Exact,
        "undefined_thing",
        (2, 20),
    );
    let m = r.remap(find(&fx, "mismatched types"));
    assert_mapped(
        &fx.src,
        m.primary.as_ref().unwrap(),
        Exact,
        "\"x\"",
        (3, 24),
    );
}

#[test]
fn g15_multi_line_spans_map_to_multi_line_ranges() {
    let fx = load("f15_multiline_span");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "`match` arms have incompatible types"));
    assert_mapped(
        &fx.src,
        m.primary.as_ref().unwrap(),
        Exact,
        "\"two\"",
        (5, 13),
    );
    assert_mapped(&fx.src, &m.secondary[0], Exact, "1", (3, 14));
    let whole = &m.secondary[1];
    assert_mapped(
        &fx.src,
        whole,
        Exact,
        "match x {\n        0 => 1,\n        _ => {\n            \"two\"\n        }\n    }",
        (2, 13),
    );
    let loc = whole.marigold.as_ref().unwrap();
    assert_eq!((loc.end.line, loc.end.column), (7, 6));
    assert_eq!(
        whole.raw.as_ref().unwrap().line_end,
        9,
        "generated side is 2 lines lower (preamble)"
    );
}

#[test]
fn g16_zero_width_insertion_inside_a_function_is_a_zero_width_marigold_point() {
    let fx = load("f16_missing_semicolon_zero_width");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "expected `;`, found `a`"));
    let p = m.primary.as_ref().unwrap();
    assert_eq!(p.precision, Exact);
    let loc = p.marigold.as_ref().unwrap();
    assert_eq!(
        loc.range.start, loc.range.end,
        "zero width stays zero width"
    );
    assert_eq!((loc.start.line, loc.start.column), (2, 18));
    assert_eq!(
        &fx.src[..loc.range.start].lines().last().unwrap(),
        &"    let a = x + 1"
    );
    // the same insertion repeated as the suggestion carries the replacement
    let sug = m.children[0]
        .primary
        .as_ref()
        .unwrap()
        .suggestion
        .as_ref()
        .unwrap();
    assert_eq!(sug.replacement, ";");
    assert!(sug.applicable_to_marigold);
    // secondary span `unexpected token` = the `a` on the next line
    assert_mapped(&fx.src, &m.secondary[0], Exact, "a", (3, 5));
}

#[test]
fn g17_inline_layout_with_prefix_maps_like_the_include_layout() {
    let inline = load("i01_undefined_fn_inline");
    assert!(!inline.prefix.is_empty() && inline.generated_file == "src/main.rs");
    remapper!(inline, r);
    let m = r.remap(find(&inline, "undefined_fn"));
    let p = m.primary.as_ref().unwrap();
    assert_mapped(
        &inline.src,
        p,
        Seg,
        "range(0, 10).map(undefined_fn).write_file(\"out.csv\", csv)",
        (1, 1),
    );
    // the same Marigold program, the include layout: identical Marigold range
    let inc = load("f01_undefined_fn");
    remapper!(inc, r2);
    let q = r2.remap(find(&inc, "undefined_fn")).primary.unwrap();
    assert_eq!(q.marigold, p.marigold);
    // rustc's line numbers differ by the prefix's two newlines
    assert_eq!(
        p.raw.as_ref().unwrap().line_start,
        q.raw.as_ref().unwrap().line_start + 2
    );
}

#[test]
fn g18_wrapper_spans_and_spans_that_cover_wrapper_plus_program_are_outside_the_generated_region() {
    let fx = load("i02_returning_stream_tail_await");
    remapper!(fx, r);
    // E0271: primary span is the `#[tokio::main]` attribute in the wrapper prefix
    let e0271 = r.remap(find(&fx, "to be a future that resolves to"));
    assert_unmapped(
        e0271.primary.as_ref().unwrap(),
        UnmappedReason::OutsideGeneratedRegion,
    );
    // E0308: the span is `program + "\n    .await"`: it ends past the generated region
    let e0308 = r.remap(find(&fx, "mismatched types"));
    assert_unmapped(
        e0308.primary.as_ref().unwrap(),
        UnmappedReason::OutsideGeneratedRegion,
    );
    // the secondary zero-width span sits inside the prefix as well
    assert_unmapped(&e0308.secondary[0], UnmappedReason::OutsideGeneratedRegion);
    // `use ::marigold::marigold_impl::*;` unused-import warning is inside the preamble of the only expression
    let w = r.remap(find(&fx, "unused import"));
    let p = w.primary.as_ref().unwrap();
    assert_eq!(p.precision, Seg);
    assert!(w.likely_generated_noise());
}

#[test]
fn g19_lints_in_the_wrapper_file_are_foreign() {
    let fx = load("f10_returning_stream_unused_must_use");
    remapper!(fx, r);
    let m = r.remap(find(
        &fx,
        "unused `marigold::marigold_impl::futures::stream::SelectAll`",
    ));
    assert_unmapped(m.primary.as_ref().unwrap(), UnmappedReason::ForeignFile);
    assert_eq!(
        m.primary.as_ref().unwrap().raw.as_ref().unwrap().file_name,
        "src/main.rs"
    );
    // its `let _ = ` suggestion is a zero-width insertion in the foreign file: unmapped, never Exact
    let sug = m.children[2].primary.as_ref().unwrap();
    assert_eq!(sug.suggestion.as_ref().unwrap().replacement, "let _ = ");
    assert!(!sug.suggestion.as_ref().unwrap().applicable_to_marigold);
    assert_unmapped(sug, UnmappedReason::ForeignFile);
    assert!(
        m.anchor().is_none(),
        "nothing in this diagnostic belongs to Marigold"
    );
    assert!(m.likely_generated_noise());
    // preamble `use` is glue-or-segment but never exact
    let imp = r.remap(find(&fx, "unused import"));
    assert_ne!(imp.primary.as_ref().unwrap().precision, Exact);
}

// ---------------------------------------------------------------------------------------------
// C. the research samples (other rustc situations), mapped through an identity-style synthetic map
// ---------------------------------------------------------------------------------------------

/// `t1_main.rs.txt` is a plain Rust file used for the research samples. Every `fn` line becomes a
/// Function segment whose Marigold source is the same text placed after a `// pad` line, so offsets
/// differ between the two sides while the text is byte-identical.
fn research_t1() -> (String, GeneratedProgram) {
    let rust = read("research", "t1_main.rs.txt");
    let mut segs = vec![];
    let mut src = String::new();
    let mut off = 0;
    for (i, line) in rust.split_inclusive('\n').enumerate() {
        let body = line.trim_end_matches('\n');
        if body.starts_with("fn ") && body != "fn main() {}" && body.ends_with('}') {
            src.push_str("// pad\n");
            let start = src.len();
            src.push_str(body);
            segs.push(Segment::new(
                ByteRange {
                    start: off,
                    end: off + body.len(),
                },
                ByteRange {
                    start,
                    end: src.len(),
                },
                SegmentKind::Function,
                i,
                None,
            ));
            src.push('\n');
        }
        off += line.len();
    }
    (src, GeneratedProgram::new(rust, SourceMap::new(segs)))
}

fn sample(name: &str) -> RustcDiagnostic {
    parse_message_line(&read("research", name))
        .unwrap()
        .unwrap()
        .diagnostic
}

#[test]
fn research_unicode_line_scalar_columns_vs_byte_offsets() {
    let (src, program) = research_t1();
    let is_gen = |f: &str| f == "src/main.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let m = r.remap(&sample("E0308_unicode_line.json"));
    let p = m.primary.as_ref().unwrap();
    assert_mapped(&src, p, Exact, "\"k\"", (12, 38));
}

#[test]
fn research_samples_never_panic_and_foreign_or_stale_spans_are_honest() {
    let (src, program) = research_t1();
    let is_gen = |f: &str| f == "src/main.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    for name in [
        "E0308_macro_rules_expansion.json",
        "E0382_borrow_moved.json",
        "E0425_unresolved.json",
        "warning_unused_with_children.json",
        "E0425_include_file.json",
        "E0425_inside_marigold_m_macro.json",
        "E0271_tokio_main_wrapper.json",
    ] {
        let m = r.remap(&sample(name));
        for s in all_spans(&m) {
            assert_span_invariants(&src, &program.rust, s);
        }
    }
}

/// Generalisation of `research_t1` for any Rust file: every single-line `fn ... }` becomes a Function segment.
fn research_identity(file: &str) -> (String, GeneratedProgram) {
    let rust = read("research", file);
    let mut segs = vec![];
    let mut src = String::new();
    let mut off = 0;
    for (i, line) in rust.split_inclusive('\n').enumerate() {
        let body = line.trim_end_matches('\n');
        if body.starts_with("fn ") && body != "fn main() {}" && body.ends_with('}') {
            src.push_str("// pad\n");
            let start = src.len();
            src.push_str(body);
            segs.push(Segment::new(
                ByteRange {
                    start: off,
                    end: off + body.len(),
                },
                ByteRange {
                    start,
                    end: src.len(),
                },
                SegmentKind::Function,
                i,
                None,
            ));
            src.push('\n');
        }
        off += line.len();
    }
    (src, GeneratedProgram::new(rust, SourceMap::new(segs)))
}

#[test]
fn research_e0382_expansion_chain_keeps_own_position_and_zero_width_clone_insertion() {
    let (src, program) = research_t1();
    let is_gen = |f: &str| f == "src/main.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let m = r.remap(&sample("E0382_borrow_moved.json"));
    // primary span is in the generated file but carries a 3-hop expansion into libstd: its own position wins
    let p = m.primary.as_ref().unwrap();
    assert_mapped(&src, p, Exact, "s", (6, 59));
    assert_eq!(p.via_macro, None);
    assert_mapped(&src, &m.secondary[0], Exact, "s", (6, 41));
    assert_mapped(&src, &m.secondary[1], Exact, "s", (6, 14));
    // `help: consider cloning`: zero-width insertion right after the moved `s`
    let help = m.children[0].primary.as_ref().unwrap();
    let loc = help.marigold.as_ref().unwrap();
    assert_eq!(loc.range.start, loc.range.end);
    assert_eq!((loc.start.line, loc.start.column), (6, 42));
    let sug = help.suggestion.as_ref().unwrap();
    assert_eq!(sug.replacement, ".clone()");
    assert!(sug.applicable_to_marigold);
    assert_eq!(help.precision, Exact);
}

#[test]
fn research_macro_rules_definition_outside_any_segment_is_glue_not_a_guess() {
    let (src, program) = research_t1();
    let is_gen = |f: &str| f == "src/main.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let m = r.remap(&sample("E0308_macro_rules_expansion.json"));
    // both spans sit in the `macro_rules!` line, which no segment covers; their expansion call site is `bad!()`
    // (inside `fn e`) but the span's own position is what rustc points at, so that is what is reported
    for s in all_spans(&m) {
        assert_unmapped(s, UnmappedReason::Glue);
    }
}

#[test]
fn research_include_layout_sample_maps_exactly() {
    let (src, program) = research_identity("t2_gen.rs.txt");
    let is_gen = |f: &str| f == "src/gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let m = r.remap(&sample("E0425_include_file.json"));
    assert_mapped(
        &src,
        m.primary.as_ref().unwrap(),
        Exact,
        "undefined_name",
        (2, 23),
    );
}

#[test]
fn research_wrapper_samples_are_foreign_or_outside() {
    let (src, program) = research_t1();
    let is_gen = |f: &str| f == "src/gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    for name in [
        "E0271_tokio_main_wrapper.json",
        "E0425_inside_marigold_m_macro.json",
    ] {
        let m = r.remap(&sample(name));
        assert_unmapped(m.primary.as_ref().unwrap(), UnmappedReason::ForeignFile);
        assert!(m.anchor().is_none(), "{name}");
    }
}

// ---------------------------------------------------------------------------------------------
// D. rules, one at a time, on hand-shaped spans over a REAL generated program
// ---------------------------------------------------------------------------------------------

#[test]
fn d01_every_offset_in_every_segment_maps_to_that_segments_expression() {
    let (src, program) = mini();
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    for seg in program.map.segments() {
        let g = seg.generated;
        let expr = seg.source;
        // sample start, middle, last byte (zero width and one byte)
        for off in [g.start, g.start + (g.end - g.start) / 2, g.end - 1] {
            if !program.rust.is_char_boundary(off) || !program.rust.is_char_boundary(off + 1) {
                continue;
            }
            for (a, b) in [(off, off), (off, off + 1)] {
                let sp = rustc_span("gen.rs", &program.rust, a, b, true);
                let m = r.map_span(&sp);
                let loc = m
                    .marigold
                    .as_ref()
                    .unwrap_or_else(|| panic!("{seg:?} {a}..{b}: {m:?}"));
                assert!(
                    expr.start <= loc.range.start && loc.range.end <= expr.end,
                    "{seg:?} {a}..{b} -> {:?}",
                    loc.range
                );
                assert_eq!(m.segment.as_ref().unwrap().expr_index, seg.expr_index);
            }
        }
    }
}

#[test]
fn d02_glue_between_segments_and_the_preamble_are_unmapped_glue() {
    let (src, program) = mini();
    let segs = program.map.segments();
    // first byte of the program, a separator byte between two segments, the very last byte
    let mut offs = vec![(0, 5), (10, 20)];
    for w in segs.windows(2) {
        if w[0].generated.end < w[1].generated.start {
            offs.push((w[0].generated.end, w[1].generated.start));
        }
    }
    let last = program.rust.len();
    offs.push((last - 1, last));
    for (a, b) in offs {
        let m = map_one(
            &src,
            &program,
            "",
            &rustc_span("gen.rs", &program.rust, a, b, true),
        );
        // glue-only spans are Glue unless they graze a segment
        let hits = segs
            .iter()
            .filter(|s| s.generated.start < b && s.generated.end > a)
            .count();
        if hits == 0 {
            assert_unmapped(&m, UnmappedReason::Glue);
        }
    }
    // the preamble itself
    let m = map_one(
        &src,
        &program,
        "",
        &rustc_span("gen.rs", &program.rust, 0, 5, true),
    );
    assert_unmapped(&m, UnmappedReason::Glue);
    // zero-width in glue
    let m = map_one(
        &src,
        &program,
        "",
        &rustc_span("gen.rs", &program.rust, 3, 3, true),
    );
    assert_unmapped(&m, UnmappedReason::Glue);
}

#[test]
fn d03_span_covering_two_expressions_is_unmapped_multiple_expressions() {
    let (src, program) = mini();
    let a = seg_of(&program, SegmentKind::Function, 0);
    let last = program
        .map
        .segments()
        .iter()
        .rfind(|s| s.kind == SegmentKind::NonReturningStream)
        .unwrap()
        .clone();
    let sp = rustc_span(
        "gen.rs",
        &program.rust,
        a.generated.start,
        last.generated.end,
        true,
    );
    assert_unmapped(
        &map_one(&src, &program, "", &sp),
        UnmappedReason::MultipleExpressions,
    );
    // one byte of each of two neighbouring expressions (across the `\n\n` glue)
    let b = program.map.segments()[1].clone();
    assert!(a.generated.end < b.generated.start);
    let sp = rustc_span(
        "gen.rs",
        &program.rust,
        a.generated.end - 1,
        b.generated.start + 1,
        true,
    );
    assert_unmapped(
        &map_one(&src, &program, "", &sp),
        UnmappedReason::MultipleExpressions,
    );
}

#[test]
fn d04_span_leaving_its_segment_into_glue_stays_attributed_to_that_one_expression() {
    let (src, program) = mini();
    let s = seg_of(&program, SegmentKind::NonReturningStream, 2);
    // from the middle of the statement to two bytes past its end (the `\n` glue and one more byte)
    let sp = rustc_span(
        "gen.rs",
        &program.rust,
        s.generated.end - 4,
        s.generated.end + 1,
        true,
    );
    let m = map_one(&src, &program, "", &sp);
    assert_eq!(m.precision, Expression, "{m:?}");
    assert_eq!(slice(&src, &m), "xs.write_file(\"o.csv\", csv)");
}

#[test]
fn d05_function_header_const_is_segment_level_but_the_rest_is_exact() {
    let (src, program) = mini();
    let f = seg_of(&program, SegmentKind::Function, 0);
    assert!(program.rust[f.generated.start..].starts_with("const fn f("));
    let m = map_one(
        &src,
        &program,
        "",
        &rustc_span(
            "gen.rs",
            &program.rust,
            f.generated.start,
            f.generated.start + 5,
            true,
        ),
    );
    assert_eq!(m.precision, Seg);
    assert_eq!(slice(&src, &m), "fn f(x: i32) -> i32 { x + 1 }");
    let plus = f.generated.start + program.rust[f.generated.start..].find('+').unwrap();
    let m = map_one(
        &src,
        &program,
        "",
        &rustc_span("gen.rs", &program.rust, plus, plus + 1, true),
    );
    assert_mapped(&src, &m, Exact, "+", (1, 25));
    // span covering `const fn f` (starts before the verbatim region) is Segment, not Exact
    let m = map_one(
        &src,
        &program,
        "",
        &rustc_span("gen.rs", &program.rust, f.generated.start, plus + 1, true),
    );
    assert_eq!(m.precision, Seg);
}

#[test]
fn d06_stream_internals_are_never_exact_even_when_text_happens_to_match() {
    let (src, program) = mini();
    let s = seg_of(&program, SegmentKind::StreamVariable, 1);
    for (a, b) in [
        (s.generated.start, s.generated.start + 3),
        (s.generated.end - 2, s.generated.end),
    ] {
        let m = map_one(
            &src,
            &program,
            "",
            &rustc_span("gen.rs", &program.rust, a, b, true),
        );
        assert_eq!(m.precision, Seg);
        assert_eq!(slice(&src, &m), "xs = range(0, 3).map(f)");
    }
}

fn corrupt_text(mut sp: RustcSpan, f: impl FnOnce(&mut Value)) -> RustcSpan {
    let mut v = span_to_json(&sp);
    f(&mut v);
    sp = serde_json::from_value(v).unwrap();
    sp
}

#[test]
fn d07_line_column_and_text_are_a_checksum_of_the_generated_file() {
    let (src, program) = mini();
    let f = seg_of(&program, SegmentKind::Function, 0);
    let plus = f.generated.start + program.rust[f.generated.start..].find('+').unwrap();
    let good = rustc_span("gen.rs", &program.rust, plus, plus + 1, true);
    assert_eq!(map_one(&src, &program, "", &good).precision, Exact);
    let stale = UnmappedReason::StaleGeneratedFile;
    // each of the checksummed fields, one at a time
    for field in ["line_start", "line_end", "column_start", "column_end"] {
        let sp = corrupt_text(good.clone(), |v| {
            v[field] = json!(v[field].as_u64().unwrap() + 1)
        });
        assert_unmapped(&map_one(&src, &program, "", &sp), stale);
    }
    let sp = corrupt_text(good.clone(), |v| {
        v["text"][0]["text"] = json!("something else entirely")
    });
    assert_unmapped(&map_one(&src, &program, "", &sp), stale);
    let sp = corrupt_text(good.clone(), |v| v["text"][0]["highlight_start"] = json!(1));
    assert_unmapped(&map_one(&src, &program, "", &sp), stale);
    let sp = corrupt_text(good.clone(), |v| v["text"] = json!([]));
    assert_eq!(
        map_one(&src, &program, "", &sp).precision,
        Exact,
        "no `text` = no text check"
    );
    let sp = corrupt_text(good.clone(), |v| {
        v["text"] = json!([v["text"][0].clone(), v["text"][0].clone()])
    });
    assert_unmapped(&map_one(&src, &program, "", &sp), stale);
    // the file was rustfmt'd / re-generated differently: same bytes offsets, different lines
    let mut other = program.rust.clone();
    other.insert_str(f.generated.start, "\n\n\n");
    let sp = rustc_span("gen.rs", &other, plus + 3, plus + 4, true);
    assert_unmapped(&map_one(&src, &program, "", &sp), stale);
    // byte offsets shifted by one but line/column/text still match THIS program: bytes win -> a different
    // token, but since text no longer matches the highlighted slice it is rejected
    let shifted = corrupt_text(good.clone(), |v| {
        v["byte_start"] = json!(plus + 1);
        v["byte_end"] = json!(plus + 2);
    });
    assert_unmapped(&map_one(&src, &program, "", &shifted), stale);
    // all-zero line/column means "not provided": bytes alone are used
    let bare = corrupt_text(good, |v| {
        for k in ["line_start", "line_end", "column_start", "column_end"] {
            v[k] = json!(0);
        }
        v["text"] = json!([]);
    });
    assert_eq!(map_one(&src, &program, "", &bare).precision, Exact);
}

#[test]
fn d08_invalid_spans_are_reported_never_panicked_on() {
    let (src, program) = mini();
    let len = program.rust.len();
    let base = rustc_span("gen.rs", &program.rust, 60, 61, true);
    let set = |bs: usize, be: usize| {
        corrupt_text(base.clone(), |v| {
            v["byte_start"] = json!(bs);
            v["byte_end"] = json!(be);
            v["line_start"] = json!(0);
            v["line_end"] = json!(0);
            v["column_start"] = json!(0);
            v["column_end"] = json!(0);
            v["text"] = json!([]);
        })
    };
    assert_unmapped(
        &map_one(&src, &program, "", &set(10, 5)),
        UnmappedReason::InvalidSpan,
    );
    assert_unmapped(
        &map_one(&src, &program, "", &set(len, len + 1)),
        UnmappedReason::OutsideGeneratedRegion,
    );
    assert_unmapped(
        &map_one(&src, &program, "", &set(len + 5, len + 9)),
        UnmappedReason::OutsideGeneratedRegion,
    );
    assert_unmapped(
        &map_one(&src, &program, "", &set(usize::MAX - 1, usize::MAX)),
        UnmappedReason::OutsideGeneratedRegion,
    );
    assert_unmapped(
        &map_one(&src, &program, "", &set(usize::MAX, 0)),
        UnmappedReason::InvalidSpan,
    );
    // a span ending exactly at the end of the program is fine
    assert_ne!(
        map_one(&src, &program, "", &set(len - 1, len)).precision,
        Precision::Unmapped {
            reason: UnmappedReason::InvalidSpan
        }
    );
    // not on a char boundary
    let src2 =
        "fn f(x: i32) -> bool { \"日本語\" == \"x\" }\nrange(0, 1).write_file(\"o.csv\", csv)";
    let p2 = marigold_generate(src2).unwrap();
    let at = p2.rust.find('日').unwrap();
    let sp = rustc_span("gen.rs", &p2.rust, at, at + 3, true);
    assert_eq!(map_one(src2, &p2, "", &sp).precision, Exact);
    let sp = corrupt_text(sp, |v| v["byte_start"] = json!(at + 1));
    assert_unmapped(&map_one(src2, &p2, "", &sp), UnmappedReason::InvalidSpan);
    let sp = corrupt_text(rustc_span("gen.rs", &p2.rust, at, at + 3, true), |v| {
        v["byte_end"] = json!(at + 2)
    });
    assert_unmapped(&map_one(src2, &p2, "", &sp), UnmappedReason::InvalidSpan);
}

#[test]
fn d09_a_stale_marigold_source_is_detected_instead_of_fabricating_a_range() {
    let (src, program) = mini();
    let f = seg_of(&program, SegmentKind::Function, 0);
    let plus = f.generated.start + program.rust[f.generated.start..].find('+').unwrap();
    let sp = rustc_span("gen.rs", &program.rust, plus, plus + 1, true);
    // the caller passes a shorter / different source than the map was built from
    let short = &src[..10];
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, short, &is_gen);
    assert_unmapped(&r.map_span(&sp), UnmappedReason::StaleSourceMap);
    // a source where a segment boundary falls inside a multibyte char
    let weird = "é".repeat(60);
    let r = Remapper::new(&program, &weird, &is_gen);
    let m = r.map_span(&sp);
    assert!(
        matches!(m.precision, Precision::Unmapped { .. } | Seg | Exact),
        "{m:?}"
    );
    if let Some(l) = &m.marigold {
        assert!(weird.is_char_boundary(l.range.start) && weird.is_char_boundary(l.range.end));
    }
}

#[test]
fn d10_expansion_chain_lands_in_the_generated_file_after_foreign_hops() {
    let (src, program) = mini();
    let f = seg_of(&program, SegmentKind::Function, 0);
    let plus = f.generated.start + program.rust[f.generated.start..].find('+').unwrap();
    let call = rustc_span("gen.rs", &program.rust, plus, plus + 1, false);
    let foreign = |n: &str| {
        serde_json::from_value::<RustcSpan>(json!({"file_name": n, "byte_start": 5, "byte_end": 9, "line_start": 1, "line_end": 1, "column_start": 1, "column_end": 5, "is_primary": true, "text": []})).unwrap()
    };
    // lib span -> expansion(call site in another lib) -> expansion(call site in generated file)
    let mid = with_expansion(foreign("/rustc/x/library/core/src/b.rs"), call, "inner!");
    let top = with_expansion(foreign("/rustc/x/library/core/src/a.rs"), mid, "outer!");
    let m = map_one(&src, &program, "", &top);
    assert_mapped(&src, &m, Exact, "+", (1, 25));
    assert_eq!(
        m.via_macro.as_deref(),
        Some("inner!"),
        "the hop whose call site is in the generated file"
    );
    // chain that never reaches the file
    let dead = with_expansion(foreign("/a.rs"), foreign("/b.rs"), "m!");
    assert_unmapped(
        &map_one(&src, &program, "", &dead),
        UnmappedReason::ForeignFile,
    );
    // a span in the generated file that itself has an expansion keeps its own position
    let own = with_expansion(
        rustc_span("gen.rs", &program.rust, plus, plus + 1, true),
        foreign("/c.rs"),
        "m!",
    );
    let m = map_one(&src, &program, "", &own);
    assert_mapped(&src, &m, Exact, "+", (1, 25));
    assert_eq!(m.via_macro, None);
}

#[test]
fn d11_expansion_chains_are_bounded() {
    let (src, program) = mini();
    let foreign = |n: usize| {
        let mut s = RustcSpan::default();
        s.file_name = format!("/lib{n}.rs");
        s.byte_end = 1;
        s
    };
    let f = seg_of(&program, SegmentKind::Function, 0);
    let target = rustc_span(
        "gen.rs",
        &program.rust,
        f.generated.start + 6,
        f.generated.start + 7,
        false,
    );
    let chain = |hops: usize| {
        let mut cur = target.clone();
        for i in 0..hops {
            cur = with_expansion(foreign(i), cur, "m!");
        }
        cur
    };
    // documented cap: at most 64 expansion hops are followed
    assert_eq!(
        map_one(&src, &program, "", &chain(3)).precision,
        Exact,
        "a tiny chain resolves"
    );
    assert_ne!(
        map_one(&src, &program, "", &chain(64)).precision,
        Precision::Unmapped {
            reason: UnmappedReason::ForeignFile
        }
    );
    assert_unmapped(
        &map_one(&src, &program, "", &chain(65)),
        UnmappedReason::ForeignFile,
    );
    // absurdly deep chain: still terminates (and is cheap)
    assert_unmapped(
        &map_one(&src, &program, "", &chain(5_000)),
        UnmappedReason::ForeignFile,
    );
}

#[test]
fn d12_prefix_is_respected_including_the_partial_first_line() {
    let (src, program) = mini();
    let f = seg_of(&program, SegmentKind::Function, 0);
    // prefix ends mid-line (4 spaces, multibyte char), like a wrapper that opens a block on the same line
    for prefix in [
        "",
        "// é\nfn main() {\n    ",
        "x",
        "\n",
        "日本語 ",
        "a\r\nb\r\n  ",
    ] {
        let file = format!("{prefix}{}\n    .await;\n}}\n", program.rust);
        let base = prefix.len();
        let plus = base + f.generated.start + program.rust[f.generated.start..].find('+').unwrap();
        let sp = rustc_span("gen.rs", &file, plus, plus + 1, true);
        let m = map_one(&src, &program, prefix, &sp);
        assert_mapped(&src, &m, Exact, "+", (1, 25));
        // a span on the FIRST generated line (preamble) is glue and must not be confused with the prefix
        let sp = rustc_span("gen.rs", &file, base, base + 5, true);
        assert_unmapped(&map_one(&src, &program, prefix, &sp), UnmappedReason::Glue);
        // a span inside the prefix is outside the region
        if !prefix.is_empty() {
            let first = file.chars().next().unwrap().len_utf8();
            let sp = rustc_span("gen.rs", &file, 0, first, true);
            assert_unmapped(
                &map_one(&src, &program, prefix, &sp),
                UnmappedReason::OutsideGeneratedRegion,
            );
        }
        // and a prefix the caller forgot to declare makes every span stale or out of range, never wrong
        if !prefix.is_empty() {
            let m = map_one(&src, &program, "", &sp_in_program(&file, plus));
            assert!(
                matches!(m.precision, Precision::Unmapped { .. }),
                "undeclared prefix {prefix:?} must not map: {m:?}"
            );
        }
    }
}

fn sp_in_program(file: &str, off: usize) -> RustcSpan {
    rustc_span("gen.rs", file, off, off + 1, true)
}

#[test]
fn d13_other_files_are_foreign_and_the_file_test_decides() {
    let (src, program) = mini();
    let f = seg_of(&program, SegmentKind::Function, 0);
    let sp = rustc_span(
        "other.rs",
        &program.rust,
        f.generated.start + 6,
        f.generated.start + 7,
        true,
    );
    assert_unmapped(
        &map_one(&src, &program, "", &sp),
        UnmappedReason::ForeignFile,
    );
    // the test is called with rustc's file_name verbatim
    let seen = std::cell::RefCell::new(vec![]);
    let is_gen = |f: &str| {
        seen.borrow_mut().push(f.to_string());
        f == "./src/gen.rs"
    };
    let r = Remapper::new(&program, &src, &is_gen);
    let sp = rustc_span(
        "./src/gen.rs",
        &program.rust,
        f.generated.start + 6,
        f.generated.start + 7,
        true,
    );
    assert_ne!(
        r.map_span(&sp).precision,
        Precision::Unmapped {
            reason: UnmappedReason::ForeignFile
        }
    );
    assert!(seen.borrow().iter().any(|s| s == "./src/gen.rs"));
}

#[test]
fn d14_diagnostics_without_spans_pass_through() {
    let (src, program) = mini();
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let d: RustcDiagnostic = serde_json::from_value(json!({
        "message": "aborting due to 2 previous errors", "code": null, "level": "error",
        "spans": [], "children": [], "rendered": "error: aborting due to 2 previous errors\n\n"
    }))
    .unwrap();
    let m = r.remap(&d);
    assert_eq!(m.message, d.message);
    assert_eq!(m.code, None);
    assert!(m.primary.is_none() && m.secondary.is_empty() && m.children.is_empty());
    assert_eq!(m.rendered, d.rendered);
    assert!(m.anchor().is_none());
    // when no span is flagged primary the first one is used
    let sp = rustc_span("gen.rs", &program.rust, 60, 61, false);
    let m = r.remap(&diag(vec![sp.clone(), sp]));
    assert!(m.primary.is_some() && m.secondary.len() == 1);
}

#[test]
fn d15_anchor_prefers_primary_then_secondary_then_children() {
    let (src, program) = mini();
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let f = seg_of(&program, SegmentKind::Function, 0);
    let inside = rustc_span(
        "gen.rs",
        &program.rust,
        f.generated.start + 6,
        f.generated.start + 7,
        false,
    );
    let outside = rustc_span("zzz.rs", &program.rust, 1, 2, true);
    // primary foreign, secondary mapped
    let m = r.remap(&diag(vec![outside.clone(), inside.clone()]));
    assert!(m.primary.as_ref().unwrap().marigold.is_none());
    assert_eq!(m.anchor().unwrap().precision, Exact);
    // only a child maps
    let mut v = serde_json::to_value(json!({
        "message": "m", "level": "error", "spans": [span_to_json(&outside)],
        "children": [{"message": "c", "level": "note", "spans": [span_to_json(&inside)], "children": []}]
    }))
    .unwrap();
    v["code"] = Value::Null;
    let d: RustcDiagnostic = serde_json::from_value(v).unwrap();
    assert_eq!(r.remap(&d).anchor().unwrap().precision, Exact);
}

#[test]
fn d16_mapping_is_deterministic_and_serialises_with_a_stable_shape() {
    let fx = load("f01_undefined_fn");
    remapper!(fx, r);
    let d = find(&fx, "undefined_fn");
    let a = serde_json::to_string(&r.remap(d)).unwrap();
    let b = serde_json::to_string(&r.remap(d)).unwrap();
    assert_eq!(a, b);
    let v: Value = serde_json::from_str(&a).unwrap();
    assert_eq!(v["level"], "error");
    assert_eq!(v["code"], "E0425");
    assert_eq!(v["primary"]["precision"], json!({"kind": "segment"}));
    assert_eq!(
        v["primary"]["marigold"]["range"],
        json!({"start": 0, "end": 57})
    );
    assert_eq!(
        v["primary"]["marigold"]["start"],
        json!({"line": 1, "column": 1})
    );
    assert_eq!(v["primary"]["segment"]["kind"], "non_returning_stream");
    assert_eq!(v["primary"]["raw"]["byte_start"], 1549);
    assert!(v["secondary"].as_array().unwrap().is_empty());
    let unm = r.remap(fx.diags.last().unwrap());
    assert!(serde_json::to_string(&unm).is_ok());
    let foreign = serde_json::to_value(Precision::Unmapped {
        reason: UnmappedReason::ForeignFile,
    })
    .unwrap();
    assert_eq!(
        foreign,
        json!({"kind": "unmapped", "reason": "foreign_file"})
    );
    assert_eq!(
        serde_json::to_value(Exact).unwrap(),
        json!({"kind": "exact"})
    );
}

#[test]
fn d17_lexical_path_matching_needs_no_filesystem() {
    let root = Path::new("/work/fx");
    let gen = Path::new("/work/fx/src/gen.rs");
    for ok in [
        "src/gen.rs",
        "./src/gen.rs",
        "src/../src/gen.rs",
        "/work/fx/src/gen.rs",
        "src//gen.rs",
    ] {
        assert!(lexical_path_matches(root, ok, gen), "{ok}");
    }
    for bad in [
        "src/gen2.rs",
        "gen.rs",
        "/other/src/gen.rs",
        "../src/gen.rs",
        "",
        "src/gen.rs/x",
    ] {
        assert!(!lexical_path_matches(root, bad, gen), "{bad}");
    }
    assert!(
        lexical_path_matches(root, "src/gen.rs", Path::new("src/gen.rs")),
        "relative generated path is rooted too"
    );
    assert!(!lexical_path_matches(
        Path::new("/nonexistent/zzz"),
        "a.rs",
        Path::new("/nonexistent/zzz/b.rs")
    ));
}

// ---------------------------------------------------------------------------------------------
// E. runtime locations (2b): panics, backtraces, Location::caller()
// ---------------------------------------------------------------------------------------------

#[test]
fn e01_panic_location_uses_display_columns_and_backtrace_uses_byte_columns() {
    let fx = load("r01_panic_in_fn");
    // generated line 4: `    let label = "héllo→日本"; if x == 2 { panic!("boom") }`
    let line = fx.program.rust.lines().nth(3).unwrap();
    assert!(line.contains("日本"));
    remapper!(fx, r);
    // what the program really printed (see run.stderr.txt): panic at 4:43, backtrace frame at 4:48
    let stderr = read("r01_panic_in_fn", "run.stderr.txt");
    assert!(stderr.contains("panicked at src/gen.rs:4:43:"));
    assert!(stderr.contains("at ./src/gen.rs:4:48"));
    let by_display = r.locate("src/gen.rs", 4, Some(43), ColumnUnit::DisplayWidth);
    let by_bytes = r.locate("./src/gen.rs", 4, Some(48), ColumnUnit::Utf8Bytes);
    let by_scalar = r.locate("src/gen.rs", 4, Some(41), ColumnUnit::ScalarValues);
    // (`./src/gen.rs` is not the test's generated name: the byte one is mapped via the matching name)
    assert_unmapped(&by_bytes, UnmappedReason::ForeignFile);
    let by_bytes = r.locate("src/gen.rs", 4, Some(48), ColumnUnit::Utf8Bytes);
    for s in [&by_display, &by_bytes, &by_scalar] {
        assert_mapped(&fx.src, s, Exact, "", (2, 41));
        assert_eq!(
            s.marigold.as_ref().unwrap().range.start,
            by_display.marigold.as_ref().unwrap().range.start
        );
    }
    let want = fx.src.find("panic!").unwrap();
    assert_eq!(by_display.marigold.as_ref().unwrap().range.start, want);
    // using the WRONG unit lands on a different token (this is the bug the unit parameter prevents)
    let wrong = r.locate("src/gen.rs", 4, Some(43), ColumnUnit::ScalarValues);
    assert_ne!(wrong.marigold.as_ref().unwrap().range.start, want);
}

#[test]
fn e02_display_width_table_matches_rustc_for_verified_characters() {
    // Ground truth: Location::caller().column() after the character, measured with rustc 1.97 (see spec 6.3).
    // (char, extra columns beyond the scalar count)
    let cases: &[(char, isize)] = &[
        ('a', 0),
        ('\u{e9}', 0),
        ('\u{2192}', 0),
        ('\u{65e5}', 1),
        ('\u{1F600}', 1),
        ('\u{0301}', -1),
        ('\u{200b}', -1),
        ('\t', 3),
        ('\u{ff01}', 1),
        ('\u{ac00}', 1),
        ('\u{0627}', 0),
        ('\u{3042}', 1),
        ('\u{00a0}', 0),
        ('\u{200d}', -1),
        ('\u{fe0f}', -1),
        ('\u{2605}', 0),
        ('\u{20000}', 1),
        ('\u{00ad}', -1),
        ('\u{0007}', 0),
        ('\u{0e31}', -1),
        ('\u{0436}', 0),
        ('\u{1100}', 1),
        ('\u{3000}', 1),
        ('\u{20dd}', -1),
    ];
    // one function per case, all on line 3 after `<prefix>`
    for (ch, extra) in cases {
        let src = format!(
            "fn f(x: i32) -> i32 {{ let s = \"{ch}\"; x }}\nrange(0, 1).write_file(\"o.csv\", csv)"
        );
        if ch.is_control() && *ch != '\t' {
            continue; // not representable in a fn body for this check
        }
        let p = marigold_generate(&src).unwrap();
        let f = seg_of(&p, SegmentKind::Function, 0);
        let line_start = p.rust[..f.generated.start].rfind('\n').map_or(0, |i| i + 1);
        let off = f.generated.start + p.rust[f.generated.start..].find(" x }").unwrap() + 1; // the `x`
        let scalar_col = p.rust[line_start..off].chars().count() + 1;
        let line_no = p.rust[..off].matches('\n').count() + 1;
        let display_col = (scalar_col as isize + extra) as usize;
        let is_gen = |f: &str| f == "gen.rs";
        let r = Remapper::new(&p, &src, &is_gen);
        let m = r.locate(
            "gen.rs",
            line_no,
            Some(display_col),
            ColumnUnit::DisplayWidth,
        );
        let want = src.find(" x }").unwrap() + 1;
        let got = m.marigold.as_ref().map(|l| l.range.start);
        assert_eq!(got, Some(want), "U+{:04X}: {m:?}", *ch as u32);
    }
}

#[test]
fn e03_crlf_source_runtime_location_and_line_only_lookup() {
    let fx = load("r02_index_oob_crlf");
    remapper!(fx, r);
    let stderr = read("r02_index_oob_crlf", "run.stderr.txt");
    assert!(stderr.contains("panicked at src/gen.rs:5:27:"));
    let m = r.locate("src/gen.rs", 5, Some(27), ColumnUnit::DisplayWidth);
    assert_eq!(m.precision, Exact);
    assert_eq!(slice(&fx.src, &m), "");
    let loc = m.marigold.as_ref().unwrap();
    assert_eq!((loc.start.line, loc.start.column), (3, 27));
    assert_eq!(&fx.src[loc.range.start..loc.range.start + 4], "v[i]");
    // line only (tracing metadata has no column): the whole line, attributed to the function
    let m = r.locate("src/gen.rs", 5, None, ColumnUnit::ScalarValues);
    assert_eq!(m.precision, Exact);
    assert!(slice(&fx.src, &m).contains("v[i]") && !slice(&fx.src, &m).contains('\r'));
    let m0 = r.locate("src/gen.rs", 5, Some(0), ColumnUnit::ScalarValues);
    assert_eq!(m0.marigold, m.marigold, "column 0 means unknown");
}

#[test]
fn e04_runtime_lines_in_glue_wrapper_and_beyond_the_file() {
    let fx = load("r01_panic_in_fn");
    remapper!(fx, r);
    // the closure/future frame `gen.rs:85:127` of the real trace is the `all_streams.collect()` glue
    let m = r.locate("src/gen.rs", 85, Some(127), ColumnUnit::Utf8Bytes);
    assert_unmapped(&m, UnmappedReason::Glue);
    assert_unmapped(
        &r.locate("src/gen.rs", 1, Some(1), ColumnUnit::Utf8Bytes),
        UnmappedReason::Glue,
    );
    assert_unmapped(
        &r.locate("src/gen.rs", 10_000, Some(1), ColumnUnit::Utf8Bytes),
        UnmappedReason::OutsideGeneratedRegion,
    );
    assert_unmapped(
        &r.locate("src/gen.rs", 0, Some(1), ColumnUnit::Utf8Bytes),
        UnmappedReason::OutsideGeneratedRegion,
    );
    assert_unmapped(
        &r.locate("src/gen.rs", 4, Some(10_000), ColumnUnit::Utf8Bytes),
        UnmappedReason::InvalidSpan,
    );
    assert_unmapped(
        &r.locate("src/main.rs", 3, Some(24), ColumnUnit::Utf8Bytes),
        UnmappedReason::ForeignFile,
    );
    // usize extremes
    for (l, c) in [
        (usize::MAX, Some(usize::MAX)),
        (1, Some(usize::MAX)),
        (4, Some(usize::MAX)),
        (4, Some(usize::MAX - 1)),
        (3, Some(usize::MAX / 2)),
        (usize::MAX, None),
    ] {
        let _ = r.locate("src/gen.rs", l, c, ColumnUnit::DisplayWidth);
        let _ = r.locate("src/gen.rs", l, c, ColumnUnit::ScalarValues);
        let _ = r.locate("src/gen.rs", l, c, ColumnUnit::Utf8Bytes);
    }
}

#[test]
fn e05_runtime_columns_on_the_prefixed_first_line() {
    let (src, program) = mini();
    let prefix = "日本 ";
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen).with_prefix(prefix);
    // line 1 of the file = prefix + `async {`
    let m = r.locate("gen.rs", 1, Some(1), ColumnUnit::ScalarValues);
    assert_unmapped(&m, UnmappedReason::OutsideGeneratedRegion);
    let after = |unit, col| r.locate("gen.rs", 1, Some(col), unit);
    assert_unmapped(
        &after(ColumnUnit::ScalarValues, 3 + 1),
        UnmappedReason::Glue,
    );
    assert_unmapped(
        &after(ColumnUnit::Utf8Bytes, prefix.len() + 1),
        UnmappedReason::Glue,
    );
    assert_unmapped(
        &after(ColumnUnit::DisplayWidth, 2 * 2 + 1 + 1),
        UnmappedReason::Glue,
    );
    assert_unmapped(
        &after(ColumnUnit::Utf8Bytes, prefix.len()),
        UnmappedReason::OutsideGeneratedRegion,
    );
}

#[test]
fn e06_trace_text_scanning_finds_panic_and_backtrace_locations() {
    let fx = load("r01_panic_in_fn");
    let is_gen = |f: &str| f == "src/gen.rs" || f == "./src/gen.rs";
    let r = Remapper::new(&fx.program, &fx.src, &is_gen);
    let stderr = read("r01_panic_in_fn", "run.stderr.txt");
    let hits = r.remap_trace(&stderr);
    let key: Vec<(&str, usize, Option<usize>, ColumnUnit)> = hits
        .iter()
        .map(|h| (h.file.as_str(), h.line, h.column, h.unit))
        .collect();
    assert_eq!(
        key,
        [
            ("src/gen.rs", 4, Some(43), ColumnUnit::DisplayWidth),
            ("./src/gen.rs", 4, Some(48), ColumnUnit::Utf8Bytes),
            ("./src/gen.rs", 85, Some(127), ColumnUnit::Utf8Bytes),
        ]
    );
    for h in &hits {
        assert_eq!(
            &stderr[h.token.start..h.token.end],
            format!("{}:{}:{}", h.file, h.line, h.column.unwrap())
        );
    }
    // both user-code locations are the same Marigold byte; the glue frame is unmapped
    assert_eq!(hits[0].mapped.marigold, hits[1].mapped.marigold);
    assert_eq!(
        hits[0].mapped.marigold.as_ref().unwrap().range.start,
        fx.src.find("panic!").unwrap()
    );
    assert_unmapped(&hits[2].mapped, UnmappedReason::Glue);
}

#[test]
fn e07_trace_scanner_is_lenient_and_never_panics() {
    let (src, program) = mini();
    let is_gen = |f: &str| f.ends_with("gen.rs");
    let r = Remapper::new(&program, &src, &is_gen);
    let texts = [
        "",
        ":",
        "::::",
        "gen.rs:",
        "gen.rs:1",
        "gen.rs:1:",
        "gen.rs:1:2:3",
        "x gen.rs:99999999999999999999999:1",
        "C:\\proj\\src\\gen.rs:7:9",
        "(gen.rs:3:4)",
        "\"gen.rs:3:4\"",
        "at /path with spaces/gen.rs:3:4",
        "12:30:45 logged",
        "日本語gen.rs:3:4 \u{1F600}",
        "é:1:2 gen.rs:2:1",
    ];
    for t in texts {
        let hits = r.remap_trace(t);
        for h in hits {
            assert!(
                t.is_char_boundary(h.token.start) && t.is_char_boundary(h.token.end),
                "{t:?}"
            );
            assert!(h.file.ends_with("gen.rs"));
        }
    }
    let h = r.remap_trace("C:\\proj\\src\\gen.rs:7:9");
    assert_eq!(h.len(), 1);
    assert_eq!(h[0].file, "C:\\proj\\src\\gen.rs");
    // documented limitation: a path containing a space is not recognised (no partial/guessed match)
    let h = r.remap_trace("at /path with spaces/gen.rs:3:4");
    assert!(h.iter().all(|x| x.file == "spaces/gen.rs"), "{h:?}");
}

#[test]
fn e08_display_width_beyond_the_verified_table_never_claims_exact() {
    // U+0F00 (Tibetan) is outside the table verified against rustc: the column may be wrong, so the
    // result must degrade to the whole segment instead of pointing at a guessed byte.
    let src =
        "fn f(x: i32) -> i32 { let s = \"\u{0f00}\"; x }\nrange(0, 1).write_file(\"o.csv\", csv)";
    let p = marigold_generate(src).unwrap();
    let f = seg_of(&p, SegmentKind::Function, 0);
    let line_start = p.rust[..f.generated.start].rfind('\n').map_or(0, |i| i + 1);
    let off = f.generated.start + p.rust[f.generated.start..].find(" x }").unwrap() + 1;
    let col = p.rust[line_start..off].chars().count() + 1;
    let line = p.rust[..off].matches('\n').count() + 1;
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&p, src, &is_gen);
    let m = r.locate("gen.rs", line, Some(col), ColumnUnit::DisplayWidth);
    assert_eq!(m.precision, Seg, "{m:?}");
    // the exact units stay exact on the same line
    assert_eq!(
        r.locate("gen.rs", line, Some(col), ColumnUnit::ScalarValues)
            .precision,
        Exact
    );
    assert_eq!(
        r.locate(
            "gen.rs",
            line,
            Some(off - line_start + 1),
            ColumnUnit::Utf8Bytes
        )
        .precision,
        Exact
    );
}

#[test]
fn e09_errors_are_never_noise_and_trace_hits_come_in_text_order() {
    let fx = load("f01_undefined_fn");
    remapper!(fx, r);
    assert!(!r.remap(find(&fx, "undefined_fn")).likely_generated_noise());
    let is_gen = |f: &str| f.ends_with(".rs");
    let (src, program) = mini();
    let r = Remapper::new(&program, &src, &is_gen);
    let hits = r.remap_trace("a.rs:3:4 and b.rs:5:6 then c.rs:7");
    let files: Vec<_> = hits
        .iter()
        .map(|h| (h.file.as_str(), h.line, h.column))
        .collect();
    assert_eq!(
        files,
        [
            ("a.rs", 3, Some(4)),
            ("b.rs", 5, Some(6)),
            ("c.rs", 7, None)
        ]
    );
    assert!(hits.windows(2).all(|w| w[0].token.end <= w[1].token.start));
}

// ---------------------------------------------------------------------------------------------
// H. nested segments (T2 will produce them) and corrupted maps
// ---------------------------------------------------------------------------------------------

/// outer expression 0 with two inner pieces, plus a second expression; synthetic but self-consistent.
fn nested() -> (String, GeneratedProgram) {
    let src = "xs = range(0, 3).map(f)\nys.return".to_string();
    let rust = "async {\n    let xs = G(range(0, 3)).M(map(f));\nlet r0 = ys;\n}\n".to_string();
    let g = |needle: &str| {
        let a = rust.find(needle).unwrap();
        ByteRange {
            start: a,
            end: a + needle.len(),
        }
    };
    let s = |needle: &str| {
        let a = src.find(needle).unwrap();
        ByteRange {
            start: a,
            end: a + needle.len(),
        }
    };
    let outer = g("let xs = G(range(0, 3)).M(map(f));");
    let segs = vec![
        Segment::new(
            outer,
            s("xs = range(0, 3).map(f)"),
            SegmentKind::StreamVariable,
            0,
            Some("xs".into()),
        ),
        Segment::new(
            g("range(0, 3)"),
            s("range(0, 3)"),
            SegmentKind::StreamVariable,
            0,
            None,
        ),
        Segment::new(
            g("map(f)"),
            s("map(f)"),
            SegmentKind::StreamVariable,
            0,
            None,
        ),
        Segment::new(
            g("let r0 = ys;"),
            s("ys.return"),
            SegmentKind::ReturningStream,
            1,
            None,
        ),
    ];
    (
        src,
        GeneratedProgram::new(rust.clone(), SourceMap::new(segs)),
    )
}

#[test]
fn h01_innermost_segment_wins_and_straddling_siblings_fall_back_to_the_enclosing_one() {
    let (src, program) = nested();
    let at = |a: usize, b: usize| {
        map_one(
            &src,
            &program,
            "",
            &rustc_span("gen.rs", &program.rust, a, b, true),
        )
    };
    let rg = |needle: &str| {
        let a = program.rust.find(needle).unwrap();
        (a, a + needle.len())
    };
    // inside an inner piece: the inner piece's source (finer than the expression)
    let (a, b) = rg("range(0, 3)");
    let m = at(a + 2, b - 1);
    assert_eq!(m.precision, Seg);
    assert_eq!(slice(&src, &m), "range(0, 3)");
    // the whole inner piece, and a piece-sized span in the other piece
    let (a, b) = rg("map(f)");
    assert_eq!(slice(&src, &at(a, b)), "map(f)");
    // spans two sibling pieces, contained in the outer segment: the outer one
    let (a, _) = rg("range(0, 3)");
    let (_, b) = rg("map(f)");
    let m = at(a + 1, b - 1);
    assert_eq!(m.precision, Seg);
    assert_eq!(slice(&src, &m), "xs = range(0, 3).map(f)");
    // glue inside the outer segment (between the pieces) is the outer segment, not unmapped
    let (_, e1) = rg("range(0, 3)");
    let m = at(e1, e1 + 1);
    assert_eq!(slice(&src, &m), "xs = range(0, 3).map(f)");
    // leaves the outer segment into trailing glue but stays in expression 0: Expression
    let (_, oe) = rg("let xs = G(range(0, 3)).M(map(f));");
    let m = at(oe - 3, oe + 1);
    assert_eq!(m.precision, Expression);
    assert_eq!(slice(&src, &m), "xs = range(0, 3).map(f)");
    // two expressions
    let m = at(oe - 3, program.rust.find("let r0").unwrap() + 4);
    assert_unmapped(&m, UnmappedReason::MultipleExpressions);
    // T1 agreement for single bytes everywhere
    for off in 0..program.rust.len() {
        let m = at(off, off + 1);
        match program.map.source_for_generated(off) {
            None => assert_unmapped(&m, UnmappedReason::Glue),
            Some(seg) => assert_eq!(
                m.marigold.as_ref().unwrap().range,
                seg.source,
                "off {off} {m:?}"
            ),
        }
    }
}

#[test]
fn h02_zero_width_insertion_at_boundaries_prefers_the_segment_that_starts_there() {
    let (src, program) = nested();
    let at = |a: usize| {
        map_one(
            &src,
            &program,
            "",
            &rustc_span("gen.rs", &program.rust, a, a, true),
        )
    };
    let a = program.rust.find("range(0, 3)").unwrap();
    // exactly at the start of the inner piece: the inner piece
    assert_eq!(slice(&src, &at(a)), "range(0, 3)");
    // exactly at the end of the inner piece (next byte is outer-only text): the outer segment
    assert_eq!(
        slice(&src, &at(a + "range(0, 3)".len())),
        "xs = range(0, 3).map(f)"
    );
    // at the end of the whole second segment, followed by glue: attributed to that segment (insertion after its last byte)
    let end = program.rust.find("let r0 = ys;").unwrap() + "let r0 = ys;".len();
    let m = at(end);
    assert_eq!(slice(&src, &m), "ys.return");
    assert_eq!(m.precision, Seg);
    // before the first segment, in the preamble: glue
    assert_unmapped(&at(3), UnmappedReason::Glue);
}

#[test]
fn h03_zero_width_insertion_at_the_very_end_of_a_function_stays_exact_and_zero_width() {
    let (src, program) = mini();
    let f = seg_of(&program, SegmentKind::Function, 0);
    let m = map_one(
        &src,
        &program,
        "",
        &rustc_span(
            "gen.rs",
            &program.rust,
            f.generated.end,
            f.generated.end,
            true,
        ),
    );
    assert_eq!(m.precision, Exact);
    let loc = m.marigold.as_ref().unwrap();
    assert_eq!(
        loc.range,
        ByteRange {
            start: f.source.end,
            end: f.source.end
        }
    );
    assert_eq!((loc.start.line, loc.start.column), (1, 30));
    // and at its very start: the segment, but not exact (`const ` precedes the verbatim region)
    let m = map_one(
        &src,
        &program,
        "",
        &rustc_span(
            "gen.rs",
            &program.rust,
            f.generated.start,
            f.generated.start,
            true,
        ),
    );
    assert_eq!(m.precision, Seg);
}

#[test]
fn h04_corrupted_maps_degrade_to_unmapped_never_to_a_wrong_range() {
    let src = "range(0, 1).write_file(\"o.csv\", csv)";
    let rust = "async {\n    let x = 1;\n}\n";
    let sp = |a: usize, b: usize| rustc_span("gen.rs", rust, a, b, true);
    let is_gen = |f: &str| f == "gen.rs";
    let cases: Vec<(&str, Segment)> = vec![
        (
            "source past end",
            Segment::new(
                ByteRange { start: 12, end: 21 },
                ByteRange {
                    start: 5,
                    end: 9999,
                },
                SegmentKind::NonReturningStream,
                0,
                None,
            ),
        ),
        (
            "source inverted",
            Segment::new(
                ByteRange { start: 12, end: 21 },
                ByteRange { start: 9, end: 3 },
                SegmentKind::NonReturningStream,
                0,
                None,
            ),
        ),
        (
            "generated past end",
            Segment::new(
                ByteRange {
                    start: 12,
                    end: 9999,
                },
                ByteRange { start: 0, end: 5 },
                SegmentKind::Function,
                0,
                None,
            ),
        ),
        (
            "generated inverted",
            Segment::new(
                ByteRange { start: 21, end: 12 },
                ByteRange { start: 0, end: 5 },
                SegmentKind::Function,
                0,
                None,
            ),
        ),
        (
            "empty",
            Segment::new(
                ByteRange { start: 12, end: 12 },
                ByteRange { start: 0, end: 0 },
                SegmentKind::Function,
                0,
                None,
            ),
        ),
    ];
    for (name, seg) in cases {
        let program = GeneratedProgram::new(rust.to_string(), SourceMap::new(vec![seg]));
        let r = Remapper::new(&program, src, &is_gen);
        for (a, b) in [
            (12, 13),
            (12, 12),
            (14, 20),
            (0, rust.len()),
            (rust.len(), rust.len()),
        ] {
            let m = r.map_span(&sp(a, b));
            assert_span_invariants(src, rust, &m);
            if name.starts_with("source") {
                if let Some(l) = &m.marigold {
                    assert!(l.range.end <= src.len(), "{name}: {m:?}");
                }
            }
        }
    }
    // an empty map: everything inside the program is glue
    let program = GeneratedProgram::new(rust.to_string(), SourceMap::new(vec![]));
    let r = Remapper::new(&program, src, &is_gen);
    assert_unmapped(&r.map_span(&sp(12, 13)), UnmappedReason::Glue);
    assert_unmapped(&r.map_span(&sp(0, 0)), UnmappedReason::Glue);
    // empty program and empty source
    let program = GeneratedProgram::new(String::new(), SourceMap::new(vec![]));
    let r = Remapper::new(&program, "", &is_gen);
    let empty: RustcSpan =
        serde_json::from_value(json!({"file_name": "gen.rs", "byte_start": 0, "byte_end": 0}))
            .unwrap();
    assert_unmapped(&r.map_span(&empty), UnmappedReason::Glue);
    let _ = r.locate("gen.rs", 1, Some(1), ColumnUnit::Utf8Bytes);
    let _ = r.locate("gen.rs", 1, None, ColumnUnit::Utf8Bytes);
}

#[test]
fn h05_verbatim_region_never_starts_inside_a_character() {
    // Stale/mismatching texts whose last BYTES agree but whose last CHARACTERS differ:
    // generated `é` = C3 A9, source `©` = C2 A9. Nothing here is verbatim.
    let rust = "async {\n    const fn f() \u{e9}\n}\n";
    let src = "fn f() \u{a9}";
    let g0 = rust.find("const").unwrap();
    let g1 = rust.find("\n}").unwrap();
    let seg = Segment::new(
        ByteRange { start: g0, end: g1 },
        ByteRange {
            start: 0,
            end: src.len(),
        },
        SegmentKind::Function,
        0,
        None,
    );
    let program = GeneratedProgram::new(rust.to_string(), SourceMap::new(vec![seg]));
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, src, &is_gen);
    // zero-width at the very end: would be `Exact` only if the last (shared) byte were treated as verbatim
    let m = r.map_span(&rustc_span("gen.rs", rust, g1, g1, true));
    assert_eq!(m.precision, Seg, "{m:?}");
    // the whole last character
    let m = r.map_span(&rustc_span("gen.rs", rust, g1 - 2, g1, true));
    assert_eq!(m.precision, Seg, "{m:?}");
    assert_eq!(slice(src, &m), src);
}

// ---------------------------------------------------------------------------------------------
// F. properties
// ---------------------------------------------------------------------------------------------

fn fixture_names() -> Vec<String> {
    let mut v: Vec<String> = std::fs::read_dir(fixtures_root())
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().join("bare.jsonl").exists())
        .map(|e| e.file_name().to_str().unwrap().to_string())
        .collect();
    v.sort();
    v
}

#[test]
fn f01_every_real_diagnostic_of_every_fixture_satisfies_all_invariants_and_none_is_stale() {
    let names = fixture_names();
    assert!(names.len() >= 18, "{names:?}");
    let mut exact = 0;
    let mut spans = 0;
    for name in &names {
        let fx = load(name);
        remapper!(fx, r);
        for d in &fx.diags {
            let m = r.remap(d);
            for s in all_spans(&m) {
                spans += 1;
                assert_span_invariants(&fx.src, &fx.program.rust, s);
                assert!(
                    !matches!(
                        s.precision,
                        Precision::Unmapped {
                            reason: UnmappedReason::StaleGeneratedFile
                                | UnmappedReason::InvalidSpan
                                | UnmappedReason::StaleSourceMap
                        }
                    ),
                    "{name}: real rustc output must never be rejected as stale/invalid: {s:?}"
                );
                if s.precision == Exact {
                    exact += 1;
                    // Exact <=> byte-identical text, checked against the raw generated bytes
                    let raw = s.raw.as_ref().unwrap();
                    let base = fx.prefix.len();
                    let mut cur = (raw.byte_start, raw.byte_end);
                    if s.via_macro.is_some() {
                        continue; // raw is the library span; identity is checked for the direct ones
                    }
                    cur.0 -= base;
                    cur.1 -= base;
                    let loc = s.marigold.as_ref().unwrap();
                    assert_eq!(
                        &fx.src[loc.range.start..loc.range.end],
                        &fx.program.rust[cur.0..cur.1],
                        "{name}: Exact must be byte-identical"
                    );
                }
            }
        }
    }
    assert!(spans >= 110 && exact >= 20, "spans={spans} exact={exact}");
}

#[test]
fn f02_remapping_the_cargo_wrapper_and_the_bare_form_gives_identical_results() {
    for name in fixture_names() {
        let fx = load(&name);
        remapper!(fx, r);
        let cargo = parse_messages(&read(&name, "cargo.jsonl"));
        let bare: Vec<_> = fx.diags.iter().map(|d| r.remap(d)).collect();
        let wrapped: Vec<_> = cargo
            .messages
            .iter()
            .map(|m| r.remap(&m.diagnostic))
            .collect();
        assert_eq!(bare, wrapped, "{name}");
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(256))]

    /// Any span lying inside one segment of a generated program maps inside that expression's range,
    /// names that segment's expression, and a mapped range never leaves the source.
    #[test]
    fn p01_random_spans_inside_a_segment_map_inside_the_same_expression(
        pick in 0usize..64, a in 0usize..10_000, len in 0usize..64,
    ) {
        let (src, program) = mini();
        let segs = program.map.segments();
        let seg = &segs[pick % segs.len()];
        let g = seg.generated;
        let a = g.start + a % (g.end - g.start);
        let b = (a + len).min(g.end);
        prop_assume!(program.rust.is_char_boundary(a) && program.rust.is_char_boundary(b));
        let is_gen = |f: &str| f == "gen.rs";
        let r = Remapper::new(&program, &src, &is_gen);
        let m = r.map_span(&rustc_span("gen.rs", &program.rust, a, b, true));
        assert_span_invariants(&src, &program.rust, &m);
        let loc = m.marigold.as_ref().expect("inside a segment => mapped");
        prop_assert!(seg.source.start <= loc.range.start && loc.range.end <= seg.source.end);
        prop_assert_eq!(m.segment.as_ref().unwrap().expr_index, seg.expr_index);
        if m.precision == Exact {
            prop_assert_eq!(&src[loc.range.start..loc.range.end], &program.rust[a..b]);
        }
    }

    /// Same, over every program of the repository corpus (no hand-picked shape).
    #[test]
    fn p02_corpus_programs_random_spans(file in 0usize..200, pick in 0usize..400, a in 0usize..100_000, len in 0usize..40) {
        let files = corpus();
        let path = &files[file % files.len()];
        let src = std::fs::read_to_string(path).unwrap();
        let Ok(program) = marigold_generate(&src) else { return Ok(()); };
        let segs = program.map.segments();
        prop_assume!(!segs.is_empty());
        let seg = &segs[pick % segs.len()];
        let g = seg.generated;
        let a = g.start + a % (g.end - g.start);
        let b = (a + len).min(g.end);
        prop_assume!(program.rust.is_char_boundary(a) && program.rust.is_char_boundary(b));
        let is_gen = |f: &str| f == "gen.rs";
        let r = Remapper::new(&program, &src, &is_gen);
        let m = r.map_span(&rustc_span("gen.rs", &program.rust, a, b, true));
        let loc = m.marigold.as_ref().unwrap();
        prop_assert!(seg.source.start <= loc.range.start && loc.range.end <= seg.source.end, "{:?}", path);
        assert_span_invariants(&src, &program.rust, &m);
    }

    /// Arbitrary offsets, line/column numbers and text: never panics; any range returned is in bounds.
    #[test]
    fn p03_arbitrary_span_numbers_never_panic(
        bs in any::<usize>(), be in any::<usize>(), l1 in any::<usize>(), c1 in any::<usize>(),
        l2 in any::<usize>(), c2 in any::<usize>(), small in 0usize..4000, small2 in 0usize..4000,
        text in "\\PC{0,20}", prefix in "\\PC{0,6}",
    ) {
        let (src, program) = mini();
        let is_gen = |f: &str| f == "gen.rs";
        let r = Remapper::new(&program, &src, &is_gen).with_prefix(&prefix);
        for (bs, be) in [(bs, be), (small, small2), (small.min(small2), small.max(small2))] {
            let sp: RustcSpan = serde_json::from_value(json!({
                "file_name": "gen.rs", "byte_start": bs, "byte_end": be,
                "line_start": l1, "line_end": l2, "column_start": c1, "column_end": c2,
                "is_primary": true,
                "text": [{"text": text, "highlight_start": c1, "highlight_end": c2}],
            })).unwrap();
            let m = r.map_span(&sp);
            assert_span_invariants(&src, &program.rust, &m);
            // honest: arbitrary numbers essentially never describe the generated file
            if let Some(l) = &m.marigold { prop_assert!(l.range.end <= src.len()); }
        }
    }

    /// Arbitrary runtime locations never panic.
    #[test]
    fn p04_arbitrary_runtime_locations_never_panic(line in any::<usize>(), col in proptest::option::of(any::<usize>()), sl in 0usize..120, sc in 0usize..200, unit in 0u8..3, prefix in "\\PC{0,6}") {
        let (src, program) = mini();
        let is_gen = |f: &str| f == "gen.rs";
        let r = Remapper::new(&program, &src, &is_gen).with_prefix(&prefix);
        let u = [ColumnUnit::ScalarValues, ColumnUnit::DisplayWidth, ColumnUnit::Utf8Bytes][unit as usize];
        for (l, c) in [(line, col), (sl, Some(sc)), (sl, None)] {
            let m = r.locate("gen.rs", l, c, u);
            assert_span_invariants(&src, &program.rust, &m);
        }
    }

    /// Randomly corrupted maps (any ranges, overlapping, out of bounds) plus random spans: never panics.
    #[test]
    fn p07_random_maps_and_random_spans_never_panic(
        segs in proptest::collection::vec((0usize..300, 0usize..300, 0usize..300, 0usize..300, 0u8..7, 0usize..5), 0..8),
        spans in proptest::collection::vec((0usize..300, 0usize..300), 1..6),
        unit in 0u8..3,
    ) {
        let src = "fn f(x: i32) -> i32 { x }\nxs = range(0, 3)\nxs.write_file(\"o\", csv)\n日本語";
        let rust = "async {\n    const fn f(x: i32) -> i32 { x }\n\nlet a = 1;\nlet b = \"日本語\";\n}\n日本";
        let kinds = [SegmentKind::Struct, SegmentKind::Enum, SegmentKind::Function, SegmentKind::StreamVariable, SegmentKind::StreamVariableRunner, SegmentKind::ReturningStream, SegmentKind::NonReturningStream];
        let segments: Vec<Segment> = segs.iter().map(|&(g0, g1, s0, s1, k, e)| Segment::new(
            ByteRange { start: g0, end: g1 }, ByteRange { start: s0, end: s1 }, kinds[k as usize], e, None)).collect();
        let program = GeneratedProgram::new(rust.to_string(), SourceMap::new(segments));
        let is_gen = |f: &str| f == "gen.rs";
        let r = Remapper::new(&program, src, &is_gen);
        for (a, b) in spans {
            let sp: RustcSpan = serde_json::from_value(json!({"file_name": "gen.rs", "byte_start": a, "byte_end": b})).unwrap();
            assert_span_invariants(src, rust, &r.map_span(&sp));
            let u = [ColumnUnit::ScalarValues, ColumnUnit::DisplayWidth, ColumnUnit::Utf8Bytes][unit as usize];
            assert_span_invariants(src, rust, &r.locate("gen.rs", a % 9, Some(b), u));
        }
    }

    /// Arbitrary text: the JSON reader and the trace scanner never panic.
    #[test]
    fn p05_garbage_input_never_panics(text in "\\PC{0,200}", bytes in proptest::collection::vec(any::<u8>(), 0..200)) {
        let _ = parse_messages(&text);
        let _ = parse_message_line(&text);
        let lossy = String::from_utf8_lossy(&bytes).into_owned();
        let _ = parse_messages(&lossy);
        let (src, program) = mini();
        let is_gen = |f: &str| f.contains("gen");
        let r = Remapper::new(&program, &src, &is_gen);
        let _ = r.remap_trace(&text);
        let _ = r.remap_trace(&lossy);
    }

    /// Structured fuzz: random JSON objects shaped like diagnostics (random numbers/strings/nesting).
    #[test]
    fn p06_random_diagnostic_shapes_never_panic(seed in proptest::collection::vec((any::<usize>(), any::<usize>(), "[a-z/._]{0,8}", any::<bool>()), 0..6), depth in 0usize..4) {
        let (src, program) = mini();
        let is_gen = |f: &str| f == "gen.rs";
        let r = Remapper::new(&program, &src, &is_gen);
        let spans: Vec<Value> = seed.iter().map(|(a, b, f, p)| json!({
            "file_name": f, "byte_start": a % 5000, "byte_end": b % 5000, "is_primary": p,
            "line_start": 1, "line_end": 1, "column_start": 1, "column_end": 1,
        })).collect();
        let mut d = json!({"message": "m", "level": "error", "spans": spans, "children": []});
        for _ in 0..depth {
            d = json!({"message": "w", "level": "note", "spans": [], "children": [d]});
        }
        let diag: RustcDiagnostic = serde_json::from_value(d).unwrap();
        let m = r.remap(&diag);
        for s in all_spans(&m) { assert_span_invariants(&src, &program.rust, s); }
    }
}

/// T1's lookup and T4's offset classification must agree on which segment owns a single byte.
#[test]
fn f03_agrees_with_source_map_lookup_for_every_offset() {
    let fx = load("f06_mismatched_variable_types");
    remapper!(fx, r);
    let file = format!("{}{}", fx.prefix, fx.program.rust);
    for off in 0..fx.program.rust.len() {
        if !fx.program.rust.is_char_boundary(off) || !fx.program.rust.is_char_boundary(off + 1) {
            continue;
        }
        let at = off + fx.prefix.len();
        let sp = rustc_span(&fx.generated_file, &file, at, at + 1, true);
        let m = r.map_span(&sp);
        match fx.program.map.source_for_generated(off) {
            None => assert_unmapped(&m, UnmappedReason::Glue),
            Some(seg) => {
                let sg = m
                    .segment
                    .as_ref()
                    .unwrap_or_else(|| panic!("off {off}: {m:?}"));
                assert_eq!(
                    (sg.kind, sg.expr_index),
                    (seg.kind, seg.expr_index),
                    "off {off}"
                );
            }
        }
    }
}

// ---------------------------------------------------------------------------------------------
// G. scale
// ---------------------------------------------------------------------------------------------

#[test]
fn g01_two_hundred_thousand_diagnostics_in_linear_time() {
    let fx = load("f06_mismatched_variable_types");
    remapper!(fx, r);
    let one = fx
        .diags
        .iter()
        .find(|d| d.message.contains("non-const function"))
        .unwrap();
    let start = std::time::Instant::now();
    let mut n_exact = 0usize;
    for _ in 0..200_000 {
        let m = r.remap(one);
        if m.primary.as_ref().unwrap().precision == Exact {
            n_exact += 1;
        }
    }
    assert_eq!(n_exact, 200_000);
    // Generous absolute bound (debug build ~1 s): this only catches a remapper that is accidentally
    // quadratic; the exact cost of a lookup is pinned by the deterministic work counters in
    // `src/remap.rs` (`complexity_tests`).
    assert!(
        start.elapsed() < std::time::Duration::from_secs(120),
        "{:?}",
        start.elapsed()
    );
}

/// `n` single-line functions; returns the time of 30_000 point lookups (bytes only, line/column not provided).
fn lookup_time(n: usize) -> std::time::Duration {
    let mut src = String::new();
    let mut segs = vec![];
    let mut rust = String::from("async {\n    ");
    for i in 0..n {
        let f = format!("fn f{i}(x: i32) -> i32 {{ x + {i} }}");
        let s0 = src.len();
        src.push_str(&f);
        let g0 = rust.len();
        rust.push_str("const ");
        rust.push_str(&f);
        segs.push(Segment::new(
            ByteRange {
                start: g0,
                end: rust.len(),
            },
            ByteRange {
                start: s0,
                end: src.len(),
            },
            SegmentKind::Function,
            i,
            None,
        ));
        src.push('\n');
        rust.push_str("\n\n");
    }
    rust.push_str("}\n");
    let program = GeneratedProgram::new(rust, SourceMap::new(segs));
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let spans: Vec<RustcSpan> = (0..30_000)
        .map(|k| {
            let seg = &program.map.segments()[(k * 7919) % n];
            let off = seg.generated.end - 3;
            serde_json::from_value(json!({
                "file_name": "gen.rs", "byte_start": off, "byte_end": off + 1, "is_primary": true
            }))
            .unwrap()
        })
        .collect();
    let t = std::time::Instant::now();
    for (k, sp) in spans.iter().enumerate() {
        let m = r.map_span(sp);
        assert_eq!(m.precision, Exact);
        assert_eq!(m.segment.as_ref().unwrap().expr_index, (k * 7919) % n);
    }
    t.elapsed()
}

#[test]
fn g02_lookup_cost_does_not_grow_with_the_number_of_segments() {
    // Functional check on a 40 000-segment map plus a generous absolute bound. How much WORK a lookup
    // does is asserted deterministically (no clock) by `src/remap.rs::complexity_tests`.
    let small = lookup_time(2_000);
    let large = lookup_time(40_000);
    assert!(
        large < std::time::Duration::from_secs(120),
        "small={small:?} large={large:?}"
    );
}

#[test]
fn g03_a_span_covering_everything_stops_at_the_second_expression() {
    // the whole-program span must not cost O(segments) per diagnostic
    let (src, program) = mini();
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let m = r.map_span(&rustc_span(
        "gen.rs",
        &program.rust,
        0,
        program.rust.len(),
        true,
    ));
    assert_unmapped(&m, UnmappedReason::MultipleExpressions);
}

// ---------------------------------------------------------------------------------------------
// I. additional tests written with the implementation: exact JSON shapes, suggestions through
//    macros, the multi-line checksum, trace scanning corner cases, unverified widths, corpus-wide
//    agreement with the T1 lookup
// ---------------------------------------------------------------------------------------------

fn key_set(v: &Value) -> Vec<&str> {
    let mut keys: Vec<&str> = v.as_object().unwrap().keys().map(String::as_str).collect();
    keys.sort_unstable();
    keys
}

#[test]
fn x01_json_shapes_have_exactly_these_keys() {
    let fx = load("f09_unused_warning_in_fn");
    remapper!(fx, r);
    let m = r.remap(find(&fx, "unused variable: `unused`"));
    let v = serde_json::to_value(&m).unwrap();
    assert_eq!(
        key_set(&v),
        [
            "children",
            "code",
            "level",
            "message",
            "primary",
            "rendered",
            "secondary"
        ]
    );
    assert_eq!(v["level"], json!("warning"));
    let p = &v["primary"];
    assert_eq!(
        key_set(p),
        [
            "is_primary",
            "label",
            "marigold",
            "precision",
            "raw",
            "segment",
            "suggestion",
            "via_macro"
        ]
    );
    assert_eq!(key_set(&p["marigold"]), ["end", "range", "start"]);
    assert_eq!(key_set(&p["marigold"]["range"]), ["end", "start"]);
    assert_eq!(key_set(&p["marigold"]["start"]), ["column", "line"]);
    assert_eq!(key_set(&p["segment"]), ["expr_index", "kind", "name"]);
    assert_eq!(
        key_set(&p["raw"]),
        [
            "byte_end",
            "byte_start",
            "column_end",
            "column_start",
            "file_name",
            "line_end",
            "line_start"
        ]
    );
    assert_eq!(p["precision"], json!({"kind": "exact"}));
    assert_eq!(p["segment"]["kind"], json!("function"));
    // the help child carries the suggestion
    let help = &v["children"][1]["primary"]["suggestion"];
    assert_eq!(
        key_set(help),
        ["applicability", "applicable_to_marigold", "replacement"]
    );
    assert_eq!(help["applicable_to_marigold"], json!(true));
    // unmapped span: same keys, nulls for what is missing
    assert_eq!(m.level, Level::Warning);
    let unmapped =
        serde_json::to_value(r.locate("other.rs", 1, None, ColumnUnit::Utf8Bytes)).unwrap();
    assert_eq!(unmapped["marigold"], Value::Null);
    assert_eq!(unmapped["segment"], Value::Null);
    assert_eq!(unmapped["raw"], Value::Null);
    assert_eq!(
        unmapped["precision"],
        json!({"kind": "unmapped", "reason": "foreign_file"})
    );
    assert_eq!(
        key_set(&unmapped),
        [
            "is_primary",
            "label",
            "marigold",
            "precision",
            "raw",
            "segment",
            "suggestion",
            "via_macro"
        ]
    );
    // every reason and unit has a stable name
    for (reason, name) in [
        (UnmappedReason::NoSpan, "no_span"),
        (UnmappedReason::ForeignFile, "foreign_file"),
        (
            UnmappedReason::OutsideGeneratedRegion,
            "outside_generated_region",
        ),
        (UnmappedReason::Glue, "glue"),
        (UnmappedReason::MultipleExpressions, "multiple_expressions"),
        (UnmappedReason::StaleGeneratedFile, "stale_generated_file"),
        (UnmappedReason::InvalidSpan, "invalid_span"),
        (UnmappedReason::StaleSourceMap, "stale_source_map"),
    ] {
        assert_eq!(serde_json::to_value(reason).unwrap(), json!(name));
    }
    for (unit, name) in [
        (ColumnUnit::ScalarValues, "scalar_values"),
        (ColumnUnit::DisplayWidth, "display_width"),
        (ColumnUnit::Utf8Bytes, "utf8_bytes"),
    ] {
        assert_eq!(serde_json::to_value(unit).unwrap(), json!(name));
    }
    assert_eq!(
        serde_json::to_value(Seg).unwrap(),
        json!({"kind": "segment"})
    );
    assert_eq!(
        serde_json::to_value(Expression).unwrap(),
        json!({"kind": "expression"})
    );
}

#[test]
fn x02_traced_locations_serialize_with_exactly_these_keys() {
    let fx = load("r01_panic_in_fn");
    let is_gen = |f: &str| f == "src/gen.rs";
    let r = Remapper::new(&fx.program, &fx.src, &is_gen);
    let stderr = read("r01_panic_in_fn", "run.stderr.txt");
    let hits = r.remap_trace(&stderr);
    assert_eq!(
        hits.len(),
        1,
        "only the panic line names `src/gen.rs` exactly"
    );
    let v = serde_json::to_value(&hits[0]).unwrap();
    assert_eq!(
        key_set(&v),
        ["column", "file", "line", "mapped", "token", "unit"]
    );
    assert_eq!(key_set(&v["token"]), ["end", "start"]);
    assert_eq!(v["unit"], json!("display_width"));
    assert_eq!(v["column"], json!(43));
}

#[test]
fn x03_a_suggestion_found_through_a_macro_call_site_is_not_applicable() {
    let (src, program) = mini();
    let f = seg_of(&program, SegmentKind::Function, 0);
    let plus = f.generated.start + program.rust[f.generated.start..].find('+').unwrap();
    // the library span (replacement targets library text) whose call site is the `+` in the user's fn
    let call = rustc_span("gen.rs", &program.rust, plus, plus + 1, false);
    let mut lib = RustcSpan::default();
    lib.file_name = "/rustc/x/library/core/src/ops.rs".into();
    lib.byte_end = 1;
    lib.suggested_replacement = Some("-".into());
    lib.suggestion_applicability = Some("MaybeIncorrect".into());
    let m = map_one(&src, &program, "", &with_expansion(lib, call, "add!"));
    assert_eq!(m.precision, Exact);
    assert_eq!(m.via_macro.as_deref(), Some("add!"));
    let sug = m.suggestion.unwrap();
    assert_eq!(sug.replacement, "-");
    assert_eq!(sug.applicability.as_deref(), Some("MaybeIncorrect"));
    assert!(
        !sug.applicable_to_marigold,
        "the replacement edits library text, not the span that was mapped"
    );
    // the same replacement written directly on the generated span is applicable
    let mut direct = rustc_span("gen.rs", &program.rust, plus, plus + 1, true);
    direct.suggested_replacement = Some("-".into());
    let m = map_one(&src, &program, "", &direct);
    assert!(m.suggestion.unwrap().applicable_to_marigold);
    // an empty replacement (a removal) is still a suggestion
    let mut removal = rustc_span("gen.rs", &program.rust, plus, plus + 1, true);
    removal.suggested_replacement = Some(String::new());
    assert_eq!(
        map_one(&src, &program, "", &removal)
            .suggestion
            .unwrap()
            .replacement,
        ""
    );
}

/// The span of the `match` expression in fixture f15: six lines.
fn six_line_span(fx: &Fx) -> RustcSpan {
    let d = find(fx, "`match` arms have incompatible types");
    d.spans.iter().find(|s| s.text.len() == 6).unwrap().clone()
}

#[test]
fn x04_multi_line_checksum_covers_the_first_and_last_line_only() {
    let fx = load("f15_multiline_span");
    let good = six_line_span(&fx);
    let precision = |s: &RustcSpan| {
        let is_gen = |f: &str| f == fx.generated_file;
        Remapper::new(&fx.program, &fx.src, &is_gen)
            .with_prefix(&fx.prefix)
            .map_span(s)
            .precision
    };
    assert_eq!(precision(&good), Exact);
    let stale = Precision::Unmapped {
        reason: UnmappedReason::StaleGeneratedFile,
    };
    let mut s = good.clone();
    s.text[0].text.push('!');
    assert_eq!(precision(&s), stale, "first line text");
    let mut s = good.clone();
    s.text[5].text = "}".into();
    assert_eq!(precision(&s), stale, "last line text");
    let mut s = good.clone();
    s.text[0].highlight_end += 1;
    assert_eq!(precision(&s), stale, "first line highlight end");
    let mut s = good.clone();
    s.text[5].highlight_end += 1;
    assert_eq!(precision(&s), stale, "last line highlight end");
    let mut s = good.clone();
    s.text[5].highlight_start = 2;
    assert_eq!(precision(&s), stale, "last line highlight start");
    let mut s = good.clone();
    s.text.pop();
    assert_eq!(precision(&s), stale, "too few lines");
    let mut s = good.clone();
    s.line_end -= 1;
    assert_eq!(precision(&s), stale, "line_end");
    // documented limit: the lines in between are not compared
    let mut s = good.clone();
    s.text[2].text = "anything".into();
    assert_eq!(precision(&s), Exact);
}

#[test]
fn x05_trace_scanner_consumes_a_whole_location_even_when_it_is_not_ours() {
    let (src, program) = mini();
    let is_gen = |f: &str| f.ends_with(".rs");
    let r = Remapper::new(&program, &src, &is_gen);
    // `:5` after a column belongs to no file, `x.rs:3:4:5` is one hit
    let hits = r.remap_trace("x.rs:3:4:5 y.rs:6");
    let got: Vec<_> = hits
        .iter()
        .map(|h| (h.file.as_str(), h.line, h.column))
        .collect();
    assert_eq!(got, [("x.rs", 3, Some(4)), ("y.rs", 6, None)]);
    // a location in a file we do not own is skipped and does not leak into the next token
    let only_y = |f: &str| f == "y.rs" || f == "x.rs:3";
    let r = Remapper::new(&program, &src, &only_y);
    let hits = r.remap_trace("x.rs:3:4 y.rs:6:7");
    let got: Vec<_> = hits.iter().map(|h| h.file.as_str()).collect();
    assert_eq!(got, ["y.rs"]);
    // a second colon-number pair right after a hit is not re-read as a file named after the first
    let any = |_: &str| true;
    let r = Remapper::new(&program, &src, &any);
    assert_eq!(r.remap_trace("a:1:2:3:4").len(), 1);
    // numbers that do not fit are not locations
    assert!(r.remap_trace("a.rs:99999999999999999999999").is_empty());
    // the unit follows the context
    let hits = r
        .remap_trace("panicked at a.rs:1:1:\nat b.rs:2:2\n  at c.rs:3:3\nbat d.rs:4:4\n(e.rs:5:5)");
    let units: Vec<_> = hits.iter().map(|h| (h.file.as_str(), h.unit)).collect();
    assert_eq!(
        units,
        [
            ("a.rs", ColumnUnit::DisplayWidth),
            ("b.rs", ColumnUnit::Utf8Bytes),
            ("c.rs", ColumnUnit::Utf8Bytes),
            ("d.rs", ColumnUnit::DisplayWidth),
            ("e.rs", ColumnUnit::DisplayWidth),
        ]
    );
}

#[test]
fn x06_a_column_after_an_unverified_width_never_points_across_expressions() {
    // line 2 is `AAA<U+0F00>BBB`: two expressions on one line, a character of unknown display width between
    let src = "AAA BBB".to_string();
    let rust = "async {\nAAA\u{0f00}BBB\n}\n".to_string();
    let a = rust.find("AAA").unwrap();
    let b = rust.find("BBB").unwrap();
    let program = GeneratedProgram::new(
        rust.clone(),
        SourceMap::new(vec![
            Segment::new(
                ByteRange {
                    start: a,
                    end: a + 3,
                },
                ByteRange { start: 0, end: 3 },
                SegmentKind::StreamVariable,
                0,
                None,
            ),
            Segment::new(
                ByteRange {
                    start: b,
                    end: b + 3,
                },
                ByteRange { start: 4, end: 7 },
                SegmentKind::StreamVariable,
                1,
                None,
            ),
        ]),
    );
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    // the `B`: scalar column 5, byte column 7, display column 5 if U+0F00 had width 1
    let scalar = r.locate("gen.rs", 2, Some(5), ColumnUnit::ScalarValues);
    assert_eq!(scalar.precision, Seg);
    assert_eq!(slice(&src, &scalar), "BBB");
    let bytes = r.locate("gen.rs", 2, Some(7), ColumnUnit::Utf8Bytes);
    assert_eq!(slice(&src, &bytes), "BBB");
    let display = r.locate("gen.rs", 2, Some(5), ColumnUnit::DisplayWidth);
    assert_unmapped(&display, UnmappedReason::MultipleExpressions);
    // before the unknown character the display width is still trusted
    let before = r.locate("gen.rs", 2, Some(2), ColumnUnit::DisplayWidth);
    assert_eq!(slice(&src, &before), "AAA");
}

#[test]
fn x07_lines_without_code_and_the_trailing_empty_line_are_not_mapped() {
    let (src, program) = mini();
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    // blank separator lines are glue, even though a segment ends on the line before
    let blank = program
        .rust
        .split('\n')
        .position(|l| l.is_empty())
        .expect("generated code has blank lines")
        + 1;
    assert_unmapped(
        &r.locate("gen.rs", blank, None, ColumnUnit::Utf8Bytes),
        UnmappedReason::Glue,
    );
    // the generated code ends in "\n": the line after it is not part of the program
    let lines = program.rust.matches('\n').count();
    assert!(program.rust.ends_with('\n'));
    for line in [lines + 1, lines + 2] {
        assert_unmapped(
            &r.locate("gen.rs", line, None, ColumnUnit::Utf8Bytes),
            UnmappedReason::OutsideGeneratedRegion,
        );
        assert_unmapped(
            &r.locate("gen.rs", line, Some(1), ColumnUnit::Utf8Bytes),
            UnmappedReason::OutsideGeneratedRegion,
        );
    }
}

#[test]
fn x08_cargo_output_with_crlf_line_endings_is_read() {
    let good =
        r#"{"$message_type":"diagnostic","message":"ok","level":"error","spans":[],"children":[]}"#;
    let p = parse_messages(&format!("{good}\r\n\r\n{good}\r\n"));
    assert_eq!(p.messages.len(), 2);
    assert!(p.errors.is_empty() && p.skipped == 0);
}

#[test]
fn x09_for_every_program_of_the_corpus_every_byte_agrees_with_the_t1_lookup() {
    let files = corpus();
    assert!(!files.is_empty());
    let mut checked = 0;
    for path in files {
        let src = std::fs::read_to_string(&path).unwrap();
        let Ok(program) = marigold_generate(&src) else {
            continue;
        };
        let is_gen = |f: &str| f == "gen.rs";
        let r = Remapper::new(&program, &src, &is_gen);
        for off in 0..program.rust.len() {
            if !program.rust.is_char_boundary(off) || !program.rust.is_char_boundary(off + 1) {
                continue;
            }
            let mut s = RustcSpan::default();
            s.file_name = "gen.rs".into();
            s.byte_start = off;
            s.byte_end = off + 1;
            let m = r.map_span(&s);
            assert_span_invariants(&src, &program.rust, &m);
            match program.map.source_for_generated(off) {
                None => assert_unmapped(&m, UnmappedReason::Glue),
                Some(seg) => {
                    let got = m.segment.as_ref().unwrap();
                    assert_eq!(
                        (got.kind, got.expr_index),
                        (seg.kind, seg.expr_index),
                        "{path:?} byte {off}"
                    );
                    let r = m.marigold.as_ref().unwrap().range;
                    assert!(
                        seg.source.start <= r.start && r.end <= seg.source.end,
                        "{path:?} byte {off}"
                    );
                }
            }
            checked += 1;
        }
    }
    assert!(checked > 10_000, "{checked}");
}

#[test]
fn x10_the_last_program_line_may_continue_into_text_after_the_program() {
    // The program has no trailing newline, so the wrapper's closing text shares its last line.
    let src = "fn f(x: i32) -> i32 { x }";
    let rust = "async {\nconst fn f(x: i32) -> i32 { x }";
    let file = format!("{rust} // wrapper tail\n");
    let segment = Segment::new(
        ByteRange {
            start: 8,
            end: rust.len(),
        },
        ByteRange {
            start: 0,
            end: src.len(),
        },
        SegmentKind::Function,
        0,
        Some("f".into()),
    );
    let program = GeneratedProgram::new(rust.to_string(), SourceMap::new(vec![segment]));
    let at = rust.find("{ x }").unwrap() + 2;
    let sp = rustc_span("gen.rs", &file, at, at + 1, true);
    assert_eq!(
        sp.text[0].text,
        "const fn f(x: i32) -> i32 { x } // wrapper tail"
    );
    assert_mapped(src, &map_one(src, &program, "", &sp), Exact, "x", (1, 23));
    // the part of the line that is the program's must still agree with the reported text
    let mut other = sp.clone();
    other.text[0].text = "const fn g(x: i32) -> i32 { x } // wrapper tail".into();
    assert_unmapped(
        &map_one(src, &program, "", &other),
        UnmappedReason::StaleGeneratedFile,
    );
    // a span ending exactly at the end of the program, and the insertion point there
    let end = rust.len();
    for (a, b) in [(end - 1, end), (end, end)] {
        let m = map_one(src, &program, "", &rustc_span("gen.rs", &file, a, b, true));
        assert_eq!(m.precision, Exact, "{a}..{b}: {m:?}");
    }
}

#[test]
fn x11_only_function_text_is_ever_exact_even_when_a_struct_or_enum_ends_in_the_same_byte() {
    // Generated structs/enums end in `}` just like their Marigold source does, so a byte-suffix
    // coincidence exists; it must not be reported as a proven-identical text.
    let src = "struct S { a: u8 }\nenum E { A, B }\nrange(0, 1).write_file(\"o.csv\", csv)";
    let program = marigold_generate(src).unwrap();
    for kind in [SegmentKind::Struct, SegmentKind::Enum] {
        let seg = program
            .map
            .segments()
            .iter()
            .find(|s| s.kind == kind)
            .unwrap()
            .clone();
        assert_eq!(program.rust.as_bytes()[seg.generated.end - 1], b'}');
        assert_eq!(src.as_bytes()[seg.source.end - 1], b'}');
        let end = seg.generated.end;
        for (a, b) in [(end - 1, end), (end, end)] {
            let m = map_one(
                src,
                &program,
                "",
                &rustc_span("gen.rs", &program.rust, a, b, true),
            );
            assert_eq!(m.precision, Seg, "{kind:?} {a}..{b}: {m:?}");
            assert_eq!(slice(src, &m), &src[seg.source.start..seg.source.end]);
        }
    }
}

#[test]
fn x12_the_declared_prefix_is_part_of_the_first_line_text_rustc_reports() {
    let (src, program) = mini();
    let prefix = "    ";
    let file = format!("{prefix}{}", program.rust);
    // bytes 4..9 are `async` on the first program line
    let sp = rustc_span("gen.rs", &file, 4, 9, true);
    assert_eq!(sp.text[0].text, format!("{prefix}async {{"));
    assert_unmapped(&map_one(&src, &program, prefix, &sp), UnmappedReason::Glue);
    // the same highlight numbers over a line text that lacks the prefix describe another file
    let mut other = sp.clone();
    other.text[0].text = "async {".to_string();
    assert_unmapped(
        &map_one(&src, &program, prefix, &other),
        UnmappedReason::StaleGeneratedFile,
    );
}

#[test]
fn x13_an_expression_span_takes_the_widest_segment_not_the_first_one() {
    // two pieces of expression 0 (no enclosing segment), the second one wider in Marigold
    let src = "AA BBBBBB";
    let rust = "async {\n  aa\n  bbbbbb\n}\n";
    let (a, b) = (rust.find("aa").unwrap(), rust.find("bbbbbb").unwrap());
    let piece = |g0: usize, g1: usize, s0: usize, s1: usize| {
        Segment::new(
            ByteRange { start: g0, end: g1 },
            ByteRange { start: s0, end: s1 },
            SegmentKind::StreamVariable,
            0,
            None,
        )
    };
    let program = GeneratedProgram::new(
        rust.to_string(),
        SourceMap::new(vec![piece(a, a + 2, 0, 2), piece(b, b + 6, 3, 9)]),
    );
    // from the last byte of the first piece to the first byte of the second one
    let m = map_one(
        src,
        &program,
        "",
        &rustc_span("gen.rs", rust, a + 1, b + 1, true),
    );
    assert_eq!(m.precision, Expression, "{m:?}");
    assert_eq!(slice(src, &m), "BBBBBB");
}

#[test]
fn x14_a_prefix_with_an_unverified_display_width_is_not_trusted_either() {
    let src = "fn a() {}";
    let rust = "fn a() {}\n";
    let program = GeneratedProgram::new(
        rust.to_string(),
        SourceMap::new(vec![Segment::new(
            ByteRange { start: 0, end: 9 },
            ByteRange { start: 0, end: 9 },
            SegmentKind::Function,
            0,
            Some("a".into()),
        )]),
    );
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, src, &is_gen).with_prefix("\u{0f00}");
    // the `a`: after the 3-byte prefix and `fn ` (6 bytes, 5 scalars)
    let bytes = r.locate("gen.rs", 1, Some(7), ColumnUnit::Utf8Bytes);
    assert_mapped(src, &bytes, Exact, "", (1, 4));
    let scalar = r.locate("gen.rs", 1, Some(5), ColumnUnit::ScalarValues);
    assert_mapped(src, &scalar, Exact, "", (1, 4));
    // U+0F00 is not in the verified table: the column cannot be trusted
    let display = r.locate("gen.rs", 1, Some(5), ColumnUnit::DisplayWidth);
    assert_eq!(display.precision, Seg, "{display:?}");
    assert_eq!(slice(src, &display), "fn a() {}");
}

#[test]
fn x15_an_empty_code_is_no_code() {
    let (src, program) = mini();
    let is_gen = |f: &str| f == "gen.rs";
    let r = Remapper::new(&program, &src, &is_gen);
    let d: RustcDiagnostic = serde_json::from_value(json!({
        "message": "m", "code": {"code": ""}, "level": "warning", "spans": [], "children": []
    }))
    .unwrap();
    assert_eq!(r.remap(&d).code, None);
}
