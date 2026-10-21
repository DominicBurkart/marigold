//! Integration tests for `marigold_generate` and the `source_map` module (issue #400, task T1).
//!
//! Oracles used here:
//! * `marigold_parse`            -- generated Rust must match byte for byte;
//! * `common::expr_ranges`       -- independent per-expression source range convention;
//! * `marigold_stream_complexities` / `marigold_symbols` -- other public APIs that already
//!   report `expr_index` + `ByteRange` for (parts of) the same expressions;
//! * brute-force lookups         -- naive O(n) scans over `SourceMap::segments()`.

mod common;

use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::source_map::{GeneratedProgram, Segment, SegmentKind as K};
use marigold_grammar::symbols::SymbolKind;
use marigold_grammar::{
    marigold_generate, marigold_parse, marigold_stream_complexities, marigold_symbols,
};
use proptest::prelude::*;

// ---------------------------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------------------------

/// Everything `generate_rust_code` writes before the first section (pinned so that a builder that
/// forgets the prefix offset is caught right away).
const PREAMBLE: &str = "async {\n    use ::marigold::marigold_impl::*;\n    ";

fn generate(src: &str) -> GeneratedProgram {
    marigold_generate(src).unwrap_or_else(|e| panic!("{src:?} should generate: {e}"))
}

fn gen_text<'a>(g: &'a GeneratedProgram, s: &Segment) -> &'a str {
    &g.rust[s.generated.start..s.generated.end]
}

fn src_text<'a>(src: &'a str, s: &Segment) -> &'a str {
    &src[s.source.start..s.source.end]
}

/// `(kind, expr_index, name, source text)` for every segment, in generated order.
type Row = (K, usize, Option<&'static str>, &'static str);

fn rows(src: &str, g: &GeneratedProgram) -> Vec<(K, usize, Option<String>, String)> {
    g.map
        .segments()
        .iter()
        .map(|s| {
            (
                s.kind,
                s.expr_index,
                s.name.clone(),
                src_text(src, s).to_string(),
            )
        })
        .collect()
}

fn assert_rows(src: &str, expected: &[Row]) {
    let g = generate(src);
    let expected: Vec<_> = expected
        .iter()
        .map(|(k, i, n, t)| (*k, *i, n.map(str::to_string), t.to_string()))
        .collect();
    assert_eq!(rows(src, &g), expected, "program: {src:?}");
    assert_invariants(src, &g);
}

fn overlaps(a: ByteRange, b: ByteRange) -> bool {
    a.start < b.end && b.start < a.end
}

/// Brute-force oracle for `source_for_generated`: innermost (shortest) segment containing the
/// byte; with equal length the later one in storage order (deeper in a pre-order list).
fn naive_at(segs: &[Segment], off: usize) -> Option<&Segment> {
    segs.iter()
        .rev()
        .filter(|s| s.generated.start <= off && off < s.generated.end)
        .min_by_key(|s| s.generated.end - s.generated.start)
}

/// Brute-force oracle for `generated_for_source`: half-open overlap, a zero-width query is a
/// point query (`start <= p < end`), an inverted query matches nothing, result in generated order.
fn naive_for_source(segs: &[Segment], q: ByteRange) -> Vec<&Segment> {
    let hit = |s: &Segment| {
        if q.start > q.end {
            false
        } else if q.start == q.end {
            s.source.start <= q.start && q.start < s.source.end
        } else {
            overlaps(s.source, q)
        }
    };
    segs.iter().filter(|s| hit(s)).collect()
}

/// Char-boundary offsets worth probing: every segment edge +-1, plus 0 and len.
fn probe_points(text: &str, edges: impl Iterator<Item = usize>) -> Vec<usize> {
    let mut pts = vec![0, text.len()];
    for e in edges {
        for d in [e.wrapping_sub(1), e, e + 1] {
            if d <= text.len() && text.is_char_boundary(d) {
                pts.push(d);
            }
        }
    }
    pts.sort_unstable();
    pts.dedup();
    pts
}

/// Every invariant that must hold for *any* accepted program. Called by golden, corpus and
/// property tests alike.
fn assert_invariants(src: &str, g: &GeneratedProgram) {
    let ctx = || format!("program {src:?}");
    let segs = g.map.segments();

    // (a) hard invariant: identical to marigold_parse, byte for byte.
    assert_eq!(g.rust, marigold_parse(src).unwrap(), "{}", ctx());
    assert!(g.rust.starts_with(PREAMBLE), "{}", ctx());

    let exprs = common::expr_ranges(src).expect("accepted program must parse with pest");

    // (b) in bounds, non-empty, on char boundaries -- both sides.
    for s in segs {
        let (gr, sr) = (s.generated, s.source);
        assert!(
            gr.start < gr.end && gr.end <= g.rust.len(),
            "{s:?} {}",
            ctx()
        );
        assert!(
            g.rust.is_char_boundary(gr.start) && g.rust.is_char_boundary(gr.end),
            "{s:?}"
        );
        assert!(sr.start < sr.end && sr.end <= src.len(), "{s:?} {}", ctx());
        assert!(
            src.is_char_boundary(sr.start) && src.is_char_boundary(sr.end),
            "{s:?}"
        );
        assert!(
            gr.start >= PREAMBLE.len(),
            "segment inside the preamble: {s:?}"
        );
    }

    // (c) order + T1 flatness: strictly increasing, pairwise disjoint, separated by glue.
    for w in segs.windows(2) {
        assert!(
            w[0].generated.end < w[1].generated.start,
            "{:?} then {:?}",
            w[0],
            w[1]
        );
    }

    // (d) one expr_index per top-level expression, legal kind combinations, exact source range.
    let mut per_expr: Vec<Vec<K>> = vec![Vec::new(); exprs.len()];
    for s in segs {
        assert!(s.expr_index < exprs.len(), "{s:?} {}", ctx());
        assert_eq!(s.source, exprs[s.expr_index], "{s:?} {}", ctx());
        let t = src_text(src, s);
        assert_eq!(
            t,
            t.trim_end(),
            "source range must not include trailing whitespace"
        );
        per_expr[s.expr_index].push(s.kind);
    }
    for (i, kinds) in per_expr.iter().enumerate() {
        let ok = matches!(
            kinds.as_slice(),
            [K::Struct]
                | [K::Enum]
                | [K::Function]
                | [K::ReturningStream]
                | [K::NonReturningStream]
                | [K::StreamVariable, K::StreamVariableRunner]
        );
        assert!(ok, "expr {i} has kinds {kinds:?} {}", ctx());
    }

    // (d') agree with marigold_stream_complexities on range, index and name.
    for n in marigold_stream_complexities(src).unwrap() {
        let seg = segs
            .iter()
            .find(|s| {
                s.expr_index == n.expr_index
                    && matches!(
                        s.kind,
                        K::StreamVariable | K::ReturningStream | K::NonReturningStream
                    )
            })
            .unwrap_or_else(|| panic!("no segment for node {n:?} {}", ctx()));
        assert_eq!(seg.source, n.range);
        assert_eq!(seg.name, n.name);
    }
    // (d'') agree with marigold_symbols on declarations.
    for d in marigold_symbols(src).declarations {
        let kind = match d.kind {
            SymbolKind::Struct => K::Struct,
            SymbolKind::Enum => K::Enum,
            SymbolKind::Function => K::Function,
            SymbolKind::StreamVariable => K::StreamVariable,
            _ => unreachable!("new SymbolKind: extend this test"),
        };
        let seg = segs
            .iter()
            .find(|s| s.expr_index == d.expr_index && s.kind == kind)
            .unwrap_or_else(|| panic!("no {kind:?} segment for {d:?} {}", ctx()));
        assert_eq!(seg.name.as_deref(), Some(d.name.as_str()));
        assert!(seg.source.start <= d.range.start && d.range.end <= seg.source.end);
    }

    // (e) generated text contains the expected construct for each kind.
    let (mut ret, mut non_ret, mut runner) = (0, 0, 0);
    for s in segs {
        let text = gen_text(g, s);
        let source = src_text(src, s);
        let name = s.name.as_deref();
        match s.kind {
            K::Struct => {
                assert!(
                    text.contains(&format!("struct {} ", name.unwrap())),
                    "{text}"
                );
                assert!(text.trim_end().ends_with('}'), "{text}");
                assert!(source.starts_with("struct") /* grammar allows `structS{..}` */);
            }
            K::Enum => {
                assert!(text.contains(&format!("enum {} ", name.unwrap())), "{text}");
                assert!(source.starts_with("enum") /* grammar allows `enumE{..}` */);
            }
            K::Function => {
                assert!(
                    text.starts_with(&format!("const fn {}(", name.unwrap())),
                    "{text}"
                );
                // the body is copied verbatim, multibyte characters and CRLF included.
                assert!(
                    text.ends_with(&source[source.find('{').unwrap()..]),
                    "{text}"
                );
                assert!(source.starts_with("fn") /* grammar allows `fnf(..)` */);
            }
            K::StreamVariable => {
                assert!(
                    text.starts_with(&format!("let mut {} = {{", name.unwrap())),
                    "{text}"
                );
                assert!(text.contains("MultiConsumerStream::new("), "{text}");
                assert!(text.ends_with("};"), "{text}");
                assert!(source.starts_with(name.unwrap()) && source.contains('='));
            }
            K::StreamVariableRunner => {
                assert!(
                    text.starts_with(&format!(
                        "let stream_variable_runners_{runner} = Box::pin({{"
                    )),
                    "{text}"
                );
                assert!(
                    text.contains(&format!("Box::pin({}.run())", name.unwrap())),
                    "{text}"
                );
                assert!(text.ends_with(");"), "{text}");
                runner += 1;
            }
            K::ReturningStream => {
                assert!(
                    text.starts_with(&format!("let returning_stream_{ret} = Box::pin({{use ")),
                    "{text}"
                );
                assert!(text.ends_with("});"), "{text}");
                assert!(source.ends_with("return"), "{source}");
                assert_eq!(s.name, None);
                ret += 1;
            }
            K::NonReturningStream => {
                assert!(
                    text.starts_with(&format!(
                        "let non_returning_stream_{non_ret} = Box::pin({{use "
                    )),
                    "{text}"
                );
                assert!(text.ends_with("});"), "{text}");
                assert!(
                    source.contains(".write_file(") && source.ends_with(')'),
                    "{source}"
                );
                assert_eq!(s.name, None);
                non_ret += 1;
            }
            _ => panic!("new SegmentKind: extend this test"),
        }
        if matches!(s.kind, K::ReturningStream | K::NonReturningStream) {
            let first = source
                .split(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
                .next()
                .unwrap();
            if !matches!(first, "range" | "read_file" | "select_all") {
                assert!(text.contains(&format!("{first}.get()")), "{text}");
            }
        }
    }
    // the glue's `vec![...]` lists exactly the streams the segments describe.
    for k in 0..ret {
        assert!(g.rust.contains(&format!("\nreturning_stream_{k},\n")));
    }
    assert!(!g.rust.contains(&format!("\nreturning_stream_{ret},\n")));

    // (f) lookups agree with brute force, on every interesting offset / range.
    for off in probe_points(
        &g.rust,
        segs.iter()
            .flat_map(|s| [s.generated.start, s.generated.end]),
    ) {
        assert_eq!(
            g.map.source_for_generated(off),
            naive_at(segs, off),
            "offset {off} {}",
            ctx()
        );
    }
    assert_eq!(g.map.source_for_generated(g.rust.len()), None);
    assert_eq!(g.map.source_for_generated(usize::MAX), None);
    let pts = probe_points(
        src,
        segs.iter().flat_map(|s| [s.source.start, s.source.end]),
    );
    let pts: Vec<usize> = pts.into_iter().take(48).collect();
    for &a in &pts {
        for &b in &pts {
            let q = ByteRange { start: a, end: b };
            assert_eq!(
                g.map.generated_for_source(q),
                naive_for_source(segs, q),
                "query {q:?} {}",
                ctx()
            );
        }
    }
    for s in segs {
        // forward/inverse consistency: the segment is found from every byte it covers ...
        for off in [
            s.generated.start,
            (s.generated.start + s.generated.end) / 2,
            s.generated.end - 1,
        ] {
            assert_eq!(g.map.source_for_generated(off), Some(s));
        }
        // ... and querying its own source range returns it, in generated order.
        let hits = g.map.generated_for_source(s.source);
        assert!(hits.contains(&s));
        assert!(hits
            .windows(2)
            .all(|w| w[0].generated.start < w[1].generated.start));
        assert!(hits.iter().all(|h| overlaps(h.source, s.source)));
    }
    let q = ByteRange {
        start: usize::MAX - 1,
        end: usize::MAX,
    };
    assert!(g.map.generated_for_source(q).is_empty());

    // (g) deterministic.
    let again = generate(src);
    assert_eq!(&again, g);
    assert_eq!(
        serde_json::to_string(&again).unwrap(),
        serde_json::to_string(g).unwrap()
    );
}

// ---------------------------------------------------------------------------------------------
// golden programs (hand-checked against generate_rust_code's section order)
// ---------------------------------------------------------------------------------------------

#[test]
fn g01_unnamed_returning() {
    assert_rows(
        "range(0, 10).return",
        &[(K::ReturningStream, 0, None, "range(0, 10).return")],
    );
}

#[test]
fn g02_unnamed_non_returning() {
    let src = "range(0, 10).write_file(\"out.csv\", csv)";
    assert_rows(src, &[(K::NonReturningStream, 0, None, src)]);
}

#[test]
fn g03_variable_then_named_returning_has_decl_stream_runner_in_generated_order() {
    assert_rows(
        "x = range(0, 5)\nx.return",
        &[
            (K::StreamVariable, 0, Some("x"), "x = range(0, 5)"),
            (K::ReturningStream, 1, None, "x.return"),
            (K::StreamVariableRunner, 0, Some("x"), "x = range(0, 5)"),
        ],
    );
}

#[test]
fn g04_named_non_returning() {
    assert_rows(
        "x = range(0, 5).map(f)\nx.write_file(\"o.csv\", csv)",
        &[
            (K::StreamVariable, 0, Some("x"), "x = range(0, 5).map(f)"),
            (
                K::NonReturningStream,
                1,
                None,
                "x.write_file(\"o.csv\", csv)",
            ),
            (
                K::StreamVariableRunner,
                0,
                Some("x"),
                "x = range(0, 5).map(f)",
            ),
        ],
    );
}

#[test]
fn g05_variable_from_prior_variable() {
    assert_rows(
        "x = range(0, 5)\ny = x.filter(g)\ny.return",
        &[
            (K::StreamVariable, 0, Some("x"), "x = range(0, 5)"),
            (K::StreamVariable, 1, Some("y"), "y = x.filter(g)"),
            (K::ReturningStream, 2, None, "y.return"),
            (K::StreamVariableRunner, 0, Some("x"), "x = range(0, 5)"),
            (K::StreamVariableRunner, 1, Some("y"), "y = x.filter(g)"),
        ],
    );
}

#[test]
fn g06_plain_struct() {
    assert_rows(
        "struct S { a: i32, b: string_10, c: Option<u8> }\nrange(0, 1).return",
        &[
            (
                K::Struct,
                0,
                Some("S"),
                "struct S { a: i32, b: string_10, c: Option<u8> }",
            ),
            (K::ReturningStream, 1, None, "range(0, 1).return"),
        ],
    );
}

#[test]
fn g07_struct_with_bounds_uses_the_bounded_code_path() {
    let src = "struct P { v: int[0, 255], w: uint[1, 10] }\nrange(0, 1).return";
    let g = generate(src);
    let s = &g.map.segments()[0];
    assert_eq!(s.kind, K::Struct);
    let text = gen_text(&g, s);
    assert!(text.contains("V_MAX: u8 = 255"), "{text}");
    assert!(text.contains("W_MIN"), "{text}");
    assert_eq!(
        src_text(src, s),
        "struct P { v: int[0, 255], w: uint[1, 10] }"
    );
    assert_invariants(src, &g);
}

#[test]
fn g08_enum_and_enum_range_stream() {
    assert_rows(
        "enum E { A, B = \"b\", default Z }\nrange(E).return",
        &[
            (K::Enum, 0, Some("E"), "enum E { A, B = \"b\", default Z }"),
            (K::ReturningStream, 1, None, "range(E).return"),
        ],
    );
}

#[test]
fn g09_function() {
    assert_rows(
        "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 3).map(double).return",
        &[
            (
                K::Function,
                0,
                Some("double"),
                "fn double(x: i32) -> i32 { x * 2 }",
            ),
            (
                K::ReturningStream,
                1,
                None,
                "range(0, 3).map(double).return",
            ),
        ],
    );
}

#[test]
fn g10_every_kind_out_of_source_order() {
    // source: stream, fn, var, struct, enum, named non-returning, enum-range stream
    let src = "range(0, 2).return\nfn f(x: i32) -> i32 { x }\nx = range(0, 3).map(f)\nstruct S { a: u8 }\nenum E { A }\nx.write_file(\"o.csv\", csv)\nrange(E).return";
    assert_rows(
        src,
        &[
            (K::Struct, 3, Some("S"), "struct S { a: u8 }"),
            (K::Enum, 4, Some("E"), "enum E { A }"),
            (K::Function, 1, Some("f"), "fn f(x: i32) -> i32 { x }"),
            (K::StreamVariable, 2, Some("x"), "x = range(0, 3).map(f)"),
            (K::ReturningStream, 0, None, "range(0, 2).return"),
            (K::ReturningStream, 6, None, "range(E).return"),
            (
                K::NonReturningStream,
                5,
                None,
                "x.write_file(\"o.csv\", csv)",
            ),
            (
                K::StreamVariableRunner,
                2,
                Some("x"),
                "x = range(0, 3).map(f)",
            ),
        ],
    );
}

#[test]
fn g11_structs_and_enums_share_one_section_in_source_order() {
    assert_rows(
        "struct B { a: u8 }\nenum A { X }\nstruct C { a: u8 }",
        &[
            (K::Struct, 0, Some("B"), "struct B { a: u8 }"),
            (K::Enum, 1, Some("A"), "enum A { X }"),
            (K::Struct, 2, Some("C"), "struct C { a: u8 }"),
        ],
    );
}

#[test]
fn g12_read_file_input() {
    assert_rows(
        "struct R { a: i32 }\nread_file(\"data.csv\", csv, struct=R).ok().return",
        &[
            (K::Struct, 0, Some("R"), "struct R { a: i32 }"),
            (
                K::ReturningStream,
                1,
                None,
                "read_file(\"data.csv\", csv, struct=R).ok().return",
            ),
        ],
    );
}

#[test]
fn g13_select_all_input() {
    let src = "select_all(range(0, 3), range(5, 8).map(f)).filter_map(g).return";
    assert_rows(src, &[(K::ReturningStream, 0, None, src)]);
}

#[test]
fn g14_multiline_expressions_cover_the_whole_expression() {
    assert_rows(
        "x = range(0, 5)\n  .map(f)\n  .filter(g)\nx\n  .return",
        &[
            (
                K::StreamVariable,
                0,
                Some("x"),
                "x = range(0, 5)\n  .map(f)\n  .filter(g)",
            ),
            (K::ReturningStream, 1, None, "x\n  .return"),
            (
                K::StreamVariableRunner,
                0,
                Some("x"),
                "x = range(0, 5)\n  .map(f)\n  .filter(g)",
            ),
        ],
    );
}

#[test]
fn g15_ordinals_follow_generated_order_not_source_order() {
    let src = "a = range(0, 2)\nb = range(2, 4)\na.return\nrange(9, 10).write_file(\"p.csv\", csv)\nb.return\nrange(7, 8).return\na.write_file(\"q.csv\", csv)\nrange(1, 2).write_file(\"r.csv\", csv)";
    let g = generate(src);
    let got: Vec<_> = g
        .map
        .segments()
        .iter()
        .map(|s| (s.kind, s.expr_index))
        .collect();
    assert_eq!(
        got,
        vec![
            (K::StreamVariable, 0),
            (K::StreamVariable, 1),
            (K::ReturningStream, 2),
            (K::ReturningStream, 4),
            (K::ReturningStream, 5),
            (K::NonReturningStream, 3),
            (K::NonReturningStream, 6),
            (K::NonReturningStream, 7),
            (K::StreamVariableRunner, 0),
            (K::StreamVariableRunner, 1),
        ]
    );
    // returning_stream_1 is the *second* returning stream in source order: `b.return`.
    let second_ret = &g.map.segments()[3];
    assert!(gen_text(&g, second_ret).starts_with("let returning_stream_1 = "));
    assert_eq!(src_text(src, second_ret), "b.return");
    assert!(gen_text(&g, second_ret).contains("b.get()"));
    assert_invariants(src, &g);
}

#[test]
fn g16_multibyte_characters_keep_byte_offsets_exact() {
    let src = "struct R { a: i32 }\nenum Mood { Happy = \"\u{1F600}\", Sad = \"\u{fc}n\u{ef}\" }\nfn pick(x: &str) -> bool { /* \u{e9} */ x == \"\u{65e5}\u{672c}\u{8a9e}}\" }\nread_file(\"d\u{e4}t\u{20ac}.csv\", csv, struct=R).ok().return\nrange(0, 1).write_file(\"\u{65e5}\u{672c}\u{8a9e}/\u{f6}.csv\", csv)";
    assert!(src.len() > src.chars().count(), "fixture must be multibyte");
    assert_rows(
        src,
        &[
            (K::Struct, 0, Some("R"), "struct R { a: i32 }"),
            (
                K::Enum,
                1,
                Some("Mood"),
                "enum Mood { Happy = \"\u{1F600}\", Sad = \"\u{fc}n\u{ef}\" }",
            ),
            (
                K::Function,
                2,
                Some("pick"),
                "fn pick(x: &str) -> bool { /* \u{e9} */ x == \"\u{65e5}\u{672c}\u{8a9e}}\" }",
            ),
            (
                K::ReturningStream,
                3,
                None,
                "read_file(\"d\u{e4}t\u{20ac}.csv\", csv, struct=R).ok().return",
            ),
            (
                K::NonReturningStream,
                4,
                None,
                "range(0, 1).write_file(\"\u{65e5}\u{672c}\u{8a9e}/\u{f6}.csv\", csv)",
            ),
        ],
    );
    // generated side: the multibyte literal is inside the segment that came from it.
    let g = generate(src);
    let seg = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::NonReturningStream)
        .unwrap();
    assert!(gen_text(&g, seg).contains("\u{65e5}\u{672c}\u{8a9e}/\u{f6}.csv"));
    // a byte offset in the middle of a multibyte char is inside the segment, and does not panic.
    // (enum values are not emitted into the Rust, so use the fn body's literal.)
    let at = g.rust.find('\u{65e5}').unwrap() + 1;
    assert!(!g.rust.is_char_boundary(at));
    assert_eq!(g.map.source_for_generated(at).unwrap().kind, K::Function);
}

#[test]
fn g17_crlf_line_endings() {
    let src = "x = range(0, 5)  \r\n\r\n  y = x.map(f)\r\nfn f(x: i32) -> i32 {\r\n  x\r\n}\r\n\r\ny.return  \r\n";
    assert_rows(
        src,
        &[
            (
                K::Function,
                2,
                Some("f"),
                "fn f(x: i32) -> i32 {\r\n  x\r\n}",
            ),
            (K::StreamVariable, 0, Some("x"), "x = range(0, 5)"),
            (K::StreamVariable, 1, Some("y"), "y = x.map(f)"),
            (K::ReturningStream, 3, None, "y.return"),
            (K::StreamVariableRunner, 0, Some("x"), "x = range(0, 5)"),
            (K::StreamVariableRunner, 1, Some("y"), "y = x.map(f)"),
        ],
    );
}

#[test]
fn g18_trailing_whitespace_is_excluded_even_when_pest_includes_it_in_the_expr_span() {
    // pest's `expr` span for a stream variable swallows the whitespace that follows it.
    let src = "  x = range(0, 5)   \n\n   \n\n y = x.map(f)   \t\ny.return   ";
    let g = generate(src);
    let var = &g.map.segments()[0];
    assert_eq!(var.source, ByteRange { start: 2, end: 17 });
    assert_eq!(src_text(src, var), "x = range(0, 5)");
    assert_rows(
        src,
        &[
            (K::StreamVariable, 0, Some("x"), "x = range(0, 5)"),
            (K::StreamVariable, 1, Some("y"), "y = x.map(f)"),
            (K::ReturningStream, 2, None, "y.return"),
            (K::StreamVariableRunner, 0, Some("x"), "x = range(0, 5)"),
            (K::StreamVariableRunner, 1, Some("y"), "y = x.map(f)"),
        ],
    );
}

#[test]
fn g19_empty_and_whitespace_only_programs_have_no_segments() {
    for src in ["", "   \n\t \r\n "] {
        let g = generate(src);
        assert!(g.map.segments().is_empty(), "{src:?}");
        assert_eq!(g.rust, marigold_parse(src).unwrap());
        for off in [0, 1, 49, 50, g.rust.len(), g.rust.len() + 1] {
            assert_eq!(g.map.source_for_generated(off), None);
        }
        assert!(g
            .map
            .generated_for_source(ByteRange {
                start: 0,
                end: src.len()
            })
            .is_empty());
        assert_invariants(src, &g);
    }
}

#[test]
fn g20_all_stream_functions_in_one_expression() {
    let src = "range(0, =10).permutations(2).combinations(2).permutations_with_replacement(2).keep_first_n(3, cmp).fold(0, add).map(a).filter(b).filter_map(c).return";
    assert_rows(src, &[(K::ReturningStream, 0, None, src)]);
}

#[test]
fn g21_bounded_struct_referencing_an_enum_and_a_stream_over_the_enum() {
    assert_rows(
        "enum Color { Red, Green }\nstruct P { c: int[0, Color.len()] }\nx = range(Color)\nx.return",
        &[
            (K::Enum, 0, Some("Color"), "enum Color { Red, Green }"),
            (K::Struct, 1, Some("P"), "struct P { c: int[0, Color.len()] }"),
            (K::StreamVariable, 2, Some("x"), "x = range(Color)"),
            (K::ReturningStream, 3, None, "x.return"),
            (K::StreamVariableRunner, 2, Some("x"), "x = range(Color)"),
        ],
    );
}

#[test]
fn g22_only_declarations_and_a_lone_stream_variable() {
    assert_rows(
        "enum E { A }\nstruct S { a: u8 }\nfn f(x: i32) -> i32 { x }",
        &[
            (K::Enum, 0, Some("E"), "enum E { A }"),
            (K::Struct, 1, Some("S"), "struct S { a: u8 }"),
            (K::Function, 2, Some("f"), "fn f(x: i32) -> i32 { x }"),
        ],
    );
    // no returning stream => the `Item=()` flavour of the glue; still no segment for it.
    assert_rows(
        "x = range(0, 3)",
        &[
            (K::StreamVariable, 0, Some("x"), "x = range(0, 3)"),
            (K::StreamVariableRunner, 0, Some("x"), "x = range(0, 3)"),
        ],
    );
}

#[test]
fn g23_realistic_multiline_program_from_examples_bounded_types() {
    let src = "        struct Sensor {\n            reading: int[-40, 85]\n        }\n\n        fn to_sensor(i: i32) -> Sensor {\n            Sensor { reading: i as i8 }\n        }\n\n        range(0, 3)\n            .map(to_sensor)\n            .return\n";
    assert_rows(
        src,
        &[
            (K::Struct, 0, Some("Sensor"), "struct Sensor {\n            reading: int[-40, 85]\n        }"),
            (K::Function, 1, Some("to_sensor"), "fn to_sensor(i: i32) -> Sensor {\n            Sensor { reading: i as i8 }\n        }"),
            (K::ReturningStream, 2, None, "range(0, 3)\n            .map(to_sensor)\n            .return"),
        ],
    );
}

// ---------------------------------------------------------------------------------------------
// lookup semantics on real maps
// ---------------------------------------------------------------------------------------------

#[test]
fn source_for_generated_is_half_open_and_none_in_glue() {
    let src = "x = range(0, 5)\nx.return";
    let g = generate(src);
    let [decl, ret, runner] = g.map.segments() else {
        panic!("{:?}", g.map)
    };

    assert_eq!(g.map.source_for_generated(0), None, "preamble");
    assert_eq!(
        g.map.source_for_generated(PREAMBLE.len() - 1),
        None,
        "preamble"
    );
    assert_eq!(g.map.source_for_generated(decl.generated.start), Some(decl));
    assert_eq!(
        g.map.source_for_generated(decl.generated.end - 1),
        Some(decl)
    );
    assert_eq!(
        g.map.source_for_generated(decl.generated.end),
        None,
        "separator after a decl"
    );
    assert_eq!(
        g.map.source_for_generated(decl.generated.end + 1),
        None,
        "separator after a decl"
    );
    assert_eq!(g.map.source_for_generated(ret.generated.start), Some(ret));
    assert_eq!(
        g.map.source_for_generated(ret.generated.end),
        None,
        "newline after a stream"
    );
    assert_eq!(
        g.map.source_for_generated(runner.generated.start),
        Some(runner)
    );
    assert_eq!(
        g.map.source_for_generated(runner.generated.end),
        None,
        "typed_stream_vec glue"
    );
    assert_eq!(
        g.map.source_for_generated(g.rust.len() - 1),
        None,
        "trailing glue"
    );
    assert_eq!(g.map.source_for_generated(g.rust.len()), None);
    assert_eq!(g.map.source_for_generated(usize::MAX), None);
    // the runner maps back to the *declaration's* source: many generated spots, one source range.
    assert_eq!(runner.source, decl.source);
}

#[test]
fn generated_for_source_returns_all_overlaps_in_generated_order() {
    let src = "x = range(0, 5)\nx.return";
    let g = generate(src);
    let [decl, ret, runner] = g.map.segments() else {
        panic!()
    };
    let q = |start, end| g.map.generated_for_source(ByteRange { start, end });

    // inside the declaration: its decl and its runner, but not the stream that merely uses `x`.
    assert_eq!(q(0, 1), vec![decl, runner]);
    // the whole program: generated order (decl, stream, runner), not source order.
    assert_eq!(q(0, src.len()), vec![decl, ret, runner]);
    // spans the end of expr 0, the newline and the start of expr 1.
    assert_eq!(q(14, 17), vec![decl, ret, runner]);
    // exactly the newline between expressions: nothing.
    assert_eq!(q(15, 16), Vec::<&Segment>::new());
    // half-open: touching ranges do not overlap.
    assert_eq!(q(15, 15 + 1), Vec::<&Segment>::new());
    assert_eq!(q(decl.source.end, ret.source.start), Vec::<&Segment>::new());
    assert_eq!(
        q(ret.source.end, ret.source.end + 5),
        Vec::<&Segment>::new()
    );
    // zero-width query == "the segment covering this byte" (cursor position semantics).
    assert_eq!(q(0, 0), vec![decl, runner]);
    assert_eq!(q(16, 16), vec![ret]);
    assert_eq!(q(15, 15), Vec::<&Segment>::new());
    assert_eq!(q(src.len(), src.len()), Vec::<&Segment>::new());
    // degenerate input never panics.
    assert!(q(5, 2).is_empty());
    assert!(q(usize::MAX - 1, usize::MAX).is_empty());
}

#[test]
fn lookups_work_on_the_out_of_order_program() {
    let src = "range(0, 2).return\nfn f(x: i32) -> i32 { x }\nx = range(0, 3).map(f)\nx.return";
    let g = generate(src);
    // the first source expression is emitted *after* the function and the variable.
    let first = g.map.generated_for_source(ByteRange { start: 0, end: 5 });
    assert_eq!(first.len(), 1);
    assert_eq!(first[0].kind, K::ReturningStream);
    assert_eq!(first[0].expr_index, 0);
    assert!(
        first[0].generated.start
            > g.map.generated_for_source(ByteRange { start: 19, end: 20 })[0]
                .generated
                .start
    );
    // the function's generated text maps back to the function's source.
    let at = g.rust.find("const fn f").unwrap();
    let back = g.map.source_for_generated(at + 3).unwrap();
    assert_eq!((back.kind, back.expr_index), (K::Function, 1));
    assert_eq!(src_text(src, back), "fn f(x: i32) -> i32 { x }");
}

// ---------------------------------------------------------------------------------------------
// corpus + properties
// ---------------------------------------------------------------------------------------------

#[test]
fn every_corpus_program_satisfies_all_invariants() {
    let files = common::corpus();
    assert!(files.len() >= 30, "{files:?}");
    for path in files {
        let src = std::fs::read_to_string(&path).unwrap();
        match marigold_parse(&src) {
            Ok(_) => assert_invariants(&src, &generate(&src)),
            Err(e) => assert_eq!(
                marigold_generate(&src).unwrap_err().0,
                e.0,
                "{}",
                path.display()
            ),
        }
    }
}

#[test]
fn corpus_programs_with_several_expressions_produce_expected_segment_counts() {
    // tests/programs/multi_consumer.marigold: fn, var, ret, var(from prior), ret
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/programs/multi_consumer.marigold");
    let src = std::fs::read_to_string(path).unwrap();
    let kinds: Vec<K> = generate(&src)
        .map
        .segments()
        .iter()
        .map(|s| s.kind)
        .collect();
    assert_eq!(
        kinds,
        vec![
            K::Function,
            K::StreamVariable,
            K::StreamVariable,
            K::ReturningStream,
            K::ReturningStream,
            K::StreamVariableRunner,
            K::StreamVariableRunner,
        ]
    );
}

/// The grammar skips ASCII whitespace between tokens, so the same expression can be written many
/// ways. Every construct the invariants inspect appears here with whitespace in each position the
/// grammar allows, and with hyphenated identifiers (hyphens are part of identifiers). The
/// mutation proptest finds such programs by chance; these cases make them deterministic.
const WHITESPACE_VARIANTS: &[&str] = &[
    // the proptest regression: whitespace after the dot
    "v0. write_file(\"\u{65e5}0.csv\", csv)",
    "v0 . write_file(\"a.csv\", csv)",
    "v0\n.\twrite_file(\"a.csv\", csv)",
    "range(0, 1) . write_file(\"a.csv\", csv)",
    "range(0, 1).write_file( \"a.csv\" , csv )",
    "range(0, 1).write_file(\"a.csv\" , csv , compression = gz)",
    "range(0, 1).write_file(\"a.csv\",csv,compression=none)",
    "range(0, 1).write_file(\"a.csv\", csv\n)",
    "x = range(0, 1)\nx\n.\nwrite_file(\"a.csv\", csv)",
    // return
    "range(0, 1) . return",
    "range(0, 1).\nreturn",
    "range(0, 1)\n\t.\r\n\treturn",
    "x = range(0, 1)\nx . return",
    "x=range(0,1)\nx.return",
    // range
    "range( 0 , 1 ).return",
    "range(0 ,= 1).return",
    "range(\n0,\n=\n1\n).return",
    "range( E ).return\nenum E { A }",
    // stream variables and chains
    "x =\n range(0, 1) . map(f)",
    "x = range(0, 1)\n  .map(f)\n  .filter(g)\nx.return",
    "x = range(0, 1)\ny = x . map(f)\ny . return",
    "x = range(0, 1)\ny=x.map(f)\ny.return",
    "x = range(0, 1)\ny =\n\tx\n\t.map(f)\ny.return",
    // read_file
    "read_file( \"d.csv\" , csv , struct = S ) . ok() . return\nstruct S { a: u8 }",
    "read_file(\"d.csv\",csv,struct=S,infer_compression=true).ok().return\nstruct S { a: u8 }",
    // select_all
    "select_all( range(0, 1) . map(f) , range(1, 2) ) . return",
    "select_all(\n\trange(0, 1),\n\trange(1, 2)\n)\n.\nfilter(g)\n.\nreturn",
    // struct, enum and fn headers
    "struct\nS\n{\na : u8 ,\nb : string_1 ,\n}\nrange(0, 1).return",
    "structS{a:u8}\nrange(0, 1).return",
    "enum\nE\n{ A , B = \"b\" , default Z }\nrange(E).return",
    "enumE{A}\nrange(E).return",
    "fn\nf ( x : i32 )\n->\ni32\n{ x }\nrange(0, 1).map(f).return",
    "fnf(x: i32) -> i32 {x}\nrange(0, 1).map(f).return",
    "fn f(x: i32) -> i32 { /* { */ x }\nrange(0, 1).map(f).return",
    // hyphens are part of identifiers: the leading letters of a name are not the whole name
    "v0-a = range(0, 1)\nv0-a.return",
    "v0-a = range(0, 1)\nv0-a.write_file(\"a.csv\", csv)",
    "x- = range(0, 1)\nx-.return",
    // a stream variable may be called like an input function
    "range = range(0, 1)\nrange.return",
    "read_file = range(0, 1)\nread_file . write_file(\"a.csv\", csv)",
    "select_all = range(0, 1)\nselect_all-x = select_all.map(f)\nselect_all-x.return",
];

#[test]
fn whitespace_variants_keep_all_invariants() {
    // Check every case before failing, so one run lists all the brittle ones.
    let failing: Vec<&str> = WHITESPACE_VARIANTS
        .iter()
        .copied()
        .filter(|src| {
            assert!(
                marigold_parse(src).is_ok(),
                "the case itself must be valid Marigold: {src:?}: {:?}",
                marigold_parse(src).map(|_| ())
            );
            std::panic::catch_unwind(|| assert_invariants(src, &generate(src))).is_err()
        })
        .collect();
    assert!(failing.is_empty(), "invariants failed for: {failing:#?}");
}

/// One chunk of program text built from a template index and a size parameter. Names are unique
/// per position, so the generated program never declares the same name twice.
fn chunk(kind: u8, n: u32, i: usize) -> String {
    match kind % 12 {
        0 => format!("struct S{i} {{ a: u8, b: string_{} }}", n % 9 + 1),
        1 => format!("struct B{i} {{ v: int[0, {}], w: uint[1, 9] }}", n + 1),
        2 => format!("enum E{i} {{ A, B = \"\u{fc}\u{1F600}{n}\", default Z }}"),
        3 => format!("fn f{i}(x: i32) -> i32 {{ /* \u{e9}\u{65e5} */ let s = \"}}{{\"; x + {n} }}"),
        4 => format!("v{i} = range(0, {}).map(f{i})", n + 1),
        5 => format!("w{i} = v{}.filter(g{i})", i / 2),
        6 => format!("range({n}, {}).return", n + 3),
        7 => format!("v{}.return", i / 2),
        8 => format!("range(0, {n}).write_file(\"\u{f6}{i}.csv\", csv)"),
        9 => format!("v{}.write_file(\"\u{65e5}{i}.csv\", csv)", i / 2),
        10 => format!("read_file(\"d{i}.csv\", csv, struct=S{i}).ok().return"),
        // two expressions in one chunk: an enum and a stream over it (declared in any order).
        _ => format!("range(E{i}).return\nenum E{i} {{ P, Q }}"),
    }
}

fn arb_whitespace() -> impl Strategy<Value = &'static str> {
    prop_oneof![
        Just("\n"),
        Just("\r\n"),
        Just(" "),
        Just("\n\n\t "),
        Just("  \r\n \r\n"),
    ]
}

fn arb_program() -> impl Strategy<Value = String> {
    (
        prop::collection::vec((any::<u8>(), 0u32..40, arb_whitespace()), 0..14),
        arb_whitespace(),
    )
        .prop_map(|(chunks, lead)| {
            let mut src = lead.to_string();
            for (i, (kind, n, ws)) in chunks.iter().enumerate() {
                src.push_str(&chunk(*kind, *n, i));
                src.push_str(ws);
            }
            src
        })
}

proptest! {
    #[test]
    fn generated_valid_programs_satisfy_all_invariants(src in arb_program()) {
        prop_assert!(marigold_parse(&src).is_ok(), "generator produced invalid program {:?}", src);
        assert_invariants(&src, &generate(&src));
    }

    #[test]
    fn generate_agrees_with_parse_on_arbitrary_text(src in "\\PC{0,80}") {
        match (marigold_parse(&src), marigold_generate(&src)) {
            (Ok(rust), Ok(g)) => prop_assert_eq!(rust, g.rust),
            (Err(a), Err(b)) => prop_assert_eq!(a.0, b.0),
            (a, b) => prop_assert!(false, "parse={:?} generate={:?}", a.map(|_| ()), b.map(|_| ())),
        }
    }

    #[test]
    fn mutated_programs_agree_with_parse_and_keep_invariants(
        base in arb_program(),
        at in any::<prop::sample::Index>(),
        insert in prop::option::of("[\\PC]"),
    ) {
        let boundaries: Vec<usize> = (0..=base.len()).filter(|i| base.is_char_boundary(*i)).collect();
        let pos = boundaries[at.index(boundaries.len())];
        let mut src = base.clone();
        match insert {
            Some(s) => src.insert_str(pos, &s),
            None => if pos < src.len() { src.remove(pos); },
        }
        match marigold_parse(&src) {
            Ok(rust) => {
                let g = generate(&src);
                prop_assert_eq!(&g.rust, &rust);
                assert_invariants(&src, &g);
            }
            Err(e) => prop_assert_eq!(marigold_generate(&src).unwrap_err().0, e.0),
        }
    }
}

// ---------------------------------------------------------------------------------------------
// error paths
// ---------------------------------------------------------------------------------------------

#[test]
fn errors_are_exactly_the_errors_of_marigold_parse() {
    let cases = [
        "range(0,",                                                 // syntax error
        "range(0, 1).retur",                                        // typo in output function
        "x = range(0,3)\nrange(0,1).retur",                         // error after valid chunk
        "// comment\nrange(0,1).return", // comments are not part of the grammar
        "range(Color).return",           // unknown enum (post-parse validation)
        "enum E { A }\nrange(Nope).return", // unknown enum, others declared
        "struct T { f: int[10, 5] }\nrange(0,1).return", // bounded type validation
        "struct T { f: int[0, Missing.len()] }\nrange(0,1).return", // undefined type in bounds
        "fn f(x: i32) -> i32\nrange(0,1).return", // fn without body
        "range(0, 1).return\u{0}",       // NUL
        "x = r\u{e9}nge(0, 1)",          // non-ASCII identifier
    ];
    for src in cases {
        let expected = marigold_parse(src).expect_err(src);
        let got = marigold_generate(src).expect_err(src);
        assert_eq!(got.0, expected.0, "{src:?}");
        assert_eq!(got.to_string(), expected.to_string(), "{src:?}");
    }
}

#[test]
fn unknown_enum_error_message_is_preserved() {
    let err = marigold_generate("range(Color).return").unwrap_err();
    assert_eq!(
        err.0,
        "range(Color): 'Color' is not a declared enum in this program"
    );
}

#[test]
fn oversized_input_is_not_rejected_because_marigold_parse_does_not_reject_it() {
    // `MAX_CHECK_INPUT_BYTES` only guards the diagnostics path (LSP/editor DoS protection).
    // `marigold_generate` must keep `Err iff marigold_parse errs`.
    let src = " ".repeat(marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES + 1);
    let g = generate(&src);
    assert!(g.map.segments().is_empty());
    assert_eq!(g.rust, marigold_parse(&src).unwrap());
}

// ---------------------------------------------------------------------------------------------
// serialization + scale
// ---------------------------------------------------------------------------------------------

#[test]
fn json_shape_is_stable() {
    let src = "x = range(0, 5)\nx.return";
    let g = generate(src);
    let v = serde_json::to_value(&g).unwrap();
    assert_eq!(v["rust"], g.rust);
    let segs = v["map"]["segments"].as_array().unwrap();
    assert_eq!(segs.len(), 3);
    let kinds: Vec<_> = segs.iter().map(|s| s["kind"].as_str().unwrap()).collect();
    assert_eq!(
        kinds,
        [
            "stream_variable",
            "returning_stream",
            "stream_variable_runner"
        ]
    );
    let s0 = &g.map.segments()[0];
    assert_eq!(
        segs[0],
        serde_json::json!({
            "generated": {"start": s0.generated.start, "end": s0.generated.end},
            "source": {"start": 0, "end": 15},
            "kind": "stream_variable",
            "expr_index": 0,
            "name": "x",
        })
    );
    // `Value` indexing yields Null for a missing key too, so look at the key sets themselves:
    // `name` is present (as null) for streams, and no lookup index leaks into the output.
    let keys = |v: &serde_json::Value| {
        let mut keys: Vec<_> = v.as_object().unwrap().keys().cloned().collect();
        keys.sort();
        keys
    };
    assert_eq!(keys(&v), ["map", "rust"]);
    assert_eq!(keys(&v["map"]), ["segments"]);
    for s in segs {
        assert_eq!(
            keys(s),
            ["expr_index", "generated", "kind", "name", "source"]
        );
        assert_eq!(keys(&s["generated"]), ["end", "start"]);
        assert_eq!(keys(&s["source"]), ["end", "start"]);
    }
    assert!(segs[1].as_object().unwrap()["name"].is_null());
    for k in [K::Struct, K::Enum, K::Function, K::NonReturningStream] {
        let s = serde_json::to_value(k).unwrap();
        assert!(
            s.as_str()
                .unwrap()
                .chars()
                .all(|c| c.is_ascii_lowercase() || c == '_'),
            "{s}"
        );
    }
}

fn many_exprs(n: usize) -> String {
    let mut src = String::new();
    for i in 0..n {
        src.push_str(&format!(
            "v{i} = range(0, 5).map(f)\nv{i}.return\nstruct S{i} {{ a: u8 }}\n"
        ));
    }
    src
}

#[test]
fn many_expressions_get_exactly_the_expected_segments() {
    let n = 1_500;
    let src = many_exprs(n);
    let g = generate(&src);
    // per i: var decl + runner + returning stream + struct
    assert_eq!(g.map.segments().len(), 4 * n);
    let count = |k| g.map.segments().iter().filter(|s| s.kind == k).count();
    assert_eq!(count(K::StreamVariable), n);
    assert_eq!(count(K::StreamVariableRunner), n);
    assert_eq!(count(K::ReturningStream), n);
    assert_eq!(count(K::Struct), n);
    assert_invariants_cheap(&src, &g);
}

/// `assert_invariants` is O(n * probes); for the big program only check the structural parts.
fn assert_invariants_cheap(src: &str, g: &GeneratedProgram) {
    assert_eq!(g.rust, marigold_parse(src).unwrap());
    let exprs = common::expr_ranges(src).unwrap();
    for s in g.map.segments() {
        assert_eq!(s.source, exprs[s.expr_index]);
        assert!(g.rust.is_char_boundary(s.generated.end));
    }
    for w in g.map.segments().windows(2) {
        assert!(w[0].generated.end < w[1].generated.start);
    }
}

/// The bytes between segments are exactly the glue code generation writes: the preamble before
/// the first segment, a blank line after each declaration and a newline after each statement.
/// This pins the exact start and end of every segment (a segment that skipped the leading
/// whitespace of a struct, or swallowed its separator, would change a gap).
fn assert_glue_is_exact(src: &str) {
    let g = generate(src);
    let mut previous: Option<(usize, K)> = None;
    for s in g.map.segments() {
        let gap = match previous {
            None => &g.rust[..s.generated.start],
            Some((end, _)) => &g.rust[end..s.generated.start],
        };
        match previous {
            None => assert_eq!(gap, PREAMBLE, "{src:?}"),
            Some((_, K::Struct | K::Enum | K::Function | K::StreamVariable)) => {
                assert_eq!(gap, "\n\n", "{src:?}")
            }
            Some(_) => assert_eq!(gap, "\n", "{src:?}"),
        }
        previous = Some((s.generated.end, s.kind));
    }
}

#[test]
fn gaps_between_segments_are_exactly_the_separators() {
    assert_glue_is_exact(
        "struct S { a: u8 }\nenum E { A }\nfn f(x: i32) -> i32 { x }\nx = range(0, 3)\nx.return",
    );
    assert_glue_is_exact("range(0, 1).return\nrange(0, 2).write_file(\"o.csv\", csv)");
    let mut checked = 0;
    for path in common::corpus() {
        let src = std::fs::read_to_string(&path).unwrap();
        if marigold_generate(&src).is_ok() {
            assert_glue_is_exact(&src);
            checked += 1;
        }
    }
    assert!(checked >= 30, "only {checked} corpus programs checked");
}
