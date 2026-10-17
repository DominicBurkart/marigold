//! Integration tests for the sub-expression segments of `marigold_generate` (issue #400, task T2).
//!
//! Oracles used here (none of them is the code under test):
//! * `marigold_parse`               -- generated Rust must stay byte for byte identical;
//! * `common::expected_nested`      -- the expected nested (kind, source range, ancestors) re-derived
//!   straight from the pest parse tree;
//! * `expected_fn_text` & friends   -- the text each generated region must have, restated from the
//!   source slice of that region (feature-flag aware);
//! * brute-force lookups            -- naive O(n) scans over `SourceMap::segments()`;
//! * `marigold.pest` itself         -- every input/stream/output alternative must have a golden case.

mod common;

use common::{expected_nested, Exp, RULE_KINDS};
use marigold_grammar::diagnostics::ByteRange;
use marigold_grammar::source_map::{GeneratedProgram, Segment, SegmentKind as K};
use marigold_grammar::{marigold_generate, marigold_parse};
use proptest::prelude::*;

const TOKIO_LIKE: bool = cfg!(any(feature = "tokio", feature = "async-std"));
const IO: bool = cfg!(feature = "io");

// ---------------------------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------------------------

fn generate(src: &str) -> GeneratedProgram {
    marigold_generate(src).unwrap_or_else(|e| panic!("{src:?} should generate: {e}"))
}

fn gtext<'a>(g: &'a GeneratedProgram, s: &Segment) -> &'a str {
    &g.rust[s.generated.start..s.generated.end]
}

fn stext<'a>(src: &'a str, s: &Segment) -> &'a str {
    &src[s.source.start..s.source.end]
}

fn depth(g: &GeneratedProgram, mut i: usize) -> usize {
    let mut d = 0;
    while let Some(p) = g.map.parent(i) {
        d += 1;
        i = p;
    }
    d
}

fn is_stmt_kind(k: K) -> bool {
    matches!(
        k,
        K::Struct
            | K::Enum
            | K::Function
            | K::StreamVariable
            | K::StreamVariableRunner
            | K::ReturningStream
            | K::NonReturningStream
    )
}

fn is_input(k: K) -> bool {
    matches!(
        k,
        K::InputRange | K::InputReadFile | K::InputSelectAll | K::InputVariable
    )
}

fn is_stream_fn(k: K) -> bool {
    matches!(
        k,
        K::StreamMap
            | K::StreamFilter
            | K::StreamFilterMap
            | K::StreamPermutations
            | K::StreamPermutationsWithReplacement
            | K::StreamCombinations
            | K::StreamKeepFirstN
            | K::StreamFold
            | K::StreamOk
            | K::StreamOkOrPanic
    )
}

fn pt_up(src: &str, points: impl Iterator<Item = usize>) -> Vec<usize> {
    let mut v: Vec<usize> = points.filter(|p| *p <= src.len()).collect();
    v.sort_unstable();
    v.dedup();
    v
}

// ---------------------------------------------------------------------------------------------
// the text every region must have, restated from the Marigold source slice
// ---------------------------------------------------------------------------------------------

const BUF: &str =
    "buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2))";

/// `name(arg, arg)` -> ("name", ["arg", "arg"]). Commas inside a double-quoted path (which may
/// also contain parentheses) do not separate arguments; no other argument contains one.
fn call_parts(slice: &str) -> (&str, Vec<&str>) {
    match slice.find('(') {
        Some(open) => {
            let inner = &slice[open + 1..slice.len() - 1];
            let mut args = vec![];
            if !inner.trim().is_empty() {
                let (mut quoted, mut from) = (false, 0);
                for (i, c) in inner.char_indices() {
                    match c {
                        '"' => quoted = !quoted,
                        ',' if !quoted => {
                            args.push(inner[from..i].trim());
                            from = i + 1;
                        }
                        _ => {}
                    }
                }
                args.push(inner[from..].trim());
            }
            (&slice[..open], args)
        }
        None => (slice, vec![]),
    }
}

fn expected_fn_text(kind: K, slice: &str) -> String {
    let (_, a) = call_parts(slice);
    match kind {
        K::StreamMap if TOKIO_LIKE => format!("map(|v| async move {{{}(v)}}).{BUF}", a[0]),
        K::StreamMap => format!("map({})", a[0]),
        K::StreamFilter if TOKIO_LIKE => format!(
            "map(|v| async move {{ if {}(v) {{ Some(v) }} else {{ None }} }}).{BUF}.filter_map(|v| v)",
            a[0]
        ),
        K::StreamFilter => format!(
            "filter(|v| ::marigold::marigold_impl::futures::future::ready({}(v.clone())))",
            a[0]
        ),
        K::StreamFilterMap if TOKIO_LIKE => {
            format!("filter_map(|v| async move {{ {}(v) }})", a[0])
        }
        K::StreamFilterMap => format!(
            "filter_map(|v| ::marigold::marigold_impl::futures::future::ready({}(v)))",
            a[0]
        ),
        K::StreamPermutations => format!(
            "permutations({0}).await.map(|v| <[_; {0}]>::try_from(v).unwrap())",
            a[0]
        ),
        K::StreamPermutationsWithReplacement => format!(
            "permutations_with_replacement({0}).await.map(|v| <[_; {0}]>::try_from(v).unwrap())",
            a[0]
        ),
        K::StreamCombinations => format!(
            "combinations({0}).await.map(|v| <[_; {0}]>::try_from(v).unwrap())",
            a[0]
        ),
        K::StreamKeepFirstN => format!("keep_first_n({}, {}).await", a[0], a[1]),
        K::StreamFold => {
            let init = if a[0].parse::<f64>().is_ok() {
                a[0].to_string()
            } else {
                format!("{}()", a[0])
            };
            format!(
                "marifold({init}, |acc, x| futures::future::ready({}(acc, x))).await",
                a[1]
            )
        }
        K::StreamOk => "filter(|r| futures::future::ready(r.is_ok())).map(|r| r.unwrap())".into(),
        K::StreamOkOrPanic => "map(|r| r.unwrap())".into(),
        k => panic!("not a stream function kind: {k:?}"),
    }
}

fn norm(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Expected text of a leaf-ish input region (select_all is checked structurally elsewhere).
fn check_input_text(kind: K, slice: &str, text: &str) {
    match kind {
        K::InputVariable => assert_eq!(text, format!("{slice}.get()")),
        K::InputRange => {
            let (_, a) = call_parts(slice);
            let expected = match a.as_slice() {
                [e] => format!(
                    "::marigold::marigold_impl::futures::stream::iter({e}::__marigold_variants())"
                ),
                [s, e] => match e.strip_prefix('=') {
                    Some(e) => format!(
                        "::marigold::marigold_impl::futures::stream::iter({s}..={})",
                        e.trim()
                    ),
                    None => format!("::marigold::marigold_impl::futures::stream::iter({s}..{e})"),
                },
                _ => panic!("{slice}"),
            };
            assert_eq!(text, expected);
        }
        K::InputReadFile => {
            let (_, a) = call_parts(slice);
            let path = a[0];
            // the grammar allows blanks around the `=` signs
            let squash = |x: &str| x.split_whitespace().collect::<String>();
            let strukt = squash(a[2]).strip_prefix("struct=").unwrap().to_string();
            let infer = a
                .get(3)
                .and_then(|x| {
                    squash(x)
                        .strip_prefix("infer_compression=")
                        .map(str::to_string)
                })
                .map(|v| v == "true");
            let gz = match infer {
                Some(false) => false,
                _ => path.trim_matches('"').ends_with(".gz"),
            };
            let expected = if gz {
                format!("::marigold::marigold_impl::csv_async::AsyncDeserializer::from_reader( ::marigold::marigold_impl::async_compression::tokio::bufread::GzipDecoder::new( ::marigold::marigold_impl::tokio::io::BufReader::new( ::marigold::marigold_impl::tokio::fs::File::open({path}) .await .expect(\"Marigold could not open file\") ) ).compat() ).into_deserialize::<{strukt}>()")
            } else {
                format!("::marigold::marigold_impl::csv_async::AsyncDeserializer::from_reader( ::marigold::marigold_impl::tokio::fs::File::open({path}) .await .expect(\"Marigold could not open file\") .compat() ).into_deserialize::<{strukt}>()")
            };
            assert_eq!(norm(text), expected, "{slice}");
        }
        K::InputSelectAll => {}
        k => panic!("{k:?}"),
    }
}

// ---------------------------------------------------------------------------------------------
// nested invariants
// ---------------------------------------------------------------------------------------------

const USE_BLOCK: &str = "{use ::marigold::marigold_impl::*; ";
const MCS: &str = "::marigold::marigold_impl::multi_consumer_stream::MultiConsumerStream::new(";
const SELECT_HEAD: &str =
    "::marigold::marigold_impl::futures::prelude::stream::select_all::select_all([";
const RUN_STREAM: &str = "::marigold::marigold_impl::run_stream::run_stream(";

/// Check that `chain` (indices of input + stream-function segments, in order) fills
/// `rust[open..close]` exactly: nothing but `.` between neighbours, texts as the formulas say,
/// select_all recursively.
fn check_chain(src: &str, g: &GeneratedProgram, chain: &[usize], open: usize, close: usize) {
    let segs = g.map.segments();
    assert!(!chain.is_empty(), "a chain starts with an input");
    assert_eq!(segs[chain[0]].generated.start, open, "chain start");
    assert_eq!(
        segs[*chain.last().unwrap()].generated.end,
        close,
        "chain end"
    );
    for (n, &i) in chain.iter().enumerate() {
        let s = &segs[i];
        if n == 0 {
            assert!(
                is_input(s.kind),
                "first chain element must be an input: {s:?}"
            );
        } else {
            assert!(is_stream_fn(s.kind), "{s:?}");
            let prev = &segs[chain[n - 1]];
            assert_eq!(
                &g.rust[prev.generated.end..s.generated.start],
                ".",
                "exactly one dot between chain elements"
            );
            // source side: only `.` and whitespace between the elements
            assert_eq!(
                src[prev.source.end..s.source.start].trim_matches(&[' ', '\t', '\r', '\n'][..]),
                ".",
                "source gap between {prev:?} and {s:?}"
            );
            assert_eq!(
                gtext(g, s),
                expected_fn_text(s.kind, stext(src, s)),
                "{s:?}"
            );
        }
        if is_input(s.kind) && s.kind != K::InputSelectAll {
            check_input_text(s.kind, stext(src, s), gtext(g, s));
        }
        if s.kind == K::InputSelectAll {
            let kids = g.map.children(i);
            assert!(!kids.is_empty());
            let text = gtext(g, s);
            assert!(text.starts_with(SELECT_HEAD), "{text}");
            assert!(text.ends_with("])"), "{text}");
            let mut cursor = s.generated.start + SELECT_HEAD.len();
            for (b, &k) in kids.iter().enumerate() {
                let branch = &segs[k];
                assert_eq!(branch.kind, K::SelectAllBranch);
                if b > 0 {
                    assert_eq!(&g.rust[cursor..branch.generated.start], ",\n");
                } else {
                    assert_eq!(branch.generated.start, cursor);
                }
                let bt = gtext(g, branch);
                assert!(bt.starts_with(RUN_STREAM) && bt.ends_with(')'), "{bt}");
                let inner = g.map.children(k);
                check_chain(
                    src,
                    g,
                    &inner,
                    branch.generated.start + RUN_STREAM.len(),
                    branch.generated.end - 1,
                );
                cursor = branch.generated.end;
            }
            assert_eq!(cursor + 2, s.generated.end, "closing `])`");
        }
    }
}

fn check_statement_layout(src: &str, g: &GeneratedProgram, i: usize) {
    let segs = g.map.segments();
    let s = &segs[i];
    let kids = g.map.children(i);
    match s.kind {
        K::ReturningStream | K::NonReturningStream | K::StreamVariable => {
            let head = match s.kind {
                K::StreamVariable => {
                    format!("let mut {} = {USE_BLOCK}{MCS}", s.name.as_deref().unwrap())
                }
                _ => format!("let {} = Box::pin({USE_BLOCK}", s.ident.as_deref().unwrap()),
            };
            let tail = if s.kind == K::StreamVariable {
                ")};"
            } else {
                "});"
            };
            let (mut chain, mut prefix, mut postfix) = (Vec::new(), None, None);
            for &k in &kids {
                if segs[k].kind == K::OutputWriteFile {
                    if chain.is_empty() {
                        assert!(prefix.replace(k).is_none(), "two setup regions");
                    } else {
                        assert!(postfix.replace(k).is_none(), "two sink regions");
                    }
                } else {
                    chain.push(k);
                }
            }
            assert_eq!(
                s.kind == K::NonReturningStream,
                prefix.is_some(),
                "write_file <=> two OutputWriteFile regions"
            );
            assert_eq!(prefix.is_some(), postfix.is_some());
            let text = gtext(g, s);
            assert!(text.starts_with(&head), "{text}");
            assert!(text.ends_with(tail), "{text}");
            let open = match prefix {
                Some(p) => {
                    let p = &segs[p];
                    assert_eq!(&g.rust[s.generated.start..p.generated.start], head);
                    p.generated.end
                }
                None => s.generated.start + head.len(),
            };
            let close = match postfix {
                Some(q) => {
                    let q = &segs[q];
                    assert_eq!(&g.rust[q.generated.end..s.generated.end], tail);
                    q.generated.start
                }
                None => s.generated.end - tail.len(),
            };
            check_chain(src, g, &chain, open, close);
            if let (Some(p), Some(q)) = (prefix, postfix) {
                let (p, q) = (&segs[p], &segs[q]);
                assert_eq!(p.source, q.source, "both regions come from the same call");
                let call = stext(src, p);
                assert!(
                    call.starts_with("write_file(") && call.ends_with(')'),
                    "{call}"
                );
                let (pt, qt) = (gtext(g, p), gtext(g, q));
                let path = call_parts(call).1[0].to_string();
                assert!(pt.starts_with('{'), "{pt}");
                assert!(
                    pt.contains(&format!("::std::path::Path::new({path})")),
                    "{pt}"
                );
                assert!(pt.contains(&format!("File::create({path})")), "{pt}");
                assert!(pt.trim_end().ends_with("let mut stream_to_write ="), "{pt}");
                assert!(qt.trim_start().starts_with(';'), "{qt}");
                assert!(qt.contains("stream_to_write.filter_map("), "{qt}");
                assert!(qt.ends_with('}'), "{qt}");
                let gz = match call_parts(call).1.get(2) {
                    Some(c) => c.contains("gz"),
                    None => path.trim_matches('"').ends_with(".gz"),
                };
                assert_eq!(pt.contains("GzipEncoder"), gz, "{pt}");
                // the source order of the pieces is chain..., call; generated order is call-setup,
                // chain..., call-sink. The chain's source must precede the call.
                let last = &segs[*chain.last().unwrap()];
                assert_eq!(
                    src[last.source.end..p.source.start].trim_matches(&[' ', '\t', '\r', '\n'][..]),
                    "."
                );
            }
        }
        K::StreamVariableRunner => assert!(kids.is_empty(), "runner has no distinct sub-regions"),
        K::Function => {
            let [sig, body] = kids.as_slice() else {
                panic!("a function has exactly signature + body: {kids:?}")
            };
            let (sig, body) = (&segs[*sig], &segs[*body]);
            assert_eq!(
                (sig.kind, body.kind),
                (K::FunctionSignature, K::FunctionBody)
            );
            assert_eq!(&g.rust[s.generated.start..sig.generated.start], "const fn ");
            assert_eq!(&g.rust[sig.generated.end..body.generated.start], " ");
            assert_eq!(body.generated.end, s.generated.end);
            // the body is copied verbatim (multibyte, CRLF, braces in strings) -> linear offset mapping.
            assert_eq!(gtext(g, body), stext(src, body));
            let name = s.name.as_deref().unwrap();
            assert!(
                gtext(g, sig).starts_with(&format!("{name}(")),
                "{}",
                gtext(g, sig)
            );
            assert!(stext(src, sig).starts_with(name));
            assert!(stext(src, sig).contains("->"));
        }
        K::Struct => {
            let name = s.name.as_deref().unwrap();
            let mut prev_end = None;
            for &k in &kids {
                let f = &segs[k];
                assert_eq!(f.kind, K::StructField);
                let fname = f.name.as_deref().expect("field name");
                assert!(stext(src, f).starts_with(fname), "{f:?}");
                assert!(
                    gtext(g, f).starts_with(&format!("{fname}: ")),
                    "{}",
                    gtext(g, f)
                );
                match prev_end {
                    None => assert!(
                        g.rust[s.generated.start..f.generated.start]
                            .trim_end()
                            .ends_with(&format!("struct {name} {{")),
                        "{}",
                        &g.rust[s.generated.start..f.generated.start]
                    ),
                    Some(e) => assert_eq!(&g.rust[e..f.generated.start], ",\n"),
                }
                prev_end = Some(f.generated.end);
            }
            if let Some(e) = prev_end {
                assert!(g.rust[e..].starts_with(",\n}"));
            }
        }
        K::Enum => {
            let mut prev_end = None;
            for &k in &kids {
                let v = &segs[k];
                assert_eq!(v.kind, K::EnumVariant);
                let vname = v.name.as_deref().expect("variant name");
                assert!(stext(src, v).starts_with(vname) || stext(src, v).starts_with("default"));
                assert!(gtext(g, v).starts_with(vname), "{}", gtext(g, v));
                assert!(
                    !gtext(g, v).contains(','),
                    "variant region excludes the separator and serde attributes"
                );
                if let Some(e) = prev_end {
                    if !IO {
                        assert_eq!(&g.rust[e..v.generated.start], ",\n");
                    }
                }
                prev_end = Some(v.generated.end);
            }
        }
        _ => panic!("not a statement-level kind: {s:?}"),
    }
}

/// Every invariant of the NESTED part of the map. T1's `assert_invariants` (patched to look at
/// statement-level segments only) covers the rest.
fn assert_nested(src: &str, g: &GeneratedProgram) {
    let ctx = || format!("program {src:?}");
    let segs = g.map.segments();
    assert_eq!(g.rust, marigold_parse(src).unwrap(), "{}", ctx());

    // N0: nest-or-disjoint; no two segments share a generated range; parent() is the smallest
    // enclosing segment (brute force).
    for (i, s) in segs.iter().enumerate() {
        assert!(
            s.generated.start < s.generated.end && s.generated.end <= g.rust.len(),
            "{s:?}"
        );
        assert!(
            s.source.start < s.source.end && s.source.end <= src.len(),
            "{s:?}"
        );
        assert!(
            g.rust.is_char_boundary(s.generated.start) && g.rust.is_char_boundary(s.generated.end)
        );
        assert!(src.is_char_boundary(s.source.start) && src.is_char_boundary(s.source.end));
        let mut best: Option<usize> = None;
        for (j, t) in segs.iter().enumerate() {
            if i == j {
                continue;
            }
            let (a, b) = (s.generated, t.generated);
            let disjoint = a.end <= b.start || b.end <= a.start;
            let a_in_b = b.start <= a.start && a.end <= b.end;
            let b_in_a = a.start <= b.start && b.end <= a.end;
            assert!(
                disjoint || a_in_b || b_in_a,
                "partial overlap {s:?} / {t:?} {}",
                ctx()
            );
            assert_ne!(
                a, b,
                "two segments with the same generated range: {s:?} / {t:?}"
            );
            if a_in_b
                && best.is_none_or(|k| {
                    let u = segs[k].generated;
                    (b.end - b.start) < (u.end - u.start)
                })
            {
                best = Some(j);
            }
        }
        assert_eq!(g.map.parent(i), best, "parent of {i}: {s:?} {}", ctx());
    }
    assert_eq!(g.map.parent(segs.len()), None);
    assert_eq!(g.map.parent(usize::MAX), None);
    assert!(g.map.children(usize::MAX).is_empty());
    let n_top = g.map.top_level().count();
    assert_eq!(
        n_top,
        (0..segs.len())
            .filter(|&i| g.map.parent(i).is_none())
            .count()
    );

    // N3/N4: children vs parent.
    for (i, s) in segs.iter().enumerate() {
        let kids = g.map.children(i);
        for w in kids.windows(2) {
            assert!(
                segs[w[0]].generated.end <= segs[w[1]].generated.start,
                "siblings are ordered and disjoint (touching is fine): {:?} {:?}",
                segs[w[0]],
                segs[w[1]]
            );
        }
        for &k in &kids {
            let c = &segs[k];
            assert_eq!(g.map.parent(k), Some(i));
            assert_eq!(c.expr_index, s.expr_index, "children share the expr_index");
            assert!(
                s.source.start <= c.source.start && c.source.end <= s.source.end,
                "child source inside parent source: {c:?} in {s:?}"
            );
            // proper sub-range, except a select_all branch that is just one input: `select_all(range(0, 3))`
            // the branch and its input are the same source text.
            if c.source == s.source {
                assert!(
                    s.kind == K::SelectAllBranch && is_input(c.kind) && kids.len() == 1,
                    "child source is a proper sub-range: {c:?} in {s:?}"
                );
            }
            assert!(
                !is_stmt_kind(c.kind),
                "statement kinds are never nested: {c:?}"
            );
            // allowed (parent -> child) kinds
            let ok = match s.kind {
                K::ReturningStream | K::StreamVariable | K::SelectAllBranch => {
                    is_input(c.kind) || is_stream_fn(c.kind)
                }
                K::NonReturningStream => {
                    is_input(c.kind) || is_stream_fn(c.kind) || c.kind == K::OutputWriteFile
                }
                K::InputSelectAll => c.kind == K::SelectAllBranch,
                K::Function => matches!(c.kind, K::FunctionSignature | K::FunctionBody),
                K::Struct => c.kind == K::StructField,
                K::Enum => c.kind == K::EnumVariant,
                _ => false,
            };
            assert!(ok, "{:?} may not contain {:?} {}", s.kind, c.kind, ctx());
        }
        if is_stmt_kind(s.kind) {
            assert_eq!(g.map.parent(i), None, "statement-level segments are roots");
        } else {
            assert!(g.map.parent(i).is_some(), "{s:?} must be nested");
        }
    }

    // N5/N6: the nested part equals the independently derived expectation.
    let mut got: Vec<Exp> = (0..segs.len())
        .filter(|&i| g.map.parent(i).is_some())
        .map(|i| {
            let mut path = Vec::new();
            let mut c = g.map.parent(i);
            while let Some(p) = c {
                path.push(segs[p].kind);
                c = g.map.parent(p);
            }
            path.reverse();
            Exp {
                expr_index: segs[i].expr_index,
                kind: segs[i].kind,
                source: segs[i].source,
                path,
            }
        })
        .collect();
    let mut want = expected_nested(src).expect("accepted program parses with pest");
    got.sort_by_key(Exp::key);
    want.sort_by_key(Exp::key);
    assert_eq!(got, want, "{}", ctx());

    // N7: laid out exactly as the code generator writes it.
    for (i, s) in segs.iter().enumerate() {
        if is_stmt_kind(s.kind) {
            check_statement_layout(src, g, i);
        }
    }

    // N8: names + identifiers.
    let (mut ret, mut non_ret, mut runner) = (0, 0, 0);
    for (i, s) in segs.iter().enumerate() {
        if g.map.parent(i).is_some() {
            assert_eq!(s.ident, None, "{s:?}");
            let declares = matches!(s.kind, K::StructField | K::EnumVariant);
            assert_eq!(s.name.is_some(), declares, "{s:?}");
            continue;
        }
        let text = gtext(g, s);
        match s.kind {
            K::Struct | K::Enum | K::Function => {
                assert_eq!(s.ident, s.name);
                let n = s.name.as_deref().unwrap();
                let lead = match s.kind {
                    K::Struct => format!("struct {n} "),
                    K::Enum => format!("enum {n} "),
                    _ => format!("const fn {n}("),
                };
                assert!(text.contains(&lead), "{text}");
            }
            K::StreamVariable => {
                assert_eq!(s.ident, s.name);
                assert!(text.starts_with(&format!("let mut {} = ", s.ident.as_deref().unwrap())));
            }
            K::StreamVariableRunner => {
                assert_eq!(
                    s.ident.as_deref(),
                    Some(format!("stream_variable_runners_{runner}").as_str())
                );
                assert!(
                    text.starts_with(&format!("let stream_variable_runners_{runner} = Box::pin("))
                );
                runner += 1;
            }
            K::ReturningStream => {
                assert_eq!(
                    s.ident.as_deref(),
                    Some(format!("returning_stream_{ret}").as_str())
                );
                ret += 1;
            }
            K::NonReturningStream => {
                assert_eq!(
                    s.ident.as_deref(),
                    Some(format!("non_returning_stream_{non_ret}").as_str())
                );
                non_ret += 1;
            }
            k => panic!("root with kind {k:?}"),
        }
        // segments_for_ident finds the definition (and only same-ident definitions)
        let id = s.ident.as_deref().unwrap();
        let found = g.map.segments_for_ident(id);
        assert!(
            found.iter().any(|f| std::ptr::eq(*f, s)),
            "{id} -> {found:?}"
        );
        assert!(found.iter().all(|f| f.ident.as_deref() == Some(id)));
        assert!(found
            .windows(2)
            .all(|w| w[0].generated.start < w[1].generated.start));
    }
    assert!(g.map.segments_for_ident("").is_empty());
    assert!(g
        .map
        .segments_for_ident("no_such_ident_\u{1F600}")
        .is_empty());

    // N9: the lookups, brute force. Every byte of the generated program for small programs,
    // edges +-1 otherwise.
    let all_bytes = g.rust.len() <= 6_000;
    let probes: Vec<usize> = if all_bytes {
        (0..=g.rust.len()).collect()
    } else {
        pt_up(
            &g.rust,
            segs.iter()
                .flat_map(|s| [s.generated.start, s.generated.end])
                .flat_map(|e| [e.wrapping_sub(1), e, e + 1]),
        )
    };
    for off in probes {
        let naive = segs
            .iter()
            .rev()
            .filter(|s| s.generated.start <= off && off < s.generated.end)
            .min_by_key(|s| s.generated.end - s.generated.start);
        assert_eq!(
            g.map.source_for_generated(off),
            naive,
            "offset {off} {}",
            ctx()
        );
    }
    assert_eq!(g.map.source_for_generated(usize::MAX), None);

    let pts = pt_up(
        src,
        segs.iter()
            .flat_map(|s| [s.source.start, s.source.end])
            .flat_map(|e| [e.wrapping_sub(1), e, e + 1]),
    );
    let pts: Vec<usize> = pts.into_iter().take(40).collect();
    for &a in &pts {
        for &b in &pts {
            let q = ByteRange { start: a, end: b };
            let hit = |s: &Segment| {
                if a > b {
                    false
                } else if a == b {
                    s.source.start <= a && a < s.source.end
                } else {
                    s.source.start < b && a < s.source.end
                }
            };
            let all: Vec<&Segment> = segs.iter().filter(|s| hit(s)).collect();
            assert_eq!(g.map.generated_for_source(q), all, "query {q:?} {}", ctx());
            // innermost = overlapping segments that have no overlapping descendant (generated containment)
            let inner: Vec<&Segment> = all
                .iter()
                .copied()
                .filter(|s| {
                    !all.iter().any(|t| {
                        !std::ptr::eq(*t, *s)
                            && s.generated.start <= t.generated.start
                            && t.generated.end <= s.generated.end
                    })
                })
                .collect();
            assert_eq!(
                g.map.generated_for_source_innermost(q),
                inner,
                "innermost {q:?} {}",
                ctx()
            );
        }
    }
    for s in segs {
        // a nested segment is found by its own source: it is among the innermost hits unless a
        // descendant is. For leaves that is exactly "contained in the innermost hits".
        let hits = g.map.generated_for_source_innermost(s.source);
        assert!(!hits.is_empty());
        assert!(hits
            .iter()
            .all(|h| h.source.start < s.source.end && s.source.start < h.source.end));
    }

    // N10: deterministic.
    let again = generate(src);
    assert_eq!(&again, g);
    assert_eq!(
        serde_json::to_string(&again).unwrap(),
        serde_json::to_string(g).unwrap()
    );
}

// ---------------------------------------------------------------------------------------------
// golden trees
// ---------------------------------------------------------------------------------------------

enum Gen {
    Exact(String),
    Starts(String),
    Skip,
}

struct Row {
    depth: usize,
    kind: K,
    name: Option<String>,
    ident: Option<String>,
    source: String,
    gen: Gen,
}

fn gx(s: impl Into<String>) -> Gen {
    Gen::Exact(s.into())
}

/// A statement-level (depth 0) row.
fn top(kind: K, name: Option<&str>, ident: &str, source: &str, gen: Gen) -> Row {
    Row {
        depth: 0,
        kind,
        name: name.map(str::to_string),
        ident: Some(ident.to_string()),
        source: source.to_string(),
        gen,
    }
}

/// A nested row without name.
fn sub(depth: usize, kind: K, source: &str, gen: Gen) -> Row {
    Row {
        depth,
        kind,
        name: None,
        ident: None,
        source: source.to_string(),
        gen,
    }
}

/// A nested row that declares something (struct field, enum variant).
fn decl(depth: usize, kind: K, name: &str, source: &str, gen: Gen) -> Row {
    Row {
        name: Some(name.to_string()),
        ..sub(depth, kind, source, gen)
    }
}

/// `(depth, kind, name, ident, source text)`
type Shape = (usize, K, Option<String>, Option<String>, String);

fn assert_tree(src: &str, expected: Vec<Row>) {
    let g = generate(src);
    let segs = g.map.segments();
    let got: Vec<Shape> = segs
        .iter()
        .enumerate()
        .map(|(i, s)| {
            (
                depth(&g, i),
                s.kind,
                s.name.clone(),
                s.ident.clone(),
                stext(src, s).to_string(),
            )
        })
        .collect();
    let want: Vec<Shape> = expected
        .iter()
        .map(|r| {
            (
                r.depth,
                r.kind,
                r.name.clone(),
                r.ident.clone(),
                r.source.clone(),
            )
        })
        .collect();
    assert_eq!(got, want, "program: {src:?}");
    for (s, r) in segs.iter().zip(&expected) {
        let text = gtext(&g, s);
        match &r.gen {
            Gen::Exact(e) => assert_eq!(text, e, "kind {:?} program {src:?}", s.kind),
            Gen::Starts(p) => assert!(text.starts_with(p.as_str()), "{text}\n   !starts_with\n{p}"),
            Gen::Skip => {}
        }
    }
    assert_nested(src, &g);
}

const ITER: &str = "::marigold::marigold_impl::futures::stream::iter";

fn map_g(f: &str) -> String {
    expected_fn_text(K::StreamMap, &format!("map({f})"))
}
fn filter_g(f: &str) -> String {
    expected_fn_text(K::StreamFilter, &format!("filter({f})"))
}
fn filter_map_g(f: &str) -> String {
    expected_fn_text(K::StreamFilterMap, &format!("filter_map({f})"))
}

#[test]
fn t2_g01_unnamed_returning_has_input_child_and_no_output_child() {
    // `.return` generates no text: the only child is the input, there is no Output segment.
    assert_tree(
        "range(0, 5).return",
        vec![
            top(
                K::ReturningStream,
                None,
                "returning_stream_0",
                "range(0, 5).return",
                gx(format!(
                    "let returning_stream_0 = Box::pin({{use ::marigold::marigold_impl::*; {ITER}(0..5)}});"
                )),
            ),
            sub(1, K::InputRange, "range(0, 5)", gx(format!("{ITER}(0..5)"))),
        ],
    );
}

#[test]
fn t2_g02_every_non_numeric_stream_function_in_chain_order() {
    let src = "range(0, =5).map(f).filter(g).filter_map(h).ok().ok_or_panic().return";
    let chain = format!(
        "{ITER}(0..=5).{}.{}.{}.filter(|r| futures::future::ready(r.is_ok())).map(|r| r.unwrap()).map(|r| r.unwrap())",
        map_g("f"),
        filter_g("g"),
        filter_map_g("h")
    );
    assert_tree(
        src,
        vec![
            top(
                K::ReturningStream,
                None,
                "returning_stream_0",
                src,
                gx(format!(
                    "let returning_stream_0 = Box::pin({{use ::marigold::marigold_impl::*; {chain}}});"
                )),
            ),
            sub(1, K::InputRange, "range(0, =5)", gx(format!("{ITER}(0..=5)"))),
            sub(1, K::StreamMap, "map(f)", gx(map_g("f"))),
            sub(1, K::StreamFilter, "filter(g)", gx(filter_g("g"))),
            sub(1, K::StreamFilterMap, "filter_map(h)", gx(filter_map_g("h"))),
            sub(
                1,
                K::StreamOk,
                "ok()",
                gx("filter(|r| futures::future::ready(r.is_ok())).map(|r| r.unwrap())"),
            ),
            sub(1, K::StreamOkOrPanic, "ok_or_panic()", gx("map(|r| r.unwrap())")),
        ],
    );
}

/// Pins the literal text for the default feature set (the helpers above restate the formulas; this
/// one test also pins them as plain literals so that a typo in the helper cannot hide a typo in the
/// code generator).
#[cfg(not(any(feature = "tokio", feature = "async-std")))]
#[test]
fn t2_g02b_literal_text_without_runtime_features() {
    let g = generate("range(0, 5).map(f).filter(g).filter_map(h).return");
    let texts: Vec<&str> = g
        .map
        .segments()
        .iter()
        .skip(1)
        .map(|s| gtext(&g, s))
        .collect();
    assert_eq!(
        texts,
        [
            "::marigold::marigold_impl::futures::stream::iter(0..5)",
            "map(f)",
            "filter(|v| ::marigold::marigold_impl::futures::future::ready(g(v.clone())))",
            "filter_map(|v| ::marigold::marigold_impl::futures::future::ready(h(v)))",
        ]
    );
}

#[cfg(any(feature = "tokio", feature = "async-std"))]
#[test]
fn t2_g02b_literal_text_with_a_runtime_feature() {
    let g = generate("range(0, 5).map(f).filter(g).filter_map(h).return");
    let texts: Vec<&str> = g
        .map
        .segments()
        .iter()
        .skip(1)
        .map(|s| gtext(&g, s))
        .collect();
    assert_eq!(texts[1], "map(|v| async move {f(v)}).buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2))");
    assert_eq!(texts[2], "map(|v| async move { if g(v) { Some(v) } else { None } }).buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2)).filter_map(|v| v)");
    assert_eq!(texts[3], "filter_map(|v| async move { h(v) })");
}

#[test]
fn t2_g03_numeric_argument_stream_functions() {
    let src = "range(0, 9).permutations(2).permutations_with_replacement(3).combinations(4).keep_first_n(5, cmp).fold(0, add).fold(zero, add).return";
    assert_tree(
        src,
        vec![
            top(K::ReturningStream, None, "returning_stream_0", src, Gen::Skip),
            sub(1, K::InputRange, "range(0, 9)", gx(format!("{ITER}(0..9)"))),
            sub(
                1,
                K::StreamPermutations,
                "permutations(2)",
                gx("permutations(2).await.map(|v| <[_; 2]>::try_from(v).unwrap())"),
            ),
            sub(
                1,
                K::StreamPermutationsWithReplacement,
                "permutations_with_replacement(3)",
                gx("permutations_with_replacement(3).await.map(|v| <[_; 3]>::try_from(v).unwrap())"),
            ),
            sub(
                1,
                K::StreamCombinations,
                "combinations(4)",
                gx("combinations(4).await.map(|v| <[_; 4]>::try_from(v).unwrap())"),
            ),
            sub(1, K::StreamKeepFirstN, "keep_first_n(5, cmp)", gx("keep_first_n(5, cmp).await")),
            // a numeric initial value is used as is ...
            sub(
                1,
                K::StreamFold,
                "fold(0, add)",
                gx("marifold(0, |acc, x| futures::future::ready(add(acc, x))).await"),
            ),
            // ... anything else is called as a constructor.
            sub(
                1,
                K::StreamFold,
                "fold(zero, add)",
                gx("marifold(zero(), |acc, x| futures::future::ready(add(acc, x))).await"),
            ),
        ],
    );
}

#[test]
fn t2_g04_write_file_is_two_regions_around_the_stream() {
    let src = "range(0, 3).map(f).write_file(\"a/b.csv\", csv)";
    let g = generate(src);
    let segs = g.map.segments();
    assert_eq!(
        segs.iter().map(|s| s.kind).collect::<Vec<_>>(),
        [
            K::NonReturningStream,
            K::OutputWriteFile,
            K::InputRange,
            K::StreamMap,
            K::OutputWriteFile
        ]
    );
    let (stmt, setup, input, map, sink) = (&segs[0], &segs[1], &segs[2], &segs[3], &segs[4]);
    assert_eq!(stext(src, setup), "write_file(\"a/b.csv\", csv)");
    assert_eq!(setup.source, sink.source);
    // generated order is setup, input, fn, sink; source order is input, fn, call.
    assert!(setup.source.start > map.source.end && input.source.end < map.source.start);
    assert_eq!(
        setup.generated.end, input.generated.start,
        "setup runs right into the stream"
    );
    assert_eq!(map.generated.end, sink.generated.start);
    let (st, sk) = (gtext(&g, setup), gtext(&g, sink));
    assert!(st.starts_with(
        "{\n            if let Some(parent) = ::std::path::Path::new(\"a/b.csv\").parent() {"
    ));
    assert!(st.contains("File::create(\"a/b.csv\")"));
    assert!(st.ends_with("let mut stream_to_write =\n\n           "));
    assert!(!st.contains("GzipEncoder"));
    assert!(sk.starts_with("\n              ;\n              stream_to_write.filter_map("));
    assert!(sk.ends_with("\n          }"));
    assert_eq!(
        &g.rust[stmt.generated.start..setup.generated.start],
        "let non_returning_stream_0 = Box::pin({use ::marigold::marigold_impl::*; "
    );
    assert_eq!(&g.rust[sink.generated.end..stmt.generated.end], "});");
    // glue inside the statement belongs to the statement
    assert_eq!(
        g.map.source_for_generated(input.generated.end),
        Some(stmt),
        "the `.` is glue of the statement"
    );
    assert_eq!(
        g.map.source_for_generated(map.generated.end),
        Some(sink),
        "the sink starts right after the chain"
    );
    assert_nested(src, &g);
}

#[test]
fn t2_g05_write_file_compression_variants() {
    for (call, gz) in [
        ("write_file(\"o.csv.gz\", csv)", true),
        ("write_file(\"o.csv\", csv, compression=gz)", true),
        ("write_file(\"o.csv.gz\", csv, compression=none)", false),
        ("write_file(\"o.csv\", csv, compression=none)", false),
    ] {
        let src = format!("range(0, 1).{call}");
        let g = generate(&src);
        let outs: Vec<&Segment> = g
            .map
            .segments()
            .iter()
            .filter(|s| s.kind == K::OutputWriteFile)
            .collect();
        assert_eq!(outs.len(), 2, "{src}");
        assert_eq!(stext(&src, outs[0]), call);
        assert_eq!(gtext(&g, outs[0]).contains("GzipEncoder"), gz, "{src}");
        assert_eq!(gtext(&g, outs[1]).contains("GzipEncoder"), gz, "{src}");
        assert_nested(&src, &g);
    }
}

#[test]
fn t2_g06_named_stream_reads_the_variable() {
    let src = "x = range(0, 5)\nx.map(f).return";
    assert_tree(
        src,
        vec![
            top(
                K::StreamVariable,
                Some("x"),
                "x",
                "x = range(0, 5)",
                Gen::Skip,
            ),
            sub(1, K::InputRange, "range(0, 5)", gx(format!("{ITER}(0..5)"))),
            top(
                K::ReturningStream,
                None,
                "returning_stream_0",
                "x.map(f).return",
                Gen::Skip,
            ),
            sub(1, K::InputVariable, "x", gx("x.get()")),
            sub(1, K::StreamMap, "map(f)", gx(map_g("f"))),
            top(
                K::StreamVariableRunner,
                Some("x"),
                "stream_variable_runners_0",
                "x = range(0, 5)",
                Gen::Skip,
            ),
        ],
    );
}

#[test]
fn t2_g07_variable_from_prior_variable() {
    let src = "x = range(0, 5)\ny = x . filter(g) .map(f)\ny.return";
    assert_tree(
        src,
        vec![
            top(
                K::StreamVariable,
                Some("x"),
                "x",
                "x = range(0, 5)",
                Gen::Skip,
            ),
            sub(1, K::InputRange, "range(0, 5)", Gen::Skip),
            top(
                K::StreamVariable,
                Some("y"),
                "y",
                "y = x . filter(g) .map(f)",
                Gen::Skip,
            ),
            sub(1, K::InputVariable, "x", gx("x.get()")),
            sub(1, K::StreamFilter, "filter(g)", gx(filter_g("g"))),
            sub(1, K::StreamMap, "map(f)", gx(map_g("f"))),
            top(
                K::ReturningStream,
                None,
                "returning_stream_0",
                "y.return",
                Gen::Skip,
            ),
            sub(1, K::InputVariable, "y", gx("y.get()")),
            top(
                K::StreamVariableRunner,
                Some("x"),
                "stream_variable_runners_0",
                "x = range(0, 5)",
                Gen::Skip,
            ),
            top(
                K::StreamVariableRunner,
                Some("y"),
                "stream_variable_runners_1",
                "y = x . filter(g) .map(f)",
                Gen::Skip,
            ),
        ],
    );
    // the declared name `x` / `y` at the start of a declaration is NOT an input.
    let g = generate(src);
    assert_eq!(
        g.map
            .segments()
            .iter()
            .filter(|s| s.kind == K::InputVariable)
            .count(),
        2
    );
}

/// Found by the one-character mutation property test: the grammar allows blanks around the `=`
/// of `struct=` and `infer_compression=`, and the restated formula of the oracle must too.
#[test]
fn t2_g08b_read_file_arguments_may_have_blanks_around_equals_signs() {
    for src in [
        "read_file(\"a.csv.gz\", csv, struct =R, infer_compression = true).ok().return",
        "read_file(\"a.csv.gz\", csv, struct= R, infer_compression =false).return",
        "read_file(\"a.csv\",csv,struct\r\n=\tR)\n.return",
    ] {
        let g = generate(src);
        assert_nested(src, &g);
        let input = g
            .map
            .segments()
            .iter()
            .find(|s| s.kind == K::InputReadFile)
            .unwrap();
        assert!(gtext(&g, input).contains("::<R>"), "{src}");
    }
}

/// Found by the one-character mutation property test: a quoted path may contain a comma or a
/// parenthesis, which the oracle's argument splitter must not split at.
#[test]
fn t2_g08c_paths_may_contain_commas_and_parentheses() {
    for src in [
        "range(0, 3).write_file(\"o,1.csv\", csv)",
        "range(0, 3).write_file(\"o(1).csv.gz\", csv, compression=none)",
        "read_file(\"a,b.csv\", csv, struct=R).ok().return",
        "read_file(\"a),(b.csv.gz\", csv, struct=R, infer_compression=true).return",
    ] {
        let g = generate(src);
        assert_nested(src, &g);
    }
}

#[test]
fn t2_g08_read_file_variants() {
    let src = "struct R { a: i32 }\nread_file(\"d.csv\", csv, struct=R).ok().return\nread_file(\"d.csv.gz\", csv, struct=R, infer_compression=false).return\nread_file(\"e.gz\", csv, struct=R, infer_compression=true).return\nread_file(\"e.gz\", csv, struct=R).return";
    let g = generate(src);
    let inputs: Vec<&Segment> = g
        .map
        .segments()
        .iter()
        .filter(|s| s.kind == K::InputReadFile)
        .collect();
    assert_eq!(inputs.len(), 4);
    let slices: Vec<&str> = inputs.iter().map(|s| stext(src, s)).collect();
    assert_eq!(
        slices,
        [
            "read_file(\"d.csv\", csv, struct=R)",
            "read_file(\"d.csv.gz\", csv, struct=R, infer_compression=false)",
            "read_file(\"e.gz\", csv, struct=R, infer_compression=true)",
            "read_file(\"e.gz\", csv, struct=R)",
        ]
    );
    let gz: Vec<bool> = inputs
        .iter()
        .map(|s| gtext(&g, s).contains("GzipDecoder"))
        .collect();
    assert_eq!(gz, [false, false, true, true]);
    // the generated region keeps the code's own leading newline: regions are the node's bytes, untrimmed.
    assert!(gtext(&g, inputs[0]).starts_with("\n"));
    assert_nested(src, &g);
}

#[test]
fn t2_g09_select_all_branches_nest_their_own_chains() {
    let src = "select_all(range(0, 3), range(5, 8).map(f)).filter_map(g).return";
    let stmt_gen = Gen::Skip;
    let g = generate(src);
    let seg = |i: usize| &g.map.segments()[i];
    assert_tree(
        src,
        vec![
            top(
                K::ReturningStream,
                None,
                "returning_stream_0",
                src,
                stmt_gen,
            ),
            sub(
                1,
                K::InputSelectAll,
                "select_all(range(0, 3), range(5, 8).map(f))",
                Gen::Starts(SELECT_HEAD.into()),
            ),
            sub(
                2,
                K::SelectAllBranch,
                "range(0, 3)",
                gx(format!("{RUN_STREAM}{ITER}(0..3))")),
            ),
            sub(3, K::InputRange, "range(0, 3)", gx(format!("{ITER}(0..3)"))),
            sub(
                2,
                K::SelectAllBranch,
                "range(5, 8).map(f)",
                gx(format!("{RUN_STREAM}{ITER}(5..8).{})", map_g("f"))),
            ),
            sub(3, K::InputRange, "range(5, 8)", gx(format!("{ITER}(5..8)"))),
            sub(3, K::StreamMap, "map(f)", gx(map_g("f"))),
            sub(
                1,
                K::StreamFilterMap,
                "filter_map(g)",
                gx(filter_map_g("g")),
            ),
        ],
    );
    // branches are separated by exactly ",\n" and the whole call ends with "])"
    let (b0, b1) = (seg(2), seg(4));
    assert_eq!(&g.rust[b0.generated.end..b1.generated.start], ",\n");
    assert_eq!(&g.rust[b1.generated.end..seg(1).generated.end], "])");
}

#[test]
fn t2_g10_nested_select_all_with_whitespace_inside() {
    // pest includes the whitespace before `,` / `)` in the branch pair; our range must not.
    let src = "select_all( select_all(range(0,1) , range(1,2)) .map(f) , read_file(\"a.csv\",csv,struct=R) ) .return";
    let g = generate(src);
    let branches: Vec<&str> = g
        .map
        .segments()
        .iter()
        .filter(|s| s.kind == K::SelectAllBranch)
        .map(|s| stext(src, s))
        .collect();
    assert_eq!(
        branches,
        [
            "select_all(range(0,1) , range(1,2)) .map(f)",
            "range(0,1)",
            "range(1,2)",
            "read_file(\"a.csv\",csv,struct=R)",
        ]
    );
    let outer = &g.map.segments()[1];
    assert_eq!(outer.kind, K::InputSelectAll);
    assert_eq!(
        stext(src, outer),
        "select_all( select_all(range(0,1) , range(1,2)) .map(f) , read_file(\"a.csv\",csv,struct=R) )"
    );
    // depth: stmt(0) > select_all(1) > branch(2) > select_all(3) > branch(4) > range(5)
    let max_depth = (0..g.map.segments().len())
        .map(|i| depth(&g, i))
        .max()
        .unwrap();
    assert_eq!(max_depth, 5);
    assert_nested(src, &g);
}

#[test]
fn t2_g11_range_over_an_enum() {
    let src = "enum E { A }\nrange(E).return";
    let g = generate(src);
    let input = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::InputRange)
        .unwrap();
    assert_eq!(stext(src, input), "range(E)");
    assert_eq!(
        gtext(&g, input),
        format!("{ITER}(E::__marigold_variants())")
    );
    assert_nested(src, &g);
}

#[test]
fn t2_g12_function_signature_and_verbatim_body() {
    let src = "fn double(x: i32, y:&str) -> string_10 { x * 2 }";
    assert_tree(
        src,
        vec![
            top(K::Function, Some("double"), "double", src, gx(
                "const fn double(x: i32, y: &str) -> ::marigold::marigold_impl::arrayvec::ArrayString<10> { x * 2 }"
            )),
            sub(
                1,
                K::FunctionSignature,
                "double(x: i32, y:&str) -> string_10",
                gx("double(x: i32, y: &str) -> ::marigold::marigold_impl::arrayvec::ArrayString<10>"),
            ),
            sub(1, K::FunctionBody, "{ x * 2 }", gx("{ x * 2 }")),
        ],
    );
    // no parameters
    assert_tree(
        "fn z() -> i32 { 1 }",
        vec![
            top(
                K::Function,
                Some("z"),
                "z",
                "fn z() -> i32 { 1 }",
                gx("const fn z() -> i32 { 1 }"),
            ),
            sub(1, K::FunctionSignature, "z() -> i32", gx("z() -> i32")),
            sub(1, K::FunctionBody, "{ 1 }", gx("{ 1 }")),
        ],
    );
}

#[test]
fn t2_g13_function_body_is_byte_identical_for_hostile_bodies() {
    let src = "fn pick(x: &str) -> bool {\r\n    /* \u{e9} } */ let s = \"}\u{65e5}{\"; // c }\r\n    if x == s { true } else { false }\r\n}\nrange(0, 1).return";
    let g = generate(src);
    let body = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::FunctionBody)
        .unwrap();
    assert_eq!(gtext(&g, body), stext(src, body));
    assert!(stext(src, body).starts_with('{') && stext(src, body).ends_with('}'));
    assert!(stext(src, body).contains('\u{65e5}'));
    assert!(body.generated.end - body.generated.start == body.source.end - body.source.start);
    assert_nested(src, &g);
}

#[test]
fn t2_g14_function_return_type_may_end_in_a_unicode_space() {
    // pest's WHITESPACE is ASCII only, so a trailing NBSP belongs to the return type. A `trim_end()`
    // based implementation would drop it from the signature range.
    let src = "fn f(x: i32) -> i32\u{a0} { x }";
    assert!(marigold_parse(src).is_ok());
    let g = generate(src);
    let sig = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::FunctionSignature)
        .unwrap();
    assert_eq!(stext(src, sig), "f(x: i32) -> i32\u{a0}");
    assert_nested(src, &g);
}

#[test]
fn t2_g15_struct_fields() {
    let src = "struct S { a: i32, b : string_10 , c: Option<u8>, d: Vec<u8> , e: int[0, 255], }";
    let a = "::marigold::marigold_impl::arrayvec::ArrayString<10>";
    assert_tree(
        src,
        vec![
            top(K::Struct, Some("S"), "S", src, Gen::Skip),
            decl(1, K::StructField, "a", "a: i32", gx("a: i32")),
            decl(
                1,
                K::StructField,
                "b",
                "b : string_10",
                gx(format!("b: {a}")),
            ),
            decl(1, K::StructField, "c", "c: Option<u8>", gx("c: Option<u8>")),
            decl(1, K::StructField, "d", "d: Vec<u8>", gx("d: Vec<u8>")),
            // unresolved bound -> i128 on the plain (no resolved bounds) path ...
            decl(
                1,
                K::StructField,
                "e",
                "e: int[0, 255]",
                Gen::Starts("e: ".into()),
            ),
        ],
    );
    // ... a resolved bound picks the smallest type; the generated impl block is NOT a sub-segment.
    let g = generate(src);
    let st = &g.map.segments()[0];
    let e = g
        .map
        .segments()
        .iter()
        .find(|s| s.name.as_deref() == Some("e"))
        .unwrap();
    assert_eq!(gtext(&g, e), "e: u8");
    let at = g.rust.find("E_MAX").unwrap();
    assert!(st.generated.start < at && at < st.generated.end);
    assert_eq!(
        g.map.source_for_generated(at),
        Some(st),
        "bounds constants belong to the struct itself"
    );
}

#[test]
fn t2_g16_struct_without_fields_has_no_children() {
    for src in ["struct E0 { }", "struct E1 {}"] {
        let g = generate(src);
        assert_eq!(g.map.segments().len(), 1, "{src}");
        assert_nested(src, &g);
    }
}

#[test]
fn t2_g17_enum_variants() {
    let src = "enum E { A, B = \"b\" , default Z }";
    assert_tree(
        src,
        vec![
            top(K::Enum, Some("E"), "E", src, Gen::Skip),
            decl(1, K::EnumVariant, "A", "A", gx("A")),
            decl(1, K::EnumVariant, "B", "B = \"b\"", gx("B")),
            decl(1, K::EnumVariant, "Z", "default Z", gx("Z")),
        ],
    );
    let sized = "enum F { A , default Other(string_10) }";
    assert_tree(
        sized,
        vec![
            top(K::Enum, Some("F"), "F", sized, Gen::Skip),
            decl(1, K::EnumVariant, "A", "A", gx("A")),
            decl(
                1,
                K::EnumVariant,
                "Other",
                "default Other(string_10)",
                gx("Other(::marigold::marigold_impl::arrayvec::ArrayString<10>)"),
            ),
        ],
    );
    let valued = "enum G { default Q = \"q\" }";
    assert_tree(
        valued,
        vec![
            top(K::Enum, Some("G"), "G", valued, Gen::Skip),
            decl(1, K::EnumVariant, "Q", "default Q = \"q\"", gx("Q")),
        ],
    );
    let empty = "enum H { }";
    assert_tree(empty, vec![top(K::Enum, Some("H"), "H", empty, Gen::Skip)]);
}

#[test]
fn t2_g18_enum_values_are_multibyte_but_variant_regions_are_exact() {
    let src = "enum Mood { Happy = \"\u{1F600}\", Sad = \"\u{fc}n\u{ef}\" }";
    let g = generate(src);
    let v: Vec<&str> = g
        .map
        .segments()
        .iter()
        .skip(1)
        .map(|s| stext(src, s))
        .collect();
    assert_eq!(v, ["Happy = \"\u{1F600}\"", "Sad = \"\u{fc}n\u{ef}\""]);
    assert_nested(src, &g);
}

#[test]
fn t2_g19_every_kind_together_keeps_generated_order_and_tree_shape() {
    // same program as T1-g10, now with the nested rows.
    let src = "range(0, 2).return\nfn f(x: i32) -> i32 { x }\nx = range(0, 3).map(f)\nstruct S { a: u8 }\nenum E { A }\nx.write_file(\"o.csv\", csv)\nrange(E).return";
    let g = generate(src);
    let shape: Vec<(usize, K, usize)> = g
        .map
        .segments()
        .iter()
        .enumerate()
        .map(|(i, s)| (depth(&g, i), s.kind, s.expr_index))
        .collect();
    assert_eq!(
        shape,
        vec![
            (0, K::Struct, 3),
            (1, K::StructField, 3),
            (0, K::Enum, 4),
            (1, K::EnumVariant, 4),
            (0, K::Function, 1),
            (1, K::FunctionSignature, 1),
            (1, K::FunctionBody, 1),
            (0, K::StreamVariable, 2),
            (1, K::InputRange, 2),
            (1, K::StreamMap, 2),
            (0, K::ReturningStream, 0),
            (1, K::InputRange, 0),
            (0, K::ReturningStream, 6),
            (1, K::InputRange, 6),
            (0, K::NonReturningStream, 5),
            (1, K::OutputWriteFile, 5),
            (1, K::InputVariable, 5),
            (1, K::OutputWriteFile, 5),
            (0, K::StreamVariableRunner, 2),
        ]
    );
    assert_nested(src, &g);
}

#[test]
fn t2_g20_multibyte_paths_and_names() {
    let src = "struct R { a: i32 }\nread_file(\"d\u{e4}t\u{20ac}.csv\", csv, struct=R).ok().return\nrange(0, 1).write_file(\"\u{65e5}\u{672c}\u{8a9e}/\u{f6}.csv\", csv)";
    let g = generate(src);
    let rf = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::InputReadFile)
        .unwrap();
    assert!(gtext(&g, rf).contains("d\u{e4}t\u{20ac}.csv"));
    let outs: Vec<&Segment> = g
        .map
        .segments()
        .iter()
        .filter(|s| s.kind == K::OutputWriteFile)
        .collect();
    assert!(gtext(&g, outs[0]).contains("\u{65e5}\u{672c}\u{8a9e}/\u{f6}.csv"));
    assert!(gtext(&g, outs[1]).len() > 10);
    // a byte in the middle of a multibyte char of the source maps like its neighbours
    let mid = src.find('\u{65e5}').unwrap() + 1;
    assert!(!src.is_char_boundary(mid));
    let hits = g.map.generated_for_source_innermost(ByteRange {
        start: mid,
        end: mid,
    });
    assert_eq!(
        hits.iter().map(|s| s.kind).collect::<Vec<_>>(),
        [K::OutputWriteFile, K::OutputWriteFile]
    );
    // a byte in the middle of a multibyte char of the generated text maps to the write_file region
    let at = g.rust.find('\u{65e5}').unwrap() + 1;
    assert!(!g.rust.is_char_boundary(at));
    assert_eq!(
        g.map.source_for_generated(at).unwrap().kind,
        K::OutputWriteFile
    );
    assert_nested(src, &g);
}

#[test]
fn t2_g21_crlf_tabs_and_blank_lines_inside_chains() {
    let src =
        "x = range(0, 5)\r\n\t.map(f)  \r\n\t.filter(g)  \r\n\r\ny\r\n .map(h) \r\n . return  \r\n";
    let g = generate(src);
    let rows: Vec<(K, &str)> = g
        .map
        .segments()
        .iter()
        .map(|s| (s.kind, stext(src, s)))
        .collect();
    assert_eq!(
        rows,
        vec![
            (
                K::StreamVariable,
                "x = range(0, 5)\r\n\t.map(f)  \r\n\t.filter(g)"
            ),
            (K::InputRange, "range(0, 5)"),
            (K::StreamMap, "map(f)"),
            (K::StreamFilter, "filter(g)"),
            (K::ReturningStream, "y\r\n .map(h) \r\n . return"),
            (K::InputVariable, "y"),
            (K::StreamMap, "map(h)"),
            (
                K::StreamVariableRunner,
                "x = range(0, 5)\r\n\t.map(f)  \r\n\t.filter(g)"
            ),
        ]
    );
    assert_nested(src, &g);
}

#[test]
fn t2_g22_return_has_no_region_and_resolves_to_the_statement() {
    let src = "x = range(0, 5)\nx.map(f).return";
    let g = generate(src);
    let ret = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::ReturningStream)
        .unwrap();
    let at = src.rfind("return").unwrap();
    let q = ByteRange {
        start: at,
        end: at + "return".len(),
    };
    // all overlaps: only the statement; innermost: the statement (the nearest enclosing region).
    assert_eq!(g.map.generated_for_source(q), vec![ret]);
    assert_eq!(g.map.generated_for_source_innermost(q), vec![ret]);
    let p = ByteRange {
        start: at + 2,
        end: at + 2,
    };
    assert_eq!(g.map.generated_for_source_innermost(p), vec![ret]);
    // and likewise for the `.` that joins the chain
    let dot = src.rfind(".return").unwrap();
    assert_eq!(
        g.map.generated_for_source_innermost(ByteRange {
            start: dot,
            end: dot + 1
        }),
        vec![ret]
    );
    // no segment anywhere is the `return` keyword
    assert!(g.map.segments().iter().all(|s| stext(src, s) != "return"));
    assert_nested(src, &g);
}

#[test]
fn t2_g23_no_distinct_region_constructs_resolve_to_the_nearest_enclosing_segment() {
    // (a) bounded-struct constants, (b) enum helper impl and serde glue, (c) a stream variable's
    // runner internals, (d) the derive attribute: all of them sit inside the statement segment and
    // have no segment of their own.
    let src = "struct P { v: int[0, 255] }\nenum E { A }\nx = range(E)\nx.return";
    let g = generate(src);
    let seg_at = |needle: &str| {
        let at = g.rust.find(needle).unwrap_or_else(|| panic!("{needle}"));
        g.map.source_for_generated(at).unwrap()
    };
    assert_eq!(seg_at("V_CARDINALITY").kind, K::Struct);
    assert_eq!(seg_at("#[derive(").kind, K::Struct);
    assert_eq!(seg_at("__marigold_variants() -> [E; 1]").kind, K::Enum);
    assert_eq!(seg_at("runner_future").kind, K::StreamVariableRunner);
    // glue that belongs to no Marigold construct has no segment at all
    let glue = g.rust.find("typed_stream_vec<").unwrap();
    assert_eq!(g.map.source_for_generated(glue), None);
    assert_nested(src, &g);
}

// ---------------------------------------------------------------------------------------------
// every grammar alternative has a golden program
// ---------------------------------------------------------------------------------------------

/// `(rule, a program whose generation must contain a segment of that rule's kind whose source
/// starts with the given text)`.
const COVERAGE: &[(&str, &str, &str)] = &[
    ("range_input", "range(0, 3).return", "range("),
    (
        "read_file_csv_input",
        "struct R { a: i32 }\nread_file(\"a.csv\", csv, struct=R).return",
        "read_file(",
    ),
    (
        "select_all_input",
        "select_all(range(0, 3)).return",
        "select_all(",
    ),
    (
        "permutations_fn",
        "range(0, 3).permutations(2).return",
        "permutations(2)",
    ),
    (
        "permutations_with_replacement_fn",
        "range(0, 3).permutations_with_replacement(2).return",
        "permutations_with_replacement(",
    ),
    (
        "combinations_fn",
        "range(0, 3).combinations(2).return",
        "combinations(",
    ),
    (
        "keep_first_n_fn",
        "range(0, 3).keep_first_n(2, cmp).return",
        "keep_first_n(",
    ),
    ("filter_fn", "range(0, 3).filter(g).return", "filter(g)"),
    (
        "filter_map_fn",
        "range(0, 3).filter_map(g).return",
        "filter_map(",
    ),
    ("map_fn", "range(0, 3).map(f).return", "map("),
    ("fold_fn", "range(0, 3).fold(0, add).return", "fold("),
    (
        "ok_fn",
        "struct R { a: i32 }\nread_file(\"a.csv\", csv, struct=R).ok().return",
        "ok()",
    ),
    (
        "ok_or_panic_fn",
        "struct R { a: i32 }\nread_file(\"a.csv\", csv, struct=R).ok_or_panic().return",
        "ok_or_panic()",
    ),
    (
        "write_file_fn",
        "range(0, 3).write_file(\"a.csv\", csv)",
        "write_file(",
    ),
    ("return_fn", "range(0, 3).return", "return"),
];

/// Names of the alternatives of the grammar rule `rule_name`, read from `marigold.pest`.
fn grammar_alternatives(rule_name: &str) -> Vec<String> {
    let grammar = include_str!("../src/marigold.pest");
    let start = grammar
        .find(&format!("\n{rule_name} = {{"))
        .unwrap_or_else(|| panic!("rule {rule_name} not found in marigold.pest"));
    let body = &grammar[start..];
    let body = &body[body.find('{').unwrap() + 1..];
    let body = &body[..body.find("\n}").unwrap()];
    body.split('|')
        .map(|alt| alt.trim().to_string())
        .filter(|alt| !alt.is_empty())
        .collect()
}

#[test]
fn grammar_alternatives_are_all_covered() {
    let mut expected: Vec<String> = Vec::new();
    for rule in ["input_function", "stream_function", "output_function"] {
        let alts = grammar_alternatives(rule);
        assert!(!alts.is_empty(), "{rule}");
        expected.extend(alts);
    }
    let mut table: Vec<String> = RULE_KINDS.iter().map(|(n, _)| n.to_string()).collect();
    expected.sort();
    table.sort();
    assert_eq!(
        expected, table,
        "marigold.pest has a stream input/function/output the segment tables do not know about: \
         add a SegmentKind, a RULE_KINDS entry and a COVERAGE program"
    );
    let mut covered: Vec<&str> = COVERAGE.iter().map(|(r, _, _)| *r).collect();
    covered.sort_unstable();
    let mut want: Vec<&str> = expected.iter().map(String::as_str).collect();
    want.sort_unstable();
    assert_eq!(covered, want);

    // the statement-level alternatives of `expr` are the T1 kinds
    let mut exprs = grammar_alternatives("expr");
    exprs.sort();
    assert_eq!(
        exprs,
        [
            "enum_decl",
            "fn_decl",
            "stream",
            "stream_variable_declaration",
            "struct_decl"
        ]
    );
}

#[test]
fn every_grammar_alternative_produces_its_segment_kind() {
    for (rule, program, starts) in COVERAGE {
        let kind = RULE_KINDS.iter().find(|(n, _)| n == rule).unwrap().1;
        let g = generate(program);
        assert_nested(program, &g);
        match kind {
            Some(kind) => {
                let found = g
                    .map
                    .segments()
                    .iter()
                    .filter(|s| s.kind == kind && stext(program, s).starts_with(starts))
                    .count();
                let expected = if kind == K::OutputWriteFile { 2 } else { 1 };
                assert_eq!(found, expected, "{rule}: {program:?}");
            }
            None => {
                // `return`: no region of its own, and nothing in the map has `return` as source.
                assert!(
                    g.map
                        .segments()
                        .iter()
                        .all(|s| stext(program, s) != *starts),
                    "{rule}"
                );
            }
        }
    }
}

// ---------------------------------------------------------------------------------------------
// lookups on nested real maps
// ---------------------------------------------------------------------------------------------

#[test]
fn source_for_generated_returns_the_innermost_region_and_the_statement_in_glue() {
    let src = "range(0, 5).map(f).filter(g).return";
    let g = generate(src);
    let [stmt, input, map, filter] = g.map.segments() else {
        panic!("{:?}", g.map)
    };
    let at = |o: usize| g.map.source_for_generated(o);
    assert_eq!(at(input.generated.start), Some(input));
    assert_eq!(at(input.generated.end - 1), Some(input));
    assert_eq!(
        at(input.generated.end),
        Some(stmt),
        "the dot after the input"
    );
    assert_eq!(at(map.generated.start), Some(map));
    assert_eq!(at(map.generated.end), Some(stmt), "the dot after map");
    assert_eq!(at(filter.generated.start), Some(filter));
    assert_eq!(at(filter.generated.end - 1), Some(filter));
    assert_eq!(at(filter.generated.end), Some(stmt), "the closing `}});`");
    assert_eq!(
        at(stmt.generated.start),
        Some(stmt),
        "the `let ... Box::pin(` head"
    );
    assert_eq!(at(stmt.generated.end - 1), Some(stmt));
    assert_eq!(
        at(stmt.generated.end),
        None,
        "separator after the statement"
    );
    // a region inside a region inside a region: every byte answers with the deepest region.
    let nested = "select_all(range(0, 1), select_all(range(2, 3).map(f)))\n.return";
    let g = generate(nested);
    let deepest = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::StreamMap)
        .unwrap();
    assert_eq!(
        g.map.source_for_generated(deepest.generated.start + 2),
        Some(deepest)
    );
    assert_nested(nested, &g);
}

#[test]
fn generated_for_source_keeps_returning_every_overlap_outer_before_inner() {
    let src = "range(0, 5).map(f).filter(g).return";
    let g = generate(src);
    let [stmt, input, map, filter] = g.map.segments() else {
        panic!()
    };
    let q = |a: usize, b: usize| g.map.generated_for_source(ByteRange { start: a, end: b });
    let at = |needle: &str| src.find(needle).unwrap();
    // a cursor inside `map(f)`: the statement and the region
    assert_eq!(q(at("map") + 1, at("map") + 1), vec![stmt, map]);
    // the dot between two functions and the keyword `return`: only the statement
    assert_eq!(q(at(".map"), at(".map") + 1), vec![stmt]);
    assert_eq!(q(at("return"), at("return") + 6), vec![stmt]);
    // the whole statement: all of it, pre-order (generated order)
    assert_eq!(q(0, src.len()), vec![stmt, input, map, filter]);
    // a range over two functions
    assert_eq!(q(at("map"), at("filter") + 3), vec![stmt, map, filter]);
}

#[test]
fn generated_for_source_innermost_picks_the_most_specific_regions() {
    let src = "x = range(0, 5)\n  .map(f)\nx.map(g).return";
    let g = generate(src);
    let segs = g.map.segments();
    let kinds_of = |a: usize, b: usize| -> Vec<(K, usize)> {
        g.map
            .generated_for_source_innermost(ByteRange { start: a, end: b })
            .iter()
            .map(|s| (s.kind, s.expr_index))
            .collect()
    };
    let at = |needle: &str| src.find(needle).unwrap();
    // inside `map(f)` of the declaration: that function, plus the runner of the whole declaration
    // (it implements the whole source range and has no children).
    assert_eq!(
        kinds_of(at("map(f)") + 1, at("map(f)") + 1),
        vec![(K::StreamMap, 0), (K::StreamVariableRunner, 0)]
    );
    // on the declared name `x`: the declaration and its runner (neither has a hit descendant)
    assert_eq!(
        kinds_of(0, 1),
        vec![(K::StreamVariable, 0), (K::StreamVariableRunner, 0)]
    );
    // on `range(`: the input region and the runner
    assert_eq!(
        kinds_of(at("range") + 2, at("range") + 2),
        vec![(K::InputRange, 0), (K::StreamVariableRunner, 0)]
    );
    // on the use `x.map(g)`: variable input, then the function.
    assert_eq!(
        kinds_of(at("x.map(g)"), at("x.map(g)") + 8),
        vec![(K::InputVariable, 1), (K::StreamMap, 1)]
    );
    // on `.return`: the statement
    assert_eq!(
        kinds_of(src.len() - 4, src.len()),
        vec![(K::ReturningStream, 1)]
    );
    // a range covering nothing
    assert!(kinds_of(src.len(), src.len()).is_empty());
    assert!(kinds_of(5, 2).is_empty());
    assert!(kinds_of(usize::MAX - 1, usize::MAX).is_empty());
    // plain overlap query is still the superset
    let all = g.map.generated_for_source(ByteRange {
        start: at("map(f)") + 1,
        end: at("map(f)") + 1,
    });
    assert_eq!(all.len(), 3, "decl, map, runner");
    assert!(segs.iter().any(|s| std::ptr::eq(s, all[0])));
}

#[test]
fn parent_children_and_top_level_describe_the_tree() {
    let src = "x = range(0, 5).map(f)\nx.return";
    let g = generate(src);
    let kinds: Vec<K> = g.map.segments().iter().map(|s| s.kind).collect();
    assert_eq!(
        kinds,
        [
            K::StreamVariable,
            K::InputRange,
            K::StreamMap,
            K::ReturningStream,
            K::InputVariable,
            K::StreamVariableRunner
        ]
    );
    let parents: Vec<Option<usize>> = (0..6).map(|i| g.map.parent(i)).collect();
    assert_eq!(parents, [None, Some(0), Some(0), None, Some(3), None]);
    assert_eq!(g.map.children(0), vec![1, 2]);
    assert_eq!(g.map.children(3), vec![4]);
    assert!(g.map.children(1).is_empty());
    assert!(g.map.children(5).is_empty());
    let top: Vec<K> = g.map.top_level().map(|s| s.kind).collect();
    assert_eq!(
        top,
        [
            K::StreamVariable,
            K::ReturningStream,
            K::StreamVariableRunner
        ]
    );
    // out of range never panics
    assert_eq!(g.map.parent(6), None);
    assert!(g.map.children(6).is_empty());
}

// ---------------------------------------------------------------------------------------------
// identifiers
// ---------------------------------------------------------------------------------------------

#[test]
fn identifiers_follow_the_generated_ordinals_not_the_source_positions() {
    // same program as T1-g15: two variables, three returning, three non-returning streams
    let src = "a = range(0, 2)\nb = range(2, 4)\na.return\nrange(9, 10).write_file(\"p.csv\", csv)\nb.return\nrange(7, 8).return\na.write_file(\"q.csv\", csv)\nrange(1, 2).write_file(\"r.csv\", csv)";
    let g = generate(src);
    let idents: Vec<(K, usize, String)> = g
        .map
        .top_level()
        .map(|s| (s.kind, s.expr_index, s.ident.clone().unwrap()))
        .collect();
    assert_eq!(
        idents,
        vec![
            (K::StreamVariable, 0, "a".into()),
            (K::StreamVariable, 1, "b".into()),
            (K::ReturningStream, 2, "returning_stream_0".into()),
            (K::ReturningStream, 4, "returning_stream_1".into()),
            (K::ReturningStream, 5, "returning_stream_2".into()),
            (K::NonReturningStream, 3, "non_returning_stream_0".into()),
            (K::NonReturningStream, 6, "non_returning_stream_1".into()),
            (K::NonReturningStream, 7, "non_returning_stream_2".into()),
            (
                K::StreamVariableRunner,
                0,
                "stream_variable_runners_0".into()
            ),
            (
                K::StreamVariableRunner,
                1,
                "stream_variable_runners_1".into()
            ),
        ]
    );
    // a trace that names a generated local finds the Marigold construct
    let hit = g.map.segments_for_ident("returning_stream_1");
    assert_eq!(hit.len(), 1);
    assert_eq!(stext(src, hit[0]), "b.return");
    let hit = g.map.segments_for_ident("stream_variable_runners_1");
    assert_eq!(
        (hit[0].kind, stext(src, hit[0])),
        (K::StreamVariableRunner, "b = range(2, 4)")
    );
    // the vec![..] glue names the same identifiers; the lookup finds the definition, not the glue
    assert!(g.rust.contains("\nreturning_stream_1,\n"));
    assert!(g.map.segments_for_ident("returning_stream_3").is_empty());
    // exact match only: neither a prefix nor an extension of a real identifier finds anything
    for not_an_ident in [
        "returning_stream_",
        "returning_stream",
        "returning_stream_01",
        "a_",
        "stream_variable_runners_",
        "",
    ] {
        assert!(
            g.map.segments_for_ident(not_an_ident).is_empty(),
            "{not_an_ident:?}"
        );
    }
    assert_eq!(g.map.segments_for_ident("a").len(), 1);
}

#[test]
fn user_names_that_collide_with_generated_identifiers_are_both_reported() {
    // a stream variable called `returning_stream_0` and the first returning stream share the
    // identifier in the generated code (Rust shadows); the map must not hide either.
    let src = "returning_stream_0 = range(0, 1)\nreturning_stream_0.return";
    let g = generate(src);
    let hits = g.map.segments_for_ident("returning_stream_0");
    assert_eq!(
        hits.iter().map(|s| s.kind).collect::<Vec<_>>(),
        [K::StreamVariable, K::ReturningStream],
        "generated order"
    );
    // a struct and a stream variable may share a name (types and values live in different namespaces)
    let src = "struct S { a: u8 }\nS = range(0, 1)\nS.return";
    let g = generate(src);
    let kinds: Vec<K> = g
        .map
        .segments_for_ident("S")
        .iter()
        .map(|s| s.kind)
        .collect();
    assert_eq!(kinds, [K::Struct, K::StreamVariable]);
    // same name declared twice
    let src = "x = range(0, 1)\nx = range(1, 2)\nx.return";
    let g = generate(src);
    assert_eq!(g.map.segments_for_ident("x").len(), 2);
}

#[test]
fn only_statement_level_segments_carry_an_identifier() {
    let g =
        generate("struct S { a: u8 }\nfn f(x: i32) -> i32 { x }\nx = range(0, 1).map(f)\nx.return");
    for (i, s) in g.map.segments().iter().enumerate() {
        assert_eq!(s.ident.is_some(), g.map.parent(i).is_none(), "{s:?}");
    }
}

// ---------------------------------------------------------------------------------------------
// JSON
// ---------------------------------------------------------------------------------------------

#[test]
fn json_has_a_stable_shape_for_nested_segments() {
    let src = "x = range(0, 5)\nx.return";
    let g = generate(src);
    let v = serde_json::to_value(&g).unwrap();
    let segs = v["map"]["segments"].as_array().unwrap();
    let kinds: Vec<&str> = segs.iter().map(|s| s["kind"].as_str().unwrap()).collect();
    assert_eq!(
        kinds,
        [
            "stream_variable",
            "input_range",
            "returning_stream",
            "input_variable",
            "stream_variable_runner"
        ]
    );
    let input = &g.map.segments()[1];
    assert_eq!(
        segs[1],
        serde_json::json!({
            "generated": {"start": input.generated.start, "end": input.generated.end},
            "source": {"start": 4, "end": 15},
            "kind": "input_range",
            "expr_index": 0,
            "name": null,
            "ident": null,
        })
    );
    assert_eq!(segs[0]["ident"], "x");
    assert_eq!(segs[2]["ident"], "returning_stream_0");
    assert_eq!(segs[4]["ident"], "stream_variable_runners_0");
    // every new kind serialises as lowercase snake_case, and no two kinds collide
    let mut seen = std::collections::HashSet::new();
    for (_, k) in RULE_KINDS {
        if let Some(k) = k {
            let s = serde_json::to_value(k).unwrap();
            let s = s.as_str().unwrap().to_string();
            assert!(s.chars().all(|c| c.is_ascii_lowercase() || c == '_'), "{s}");
            assert!(seen.insert(s));
        }
    }
    for k in [
        K::SelectAllBranch,
        K::FunctionSignature,
        K::FunctionBody,
        K::StructField,
        K::EnumVariant,
        K::InputVariable,
    ] {
        let s = serde_json::to_value(k).unwrap();
        assert!(seen.insert(s.as_str().unwrap().to_string()), "{s}");
    }
    assert_eq!(
        serde_json::to_value(K::StreamPermutationsWithReplacement).unwrap(),
        "stream_permutations_with_replacement"
    );
}

// ---------------------------------------------------------------------------------------------
// corpus, bigger programs
// ---------------------------------------------------------------------------------------------

#[test]
fn every_corpus_program_satisfies_the_nested_invariants() {
    let files = common::corpus();
    assert!(files.len() >= 30, "{files:?}");
    let mut with_children = 0;
    for path in files {
        let src = std::fs::read_to_string(&path).unwrap();
        if marigold_parse(&src).is_ok() {
            let g = generate(&src);
            assert_nested(&src, &g);
            with_children += g.map.segments().len() - g.map.top_level().count();
        }
    }
    assert!(
        with_children >= 80,
        "corpus produced only {with_children} nested segments"
    );
}

#[test]
fn corpus_multi_consumer_program_has_the_expected_tree() {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/programs/multi_consumer.marigold");
    let src = std::fs::read_to_string(path).unwrap();
    let g = generate(&src);
    let stmts: Vec<K> = g.map.top_level().map(|s| s.kind).collect();
    assert_eq!(
        stmts,
        [
            K::Function,
            K::StreamVariable,
            K::StreamVariable,
            K::ReturningStream,
            K::ReturningStream,
            K::StreamVariableRunner,
            K::StreamVariableRunner
        ]
    );
    // each stream variable / stream has an input
    let inputs = g
        .map
        .segments()
        .iter()
        .filter(|s| {
            matches!(
                s.kind,
                K::InputRange | K::InputVariable | K::InputReadFile | K::InputSelectAll
            )
        })
        .count();
    assert_eq!(inputs, 4);
}

#[test]
fn a_long_chain_gets_exactly_one_segment_per_function() {
    let n = 400;
    let mut src = String::from("range(0, 10)");
    for i in 0..n {
        src.push_str(&format!(".map(f{i})"));
    }
    src.push_str(".return");
    let g = generate(&src);
    let maps: Vec<&Segment> = g
        .map
        .segments()
        .iter()
        .filter(|s| s.kind == K::StreamMap)
        .collect();
    assert_eq!(maps.len(), n);
    for (i, m) in maps.iter().enumerate() {
        assert_eq!(stext(&src, m), format!("map(f{i})"));
    }
    assert_eq!(g.map.segments().len(), n + 2);
    assert_nested(&src, &g);
}

#[test]
fn deeply_nested_select_all_does_not_overflow_or_lose_segments() {
    let depth = 40;
    let mut src = String::from("range(0, 1)");
    for _ in 0..depth {
        src = format!("select_all({src}, range(0, 2))");
    }
    src.push_str(".return");
    let g = generate(&src);
    assert_eq!(
        g.map
            .segments()
            .iter()
            .filter(|s| s.kind == K::InputSelectAll)
            .count(),
        depth
    );
    assert_eq!(
        g.map
            .segments()
            .iter()
            .filter(|s| s.kind == K::SelectAllBranch)
            .count(),
        2 * depth
    );
    assert_eq!(
        g.map
            .segments()
            .iter()
            .filter(|s| s.kind == K::InputRange)
            .count(),
        depth + 1
    );
    let max_depth = (0..g.map.segments().len())
        .map(|i| depth_of(&g, i))
        .max()
        .unwrap();
    assert_eq!(max_depth, 1 + 2 * depth);
    assert_nested(&src, &g);
}

fn depth_of(g: &GeneratedProgram, i: usize) -> usize {
    depth(g, i)
}

fn many_chain_exprs(n: usize) -> String {
    let mut src = String::new();
    for i in 0..n {
        src.push_str(&format!(
            "v{i} = range(0, 5).map(f).filter(g)\nv{i}.map(h).return\nstruct S{i} {{ a: u8, b: u8 }}\nfn f{i}(x: i32) -> i32 {{ x }}\n"
        ));
    }
    src
}

#[test]
fn many_expressions_get_exactly_the_expected_nested_segments() {
    let n = 800;
    let src = many_chain_exprs(n);
    let g = generate(&src);
    // per i: decl(+input,+2 fns)=4, returning(+input,+1 fn)=3, struct(+2 fields)=3, fn(+sig,+body)=3, runner=1
    assert_eq!(g.map.segments().len(), n * (4 + 3 + 3 + 3 + 1));
    let count = |k: K| g.map.segments().iter().filter(|s| s.kind == k).count();
    assert_eq!(count(K::InputRange), n);
    assert_eq!(count(K::InputVariable), n);
    assert_eq!(count(K::StreamMap), 2 * n);
    assert_eq!(count(K::StructField), 2 * n);
    assert_eq!(count(K::FunctionBody), n);
    // cheap structural check (assert_nested is quadratic)
    assert_eq!(g.rust, marigold_parse(&src).unwrap());
    let segs = g.map.segments();
    let mut stack: Vec<&Segment> = Vec::new();
    for s in segs {
        while let Some(top) = stack.last() {
            if top.generated.end <= s.generated.start {
                stack.pop();
            } else {
                break;
            }
        }
        if let Some(top) = stack.last() {
            assert!(
                s.generated.end <= top.generated.end,
                "partial overlap {top:?} {s:?}"
            );
            assert!(top.source.start <= s.source.start && s.source.end <= top.source.end);
        }
        stack.push(s);
    }
}

// There is deliberately no wall-clock ratio test here: a fixed-margin ratio of two timings is
// flaky on a loaded machine. The linearity of the work is pinned by deterministic counters in
// `src/source_map.rs` (`construction_work_is_n_log_n`,
// `pairing_parts_with_pieces_does_linear_work_in_the_chain_length`) and by the paired-round
// timing test `mapped_code_generation_grows_linearly_with_program_size` in `src/parser.rs`,
// whose program contains nested segments (inputs, stream functions, struct fields).

#[test]
fn nested_lookups_are_fast_on_a_big_program() {
    use std::time::Instant;
    let src = many_chain_exprs(1_000);
    let g = generate(&src);
    let t = Instant::now();
    let mut hits = 0usize;
    for s in g.map.segments().iter().step_by(7) {
        hits += g
            .map
            .source_for_generated((s.generated.start + s.generated.end) / 2)
            .is_some() as usize;
        hits += g.map.generated_for_source_innermost(s.source).len();
        hits += g.map.generated_for_source(s.source).len();
    }
    assert!(hits > 0);
    assert!(t.elapsed().as_secs() < 5, "{:?}", t.elapsed());
}

// ---------------------------------------------------------------------------------------------
// properties: arbitrary valid programs, whitespace everywhere
// ---------------------------------------------------------------------------------------------

fn ws() -> impl Strategy<Value = &'static str> {
    prop_oneof![
        Just(""),
        Just(" "),
        Just("\n"),
        Just("\r\n"),
        Just("\t"),
        Just("  \r\n \t")
    ]
}

fn name(prefix: &'static str) -> impl Strategy<Value = String> {
    (0u32..4).prop_map(move |n| format!("{prefix}{n}"))
}

/// One stream function with random inner whitespace where the grammar allows it.
fn arb_fn() -> impl Strategy<Value = String> {
    (0u8..11, name("f"), name("g"), 1u32..4, ws(), ws()).prop_map(|(k, f, g, n, w1, w2)| match k {
        0 => format!("map({w1}{f}{w2})"),
        1 => format!("filter({w1}{f}{w2})"),
        2 => format!("filter_map({w1}{f}{w2})"),
        3 => format!("permutations({w1}{n}{w2})"),
        4 => format!("permutations_with_replacement({w1}{n}{w2})"),
        5 => format!("combinations({w1}{n}{w2})"),
        6 => format!("keep_first_n({w1}{n}{w2},{w1}{f}{w2})"),
        7 => format!("fold({w1}{n}{w2},{w1}{f}{w2})"),
        8 => format!("fold({w1}{g}{w2},{w2}{f}{w1})"),
        9 => "ok()".to_string(),
        _ => "ok_or_panic()".to_string(),
    })
}

fn dot(w1: &str, w2: &str) -> String {
    format!("{w1}.{w2}")
}

fn arb_leaf_input() -> impl Strategy<Value = String> {
    (0u8..4, 0u32..9, ws(), ws()).prop_map(|(k, n, w1, w2)| match k {
        0 => format!("range({w1}{n},{w2}{})", n + 3),
        1 => format!("range({n},{w1}={w2}{})", n + 3),
        2 => format!("read_file({w1}\"in{n}.csv\"{w2},csv,struct=R{n})"),
        _ => format!(
            "read_file(\"in{n}.csv.gz\",{w1}csv,struct=R{n}{w2},{w1}infer_compression={})",
            n % 2 == 0
        ),
    })
}

/// input, optionally wrapped in select_all (recursively), followed by 0..4 functions.
fn arb_input() -> impl Strategy<Value = String> {
    arb_leaf_input().prop_recursive(3, 12, 3, |inner| {
        (
            prop::collection::vec(
                (inner, prop::collection::vec(arb_fn(), 0..3), ws(), ws()),
                1..4,
            ),
            ws(),
        )
            .prop_map(|(branches, w)| {
                let parts: Vec<String> = branches
                    .into_iter()
                    .map(|(inp, fns, w1, w2)| {
                        let mut s = inp;
                        for f in fns {
                            s.push_str(&dot(w1, w2));
                            s.push_str(&f);
                        }
                        s.push_str(w2);
                        s
                    })
                    .collect();
                format!("select_all({w}{}{w})", parts.join(&format!(",{w}")))
            })
    })
}

fn arb_chain() -> impl Strategy<Value = String> {
    (
        arb_input(),
        prop::collection::vec(arb_fn(), 0..5),
        ws(),
        ws(),
    )
        .prop_map(|(inp, fns, w1, w2)| {
            let mut s = inp;
            for f in fns {
                s.push_str(&dot(w1, w2));
                s.push_str(&f);
            }
            s
        })
}

fn arb_output() -> impl Strategy<Value = String> {
    (0u8..6, 0u32..5).prop_map(|(k, n)| match k {
        0 | 1 => "return".to_string(),
        2 => format!("write_file(\"o{n}.csv\", csv)"),
        3 => format!("write_file(\"o{n}.csv.gz\",csv)"),
        4 => format!("write_file( \"\u{65e5}{n}.csv\" , csv , compression=gz )"),
        _ => format!("write_file(\"o{n}.csv\", csv, compression=none)"),
    })
}

fn arb_struct(i: usize) -> impl Strategy<Value = String> {
    (
        prop::collection::vec((0u8..6, 1u32..30), 0..4),
        ws(),
        any::<bool>(),
    )
        .prop_map(move |(fields, w, trailing)| {
            let fs: Vec<String> = fields
                .iter()
                .enumerate()
                .map(|(j, (k, n))| {
                    let ty = match k {
                        0 => "i32".to_string(),
                        1 => format!("string_{n}"),
                        2 => "Option<u8>".to_string(),
                        3 => "Vec<u8>".to_string(),
                        4 => format!("int[0, {n}]"),
                        _ => format!("uint[1, {}]", n + 1),
                    };
                    format!("f{j}{w}:{w}{ty}{w}")
                })
                .collect();
            let tail = if trailing && !fs.is_empty() { "," } else { "" };
            format!("struct S{i} {{{w}{}{tail}{w}}}", fs.join(","))
        })
}

fn arb_enum(i: usize) -> impl Strategy<Value = String> {
    (prop::collection::vec(any::<bool>(), 0..4), 0u8..4, ws()).prop_map(
        move |(variants, default, w)| {
            let mut parts: Vec<String> = variants
                .iter()
                .enumerate()
                .map(|(j, valued)| {
                    if *valued {
                        format!("V{j}{w}={w}\"\u{fc}{j}\"{w}")
                    } else {
                        format!("V{j}{w}")
                    }
                })
                .collect();
            match default {
                0 => {}
                1 => parts.push(format!("default{w} Dflt{w}")),
                2 => parts.push(format!("default{w} Dflt{w}({w}string_8{w})")),
                _ => parts.push(format!("default{w} Dflt{w}={w}\"d\"{w}")),
            }
            format!("enum E{i} {{{w}{}{w}}}", parts.join(","))
        },
    )
}

fn arb_fn_decl(i: usize) -> impl Strategy<Value = String> {
    (prop::collection::vec((any::<bool>(), 0u8..3), 0..3), 0u8..3, ws(), 1u32..99).prop_map(move |(params, ret, w, n)| {
        let ps: Vec<String> = params
            .iter()
            .enumerate()
            .map(|(j, (by_ref, t))| {
                let ty = ["i32", "string_10", "Vec<u8>"][*t as usize];
                format!("p{j}{w}:{w}{}{ty}", if *by_ref { "&" } else { "" })
            })
            .collect();
        let rty = ["i32", "bool", "string_5"][ret as usize];
        format!(
            "fn h{i}({}){w}->{w}{rty}{w}{{\r\n  /* \u{e9}\u{65e5} */ let s = \"}}{{\"; {n} // }}\r\n}}",
            ps.join(",")
        )
    })
}

/// A full program: 0..8 items of random kind (declarations and streams), unique names per
/// position, random separators.
fn arb_program2() -> impl Strategy<Value = String> {
    let item = |i: usize| -> BoxedStrategy<String> {
        prop_oneof![
            4 => (arb_chain(), arb_output(), ws(), ws()).prop_map(|(c, o, w1, w2)| format!("{c}{}{o}", dot(w1, w2))),
            3 => (arb_chain(), 0u32..3).prop_map(move |(c, n)| format!("w{n}_{i} = {c}")),
            2 => (0u32..3, prop::collection::vec(arb_fn(), 0..3), arb_output(), ws(), ws())
                .prop_map(|(n, fns, o, w1, w2)| {
                    // reads a variable (it need not be declared; the grammar does not care)
                    let mut s = format!("w{n}_0");
                    for f in fns {
                        s.push_str(&dot(w1, w2));
                        s.push_str(&f);
                    }
                    s.push_str(&dot(w1, w2));
                    s.push_str(&o);
                    s
                }),
            2 => (0u32..3, prop::collection::vec(arb_fn(), 0..3), ws(), ws())
                .prop_map(move |(n, fns, w1, w2)| {
                    let mut s = format!("d{i} = w{n}_0");
                    for f in fns {
                        s.push_str(&dot(w1, w2));
                        s.push_str(&f);
                    }
                    s
                }),
            2 => arb_struct(i).boxed(),
            2 => arb_enum(i).boxed(),
            2 => arb_fn_decl(i).boxed(),
        ]
        .boxed()
    };
    let items: Vec<BoxedStrategy<String>> = (0..8).map(item).collect();
    (items, 0usize..=8, prop::collection::vec(ws(), 8), ws()).prop_map(|(items, n, seps, lead)| {
        let mut src = lead.to_string();
        for i in 0..n {
            src.push_str(&items[i]);
            src.push_str(if seps[i].is_empty() { "\n" } else { seps[i] });
        }
        src
    })
}

proptest! {

    #[test]
    fn nested_invariants_hold_for_random_valid_programs(src in arb_program2()) {
        prop_assert!(marigold_parse(&src).is_ok(), "generator produced invalid program {:?}", src);
        assert_nested(&src, &generate(&src));
    }

    #[test]
    fn nested_invariants_hold_for_one_character_mutations(
        base in arb_program2(),
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
                assert_nested(&src, &g);
            }
            Err(e) => prop_assert_eq!(marigold_generate(&src).unwrap_err().0, e.0),
        }
    }

    #[test]
    fn arbitrary_text_never_breaks_the_nested_map(src in "\\PC{0,80}") {
        if marigold_parse(&src).is_ok() {
            assert_nested(&src, &generate(&src));
        }
    }
}

#[test]
fn the_program_generator_reaches_every_segment_kind() {
    use proptest::strategy::{Strategy, ValueTree};
    use proptest::test_runner::TestRunner;
    let mut runner = TestRunner::deterministic();
    let strategy = arb_program2();
    let mut kinds = std::collections::HashSet::new();
    let (mut max_depth, mut programs) = (0, 0);
    for _ in 0..400 {
        let src = strategy.new_tree(&mut runner).unwrap().current();
        let g = generate(&src);
        programs += 1;
        for (i, s) in g.map.segments().iter().enumerate() {
            kinds.insert(s.kind);
            max_depth = max_depth.max(depth(&g, i));
        }
    }
    let mut want: Vec<K> = RULE_KINDS.iter().filter_map(|(_, k)| *k).collect();
    want.extend([
        K::Struct,
        K::Enum,
        K::Function,
        K::StreamVariable,
        K::StreamVariableRunner,
        K::ReturningStream,
        K::NonReturningStream,
        K::InputVariable,
        K::SelectAllBranch,
        K::FunctionSignature,
        K::FunctionBody,
        K::StructField,
        K::EnumVariant,
    ]);
    for k in want {
        assert!(
            kinds.contains(&k),
            "generator never produced {k:?} in {programs} programs"
        );
    }
    assert!(
        max_depth >= 4,
        "generator never nests select_all: max depth {max_depth}"
    );
}

// ---------------------------------------------------------------------------------------------
// more goldens, edge cases and API contract
// ---------------------------------------------------------------------------------------------

#[test]
fn t2_g24_select_all_with_a_single_bare_input_branch() {
    // branch and input have the same source text: the one case where a child is not a proper sub-range
    let src = "select_all(range(0, 3)).return";
    let g = generate(src);
    let rows: Vec<(usize, K, &str)> = g
        .map
        .segments()
        .iter()
        .enumerate()
        .map(|(i, s)| (depth(&g, i), s.kind, stext(src, s)))
        .collect();
    assert_eq!(
        rows,
        vec![
            (0, K::ReturningStream, src),
            (1, K::InputSelectAll, "select_all(range(0, 3))"),
            (2, K::SelectAllBranch, "range(0, 3)"),
            (3, K::InputRange, "range(0, 3)"),
        ]
    );
    let q = ByteRange { start: 12, end: 13 };
    assert_eq!(g.map.generated_for_source(q).len(), 4);
    let inner = g.map.generated_for_source_innermost(q);
    assert_eq!(inner.len(), 1);
    assert_eq!(inner[0].kind, K::InputRange);
    assert_nested(src, &g);
}

#[test]
fn t2_g25_signatures_and_field_types_are_normalised_not_verbatim() {
    let src = "fn cmp(a: &[u8;4],b:&[u8; 4]) -> std::cmp::Ordering { a.cmp(b) }
struct M { m: HashMap<String,Vec<u8>>, o: Option<Option<u8>> , t : Foo   }";
    let g = generate(src);
    let sig = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::FunctionSignature)
        .unwrap();
    assert_eq!(
        stext(src, sig),
        "cmp(a: &[u8;4],b:&[u8; 4]) -> std::cmp::Ordering"
    );
    assert_eq!(
        gtext(&g, sig),
        "cmp(a: &[u8;4], b: &[u8; 4]) -> std::cmp::Ordering"
    );
    let fields: Vec<(&str, &str)> = g
        .map
        .segments()
        .iter()
        .filter(|s| s.kind == K::StructField)
        .map(|s| (stext(src, s), gtext(&g, s)))
        .collect();
    assert_eq!(
        fields,
        [
            ("m: HashMap<String,Vec<u8>>", "m: HashMap<String, Vec<u8>>"),
            ("o: Option<Option<u8>>", "o: Option<Option<u8>>"),
            ("t : Foo", "t: Foo"),
        ]
    );
    assert_nested(src, &g);
}

#[test]
fn t2_g26_wide_declarations_pair_every_field_and_variant_with_its_own_source() {
    let n = 300;
    let fields: Vec<String> = (0..n).map(|i| format!("field_{i}: u32")).collect();
    let variants: Vec<String> = (0..n).map(|i| format!("Variant{i}")).collect();
    let src = format!(
        "struct Wide {{ {} }}\nenum Many {{ {}, default Fallback }}",
        fields.join(", "),
        variants.join(", ")
    );
    let g = generate(&src);
    let f: Vec<&Segment> = g
        .map
        .segments()
        .iter()
        .filter(|s| s.kind == K::StructField)
        .collect();
    let v: Vec<&Segment> = g
        .map
        .segments()
        .iter()
        .filter(|s| s.kind == K::EnumVariant)
        .collect();
    assert_eq!((f.len(), v.len()), (n, n + 1));
    for (i, s) in f.iter().enumerate() {
        assert_eq!(s.name.as_deref(), Some(format!("field_{i}").as_str()));
        assert_eq!(stext(&src, s), format!("field_{i}: u32"));
        assert_eq!(gtext(&g, s), format!("field_{i}: u32"));
    }
    assert_eq!(v[n].name.as_deref(), Some("Fallback"));
    assert_eq!(stext(&src, v[n]), "default Fallback");
    assert_eq!(gtext(&g, v[n]), "Fallback");
    assert_nested(&src, &g);
}

#[test]
fn programs_that_fail_after_parsing_return_the_same_error_and_never_panic() {
    let cases = [
        "range(0, 1).write_file(\"o.json\", json)", // unsupported write_file format (AST builder error)
        "range(5, 1).return",                       // negative range count (AST builder error)
        "range(Nope).return",                       // unknown enum
        "enum E { A }\nx = range(Nope)\nx.return",
        "struct T { f: int[10, 5] }\nrange(0, 1).return", // bound validation
        "fn f(x: i32) -> i32\nrange(0,1).return",         // pest error
        "select_all(range(5, 1)).return",
    ];
    for src in cases {
        let expected = marigold_parse(src).expect_err(src);
        let got = marigold_generate(src).expect_err(src);
        assert_eq!(got.0, expected.0, "{src:?}");
    }
}

#[test]
fn segment_and_map_constructors_support_nested_hand_built_maps() {
    use marigold_grammar::source_map::SourceMap;
    let mk = |g: (usize, usize), s: (usize, usize), k: K| {
        Segment::new(
            ByteRange {
                start: g.0,
                end: g.1,
            },
            ByteRange {
                start: s.0,
                end: s.1,
            },
            k,
            0,
            None,
        )
    };
    let outer =
        mk((0, 50), (0, 20), K::ReturningStream).with_ident(Some("returning_stream_0".into()));
    let inner = mk((10, 20), (3, 9), K::StreamMap);
    assert_eq!(outer.ident.as_deref(), Some("returning_stream_0"));
    assert_eq!(inner.ident, None);
    let m = SourceMap::new(vec![inner.clone(), outer.clone()]);
    assert_eq!(m.segments(), &[outer.clone(), inner.clone()]);
    assert_eq!(m.parent(1), Some(0));
    assert_eq!(m.source_for_generated(15), Some(&inner));
    assert_eq!(m.source_for_generated(5), Some(&outer));
    assert_eq!(m.segments_for_ident("returning_stream_0"), vec![&outer]);
    assert_eq!(
        m.generated_for_source_innermost(ByteRange { start: 4, end: 5 }),
        vec![&inner]
    );
}

#[test]
fn identifiers_and_tree_shape_of_a_program_survive_appending_unrelated_items() {
    let base = "x = range(0, 5).map(f)\nx.return\nrange(0, 2).write_file(\"o.csv\", csv)";
    let a = generate(base);
    let idents = |g: &GeneratedProgram| -> Vec<(K, usize, Option<String>)> {
        g.map
            .segments()
            .iter()
            .map(|s| (s.kind, s.expr_index, s.ident.clone()))
            .collect()
    };
    for extra in [
        "\nstruct Z { a: u8 }",
        "\nenum Z { A }",
        "\nfn z() -> i32 { 1 }",
        "\ny = x.map(g)",
    ] {
        let b = generate(&format!("{base}{extra}"));
        // every segment of `a` is still there with the same kind, expr_index and identifier ...
        let before = idents(&a);
        let after = idents(&b);
        for row in &before {
            assert!(
                after.contains(row),
                "{row:?} lost after appending {extra:?}"
            );
        }
        // ... and the sources of the statements are unchanged
        for s in a.map.top_level() {
            assert!(
                b.map
                    .top_level()
                    .any(|t| t.source == s.source && t.kind == s.kind && t.ident == s.ident),
                "{s:?}"
            );
        }
    }
    // appending a returning stream keeps the numbering of the earlier ones; prepending one renumbers
    let appended = generate(&format!("{base}\nrange(0, 9).return"));
    let prepended = generate(&format!("range(0, 9).return\n{base}"));
    let ret_ident = |g: &GeneratedProgram, text: &str, src: &str| {
        g.map
            .top_level()
            .find(|s| s.kind == K::ReturningStream && stext(src, s) == text)
            .unwrap()
            .ident
            .clone()
            .unwrap()
    };
    assert_eq!(ret_ident(&a, "x.return", base), "returning_stream_0");
    assert_eq!(
        ret_ident(
            &appended,
            "x.return",
            &format!("{base}\nrange(0, 9).return")
        ),
        "returning_stream_0"
    );
    assert_eq!(
        ret_ident(
            &prepended,
            "x.return",
            &format!("range(0, 9).return\n{base}")
        ),
        "returning_stream_1"
    );
}

#[test]
fn index_of_maps_a_stored_segment_to_its_index_and_rejects_everything_else() {
    let g = generate("x = range(0, 5).map(f)\nx.return");
    for (i, s) in g.map.segments().iter().enumerate() {
        assert_eq!(g.map.index_of(s), Some(i));
    }
    // a clone lives somewhere else; so does a segment of another map
    let copy = g.map.segments()[1].clone();
    assert_eq!(g.map.index_of(&copy), None);
    let other = generate("range(0, 1).return");
    assert_eq!(g.map.index_of(&other.map.segments()[0]), None);
    assert_eq!(other.map.index_of(&g.map.segments()[0]), None);
    // the point of it: from a lookup result to its ancestors
    // (not `rust.find(".map(f)")`: with `tokio`/`async-std` the region text is different)
    let map = g
        .map
        .segments()
        .iter()
        .find(|s| s.kind == K::StreamMap)
        .unwrap();
    let hit = g.map.source_for_generated(map.generated.start + 1).unwrap();
    assert_eq!(hit.kind, K::StreamMap);
    let parent = g.map.parent(g.map.index_of(hit).unwrap()).unwrap();
    assert_eq!(g.map.segments()[parent].kind, K::StreamVariable);
    assert_eq!(g.map.parent(parent), None);
}
