#![cfg(feature = "otel")]

use marigold_grammar::instrument::CodegenOptions;
use marigold_grammar::node_ids::NodeKind;
use marigold_grammar::source_map::{GeneratedProgram, SegmentKind};
use marigold_grammar::{
    marigold_generate, marigold_generate_instrumented, marigold_node_ids, marigold_parse,
    marigold_parse_instrumented,
};

const PROGRAMS: &[&str] = &[
    "range(0, 10).return",
    "range(0, 5).map(double).filter(is_even).return",
    "range(0, 100).filter(is_even).keep_first_n(5, compare).return",
    "read_file(\"data.csv\", csv, struct=Data).ok().map(transform).return",
    "select_all(range(0, 10).map(double), range(0, 20)).filter(is_odd).return",
    "range(0, 3).map(to_row).write_file(\"out.csv\", csv)",
    "fn doubled_plus_ten(i: i32) -> i32 {\n    (i * 2) + 10\n}\n\ndigits = range(0, 10)\n\ndigits\n    .return\n\ndigits\n    .filter(is_even)\n    .map(doubled_plus_ten)\n    .return",
    "digits = range(0, 10)\nodd = digits.filter(is_odd)\nodd.map(double).return\ndigits.return",
    "struct Ship { class: string_8 }\nenum Hull { A, B }\nrange(Hull).return\nrange(0, 2).return",
    "range(0, 2).return\nfn f(x: i32) -> i32 { x }\nx = range(0, 3).map(f)\nstruct S { a: u8 }\nenum E { A }\nx.write_file(\"o.csv\", csv)\nrange(E).return",
    "x = range(0, 3)\ny = x.map(f)\ny.return\nx.return",
    "range(0, 3).return\nrange(0, 3).return",
    "range(0, \u{1F600}",
    "",
];

fn options() -> CodegenOptions {
    CodegenOptions::new("demo")
}

fn is_stream_kind(kind: SegmentKind) -> bool {
    matches!(
        kind,
        SegmentKind::ReturningStream
            | SegmentKind::NonReturningStream
            | SegmentKind::StreamVariable
            | SegmentKind::StreamVariableRunner
    )
}

fn outside_segments(program: &GeneratedProgram) -> Vec<&str> {
    let mut gaps = Vec::new();
    let mut at = 0;
    let mut segments: Vec<_> = program.map.segments().iter().collect();
    segments.sort_by_key(|s| s.generated.start);
    for s in segments {
        gaps.push(&program.rust[at..s.generated.start]);
        at = s.generated.end;
    }
    gaps.push(&program.rust[at..]);
    gaps
}

#[test]
fn without_a_plan_generation_is_exactly_the_plain_output() {
    for src in PROGRAMS {
        match (marigold_generate(src), marigold_parse(src)) {
            (Ok(program), Ok(plain)) => {
                assert_eq!(program.rust, plain, "{src}");
                assert!(!program.rust.contains("telemetry"), "{src}");
            }
            (Err(a), Err(b)) => assert_eq!(a.0, b.0, "{src}"),
            _ => panic!("generate and parse disagree on {src}"),
        }
    }
}

#[test]
fn generate_instrumented_returns_exactly_the_code_of_parse_instrumented() {
    for src in PROGRAMS {
        match (
            marigold_generate_instrumented(src, &options()),
            marigold_parse_instrumented(src, &options()),
        ) {
            (Ok(program), Ok(code)) => assert_eq!(program.rust, code, "{src}"),
            (Err(a), Err(b)) => assert_eq!(a.0, b.0, "{src}"),
            _ => panic!("instrumented generate and parse disagree on {src}"),
        }
    }
}

#[test]
fn instrumentation_does_not_change_the_shape_of_the_map() {
    for src in PROGRAMS {
        let (Ok(plain), Ok(instrumented)) = (
            marigold_generate(src),
            marigold_generate_instrumented(src, &options()),
        ) else {
            continue;
        };
        let shape = |p: &GeneratedProgram| {
            p.map
                .segments()
                .iter()
                .map(|s| (s.kind, s.expr_index, s.name.clone(), s.source))
                .collect::<Vec<_>>()
        };
        assert_eq!(shape(&plain), shape(&instrumented), "{src}");
        assert_eq!(
            outside_segments(&plain),
            outside_segments(&instrumented),
            "{src}"
        );
    }
}

#[test]
fn wrapper_text_maps_to_the_expression_it_instruments() {
    for src in PROGRAMS {
        let Ok(program) = marigold_generate_instrumented(src, &options()) else {
            continue;
        };
        let nodes = marigold_node_ids(src, "demo");
        for segment in program.map.segments() {
            assert!(segment.generated.end <= program.rust.len(), "{src}");
            assert!(segment.source.end <= src.len(), "{src}");
            let text = &program.rust[segment.generated.start..segment.generated.end];
            for node in &nodes {
                if node.kind == NodeKind::VariableDeclaration {
                    continue;
                }
                let embedded = text.contains(&format!("\"{}\"", node.id));
                if node.expr_index == segment.expr_index && is_stream_kind(segment.kind) {
                    if segment.kind != SegmentKind::StreamVariableRunner {
                        assert!(embedded, "node {} missing from its segment: {src}", node.id);
                    }
                } else {
                    assert!(
                        !embedded,
                        "node {} of expr {} leaked into a segment of expr {}: {src}",
                        node.id, node.expr_index, segment.expr_index
                    );
                }
                if embedded {
                    assert!(
                        segment.source.start <= node.range.start
                            && node.range.end <= segment.source.end,
                        "{src}"
                    );
                    let at =
                        segment.generated.start + text.find(&format!("\"{}\"", node.id)).unwrap();
                    assert_eq!(program.map.source_for_generated(at), Some(segment), "{src}");
                }
            }
        }
    }
}

#[test]
fn every_instrumented_stream_segment_carries_the_adapters() {
    let src = "x = range(0, 3).map(f)\nx.return\nrange(0, 2).write_file(\"o.csv\", csv)";
    let program = marigold_generate_instrumented(src, &options()).unwrap();
    let mut seen = 0;
    for segment in program.map.segments() {
        let text = &program.rust[segment.generated.start..segment.generated.end];
        match segment.kind {
            SegmentKind::StreamVariable
            | SegmentKind::ReturningStream
            | SegmentKind::NonReturningStream => {
                assert!(text.contains("telemetry::instrument"), "{text}");
                seen += 1;
            }
            _ => {}
        }
    }
    assert_eq!(seen, 3);
}
