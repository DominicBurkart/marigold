#![cfg(feature = "otel")]

use marigold_grammar::instrument::CodegenOptions;
use marigold_grammar::node_ids::NodeKind;
use marigold_grammar::{marigold_node_ids, marigold_parse, marigold_parse_instrumented};

const PROGRAMS: &[&str] = &[
    "range(0, 10).return",
    "range(0, 5).map(double).filter(is_even).return",
    "range(0, 100).filter(is_even).keep_first_n(5, compare).return",
    "range(0, 10).permutations(2).combinations(2).return",
    "range(0, 100).fold(0, add).return",
    "range(0, 10).permutations_with_replacement(2).return",
    "read_file(\"data.csv\", csv, struct=Data).ok().map(transform).return",
    "read_file(\"data.csv\", csv, struct=Data).ok_or_panic().return",
    "select_all(range(0, 10).map(double), range(0, 20)).filter(is_odd).return",
    "read_file(\"in.csv\", csv, struct=Data).ok_or_panic().write_file(\"out/o.csv.gz\", csv)",
    "range(0, 3).map(to_row).write_file(\"out.csv\", csv)",
    "fn doubled_plus_ten(i: i32) -> i32 {\n    (i * 2) + 10\n}\n\ndigits = range(0, 10)\n\ndigits\n    .return\n\ndigits\n    .filter(is_even)\n    .map(doubled_plus_ten)\n    .return",
    "digits = range(0, 10)\nodd = digits.filter(is_odd)\nodd.map(double).return\ndigits.return",
    "struct Ship { class: string_8 }\nenum Hull { A, B }\nrange(Hull).return\nrange(0, 2).return",
    "range(0, 3).return\nrange(0, 3).return",
];

fn options() -> CodegenOptions {
    CodegenOptions::new("demo")
}

#[test]
fn every_node_id_is_embedded_in_the_generated_code() {
    for src in PROGRAMS {
        let code = marigold_parse_instrumented(src, &options()).unwrap();
        for node in marigold_node_ids(src, "demo") {
            if node.kind == NodeKind::VariableDeclaration {
                continue;
            }
            assert!(
                code.contains(&format!("\"{}\"", node.id)),
                "missing {} ({:?}) for {src}",
                node.id,
                node.kind
            );
        }
    }
}

#[test]
fn every_node_is_wrapped_with_the_instrument_adapters() {
    let src = "range(0, 5).map(double).filter(is_even).return";
    let code = marigold_parse_instrumented(src, &options()).unwrap();
    let t = "::marigold::marigold_impl::telemetry";
    assert_eq!(code.matches(&format!("{t}::instrument(")).count(), 4);
    assert_eq!(code.matches(&format!("{t}::instrument_in(")).count(), 3);
    assert_eq!(code.matches(&format!("{t}::NodeMeta::new(")).count(), 7);
    assert!(code.contains("\"demo\""));
}

#[test]
fn node_metas_carry_ranges_into_the_source() {
    let src = "range(0, 5).map(double).return";
    let code = marigold_parse_instrumented(src, &options().with_file("p.marigold")).unwrap();
    for node in marigold_node_ids(src, "demo") {
        let meta = format!(
            "NodeMeta::new(\"{}\", \"demo\", \"p.marigold\", {}, {})",
            node.id, node.range.start, node.range.end
        );
        assert!(code.contains(&meta), "{meta}");
    }
}

#[test]
fn file_defaults_to_the_expansion_site() {
    let code = marigold_parse_instrumented("range(0, 2).return", &options()).unwrap();
    assert!(code.contains("\"demo\", file!(), "));
}

#[test]
fn result_items_use_the_results_adapters() {
    let code = marigold_parse_instrumented(
        "read_file(\"d.csv\", csv, struct=D).ok().map(f).return",
        &options(),
    )
    .unwrap();
    assert!(code.contains("telemetry::instrument_results("));
    assert!(code.contains("telemetry::instrument_in_results("));
    let plain = marigold_parse_instrumented("range(0, 2).map(f).return", &options()).unwrap();
    assert!(!plain.contains("_results("));
    let read_only = marigold_parse_instrumented(
        "read_file(\"d.csv\", csv, struct=D).permutations(2).return",
        &options(),
    )
    .unwrap();
    assert_eq!(
        read_only.matches("telemetry::instrument_results(").count(),
        1
    );
    assert!(!read_only.contains("telemetry::instrument_in_results("));
    let ok_or_panic = marigold_parse_instrumented(
        "read_file(\"d.csv\", csv, struct=D).ok_or_panic().return",
        &options(),
    )
    .unwrap();
    assert_eq!(
        ok_or_panic
            .matches("telemetry::instrument_in_results(")
            .count(),
        1
    );
}

#[test]
fn select_all_inner_streams_are_not_instrumented_separately() {
    let src = "select_all(range(0, 10).map(double), range(0, 20)).return";
    let code = marigold_parse_instrumented(src, &options()).unwrap();
    let ids = marigold_node_ids(src, "demo");
    assert_eq!(ids.len(), 2);
    assert_eq!(code.matches("NodeMeta::new(").count(), 3);
}

#[test]
fn generation_is_deterministic_and_program_scoped() {
    for src in PROGRAMS {
        let a = marigold_parse_instrumented(src, &options()).unwrap();
        let b = marigold_parse_instrumented(src, &options()).unwrap();
        assert_eq!(a, b);
        let other = marigold_parse_instrumented(src, &CodegenOptions::new("other")).unwrap();
        assert_ne!(a, other);
    }
}

#[test]
fn plain_generation_is_unchanged_by_the_feature() {
    for src in PROGRAMS {
        let plain = marigold_parse(src).unwrap();
        assert!(!plain.contains("telemetry"), "{src}");
        let instrumented = marigold_parse_instrumented(src, &options()).unwrap();
        assert_ne!(plain, instrumented);
    }
}

#[test]
fn invalid_programs_still_fail_to_generate() {
    assert!(marigold_parse_instrumented("range(0,", &options()).is_err());
    assert!(marigold_parse_instrumented("range(Missing).return", &options()).is_err());
}
