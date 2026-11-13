use std::fs;
use std::path::PathBuf;

use marigold_grammar::complexity::{Cardinality, ComplexityClass, SizeBindings, SizeName};
use marigold_grammar::sizes::{
    InputSizes, ReasonCode, Resolution, ResolutionKind, SizeAnalysisError, StreamResolution,
    SymbolicAnalysis,
};
use marigold_grammar::{marigold_analyze, marigold_analyze_with_sizes};
use num_bigint::BigUint;
use serde_json::json;

const PERMUTATIONS: &str =
    "struct D { x: i32 }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().permutations(3).return";

fn sizes(bindings: &[(usize, &str)]) -> InputSizes {
    let mut sizes = InputSizes::new();
    for (ordinal, name) in bindings {
        sizes = sizes.bind(*ordinal, name).expect("valid binding");
    }
    sizes
}

fn analyze(source: &str, bindings: &[(usize, &str)]) -> SymbolicAnalysis {
    marigold_analyze_with_sizes(source, &sizes(bindings))
        .unwrap_or_else(|e| panic!("analysis failed: {e}"))
}

fn kinds(stream: &StreamResolution) -> Vec<ResolutionKind> {
    stream
        .stages
        .iter()
        .map(|stage| stage.resolution.kind())
        .collect()
}

fn labels(stream: &StreamResolution) -> Vec<&str> {
    stream.stages.iter().map(|s| s.stage.as_str()).collect()
}

fn last(stream: &StreamResolution) -> &Resolution {
    &stream
        .stages
        .last()
        .expect("a stream has an input")
        .resolution
}

fn at(n: u64) -> SizeBindings {
    SizeBindings::from([(SizeName::new("n").unwrap(), BigUint::from(n))])
}

fn big(value: u64) -> Option<BigUint> {
    Some(BigUint::from(value))
}

#[test]
fn a_bound_input_is_analyzed_with_symbolic_classes_and_stage_resolutions() {
    let analysis = analyze(PERMUTATIONS, &[(0, "n")]);
    assert_eq!(analysis.streams.len(), 1);
    let stream = &analysis.streams[0];
    assert_eq!(
        labels(stream),
        vec!["input", "ok_or_panic()", "permutations(3)"]
    );
    assert_eq!(kinds(stream), vec![ResolutionKind::Symbolic; 3]);

    let complexity = &analysis.program.streams[0];
    assert_eq!(complexity.time_class, ComplexityClass::OPermutational(3));
    assert!(complexity.collects_input);
    assert_eq!(complexity.cardinality.to_string(), "P($n, 3)");

    for (n, want) in [(0, 0), (1, 0), (2, 0), (3, 6), (5, 60)] {
        let range = last(stream).evaluate(&at(n));
        assert_eq!((range.lo, range.hi), (big(want), big(want)), "n={n}");
    }
}

#[test]
fn an_unbound_program_reports_exactly_the_default_analysis_and_names_why_it_is_unbound() {
    let analysis = analyze(PERMUTATIONS, &[]);
    let default = marigold_analyze(PERMUTATIONS).unwrap();
    assert_eq!(
        serde_json::to_string(&analysis.program).unwrap(),
        serde_json::to_string(&default).unwrap()
    );
    let stream = &analysis.streams[0];
    assert_eq!(kinds(stream), vec![ResolutionKind::Unbound; 3]);
    for stage in &stream.stages {
        assert_eq!(stage.resolution.reasons(), &[ReasonCode::UnboundInput]);
    }
}

#[test]
fn a_filter_over_a_bound_input_is_bounded_with_an_opaque_predicate_reason() {
    let source = "struct D { x: i32 }\nfn keep(d: &D) -> bool { true }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().filter(keep).return";
    let analysis = analyze(source, &[(0, "n")]);
    let stream = &analysis.streams[0];
    assert_eq!(
        kinds(stream),
        vec![
            ResolutionKind::Symbolic,
            ResolutionKind::Symbolic,
            ResolutionKind::Bounded
        ]
    );
    let resolution = last(stream);
    assert_eq!(resolution.reasons(), &[ReasonCode::OpaquePredicate]);
    let range = resolution.evaluate(&at(7));
    assert_eq!((range.lo, range.hi), (big(0), big(7)));
    assert_eq!(
        analysis.program.streams[0].cardinality.to_string(),
        "\u{2264}$n"
    );
}

#[test]
fn keep_first_n_bounds_an_unbound_input_and_ok_names_fallible_items() {
    let source = "struct D { x: i32 }\nfn cmp(a: &[D; 2], b: &[D; 2]) -> Ordering { Ordering::Equal }\nread_file(\"d.csv\", csv, struct=D).ok().combinations(2).keep_first_n(5, cmp).return";
    let analysis = analyze(source, &[]);
    let stream = &analysis.streams[0];
    assert_eq!(
        labels(stream),
        vec!["input", "ok()", "combinations(2)", "keep_first_n(5, ...)"]
    );
    assert_eq!(
        kinds(stream),
        vec![
            ResolutionKind::Unbound,
            ResolutionKind::Unbound,
            ResolutionKind::Unbound,
            ResolutionKind::Bounded
        ]
    );
    let capped = last(stream);
    assert_eq!(
        capped.reasons(),
        &[ReasonCode::UnboundInput, ReasonCode::FallibleItems]
    );
    let range = capped.evaluate(&SizeBindings::new());
    assert_eq!((range.lo, range.hi), (big(0), big(5)));
}

#[test]
fn a_fold_is_exactly_one_item_even_over_an_unbound_input() {
    let source = "struct D { x: i32 }\nfn add(acc: i32, d: D) -> i32 { acc + d.x }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().fold(0, add).return";
    for bindings in [vec![], vec![(0, "n")]] {
        let analysis = analyze(source, &bindings);
        let resolution = last(&analysis.streams[0]);
        assert_eq!(resolution.kind(), ResolutionKind::Exact);
        assert!(resolution.reasons().is_empty());
        let range = resolution.evaluate(&SizeBindings::new());
        assert_eq!((range.lo, range.hi), (big(1), big(1)));
    }
}

#[test]
fn an_undeclared_stream_variable_is_unresolvable_for_its_own_reason() {
    let analysis = analyze("ghost.map(f).return", &[]);
    let stream = &analysis.streams[0];
    assert_eq!(kinds(stream), vec![ResolutionKind::Unresolvable; 2]);
    assert_eq!(
        last(stream).reasons(),
        &[ReasonCode::UndeclaredStreamVariable]
    );
}

#[test]
fn a_declared_variable_consumed_twice_shares_one_size() {
    let source = "struct D { x: i32 }\nrows = read_file(\"d.csv\", csv, struct=D).ok_or_panic()\nrows.map(f).return\nrows.permutations(2).return";
    let analysis = analyze(source, &[(0, "n")]);
    let names: Vec<Option<&str>> = analysis.streams.iter().map(|s| s.name.as_deref()).collect();
    assert_eq!(names, vec![Some("rows"), None, None]);
    let mapped = &analysis.streams[1];
    let permuted = &analysis.streams[2];
    assert_eq!(labels(mapped), vec!["input", "ok_or_panic()", "map(...)"]);
    assert_eq!(
        labels(permuted),
        vec!["input", "ok_or_panic()", "permutations(2)"]
    );
    assert_eq!(
        mapped.stages[0].resolution, permuted.stages[0].resolution,
        "both consumers start from the same size"
    );
    let range = last(permuted).evaluate(&at(5));
    assert_eq!((range.lo, range.hi), (big(20), big(20)));
    assert_eq!(analysis.streams[1].expr_index, 2);
    assert_eq!(analysis.streams[2].expr_index, 3);
}

#[test]
fn streams_pair_with_program_streams_through_program_stream_not_by_position() {
    let source = "struct D { x: i32 }\nrows = read_file(\"d.csv\", csv, struct=D).ok_or_panic()\nrows.map(f).return\nrows.permutations(2).return";
    let analysis = analyze(source, &[(0, "n")]);
    assert_eq!(analysis.streams.len(), 3);
    assert_eq!(analysis.program.streams.len(), 2);
    let keys: Vec<Option<usize>> = analysis.streams.iter().map(|s| s.program_stream).collect();
    assert_eq!(keys, vec![None, Some(0), Some(1)]);
    for stream in &analysis.streams {
        if let Some(index) = stream.program_stream {
            assert_eq!(
                analysis.program.streams[index].description,
                stream.description
            );
        }
    }

    let unnamed_first = "struct D { x: i32 }\nrange(0, 3).map(f).return\nrows = read_file(\"a.csv\", csv, struct=D).ok_or_panic()\nmore = rows.map(f)\nrows.map(f).return\nmore.return";
    let analysis = analyze(unnamed_first, &[(1, "n")]);
    let paired: Vec<(Option<&str>, Option<usize>)> = analysis
        .streams
        .iter()
        .map(|s| (s.name.as_deref(), s.program_stream))
        .collect();
    assert_eq!(
        paired,
        vec![
            (None, Some(0)),
            (Some("rows"), None),
            (Some("more"), None),
            (None, Some(1)),
            (None, Some(2)),
        ]
    );
    assert_eq!(analysis.program.streams.len(), 3);
    assert_eq!(
        analysis.program.streams[2].description,
        analysis.streams[4].description
    );
}

fn ceiling(cardinality: &Cardinality, n: u64) -> BigUint {
    match cardinality {
        Cardinality::Exact(v) => v.clone(),
        Cardinality::Bounded(sym) => sym.upper_bound_with(&at(n)).expect("n is bound"),
        Cardinality::Unknown => panic!("an unknown cardinality has no ceiling"),
    }
}

#[test]
fn program_cardinality_with_a_bound_input_does_not_depend_on_stream_order() {
    let declaration = "struct D { x: i32 }\nfn keep(d: &D) -> bool { true }\nrows = read_file(\"d.csv\", csv, struct=D).ok_or_panic()\n";
    let consumers = [
        "rows.map(f).return",
        "rows.permutations(2).return",
        "range(0, 7).return",
        "rows.filter(keep).combinations(2).return",
    ];
    let program = |order: &[usize]| {
        let body: Vec<&str> = order.iter().map(|i| consumers[*i]).collect();
        format!("{declaration}{}", body.join("\n"))
    };

    let forward = analyze(&program(&[0, 1]), &[(0, "n")]).program;
    let backward = analyze(&program(&[1, 0]), &[(0, "n")]).program;
    assert_eq!(forward.program_cardinality, backward.program_cardinality);
    assert_eq!(forward.program_cardinality.to_string(), "$n + P($n, 2)");

    let orders: [&[usize]; 4] = [&[0, 1, 2, 3], &[3, 2, 1, 0], &[2, 0, 3, 1], &[1, 3, 0, 2]];
    let results: Vec<Cardinality> = orders
        .iter()
        .map(|order| {
            analyze(&program(order), &[(0, "n")])
                .program
                .program_cardinality
        })
        .collect();
    for result in &results {
        assert_eq!(result, &results[0]);
    }
    let joined = &results[0];
    for n in 0..10 {
        let ceiling_of_joined = ceiling(joined, n);
        for stream in analyze(&program(&[0, 1, 2, 3]), &[(0, "n")])
            .program
            .streams
        {
            assert!(
                ceiling_of_joined >= ceiling(&stream.cardinality, n),
                "{joined} is below {} at n={n}",
                stream.cardinality
            );
        }
    }
}

#[test]
fn a_stage_that_ignores_its_unbound_input_reports_no_reasons() {
    let source = "struct D { x: i32 }\nfn cmp(a: &D, b: &D) -> Ordering { Ordering::Equal }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().keep_first_n(0, cmp).combinations(0).return";
    let analysis = analyze(source, &[]);
    let stages = &analysis.streams[0].stages;
    let emptied = &stages[2].resolution;
    assert_eq!(emptied.kind(), ResolutionKind::Exact);
    assert_eq!(emptied.lo().to_string(), "0");
    assert!(emptied.reasons().is_empty());
    let chosen = &stages[3].resolution;
    assert_eq!(chosen.kind(), ResolutionKind::Exact);
    assert_eq!(chosen.lo().to_string(), "1");
    assert!(chosen.reasons().is_empty());
    assert_eq!(stages[1].resolution.reasons(), &[ReasonCode::UnboundInput]);
}

#[test]
fn inputs_are_numbered_in_source_order_and_each_binding_applies_to_one() {
    let source = "struct D { x: i32 }\nread_file(\"a.csv\", csv, struct=D).ok_or_panic().map(f).return\nread_file(\"b.csv\", csv, struct=D).ok_or_panic().map(f).return";
    let analysis = analyze(source, &[(1, "m")]);
    assert_eq!(last(&analysis.streams[0]).kind(), ResolutionKind::Unbound);
    assert_eq!(last(&analysis.streams[1]).kind(), ResolutionKind::Symbolic);
    assert_eq!(last(&analysis.streams[1]).lo().to_string(), "$m");

    let both = analyze(source, &[(0, "n"), (1, "m")]);
    assert_eq!(last(&both.streams[0]).lo().to_string(), "$n");
    assert_eq!(last(&both.streams[1]).lo().to_string(), "$m");

    let same_name = analyze(source, &[(0, "n"), (1, "n")]);
    assert_eq!(last(&same_name.streams[0]).lo().to_string(), "$n");
    assert_eq!(last(&same_name.streams[1]).lo().to_string(), "$n");
}

#[test]
fn named_consumers_and_derived_variables_do_not_take_an_input_number() {
    let source = "struct D { x: i32 }\nrows = read_file(\"a.csv\", csv, struct=D).ok_or_panic()\nmore = rows.map(f)\nrows.map(f).return\nmore.map(f).return\nread_file(\"b.csv\", csv, struct=D).ok_or_panic().permutations(2).return";
    let second = analyze(source, &[(1, "m")]);
    assert_eq!(second.streams.len(), 5);
    for stream in &second.streams[..4] {
        assert_eq!(last(stream).kind(), ResolutionKind::Unbound);
    }
    assert_eq!(last(&second.streams[4]).lo().to_string(), "P($m, 2)");

    let first = analyze(source, &[(0, "n")]);
    for stream in &first.streams[..4] {
        assert_eq!(stream.stages[0].resolution.lo().to_string(), "$n");
    }
    assert_eq!(last(&first.streams[4]).kind(), ResolutionKind::Unbound);
    assert!(matches!(
        marigold_analyze_with_sizes(source, &sizes(&[(2, "x")])),
        Err(SizeAnalysisError::UnknownInput {
            ordinal: 2,
            inputs: 2
        })
    ));
}

#[test]
fn streams_and_their_stages_stay_aligned_when_a_variable_follows_an_unnamed_stream() {
    let source = "struct D { x: i32 }\nrange(0, 3).map(f).return\nrows = read_file(\"a.csv\", csv, struct=D).ok_or_panic()\nrows.map(f).return";
    let analysis = analyze(source, &[(1, "n")]);
    let described: Vec<(Option<&str>, usize, Vec<&str>)> = analysis
        .streams
        .iter()
        .map(|s| (s.name.as_deref(), s.expr_index, labels(s)))
        .collect();
    assert_eq!(
        described,
        vec![
            (None, 1, vec!["input", "map(...)"]),
            (Some("rows"), 2, vec!["input", "ok_or_panic()"]),
            (None, 3, vec!["input", "ok_or_panic()", "map(...)"]),
        ]
    );
    assert_eq!(last(&analysis.streams[0]).kind(), ResolutionKind::Exact);
    assert_eq!(last(&analysis.streams[1]).lo().to_string(), "$n");
    assert_eq!(last(&analysis.streams[2]).lo().to_string(), "$n");
}

#[test]
fn a_write_file_stream_has_an_input_that_can_be_bound() {
    let source = "struct D { x: i32 }\nread_file(\"a.csv\", csv, struct=D).ok_or_panic().write_file(\"/dev/stdout\", csv)";
    let analysis = analyze(source, &[(0, "n")]);
    assert_eq!(last(&analysis.streams[0]).lo().to_string(), "$n");
    assert_eq!(
        analysis.streams[0].description,
        "input.ok_or_panic().write_file(...)"
    );
}

#[test]
fn a_select_all_is_one_input_whose_total_size_is_bound() {
    let source = "struct D { x: i32 }\nselect_all(range(0, 5), read_file(\"d.csv\", csv, struct=D).ok_or_panic()).return";
    let analysis = analyze(source, &[(0, "total")]);
    assert_eq!(last(&analysis.streams[0]).lo().to_string(), "$total");
    assert!(matches!(
        marigold_analyze_with_sizes(source, &sizes(&[(1, "x")])),
        Err(SizeAnalysisError::UnknownInput {
            ordinal: 1,
            inputs: 1
        })
    ));
}

#[test]
fn binding_a_missing_or_known_size_input_is_an_error() {
    assert!(matches!(
        marigold_analyze_with_sizes(PERMUTATIONS, &sizes(&[(5, "n")])),
        Err(SizeAnalysisError::UnknownInput {
            ordinal: 5,
            inputs: 1
        })
    ));
    assert!(matches!(
        marigold_analyze_with_sizes("range(0, 5).return", &sizes(&[(0, "n")])),
        Err(SizeAnalysisError::InputSizeKnown { ordinal: 0 })
    ));
    assert!(matches!(
        marigold_analyze_with_sizes("enum C { A, B }\nrange(C).return", &sizes(&[(0, "n")])),
        Err(SizeAnalysisError::InputSizeKnown { ordinal: 0 })
    ));
    let known_then_unknown = "struct D { x: i32 }\nrange(0, 4).return\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().return";
    assert!(matches!(
        marigold_analyze_with_sizes(known_then_unknown, &sizes(&[(0, "n")])),
        Err(SizeAnalysisError::InputSizeKnown { ordinal: 0 })
    ));
    assert!(marigold_analyze_with_sizes(known_then_unknown, &sizes(&[(1, "n")])).is_ok());
    let unknown_then_known = "struct D { x: i32 }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().return\nrange(0, 4).return";
    assert!(matches!(
        marigold_analyze_with_sizes(unknown_then_known, &sizes(&[(1, "n")])),
        Err(SizeAnalysisError::InputSizeKnown { ordinal: 1 })
    ));
}

#[test]
fn param_names_must_be_identifiers() {
    for name in [
        "",
        "1n",
        "a-b",
        "a b",
        "$n",
        "n!",
        "\u{00e9}",
        "a\u{00e9}",
        "n\u{0663}",
    ] {
        assert!(
            matches!(
                InputSizes::new().bind(0, name),
                Err(SizeAnalysisError::InvalidParamName(ref bad)) if bad == name
            ),
            "{name:?} should be rejected"
        );
    }
    for name in ["n", "_", "rows_2", "N", "x1"] {
        assert!(InputSizes::new().bind(0, name).is_ok(), "{name:?}");
    }
}

#[test]
fn binding_the_same_input_again_replaces_the_name() {
    let source = PERMUTATIONS;
    let replaced = InputSizes::new()
        .bind(0, "a")
        .unwrap()
        .bind(0, "b")
        .unwrap();
    let analysis = marigold_analyze_with_sizes(source, &replaced).unwrap();
    assert_eq!(last(&analysis.streams[0]).lo().to_string(), "P($b, 3)");
}

#[test]
fn parse_errors_keep_their_default_text() {
    let error = marigold_analyze_with_sizes("range(0, N).return", &InputSizes::new())
        .map(|_| ())
        .expect_err("an identifier range bound does not parse");
    assert!(matches!(error, SizeAnalysisError::Parse(_)));
    assert!(error.to_string().contains("Could not parse range end"));
}

#[test]
fn a_bound_programs_json_round_trips_through_the_program_complexity_type() {
    use marigold_grammar::complexity::ProgramComplexity;

    for source in [PERMUTATIONS, "struct D { x: i32 }\nfn keep(d: &D) -> bool { true }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().filter(keep).permutations(2).return"] {
        let analysis = analyze(source, &[(0, "n")]);
        let text = serde_json::to_string(&analysis.program).unwrap();
        let reread: ProgramComplexity = serde_json::from_str(&text).unwrap();
        assert_eq!(serde_json::to_string(&reread).unwrap(), text);
    }
    let analysis = analyze(PERMUTATIONS, &[(0, "n")]);
    let text = serde_json::to_string(&analysis.program).unwrap();
    assert!(text.contains("P($n, 3)"), "{text}");
}

#[test]
fn the_analysis_serializes_with_a_stable_shape() {
    let analysis = analyze("range(0, 3).map(f).return", &[]);
    let value = serde_json::to_value(&analysis).unwrap();
    assert_eq!(
        value["streams"],
        json!([{
            "expr_index": 0,
            "program_stream": 0,
            "name": null,
            "description": "input.map(...).return",
            "stages": [
                {"stage": "input", "resolution": {"kind": "exact", "lo": "3", "hi": "3", "reasons": []}},
                {"stage": "map(...)", "resolution": {"kind": "exact", "lo": "3", "hi": "3", "reasons": []}}
            ]
        }])
    );
    assert_eq!(
        value["program"],
        serde_json::to_value(marigold_analyze("range(0, 3).map(f).return").unwrap()).unwrap()
    );
}

fn marigold_files(relative: &str) -> Vec<PathBuf> {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join(relative);
    let mut files: Vec<PathBuf> = fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("could not read {}: {e}", dir.display()))
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().and_then(|e| e.to_str()) == Some("marigold"))
        .collect();
    files.sort();
    files
}

#[test]
fn analysis_without_bindings_equals_the_default_analysis_on_every_pinned_program() {
    let mut checked = 0;
    for relative in [
        "tests/programs",
        "../examples/algorithms/programs",
        "../examples",
    ] {
        for path in marigold_files(relative) {
            let source = fs::read_to_string(&path).unwrap();
            let default = marigold_analyze(&source).map(|p| serde_json::to_string(&p).unwrap());
            let sized = marigold_analyze_with_sizes(&source, &InputSizes::new())
                .map(|a| serde_json::to_string(&a.program).unwrap());
            match (default, sized) {
                (Ok(a), Ok(b)) => assert_eq!(a, b, "{}", path.display()),
                (Err(a), Err(b)) => assert_eq!(a.to_string(), b.to_string()),
                (a, b) => panic!("{}: {:?} vs {:?}", path.display(), a.is_ok(), b.is_ok()),
            }
            checked += 1;
        }
    }
    assert!(checked >= 48, "only {checked} programs were compared");
}

#[test]
fn program_complexity_with_a_bound_input_survives_a_json_round_trip() {
    let prelude = "struct D { x: i32 }\nfn cmp(a: &D, b: &D) -> Ordering { Ordering::Equal }\n";
    let bodies = [
        "read_file(\"d.csv\", csv, struct=D).ok_or_panic().keep_first_n(5, cmp).permutations_with_replacement(2).return",
        "read_file(\"d.csv\", csv, struct=D).ok_or_panic().permutations(2).permutations_with_replacement(2).return",
        "read_file(\"d.csv\", csv, struct=D).ok_or_panic().combinations(2).permutations_with_replacement(2).return",
        "read_file(\"d.csv\", csv, struct=D).ok().permutations_with_replacement(2).permutations_with_replacement(2).return",
    ];
    for body in bodies {
        let source = format!("{prelude}{body}");
        let analysis = analyze(&source, &[(0, "n")]);
        let text = serde_json::to_string(&analysis.program).unwrap();
        let parsed: marigold_grammar::complexity::ProgramComplexity =
            serde_json::from_str(&text).unwrap_or_else(|e| panic!("{body}: {e}"));
        assert_eq!(serde_json::to_string(&parsed).unwrap(), text, "{body}");
    }
}
