use std::str::FromStr;
use std::time::{Duration, Instant};

use marigold_grammar::complexity::{
    Cardinality, ProgramComplexity, SizeBindings, SizeName, Symbolic,
};
use num_bigint::BigUint;
use proptest::prelude::*;
use proptest::test_runner::{Config, TestRunner};

const LIMIT: usize = Symbolic::MAX_NOTATION_DEPTH;
const SMALL_STACK: usize = 2 << 20;

const DEADLINE: Duration = Duration::from_secs(60);

fn on_small_stack(body: fn()) {
    let (done, finished) = std::sync::mpsc::channel();
    let worker = std::thread::Builder::new()
        .stack_size(SMALL_STACK)
        .spawn(move || {
            body();
            let _ = done.send(());
        })
        .expect("spawn a 2 MiB thread");
    match finished.recv_timeout(DEADLINE) {
        Ok(()) => worker.join().expect("the check panicked"),
        Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
            panic!("the check did not finish within {DEADLINE:?}")
        }
        Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
            let _ = worker.join();
            panic!("the check panicked")
        }
    }
}

fn children(expression: &Symbolic) -> Vec<&Symbolic> {
    match expression {
        Symbolic::Constant(_) | Symbolic::Param(_) | Symbolic::Unknown => vec![],
        Symbolic::Filtered(n)
        | Symbolic::Permutations { n, .. }
        | Symbolic::PermutationsWithReplacement { n, .. }
        | Symbolic::Combinations { n, .. } => vec![n],
        Symbolic::Min(a, b) => vec![a, b],
        Symbolic::Sum(parts) => parts.iter().collect(),
        _ => panic!("a new Symbolic variant needs a rule in children"),
    }
}

fn node_cost(expression: &Symbolic) -> usize {
    match expression {
        Symbolic::Min(..) | Symbolic::Permutations { .. } | Symbolic::Combinations { .. } => 3,
        Symbolic::Filtered(_) => 1,
        _ => 0,
    }
}

fn height_and_cost(root: &Symbolic) -> (usize, usize) {
    let mut deepest = (0, 0);
    let mut pending = vec![(root, 0usize, node_cost(root))];
    while let Some((node, depth, cost)) = pending.pop() {
        deepest = (deepest.0.max(depth), deepest.1.max(cost));
        for child in children(node) {
            pending.push((child, depth + 1, cost + node_cost(child)));
        }
    }
    deepest
}

fn exercise(parsed: &Symbolic) {
    let bindings = SizeBindings::from([(SizeName::new("n").unwrap(), BigUint::from(3u32))]);
    let text = parsed.to_string();
    assert!(!text.is_empty());
    let _ = parsed.try_evaluate();
    let _ = parsed.upper_bound_with(&bindings);
    let copy = parsed.clone();
    assert_eq!(&copy, parsed);
    drop(copy);
}

fn shapes(depth: usize) -> Vec<(&'static str, usize, String)> {
    vec![
        (
            "min",
            20,
            format!("{}1{}", "min(".repeat(depth), ", 1)".repeat(depth)),
        ),
        (
            "min second argument",
            20,
            format!("{}$n{}", "min(1, ".repeat(depth), ")".repeat(depth)),
        ),
        (
            "P",
            20,
            format!("{}$n{}", "P(".repeat(depth), ", 2)".repeat(depth)),
        ),
        (
            "C",
            20,
            format!("{}$n{}", "C(".repeat(depth), ", 3)".repeat(depth)),
        ),
        (
            "power after a parenthesis",
            20,
            format!("{}$n{}", "P(".repeat(depth), ", 2)^2^3".repeat(depth)),
        ),
        (
            "sum",
            20,
            format!("{}$n{}", "min(1 + ".repeat(depth), ", 3 + 4)".repeat(depth)),
        ),
        ("filtered", 60, format!("{}$n", "\u{2264}".repeat(depth))),
        ("power chain", 60, format!("1{}", "^1".repeat(depth))),
        (
            "filtered then powered",
            60,
            format!(
                "{}2{}",
                "\u{2264}".repeat(depth / 2),
                "^2".repeat(depth - depth / 2)
            ),
        ),
        (
            "filtered combinations",
            15,
            format!("{}$n{}", "C(\u{2264}".repeat(depth), ", 2)^2".repeat(depth)),
        ),
    ]
}

#[test]
fn every_shape_parses_at_its_limit_and_fails_one_past_it() {
    on_small_stack(|| {
        for (name, limit, _) in shapes(0) {
            let (_, _, at_limit) = shapes(limit).into_iter().find(|s| s.0 == name).unwrap();
            let parsed = Symbolic::from_str(&at_limit).unwrap_or_else(|e| panic!("{name}: {e}"));
            assert_eq!(parsed.to_string(), at_limit, "{name}");
            let (height, cost) = height_and_cost(&parsed);
            assert!(height <= LIMIT && cost <= LIMIT, "{name}: {height} {cost}");
            assert!(
                height == LIMIT || cost >= LIMIT - 3,
                "{name} is not at its limit: height {height}, cost {cost}"
            );
            exercise(&parsed);
            assert_eq!(
                Cardinality::from_str(&at_limit).unwrap(),
                Cardinality::Bounded(parsed),
                "{name}"
            );

            let (_, _, past) = shapes(limit + 1).into_iter().find(|s| s.0 == name).unwrap();
            let error = Symbolic::from_str(&past).expect_err(name);
            assert!(
                error.contains(&format!("notation nested more than {LIMIT} levels")),
                "{name}: {error}"
            );
            assert!(Cardinality::from_str(&past).is_err(), "{name}");
        }
    });
}

#[test]
fn a_power_after_a_closed_parenthesis_sits_on_top_of_its_contents() {
    on_small_stack(|| {
        let base = "P(P(1, 2)^2, 2)^2";
        let parsed = Symbolic::from_str(base).unwrap();
        assert_eq!(height_and_cost(&parsed).0, 4);
        let at_limit = format!("{base}{}", "^2".repeat(LIMIT - 4));
        assert_eq!(
            height_and_cost(&Symbolic::from_str(&at_limit).unwrap()).0,
            LIMIT
        );
        assert!(Symbolic::from_str(&format!("{at_limit}^2")).is_err());
        let summed = format!("{} + 1", at_limit);
        assert!(Symbolic::from_str(&summed).is_err());
        let inside = format!("min({}, 1)", "1".to_string() + &"^2".repeat(LIMIT - 1));
        assert_eq!(
            height_and_cost(&Symbolic::from_str(&inside).unwrap()).0,
            LIMIT
        );
        assert!(Symbolic::from_str(&format!("{inside}^2")).is_err());
    });
}

#[test]
fn separators_end_the_filter_prefixes_of_a_term() {
    on_small_stack(|| {
        let filters = "\u{2264}".repeat(LIMIT - 1);
        for text in [
            format!("{filters}1 + {filters}2"),
            format!(
                "min({}1, {}2)",
                "\u{2264}".repeat(LIMIT - 3),
                "\u{2264}".repeat(LIMIT - 3)
            ),
        ] {
            let parsed = Symbolic::from_str(&text).unwrap_or_else(|e| panic!("{text}: {e}"));
            exercise(&parsed);
        }
        assert!(Symbolic::from_str(&format!("min({}1, 2)", "\u{2264}".repeat(LIMIT - 2))).is_err());
    });
}

#[test]
fn the_limit_keeps_at_least_half_of_the_measured_stack_headroom() {
    let shallowest_parenthesis_overflow = 40;
    let shallowest_filter_overflow = 126;
    let shallowest_display_overflow = 2_000;
    assert_eq!(LIMIT, 60);
    assert!(2 * (LIMIT / 3) <= shallowest_parenthesis_overflow);
    assert!(2 * LIMIT <= shallowest_filter_overflow);
    assert!(2 * LIMIT <= shallowest_display_overflow);
}

#[test]
fn a_hundred_thousand_levels_are_rejected_quickly_without_overflowing() {
    on_small_stack(|| {
        let levels = 100_000;
        for text in [
            format!("{}1{}", "min(".repeat(levels), ", 1)".repeat(levels)),
            format!("{}$n", "\u{2264}".repeat(levels)),
            "(".repeat(levels),
            format!("{}1", "C(\u{2264}".repeat(levels)),
            format!("1{}", "^1".repeat(levels)),
            format!("min(1, 2){}", "^2".repeat(levels)),
        ] {
            let start = Instant::now();
            assert!(Symbolic::from_str(&text).is_err());
            assert!(Cardinality::from_str(&text).is_err());
            let json = serde_json::to_string(&text).unwrap();
            assert!(serde_json::from_str::<Cardinality>(&json).is_err());
            assert!(
                start.elapsed() < Duration::from_secs(1),
                "{:?}",
                start.elapsed()
            );
        }
    });
}

#[test]
fn a_program_complexity_with_a_hostile_cardinality_fails_to_deserialize() {
    on_small_stack(|| {
        let program = marigold_grammar::marigold_analyze("range(0, 3).return").unwrap();
        for hostile in [
            format!("{}1", "\u{2264}".repeat(100_000)),
            format!("1{}", "^1".repeat(100_000)),
        ] {
            let mut value = serde_json::to_value(&program).unwrap();
            value["program_cardinality"] = serde_json::Value::String(hostile);
            let error = serde_json::from_value::<ProgramComplexity>(value)
                .expect_err("a hostile cardinality is rejected");
            assert!(error.to_string().contains("nested more than"), "{error}");
        }
    });
}

fn filtered_program(filters: usize) -> String {
    format!("range(0, 5){}.return", ".filter(f)".repeat(filters))
}

#[test]
fn default_output_past_the_limit_is_unchanged_but_does_not_parse_back() {
    on_small_stack(|| {
        for filters in [21, LIMIT] {
            let program = marigold_grammar::marigold_analyze(&filtered_program(filters)).unwrap();
            let text = serde_json::to_string(&program).unwrap();
            let reread: ProgramComplexity = serde_json::from_str(&text).unwrap();
            assert_eq!(serde_json::to_string(&reread).unwrap(), text);
        }
        let program = marigold_grammar::marigold_analyze(&filtered_program(LIMIT + 1)).unwrap();
        let printed = program.program_cardinality.to_string();
        assert_eq!(printed, format!("{}5", "\u{2264}".repeat(LIMIT + 1)));
        assert!(Cardinality::from_str(&printed).is_err());
        let text = serde_json::to_string(&program).unwrap();
        assert!(serde_json::from_str::<ProgramComplexity>(&text).is_err());
    });
}

fn leaf() -> BoxedStrategy<Symbolic> {
    prop_oneof![
        (0u64..20).prop_map(|v| Symbolic::Constant(BigUint::from(v))),
        Just(Symbolic::Param(SizeName::new("n").unwrap())),
        Just(Symbolic::Unknown),
    ]
    .boxed()
}

fn wrap(inner: Symbolic, step: u8, other: Symbolic) -> Symbolic {
    let ambiguous_base = matches!(inner, Symbolic::Filtered(_) | Symbolic::Sum(_));
    match step % 6 {
        0 if !matches!(inner, Symbolic::Sum(_)) => Symbolic::Filtered(Box::new(inner)),
        1 if !ambiguous_base => Symbolic::PermutationsWithReplacement {
            n: Box::new(inner),
            k: 2,
        },
        2 => Symbolic::Combinations {
            n: Box::new(inner),
            k: 2,
        },
        3 => Symbolic::Min(Box::new(other), Box::new(inner)),
        4 if !matches!(inner, Symbolic::Sum(_)) => Symbolic::Sum(vec![inner, other]),
        _ => Symbolic::Permutations {
            n: Box::new(inner),
            k: 1,
        },
    }
}

fn chained_tree() -> impl Strategy<Value = Symbolic> {
    (leaf(), prop::collection::vec((any::<u8>(), leaf()), 0..80)).prop_map(|(start, steps)| {
        steps
            .into_iter()
            .fold(start, |tree, (step, other)| wrap(tree, step, other))
    })
}

const TOKENS: &[&str] = &[
    "min(",
    "P(",
    "C(",
    ", ",
    ", 2)",
    ", 1)",
    ")",
    ")^2",
    "^2",
    "^1^1^1",
    "\u{2264}",
    "\u{2264}\u{2264}\u{2264}",
    "1",
    "$n",
    "?",
    " + ",
    "3",
];

fn notation_string() -> impl Strategy<Value = String> {
    prop::collection::vec((0..TOKENS.len(), 1usize..30), 1..40).prop_map(|parts| {
        parts
            .into_iter()
            .map(|(token, repeat)| TOKENS[token].repeat(if repeat > 3 { 1 } else { repeat * 10 }))
            .collect()
    })
}

#[test]
fn every_accepted_string_builds_a_tree_within_the_documented_bound() {
    on_small_stack(|| {
        let mut runner = TestRunner::new(Config::with_cases(3000));
        runner
            .run(&notation_string(), |text| {
                if let Ok(parsed) = Symbolic::from_str(&text) {
                    let (height, cost) = height_and_cost(&parsed);
                    prop_assert!(height <= LIMIT, "{} is {} deep", text, height);
                    prop_assert!(cost <= LIMIT, "{} costs {}", text, cost);
                    exercise(&parsed);
                }
                Ok(())
            })
            .unwrap();
    });
}

#[test]
fn a_printed_tree_parses_exactly_when_it_is_within_the_limit() {
    on_small_stack(|| {
        let mut runner = TestRunner::new(Config::with_cases(3000));
        runner
            .run(&chained_tree(), |tree| {
                let (height, cost) = height_and_cost(&tree);
                let text = tree.to_string();
                let parsed = Symbolic::from_str(&text);
                prop_assert_eq!(
                    parsed.is_ok(),
                    height <= LIMIT && cost <= LIMIT,
                    "{} (height {}, cost {})",
                    text,
                    height,
                    cost
                );
                if let Ok(parsed) = parsed {
                    prop_assert_eq!(&parsed, &tree);
                    exercise(&parsed);
                }
                Ok(())
            })
            .unwrap();
    });
}

#[test]
fn counts_too_large_to_compute_evaluate_to_none_quickly() {
    on_small_stack(|| {
        let start = Instant::now();
        for text in [
            format!("9{}", "^9".repeat(LIMIT)),
            "2^5000000000".to_string(),
            "P(1000000000000, 4000000000)".to_string(),
            "C(P(1000000000000, 3)^9^9^9^9, 9)".to_string(),
        ] {
            let parsed = Symbolic::from_str(&text).unwrap_or_else(|e| panic!("{text}: {e}"));
            assert_eq!(parsed.try_evaluate(), None, "{text}");
            assert_eq!(
                parsed.upper_bound_with(&SizeBindings::new()),
                None,
                "{text}"
            );
        }
        assert_eq!(
            Symbolic::from_str("C(5, 4000000000)")
                .unwrap()
                .try_evaluate(),
            Some(BigUint::from(0u32))
        );
        assert_eq!(
            Symbolic::from_str("3^2^2").unwrap().try_evaluate(),
            Some(BigUint::from(81u32))
        );
        assert_eq!(
            Symbolic::from_str("2^1048576").unwrap().try_evaluate(),
            None
        );
        assert_eq!(
            Symbolic::from_str("2^524288")
                .unwrap()
                .try_evaluate()
                .map(|v| v.bits()),
            Some(524289)
        );
        assert!(
            start.elapsed() < Duration::from_secs(5),
            "{:?}",
            start.elapsed()
        );
    });
}

#[test]
fn trivial_bases_and_impossible_choices_skip_the_size_estimate() {
    on_small_stack(|| {
        let start = Instant::now();
        for (text, value) in [
            ("1^2000000", 1u32),
            ("0^5000000000", 0),
            ("1^5000000000", 1),
            ("C(3, 2000000)", 0),
            ("P(3, 2000000)", 0),
            ("C(1000000000000, 0)", 1),
        ] {
            assert_eq!(
                Symbolic::from_str(text).unwrap().try_evaluate(),
                Some(BigUint::from(value)),
                "{text}"
            );
        }
        let program = marigold_grammar::marigold_analyze(
            "range(0, 1).permutations_with_replacement(2000000).return",
        )
        .unwrap();
        assert_eq!(
            program.program_cardinality,
            Cardinality::Exact(BigUint::from(1u32))
        );
        assert!(
            start.elapsed() < Duration::from_secs(5),
            "{:?}",
            start.elapsed()
        );
    });
}

#[test]
fn a_combination_is_computed_from_the_smaller_side() {
    on_small_stack(|| {
        let start = Instant::now();
        let near_all = Symbolic::from_str("C(100000, 99990)")
            .unwrap()
            .try_evaluate();
        let few = Symbolic::from_str("C(100000, 10)").unwrap().try_evaluate();
        assert!(near_all.is_some());
        assert_eq!(near_all, few);
        assert_eq!(near_all.unwrap().to_string().len(), 44);
        assert!(Symbolic::from_str("C(123361, 61680)")
            .unwrap()
            .try_evaluate()
            .is_some());
        assert_eq!(
            Symbolic::from_str("C(123362, 61681)")
                .unwrap()
                .try_evaluate(),
            None
        );
        let program =
            marigold_grammar::marigold_analyze("range(0, 100000).combinations(99990).return")
                .unwrap();
        assert!(matches!(program.program_cardinality, Cardinality::Exact(_)));
        assert!(
            start.elapsed() < Duration::from_secs(30),
            "{:?}",
            start.elapsed()
        );
    });
}

fn classes(source: &str) -> Vec<String> {
    let program = marigold_grammar::marigold_analyze(source).unwrap();
    let mut out = vec![
        serde_json::to_string(&program.program_time).unwrap(),
        serde_json::to_string(&program.program_exact_time).unwrap(),
        serde_json::to_string(&program.program_space).unwrap(),
        serde_json::to_string(&program.program_exact_space).unwrap(),
    ];
    for stream in &program.streams {
        out.push(serde_json::to_string(&stream.time_class).unwrap());
        out.push(serde_json::to_string(&stream.exact_time).unwrap());
        out.push(serde_json::to_string(&stream.space_class).unwrap());
        out.push(serde_json::to_string(&stream.exact_space).unwrap());
        out.push(stream.collects_input.to_string());
    }
    out
}

#[test]
fn capped_constant_programs_keep_the_default_cost_classes() {
    on_small_stack(|| {
        for (source, exact) in [
            ("range(0, 1000000).permutations(52429).map(f).return", "\"O(2)\""),
            (
                "range(0, 1000000).permutations_with_replacement(300).permutations_with_replacement(300).map(f).return",
                "\"O(3)\"",
            ),
            (
                "range(0, 1000000).permutations(52429).permutations(2).map(f).return",
                "\"O(3)\"",
            ),
        ] {
            let found = classes(source);
            assert_eq!(
                found[..4],
                ["\"O(1)\"", exact, "\"O(1)\"", exact],
                "{source}"
            );
            assert_eq!(found[4..8], ["\"O(1)\"", exact, "\"O(1)\"", exact], "{source}");
            let program = marigold_grammar::marigold_analyze(source).unwrap();
            assert!(
                matches!(program.program_cardinality, Cardinality::Bounded(_)),
                "{source}"
            );
        }
    });
}

#[test]
fn a_capped_constant_program_has_the_classes_of_the_same_program_with_small_constants() {
    on_small_stack(|| {
        for (capped, small) in [
            (
                "range(0, 1000000).permutations(52429).map(f).return",
                "range(0, 10).permutations(3).map(f).return",
            ),
            (
                "range(0, 1000000).permutations_with_replacement(300).permutations_with_replacement(300).map(f).return",
                "range(0, 10).permutations_with_replacement(3).permutations_with_replacement(3).map(f).return",
            ),
            (
                "range(0, 1000000).combinations(300).combinations(300).map(f).return",
                "range(0, 10).combinations(3).combinations(3).map(f).return",
            ),
            (
                "range(0, 10).permutations_with_replacement(1000).permutations(1000).map(f).return",
                "range(0, 10).permutations_with_replacement(2).permutations(2).map(f).return",
            ),
            (
                "range(0, 1000000).permutations(52429).permutations(2).map(f).return",
                "range(0, 10).permutations(3).permutations(2).map(f).return",
            ),
            (
                "range(0, 1000000).permutations(52429).combinations(2).return",
                "range(0, 10).permutations(3).combinations(2).return",
            ),
            (
                "range(0, 1000000).permutations(52429).permutations_with_replacement(2).map(f).return",
                "range(0, 10).permutations(3).permutations_with_replacement(2).map(f).return",
            ),
        ] {
            assert_eq!(classes(capped), classes(small), "{capped}");
        }
    });
}

#[test]
fn filtered_or_unknown_counts_are_not_constant() {
    on_small_stack(|| {
        let o1 = "\"O(1)\"".to_string();
        for source in [
            "range(0, 10).filter(f).permutations(2).map(f).return",
            "range(0, 1000000).filter(f).permutations(52429).map(f).return",
            "struct D { x: i32 }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().permutations(2).map(f).return",
        ] {
            assert_ne!(classes(source)[0], o1, "{source}");
        }
        assert_eq!(
            classes("range(0, 1000000).filter(f).permutations(52429).map(f).return")[4..],
            classes("range(0, 10).filter(f).permutations(52429).map(f).return")[4..]
        );
    });
}
