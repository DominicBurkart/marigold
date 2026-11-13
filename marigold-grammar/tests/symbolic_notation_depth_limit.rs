use std::str::FromStr;
use std::time::{Duration, Instant};

use marigold_grammar::complexity::{Cardinality, ProgramComplexity, Symbolic};

const LIMIT: usize = Symbolic::MAX_NOTATION_DEPTH;
const SMALL_STACK: usize = 2 << 20;

fn on_small_stack(body: fn()) {
    std::thread::Builder::new()
        .stack_size(SMALL_STACK)
        .spawn(body)
        .expect("spawn a 2 MiB thread")
        .join()
        .expect("the check panicked");
}

#[test]
fn the_limit_keeps_at_least_half_of_the_measured_stack_headroom() {
    let shallowest_measured_overflow = 40;
    assert_eq!(LIMIT, 20);
    assert!(2 * LIMIT <= shallowest_measured_overflow);
    on_small_stack(|| {
        let at_limit = format!("{}1{}", "min(1 + ".repeat(20), ", 2)".repeat(20));
        assert!(Symbolic::from_str(&at_limit).is_ok());
        let past_limit = format!("{}1{}", "min(1 + ".repeat(21), ", 2)".repeat(21));
        assert!(Symbolic::from_str(&past_limit).is_err());
    });
}

fn shapes(depth: usize) -> Vec<(&'static str, String)> {
    let pairs = "C(\u{2264}".repeat(depth / 2);
    let mixed_prefix = if depth % 2 == 1 { "\u{2264}" } else { "" };
    vec![
        (
            "min",
            format!("{}1{}", "min(".repeat(depth), ", 1)".repeat(depth)),
        ),
        (
            "min second argument",
            format!("{}$n{}", "min(1, ".repeat(depth), ")".repeat(depth)),
        ),
        (
            "P",
            format!("{}$n{}", "P(".repeat(depth), ", 2)".repeat(depth)),
        ),
        (
            "C",
            format!("{}$n{}", "C(".repeat(depth), ", 3)".repeat(depth)),
        ),
        (
            "power",
            format!("{}$n{}", "P(".repeat(depth), ", 2)^2^3".repeat(depth)),
        ),
        (
            "sum",
            format!("{}$n{}", "min(1 + ".repeat(depth), ", 3 + 4)".repeat(depth)),
        ),
        ("filtered", format!("{}$n", "\u{2264}".repeat(depth))),
        (
            "filtered and combined",
            format!("{mixed_prefix}{pairs}$n{}", ", 2)^2".repeat(depth / 2)),
        ),
    ]
}

#[test]
fn every_shape_parses_at_the_limit_and_round_trips() {
    on_small_stack(|| {
        for (name, text) in shapes(LIMIT) {
            let parsed = Symbolic::from_str(&text).unwrap_or_else(|e| panic!("{name}: {e}"));
            assert_eq!(parsed.to_string(), text, "{name}");
            assert_eq!(
                Cardinality::from_str(&text).unwrap(),
                Cardinality::Bounded(parsed),
                "{name}"
            );
        }
    });
}

#[test]
fn every_shape_one_level_past_the_limit_is_an_error() {
    on_small_stack(|| {
        for (name, text) in shapes(LIMIT + 1) {
            let error = Symbolic::from_str(&text).expect_err(name);
            assert!(
                error.contains(&format!("notation nested more than {LIMIT} levels")),
                "{name}: {error}"
            );
            assert!(Cardinality::from_str(&text).is_err(), "{name}");
        }
    });
}

#[test]
fn separators_end_the_filter_prefixes_of_a_term() {
    on_small_stack(|| {
        let filters = "\u{2264}".repeat(LIMIT);
        for text in [
            format!("{filters}1 + {filters}2"),
            format!(
                "min({}1, {}2)",
                "\u{2264}".repeat(LIMIT - 1),
                "\u{2264}".repeat(LIMIT - 1)
            ),
            format!(
                "{}1{} + {filters}3",
                "min(".repeat(LIMIT),
                ", 2)".repeat(LIMIT)
            ),
        ] {
            assert!(Symbolic::from_str(&text).is_ok(), "{text}");
        }
        assert!(Symbolic::from_str(&format!("min({filters}1, 2)")).is_err());
    });
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
        let mut value = serde_json::to_value(&program).unwrap();
        value["program_cardinality"] =
            serde_json::Value::String(format!("{}1", "\u{2264}".repeat(100_000)));
        let error = serde_json::from_value::<ProgramComplexity>(value)
            .expect_err("a hostile cardinality is rejected");
        assert!(error.to_string().contains("nested more than"), "{error}");
    });
}
