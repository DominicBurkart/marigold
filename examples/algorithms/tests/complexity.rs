use std::str::FromStr;

use marigold_grammar::complexity::{
    Cardinality, ComplexityClass, ExactComplexity, ProgramComplexity,
};

fn analyze_prefix(name: &str, through: Option<&str>) -> ProgramComplexity {
    let path = format!("{}/programs/{name}.marigold", env!("CARGO_MANIFEST_DIR"));
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    let (body, stream) = source
        .trim_end()
        .rsplit_once('\n')
        .expect("program has a final stream line");
    let end = match through {
        None => stream.find('.').expect("stream has a stage"),
        Some(stage) => {
            let start = stream
                .find(&format!(".{stage}("))
                .unwrap_or_else(|| panic!("no `{stage}` stage in {path}"));
            start + stream[start..].find(')').expect("stage closes") + 1
        }
    };
    marigold_grammar::marigold_analyze(&format!("{body}\n{}.return\n", &stream[..end]))
        .unwrap_or_else(|e| panic!("Failed to analyze prefix of {path}: {e}"))
}

fn analyze_example(name: &str) -> ProgramComplexity {
    let path = format!("{}/programs/{name}.marigold", env!("CARGO_MANIFEST_DIR"));
    let source =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    marigold_grammar::marigold_analyze(&source)
        .unwrap_or_else(|e| panic!("Failed to analyze {path}: {e}"))
}

#[test]
fn gcd() {
    let result = analyze_example("gcd");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn gcd_diverges_from_canonical_class() {
    let canonical = ComplexityClass::OLogN;
    let result = analyze_example("gcd");
    assert_ne!(result.streams[0].time_class, canonical);
}

#[test]
fn tsp_brute_force() {
    let result = analyze_example("tsp_brute_force");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(3)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(3)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(stream.collects_input);
}

#[test]
fn tsp_brute_force_canonical_class_differs() {
    let result = analyze_example("tsp_brute_force");
    assert_ne!(result.streams[0].time_class, ComplexityClass::OFactorial);
    assert_ne!(
        result.streams[0].time_class,
        ComplexityClass::OPermutational(9)
    );
    let permuted = analyze_prefix("tsp_brute_force", Some("permutations"));
    assert_eq!(
        permuted.streams[0].cardinality,
        Cardinality::Exact(362_880u32.into())
    );
}

#[test]
fn tsp_held_karp() {
    let result = analyze_example("tsp_held_karp");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn held_karp_canonical_class_is_not_representable() {
    let masks = analyze_prefix("tsp_held_karp", None);
    assert_eq!(
        masks.streams[0].cardinality,
        Cardinality::Exact(((1u32 << 9) - 1).into())
    );
    let result = analyze_example("tsp_held_karp");
    assert_eq!(result.streams[0].time_class, ComplexityClass::O1);
}

#[test]
fn knapsack() {
    let result = analyze_example("knapsack");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn knapsack_canonical_class_is_not_representable() {
    let items = analyze_prefix("knapsack", None);
    assert_eq!(
        items.streams[0].cardinality,
        Cardinality::Exact(14u32.into())
    );
    let folded = analyze_prefix("knapsack", Some("fold"));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
    let result = analyze_example("knapsack");
    assert_ne!(
        result.streams[0].time_class,
        ComplexityClass::OPolynomial(2)
    );
    assert_ne!(result.streams[0].space_class, ComplexityClass::ON);
}

#[test]
fn floyd_warshall() {
    let result = analyze_example("floyd_warshall");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(1)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(1)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn floyd_warshall_canonical_class_is_not_representable() {
    let vertices = analyze_prefix("floyd_warshall", None);
    assert_eq!(
        vertices.streams[0].cardinality,
        Cardinality::Exact(8u32.into())
    );
    let result = analyze_example("floyd_warshall");
    assert_ne!(
        result.streams[0].time_class,
        ComplexityClass::OPolynomial(3)
    );
    assert_ne!(
        result.streams[0].space_class,
        ComplexityClass::OPolynomial(2)
    );
}

#[test]
fn binary_search() {
    let result = analyze_example("binary_search");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn binary_search_canonical_class_is_not_representable() {
    let rounds = analyze_prefix("binary_search", None);
    assert_eq!(
        rounds.streams[0].cardinality,
        Cardinality::Exact(5u32.into())
    );
    let folded = analyze_prefix("binary_search", Some("fold"));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
    let result = analyze_example("binary_search");
    assert_ne!(result.streams[0].time_class, ComplexityClass::OLogN);
}

#[test]
fn quicksort() {
    let result = analyze_example("quicksort");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn quicksort_canonical_class_is_not_representable() {
    let rounds = analyze_prefix("quicksort", None);
    assert_eq!(
        rounds.streams[0].cardinality,
        Cardinality::Exact(15u32.into())
    );
    let folded = analyze_prefix("quicksort", Some("fold"));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
    let result = analyze_example("quicksort");
    assert_ne!(result.streams[0].time_class, ComplexityClass::ONLogN);
}

#[test]
fn dijkstra() {
    let result = analyze_example("dijkstra");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn dijkstra_canonical_class_is_not_representable() {
    let rounds = analyze_prefix("dijkstra", None);
    assert_eq!(
        rounds.streams[0].cardinality,
        Cardinality::Exact(9u32.into())
    );
    let folded = analyze_prefix("dijkstra", Some("fold"));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
    let result = analyze_example("dijkstra");
    assert_ne!(
        result.streams[0].time_class,
        ComplexityClass::OPolynomial(2)
    );
}

#[test]
fn tsp_simulated_annealing() {
    let result = analyze_example("tsp_simulated_annealing");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn tsp_simulated_annealing_canonical_class_is_not_representable() {
    let rounds = analyze_prefix("tsp_simulated_annealing", None);
    assert_eq!(
        rounds.streams[0].cardinality,
        Cardinality::Exact(2000u32.into())
    );
    let folded = analyze_prefix("tsp_simulated_annealing", Some("fold"));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
    let result = analyze_example("tsp_simulated_annealing");
    assert_ne!(result.streams[0].time_class, ComplexityClass::ON);
}

#[test]
fn tsp_genetic() {
    let result = analyze_example("tsp_genetic");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn tsp_genetic_canonical_class_is_not_representable() {
    let rounds = analyze_prefix("tsp_genetic", None);
    assert_eq!(
        rounds.streams[0].cardinality,
        Cardinality::Exact(150u32.into())
    );
    let folded = analyze_prefix("tsp_genetic", Some("fold"));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
    let result = analyze_example("tsp_genetic");
    assert_ne!(result.streams[0].time_class, ComplexityClass::ON);
}

#[test]
fn tsp_ant_colony() {
    let result = analyze_example("tsp_ant_colony");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_time,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(2)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(!stream.collects_input);
}

#[test]
fn tsp_ant_colony_canonical_class_is_not_representable() {
    let rounds = analyze_prefix("tsp_ant_colony", None);
    assert_eq!(
        rounds.streams[0].cardinality,
        Cardinality::Exact(200u32.into())
    );
    let folded = analyze_prefix("tsp_ant_colony", Some("fold"));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
    let result = analyze_example("tsp_ant_colony");
    assert_ne!(
        result.streams[0].time_class,
        ComplexityClass::OPolynomial(2)
    );
}

#[test]
fn kruskal() {
    let result = analyze_example("kruskal");
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.time_class, ComplexityClass::OCombinatorial(2));
    let mut expected_time = ExactComplexity::new();
    expected_time.add_work(ComplexityClass::O1, 3);
    expected_time.add_work(ComplexityClass::OCombinatorial(2), 2);
    assert_eq!(stream.exact_time, expected_time);
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str("O(5)").unwrap()
    );
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert!(stream.collects_input);
}

#[test]
fn kruskal_candidate_counts_are_pinned_per_stage() {
    let pairs = analyze_prefix("kruskal", Some("combinations"));
    assert_eq!(
        pairs.streams[0].cardinality,
        Cardinality::Exact(28u32.into())
    );
    let weighed = analyze_prefix("kruskal", Some("map"));
    assert_eq!(
        weighed.streams[0].cardinality,
        Cardinality::Exact(28u32.into())
    );
    let filtered = analyze_prefix("kruskal", Some("filter"));
    assert_eq!(
        filtered.streams[0].cardinality.to_string(),
        "\u{2264}C(8, 2)"
    );
    let sorted = analyze_prefix("kruskal", Some("keep_first_n"));
    assert_eq!(
        sorted.streams[0].cardinality.to_string(),
        "min(\u{2264}C(8, 2), 13)"
    );
    let folded = analyze_prefix("kruskal", Some("fold"));
    assert_eq!(
        folded.streams[0].cardinality,
        Cardinality::Exact(1u32.into())
    );
}

#[test]
fn kruskal_canonical_class_is_not_representable() {
    let result = analyze_example("kruskal");
    assert_ne!(result.streams[0].time_class, ComplexityClass::ONLogN);
}

fn assert_constant_fold_pipeline(name: &str, exact: &str, description: &str) {
    let result = analyze_example(name);
    assert_eq!(result.streams.len(), 1);
    let stream = &result.streams[0];
    assert_eq!(stream.description, description);
    assert_eq!(stream.cardinality, Cardinality::Exact(1u32.into()));
    assert_eq!(stream.time_class, ComplexityClass::O1);
    assert_eq!(stream.exact_time, ExactComplexity::from_str(exact).unwrap());
    assert_eq!(stream.space_class, ComplexityClass::O1);
    assert_eq!(
        stream.exact_space,
        ExactComplexity::from_str(exact).unwrap()
    );
    assert!(!stream.collects_input);
}

#[test]
fn tsp_nearest_neighbour() {
    assert_constant_fold_pipeline(
        "tsp_nearest_neighbour",
        "O(2)",
        "input.fold(...).map(...).return",
    );
}

#[test]
fn tsp_nearest_neighbour_diverges_from_canonical_class() {
    let canonical = ComplexityClass::OPolynomial(2);
    let result = analyze_example("tsp_nearest_neighbour");
    assert_ne!(result.streams[0].time_class, canonical);
}

#[test]
fn tsp_christofides() {
    assert_constant_fold_pipeline(
        "tsp_christofides",
        "O(5)",
        "input.fold(...).map(...).map(...).map(...).map(...).return",
    );
}

#[test]
fn tsp_christofides_diverges_from_canonical_class() {
    let canonical = ComplexityClass::OPolynomial(3);
    let result = analyze_example("tsp_christofides");
    assert_ne!(result.streams[0].time_class, canonical);
}

#[test]
fn tsp_lkh() {
    assert_constant_fold_pipeline("tsp_lkh", "O(2)", "input.fold(...).map(...).return");
}

#[test]
fn tsp_lkh_diverges_from_canonical_class() {
    let canonical = ComplexityClass::OPolynomial(2);
    let result = analyze_example("tsp_lkh");
    assert_ne!(result.streams[0].time_class, canonical);
}
