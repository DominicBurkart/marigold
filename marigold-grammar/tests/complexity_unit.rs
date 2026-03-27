//! Deterministic unit tests for ComplexityClass, ExactComplexity,
//! and Cardinality that complement the existing proptests.

use std::str::FromStr;

use marigold_grammar::complexity::{
    Cardinality, ComplexityClass, ExactComplexity,
};
use num_bigint::BigUint;

// -- ComplexityClass ordering -----------------------------------------------

#[test]
fn ordering_o1_is_smallest() {
    assert!(ComplexityClass::O1 < ComplexityClass::OLogN);
    assert!(ComplexityClass::O1 < ComplexityClass::ON);
    assert!(ComplexityClass::O1 < ComplexityClass::OFactorial);
    assert!(ComplexityClass::O1 < ComplexityClass::Unknown);
}

#[test]
fn ordering_chain() {
    let chain = vec![
        ComplexityClass::O1,
        ComplexityClass::OLogN,
        ComplexityClass::ON,
        ComplexityClass::ONLogN,
        ComplexityClass::OPolynomial(2),
        ComplexityClass::OPolynomial(3),
        ComplexityClass::OFactorial,
        ComplexityClass::Unknown,
    ];
    for w in chain.windows(2) {
        assert!(
            w[0] < w[1],
            "{:?} should be less than {:?}",
            w[0],
            w[1]
        );
    }
}

#[test]
fn ordering_polynomial_by_degree() {
    assert!(ComplexityClass::OPolynomial(2) < ComplexityClass::OPolynomial(3));
    assert!(ComplexityClass::OPolynomial(3) < ComplexityClass::OPolynomial(10));
}

#[test]
fn ordering_combinatorial_by_k() {
    assert!(ComplexityClass::OCombinatorial(2) < ComplexityClass::OCombinatorial(5));
}

#[test]
fn ordering_permutational_by_k() {
    assert!(ComplexityClass::OPermutational(2) < ComplexityClass::OPermutational(5));
}

#[test]
fn ordering_combinatorial_before_permutational() {
    assert!(ComplexityClass::OCombinatorial(3) < ComplexityClass::OPermutational(3));
}

#[test]
fn max_returns_larger() {
    assert_eq!(
        ComplexityClass::O1.max(ComplexityClass::ON),
        ComplexityClass::ON
    );
    assert_eq!(
        ComplexityClass::OFactorial.max(ComplexityClass::ON),
        ComplexityClass::OFactorial
    );
}

#[test]
fn equality_reflexive() {
    let classes = vec![
        ComplexityClass::O1,
        ComplexityClass::OLogN,
        ComplexityClass::ON,
        ComplexityClass::ONLogK(5),
        ComplexityClass::ONLogN,
        ComplexityClass::OPolynomial(3),
        ComplexityClass::OCombinatorial(4),
        ComplexityClass::OPermutational(2),
        ComplexityClass::OFactorial,
        ComplexityClass::Unknown,
    ];
    for c in &classes {
        assert_eq!(c, c);
    }
}

// -- ComplexityClass Display/FromStr roundtrip --------------------------------

#[test]
fn display_fromstr_roundtrip_all_variants() {
    let classes = vec![
        ComplexityClass::O1,
        ComplexityClass::OLogN,
        ComplexityClass::ON,
        ComplexityClass::ONLogK(7),
        ComplexityClass::ONLogN,
        ComplexityClass::OPolynomial(4),
        ComplexityClass::OCombinatorial(3),
        ComplexityClass::OPermutational(5),
        ComplexityClass::OFactorial,
        ComplexityClass::Unknown,
    ];
    for c in classes {
        let s = c.to_string();
        let parsed = ComplexityClass::from_str(&s)
            .unwrap_or_else(|e| panic!("Failed to parse '{}': {}", s, e));
        assert_eq!(c, parsed, "roundtrip failed for '{}'", s);
    }
}

#[test]
fn display_format_spot_check() {
    assert_eq!(ComplexityClass::O1.to_string(), "O(1)");
    assert_eq!(ComplexityClass::ON.to_string(), "O(n)");
    assert_eq!(ComplexityClass::OPolynomial(2).to_string(), "O(n^2)");
    assert_eq!(ComplexityClass::OFactorial.to_string(), "O(n!)");
    assert_eq!(ComplexityClass::Unknown.to_string(), "O(?)");
}

// -- ExactComplexity ---------------------------------------------------------

#[test]
fn exact_complexity_new_is_empty() {
    let ec = ExactComplexity::new();
    assert!(ec.is_empty());
}

#[test]
fn exact_complexity_add_work_and_simplify() {
    let mut ec = ExactComplexity::new();
    ec.add_work(ComplexityClass::ON, 1);
    assert_eq!(ec.simplified(), ComplexityClass::ON);
}

#[test]
fn exact_complexity_simplified_picks_highest_class() {
    let mut ec = ExactComplexity::new();
    ec.add_work(ComplexityClass::O1, 5);
    ec.add_work(ComplexityClass::ON, 2);
    ec.add_work(ComplexityClass::OPolynomial(2), 1);
    assert_eq!(ec.simplified(), ComplexityClass::OPolynomial(2));
}

#[test]
fn exact_complexity_merge_combines_terms() {
    let mut a = ExactComplexity::new();
    a.add_work(ComplexityClass::ON, 1);

    let mut b = ExactComplexity::new();
    b.add_work(ComplexityClass::ON, 2);

    a.merge(&b);
    assert_eq!(a.simplified(), ComplexityClass::ON);
    // Display should show the combined coefficient
    let display = a.to_string();
    assert!(display.contains("3n"), "merged display should show 3n, got: {}", display);
}

#[test]
fn exact_complexity_display_fromstr_roundtrip() {
    let mut ec = ExactComplexity::new();
    ec.add_work(ComplexityClass::ON, 2);
    let s = ec.to_string();
    let parsed = ExactComplexity::from_str(&s)
        .unwrap_or_else(|e| panic!("Failed to parse '{}': {}", s, e));
    assert_eq!(ec, parsed);
}

// -- Cardinality -------------------------------------------------------------

#[test]
fn cardinality_exact_display() {
    let c = Cardinality::Exact(BigUint::from(42u64));
    assert_eq!(c.to_string(), "42");
}

#[test]
fn cardinality_unknown_display() {
    assert_eq!(Cardinality::Unknown.to_string(), "?");
}

#[test]
fn cardinality_max_both_exact_picks_larger() {
    let a = Cardinality::Exact(BigUint::from(10u64));
    let b = Cardinality::Exact(BigUint::from(20u64));
    // Cardinality::max is a custom method that compares BigUint values
    let result = a.max(b);
    assert_eq!(result, Cardinality::Exact(BigUint::from(20u64)));
}

#[test]
fn cardinality_max_is_commutative_for_exact() {
    let a = Cardinality::Exact(BigUint::from(5u64));
    let b = Cardinality::Exact(BigUint::from(100u64));
    let ab = a.clone().max(b.clone());
    let ba = b.max(a);
    assert_eq!(ab, ba);
}

#[test]
fn cardinality_max_exact_vs_unknown() {
    let a = Cardinality::Exact(BigUint::from(100u64));
    let b = Cardinality::Unknown;
    let result = a.max(b);
    assert_eq!(result, Cardinality::Unknown);
}

#[test]
fn cardinality_ordering_exact_less_than_unknown() {
    let exact = Cardinality::Exact(BigUint::from(1_000_000u64));
    let unknown = Cardinality::Unknown;
    assert!(exact < unknown);
}

#[test]
fn cardinality_max_symmetric_for_unknown() {
    let a = Cardinality::Unknown;
    let b = Cardinality::Exact(BigUint::from(999u64));
    let ab = a.clone().max(b.clone());
    let ba = b.max(a);
    assert_eq!(ab, ba);
    assert_eq!(ab, Cardinality::Unknown);
}
