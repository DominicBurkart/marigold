//! Regression guard: issue #68.
//! Fails if the `git config commit.gpgsign false` workaround is reintroduced.

#[test]
fn bisect_tests_do_not_configure_gpg() {
    let cardinality = include_str!("bisect_cardinality.rs");
    let complexity = include_str!("bisect_complexity.rs");
    for (name, src) in [("cardinality", cardinality), ("complexity", complexity)] {
        assert!(
            !src.contains("commit.gpgsign"),
            "{name} bisect test reintroduces the gpg config workaround (issue #68)",
        );
    }
}
