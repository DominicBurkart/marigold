//! Tests that `range(EnumName)` produces the correct stream cardinality.
//!
//! The `range(EnumName)` feature iterates over all *unit* variants of a
//! Marigold enum (those included in `__marigold_variants()`). The resolved
//! cardinality must equal `unit_variant_count()`, not `variants.len() + 1`.
//!
//! This module also acts as a regression guard for a latent bug in
//! `SymbolTable::add_enum` where enums with a `Sized` default variant are
//! over-counted by 1. That path is exercised by the symbol-table unit tests;
//! here we confirm that the *stream cardinality* path (which goes through
//! `unit_variant_count()`, not through the symbol table) is also correct and
//! stays correct even if someone later rewires it to go via the symbol table.

use marigold_grammar::complexity::Cardinality;
use num_bigint::BigUint;

/// Parse and analyze a `.marigold` source file from the `tests/programs/`
/// directory, exactly as the other e2e cardinality tests do.
fn analyze_file(path: &str) -> marigold_grammar::complexity::ProgramComplexity {
    let source =
        std::fs::read_to_string(path).unwrap_or_else(|e| panic!("Failed to read {path}: {e}"));
    marigold_grammar::marigold_analyze(&source)
        .unwrap_or_else(|e| panic!("Failed to analyze {path}: {e}"))
}

/// Analyze an inline Marigold program string, returning the raw Result.
fn analyze_str(
    source: &str,
) -> Result<
    marigold_grammar::complexity::ProgramComplexity,
    marigold_grammar::parser::MarigoldParseError,
> {
    marigold_grammar::marigold_analyze(source)
}

// ---------------------------------------------------------------------------
// Four unit variants, no default — cardinality must be exactly 4
// ---------------------------------------------------------------------------

#[test]
fn range_enum_four_unit_variants() {
    // Direction has exactly 4 named unit variants and no default variant.
    // range(Direction) must produce Cardinality::Exact(4).
    let result = analyze_file("tests/programs/card_range_enum_four.marigold");

    assert_eq!(result.streams.len(), 1);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(4u64)),
        "range(Direction) with 4 unit variants should have exact cardinality 4"
    );
    assert_eq!(
        result.program_cardinality,
        Cardinality::Exact(BigUint::from(4u64))
    );
}

// ---------------------------------------------------------------------------
// 2 named variants + 1 WithDefaultValue default — cardinality must be 3
//
// A WithDefaultValue default IS included in `__marigold_variants()`, so
// `unit_variant_count()` returns base + 1 = 3.
// ---------------------------------------------------------------------------

#[test]
fn range_enum_with_default_value_variant() {
    // Status: Active, Inactive, + default Unknown (WithDefaultValue).
    // unit_variant_count = 3, so cardinality must be Exact(3).
    let result = analyze_file("tests/programs/card_range_enum_with_default_value.marigold");

    assert_eq!(result.streams.len(), 1);
    assert_eq!(
        result.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(3u64)),
        "range(Status) with 2 named + 1 WithDefaultValue default should have exact cardinality 3"
    );
    assert_eq!(
        result.program_cardinality,
        Cardinality::Exact(BigUint::from(3u64))
    );
}

// ---------------------------------------------------------------------------
// 2 named variants + Sized default — cardinality must NOT be 3
//
// A Sized default (e.g. `default Other(string_64)`) is NOT a unit variant:
// it holds data and cannot be copy-constructed into the fixed-size variants
// array. It is therefore excluded from `__marigold_variants()`, so
// `unit_variant_count()` returns 2, not 3.
//
// This is the regression test for the `SymbolTable::add_enum` over-counting
// bug. That function naively adds 1 for *any* `default_variant.is_some()`,
// including Sized defaults — giving a count of 3 instead of 2. The stream
// cardinality resolution path correctly uses `unit_variant_count()` instead,
// but this test ensures no future refactor accidentally routes through the
// symbol table for cardinality and inherits the bug.
//
// Accepted outcomes:
//   a) Ok with cardinality Exact(2) — correct behaviour.
//   b) Err — the parser rejects ranging over an enum with a Sized default
//      (also acceptable; Sized variants can't be copy-constructed).
//
// The test FAILS if the result is Ok with cardinality Exact(3) (the
// over-counted wrong answer that would come from using the symbol table).
// ---------------------------------------------------------------------------

#[test]
fn range_enum_sized_default_not_counted() {
    // Color: Red, Green, + default Other(string_64)  [Sized].
    // Correct unit_variant_count = 2; the Sized default is NOT a unit variant.
    let source =
        std::fs::read_to_string("tests/programs/card_range_enum_sized_default.marigold")
            .expect("Failed to read card_range_enum_sized_default.marigold");

    match marigold_grammar::marigold_analyze(&source) {
        Ok(result) => {
            // If the parser accepts ranging over a Sized-default enum, the
            // cardinality must be 2 (only the two named unit variants).
            // Cardinality 3 would be the symbol_table over-counting bug.
            assert_ne!(
                result.streams[0].cardinality,
                Cardinality::Exact(BigUint::from(3u64)),
                "range(Color): Sized default must NOT be counted as a unit variant \
                 (cardinality 3 would be the SymbolTable::add_enum over-counting bug)"
            );
            assert_eq!(
                result.streams[0].cardinality,
                Cardinality::Exact(BigUint::from(2u64)),
                "range(Color) with 2 named variants + Sized default should have exact cardinality 2"
            );
        }
        Err(_) => {
            // Rejecting range(EnumWithSizedDefault) is also acceptable:
            // a Sized variant cannot appear in __marigold_variants().
        }
    }
}

// ---------------------------------------------------------------------------
// range(NonExistent) — must be rejected when the enum is not declared
// ---------------------------------------------------------------------------

#[test]
fn range_unknown_enum_is_rejected() {
    // There is no declaration of `GhostEnum` anywhere in this program.
    // `resolve_enum_range_counts` in the parser must return an error rather
    // than silently emitting invalid Rust that references an undefined type.
    let source = "range(GhostEnum).return";
    let result = analyze_str(source);
    assert!(
        result.is_err(),
        "range(GhostEnum) should be rejected when GhostEnum is not declared, got: {:?}",
        result.ok()
    );
}
