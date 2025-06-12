/// Static analysis framework for marigold language constructs.
/// 
/// This module provides traits and types for analyzing the structure and properties
/// of marigold programs at compile time, including cardinality tracking and source spans.

/// Represents the cardinality (number of items) that a stream operation produces.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Cardinality {
    /// The cardinality cannot be determined at compile time.
    Unknown,
    /// The exact number of items is known.
    Known(num_bigint::BigUint),
    /// The stream produces an infinite number of items.
    Infinite,
}

/// Trait for types that have a deterministic output cardinality.
pub trait Cardinal {
    /// Returns the cardinality of items this operation produces.
    fn get_output_cardinality(&self) -> Cardinality;
}

/// Trait for transformations whose output cardinality depends on input cardinality.
pub trait DerivedCardinal {
    /// Returns the output cardinality given the input cardinality.
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality;
}

/// Represents a span in the source code (character positions).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    /// Starting character position (inclusive).
    pub start: usize,
    /// Ending character position (exclusive).
    pub end: usize,
}

/// Trait for types that have associated source code spans.
pub trait Spanned {
    /// Returns the span of the input portion of this construct.
    fn get_input_span(&self) -> Span;
    /// Returns the span of the output portion of this construct.
    fn get_output_span(&self) -> Span;
}

/// Trait for types that can generate executable code.
pub trait Implementable {
    /// Returns the generated code for this construct.
    fn get_code<'a>(&'a self) -> &'a str;
}

/// Trait for types that have associated source code.
pub trait Sourceable {
    /// Returns the original source code for this construct.
    fn get_source<'a>(&'a self) -> &'a str;
}