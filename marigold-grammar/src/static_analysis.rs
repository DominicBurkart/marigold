#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Cardinality {
    Unknown,
    Known(num_bigint::BigUint),
    Infinite,
}

pub trait Cardinal {
    fn get_output_cardinality(&self) -> Cardinality;
}

pub trait DerivedCardinal {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub trait Spanned {
    fn get_input_span(&self) -> Span;
    fn get_output_span(&self) -> Span;
}

pub trait Implementable {
    fn get_code<'a>(&'a self) -> &'a str;
}

pub trait Sourceable {
    fn get_source<'a>(&'a self) -> &'a str;
}