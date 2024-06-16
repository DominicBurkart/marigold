use core::fmt::Debug;
use core::hash::Hash;
use std::{collections::HashSet, ops::Deref};

#[derive(Debug)]
pub struct MarigoldProgram {
    pub streams: Vec<Stream>,
}

impl PartialEq for MarigoldProgram {
    /// unordered comparison of streams
    fn eq(&self, other: &Self) -> bool {
        self.streams.iter().collect::<HashSet<_>>() == other.streams.iter().collect::<HashSet<_>>()
    }
}

impl Eq for MarigoldProgram {}

pub struct Stream {
    pub input: StreamInput,
    pub transformations: Vec<Box<dyn StreamTransformation>>,
    pub output: StreamOutput,
}

impl PartialEq for Stream {
    fn eq(&self, other: &Self) -> bool {
        self.input == other.input
            && self.transformations == other.transformations
            && self.output == other.output
    }
}

impl Hash for Stream {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.input.hash(state);
        self.transformations.hash(state);
        self.output.hash(state);
    }
}

impl Debug for Stream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Stream")
            .field("input", &self.input)
            .field("transformations", &self.transformations)
            .field("output", &self.output)
            .finish()
    }
}

impl PartialEq for dyn StreamTransformation {
    fn eq(&self, other: &Self) -> bool {
        format!("{:?}", self) == format!("{:?}", other) // hack to handle trait cycle with Box<dyn T>
    }
}

impl Hash for dyn StreamTransformation {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        format!("{:?}", self).hash(state) // hack to handle trait cycle with Box<dyn T>
    }
}

impl Eq for Stream {}

#[derive(Debug)]
pub struct StreamInput {
    pub format: DataStreamFormat,
    pub source: Box<dyn TransportOrStorageSite>,
    pub type_ident: Option<String>,
}

impl PartialEq for StreamInput {
    fn eq(&self, other: &Self) -> bool {
        self.format == other.format
            && *self.source == *other.source
            && self.type_ident == other.type_ident
    }
}

impl Hash for StreamInput {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.format.hash(state);
        self.source.hash(state);
        self.type_ident.hash(state);
    }
}

impl PartialEq for dyn TransportOrStorageSite {
    fn eq(&self, other: &Self) -> bool {
        format!("{:?}", self) == format!("{:?}", other) // hack to handle trait cycle with Box<dyn T>
    }
}

impl Hash for dyn TransportOrStorageSite {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        format!("{:?}", self).hash(state) // hack to handle trait cycle with Box<dyn T>
    }
}

#[derive(Debug)]
pub struct StreamOutput {
    pub format: DataStreamFormat,
    pub target: Box<dyn TransportOrStorageSite>,
}

impl PartialEq for StreamOutput {
    fn eq(&self, other: &Self) -> bool {
        self.format == other.format && *self.target == *other.target
    }
}

impl Hash for StreamOutput {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.format.hash(state);
        self.target.deref().hash(state);
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum DataStreamFormat {
    CSV,
    INFER,
}

pub trait TransportOrStorageSite: Debug {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct File {
    pub path: String,
}

impl TransportOrStorageSite for File {}

pub trait StreamTransformation: Debug {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OkOrPanic {}

impl StreamTransformation for OkOrPanic {}
