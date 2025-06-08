use core::fmt::Debug;
use core::hash::Hash;
use std::{collections::HashSet, ops::Deref};

#[cfg(feature = "static_analysis")]
use crate::static_analysis::{self, Cardinal, Cardinality, DerivedCardinal, Spanned, Implementable, Sourceable, Span};

#[derive(Debug)]
pub struct MarigoldProgram {
    pub streams: Vec<Stream>,
    pub functions: Vec<FunctionDefinition>,
    pub structs: Vec<StructDefinition>,
    pub variables: Vec<StreamVariable>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: String,
    pub parameters: Vec<(String, String)>,
    pub return_type: String,
    pub body: String,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructDefinition {
    pub name: String,
    pub fields: Vec<(String, String)>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StreamVariable {
    pub name: String,
    pub input: StreamInput,
}

impl PartialEq for MarigoldProgram {
    /// unordered comparison of streams, functions, structs, and variables
    fn eq(&self, other: &Self) -> bool {
        self.streams.iter().collect::<HashSet<_>>() == other.streams.iter().collect::<HashSet<_>>()
            && self.functions == other.functions
            && self.structs == other.structs
            && self.variables == other.variables
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

impl Eq for Stream {}

impl Hash for Stream {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.input.hash(state);
        // Can't hash trait objects directly, use debug representation
        format!("{:?}", self.transformations).hash(state);
        self.output.hash(state);
    }
}

pub struct StreamInput {
    pub format: DataStreamFormat,
    pub source: Box<dyn TransportOrStorageSite>,
    pub type_ident: Option<String>,
}

impl PartialEq for StreamInput {
    fn eq(&self, other: &Self) -> bool {
        self.format == other.format
            && self.source.deref() == other.source.deref()
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

impl Eq for StreamInput {}

impl Clone for StreamInput {
    fn clone(&self) -> Self {
        // Manual clone since we need to handle the trait object
        let source = if let Some(file) = self.source.as_any().downcast_ref::<RuntimeAccessibleFile>() {
            Box::new(file.clone()) as Box<dyn TransportOrStorageSite>
        } else if let Some(range) = self.source.as_any().downcast_ref::<RangeInput>() {
            Box::new(range.clone()) as Box<dyn TransportOrStorageSite>
        } else if let Some(ret) = self.source.as_any().downcast_ref::<ReturnTarget>() {
            Box::new(ret.clone()) as Box<dyn TransportOrStorageSite>
        } else if let Some(var) = self.source.as_any().downcast_ref::<VariableReference>() {
            Box::new(var.clone()) as Box<dyn TransportOrStorageSite>
        } else if let Some(select) = self.source.as_any().downcast_ref::<SelectAllInput>() {
            Box::new(select.clone()) as Box<dyn TransportOrStorageSite>
        } else {
            panic!("Unknown TransportOrStorageSite type for cloning")
        };
        
        StreamInput {
            format: self.format.clone(),
            source,
            type_ident: self.type_ident.clone(),
        }
    }
}

impl PartialEq for dyn TransportOrStorageSite {
    fn eq(&self, other: &Self) -> bool {
        format!("{:?}", self) == format!("{:?}", other) // hack to handle trait cycle with Box<dyn T>
    }
}

impl Hash for dyn TransportOrStorageSite {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        format!("{:?}", self).hash(state); // hack to handle trait cycle with Box<dyn T>
    }
}

pub struct StreamOutput {
    pub format: DataStreamFormat,
    pub target: Box<dyn TransportOrStorageSite>,
}

impl PartialEq for StreamOutput {
    fn eq(&self, other: &Self) -> bool {
        self.format == other.format && self.target.deref() == other.target.deref()
    }
}

impl Eq for StreamOutput {}

impl Hash for StreamOutput {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.format.hash(state);
        self.target.hash(state);
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum DataStreamFormat {
    CSV,
    INFER,
}

#[cfg(not(feature = "static_analysis"))]
pub trait TransportOrStorageSite: Debug {
    fn as_any(&self) -> &dyn std::any::Any;
}

#[cfg(feature = "static_analysis")]
pub trait TransportOrStorageSite: Debug + Cardinal {
    fn as_any(&self) -> &dyn std::any::Any;
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RuntimeAccessibleFile {
    pub path: String,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RangeInput {
    pub start: i32,
    pub end: i32,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ReturnTarget {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct VariableReference {
    pub name: String,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SelectAllInput {
    pub inputs: Vec<StreamInput>,
}

#[cfg(feature = "static_analysis")]
impl Cardinal for RuntimeAccessibleFile {
    fn get_output_cardinality(&self) -> Cardinality {
        Cardinality::Unknown
    }
}

#[cfg(feature = "static_analysis")]
impl TransportOrStorageSite for RuntimeAccessibleFile {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(feature = "static_analysis")]
impl Cardinal for RangeInput {
    fn get_output_cardinality(&self) -> Cardinality {
        let count = (self.end - self.start).max(0) as u64;
        Cardinality::Known(count.into())
    }
}

#[cfg(feature = "static_analysis")]
impl TransportOrStorageSite for RangeInput {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(not(feature = "static_analysis"))]
impl TransportOrStorageSite for RuntimeAccessibleFile {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(not(feature = "static_analysis"))]
impl TransportOrStorageSite for RangeInput {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(feature = "static_analysis")]
impl Cardinal for ReturnTarget {
    fn get_output_cardinality(&self) -> Cardinality {
        // Return doesn't change cardinality
        Cardinality::Unknown
    }
}

#[cfg(feature = "static_analysis")]
impl TransportOrStorageSite for ReturnTarget {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(not(feature = "static_analysis"))]
impl TransportOrStorageSite for ReturnTarget {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(feature = "static_analysis")]
impl Cardinal for StreamInput {
    fn get_output_cardinality(&self) -> static_analysis::Cardinality {
        self.source.as_ref().get_output_cardinality()
    }
}

#[cfg(not(feature = "static_analysis"))]
pub trait StreamTransformation: Debug {}

#[cfg(feature = "static_analysis")]
pub trait StreamTransformation: Debug + DerivedCardinal {}

impl PartialEq for dyn StreamTransformation {
    fn eq(&self, other: &Self) -> bool {
        format!("{:?}", self) == format!("{:?}", other) // hack to handle trait cycle with Box<dyn T>
    }
}

impl Eq for dyn StreamTransformation {}

impl Hash for dyn StreamTransformation {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        format!("{:?}", self).hash(state); // hack to handle trait cycle with Box<dyn T>
    }
}

impl Clone for Box<dyn StreamTransformation> {
    fn clone(&self) -> Self {
        // Manual clone since we need to handle the trait object
        if let Some(ok_or_panic) = self.as_ref().as_any().downcast_ref::<OkOrPanic>() {
            Box::new(ok_or_panic.clone()) as Box<dyn StreamTransformation>
        } else if let Some(map_transform) = self.as_ref().as_any().downcast_ref::<MapTransformation>() {
            Box::new(map_transform.clone()) as Box<dyn StreamTransformation>
        } else if let Some(filter_transform) = self.as_ref().as_any().downcast_ref::<FilterTransformation>() {
            Box::new(filter_transform.clone()) as Box<dyn StreamTransformation>
        } else if let Some(fold_transform) = self.as_ref().as_any().downcast_ref::<FoldTransformation>() {
            Box::new(fold_transform.clone()) as Box<dyn StreamTransformation>
        } else if let Some(permutations_transform) = self.as_ref().as_any().downcast_ref::<PermutationsTransformation>() {
            Box::new(permutations_transform.clone()) as Box<dyn StreamTransformation>
        } else if let Some(combinations_transform) = self.as_ref().as_any().downcast_ref::<CombinationsTransformation>() {
            Box::new(combinations_transform.clone()) as Box<dyn StreamTransformation>
        } else if let Some(keep_first_n_transform) = self.as_ref().as_any().downcast_ref::<KeepFirstNTransformation>() {
            Box::new(keep_first_n_transform.clone()) as Box<dyn StreamTransformation>
        } else if let Some(pwr_transform) = self.as_ref().as_any().downcast_ref::<PermutationsWithReplacementTransformation>() {
            Box::new(pwr_transform.clone()) as Box<dyn StreamTransformation>
        } else {
            panic!("Unknown StreamTransformation type for cloning")
        }
    }
}

pub trait AsAny {
    fn as_any(&self) -> &dyn std::any::Any;
}

impl<T: std::any::Any> AsAny for T {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(not(feature = "static_analysis"))]
pub trait OutputSite: Debug {}

#[cfg(feature = "static_analysis")]
pub trait OutputSite: Debug + Cardinal {}

pub enum OutputType {
    File(String, DataStreamFormat),
    Stdout(DataStreamFormat),
    Return,
    Other(String),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OkOrPanic {}

impl StreamTransformation for OkOrPanic {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MapTransformation {
    pub function_name: String,
}

impl StreamTransformation for MapTransformation {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FilterTransformation {
    pub function_name: String,
}

impl StreamTransformation for FilterTransformation {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FoldTransformation {
    pub initial_value: String, // could be function name or literal value
    pub accumulator_function: String,
}

impl StreamTransformation for FoldTransformation {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PermutationsTransformation {
    pub size: i32,
}

impl StreamTransformation for PermutationsTransformation {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CombinationsTransformation {
    pub size: i32,
}

impl StreamTransformation for CombinationsTransformation {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct KeepFirstNTransformation {
    pub n: i32,
    pub comparator_function: String,
}

impl StreamTransformation for KeepFirstNTransformation {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PermutationsWithReplacementTransformation {
    pub size: i32,
}

impl StreamTransformation for PermutationsWithReplacementTransformation {}

#[cfg(feature = "static_analysis")]
impl DerivedCardinal for OkOrPanic {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality {
        return input_cardinality;
    }
}

#[cfg(feature = "static_analysis")]
impl DerivedCardinal for MapTransformation {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality {
        return input_cardinality;
    }
}

#[cfg(feature = "static_analysis")]
impl DerivedCardinal for FilterTransformation {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality {
        // Filter could reduce cardinality, but we don't know by how much
        return Cardinality::Unknown;
    }
}

#[cfg(feature = "static_analysis")]
impl Cardinal for Stream {
    fn get_output_cardinality(&self) -> Cardinality {
        let mut cardinality = self.input.get_output_cardinality();
        for transformation in self.transformations.iter() {
            cardinality = transformation.as_ref().get_output_cardinality(cardinality);
        }
        cardinality
    }
}

impl std::fmt::Display for MarigoldProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Generate actual Rust code for the streams
        write!(f, "// TODO: Implement code generation for MarigoldProgram\n// {} streams found", self.streams.len())
    }
}

#[cfg(feature = "static_analysis")]
impl Cardinal for VariableReference {
    fn get_output_cardinality(&self) -> Cardinality {
        // Variable cardinality depends on the referenced variable
        Cardinality::Unknown
    }
}

#[cfg(feature = "static_analysis")]
impl TransportOrStorageSite for VariableReference {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(not(feature = "static_analysis"))]
impl TransportOrStorageSite for VariableReference {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(feature = "static_analysis")]
impl Cardinal for SelectAllInput {
    fn get_output_cardinality(&self) -> Cardinality {
        // Select all combines multiple inputs
        Cardinality::Unknown
    }
}

#[cfg(feature = "static_analysis")]
impl TransportOrStorageSite for SelectAllInput {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(not(feature = "static_analysis"))]
impl TransportOrStorageSite for SelectAllInput {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}