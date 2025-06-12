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

#[derive(Debug)]
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

impl Eq for Stream {}

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

#[cfg(not(feature = "static_analysis"))]
pub trait TransportOrStorageSite: Debug {
    fn as_any(&self) -> &dyn std::any::Any;
    fn is_variable_reference(&self) -> bool { false }
    fn get_variable_name(&self) -> Option<&str> { None }
}

#[cfg(feature = "static_analysis")]
pub trait TransportOrStorageSite: Debug + Cardinal {
    fn as_any(&self) -> &dyn std::any::Any;
    fn is_variable_reference(&self) -> bool { false }
    fn get_variable_name(&self) -> Option<&str> { None }
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
pub trait StreamTransformation: Debug {
    fn as_any(&self) -> &dyn std::any::Any;
    fn is_fold(&self) -> bool { false }
    fn is_map(&self) -> bool { false }
    fn get_map_function(&self) -> Option<&str> { None }
    fn get_fold_initial_value(&self) -> Option<&str> { None }
    fn get_fold_accumulator_function(&self) -> Option<&str> { None }
}

#[cfg(feature = "static_analysis")]
pub trait StreamTransformation: Debug + DerivedCardinal {
    fn as_any(&self) -> &dyn std::any::Any;
    fn is_fold(&self) -> bool { false }
    fn is_map(&self) -> bool { false }
    fn get_map_function(&self) -> Option<&str> { None }
    fn get_fold_initial_value(&self) -> Option<&str> { None }
    fn get_fold_accumulator_function(&self) -> Option<&str> { None }
}


#[cfg(feature = "static_analysis")]
struct ExponentialBackoffParameters {
    first_delay_ms: i32,
    max_delay_ms: i32,
    jitter_ms: i32,
    factor: i32,
    max_retries: Option<i32>
}

#[cfg(feature = "static_analysis")]
impl Default for ExponentialBackoffParameters {
    fn default() -> Self {
        Self {
            first_delay_ms: 100,
            max_delay_ms: 2 * 60 * 1000,
            jitter_ms: 1000,
            factor: 2,
            max_retries: None
        }
    }
}

impl Eq for dyn StreamTransformation {}

#[cfg(feature = "static_analysis")]
struct LinearParameters {
    delay_ms: i32,
    jitter_ms: i32,
    max_retries: Option<i32>
}

#[cfg(feature = "static_analysis")]
impl Default for LinearParameters {
    fn default() -> Self {
        Self {
            delay_ms: 15 * 1000,
            jitter_ms: 15 * 1000,
            max_retries: None
        }
    }
}

#[cfg(feature = "static_analysis")]
enum Retry {
    ExponentialBackoff(ExponentialBackoffParameters),
    Linear(LinearParameters),
    Panic,
}

#[cfg(feature = "static_analysis")]
pub trait RetryPolicy: Effects {
    fn get_retry_policy(&self) -> Retry;
}

#[cfg(feature = "static_analysis")]
pub trait Effects {
    fn get_effects() -> Vec<Effect>;
}

#[cfg(feature = "static_analysis")]
pub enum Effect {
    EnvironmentVariable(EnvironmentVariableEffect),
    MessageQueue(),
    SharedMemory(SharedMemoryEffect),
    File(FileEffect),
    Network(NetworkEffect),
}

#[cfg(feature = "static_analysis")]
pub enum EnvironmentVariableEffect {
    Create(String),
    CheckExists(String),
    Read(String),
    Write(String),
    Delete(String)
}

#[cfg(feature = "static_analysis")]
pub enum SharedMemoryEffect {
    Read(MemoryIdentifier),
    Write(MemoryIdentifier)
}

#[cfg(feature = "static_analysis")]
pub enum MemoryIdentifier {
    NamedMemory(String),
    MemoryFile(FilePath),
}

#[cfg(feature = "static_analysis")]
type FilePath = std::path::PathBuf;

#[cfg(feature = "static_analysis")]
pub enum FileEffect {
    Create(String),
    CheckExists(String),
    Read(FilePath),
    Write(FilePath),
    Delete(String)
}

#[cfg(feature = "static_analysis")]
pub enum NetworkEffect {
    Read(NetworkInterface),
    Write(NetworkInterface)
}

#[cfg(feature = "static_analysis")]
pub enum NetworkInterface {
    Socket(std::net::SocketAddr, InternetTransportProtocol),
    Other(String)
}

#[cfg(feature = "static_analysis")]
pub enum InternetTransportProtocol {
    Quic(QuicApplicationProtocol),
    Tcp(TcpApplicationProtocol),
    Udp(UdpApplicationProtocol),
    Other(String)
}

#[cfg(feature = "static_analysis")]
pub enum QuicApplicationProtocol {
    Https,
    Http,
    Dns,
    Other(String)
}

#[cfg(feature = "static_analysis")]
pub enum TcpApplicationProtocol {
    Https,
    Http,
    Dns,
    Websocket,
    Other(String)
}

#[cfg(feature = "static_analysis")]
pub enum UdpApplicationProtocol {
    Dns,
    Other(String)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct OkOrPanic {}

impl StreamTransformation for OkOrPanic {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MapTransformation {
    pub function_name: String,
}

impl StreamTransformation for MapTransformation {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    
    fn is_map(&self) -> bool {
        true
    }
    
    fn get_map_function(&self) -> Option<&str> {
        Some(&self.function_name)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FilterTransformation {
    pub function_name: String,
}

impl StreamTransformation for FilterTransformation {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FoldTransformation {
    pub initial_value: String, // could be function name or literal value
    pub accumulator_function: String,
}

impl StreamTransformation for FoldTransformation {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    
    fn is_fold(&self) -> bool {
        true
    }
    
    fn get_fold_initial_value(&self) -> Option<&str> {
        Some(&self.initial_value)
    }
    
    fn get_fold_accumulator_function(&self) -> Option<&str> {
        Some(&self.accumulator_function)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PermutationsTransformation {
    pub size: i32,
}

impl StreamTransformation for PermutationsTransformation {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct CombinationsTransformation {
    pub size: i32,
}

impl StreamTransformation for CombinationsTransformation {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct KeepFirstNTransformation {
    pub n: i32,
    pub comparator_function: String,
}

impl StreamTransformation for KeepFirstNTransformation {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PermutationsWithReplacementTransformation {
    pub size: i32,
}

impl StreamTransformation for PermutationsWithReplacementTransformation {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

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
impl DerivedCardinal for FoldTransformation {
    fn get_output_cardinality(&self, _input_cardinality: Cardinality) -> Cardinality {
        // Fold reduces to a single value
        Cardinality::Known(1u64.into())
    }
}

#[cfg(feature = "static_analysis")]
impl DerivedCardinal for PermutationsTransformation {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality {
        // Permutations increase cardinality but we can't calculate without more info
        Cardinality::Unknown
    }
}

#[cfg(feature = "static_analysis")]
impl DerivedCardinal for CombinationsTransformation {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality {
        // Combinations change cardinality but we can't calculate without more info
        Cardinality::Unknown
    }
}

#[cfg(feature = "static_analysis")]
impl DerivedCardinal for KeepFirstNTransformation {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality {
        // KeepFirstN limits output to at most n items
        match input_cardinality {
            Cardinality::Known(count) => Cardinality::Known(count.min((self.n as u64).into())),
            _ => Cardinality::Unknown,
        }
    }
}

#[cfg(feature = "static_analysis")]
impl DerivedCardinal for PermutationsWithReplacementTransformation {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality {
        // Permutations with replacement increase cardinality but we can't calculate without more info
        Cardinality::Unknown
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

#[cfg(feature = "static_analysis")]
impl Spanned for Stream {
    fn get_input_span(&self) -> Span {
        todo!()
    }

    fn get_output_span(&self) -> Span {
        todo!()
    }
}

impl Implementable for Stream {
    fn get_code<'a>(&'a self) -> &'a str {
        todo!()
    }
}

impl Sourceable for Stream {
    fn get_source<'a>(&'a self) -> &'a str {
        todo!()
    }
}

impl std::fmt::Display for MarigoldProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "async move {{")?;
        writeln!(f, "    use std::pin::Pin;")?;
        writeln!(f, "    use marigold::marigold_impl::StreamExt;")?;
        writeln!(f, "    use marigold::marigold_impl::futures::FutureExt;")?;
        writeln!(f, "    use marigold::marigold_impl::{{Permutable, Combinable, KeepFirstN, CollectAndAppliable, Marifold}};")?;
        
        // Generate function definitions
        for function in &self.functions {
            writeln!(f, "    {}", function)?;
        }
        
        // Generate struct definitions  
        for struct_def in &self.structs {
            writeln!(f, "    {}", struct_def)?;
        }
        
        // Generate stream variable assignments
        for variable in &self.variables {
            writeln!(f, "    let {} = {};", variable.name, variable.input)?;
        }
        
        // Generate the main stream pipeline - use the first stream if available
        if let Some(stream) = self.streams.first() {
            // Check if any transformation is a fold
            let has_fold = stream.transformations.iter().any(|t| t.is_fold());
            
            if has_fold {
                // Find the fold transformation index
                let fold_idx = stream.transformations.iter().position(|t| t.is_fold()).unwrap();
                
                // Split transformations into phases
                let pre_fold = &stream.transformations[..fold_idx];
                let fold_transform = &stream.transformations[fold_idx];
                let post_fold = &stream.transformations[fold_idx + 1..];
                
                let initial_value = fold_transform.get_fold_initial_value().unwrap();
                let accumulator_function = fold_transform.get_fold_accumulator_function().unwrap();
                let init_val = if initial_value.chars().any(|c| c.is_alphabetic()) {
                    format!("{}()", initial_value)
                } else {
                    initial_value.to_string()
                };
                
                // For fold operations, use futures fold and wrap result in a stream
                write!(f, "    marigold::marigold_impl::run_stream::run_stream(")?;
                write!(f, "marigold::marigold_impl::futures::stream::once(")?;
                write!(f, "{}", stream.input)?;
                for transformation in pre_fold {
                    write!(f, ".{}", transformation)?;
                }
                write!(f, ".fold({}, |acc, item| async move {{ {}(acc, item) }})", init_val, accumulator_function)?;
                write!(f, ")")?;
                
                if !post_fold.is_empty() {
                    // For post-fold transformations, we need to map over the single result
                    for transformation in post_fold {
                        if transformation.is_map() {
                            if let Some(function_name) = transformation.get_map_function() {
                                write!(f, ".map(|result| {}(result))", function_name)?;
                            }
                        }
                    }
                }
                write!(f, ")")?;
            } else {
                write!(f, "    marigold::marigold_impl::run_stream::run_stream(Box::pin(")?;
                
                // Generate input
                write!(f, "{}", stream.input)?;
                
                // Chain transformations
                for transformation in &stream.transformations {
                    write!(f, ".{}", transformation)?;
                }
                write!(f, ")")?;
                
                // Handle output - for .return we don't need additional chaining
                if let Some(file) = stream.output.target.as_any().downcast_ref::<RuntimeAccessibleFile>() {
                    write!(f, ".write_file(\"{}\", {:?})", file.path, stream.output.format)?;
                } else if let Some(_) = stream.output.target.as_any().downcast_ref::<ReturnTarget>() {
                    // No additional transformation needed for return
                }
                
                write!(f, ")")?;
            }
            
            writeln!(f)?;
        } else {
            writeln!(f, "    marigold::marigold_impl::run_stream(marigold::marigold_impl::futures::stream::empty())")?;
        }
        
        writeln!(f, "}}")
    }
}

// Display implementations for code generation
impl std::fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}: {}", param.0, param.1)?;
        }
        
        // Replace the function body markers with proper braces
        let body = self.body
            .replace("%%%MARIGOLD_FUNCTION_START%%%", "{")
            .replace("%%%MARIGOLD_FUNCTION_END%%%", "}");
        
        write!(f, ") -> {} {}", self.return_type, body)
    }
}

impl std::fmt::Display for StructDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {} {{", self.name)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}: {}", field.0, field.1)?;
        }
        write!(f, "}}")
    }
}

impl std::fmt::Display for StreamInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Use Any to downcast the trait object to concrete types
        if let Some(range) = self.source.as_any().downcast_ref::<RangeInput>() {
            write!(f, "marigold::marigold_impl::futures::stream::iter({}..{})", range.start, range.end)
        } else if let Some(_file) = self.source.as_any().downcast_ref::<RuntimeAccessibleFile>() {
            write!(f, "marigold::marigold_impl::futures::stream::iter(std::iter::empty::<i32>())") // TODO: implement file reading
        } else if let Some(select_all) = self.source.as_any().downcast_ref::<SelectAllInput>() {
            write!(f, "marigold::marigold_impl::futures::stream::select_all(vec![")?;
            for (i, input) in select_all.inputs.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{}", input)?;
            }
            write!(f, "])")
        } else if self.source.is_variable_reference() {
            let var_name = self.source.get_variable_name().unwrap();
            write!(f, "{}", var_name)
        } else {
            // Fallback: try to parse from debug representation as a temporary fix
            let debug_str = format!("{:?}", self.source);
            // println!("DEBUG: Unknown source type: {}", debug_str);
            if debug_str.contains("SelectAllInput") {
                // Extract the inputs and generate select_all
                write!(f, "marigold::marigold_impl::futures::stream::select_all(vec![")?;
                // Parse out individual range inputs from the debug string
                let inputs_start = debug_str.find("inputs: [").unwrap_or(0) + 9;
                let inputs_end = debug_str.rfind("]").unwrap_or(debug_str.len());
                let inputs_section = &debug_str[inputs_start..inputs_end];
                
                let mut first = true;
                // Simple regex-like parsing for RangeInput patterns
                let mut current_pos = 0;
                while let Some(range_start) = inputs_section[current_pos..].find("RangeInput { start: ") {
                    let start_pos = current_pos + range_start + 20;
                    if let Some(comma_pos) = inputs_section[start_pos..].find(',') {
                        let start_val = &inputs_section[start_pos..start_pos + comma_pos].trim();
                        if let Some(end_start) = inputs_section[start_pos + comma_pos..].find("end: ") {
                            let end_pos = start_pos + comma_pos + end_start + 5;
                            if let Some(end_end) = inputs_section[end_pos..].find(' ') {
                                let end_val = &inputs_section[end_pos..end_pos + end_end].trim();
                                if !first { write!(f, ", ")?; }
                                write!(f, "marigold::marigold_impl::futures::stream::iter({}..{})", start_val, end_val)?;
                                first = false;
                                current_pos = end_pos + end_end;
                            } else { break; }
                        } else { break; }
                    } else { break; }
                }
                write!(f, "])")
            } else if debug_str.contains("RangeInput") {
                if let (Some(start_idx), Some(end_idx)) = (debug_str.find("start: "), debug_str.find(", end: ")) {
                    let after_start = &debug_str[start_idx + 7..];
                    let after_end = &debug_str[end_idx + 6..];
                    
                    let start_str = after_start.split(',').next().unwrap().trim();
                    let end_str = after_end.trim().split(' ').next().unwrap().trim();
                    if let (Ok(start), Ok(end)) = (start_str.parse::<i32>(), end_str.parse::<i32>()) {
                        return write!(f, "marigold::marigold_impl::futures::stream::iter({}..{})", start, end);
                    }
                }
                write!(f, "marigold::marigold_impl::futures::stream::iter(std::iter::empty::<i32>())")
            } else {
                write!(f, "marigold::marigold_impl::futures::stream::iter(std::iter::empty::<i32>())")
            }
        }
    }
}

impl std::fmt::Display for dyn StreamTransformation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Use Any to downcast the trait object to concrete types
        if let Some(_) = self.as_any().downcast_ref::<OkOrPanic>() {
            write!(f, "map(|x| x.unwrap())")
        } else if let Some(map) = self.as_any().downcast_ref::<MapTransformation>() {
            write!(f, "map({})", map.function_name)
        } else if let Some(filter) = self.as_any().downcast_ref::<FilterTransformation>() {
            write!(f, "filter(|x| {{ let x_clone = x.clone(); async move {{ {}(&x_clone) }} }})", filter.function_name)
        } else if let Some(fold) = self.as_any().downcast_ref::<FoldTransformation>() {
            // Check if initial_value looks like a function call (contains letters)
            let init_val = if fold.initial_value.chars().any(|c| c.is_alphabetic()) {
                format!("{}()", fold.initial_value)
            } else {
                fold.initial_value.clone()
            };
            write!(f, "fold({}, |acc, item| async move {{ {}(acc, item) }})", init_val, fold.accumulator_function)
        } else if let Some(perm) = self.as_any().downcast_ref::<PermutationsTransformation>() {
            write!(f, "permutations({}).await", perm.size)
        } else if let Some(comb) = self.as_any().downcast_ref::<CombinationsTransformation>() {
            write!(f, "combinations({}).await", comb.size)
        } else if let Some(keep) = self.as_any().downcast_ref::<KeepFirstNTransformation>() {
            write!(f, "keep_first_n({}, {}).await", keep.n, keep.comparator_function)
        } else if let Some(perm) = self.as_any().downcast_ref::<PermutationsWithReplacementTransformation>() {
            write!(f, "permutations_with_replacement({}).await", perm.size)
        } else {
            write!(f, "map(|x| x)")
        }
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
    
    fn is_variable_reference(&self) -> bool { true }
    fn get_variable_name(&self) -> Option<&str> { Some(&self.name) }
}

#[cfg(not(feature = "static_analysis"))]
impl TransportOrStorageSite for VariableReference {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
    
    fn is_variable_reference(&self) -> bool { true }
    fn get_variable_name(&self) -> Option<&str> { Some(&self.name) }
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