use core::fmt::Debug;
use core::hash::Hash;
use std::{collections::HashSet, ops::Deref};

#[cfg(feature = "static_analysis")]
use crate::static_analysis::{self, Cardinal, Cardinality, DerivedCardinal, Spanned, Implementable, Sourceable, Span};

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

#[cfg(not(feature = "static_analysis"))]
pub trait TransportOrStorageSite: Debug {}

#[cfg(feature = "static_analysis")]
pub trait TransportOrStorageSite: Debug + Cardinal {}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct RuntimeAccessibleFile {
    pub path: String,
}

#[cfg(feature = "static_analysis")]
impl Cardinal for RuntimeAccessibleFile {
    fn get_output_cardinality(&self) -> Cardinality {
        Cardinality::Unknown
    }
}

#[cfg(feature = "static_analysis")]
impl TransportOrStorageSite for RuntimeAccessibleFile {}

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

impl StreamTransformation for OkOrPanic {}


#[cfg(feature = "static_analysis")]
impl DerivedCardinal for OkOrPanic {
    fn get_output_cardinality(&self, input_cardinality: Cardinality) -> Cardinality {
        return input_cardinality;
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
        // TODO: Generate actual Rust code for the streams
        write!(f, "// TODO: Implement code generation for MarigoldProgram\n// {} streams found", self.streams.len())
    }
}