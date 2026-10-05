//! Opt-in OpenTelemetry instrumentation for generated Marigold streams.
//!
//! Enabled with the `otel` feature. When Marigold generates code with instrumentation on, every
//! input, stream function and output is wrapped with the adapters in this module. The adapters
//! report to the global OpenTelemetry meter and tracer providers. If no provider is installed
//! the global providers are no-ops, so the adapters record nothing and never panic. Call
//! [`init`] to install an OTLP exporter configured from the standard `OTEL_*` variables.
//!
//! # Metrics
//!
//! All metrics are tagged with exactly two attributes, `marigold.program` and
//! `marigold.node_id`, which keeps cardinality bounded by the number of nodes in the program.
//! The source file and byte range of a node are attached to its span only. The range is
//! approximate: it indexes the text the code generator saw, which for the `m!` macro is the
//! stringified macro body rather than the Rust source file. Node ids are exact.
//!
//! | metric | kind | meaning |
//! | --- | --- | --- |
//! | `marigold.node.items_in` | counter | items that reached the node |
//! | `marigold.node.items_out` | counter | items the node produced |
//! | `marigold.node.runs` | counter | times the node's stream was started |
//! | `marigold.node.errors.item` | counter | `Err` items observed (only where items are `Result`) |
//! | `marigold.node.errors.abort` | counter | runs aborted by a panic |
//! | `marigold.node.duration` | histogram (s) | latency of a sampled `poll_next` call |
//!
//! Counts are accumulated locally and flushed every [`FLUSH_EVERY`] items, when the stream ends
//! and when it is dropped. The latency histogram records one `poll_next` in every
//! [`LATENCY_SAMPLE_EVERY`] that produced an item.

use core::pin::Pin;
use core::task::{Context, Poll};
use futures::Stream;
use opentelemetry::metrics::{Counter, Histogram, Meter};
use opentelemetry::trace::{Span, Status, Tracer};
use opentelemetry::{global, KeyValue};
use pin_project_lite::pin_project;
use std::marker::PhantomData;
use std::sync::OnceLock;
use std::time::Instant;

pub use opentelemetry;
pub use opentelemetry_sdk;

pub const METER_NAME: &str = "marigold";

pub const FLUSH_EVERY: u64 = 256;

pub const LATENCY_SAMPLE_EVERY: u64 = 64;

pub const PROGRAM_KEY: &str = "marigold.program";
pub const NODE_ID_KEY: &str = "marigold.node_id";
pub const FILE_KEY: &str = "marigold.file";
pub const RANGE_START_KEY: &str = "marigold.range.start";
pub const RANGE_END_KEY: &str = "marigold.range.end";

pub const ITEMS_IN: &str = "marigold.node.items_in";
pub const ITEMS_OUT: &str = "marigold.node.items_out";
pub const RUNS: &str = "marigold.node.runs";
pub const ITEM_ERRORS: &str = "marigold.node.errors.item";
pub const ABORT_ERRORS: &str = "marigold.node.errors.abort";
pub const DURATION: &str = "marigold.node.duration";

/// Source identity of one node of a Marigold program, embedded in generated code.
///
/// `start` and `end` are approximate byte offsets relative to the program text given to the code
/// generator, which for the `m!` macro is the stringified macro body.
///
/// ```
/// use marigold_impl::telemetry::NodeMeta;
///
/// let meta = NodeMeta::new("mg1-0123456789abcdef", "demo", "main.marigold", 0, 12);
/// assert_eq!(meta.node_id, "mg1-0123456789abcdef");
/// assert_eq!(meta.range(), 0..12);
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeMeta {
    pub node_id: &'static str,
    pub program: &'static str,
    pub file: &'static str,
    pub start: usize,
    pub end: usize,
}

impl NodeMeta {
    pub const fn new(
        node_id: &'static str,
        program: &'static str,
        file: &'static str,
        start: usize,
        end: usize,
    ) -> Self {
        Self {
            node_id,
            program,
            file,
            start,
            end,
        }
    }

    pub fn range(&self) -> core::ops::Range<usize> {
        self.start..self.end
    }
}

fn program_override() -> Option<&'static str> {
    static OVERRIDE: OnceLock<Option<&'static str>> = OnceLock::new();
    *OVERRIDE.get_or_init(|| {
        std::env::var("OTEL_SERVICE_NAME")
            .ok()
            .filter(|name| !name.is_empty())
            .map(|name| &*Box::leak(name.into_boxed_str()))
    })
}

/// The program identity reported as `marigold.program`: `OTEL_SERVICE_NAME` if set, otherwise
/// the name captured when the code was generated.
pub fn resolve_program(codegen_program: &'static str) -> &'static str {
    program_override().unwrap_or(codegen_program)
}

/// Decides whether an item counts as an error.
pub trait Probe<T> {
    fn is_error(item: &T) -> bool;
}

/// Items are never errors.
#[derive(Clone, Copy, Debug)]
pub struct AnyItem;

impl<T> Probe<T> for AnyItem {
    #[inline(always)]
    fn is_error(_item: &T) -> bool {
        false
    }
}

/// `Err` items are errors.
#[derive(Clone, Copy, Debug)]
pub struct ResultItem;

impl<T, E> Probe<Result<T, E>> for ResultItem {
    #[inline(always)]
    fn is_error(item: &Result<T, E>) -> bool {
        item.is_err()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Role {
    In,
    Out,
}

struct Instruments {
    items_in: Counter<u64>,
    items_out: Counter<u64>,
    runs: Counter<u64>,
    item_errors: Counter<u64>,
    abort_errors: Counter<u64>,
    duration: Histogram<f64>,
}

impl Instruments {
    fn new(meter: &Meter) -> Self {
        Self {
            items_in: meter
                .u64_counter(ITEMS_IN)
                .with_unit("{item}")
                .with_description("Items that reached the node")
                .build(),
            items_out: meter
                .u64_counter(ITEMS_OUT)
                .with_unit("{item}")
                .with_description("Items produced by the node")
                .build(),
            runs: meter
                .u64_counter(RUNS)
                .with_unit("{run}")
                .with_description("Times the node's stream was started")
                .build(),
            item_errors: meter
                .u64_counter(ITEM_ERRORS)
                .with_unit("{item}")
                .with_description("Err items observed at the node")
                .build(),
            abort_errors: meter
                .u64_counter(ABORT_ERRORS)
                .with_unit("{run}")
                .with_description("Runs of the node aborted by a panic")
                .build(),
            duration: meter
                .f64_histogram(DURATION)
                .with_unit("s")
                .with_description("Latency of sampled poll_next calls")
                .build(),
        }
    }
}

struct Recorder {
    meta: NodeMeta,
    role: Role,
    instruments: Instruments,
    attributes: [KeyValue; 2],
    pending_items: u64,
    pending_errors: u64,
    polls: u64,
    started: bool,
    finished: bool,
    in_poll: bool,
    span: Option<global::BoxedSpan>,
}

impl Recorder {
    fn new(meter: &Meter, meta: NodeMeta, role: Role) -> Self {
        let program = resolve_program(meta.program);
        Self {
            meta,
            role,
            instruments: Instruments::new(meter),
            attributes: [
                KeyValue::new(PROGRAM_KEY, program),
                KeyValue::new(NODE_ID_KEY, meta.node_id),
            ],
            pending_items: 0,
            pending_errors: 0,
            polls: 0,
            started: false,
            finished: false,
            in_poll: false,
            span: None,
        }
    }

    fn start_span(&mut self) {
        let tracer = global::tracer(METER_NAME);
        let mut span = tracer.start("marigold.node");
        span.set_attribute(self.attributes[0].clone());
        span.set_attribute(self.attributes[1].clone());
        span.set_attribute(KeyValue::new(FILE_KEY, self.meta.file));
        span.set_attribute(KeyValue::new(RANGE_START_KEY, self.meta.start as i64));
        span.set_attribute(KeyValue::new(RANGE_END_KEY, self.meta.end as i64));
        self.span = Some(span);
    }

    #[inline]
    fn before_poll(&mut self) -> Option<Instant> {
        if !self.started {
            self.started = true;
            if self.role == Role::Out {
                self.instruments.runs.add(1, &self.attributes);
                self.start_span();
            }
        }
        self.in_poll = true;
        let sampled = self.role == Role::Out && self.polls.is_multiple_of(LATENCY_SAMPLE_EVERY);
        self.polls += 1;
        sampled.then(Instant::now)
    }

    #[inline]
    fn on_item(&mut self, failed: bool, timer: Option<Instant>) {
        self.in_poll = false;
        self.pending_items += 1;
        self.pending_errors += u64::from(failed);
        if self.pending_items >= FLUSH_EVERY {
            self.flush();
        }
        if let Some(start) = timer {
            self.instruments
                .duration
                .record(start.elapsed().as_secs_f64(), &self.attributes);
        }
    }

    fn flush(&mut self) {
        if self.pending_items > 0 {
            let counter = match self.role {
                Role::In => &self.instruments.items_in,
                Role::Out => &self.instruments.items_out,
            };
            counter.add(self.pending_items, &self.attributes);
            self.pending_items = 0;
        }
        if self.pending_errors > 0 {
            self.instruments
                .item_errors
                .add(self.pending_errors, &self.attributes);
            self.pending_errors = 0;
        }
    }

    fn finish(&mut self) {
        self.in_poll = false;
        if self.finished {
            return;
        }
        self.finished = true;
        self.flush();
        if let Some(mut span) = self.span.take() {
            span.end();
        }
    }
}

impl Drop for Recorder {
    fn drop(&mut self) {
        if self.in_poll && std::thread::panicking() && self.role == Role::Out {
            self.instruments.abort_errors.add(1, &self.attributes);
            if let Some(span) = self.span.as_mut() {
                span.set_status(Status::error("panic while polling"));
            }
        }
        self.finish();
    }
}

pin_project! {
    /// Stream adapter that records OpenTelemetry measurements for one node.
    ///
    /// Items, ordering and `size_hint` of the wrapped stream are passed through unchanged.
    /// Build one with [`instrument`], [`instrument_in`], [`instrument_results`] or
    /// [`instrument_in_results`].
    pub struct Instrumented<S, P = AnyItem> {
        #[pin]
        inner: S,
        recorder: Recorder,
        probe: PhantomData<fn() -> P>,
    }
}

impl<S, P> Instrumented<S, P> {
    fn new(inner: S, recorder: Recorder) -> Self {
        Self {
            inner,
            recorder,
            probe: PhantomData,
        }
    }
}

impl<S, P> Stream for Instrumented<S, P>
where
    S: Stream,
    P: Probe<S::Item>,
{
    type Item = S::Item;

    #[inline]
    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.project();
        let timer = this.recorder.before_poll();
        let polled = this.inner.poll_next(cx);
        match &polled {
            Poll::Ready(Some(item)) => this.recorder.on_item(P::is_error(item), timer),
            Poll::Ready(None) => this.recorder.finish(),
            Poll::Pending => this.recorder.in_poll = false,
        }
        polled
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

fn build<S, P>(inner: S, meter: &Meter, meta: NodeMeta, role: Role) -> Instrumented<S, P> {
    Instrumented::new(inner, Recorder::new(meter, meta, role))
}

fn global_meter() -> Meter {
    global::meter(METER_NAME)
}

/// Record the items a node produces, its runs, latency, and aborts, with the global meter.
pub fn instrument<S: Stream>(stream: S, meta: NodeMeta) -> Instrumented<S, AnyItem> {
    build(stream, &global_meter(), meta, Role::Out)
}

/// Like [`instrument`], and also count `Err` items.
pub fn instrument_results<S: Stream>(stream: S, meta: NodeMeta) -> Instrumented<S, ResultItem> {
    build(stream, &global_meter(), meta, Role::Out)
}

/// Record the items that reach a node, with the global meter.
pub fn instrument_in<S: Stream>(stream: S, meta: NodeMeta) -> Instrumented<S, AnyItem> {
    build(stream, &global_meter(), meta, Role::In)
}

/// Like [`instrument_in`], and also count `Err` items.
pub fn instrument_in_results<S: Stream>(stream: S, meta: NodeMeta) -> Instrumented<S, ResultItem> {
    build(stream, &global_meter(), meta, Role::In)
}

/// [`instrument`] reporting to an explicit meter.
pub fn instrument_with<S: Stream>(
    stream: S,
    meta: NodeMeta,
    meter: &Meter,
) -> Instrumented<S, AnyItem> {
    build(stream, meter, meta, Role::Out)
}

/// [`instrument_results`] reporting to an explicit meter.
pub fn instrument_results_with<S: Stream>(
    stream: S,
    meta: NodeMeta,
    meter: &Meter,
) -> Instrumented<S, ResultItem> {
    build(stream, meter, meta, Role::Out)
}

/// [`instrument_in`] reporting to an explicit meter.
pub fn instrument_in_with<S: Stream>(
    stream: S,
    meta: NodeMeta,
    meter: &Meter,
) -> Instrumented<S, AnyItem> {
    build(stream, meter, meta, Role::In)
}

/// [`instrument_in_results`] reporting to an explicit meter.
pub fn instrument_in_results_with<S: Stream>(
    stream: S,
    meta: NodeMeta,
    meter: &Meter,
) -> Instrumented<S, ResultItem> {
    build(stream, meter, meta, Role::In)
}

/// Failure to set up the OTLP exporters.
#[derive(Debug)]
pub struct InitError(String);

impl core::fmt::Display for InitError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "could not initialise marigold telemetry: {}", self.0)
    }
}

impl std::error::Error for InitError {}

/// Keeps the installed OTLP providers alive. Dropping it flushes and shuts them down.
pub struct TelemetryGuard {
    meter_provider: opentelemetry_sdk::metrics::SdkMeterProvider,
    tracer_provider: opentelemetry_sdk::trace::SdkTracerProvider,
}

impl TelemetryGuard {
    /// Export everything recorded so far.
    pub fn flush(&self) {
        let _ = self.meter_provider.force_flush();
        let _ = self.tracer_provider.force_flush();
    }
}

impl Drop for TelemetryGuard {
    fn drop(&mut self) {
        let _ = self.meter_provider.shutdown();
        let _ = self.tracer_provider.shutdown();
    }
}

/// Install OTLP metric and trace exporters as the global providers.
///
/// The exporters and the SDK resource are configured by the OpenTelemetry SDK from the standard
/// variables, such as `OTEL_EXPORTER_OTLP_ENDPOINT`, `OTEL_EXPORTER_OTLP_HEADERS`,
/// `OTEL_SERVICE_NAME` and `OTEL_RESOURCE_ATTRIBUTES`. The protocol is OTLP over HTTP with protobuf
/// bodies. Keep the returned guard alive for as long as telemetry should be exported.
///
/// Marigold never calls this on its own.
pub fn init() -> Result<TelemetryGuard, InitError> {
    use opentelemetry_otlp::{MetricExporter, SpanExporter};
    use opentelemetry_sdk::metrics::SdkMeterProvider;
    use opentelemetry_sdk::trace::SdkTracerProvider;

    let metric_exporter = MetricExporter::builder()
        .with_http()
        .build()
        .map_err(|e| InitError(e.to_string()))?;
    let span_exporter = SpanExporter::builder()
        .with_http()
        .build()
        .map_err(|e| InitError(e.to_string()))?;

    let meter_provider = SdkMeterProvider::builder()
        .with_periodic_exporter(metric_exporter)
        .build();
    let tracer_provider = SdkTracerProvider::builder()
        .with_batch_exporter(span_exporter)
        .build();

    global::set_meter_provider(meter_provider.clone());
    global::set_tracer_provider(tracer_provider.clone());

    Ok(TelemetryGuard {
        meter_provider,
        tracer_provider,
    })
}
