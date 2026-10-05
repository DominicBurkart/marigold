use opentelemetry::global;

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
