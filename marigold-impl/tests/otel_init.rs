#![cfg(feature = "otel")]

use futures::StreamExt;
use marigold_impl::telemetry::{init, instrument, NodeMeta};

#[tokio::test(flavor = "multi_thread")]
async fn init_with_unreachable_endpoint_does_not_panic_inside_a_runtime() {
    std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", "http://127.0.0.1:9");
    std::env::set_var("OTEL_SERVICE_NAME", "marigold-init-test");
    let guard = init().expect("exporters should build without contacting the endpoint");

    let meta = NodeMeta::new("mg1-fedcba0987654321", "init_demo", "init.marigold", 0, 5);
    let out: Vec<u32> = instrument(futures::stream::iter(0..1000u32), meta)
        .collect()
        .await;
    assert_eq!(out.len(), 1000);

    guard.flush();
    drop(guard);
}
