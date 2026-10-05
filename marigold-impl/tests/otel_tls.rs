#![cfg(feature = "otel-otlp")]

use futures::executor::block_on;
use futures::StreamExt;
use marigold_impl::telemetry::{init, instrument, NodeMeta};
use std::io::Read;
use std::net::TcpListener;
use std::sync::mpsc;
use std::time::Duration;

#[test]
fn https_endpoints_start_a_tls_handshake_and_http_endpoints_do_not() {
    for (scheme, expect_tls) in [("https", true), ("http", false)] {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let port = listener.local_addr().unwrap().port();
        let (tx, rx) = mpsc::channel();
        std::thread::spawn(move || {
            let (mut stream, _) = listener.accept().unwrap();
            stream
                .set_read_timeout(Some(Duration::from_secs(5)))
                .unwrap();
            let mut first = [0u8; 4];
            let n = stream.read(&mut first).unwrap_or(0);
            tx.send(first[..n].to_vec()).unwrap();
        });

        std::env::set_var(
            "OTEL_EXPORTER_OTLP_ENDPOINT",
            format!("{scheme}://127.0.0.1:{port}"),
        );
        std::env::set_var("OTEL_EXPORTER_OTLP_TIMEOUT", "2000");
        let guard = init().expect("exporters build for both schemes");
        let meta = NodeMeta::new("mg1-0000000000000001", "tls_demo", "t.marigold", 0, 1);
        let out: Vec<u32> = block_on(instrument(futures::stream::iter(0..10u32), meta).collect());
        assert_eq!(out.len(), 10);
        guard.flush();

        let first = rx.recv_timeout(Duration::from_secs(20)).unwrap();
        if expect_tls {
            assert_eq!(first.first(), Some(&0x16), "{first:?}");
        } else {
            assert_eq!(first.as_slice(), b"POST", "{first:?}");
        }
        drop(guard);
    }
}
