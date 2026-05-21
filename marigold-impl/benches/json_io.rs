//! Criterion benches for the `json` and `jsonl` read/write helpers.
//!
//! These measure the round-trip throughput for 1_000-item payloads in
//! both formats. They are intentionally small (1k items, 10-sample runs)
//! to keep CI time bounded while still producing a stable baseline we can
//! regression-check against.

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use futures::StreamExt;
use marigold_impl::json_io::{read_json_array, read_jsonl, JsonArrayWriter, JsonlWriter};
use serde::{Deserialize, Serialize};
use std::time::Duration;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Row {
    id: u32,
    name: String,
    value: f64,
}

fn make_rows(n: usize) -> Vec<Row> {
    (0..n)
        .map(|i| Row {
            id: i as u32,
            name: format!("row_{i}"),
            value: (i as f64) * 0.5,
        })
        .collect()
}

fn bench_json_io(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let rows = make_rows(1_000);

    // Pre-built payloads used in read benches.
    let jsonl_payload: Vec<u8> = rt.block_on(async {
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut w = JsonlWriter::new(&mut buf);
            for r in &rows {
                w.write(r).await.unwrap();
            }
            w.shutdown().await.unwrap();
        }
        buf
    });
    let json_array_payload: Vec<u8> = rt.block_on(async {
        let mut buf: Vec<u8> = Vec::new();
        {
            let mut w = JsonArrayWriter::new(&mut buf);
            for r in &rows {
                w.write(r).await.unwrap();
            }
            w.shutdown().await.unwrap();
        }
        buf
    });

    let mut group = c.benchmark_group("json_io");
    group.sample_size(10);
    group.measurement_time(Duration::from_secs(10));

    group.bench_function("bench_jsonl_write_1k", |b| {
        b.iter_batched(
            || rows.clone(),
            |rs| {
                rt.block_on(async move {
                    let mut buf: Vec<u8> = Vec::new();
                    {
                        let mut w = JsonlWriter::new(&mut buf);
                        for r in &rs {
                            w.write(r).await.unwrap();
                        }
                        w.shutdown().await.unwrap();
                    }
                    buf
                })
            },
            BatchSize::PerIteration,
        )
    });

    group.bench_function("bench_jsonl_read_1k", |b| {
        b.iter_batched(
            || jsonl_payload.clone(),
            |buf| {
                rt.block_on(async move {
                    let reader = std::io::Cursor::new(buf);
                    let _items: Vec<Row> = read_jsonl::<_, Row>(reader)
                        .map(|r| r.expect("json row"))
                        .collect()
                        .await;
                })
            },
            BatchSize::PerIteration,
        )
    });

    group.bench_function("bench_json_array_write_1k", |b| {
        b.iter_batched(
            || rows.clone(),
            |rs| {
                rt.block_on(async move {
                    let mut buf: Vec<u8> = Vec::new();
                    {
                        let mut w = JsonArrayWriter::new(&mut buf);
                        for r in &rs {
                            w.write(r).await.unwrap();
                        }
                        w.shutdown().await.unwrap();
                    }
                    buf
                })
            },
            BatchSize::PerIteration,
        )
    });

    group.bench_function("bench_json_array_read_1k", |b| {
        b.iter_batched(
            || json_array_payload.clone(),
            |buf| {
                rt.block_on(async move {
                    let reader = std::io::Cursor::new(buf);
                    let _items: Vec<Row> = read_json_array::<_, Row>(reader)
                        .map(|r| r.expect("json row"))
                        .collect()
                        .await;
                })
            },
            BatchSize::PerIteration,
        )
    });

    group.finish();
}

criterion_group!(benches, bench_json_io);
criterion_main!(benches);
