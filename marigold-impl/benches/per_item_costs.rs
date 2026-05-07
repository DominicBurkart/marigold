use criterion::{criterion_group, criterion_main, Criterion};
use futures::StreamExt;
use parking_lot::{Mutex, RwLock};
use std::collections::BinaryHeap;
use std::sync::Arc;

fn bench_spawn_join(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("tokio_spawn_join", |b| {
        b.iter(|| rt.block_on(async { tokio::spawn(async {}).await.unwrap() }))
    });
}

fn bench_stream_advance(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("stream_advance_enumerate_next", |b| {
        b.iter(|| {
            rt.block_on(async {
                futures::stream::iter(std::iter::once(1u32))
                    .enumerate()
                    .next()
                    .await
            })
        })
    });
}

fn bench_arc_clone(c: &mut Criterion) {
    let heap: Arc<Mutex<BinaryHeap<(usize, u32)>>> =
        Arc::new(Mutex::new(BinaryHeap::with_capacity(16)));
    let rw: Arc<RwLock<(usize, u32)>> = Arc::new(RwLock::new((0, 42u32)));

    c.bench_function("arc_clone_x2", |b| {
        b.iter(|| {
            let _h = heap.clone();
            let _r = rw.clone();
        })
    });
}

fn bench_rwlock_read_compare(c: &mut Criterion) {
    let rw: Arc<RwLock<(usize, u32)>> = Arc::new(RwLock::new((0, 42u32)));
    c.bench_function("rwlock_read_compare", |b| {
        b.iter(|| {
            let guard = rw.read();
            let result = guard.1.cmp(&100u32);
            drop(guard);
            result
        })
    });
}

fn bench_mutex_heap_ops(c: &mut Criterion) {
    let heap: Arc<Mutex<BinaryHeap<(usize, u32)>>> = Arc::new(Mutex::new({
        let mut h = BinaryHeap::with_capacity(8);
        for i in 0u32..5 {
            h.push((i as usize, i));
        }
        h
    }));

    c.bench_function("mutex_heap_peek_pop_push", |b| {
        b.iter(|| {
            let mut guard = heap.lock();
            let _ = guard.peek().copied();
            let _ = guard.pop();
            guard.push((999, 42u32));
        })
    });
}

fn bench_joinhandle_poll(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    c.bench_function("joinhandle_poll_completed", |b| {
        b.iter_batched(
            || {
                rt.block_on(async {
                    let handle = tokio::spawn(async {});
                    tokio::task::yield_now().await;
                    handle
                })
            },
            |handle| rt.block_on(async { handle.await.unwrap() }),
            criterion::BatchSize::PerIteration,
        )
    });
}

fn bench_comparator_work(c: &mut Criterion) {
    use std::cmp::Ordering;
    fn compare_by_sum(a: &[u16; 3], b: &[u16; 3]) -> Ordering {
        let sa: u32 = a.iter().map(|&x| x as u32).sum();
        let sb: u32 = b.iter().map(|&x| x as u32).sum();
        sa.cmp(&sb)
    }

    let a = [100u16, 200, 300];
    let b = [150u16, 250, 350];
    c.bench_function("compare_by_sum_array3", |b_bench| {
        b_bench.iter(|| compare_by_sum(&a, &b))
    });
}

criterion_group!(
    benches,
    bench_spawn_join,
    bench_stream_advance,
    bench_arc_clone,
    bench_rwlock_read_compare,
    bench_mutex_heap_ops,
    bench_joinhandle_poll,
    bench_comparator_work
);
criterion_main!(benches);
