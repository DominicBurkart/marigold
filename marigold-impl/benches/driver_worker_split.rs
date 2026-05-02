use futures::StreamExt;
use marigold_impl::keep_first_n::KeepFirstN;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tracing::subscriber::set_global_default;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::Layer;

fn compare_by_sum(a: &[u16; 3], b: &[u16; 3]) -> Ordering {
    let sa: u32 = a.iter().map(|&x| x as u32).sum();
    let sb: u32 = b.iter().map(|&x| x as u32).sum();
    sa.cmp(&sb)
}

#[derive(Default)]
struct SpanState {
    enters: HashMap<tracing::Id, Instant>,
    totals: HashMap<String, Duration>,
    counts: HashMap<String, u64>,
}

struct TimingLayer(Arc<Mutex<SpanState>>);

impl<S> Layer<S> for TimingLayer
where
    S: tracing::Subscriber + for<'lookup> tracing_subscriber::registry::LookupSpan<'lookup>,
{
    fn on_enter(&self, id: &tracing::Id, _ctx: tracing_subscriber::layer::Context<'_, S>) {
        let mut state = self.0.lock().unwrap();
        state.enters.insert(id.clone(), Instant::now());
    }

    fn on_exit(&self, id: &tracing::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        let now = Instant::now();
        let mut state = self.0.lock().unwrap();
        if let Some(start) = state.enters.remove(id) {
            if let Some(span) = ctx.span(id) {
                let name = span.name().to_string();
                *state.totals.entry(name.clone()).or_insert(Duration::ZERO) += now - start;
                *state.counts.entry(name).or_insert(0) += 1;
            }
        }
    }
}

fn main() {
    let state = Arc::new(Mutex::new(SpanState::default()));
    let timing_layer = TimingLayer(state.clone());
    let subscriber = tracing_subscriber::registry().with(timing_layer);
    set_global_default(subscriber).expect("failed to set tracing subscriber");

    let rt = tokio::runtime::Runtime::new().unwrap();

    let items: Vec<[u16; 3]> = {
        let mut v = Vec::with_capacity(22_238_720);
        for a in 0u16..512 {
            for b in (a + 1)..512 {
                for c in (b + 1)..512 {
                    v.push([a, b, c]);
                }
            }
        }
        v
    };

    eprintln!("dataset: {} items", items.len());

    let wall_start = Instant::now();
    rt.block_on(async move {
        futures::stream::iter(items)
            .keep_first_n(20, compare_by_sum)
            .await
            .collect::<Vec<_>>()
            .await
    });
    let total_wall = wall_start.elapsed();

    let state = state.lock().unwrap();

    let worker_total_cpu = state
        .totals
        .get("keep_first_n_worker_task")
        .copied()
        .unwrap_or(Duration::ZERO);
    let worker_task_count = state
        .counts
        .get("keep_first_n_worker_task")
        .copied()
        .unwrap_or(0);
    let parallel_wall = state
        .totals
        .get("keep_first_n_parallel_section")
        .copied()
        .unwrap_or(Duration::ZERO);
    let driver_wall = total_wall.saturating_sub(parallel_wall);

    println!("driver_wall_time_s:      {:.3}", driver_wall.as_secs_f64());
    println!(
        "total_worker_cpu_s:      {:.3}  ({} tasks)",
        worker_total_cpu.as_secs_f64(),
        worker_task_count
    );
    println!(
        "worker_wall_time_s:      {:.3}",
        parallel_wall.as_secs_f64()
    );
    println!("total_wall_time_s:       {:.3}", total_wall.as_secs_f64());
    if parallel_wall.as_secs_f64() > 0.0 {
        println!(
            "effective_parallelism:   {:.2}x  (worker_cpu / parallel_wall)",
            worker_total_cpu.as_secs_f64() / parallel_wall.as_secs_f64()
        );
    }
}
