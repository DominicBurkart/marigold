use cpu_time::ProcessTime;
use futures::StreamExt;
use marigold_impl::keep_first_n::KeepFirstN;
use std::time::Instant;

fn cpu_work(v: u64) -> u64 {
    (0u64..500).fold(v, |acc, i| {
        acc.wrapping_add(i).wrapping_mul(v.wrapping_add(1))
    })
}

fn measure<F: FnOnce() -> ()>(label: &str, f: F) -> f64 {
    let cpu_start = ProcessTime::now();
    let wall_start = Instant::now();
    f();
    let cpu_elapsed = cpu_start.elapsed();
    let wall_elapsed = wall_start.elapsed();
    let effective_cores = cpu_elapsed.as_secs_f64() / wall_elapsed.as_secs_f64();
    eprintln!(
        "{}: wall={:.2}s  cpu={:.2}s  effective_cores={:.2}",
        label,
        wall_elapsed.as_secs_f64(),
        cpu_elapsed.as_secs_f64(),
        effective_cores,
    );
    effective_cores
}

fn main() {
    let rt = tokio::runtime::Runtime::new().unwrap();
    const N: usize = 1_000_000;
    const CONCURRENCY: usize = 256;
    const ITERS: u32 = 3;

    println!("=== map/filter CPU utilization ===");
    println!("items={N}  concurrency={CONCURRENCY}  iters={ITERS}");
    println!();

    for scenario in [
        "buffered_map",
        "spawn_buffer_unordered",
        "filter_keep_first_n",
    ] {
        let mut samples = Vec::with_capacity(ITERS as usize);

        for i in 0..ITERS {
            let label = format!("{scenario}[{i}]");
            let cores = match scenario {
                "buffered_map" => measure(&label, || {
                    rt.block_on(async {
                        futures::stream::iter(0u64..N as u64)
                            .map(|v| async move { cpu_work(v) })
                            .buffered(CONCURRENCY)
                            .for_each(|_| async {})
                            .await
                    });
                }),
                "spawn_buffer_unordered" => measure(&label, || {
                    rt.block_on(async {
                        futures::stream::iter(0u64..N as u64)
                            .map(|v| tokio::spawn(async move { cpu_work(v) }))
                            .buffer_unordered(CONCURRENCY)
                            .for_each(|_| async {})
                            .await
                    });
                }),
                "filter_keep_first_n" => measure(&label, || {
                    rt.block_on(async {
                        Box::pin(
                            futures::stream::iter(0u64..N as u64).filter_map(|v| async move {
                                let r = cpu_work(v);
                                if r % 2 == 0 {
                                    Some(r)
                                } else {
                                    None
                                }
                            }),
                        )
                        .keep_first_n(20, |a: &u64, b: &u64| a.cmp(b))
                        .await
                        .for_each(|_| async {})
                        .await
                    });
                }),
                _ => unreachable!(),
            };
            samples.push(cores);
        }

        let mean = samples.iter().sum::<f64>() / ITERS as f64;
        let variance = samples.iter().map(|&x| (x - mean).powi(2)).sum::<f64>() / ITERS as f64;
        println!(
            "{scenario}: mean_effective_cores={mean:.2} stddev={:.2}",
            variance.sqrt()
        );
        println!();
    }
}
