use cpu_time::ProcessTime;
use futures::StreamExt;
use marigold_impl::keep_first_n::KeepFirstN;
use std::cmp::Ordering;
use std::time::Instant;

fn compare_by_sum(a: &[u16; 3], b: &[u16; 3]) -> Ordering {
    let sa: u32 = a.iter().map(|&x| x as u32).sum();
    let sb: u32 = b.iter().map(|&x| x as u32).sum();
    sa.cmp(&sb)
}

fn main() {
    let rt = tokio::runtime::Runtime::new().unwrap();

    const ITERS: u32 = 3;
    let mut effective_cores_samples = Vec::with_capacity(ITERS as usize);

    for i in 0..ITERS {
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

        let cpu_start = ProcessTime::now();
        let wall_start = Instant::now();

        rt.block_on(async move {
            futures::stream::iter(items)
                .keep_first_n(20, compare_by_sum)
                .await
                .collect::<Vec<_>>()
                .await
        });

        let cpu_elapsed = cpu_start.elapsed();
        let wall_elapsed = wall_start.elapsed();

        let effective_cores = cpu_elapsed.as_secs_f64() / wall_elapsed.as_secs_f64();
        effective_cores_samples.push(effective_cores);

        eprintln!(
            "iter {}: wall={:.2}s  cpu={:.2}s  effective_cores={:.2}",
            i,
            wall_elapsed.as_secs_f64(),
            cpu_elapsed.as_secs_f64(),
            effective_cores,
        );
    }

    let mean = effective_cores_samples.iter().sum::<f64>() / ITERS as f64;
    let variance = effective_cores_samples
        .iter()
        .map(|&x| (x - mean).powi(2))
        .sum::<f64>()
        / ITERS as f64;
    let stddev = variance.sqrt();

    println!("effective_cores: mean={mean:.2} stddev={stddev:.2}");
}
