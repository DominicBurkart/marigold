# Benchmark Results — Issue #82 Raw Data

Measurements produced by running the benchmarks on branch `feat/benchmarks-keep-first-n`
at commit `a1bd575f7c2376190a40ecfb962497a095ccda0e` (HEAD as of 2026-03-29).

**Machine:** 4-core Linux x86_64 (this run)
**Original issue measurements:** 16-core Linux x86_64, commit `b2df213`

---

## keep_first_n wall time (`benches/keep_first_n.rs`)

| Metric | This run (4-core) | Issue #82 before (3d020a1, 16-core) | Issue #82 after (b2df213, 16-core) |
|---|---|---|---|
| `keep_first_n/c_512_3_stream_iter` mean | 1,779,909,008 ns (1.780 s) | 7,916,714,033 ns (7.92 s) | 1,419,443,994 ns (1.42 s) |

---

## Per-item costs (`benches/per_item_costs.rs`)

| Benchmark | Mean (ns) | Category |
|---|---|---|
| `tokio_spawn_join` | 40,679 | overhead |
| `stream_advance_enumerate_next` | 96 | overhead |
| `arc_clone_x2` | 27 | overhead |
| `joinhandle_poll_completed` | 32,176 | overhead |
| `rwlock_read_compare` | 17 | overhead |
| `mutex_heap_peek_pop_push` | 25 | overhead |
| `compare_by_sum_array3` | 0.16 | useful work |

---

## CPU utilization (`benches/cpu_utilization.rs`, stdout → `cpu_utilization.txt`)

| Metric | This run (4-core) | Issue #82 before (3d020a1) | Issue #82 after (b2df213) |
|---|---|---|---|
| effective_cores mean | 3.57 | ~2.82 | ~13.31 |
| effective_cores stddev | 0.04 | — | — |

---

## Driver / worker time split (`benches/driver_worker_split.rs`, stdout → `driver_worker_split.txt`)

| Metric | This run (4-core) | Issue #82 (b2df213, 16-core) |
|---|---|---|
| `driver_wall_time_s` | 1.708 | 1.562 |
| `total_worker_cpu_s` | 6.717 | 26.457 |
| `worker_wall_time_s` | 0.232 | — |
| `total_wall_time_s` | 1.940 | — |
| `effective_parallelism` | 28.95× | 13.31× |
| worker tasks | 86,870 | — |

---

## Pipeline stage breakdown (`benches/pipeline_stages.rs`)

| Benchmark | Mean (ns) | Mean (human) |
|---|---|---|
| `pipeline_stages/range_8_only` | 128.8 | 129 ns |
| `pipeline_stages/permutations_with_replacement_8_k3` | 21,075,430 | 21.1 µs |
| `pipeline_stages/combinations_512_k3` | 4,315,816,055 | 4.316 s |
| `pipeline_stages/full_pipeline_range8_pwr3_comb3_kfn20` | 12,491,103,211 | 12.491 s |

---

## color-palette-picker end-to-end (`examples/color-palette-picker/benches/color_picker_bench.rs`)

| Benchmark | Mean (ns) | Mean (human) |
|---|---|---|
| `bench_color_picker` | 1,319,412,931 | 1.319 s |
| `range_8_pipeline/pwr3_comb3_kfn20_compare_by_sum` | 1,587,680,551 | 1.588 s |

---

## Derived projections

Projection uses `range_8_pipeline` throughput against ~463 trillion full-scale palettes.

| Metric | This run (4-core) |
|---|---|
| Benchmark items | 22,238,720 |
| Benchmark wall time | 1.780 s |
| Throughput | ~12,494,000 items/sec |
| Full-scale items | ~463,000,000,000,000 |
| Projected wall time | ~37,056,000 s (~429 days) |
