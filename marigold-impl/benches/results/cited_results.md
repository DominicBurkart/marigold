# Benchmark Results — Issue #82 Cited Data

Each value in this file carries a GitHub permalink to the exact benchmark function
that produced it. Criterion artifacts are in `criterion/*/new/estimates.json` alongside
this file. Custom-harness outputs are in `cpu_utilization.txt` and `driver_worker_split.txt`.

**Run commit:** [`a1bd575`](https://github.com/DominicBurkart/marigold/commit/a1bd575f7c2376190a40ecfb962497a095ccda0e)
(HEAD of `feat/benchmarks-keep-first-n` as of 2026-03-29, 4-core Linux x86_64)

**Issue #82 numbers** were measured on a 16-core machine at commit
[`b2df213`](https://github.com/DominicBurkart/marigold/commit/b2df213)
(after optimization) and
[`3d020a1`](https://github.com/DominicBurkart/marigold/commit/3d020a1)
(before optimization). Per-core count differences explain the throughput and
effective-parallelism deltas between this file and the issue.

---

## keep_first_n wall time

Artifact: [`criterion/keep_first_n/c_512_3_stream_iter/new/estimates.json`](criterion/keep_first_n/c_512_3_stream_iter/new/estimates.json)

| Metric | This run (4-core, a1bd575) | Issue #82 before (3d020a1, 16-core) | Issue #82 after (b2df213, 16-core) | Source |
|---|---|---|---|---|
| `keep_first_n/c_512_3_stream_iter` mean | 1,779,909,008 ns (1.780 s) | 7,916,714,033 ns (7.92 s) | 1,419,443,994 ns (1.42 s) | [`keep_first_n.rs` L33–L47](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/keep_first_n.rs#L33-L47) |

---

## CPU utilization (effective parallelism)

Artifact: [`cpu_utilization.txt`](cpu_utilization.txt)

| Metric | This run (4-core) | Issue #82 before (3d020a1) | Issue #82 after (b2df213) | Source |
|---|---|---|---|---|
| effective_cores mean | 3.57 | ~2.82 | ~13.31 | [`cpu_utilization.rs` L46](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/cpu_utilization.rs#L46) |
| effective_cores stddev | 0.04 | — | — | [`cpu_utilization.rs` L58–L66](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/cpu_utilization.rs#L58-L66) |

---

## Per-item cost breakdown

Artifacts: `criterion/{bench_name}/new/estimates.json` (one file per row)

| Benchmark | Mean (ns) | Category | Artifact | Source |
|---|---|---|---|---|
| `tokio_spawn_join` | 40,679 | overhead | [`criterion/tokio_spawn_join/new/estimates.json`](criterion/tokio_spawn_join/new/estimates.json) | [`per_item_costs.rs` L7–L12](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/per_item_costs.rs#L7-L12) |
| `stream_advance_enumerate_next` | 96 | overhead | [`criterion/stream_advance_enumerate_next/new/estimates.json`](criterion/stream_advance_enumerate_next/new/estimates.json) | [`per_item_costs.rs` L14–L26](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/per_item_costs.rs#L14-L26) |
| `arc_clone_x2` | 27 | overhead | [`criterion/arc_clone_x2/new/estimates.json`](criterion/arc_clone_x2/new/estimates.json) | [`per_item_costs.rs` L28–L39](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/per_item_costs.rs#L28-L39) |
| `rwlock_read_compare` | 17 | overhead | [`criterion/rwlock_read_compare/new/estimates.json`](criterion/rwlock_read_compare/new/estimates.json) | [`per_item_costs.rs` L41–L51](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/per_item_costs.rs#L41-L51) |
| `mutex_heap_peek_pop_push` | 25 | overhead | [`criterion/mutex_heap_peek_pop_push/new/estimates.json`](criterion/mutex_heap_peek_pop_push/new/estimates.json) | [`per_item_costs.rs` L53–L70](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/per_item_costs.rs#L53-L70) |
| `joinhandle_poll_completed` | 32,176 | overhead | [`criterion/joinhandle_poll_completed/new/estimates.json`](criterion/joinhandle_poll_completed/new/estimates.json) | [`per_item_costs.rs` L72–L87](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/per_item_costs.rs#L72-L87) |
| `compare_by_sum_array3` | 0.16 | useful work | [`criterion/compare_by_sum_array3/new/estimates.json`](criterion/compare_by_sum_array3/new/estimates.json) | [`per_item_costs.rs` L89–L102](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/per_item_costs.rs#L89-L102) |

---

## Driver / worker time split

Artifact: [`driver_worker_split.txt`](driver_worker_split.txt)

| Metric | This run (4-core) | Issue #82 (b2df213, 16-core) | Source |
|---|---|---|---|
| `driver_wall_time_s` | 1.708 | 1.562 | [`driver_worker_split.rs` L99](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/driver_worker_split.rs#L99) |
| `total_worker_cpu_s` | 6.717 | 26.457 | [`driver_worker_split.rs` L100–L104](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/driver_worker_split.rs#L100-L104) |
| `worker_wall_time_s` | 0.232 | — | [`driver_worker_split.rs` L105–L108](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/driver_worker_split.rs#L105-L108) |
| `total_wall_time_s` | 1.940 | — | [`driver_worker_split.rs` L109](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/driver_worker_split.rs#L109) |
| `effective_parallelism` | 28.95× | — | [`driver_worker_split.rs` L110–L115](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/driver_worker_split.rs#L110-L115) |
| worker tasks | 86,870 | — | — |

---

## Pipeline stage breakdown

Artifacts: `criterion/pipeline_stages/*/new/estimates.json`

| Benchmark | Mean (ns) | Mean (human) | Artifact | Source |
|---|---|---|---|---|
| `range_8_only` | 128.8 | 129 ns | [`criterion/pipeline_stages/range_8_only/new/estimates.json`](criterion/pipeline_stages/range_8_only/new/estimates.json) | [`pipeline_stages.rs` L35–L37](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/pipeline_stages.rs#L35-L37) |
| `permutations_with_replacement_8_k3` | 21,075,430 | 21.1 µs | [`criterion/pipeline_stages/permutations_with_replacement_8_k3/new/estimates.json`](criterion/pipeline_stages/permutations_with_replacement_8_k3/new/estimates.json) | [`pipeline_stages.rs` L39–L49](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/pipeline_stages.rs#L39-L49) |
| `combinations_512_k3` | 4,315,816,055 | 4.316 s | [`criterion/pipeline_stages/combinations_512_k3/new/estimates.json`](criterion/pipeline_stages/combinations_512_k3/new/estimates.json) | [`pipeline_stages.rs` L53–L67](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/pipeline_stages.rs#L53-L67) |
| `full_pipeline_range8_pwr3_comb3_kfn20` | 12,491,103,211 | 12.491 s | [`criterion/pipeline_stages/full_pipeline_range8_pwr3_comb3_kfn20/new/estimates.json`](criterion/pipeline_stages/full_pipeline_range8_pwr3_comb3_kfn20/new/estimates.json) | [`pipeline_stages.rs` L71–L85](https://github.com/DominicBurkart/marigold/blob/b2df213/marigold-impl/benches/pipeline_stages.rs#L71-L85) |

---

## color-palette-picker end-to-end

Artifacts: `criterion/bench_color_picker/`, `criterion/range_8_pipeline/`

| Benchmark | Mean (ns) | Mean (human) | Artifact | Source |
|---|---|---|---|---|
| `bench_color_picker` | 1,319,412,931 | 1.319 s | [`criterion/bench_color_picker/new/estimates.json`](criterion/bench_color_picker/new/estimates.json) | [`color_picker_bench.rs` L17–L36](https://github.com/DominicBurkart/marigold/blob/b2df213/examples/color-palette-picker/benches/color_picker_bench.rs#L17-L36) |
| `range_8_pipeline/pwr3_comb3_kfn20_compare_by_sum` | 1,587,680,551 | 1.588 s | [`criterion/range_8_pipeline/pwr3_comb3_kfn20_compare_by_sum/new/estimates.json`](criterion/range_8_pipeline/pwr3_comb3_kfn20_compare_by_sum/new/estimates.json) | [`color_picker_bench.rs` L38–L60](https://github.com/DominicBurkart/marigold/blob/b2df213/examples/color-palette-picker/benches/color_picker_bench.rs#L38-L60) |
