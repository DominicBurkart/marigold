# Benchmark Results — Issue #82 Cited Data (16-core)

Each value in this file carries a GitHub permalink to the exact benchmark function
that produced it. Criterion artifacts are in `criterion/*/new/estimates.json` alongside
this file. Custom-harness outputs are in `cpu_utilization.txt` and `driver_worker_split.txt`.

**Run commit:** [`a1bd575`](https://github.com/DominicBurkart/marigold/commit/a1bd575f7c2376190a40ecfb962497a095ccda0e)
(HEAD of `feat/benchmarks-keep-first-n`, clean detached worktree, 16-core Linux x86_64)

**Machine info:**
```
nproc: 16
Architecture:            x86_64
CPU op-mode(s):          32-bit, 64-bit
Address sizes:           48 bits physical, 48 bits virtual
Byte Order:              Little Endian
CPU(s):                  16
```

**Issue #82 numbers** were measured on a 16-core machine at commit
[`b2df213`](https://github.com/DominicBurkart/marigold/commit/b2df213)
(after optimization) and
[`3d020a1`](https://github.com/DominicBurkart/marigold/commit/3d020a1)
(before optimization).

---

## keep_first_n wall time

Artifact: [`criterion/keep_first_n/c_512_3_stream_iter/new/estimates.json`](criterion/keep_first_n/c_512_3_stream_iter/new/estimates.json)

| Metric | This run (16-core, a1bd575) | Issue #82 after (b2df213, 16-core) | 4-core run (a1bd575) | Source |
|---|---|---|---|---|
| `keep_first_n/c_512_3_stream_iter` mean | 1,256,145,187 ns (1.256 s) | 1,419,443,994 ns (1.42 s) | 1,779,909,008 ns (1.780 s) | [`keep_first_n.rs` L33–L47](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/keep_first_n.rs#L33-L47) |

---

## CPU utilization (effective parallelism)

Artifact: [`cpu_utilization.txt`](cpu_utilization.txt)

| Metric | This run (16-core) | Issue #82 before (3d020a1) | Issue #82 after (b2df213) | 4-core run (a1bd575) | Source |
|---|---|---|---|---|---|
| effective_cores mean | 14.62 | ~2.82 | ~13.31 | 3.57 | [`cpu_utilization.rs` L46](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/cpu_utilization.rs#L46) |
| effective_cores stddev | 0.19 | — | — | 0.04 | [`cpu_utilization.rs` L58–L66](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/cpu_utilization.rs#L58-L66) |

---

## Per-item cost breakdown

Artifacts: `criterion/{bench_name}/new/estimates.json` (one file per row)

| Benchmark | This run (16-core, ns) | 4-core run (ns) | Category | Artifact | Source |
|---|---|---|---|---|---|
| `tokio_spawn_join` | 4,462 | 40,679 | overhead | [`criterion/tokio_spawn_join/new/estimates.json`](criterion/tokio_spawn_join/new/estimates.json) | [`per_item_costs.rs` L7–L12](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/per_item_costs.rs#L7-L12) |
| `stream_advance_enumerate_next` | 73.0 | 96 | overhead | [`criterion/stream_advance_enumerate_next/new/estimates.json`](criterion/stream_advance_enumerate_next/new/estimates.json) | [`per_item_costs.rs` L14–L26](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/per_item_costs.rs#L14-L26) |
| `arc_clone_x2` | 20.2 | 27 | overhead | [`criterion/arc_clone_x2/new/estimates.json`](criterion/arc_clone_x2/new/estimates.json) | [`per_item_costs.rs` L28–L39](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/per_item_costs.rs#L28-L39) |
| `rwlock_read_compare` | 8.3 | 17 | overhead | [`criterion/rwlock_read_compare/new/estimates.json`](criterion/rwlock_read_compare/new/estimates.json) | [`per_item_costs.rs` L41–L51](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/per_item_costs.rs#L41-L51) |
| `mutex_heap_peek_pop_push` | 14.2 | 25 | overhead | [`criterion/mutex_heap_peek_pop_push/new/estimates.json`](criterion/mutex_heap_peek_pop_push/new/estimates.json) | [`per_item_costs.rs` L53–L70](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/per_item_costs.rs#L53-L70) |
| `joinhandle_poll_completed` | 3,287 | 32,176 | overhead | [`criterion/joinhandle_poll_completed/new/estimates.json`](criterion/joinhandle_poll_completed/new/estimates.json) | [`per_item_costs.rs` L72–L87](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/per_item_costs.rs#L72-L87) |
| `compare_by_sum_array3` | 0.10 | 0.16 | useful work | [`criterion/compare_by_sum_array3/new/estimates.json`](criterion/compare_by_sum_array3/new/estimates.json) | [`per_item_costs.rs` L89–L102](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/per_item_costs.rs#L89-L102) |

---

## Driver / worker time split

Artifact: [`driver_worker_split.txt`](driver_worker_split.txt)

| Metric | This run (16-core) | Issue #82 (b2df213, 16-core) | 4-core run (a1bd575) | Source |
|---|---|---|---|---|
| `driver_wall_time_s` | 1.534 | 1.562 | 1.708 | [`driver_worker_split.rs` L99](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/driver_worker_split.rs#L99) |
| `total_worker_cpu_s` | 26.091 | 26.457 | 6.717 | [`driver_worker_split.rs` L100–L104](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/driver_worker_split.rs#L100-L104) |
| `worker_wall_time_s` | 0.160 | — | 0.232 | [`driver_worker_split.rs` L105–L108](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/driver_worker_split.rs#L105-L108) |
| `total_wall_time_s` | 1.694 | — | 1.940 | [`driver_worker_split.rs` L109](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/driver_worker_split.rs#L109) |
| `effective_parallelism` | 163.08× | — | 28.95× | [`driver_worker_split.rs` L110–L115](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/driver_worker_split.rs#L110-L115) |
| worker tasks | 86,870 | — | 86,870 | — |

---

## Pipeline stage breakdown

Artifacts: `criterion/pipeline_stages/*/new/estimates.json`

| Benchmark | This run (16-core, ns) | This run (human) | 4-core run (ns) | 4-core (human) | Artifact | Source |
|---|---|---|---|---|---|---|
| `range_8_only` | 80.4 | 80 ns | 128.8 | 129 ns | [`criterion/pipeline_stages/range_8_only/new/estimates.json`](criterion/pipeline_stages/range_8_only/new/estimates.json) | [`pipeline_stages.rs` L35–L37](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/pipeline_stages.rs#L35-L37) |
| `permutations_with_replacement_8_k3` | 10,536 | 10.5 µs | 21,075,430 | 21.1 ms | [`criterion/pipeline_stages/permutations_with_replacement_8_k3/new/estimates.json`](criterion/pipeline_stages/permutations_with_replacement_8_k3/new/estimates.json) | [`pipeline_stages.rs` L39–L49](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/pipeline_stages.rs#L39-L49) |
| `combinations_512_k3` | 2,358,749,810 | 2.359 s | 4,315,816,055 | 4.316 s | [`criterion/pipeline_stages/combinations_512_k3/new/estimates.json`](criterion/pipeline_stages/combinations_512_k3/new/estimates.json) | [`pipeline_stages.rs` L53–L67](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/pipeline_stages.rs#L53-L67) |
| `full_pipeline_range8_pwr3_comb3_kfn20` | 4,341,730,526 | 4.342 s | 12,491,103,211 | 12.491 s | [`criterion/pipeline_stages/full_pipeline_range8_pwr3_comb3_kfn20/new/estimates.json`](criterion/pipeline_stages/full_pipeline_range8_pwr3_comb3_kfn20/new/estimates.json) | [`pipeline_stages.rs` L71–L85](https://github.com/DominicBurkart/marigold/blob/a1bd575/marigold-impl/benches/pipeline_stages.rs#L71-L85) |

---

## color-palette-picker end-to-end

Artifacts: `criterion/bench_color_picker/`, `criterion/range_8_pipeline/`

| Benchmark | This run (16-core, ns) | This run (human) | 4-core run (ns) | 4-core (human) | Artifact | Source |
|---|---|---|---|---|---|---|
| `bench_color_picker` | 384,393,377 | 384.4 ms | 1,319,412,931 | 1.319 s | [`criterion/bench_color_picker/new/estimates.json`](criterion/bench_color_picker/new/estimates.json) | [`color_picker_bench.rs` L17–L36](https://github.com/DominicBurkart/marigold/blob/a1bd575/examples/color-palette-picker/benches/color_picker_bench.rs#L17-L36) |
| `range_8_pipeline/pwr3_comb3_kfn20_compare_by_sum` | 1,139,689,469 | 1.140 s | 1,587,680,551 | 1.588 s | [`criterion/range_8_pipeline/pwr3_comb3_kfn20_compare_by_sum/new/estimates.json`](criterion/range_8_pipeline/pwr3_comb3_kfn20_compare_by_sum/new/estimates.json) | [`color_picker_bench.rs` L38–L60](https://github.com/DominicBurkart/marigold/blob/a1bd575/examples/color-palette-picker/benches/color_picker_bench.rs#L38-L60) |

---

## Full-scale projection

| Metric | This run (16-core) | 4-core run |
|---|---|---|
| Benchmark items | 22,238,720 | 22,238,720 |
| Throughput | 18,261,904 items/sec | ~12,500,000 items/sec |
| Projected wall time | 293.4 days (0.8 years) | ~429 days (~1.2 years) |
