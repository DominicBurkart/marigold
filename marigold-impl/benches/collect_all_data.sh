#!/usr/bin/env bash
set -euo pipefail

# Usage: ./collect_all_data.sh [output_file]
#
# Runs all benchmarks and produces a structured markdown summary for posting
# to GitHub issue #82. Output contains real measured values to replace every
# simulated data point in the issue.
#
# Optional:
#   output_file  Path to write the markdown summary (default: bench_summary.md)
#
# Must be run from the marigold-impl directory or workspace root.
# Requirements: bc, awk, cargo, jq

REPO_ROOT="$(git rev-parse --show-toplevel)"
BENCH_DIR="${REPO_ROOT}/marigold-impl"
SCRIPT_DIR="${REPO_ROOT}/marigold-impl/benches"
OUTPUT="${1:-bench_summary.md}"

command -v jq >/dev/null 2>&1 || { echo "error: jq is required" >&2; exit 1; }

cd "$BENCH_DIR"

echo "=== Collecting benchmark data for issue #82 ==="
echo ""

criterion_ns() {
    local bench_name="$1"
    jq -r '.mean.point_estimate' "${REPO_ROOT}/target/criterion/${bench_name}/new/estimates.json" 2>/dev/null || echo "0"
}

# ─── 1. per_item_costs ────────────────────────────────────────────────────────
echo "--- Running per_item_costs ---"
cargo bench --bench per_item_costs --features tokio 2>&1

spawn_ns=$(criterion_ns "tokio_spawn_join")
iter_ns=$(criterion_ns "stream_advance_enumerate_next")
joinhandle_ns=$(criterion_ns "joinhandle_poll_completed")
arc_ns=$(criterion_ns "arc_clone_x2")
rwlock_ns=$(criterion_ns "rwlock_read_compare")
mutex_heap_ns=$(criterion_ns "mutex_heap_peek_pop_push")
useful_ns=$(criterion_ns "compare_by_sum_array3")

echo "  spawn_task:      ${spawn_ns} ns"
echo "  iter_advance:    ${iter_ns} ns"
echo "  joinhandle_poll: ${joinhandle_ns} ns"
echo "  arc_clone:       ${arc_ns} ns"
echo "  rwlock_read:     ${rwlock_ns} ns"
echo "  mutex_heap:      ${mutex_heap_ns} ns"
echo "  compare_by_sum:  ${useful_ns} ns"

# ─── 2. keep_first_n wall time ───────────────────────────────────────────────
echo ""
echo "--- Running keep_first_n ---"
cargo bench --bench keep_first_n --features tokio 2>&1
kfn_ns=$(criterion_ns "keep_first_n/c_512_3_stream_iter")
kfn_secs=$(echo "scale=9; $kfn_ns / 1000000000" | bc 2>/dev/null || echo "")
kfn_time="${kfn_ns} ns"
echo "  keep_first_n wall time: $kfn_time"

# ─── 3. cpu_utilization ──────────────────────────────────────────────────────
echo ""
echo "--- Running cpu_utilization ---"
CPU_OUT=$(cargo bench --bench cpu_utilization --features tokio 2>&1)
effective_cores=$(echo "$CPU_OUT" | grep -oP 'effective_cores: mean=\K[\d.]+' | head -1 || echo "unknown")
echo "  effective_cores: $effective_cores"

# ─── 4. driver_worker_split ──────────────────────────────────────────────────
echo ""
echo "--- Running driver_worker_split ---"
DWS_OUT=$(cargo bench --bench driver_worker_split --features "tokio,bench-instrumentation" 2>&1)
driver_wall=$(echo "$DWS_OUT"  | grep -oP 'driver_wall_time_s:\s+\K[\d.]+' | head -1 || echo "unknown")
worker_cpu=$(echo "$DWS_OUT"   | grep -oP 'total_worker_cpu_s:\s+\K[\d.]+'  | head -1 || echo "unknown")
parallel_wall=$(echo "$DWS_OUT"| grep -oP 'worker_wall_time_s:\s+\K[\d.]+'  | head -1 || echo "unknown")
echo "  driver_wall:   $driver_wall"
echo "  worker_cpu:    $worker_cpu"
echo "  parallel_wall: $parallel_wall"

# ─── 5. pipeline_stages ──────────────────────────────────────────────────────
echo ""
echo "--- Running pipeline_stages ---"
cargo bench --bench pipeline_stages --features tokio 2>&1
pwr3_ns=$(criterion_ns "pipeline_stages/permutations_with_replacement_8_k3")
comb3_ns=$(criterion_ns "pipeline_stages/combinations_512_k3")
full_ns=$(criterion_ns "pipeline_stages/full_pipeline_range8_pwr3_comb3_kfn20")
pwr3_time="${pwr3_ns} ns"
comb3_time="${comb3_ns} ns"
full_time="${full_ns} ns"
echo "  pwr(3) 8 items:       $pwr3_time"
echo "  combinations(3) 512:  $comb3_time"
echo "  full pipeline:        $full_time"

# ─── 6. CPU time-series (sample alongside keep_first_n) ──────────────────────
echo ""
echo "--- Sampling CPU utilization alongside keep_first_n ---"
CPU_CSV="/tmp/bench_cpu_samples_$$.csv"
"${SCRIPT_DIR}/sample_cpu.sh" 100 "$CPU_CSV" &
SAMPLER_PID=$!
cargo bench --bench keep_first_n --features tokio >/dev/null 2>&1
kill "$SAMPLER_PID" 2>/dev/null || true
wait "$SAMPLER_PID" 2>/dev/null || true
echo "  CPU samples written to: $CPU_CSV"

CPU_CHART=$("${SCRIPT_DIR}/render_cpu_chart.sh" "$CPU_CSV" 50)

# ─── 7. color-palette-picker ─────────────────────────────────────────────────
echo ""
echo "--- Running color-palette-picker bench ---"
COLOR_PICKER_DIR="${REPO_ROOT}/examples/color-palette-picker"
if [[ -d "$COLOR_PICKER_DIR" ]]; then
    (cd "$COLOR_PICKER_DIR" && cargo bench --bench color_picker_bench 2>&1)
    color_picker_ns=$(jq -r '.mean.point_estimate' \
        "${REPO_ROOT}/target/criterion/bench_color_picker/new/estimates.json" 2>/dev/null || echo "0")
    range8_ns=$(jq -r '.mean.point_estimate' \
        "${REPO_ROOT}/target/criterion/range_8_pipeline/pwr3_comb3_kfn20_compare_by_sum/new/estimates.json" 2>/dev/null || echo "0")
    echo "  bench_color_picker:      ${color_picker_ns} ns"
    echo "  range_8_pipeline:        ${range8_ns} ns"
else
    color_picker_ns="unknown"
    range8_ns="unknown"
    echo "  color-palette-picker example not found at $COLOR_PICKER_DIR"
fi

# ─── 8. Derived values ───────────────────────────────────────────────────────
echo ""
echo "--- Computing derived values ---"

# Spawn overhead fraction: spawn + iter + joinhandle + arc are overhead;
# rwlock + mutex_heap + compare are "useful work" per item
overhead_total=$(echo "$spawn_ns + $iter_ns + $joinhandle_ns + $arc_ns" | bc 2>/dev/null || echo "0")
useful_total=$(echo "$useful_ns + $rwlock_ns + $mutex_heap_ns" | bc 2>/dev/null || echo "0")
total_cost=$(echo "$overhead_total + $useful_total" | bc 2>/dev/null || echo "0")
if [[ "$total_cost" != "0" ]]; then
    overhead_pct=$(echo "scale=1; $overhead_total * 100 / $total_cost" | bc 2>/dev/null || echo "unknown")
else
    overhead_pct="unknown"
fi
echo "  overhead_pct: $overhead_pct%"

# Throughput and projected full-scale time
# Pipeline: 22,238,720 items processed in the benchmark
# Full scale: 463,000,000,000,000 items
BENCH_ITEMS=22238720
FULL_SCALE_ITEMS=463000000000000

if [[ -n "$kfn_secs" && "$kfn_secs" != "0" ]]; then
    throughput=$(echo "scale=0; $BENCH_ITEMS / $kfn_secs" | bc 2>/dev/null || echo "unknown")
    projected_secs=$(echo "scale=1; $FULL_SCALE_ITEMS / ($BENCH_ITEMS / $kfn_secs)" | bc 2>/dev/null || echo "unknown")
    projected_days=$(echo "scale=1; $projected_secs / 86400" | bc 2>/dev/null || echo "unknown")
    projected_years=$(echo "scale=1; $projected_secs / 31536000" | bc 2>/dev/null || echo "unknown")
    echo "  throughput:      $throughput items/sec"
    echo "  projected_days:  $projected_days days"
    echo "  projected_years: $projected_years years"
else
    throughput="unknown"
    projected_days="unknown"
    projected_years="unknown"
fi

# ─── 8. Write markdown summary ───────────────────────────────────────────────
echo ""
echo "--- Writing summary to $OUTPUT ---"

cat > "$OUTPUT" << MARKDOWN
# Benchmark Results — Issue #82 Real Data

## Per-item cost breakdown

| Component | Time (ns) | Category |
|---|---|---|
| spawn_task | ${spawn_ns} | overhead |
| iter_advance | ${iter_ns} | overhead |
| joinhandle_poll | ${joinhandle_ns} | overhead |
| arc_clone | ${arc_ns} | overhead |
| rwlock_read_compare | ${rwlock_ns} | useful work |
| mutex_heap_peek_pop_push | ${mutex_heap_ns} | useful work |
| compare_by_sum | ${useful_ns} | useful work |
| **overhead total** | **${overhead_total}** | |
| **overhead fraction** | **${overhead_pct}%** | |

## keep_first_n wall time

| Metric | Value |
|---|---|
| Wall time | ${kfn_time} |
| Effective cores | ${effective_cores} |
| Throughput | ${throughput} items/sec |

## Driver vs worker time split

| Metric | Value |
|---|---|
| driver_wall | ${driver_wall} |
| worker_cpu | ${worker_cpu} |
| parallel_wall | ${parallel_wall} |

## Pipeline stage breakdown

| Stage | Time |
|---|---|
| pwr(3) on 8 items | ${pwr3_time} |
| combinations(3) on 512 items | ${comb3_time} |
| full pipeline | ${full_time} |

## color-palette-picker end-to-end

| Benchmark | Time (ns) |
|---|---|
| bench_color_picker | ${color_picker_ns} |
| range_8_pipeline | ${range8_ns} |

## Full-scale projection

| Metric | Value |
|---|---|
| Benchmark items | ${BENCH_ITEMS} |
| Full-scale items | ${FULL_SCALE_ITEMS} |
| Projected wall time | ${projected_days} days (${projected_years} years) |

## Per-core CPU utilization over time

${CPU_CHART}
MARKDOWN

echo ""
echo "=== Done. Summary written to: $OUTPUT ==="
