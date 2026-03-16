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
# Requirements: bc, awk, cargo

REPO_ROOT="$(git rev-parse --show-toplevel)"
BENCH_DIR="${REPO_ROOT}/marigold-impl"
SCRIPT_DIR="${REPO_ROOT}/marigold-impl/benches"
OUTPUT="${1:-bench_summary.md}"

cd "$BENCH_DIR"

echo "=== Collecting benchmark data for issue #82 ==="
echo ""

# ─── Helper: extract median ns from Criterion output ─────────────────────────
# Criterion prints lines like:  "spawn_task            time:   [345.12 ns 350.34 ns 356.78 ns]"
# We capture the middle value (median).
extract_median_ns() {
    local bench_output="$1"
    echo "$bench_output" | grep -oP 'time:\s+\[\S+\s+\K[\d.]+(?=\s+\w+\s+\d)' | head -1
}

extract_median_with_unit() {
    local bench_output="$1"
    echo "$bench_output" | grep -oP 'time:\s+\[\S+\s+\K[\d.]+ \w+(?=\s+\d)' | head -1
}

# ─── 1. per_item_costs ────────────────────────────────────────────────────────
echo "--- Running per_item_costs ---"
PER_ITEM_OUT=$(cargo bench --bench per_item_costs --features tokio 2>&1)

spawn_ns=$(echo "$PER_ITEM_OUT"      | grep -A2 'spawn_task'           | extract_median_ns "$(echo "$PER_ITEM_OUT" | grep -A2 'spawn_task')" || echo "0")
iter_ns=$(echo "$PER_ITEM_OUT"       | grep -A2 'iter_advance'         | extract_median_ns "$(echo "$PER_ITEM_OUT" | grep -A2 'iter_advance')" || echo "0")
joinhandle_ns=$(echo "$PER_ITEM_OUT" | grep -A2 'joinhandle_poll'      | extract_median_ns "$(echo "$PER_ITEM_OUT" | grep -A2 'joinhandle_poll')" || echo "0")
arc_ns=$(echo "$PER_ITEM_OUT"        | grep -A2 'arc_clone'            | extract_median_ns "$(echo "$PER_ITEM_OUT" | grep -A2 'arc_clone')" || echo "0")
useful_ns=$(echo "$PER_ITEM_OUT"     | grep -A2 'compare_by_sum'       | extract_median_ns "$(echo "$PER_ITEM_OUT" | grep -A2 'compare_by_sum')" || echo "0")

echo "  spawn_task:      ${spawn_ns} ns"
echo "  iter_advance:    ${iter_ns} ns"
echo "  joinhandle_poll: ${joinhandle_ns} ns"
echo "  arc_clone:       ${arc_ns} ns"
echo "  compare_by_sum:  ${useful_ns} ns"

# ─── 2. keep_first_n wall time ───────────────────────────────────────────────
echo ""
echo "--- Running keep_first_n ---"
KFN_OUT=$(cargo bench --bench keep_first_n --features tokio 2>&1)
kfn_time=$(echo "$KFN_OUT" | grep -oP 'time:\s+\[\S+\s+\K[\d.]+ \w+(?=\s+\d)' | head -1 || echo "unknown")
echo "  keep_first_n wall time: $kfn_time"

# ─── 3. cpu_utilization ──────────────────────────────────────────────────────
echo ""
echo "--- Running cpu_utilization ---"
CPU_OUT=$(cargo bench --bench cpu_utilization --features tokio 2>&1)
effective_cores=$(echo "$CPU_OUT" | grep -oP 'effective_cores\s+\|\s+\K[\d.]+' | head -1 || echo "unknown")
echo "  effective_cores: $effective_cores"

# ─── 4. driver_worker_split ──────────────────────────────────────────────────
echo ""
echo "--- Running driver_worker_split ---"
DWS_OUT=$(cargo bench --bench driver_worker_split --features "tokio,bench-instrumentation" 2>&1)
driver_wall=$(echo "$DWS_OUT"  | grep -oP 'driver_wall\s+\|\s+\K[\d.]+\s*s' | head -1 || echo "unknown")
worker_cpu=$(echo "$DWS_OUT"   | grep -oP 'worker_cpu\s+\|\s+\K[\d.]+\s*s'  | head -1 || echo "unknown")
parallel_wall=$(echo "$DWS_OUT"| grep -oP 'parallel_wall\s+\|\s+\K[\d.]+\s*s'| head -1 || echo "unknown")
echo "  driver_wall:   $driver_wall"
echo "  worker_cpu:    $worker_cpu"
echo "  parallel_wall: $parallel_wall"

# ─── 5. pipeline_stages ──────────────────────────────────────────────────────
echo ""
echo "--- Running pipeline_stages ---"
PIPE_OUT=$(cargo bench --bench pipeline_stages --features tokio 2>&1)
pwr3_time=$(echo "$PIPE_OUT"  | grep -A2 'pwr_3_items_8'  | grep -oP 'time:\s+\[\S+\s+\K[\d.]+ \w+(?=\s+\d)' | head -1 || echo "unknown")
comb3_time=$(echo "$PIPE_OUT" | grep -A2 'comb_3_items_512'| grep -oP 'time:\s+\[\S+\s+\K[\d.]+ \w+(?=\s+\d)'| head -1 || echo "unknown")
full_time=$(echo "$PIPE_OUT"  | grep -A2 'full_pipeline'  | grep -oP 'time:\s+\[\S+\s+\K[\d.]+ \w+(?=\s+\d)'| head -1 || echo "unknown")
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

# ─── 7. Derived values ───────────────────────────────────────────────────────
echo ""
echo "--- Computing derived values ---"

# Spawn overhead fraction (overhead components / total per-item cost)
overhead_total=$(echo "$spawn_ns + $iter_ns + $joinhandle_ns + $arc_ns" | bc 2>/dev/null || echo "0")
total_cost=$(echo "$overhead_total + $useful_ns" | bc 2>/dev/null || echo "1")
if [[ "$total_cost" != "0" && "$total_cost" != "1" ]]; then
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

kfn_secs=$(echo "$KFN_OUT" | grep -oP 'time:\s+\[\S+\s+\K[\d.]+(?=\s+s\s+\d)' | head -1 || echo "")
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

| Component | Time (ns) |
|---|---|
| spawn_task | ${spawn_ns} |
| iter_advance | ${iter_ns} |
| joinhandle_poll | ${joinhandle_ns} |
| arc_clone | ${arc_ns} |
| compare_by_sum (useful work) | ${useful_ns} |
| **overhead total** | **${overhead_total}** |
| **overhead fraction** | **${overhead_pct}%** |

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
