#!/usr/bin/env bash
set -euo pipefail

# Usage: ./compare_baseline.sh [pre_commit] [post_commit] [output_file]
#
# Arguments (optional):
#   pre_commit   Commit SHA before optimization (default: 3d020a1)
#   post_commit  Commit SHA after optimization  (default: 1ec3465)
#   output_file  Path for the markdown comparison (default: compare_results.md)
#
# Captures metrics at both commits and writes a structured before/after
# compare_results.md with percentage changes for posting to issue #82.
#
# Must be run from the marigold-impl directory (or the workspace root).
# Requires a clean working tree (no uncommitted changes).
# Requirements: bc, jq

REPO_ROOT="$(git rev-parse --show-toplevel)"
BENCH_DIR="${REPO_ROOT}/marigold-impl"
SCRIPT_DIR="${REPO_ROOT}/marigold-impl/benches"

PRE_COMMIT="${1:-HEAD~1}"
POST_COMMIT="${2:-HEAD}"
OUTPUT="${3:-compare_results.md}"

command -v jq >/dev/null 2>&1 || { echo "error: jq is required" >&2; exit 1; }

cd "$BENCH_DIR"

if [[ -n "$(git status --porcelain)" ]]; then
    echo "error: working tree is not clean. Stash or commit changes first." >&2
    exit 1
fi

CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"

cleanup() {
    echo "restoring branch: $CURRENT_BRANCH"
    git checkout "$CURRENT_BRANCH" --quiet
}
trap cleanup EXIT

criterion_ns() {
    local bench_name="$1"
    jq -r '.mean.point_estimate' "${REPO_ROOT}/target/criterion/${bench_name}/new/estimates.json" 2>/dev/null || echo "0"
}

collect_metrics() {
    local label="$1"
    local cpu_csv="$2"

    echo "  running per_item_costs..." >&2
    cargo bench --bench per_item_costs --features tokio >/dev/null 2>&1 || true

    local spawn_ns iter_ns joinhandle_ns arc_ns rwlock_ns mutex_heap_ns useful_ns
    spawn_ns=$(criterion_ns "tokio_spawn_join")
    iter_ns=$(criterion_ns "stream_advance_enumerate_next")
    joinhandle_ns=$(criterion_ns "joinhandle_poll_completed")
    arc_ns=$(criterion_ns "arc_clone_x2")
    rwlock_ns=$(criterion_ns "rwlock_read_compare")
    mutex_heap_ns=$(criterion_ns "mutex_heap_peek_pop_push")
    useful_ns=$(criterion_ns "compare_by_sum_array3")

    echo "  running keep_first_n (with CPU sampling)..." >&2
    "${SCRIPT_DIR}/sample_cpu.sh" 100 "$cpu_csv" &
    local sampler_pid=$!
    cargo bench --bench keep_first_n --features tokio >/dev/null 2>&1 || true
    kill "$sampler_pid" 2>/dev/null || true
    wait "$sampler_pid" 2>/dev/null || true

    local kfn_ns
    kfn_ns=$(criterion_ns "keep_first_n/c_512_3_stream_iter")

    echo "  running cpu_utilization..." >&2
    local cpu_out
    cpu_out=$(cargo bench --bench cpu_utilization --features tokio 2>&1 || true)
    local effective_cores
    effective_cores=$(echo "$cpu_out" | sed -n 's/.*effective_cores: mean=\([0-9.]*\).*/\1/p' | head -1 || echo "unknown")

    echo "  running driver_worker_split..." >&2
    local dws_out
    dws_out=$(cargo bench --bench driver_worker_split --features "tokio,bench-instrumentation" 2>&1 || true)
    local driver_wall worker_cpu parallel_wall
    driver_wall=$(echo "$dws_out"  | sed -n 's/.*driver_wall_time_s:[[:space:]]*\([0-9.]*\).*/\1/p' | head -1 || echo "unknown")
    worker_cpu=$(echo "$dws_out"   | sed -n 's/.*total_worker_cpu_s:[[:space:]]*\([0-9.]*\).*/\1/p'  | head -1 || echo "unknown")
    parallel_wall=$(echo "$dws_out"| sed -n 's/.*worker_wall_time_s:[[:space:]]*\([0-9.]*\).*/\1/p'  | head -1 || echo "unknown")

    printf '%s\n' \
        "$spawn_ns" "$iter_ns" "$joinhandle_ns" "$arc_ns" \
        "$rwlock_ns" "$mutex_heap_ns" "$useful_ns" \
        "$kfn_ns" "$effective_cores" \
        "$driver_wall" "$worker_cpu" "$parallel_wall"
}

pct_change() {
    local before="$1" after="$2"
    if [[ "$before" == "unknown" || "$after" == "unknown" || "$before" == "0" ]]; then
        echo "n/a"
        return
    fi
    echo "scale=1; ($after - $before) * 100 / $before" | bc 2>/dev/null || echo "n/a"
}

fmt_pct() {
    local p="$1"
    if [[ "$p" == "n/a" ]]; then echo "n/a"; return; fi
    if echo "$p" | grep -q '^-'; then
        echo "${p}% (improvement)"
    else
        echo "+${p}% (regression)"
    fi
}

# ─── Step 1: pre-commit ───────────────────────────────────────────────────────
echo "=== Step 1: metrics at $PRE_COMMIT ==="
git checkout "$PRE_COMMIT" --quiet
CPU_CSV_PRE="/tmp/bench_cpu_pre_$$.csv"
mapfile -t PRE < <(collect_metrics "pre" "$CPU_CSV_PRE")

pre_spawn="${PRE[0]}"   pre_iter="${PRE[1]}"   pre_joinhandle="${PRE[2]}"
pre_arc="${PRE[3]}"     pre_rwlock="${PRE[4]}"  pre_mutex="${PRE[5]}"
pre_useful="${PRE[6]}"  pre_kfn="${PRE[7]}"
pre_cores="${PRE[8]}"   pre_dwall="${PRE[9]}"
pre_wcpu="${PRE[10]}"   pre_pwall="${PRE[11]}"

# ─── Step 2: post-commit ──────────────────────────────────────────────────────
echo ""
echo "=== Step 2: metrics at $POST_COMMIT ==="
git checkout "$POST_COMMIT" --quiet
CPU_CSV_POST="/tmp/bench_cpu_post_$$.csv"
mapfile -t POST < <(collect_metrics "post" "$CPU_CSV_POST")

post_spawn="${POST[0]}"   post_iter="${POST[1]}"   post_joinhandle="${POST[2]}"
post_arc="${POST[3]}"     post_rwlock="${POST[4]}"  post_mutex="${POST[5]}"
post_useful="${POST[6]}"  post_kfn="${POST[7]}"
post_cores="${POST[8]}"   post_dwall="${POST[9]}"
post_wcpu="${POST[10]}"   post_pwall="${POST[11]}"

# ─── Step 3: render CPU charts ────────────────────────────────────────────────
echo ""
echo "=== Step 3: rendering CPU charts ==="
CPU_CHART_PRE=$("${SCRIPT_DIR}/render_cpu_chart.sh" "$CPU_CSV_PRE" 50 2>/dev/null || echo "<!-- no CPU data -->")
CPU_CHART_POST=$("${SCRIPT_DIR}/render_cpu_chart.sh" "$CPU_CSV_POST" 50 2>/dev/null || echo "<!-- no CPU data -->")

# ─── Step 4: write compare_results.md ─────────────────────────────────────────
echo ""
echo "=== Step 4: writing $OUTPUT ==="

cat > "$OUTPUT" << MARKDOWN
# Before/After Benchmark Comparison — Issue #82

Commits compared: \`${PRE_COMMIT}\` (before) → \`${POST_COMMIT}\` (after)

## Per-item cost breakdown

| Component | Before (ns) | After (ns) | Change |
|---|---|---|---|
| spawn_task | ${pre_spawn} | ${post_spawn} | $(fmt_pct "$(pct_change "$pre_spawn" "$post_spawn")") |
| iter_advance | ${pre_iter} | ${post_iter} | $(fmt_pct "$(pct_change "$pre_iter" "$post_iter")") |
| joinhandle_poll | ${pre_joinhandle} | ${post_joinhandle} | $(fmt_pct "$(pct_change "$pre_joinhandle" "$post_joinhandle")") |
| arc_clone | ${pre_arc} | ${post_arc} | $(fmt_pct "$(pct_change "$pre_arc" "$post_arc")") |
| rwlock_read_compare | ${pre_rwlock} | ${post_rwlock} | $(fmt_pct "$(pct_change "$pre_rwlock" "$post_rwlock")") |
| mutex_heap_peek_pop_push | ${pre_mutex} | ${post_mutex} | $(fmt_pct "$(pct_change "$pre_mutex" "$post_mutex")") |
| compare_by_sum | ${pre_useful} | ${post_useful} | $(fmt_pct "$(pct_change "$pre_useful" "$post_useful")") |

## keep_first_n wall time

| Metric | Before | After | Change |
|---|---|---|---|
| Wall time (ns) | ${pre_kfn} | ${post_kfn} | $(fmt_pct "$(pct_change "$pre_kfn" "$post_kfn")") |
| Effective cores | ${pre_cores} | ${post_cores} | $(fmt_pct "$(pct_change "$pre_cores" "$post_cores")") |

## Driver vs worker time split

| Metric | Before (s) | After (s) | Change |
|---|---|---|---|
| driver_wall | ${pre_dwall} | ${post_dwall} | $(fmt_pct "$(pct_change "$pre_dwall" "$post_dwall")") |
| worker_cpu | ${pre_wcpu} | ${post_wcpu} | $(fmt_pct "$(pct_change "$pre_wcpu" "$post_wcpu")") |
| parallel_wall | ${pre_pwall} | ${post_pwall} | $(fmt_pct "$(pct_change "$pre_pwall" "$post_pwall")") |

## Per-core CPU utilization — before (${PRE_COMMIT})

${CPU_CHART_PRE}

## Per-core CPU utilization — after (${POST_COMMIT})

${CPU_CHART_POST}
MARKDOWN

echo ""
echo "=== Done. Comparison written to: $OUTPUT ==="
