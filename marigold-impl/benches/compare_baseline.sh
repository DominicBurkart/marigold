#!/usr/bin/env bash
set -euo pipefail

# Usage: ./compare_baseline.sh [pre_commit] [post_commit]
#
# Arguments (optional):
#   pre_commit   Commit SHA before optimization (default: 3d020a1)
#   post_commit  Commit SHA after optimization  (default: 1ec3465)
#
# Captures Criterion baseline at the pre-optimization commit (3d020a1, first
# commit of PR #83 — has bench infra but not the ready_chunks optimization),
# then runs the same benchmarks at the post-optimization commit (1ec3465) so
# Criterion can print a before/after comparison.
#
# Must be run from the marigold-impl directory (or the workspace root).
# Requires a clean working tree (no uncommitted changes).

REPO_ROOT="$(git rev-parse --show-toplevel)"
BENCH_DIR="${REPO_ROOT}/marigold-impl"

PRE_COMMIT="${1:-3d020a1}"   # PR #83: bench infra, no ready_chunks optimization
POST_COMMIT="${2:-1ec3465}"  # PR #83: ready_chunks optimization applied

BASELINE_NAME="pre_optimization"

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

echo "=== Step 1: baseline at $PRE_COMMIT ==="
git checkout "$PRE_COMMIT" --quiet
cargo bench --bench keep_first_n --features tokio -- \
    --save-baseline "$BASELINE_NAME" 2>&1 | tail -20

echo ""
echo "=== Step 2: per_item_costs + driver_worker_split at $PRE_COMMIT ==="
cargo bench --bench per_item_costs --features tokio 2>&1 | tail -30
cargo bench --bench driver_worker_split --features "tokio,bench-instrumentation" 2>&1 | tail -10

echo ""
echo "=== Step 3: cpu_utilization at $PRE_COMMIT ==="
cargo bench --bench cpu_utilization --features tokio 2>&1 | tail -5

echo ""
echo "=== Step 4: post-optimization at $POST_COMMIT ==="
git checkout "$POST_COMMIT" --quiet
cargo bench --bench keep_first_n --features tokio -- \
    --baseline "$BASELINE_NAME" 2>&1 | tail -20

echo ""
echo "=== Step 5: per_item_costs + driver_worker_split at $POST_COMMIT ==="
cargo bench --bench per_item_costs --features tokio 2>&1 | tail -30
cargo bench --bench driver_worker_split --features "tokio,bench-instrumentation" 2>&1 | tail -10

echo ""
echo "=== Step 6: cpu_utilization at $POST_COMMIT ==="
cargo bench --bench cpu_utilization --features tokio 2>&1 | tail -5

echo ""
echo "Done. Criterion HTML report: target/criterion/keep_first_n/report/index.html"
