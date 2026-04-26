#!/usr/bin/env bash
set -euo pipefail

# Usage: ./run_cpu_profile.sh <bench_target>

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"

BENCH_TARGET="${1:?Usage: $0 <bench_target>}"
CPU_CSV="/tmp/cpu_profile_$$.csv"

"${SCRIPT_DIR}/sample_cpu.sh" 100 "$CPU_CSV" &
SAMPLER_PID=$!

cd "${REPO_ROOT}/marigold-impl"
cargo bench --bench "$BENCH_TARGET" --features tokio

kill "$SAMPLER_PID" 2>/dev/null || true
wait "$SAMPLER_PID" 2>/dev/null || true

"${SCRIPT_DIR}/render_cpu_chart.sh" "$CPU_CSV"
