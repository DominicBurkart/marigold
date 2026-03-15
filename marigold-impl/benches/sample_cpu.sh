#!/usr/bin/env bash
set -euo pipefail

# Usage: ./sample_cpu.sh [interval_ms] [output_csv]
# Samples /proc/stat per-core utilization at the given interval.
# Run this in the background while executing a benchmark, then kill it when done.
#
# Example:
#   ./sample_cpu.sh 100 cpu_samples.csv &
#   SAMPLER_PID=$!
#   cargo bench --bench keep_first_n
#   kill $SAMPLER_PID
#   # plot cpu_samples.csv with your tool of choice

INTERVAL_MS="${1:-100}"
OUTPUT="${2:-cpu_samples.csv}"
INTERVAL_SEC=$(echo "scale=3; $INTERVAL_MS / 1000" | bc)

echo "timestamp_ms,core_id,utilization_pct" > "$OUTPUT"

declare -A prev_idle prev_total

# Read initial snapshot
while IFS= read -r line; do
    if [[ "$line" =~ ^(cpu[0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+) ]]; then
        core="${BASH_REMATCH[1]}"
        user="${BASH_REMATCH[2]}"
        nice="${BASH_REMATCH[3]}"
        system="${BASH_REMATCH[4]}"
        idle="${BASH_REMATCH[5]}"
        total=$((user + nice + system + idle))
        prev_idle[$core]=$idle
        prev_total[$core]=$total
    fi
done < /proc/stat

while true; do
    sleep "$INTERVAL_SEC"
    ts_ms=$(date +%s%3N)

    while IFS= read -r line; do
        if [[ "$line" =~ ^(cpu[0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+) ]]; then
            core="${BASH_REMATCH[1]}"
            user="${BASH_REMATCH[2]}"
            nice="${BASH_REMATCH[3]}"
            system="${BASH_REMATCH[4]}"
            idle="${BASH_REMATCH[5]}"
            total=$((user + nice + system + idle))

            delta_idle=$((idle - ${prev_idle[$core]:-0}))
            delta_total=$((total - ${prev_total[$core]:-0}))

            if [[ $delta_total -gt 0 ]]; then
                utilization=$(echo "scale=1; (1 - $delta_idle / $delta_total) * 100" | bc)
            else
                utilization=0
            fi

            echo "$ts_ms,$core,$utilization" >> "$OUTPUT"

            prev_idle[$core]=$idle
            prev_total[$core]=$total
        fi
    done < /proc/stat
done
