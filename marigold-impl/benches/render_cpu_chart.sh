#!/usr/bin/env bash
set -euo pipefail

# Usage: ./render_cpu_chart.sh <csv_file> [max_points]
#
# Reads a CPU samples CSV (timestamp_ms,core_id,utilization_pct) produced by
# sample_cpu.sh and outputs a Mermaid xychart-beta that can be pasted into a
# GitHub issue or PR.
#
# Example:
#   ./render_cpu_chart.sh cpu_samples.csv
#   ./render_cpu_chart.sh cpu_samples.csv 50

CSV="${1:?Usage: $0 <csv_file> [max_points]}"
MAX_POINTS="${2:-50}"

if [[ ! -f "$CSV" ]]; then
    echo "error: file not found: $CSV" >&2
    exit 1
fi

# Collect unique core IDs and timestamps (sorted)
mapfile -t ALL_CORES < <(tail -n +2 "$CSV" | cut -d, -f2 | sort -u)
mapfile -t ALL_TS    < <(tail -n +2 "$CSV" | cut -d, -f1 | sort -nu)

TOTAL_TS="${#ALL_TS[@]}"
if [[ $TOTAL_TS -eq 0 ]]; then
    echo "error: no data rows in $CSV" >&2
    exit 1
fi

# Subsample timestamps to MAX_POINTS for readability
declare -a SAMPLED_TS
if [[ $TOTAL_TS -le $MAX_POINTS ]]; then
    SAMPLED_TS=("${ALL_TS[@]}")
else
    step=$(( TOTAL_TS / MAX_POINTS ))
    for (( i=0; i<TOTAL_TS; i+=step )); do
        SAMPLED_TS+=("${ALL_TS[$i]}")
    done
fi

# Build lookup: ts,core -> utilization
declare -A UTIL
while IFS=, read -r ts core util; do
    [[ "$ts" == "timestamp_ms" ]] && continue
    UTIL["${ts},${core}"]="$util"
done < "$CSV"

# Relative timestamps from first sample
T0="${SAMPLED_TS[0]}"
declare -a X_LABELS
for ts in "${SAMPLED_TS[@]}"; do
    X_LABELS+=("$(( ts - T0 ))ms")
done

x_axis_str=$(IFS=,; echo "${X_LABELS[*]}")

echo '```mermaid'
echo 'xychart-beta'
echo '    title "Per-core CPU utilization over time"'
echo "    x-axis \"Time (ms)\" [${x_axis_str}]"
echo '    y-axis "CPU %" 0 --> 100'

for core in "${ALL_CORES[@]}"; do
    values=()
    for ts in "${SAMPLED_TS[@]}"; do
        val="${UTIL["${ts},${core}"]:-0}"
        values+=("$val")
    done
    values_str=$(IFS=,; echo "${values[*]}")
    echo "    line [${values_str}]"
done

echo '```'
echo ''
echo "<!-- cores: ${ALL_CORES[*]} | samples: ${#SAMPLED_TS[@]} of ${TOTAL_TS} -->"
