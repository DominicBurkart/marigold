#!/usr/bin/env bash
set -euo pipefail

# Usage: ./sample_cpu.sh [interval_ms] [output_csv] [pid]
#
# Modes:
#   System-wide (no pid):  samples /proc/stat per-core utilization
#   Process-specific (pid): samples /proc/<pid>/stat CPU time and reports
#                           process CPU % relative to system total
#
# Run this in the background while executing a benchmark, then kill it when done.
#
# Examples:
#   # System-wide:
#   ./sample_cpu.sh 100 cpu_samples.csv &
#   SAMPLER_PID=$!
#   cargo bench --bench keep_first_n
#   kill $SAMPLER_PID
#
#   # Process-specific:
#   cargo bench --bench keep_first_n &
#   BENCH_PID=$!
#   ./sample_cpu.sh 100 cpu_samples.csv $BENCH_PID &
#   SAMPLER_PID=$!
#   wait $BENCH_PID
#   kill $SAMPLER_PID
#
#   # Sanity-check with perf:
#   perf stat -e task-clock cargo bench --bench keep_first_n

# This script reads /proc/stat and /proc/<pid>/stat — Linux only.
if [[ ! -f /proc/stat ]]; then
    echo "error: /proc/stat not found. This script requires Linux." >&2
    exit 1
fi

INTERVAL_MS="${1:-100}"
OUTPUT="${2:-cpu_samples.csv}"
TARGET_PID="${3:-}"
INTERVAL_SEC=$(echo "scale=3; $INTERVAL_MS / 1000" | bc)

if [[ -n "$TARGET_PID" ]]; then
    echo "timestamp_ms,core_id,utilization_pct" > "$OUTPUT"

    declare -A prev_sys_idle prev_sys_total
    prev_proc_ticks=0

    # Read initial system snapshot
    while IFS= read -r line; do
        if [[ "$line" =~ ^(cpu[0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+) ]]; then
            core="${BASH_REMATCH[1]}"
            user="${BASH_REMATCH[2]}"
            nice="${BASH_REMATCH[3]}"
            system="${BASH_REMATCH[4]}"
            idle="${BASH_REMATCH[5]}"
            prev_sys_idle[$core]=$idle
            prev_sys_total[$core]=$(( user + nice + system + idle ))
        fi
    done < /proc/stat

    while kill -0 "$TARGET_PID" 2>/dev/null; do
        sleep "$INTERVAL_SEC"
        ts_ms=$(date +%s%3N)

        # Process ticks: fields 14 (utime) + 15 (stime) from /proc/<pid>/stat
        if [[ ! -f "/proc/$TARGET_PID/stat" ]]; then break; fi
        read -ra proc_fields < "/proc/$TARGET_PID/stat"
        proc_ticks=$(( proc_fields[13] + proc_fields[14] ))
        delta_proc=$(( proc_ticks - prev_proc_ticks ))
        prev_proc_ticks=$proc_ticks

        # Total system ticks across all cores (delta)
        total_sys_delta=0
        while IFS= read -r line; do
            if [[ "$line" =~ ^(cpu[0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+)[[:space:]]+([0-9]+) ]]; then
                core="${BASH_REMATCH[1]}"
                user="${BASH_REMATCH[2]}"
                nice="${BASH_REMATCH[3]}"
                system="${BASH_REMATCH[4]}"
                idle="${BASH_REMATCH[5]}"
                total=$(( user + nice + system + idle ))
                delta=$(( total - ${prev_sys_total[$core]:-0} ))
                total_sys_delta=$(( total_sys_delta + delta ))
                prev_sys_idle[$core]=$idle
                prev_sys_total[$core]=$total
            fi
        done < /proc/stat

        if [[ $total_sys_delta -gt 0 ]]; then
            utilization=$(echo "scale=1; $delta_proc * 100 / $total_sys_delta" | bc)
        else
            utilization=0
        fi
        echo "$ts_ms,process,$utilization" >> "$OUTPUT"
    done
else
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
fi
