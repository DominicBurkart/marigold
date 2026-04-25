#!/usr/bin/env bash
set -euo pipefail

# Usage: check_regression.sh <threshold> [--require-runs N] [file...]
#
# <threshold>     Default percentage threshold when thresholds.json is absent (default: 15).
# --require-runs N  Number of files that must agree on a regression before it is reported
#                   as a failure (default: 1). Pass 2 to require consensus across two bench
#                   runs, preventing single-run CI noise from blocking a PR.
#
# thresholds.json (same directory as this script) is read when present.
# It can define per-tier thresholds; see the accompanying thresholds.json.

THRESHOLD="${1:-15}"
shift || true

REQUIRED_RUNS=1

declare -a FILES
while [[ $# -gt 0 ]]; do
    case "$1" in
        --require-runs)
            REQUIRED_RUNS="$2"
            shift 2
            ;;
        *)
            FILES+=("$1")
            shift
            ;;
    esac
done

if [[ ${#FILES[@]} -eq 0 ]]; then
    echo "SKIPPED: no benchmark output files provided (baseline cache may not exist yet)"
    exit 0
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
THRESHOLDS_FILE="${SCRIPT_DIR}/thresholds.json"

# Load per-tier thresholds from thresholds.json when present and jq is available.
tier_async_threshold="$THRESHOLD"
declare -a tier_async_prefixes=()
default_threshold="$THRESHOLD"

if [[ -f "$THRESHOLDS_FILE" ]] && command -v jq >/dev/null 2>&1; then
    _dt=$(jq -r '.default_threshold_pct // empty' "$THRESHOLDS_FILE" 2>/dev/null || true)
    [[ -n "$_dt" && "$_dt" != "null" ]] && default_threshold="$_dt"

    _at=$(jq -r '."tier-async".threshold_pct // empty' "$THRESHOLDS_FILE" 2>/dev/null || true)
    [[ -n "$_at" && "$_at" != "null" ]] && tier_async_threshold="$_at"

    mapfile -t tier_async_prefixes < <(
        jq -r '."tier-async".id_prefixes[] // empty' "$THRESHOLDS_FILE" 2>/dev/null || true
    )
fi

threshold_for() {
    local name="$1"
    local prefix
    for prefix in "${tier_async_prefixes[@]}"; do
        if [[ "$name" == "$prefix"* ]]; then
            echo "$tier_async_threshold"
            return
        fi
    done
    echo "$default_threshold"
}

# regression_counts[bench] = how many files showed this bench above its threshold
declare -A regression_counts
# regression_pct_max[bench] = highest regression percentage seen across all files
declare -A regression_pct_max

improvements=()
no_baseline=0

# Shared awk extractor — emits "bench|pct|kind" lines from a Criterion text log.
EXTRACTOR='
    /^Benchmarking / {
        bench = substr($0, 14)
        sub(/[[:space:]]*$/, "", bench)
    }
    /Performance has regressed/ {
        pct = ""
        for (i = NR-1; i >= NR-5 && i >= 1; i--) {
            if (match(lines[i], /\+[0-9]+\.[0-9]+%/)) {
                pct = substr(lines[i], RSTART+1, RLENGTH-2)
                break
            }
        }
        if (pct != "") { print bench "|" pct "|regression" }
    }
    /Performance has improved/ {
        pct = ""
        for (i = NR-1; i >= NR-5 && i >= 1; i--) {
            if (match(lines[i], /-[0-9]+\.[0-9]+%/)) {
                pct = substr(lines[i], RSTART+1, RLENGTH-2)
                break
            }
        }
        if (pct != "") { print bench "|" pct "|improvement" }
    }
    { lines[NR] = $0 }
'

for file in "${FILES[@]}"; do
    if [[ ! -f "$file" ]]; then
        echo "warning: file not found: $file" >&2
        continue
    fi

    if grep -q "Failed to load baseline" "$file" 2>/dev/null; then
        echo "warning: no baseline in $file — skipping (first run or cache eviction)" >&2
        no_baseline=1
        continue
    fi

    while IFS= read -r result; do
        bench=$(echo "$result" | cut -d'|' -f1)
        pct=$(echo "$result"   | cut -d'|' -f2)
        kind=$(echo "$result"  | cut -d'|' -f3)

        if [[ "$kind" == "improvement" ]]; then
            improvements+=("${bench}: ${pct}% [${file}]")
            continue
        fi

        bench_threshold=$(threshold_for "$bench")
        int_pct=$(LC_NUMERIC=C printf '%.0f' "$pct" 2>/dev/null || echo "$pct" | cut -d. -f1)

        if (( int_pct > bench_threshold )); then
            regression_counts["$bench"]=$(( ${regression_counts["$bench"]:-0} + 1 ))
            cur="${regression_pct_max["$bench"]:-0}"
            if (( $(echo "$pct > $cur" | bc -l 2>/dev/null || echo "0") )); then
                regression_pct_max["$bench"]="$pct"
            fi
        else
            echo "ok: ${bench} +${pct}% (within $(threshold_for "$bench")% threshold) [${file}]"
        fi
    done < <(awk "$EXTRACTOR" "$file")
done

echo ""
echo "=== Bench regression check (require-runs: ${REQUIRED_RUNS}) ==="

if [[ ${#improvements[@]} -gt 0 ]]; then
    echo "Improvements (${#improvements[@]} benchmark(s)):"
    for imp in "${improvements[@]}"; do
        echo "  + $imp"
    done
fi

FAIL=0
noise_msgs=()
fail_msgs=()

for bench in "${!regression_counts[@]}"; do
    count="${regression_counts[$bench]}"
    pct="${regression_pct_max[$bench]:-?}"
    bench_threshold=$(threshold_for "$bench")
    if (( count >= REQUIRED_RUNS )); then
        fail_msgs+=("${bench}: +${pct}% regression in ${count}/${#FILES[@]} runs (threshold: ${bench_threshold}%)")
        FAIL=1
    else
        noise_msgs+=("${bench}: +${pct}% in only ${count}/${#FILES[@]} runs (noise — not counted)")
    fi
done

if [[ ${#noise_msgs[@]} -gt 0 ]]; then
    echo "Noise (below consensus threshold — not a failure):"
    for m in "${noise_msgs[@]}"; do
        echo "  ~ $m"
    done
fi

if [[ ${#fail_msgs[@]} -gt 0 ]]; then
    echo "FAILED: ${#fail_msgs[@]} benchmark(s) regressed in ${REQUIRED_RUNS}+ of ${#FILES[@]} runs:"
    for m in "${fail_msgs[@]}"; do
        echo "  - $m"
    done
    exit 1
elif [[ $no_baseline -eq 1 ]]; then
    echo "SKIPPED: no cached baseline — regression check skipped (cache not yet populated)"
    exit 0
else
    echo "PASSED: all benchmarks within threshold"
    exit 0
fi
