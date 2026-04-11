#!/usr/bin/env bash
set -euo pipefail

THRESHOLD="${1:-15}"
shift || true

if [[ $# -eq 0 ]]; then
    echo "SKIPPED: no benchmark output files provided (baseline cache may not exist yet)"
    exit 0
fi

regressions=()
improvements=()
no_baseline=0

for file in "$@"; do
    if [[ ! -f "$file" ]]; then
        echo "warning: file not found: $file" >&2
        continue
    fi

    if grep -q "Failed to load baseline" "$file" 2>/dev/null; then
        echo "warning: no baseline found in $file — skipping (first run or cache eviction)" >&2
        no_baseline=1
        continue
    fi

    while IFS= read -r result; do
        bench=$(echo "$result" | cut -d'|' -f1)
        pct=$(echo "$result" | cut -d'|' -f2)
        kind=$(echo "$result" | cut -d'|' -f3)
        # Use LC_NUMERIC=C so printf always uses '.' as the decimal separator
        # regardless of the runner locale. The cut fallback handles the rare
        # case where printf is unavailable.
        int_pct=$(LC_NUMERIC=C printf '%.0f' "$pct" 2>/dev/null || echo "$pct" | cut -d. -f1)
        if [[ "$kind" == "improvement" ]]; then
            improvements+=("${bench}: ${pct}% improvement [${file}]")
        elif (( int_pct > THRESHOLD )); then
            regressions+=("${bench}: +${pct}% regression (threshold: ${THRESHOLD}%) [${file}]")
        else
            echo "ok: ${bench} regressed ${pct}% (within ${THRESHOLD}% threshold)"
        fi
    done < <(awk '
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
            if (pct != "") {
                print bench "|" pct "|regression"
            }
        }
        /Performance has improved/ {
            pct = ""
            for (i = NR-1; i >= NR-5 && i >= 1; i--) {
                if (match(lines[i], /-[0-9]+\.[0-9]+%/)) {
                    pct = substr(lines[i], RSTART+1, RLENGTH-2)
                    break
                }
            }
            if (pct != "") {
                print bench "|" pct "|improvement"
            }
        }
        { lines[NR] = $0 }
    ' "$file")
done

echo ""
echo "=== Bench regression check ==="

if [[ ${#improvements[@]} -gt 0 ]]; then
    echo "Improvements (${#improvements[@]} benchmark(s)):"
    for imp in "${improvements[@]}"; do
        echo "  + $imp"
    done
fi

if [[ ${#regressions[@]} -gt 0 ]]; then
    echo "FAILED: ${#regressions[@]} benchmark(s) exceeded ${THRESHOLD}% threshold:"
    for r in "${regressions[@]}"; do
        echo "  - $r"
    done
    exit 1
elif [[ $no_baseline -eq 1 ]]; then
    echo "SKIPPED: no cached baseline — regression check skipped (cache not yet populated)"
    exit 0
else
    echo "PASSED: all benchmarks within ${THRESHOLD}% threshold"
    exit 0
fi
