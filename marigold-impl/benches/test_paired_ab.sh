#!/usr/bin/env bash
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/paired_ab.sh"
checker="$here/check_paired_regression.py"
fails=0
passes=0

workdirs=(marigold-impl marigold-grammar examples/color-palette-picker examples/algorithms)

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

stub_dir="$work/stubbin"
mkdir -p "$stub_dir"
cat > "$stub_dir/cargo" <<'STUB'
#!/usr/bin/env bash
set -eu
echo "cwd=$PWD args=$*" >> "$STUB_LOG"
target=""
name=""
prev=""
for a in "$@"; do
    [[ "$prev" == "--bench" ]] && target="$a"
    [[ "$prev" == "--save-baseline" ]] && name="$a"
    prev="$a"
done
if [[ "$CARGO_TARGET_DIR" == "$RUNNER_TEMP/base-target" ]]; then
    side=base
    if [[ ",${STUB_BASE_FAIL:-}," == *",$target,"* ]]; then
        echo "error: no bench target named $target" >&2
        exit 101
    fi
else
    side=head
fi
if [[ "${STUB_ALL_FAIL:-}" == "$side" ]]; then
    exit 101
fi
round="${name##*x}"
base_val="${STUB_BASE_VAL:-1000000}"
value="$base_val"
if [[ "$side" == head ]]; then
    ratios=(${STUB_HEAD_RATIOS:-1.0 1.0 1.0})
    ratio="${ratios[$((round - 1))]}"
    value="$(awk -v b="$base_val" -v r="$ratio" 'BEGIN { printf "%.3f", b * r }')"
fi
d="$CARGO_TARGET_DIR/criterion/$target/case/$name"
mkdir -p "$d"
printf '{"mean":{"point_estimate":%s},"median":{"confidence_interval":{"confidence_level":0.95,"lower_bound":1.0,"upper_bound":2.0},"point_estimate":%s,"standard_error":1.0},"median_abs_dev":{"point_estimate":1.0},"slope":{"point_estimate":%s},"std_dev":{"point_estimate":1.0}}\n' "$value" "$value" "$value" > "$d/estimates.json"
STUB
chmod +x "$stub_dir/cargo"

regress_out_for() {
    local out="$1"
    shift
    {
        echo "=== Bench regression check ==="
        if [[ $# -gt 0 ]]; then
            echo "FAILED: $# benchmark(s) exceeded 15% threshold:"
            for t in "$@"; do
                echo "  - $t/case: +21.50% regression (threshold: 15%) [/tmp/bench_$t.txt]"
            done
        else
            echo "PASSED: all benchmarks within 15% threshold"
        fi
    } > "$out"
}

setup_case() {
    case_dir="$work/$1"
    mkdir -p "$case_dir/repo" "$case_dir/runner" "$case_dir/head-target"
    (
        cd "$case_dir/repo"
        git init -q .
        git config user.email t@example.invalid
        git config user.name t
        git config commit.gpgsign false
        for d in "${workdirs[@]}"; do
            mkdir -p "$d"
            touch "$d/.keep"
        done
        git add -A
        git commit -q -m base
    )
    base_sha="$(git -C "$case_dir/repo" rev-parse HEAD)"
    : > "$case_dir/stub.log"
    : > "$case_dir/summary.md"
}

run_case() {
    local regress="$1"
    (
        cd "$case_dir/repo"
        env PATH="$stub_dir:$PATH" \
            STUB_LOG="$case_dir/stub.log" \
            RUNNER_TEMP="$case_dir/runner" \
            CARGO_TARGET_DIR="$case_dir/head-target" \
            BASE_SHA="$base_sha" \
            GITHUB_RUN_ID=777 \
            GITHUB_STEP_SUMMARY="$case_dir/summary.md" \
            bash "$script" "$regress" > "$case_dir/out.txt" 2>&1
    )
    rc=$?
}

check() {
    local desc="$1" ok="$2"
    if [[ "$ok" == 1 ]]; then
        passes=$((passes + 1))
        echo "ok: $desc"
    else
        fails=$((fails + 1))
        echo "FAIL: $desc"
        [[ -f "$case_dir/out.txt" ]] && sed 's/^/    | /' "$case_dir/out.txt"
    fi
}

expect_rc() {
    local desc="$1" want="$2"
    if [[ "$want" == nonzero ]]; then
        [[ "$rc" -ne 0 ]] && check "$desc" 1 || check "$desc (rc=$rc)" 0
    else
        [[ "$rc" -eq "$want" ]] && check "$desc" 1 || check "$desc (rc=$rc, want $want)" 0
    fi
}

export STUB_BASE_VAL=1000000

setup_case none
regress_out_for "$case_dir/regress.out"
run_case "$case_dir/regress.out"
expect_rc "no flagged benchmarks exits 0" 0
[[ ! -s "$case_dir/stub.log" ]] && check "no flagged benchmarks never invokes cargo" 1 || check "no flagged benchmarks never invokes cargo" 0

setup_case equal
regress_out_for "$case_dir/regress.out" keep_first_n
STUB_HEAD_RATIOS="1.0 1.0 1.0" run_case "$case_dir/regress.out"
expect_rc "equal base and head passes" 0

setup_case stable_offset
regress_out_for "$case_dir/regress.out" keep_first_n pipeline_stages
STUB_HEAD_RATIOS="1.0 1.0 1.0" run_case "$case_dir/regress.out"
expect_rc "stable offset against cache but equal base/head passes" 0

setup_case regress_all
regress_out_for "$case_dir/regress.out" keep_first_n
STUB_HEAD_RATIOS="1.4 1.4 1.4" run_case "$case_dir/regress.out"
expect_rc "head 1.4x base in every round fails" 1
grep -q "keep_first_n/case" "$case_dir/summary.md" && check "summary lists the paired table" 1 || check "summary lists the paired table" 0
grep -q "keep_first_n/case" "$case_dir/summary.md" && grep -qi "flagged" "$case_dir/summary.md" && check "summary lists first-pass flagged benchmarks" 1 || check "summary lists first-pass flagged benchmarks" 0

setup_case regress_one_round
regress_out_for "$case_dir/regress.out" keep_first_n
STUB_HEAD_RATIOS="1.4 1.0 1.0" run_case "$case_dir/regress.out"
expect_rc "head 1.4x in only one of three rounds passes" 0

setup_case regress_two_rounds
regress_out_for "$case_dir/regress.out" keep_first_n
STUB_HEAD_RATIOS="1.4 1.4 1.0" run_case "$case_dir/regress.out"
expect_rc "head 1.4x in two of three rounds fails" 1

setup_case base_lacks_target
regress_out_for "$case_dir/regress.out" keep_first_n
STUB_BASE_FAIL=keep_first_n run_case "$case_dir/regress.out"
expect_rc "base lacking the bench target fails closed" nonzero
grep -qi "base" "$case_dir/summary.md" && check "summary explains the failed comparison" 1 || check "summary explains the failed comparison" 0

setup_case head_cargo_fails
regress_out_for "$case_dir/regress.out" keep_first_n
STUB_ALL_FAIL=head run_case "$case_dir/regress.out"
expect_rc "head cargo failure fails closed" nonzero

setup_case failed_unparseable
printf 'FAILED: 1 benchmark(s) exceeded 15%% threshold:\n  - weird line without a file\n' > "$case_dir/regress.out"
run_case "$case_dir/regress.out"
expect_rc "FAILED output with no parseable target fails closed" nonzero

setup_case unknown_target
regress_out_for "$case_dir/regress.out" mystery_bench
run_case "$case_dir/regress.out"
expect_rc "unknown bench target fails closed" nonzero

setup_case stale
regress_out_for "$case_dir/regress.out" keep_first_n
for side in "$case_dir/head-target" "$case_dir/runner/base-target"; do
    d="$side/criterion/stale_group/case/pr777x1"
    mkdir -p "$d"
    v=1000000
    [[ "$side" == "$case_dir/head-target" ]] && v=3000000
    printf '{"median":{"point_estimate":%s}}\n' "$v" > "$d/estimates.json"
    touch -d "2001-01-01" "$d/estimates.json"
done
STUB_HEAD_RATIOS="1.0 1.0 1.0" run_case "$case_dir/regress.out"
expect_rc "stale estimates from older runs are ignored" 0

setup_case mapping
regress_out_for "$case_dir/regress.out" keep_first_n per_item_costs pipeline_stages parser_bench color_picker_bench algorithms_bench
run_case "$case_dir/regress.out"
expect_rc "all six targets run" 0
root="$(cd "$case_dir/repo" && pwd -P)"
base_root="$case_dir/runner/base"
for spec in \
    "keep_first_n|marigold-impl|--features tokio" \
    "per_item_costs|marigold-impl|--features tokio" \
    "pipeline_stages|marigold-impl|--features tokio" \
    "parser_bench|marigold-grammar|" \
    "color_picker_bench|examples/color-palette-picker|--features tokio" \
    "algorithms_bench|examples/algorithms|--features tokio"; do
    IFS='|' read -r t wd feat <<< "$spec"
    tail="--bench $t"
    [[ -n "$feat" ]] && tail="$tail $feat"
    for round in 1 2 3; do
        line="args=bench $tail -- --save-baseline pr777x$round"
        for r in "$root" "$base_root"; do
            grep -qxF "cwd=$r/$wd $line" "$case_dir/stub.log" && check "$t round $round runs in $(basename "$r")/$wd" 1 || check "$t round $round runs in $(basename "$r")/$wd" 0
        done
    done
done

setup_case interleave
regress_out_for "$case_dir/regress.out" keep_first_n
run_case "$case_dir/regress.out"
order="$(sed -E 's/^cwd=([^ ]*) args=.*(pr777x[0-9]).*/\1 \2/' "$case_dir/stub.log" | awk '{ side = ($1 ~ /\/base\//) ? "base" : "head"; print side, $2 }' | tr '\n' ',')"
want="base pr777x1,head pr777x1,base pr777x2,head pr777x2,base pr777x3,head pr777x3,"
[[ "$order" == "$want" ]] && check "base and head runs interleave per round" 1 || { check "base and head runs interleave per round ($order)" 0; }

setup_case cleanup
regress_out_for "$case_dir/regress.out" keep_first_n
run_case "$case_dir/regress.out"
[[ ! -d "$case_dir/runner/base" ]] && check "base worktree removed afterwards" 1 || check "base worktree removed afterwards" 0

[[ -f "$checker" ]] || { echo "FAIL: missing $checker"; fails=$((fails + 1)); }

echo ""
echo "$passes passed, $fails failed"
[[ "$fails" -eq 0 ]]
