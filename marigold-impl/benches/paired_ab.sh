#!/usr/bin/env bash
set -uo pipefail

regress_out="${1:?usage: paired_ab.sh REGRESS_OUTPUT_FILE}"
rounds="${ROUNDS:-3}"
summary="${GITHUB_STEP_SUMMARY:-/dev/null}"
run_id="${GITHUB_RUN_ID:-local}"
runner_temp="${RUNNER_TEMP:?RUNNER_TEMP is required}"
base_sha="${BASE_SHA:?BASE_SHA is required}"
head_target_dir="${CARGO_TARGET_DIR:?CARGO_TARGET_DIR is required}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(pwd -P)"
base_tree="$runner_temp/base"
base_target_dir="$runner_temp/base-target"

note() {
    echo "$*"
    echo "$*" >> "$summary"
}

fail() {
    note ""
    note "**Paired base-vs-head check: FAILED (fail closed).** $*"
    exit 1
}

cleanup() {
    git -C "$repo_root" worktree remove --force "$base_tree" > /dev/null 2>&1 || true
    rm -rf "$base_tree" "$base_target_dir"
}

target_spec() {
    case "$1" in
        keep_first_n | per_item_costs | pipeline_stages) echo "marigold-impl|--features tokio" ;;
        parser_bench) echo "marigold-grammar|" ;;
        color_picker_bench) echo "examples/color-palette-picker|--features tokio" ;;
        algorithms_bench) echo "examples/algorithms|--features tokio" ;;
        *) return 1 ;;
    esac
}

if ! grep -q '^FAILED:' "$regress_out" 2> /dev/null; then
    echo "No benchmarks flagged by the first pass; no paired comparison needed."
    exit 0
fi

flagged="$(grep -E '^  - ' "$regress_out" | grep -E '\[[^]]*bench_[^]/]*\.txt\]$' || true)"
targets=()
while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    file="${line##*[}"
    file="${file%]}"
    t="$(basename "$file" .txt)"
    targets+=("${t#bench_}")
done <<< "$flagged"

note "### Paired base-vs-head comparison"
note ""
note "Benchmarks flagged by the first pass (against the cached baseline):"
note ""
while IFS= read -r line; do
    [[ -n "$line" ]] && note "$line"
done <<< "$flagged"
note ""

[[ ${#targets[@]} -gt 0 ]] || fail "the first pass reported a failure but no bench target could be identified."

mapfile -t targets < <(printf '%s\n' "${targets[@]}" | sort -u)

for t in "${targets[@]}"; do
    target_spec "$t" > /dev/null || fail "no working directory mapping for bench target \`$t\`."
done

trap cleanup EXIT
cleanup
start_epoch="$(date +%s)"

git -C "$repo_root" worktree add --detach "$base_tree" "$base_sha" > /dev/null 2>&1 \
    || fail "could not check out base commit \`$base_sha\`."

names=()
for ((round = 1; round <= rounds; round++)); do
    names+=("pr${run_id}x${round}")
done

run_bench() {
    local tree="$1" tdir="$2" t="$3" name="$4" spec wd feat
    spec="$(target_spec "$t")"
    wd="${spec%%|*}"
    feat="${spec#*|}"
    (
        cd "$tree/$wd" || exit 1
        # shellcheck disable=SC2086
        CARGO_TARGET_DIR="$tdir" cargo bench --bench "$t" $feat -- --save-baseline "$name"
    )
}

for ((round = 1; round <= rounds; round++)); do
    name="pr${run_id}x${round}"
    for t in "${targets[@]}"; do
        run_bench "$base_tree" "$base_target_dir" "$t" "$name" \
            || fail "base commit run of \`$t\` failed in round $round (the base may lack this bench target)."
        run_bench "$repo_root" "$head_target_dir" "$t" "$name" \
            || fail "head run of \`$t\` failed in round $round."
    done
done

NOT_BEFORE_EPOCH="$start_epoch" python3 "$here/check_paired_regression.py" 15 \
    "$base_target_dir/criterion" "$head_target_dir/criterion" "${names[@]}"
rc=$?
case "$rc" in
    0) note "**Paired base-vs-head check: PASSED.** The first-pass flags were not reproduced against the base commit." ; exit 0 ;;
    1) note "**Paired base-vs-head check: FAILED.** Regression reproduced against the base commit."; exit 1 ;;
    *) fail "the paired comparison could not run (exit $rc)." ;;
esac
