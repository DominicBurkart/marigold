#!/usr/bin/env python3
import json
import math
import os
import statistics
import sys


def collect(root, baselines, not_before):
    found = {}
    if not os.path.isdir(root):
        return found
    for dirpath, _dirs, files in os.walk(root):
        if "estimates.json" not in files:
            continue
        name = os.path.basename(dirpath)
        if name not in baselines:
            continue
        path = os.path.join(dirpath, "estimates.json")
        try:
            if os.path.getmtime(path) < not_before:
                continue
        except OSError:
            continue
        bench = os.path.relpath(os.path.dirname(dirpath), root)
        try:
            with open(path) as f:
                value = float(json.load(f)["median"]["point_estimate"])
        except (OSError, ValueError, KeyError, TypeError):
            continue
        if not math.isfinite(value):
            continue
        found.setdefault(bench, {})[name] = value
    return found


LOWER_TIER_NS = 100.0
LOWER_THRESHOLD = 30.0


def fmt_ns(v):
    return "%.1f" % v


def main(argv):
    if len(argv) < 5:
        print("usage: check_paired_regression.py THRESHOLD_PCT BASE_CRITERION_DIR HEAD_CRITERION_DIR BASELINE...", file=sys.stderr)
        return 2
    threshold = float(argv[1])
    base_dir, head_dir = argv[2], argv[3]
    rounds = argv[4:]
    floor = float(os.environ.get("MIN_DELTA_NS", "5"))
    not_before = float(os.environ.get("NOT_BEFORE_EPOCH", "0"))

    base = collect(base_dir, set(rounds), not_before)
    head = collect(head_dir, set(rounds), not_before)

    lines = []
    missing_head = sorted(b for b in base if b not in head)
    missing_base = sorted(b for b in head if b not in base)
    rows = []
    failed = []
    for bench in sorted(set(base) & set(head)):
        paired = [r for r in rounds if r in base[bench] and r in head[bench]]
        if not paired:
            missing_head.append(bench)
            continue
        med_base = statistics.median(base[bench][r] for r in paired)
        lower = med_base < LOWER_TIER_NS
        limit = 1.0 + (LOWER_THRESHOLD if lower else threshold) / 100.0
        ratios, deltas, regress = [], [], 0
        for r in paired:
            b, h = base[bench][r], head[bench][r]
            ratio = h / b if b > 0 else float("inf")
            ratios.append(ratio)
            deltas.append(h - b)
            if ratio > limit and h - b > floor:
                regress += 1
        med_ratio = statistics.median(ratios)
        med_delta = statistics.median(deltas)
        if lower:
            hit = regress == len(paired)
        else:
            hit = regress * 2 > len(paired)
        tier = "lower" if lower else "upper"
        if med_ratio > limit and med_delta > floor and hit:
            status = "FAIL"
            failed.append((bench, med_ratio, regress, len(paired)))
        elif med_ratio > limit and med_delta <= floor:
            status = "below floor"
        else:
            status = "ok"
        rows.append((bench, med_base, med_ratio, ratios, len(paired), status, tier))

    if not rows:
        print("error: no comparable benchmarks found in %s and %s" % (base_dir, head_dir))
        for b in missing_head:
            print("missing in head: %s" % b)
        for b in missing_base:
            print("missing in base: %s" % b)
        return 2

    table = ["| benchmark | base median (ns) | tier | rounds | per-round head/base | median ratio | status |",
             "|---|---|---|---|---|---|---|"]
    for bench, med_base, med_ratio, ratios, n, status, tier in rows:
        note = "" if n == len(rounds) else " (%d of %d rounds)" % (n, len(rounds))
        table.append("| %s | %s | %s | %d%s | %s | %.3f | %s |" % (
            bench, fmt_ns(med_base), tier, n, note, " ".join("%.3f" % r for r in ratios), med_ratio, status))
    text = "\n".join(table)
    print(text)
    summary = os.environ.get("GITHUB_STEP_SUMMARY")
    if summary:
        with open(summary, "a") as f:
            f.write("### Paired bench comparison (threshold %g%%, floor %g ns, %d rounds)\n\n%s\n\n" % (
                threshold, floor, len(rounds), text))

    print("")
    for bench, med_base, med_ratio, ratios, n, status, tier in rows:
        if n != len(rounds):
            print("note: %s compared in %d of %d rounds" % (bench, n, len(rounds)))
    for b in sorted(missing_head):
        print("missing in head: %s" % b)
    for b in missing_base:
        print("missing in base: %s" % b)
    for bench, med_ratio, k, n in failed:
        print("regression: %s median ratio %.3f, regressed in %d of %d rounds" % (bench, med_ratio, k, n))
    if failed:
        print("FAILED: %d benchmark(s) exceeded %g%% threshold" % (len(failed), threshold))
        return 1
    print("PASSED: all benchmarks within %g%% threshold" % threshold)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
