import json
import os
import subprocess
import sys
import tempfile
import unittest

SCRIPT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "check_paired_regression.py")


def write_estimate(root, bench, baseline, median):
    d = os.path.join(root, "criterion", *bench.split("/"), baseline)
    os.makedirs(d, exist_ok=True)
    with open(os.path.join(d, "estimates.json"), "w") as f:
        json.dump({"median": {"point_estimate": median}, "mean": {"point_estimate": median * 3}}, f)


class Fixture:
    def __init__(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.base = os.path.join(self.tmp.name, "base")
        self.head = os.path.join(self.tmp.name, "head")

    def put(self, bench, base_vals, head_vals):
        for i, v in enumerate(base_vals):
            write_estimate(self.base, bench, "r%d" % (i + 1), v)
        for i, v in enumerate(head_vals):
            write_estimate(self.head, bench, "r%d" % (i + 1), v)

    def run(self, rounds, threshold="15", summary=None, floor=None, not_before=None):
        names = ["r%d" % (i + 1) for i in range(rounds)]
        env = dict(os.environ)
        env.pop("GITHUB_STEP_SUMMARY", None)
        env.pop("MIN_DELTA_NS", None)
        if summary:
            env["GITHUB_STEP_SUMMARY"] = summary
        env.pop("NOT_BEFORE_EPOCH", None)
        if not_before is not None:
            env["NOT_BEFORE_EPOCH"] = str(not_before)
        if floor is not None:
            env["MIN_DELTA_NS"] = str(floor)
        return subprocess.run(
            [sys.executable, SCRIPT, threshold,
             os.path.join(self.base, "criterion"), os.path.join(self.head, "criterion")] + names,
            capture_output=True, text=True, env=env)


def scaled(factors, base=1000.0):
    return [base * f for f in factors]


class PairedRegression(unittest.TestCase):
    def setUp(self):
        self.fx = Fixture()
        self.addCleanup(self.fx.tmp.cleanup)

    def check(self, bench_factors, rounds=3, threshold="15"):
        self.fx.put("g/b", [1000.0] * rounds, scaled(bench_factors))
        return self.fx.run(rounds, threshold)

    def test_identical_passes(self):
        r = self.check([1.0, 1.0, 1.0])
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn("PASSED", r.stdout)

    def test_consistent_25_percent_fails(self):
        r = self.check([1.25, 1.25, 1.25])
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        self.assertIn("g/b", r.stdout)
        self.assertIn("FAILED", r.stdout)

    def test_single_noisy_round_passes(self):
        r = self.check([1.0, 1.8, 1.0])
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_single_noisy_round_passes_five_rounds(self):
        r = self.check([1.0, 1.0, 1.8, 1.0, 1.0], rounds=5)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_14_percent_consistent_passes(self):
        r = self.check([1.14, 1.14, 1.14])
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_16_percent_consistent_fails(self):
        r = self.check([1.16, 1.16, 1.16])
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_exactly_threshold_passes(self):
        r = self.check([1.15, 1.15, 1.15])
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_majority_of_rounds_regressing_fails(self):
        r = self.check([1.3, 1.3, 1.0])
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_median_above_but_minority_cannot_happen_for_odd_rounds_improvement_passes(self):
        r = self.check([0.5, 0.6, 0.7])
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_custom_threshold(self):
        r = self.check([1.08, 1.08, 1.08], threshold="5")
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_one_bad_benchmark_fails_whole_run(self):
        self.fx.put("g/ok", [1000.0] * 3, [1000.0] * 3)
        self.fx.put("g/bad", [1000.0] * 3, [1300.0] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        self.assertIn("g/bad", r.stdout)

    def test_nested_parameterised_ids(self):
        self.fx.put("keep_first_n/c_512_3_stream_iter/512", [1000.0] * 3, [1300.0] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        self.assertIn("keep_first_n/c_512_3_stream_iter/512", r.stdout)

    def test_benchmark_missing_in_head_reported_not_failed(self):
        self.fx.put("g/ok", [1000.0] * 3, [1000.0] * 3)
        self.fx.put("g/removed", [1000.0] * 3, [])
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn("missing in head: g/removed", r.stdout)

    def test_benchmark_missing_in_base_reported_not_failed(self):
        self.fx.put("g/ok", [1000.0] * 3, [1000.0] * 3)
        self.fx.put("g/new", [], [1000.0] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn("missing in base: g/new", r.stdout)

    def test_round_missing_on_one_side_is_skipped_deterministically(self):
        self.fx.put("g/b", [1000.0] * 3, [1300.0, 1300.0])
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        self.assertIn("2 of 3 rounds", r.stdout)

    def test_no_comparable_benchmarks_is_error(self):
        self.fx.put("g/a", [1000.0] * 3, [])
        self.fx.put("g/b", [], [1000.0] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 2, r.stdout + r.stderr)
        self.assertIn("no comparable benchmarks", r.stdout + r.stderr)

    def test_missing_directories_is_error(self):
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 2, r.stdout + r.stderr)

    def test_tiny_benchmark_80_percent_every_round_fails(self):
        self.fx.put("g/arc_clone_x2", [16.0] * 3, [16.0 * 1.8] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        self.assertIn("g/arc_clone_x2", r.stdout)

    def test_tiny_benchmark_80_percent_one_round_passes(self):
        self.fx.put("g/tiny", [16.0] * 3, [16.0, 16.0 * 1.8, 16.0])
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_tiny_benchmark_80_percent_two_of_three_rounds_passes(self):
        self.fx.put("g/tiny", [16.0] * 3, [16.0 * 1.8, 16.0 * 1.8, 16.0])
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_tiny_benchmark_20_percent_passes(self):
        self.fx.put("g/tiny", [16.0] * 3, [16.0 * 1.2] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_150ns_16_percent_two_of_three_rounds_fails(self):
        self.fx.put("g/mid", [150.0] * 3, [150.0 * 1.16, 150.0 * 1.16, 150.0])
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_90ns_25_percent_all_rounds_passes(self):
        self.fx.put("g/low", [90.0] * 3, [90.0 * 1.25] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_90ns_35_percent_all_rounds_fails(self):
        self.fx.put("g/low", [90.0] * 3, [90.0 * 1.35] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_exactly_100ns_uses_upper_tier(self):
        self.fx.put("g/edge", [100.0] * 3, [120.0] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        self.assertIn("upper", r.stdout)

    def test_99ns_uses_lower_tier(self):
        self.fx.put("g/edge", [99.0] * 3, [99.0 * 1.2] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn("lower", r.stdout)

    def test_floor_boundary_default_is_5ns(self):
        self.fx.put("g/b", [10.0] * 3, [15.0] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.fx.put("g/b", [10.0] * 3, [15.5] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_upper_tier_below_floor_reported(self):
        self.fx.put("g/mid", [200.0] * 3, [200.0 * 1.16] * 3)
        r = self.fx.run(3, floor=40)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn("below floor", r.stdout)
        r = self.fx.run(3, floor=1)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_tier_printed_in_table(self):
        self.fx.put("g/mid", [1000.0] * 3, [1000.0] * 3)
        self.fx.put("g/low", [10.0] * 3, [10.0] * 3)
        r = self.fx.run(3)
        self.assertIn("| tier |", r.stdout)
        self.assertIn("upper", r.stdout)
        self.assertIn("lower", r.stdout)

    def test_nan_and_inf_are_unreadable(self):
        self.fx.put("g/a", [1000.0] * 3, [float("nan")] * 3)
        self.fx.put("g/b", [float("inf")] * 3, [1000.0] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 2, r.stdout + r.stderr)

    def test_nonfinite_round_not_counted(self):
        self.fx.put("g/a", [1000.0] * 3, [1300.0, float("nan"), 1300.0])
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        self.assertIn("2 of 3 rounds", r.stdout)

    def test_nonfinite_does_not_mask_good_benchmark(self):
        self.fx.put("g/a", [1000.0] * 3, [float("inf")] * 3)
        self.fx.put("g/ok", [1000.0] * 3, [1000.0] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)
        self.assertIn("missing in head: g/a", r.stdout)

    def test_stale_round_dirs_older_than_cutoff_ignored(self):
        self.fx.put("g/a", [1000.0] * 3, [1300.0] * 3)
        for root in (self.fx.base, self.fx.head):
            for dirpath, _d, files in os.walk(root):
                for f in files:
                    os.utime(os.path.join(dirpath, f), (1000, 1000))
        r = self.fx.run(3, not_before=5000)
        self.assertEqual(r.returncode, 2, r.stdout + r.stderr)
        r = self.fx.run(3, not_before=500)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_stale_round_mixed_with_fresh_only_uses_fresh(self):
        self.fx.put("g/a", [1000.0] * 3, [1000.0] * 3)
        stale = os.path.join(self.fx.head, "criterion", "g", "a", "r1", "estimates.json")
        write_estimate(self.fx.head, "g/a", "r1", 5000.0)
        os.utime(stale, (1000, 1000))
        r = self.fx.run(3, not_before=5000)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_five_ms_benchmark_20_percent_fails(self):
        self.fx.put("g/slow", [5e6] * 3, [6e6] * 3)
        r = self.fx.run(3)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_consistent_regression_fails_despite_noise(self):
        r = self.check([1.4, 1.2, 1.9, 1.17, 1.6], rounds=5)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)

    def test_majority_regressing_but_median_below_threshold_passes(self):
        r = self.check([1.16, 1.16, 1.0, 1.0, 1.0], rounds=5)
        self.assertEqual(r.returncode, 0, r.stdout + r.stderr)

    def test_summary_table_written(self):
        self.fx.put("g/a", [1000.0] * 3, [1300.0, 1250.0, 1000.0])
        path = os.path.join(self.fx.tmp.name, "summary.md")
        r = self.fx.run(3, summary=path)
        self.assertEqual(r.returncode, 1, r.stdout + r.stderr)
        text = open(path).read()
        self.assertIn("| benchmark |", text)
        self.assertIn("| g/a |", text)
        self.assertIn("1.300", text)
        self.assertIn("1.250", text)
        self.assertIn("FAIL", text)

    def test_stdout_contains_table_without_summary_env(self):
        self.fx.put("g/a", [1000.0] * 3, [1000.0] * 3)
        r = self.fx.run(3)
        self.assertIn("| benchmark |", r.stdout)

    def test_output_is_deterministic(self):
        self.fx.put("g/z", [1000.0] * 3, [1300.0] * 3)
        self.fx.put("g/a", [1000.0] * 3, [1300.0] * 3)
        a = self.fx.run(3).stdout
        b = self.fx.run(3).stdout
        self.assertEqual(a, b)
        self.assertLess(a.index("g/a"), a.index("g/z"))


if __name__ == "__main__":
    unittest.main()
