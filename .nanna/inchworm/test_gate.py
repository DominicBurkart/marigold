import pathlib
import subprocess
import sys
import tempfile
import unittest

GATE = pathlib.Path(__file__).resolve().parent / "gate.py"

BASE = {
    "id": "a-one",
    "surface": "interface",
    "statement": "something holds",
    "status": "candidate",
    "provable": "none",
    "covered_by": [],
}


def fmt(v):
    if isinstance(v, list):
        return "[" + ", ".join(fmt(x) for x in v) + "]"
    if isinstance(v, int):
        return str(v)
    return '"' + v.replace("\\", "\\\\").replace('"', '\\"') + '"'


def render(entries):
    out = []
    for e in entries:
        out.append("[[invariant]]")
        for k, v in e.items():
            out.append(f"{k} = {fmt(v)}")
        out.append("")
    return "\n".join(out)


class GateCase(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.tmp.name)
        (self.root / "tests").mkdir()
        (self.root / "marigold-impl/src").mkdir(parents=True)
        (self.root / "tests/t.rs").write_text("fn real_test() {}\n")
        (self.root / "marigold-impl/src/x.rs").write_text("fn other_test() {}\n")
        (self.root / "outside.rs").write_text("fn real_test() {}\n")

    def tearDown(self):
        self.tmp.cleanup()

    def ledger(self, *entries):
        merged = [{**BASE, **e} for e in entries]
        (self.root / "tests/invariants.toml").write_text(render(merged))

    def run_gate(self, *args):
        p = subprocess.run(
            [sys.executable, "-I", str(GATE), *args],
            capture_output=True,
            text=True,
        )
        return p.returncode, p.stdout, p.stderr

    def phase(self, phase="ledger"):
        return self.run_gate(phase, str(self.root))

    def assert_ok(self, phase="ledger"):
        code, out, err = self.phase(phase)
        self.assertEqual(code, 0, out + err)

    def assert_fail(self, needle, phase="ledger"):
        code, out, err = self.phase(phase)
        self.assertEqual(code, 1, out + err)
        self.assertNotIn("Traceback", out + err)
        self.assertIn(needle, out + err)


class UsageTests(GateCase):
    def test_no_args_is_usage_error(self):
        code, out, err = self.run_gate()
        self.assertEqual(code, 2)
        self.assertIn("usage", err.lower())
        self.assertNotIn("Traceback", err)

    def test_unknown_phase_is_usage_error(self):
        self.ledger({})
        code, out, err = self.run_gate("bogus", str(self.root))
        self.assertEqual(code, 2)
        self.assertNotIn("Traceback", err)

    def test_missing_ledger_is_clean_error(self):
        self.assert_fail("invariants.toml")

    def test_invalid_toml_is_clean_error(self):
        (self.root / "tests/invariants.toml").write_text("[[invariant\n")
        self.assert_fail("invariants.toml")

    def test_no_invariants_table_is_clean_error(self):
        (self.root / "tests/invariants.toml").write_text("x = 1\n")
        self.assert_fail("invariant")


class SchemaTests(GateCase):
    def test_valid_candidate_passes(self):
        self.ledger({})
        self.assert_ok()

    def test_duplicate_ids_rejected(self):
        self.ledger({}, {})
        self.assert_fail("duplicate id")

    def test_bad_status_rejected(self):
        self.ledger({"status": "done"})
        self.assert_fail("status")

    def test_bad_surface_rejected(self):
        self.ledger({"surface": "public"})
        self.assert_fail("surface")

    def test_bad_provable_rejected(self):
        self.ledger({"provable": "coq"})
        self.assert_fail("provable")

    def test_missing_field_rejected(self):
        self.ledger({})
        path = self.root / "tests/invariants.toml"
        path.write_text(path.read_text().replace('statement = "something holds"\n', ""))
        self.assert_fail("statement")

    def test_covered_by_must_be_list_of_strings(self):
        (self.root / "tests/invariants.toml").write_text(
            '[[invariant]]\nid="a"\nsurface="interface"\nstatement="s"\n'
            'status="candidate"\nprovable="none"\ncovered_by="tests/t.rs"\n'
        )
        self.assert_fail("covered_by")

    def test_proof_blocker_accepted(self):
        self.ledger({"proof_blocker": "needs a pure sync core"})
        self.assert_ok()

    def test_proof_blocker_must_be_string(self):
        self.ledger({"proof_blocker": 3})
        self.assert_fail("proof_blocker")

    def test_untested_and_disputed_statuses_accepted(self):
        self.ledger({"id": "a", "status": "untested"}, {"id": "b", "status": "disputed"})
        self.assert_ok()


class CoveredByTests(GateCase):
    def test_covered_requires_entries(self):
        self.ledger({"status": "covered"})
        self.assert_fail("without covered_by")

    def test_covered_with_existing_test_passes(self):
        self.ledger({"status": "covered", "covered_by": ["tests/t.rs::real_test"]})
        self.assert_ok()

    def test_path_without_name_passes_when_file_exists(self):
        self.ledger({"status": "covered", "covered_by": ["tests/t.rs"]})
        self.assert_ok()

    def test_crate_dir_path_passes(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/src/x.rs::other_test"]})
        self.assert_ok()

    def test_absolute_path_rejected(self):
        self.ledger({"status": "covered", "covered_by": [str(self.root / "tests/t.rs")]})
        self.assert_fail("absolute")

    def test_traversal_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["tests/../outside.rs::real_test"]})
        self.assert_fail("..")

    def test_path_outside_allowed_dirs_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["outside.rs::real_test"]})
        self.assert_fail("tests/")

    def test_directory_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["tests"]})
        self.assert_fail("not a file")

    def test_missing_file_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["tests/none.rs::real_test"]})
        self.assert_fail("missing")

    def test_symlink_escape_rejected(self):
        (self.root / "tests/link.rs").symlink_to(self.root / "outside.rs")
        self.ledger({"status": "covered", "covered_by": ["tests/link.rs::real_test"]})
        self.assert_fail("outside")

    def test_named_test_must_exist_in_file(self):
        self.ledger({"status": "covered", "covered_by": ["tests/t.rs::ghost_test"]})
        self.assert_fail("ghost_test")

    def test_name_must_be_a_fn(self):
        (self.root / "tests/t.rs").write_text("// real_test\nlet real_test_x = 1;\n")
        self.ledger({"status": "covered", "covered_by": ["tests/t.rs::real_test"]})
        self.assert_fail("real_test")

    def test_nested_module_name_uses_last_segment(self):
        self.ledger({"status": "covered", "covered_by": ["tests/t.rs::m::real_test"]})
        self.assert_ok()

    def test_candidate_with_bad_path_still_rejected(self):
        self.ledger({"covered_by": ["/etc/passwd"]})
        self.assert_fail("absolute")

    def test_candidate_name_not_required_to_exist(self):
        self.ledger({"covered_by": ["tests/t.rs::ghost_test"]})
        self.assert_ok()


class ProvenTests(GateCase):
    def proven(self, **kw):
        base = {
            "status": "proven",
            "provable": "kani",
            "covered_by": ["tests/t.rs::real_test"],
        }
        self.ledger({**base, **kw})

    def test_proven_requires_proof(self):
        self.proven()
        self.assert_fail("proof")

    def test_proven_with_existing_proof_passes(self):
        (self.root / "tests/proofs/kani").mkdir(parents=True)
        (self.root / "tests/proofs/kani/p.rs").write_text("")
        self.proven(proof="tests/proofs/kani/p.rs")
        self.assert_ok()

    def test_proof_must_be_under_proofs(self):
        self.proven(proof="tests/t.rs")
        self.assert_fail("tests/proofs/")

    def test_proof_traversal_rejected(self):
        self.proven(proof="tests/proofs/../t.rs")
        self.assert_fail("..")

    def test_proof_absolute_rejected(self):
        self.proven(proof="/tests/proofs/p.rs")
        self.assert_fail("absolute")

    def test_proof_missing_file_rejected(self):
        self.proven(provable="lean", proof="tests/proofs/lean/none.lean")
        self.assert_fail("missing")

    def test_proven_with_provable_none_rejected(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.lean").write_text("")
        self.proven(provable="none", proof="tests/proofs/p.lean")
        self.assert_fail("provable")


class CoveragePhaseTests(GateCase):
    def test_interface_candidate_blocks_coverage(self):
        self.ledger({})
        self.assert_fail("not covered", "coverage")

    def test_internal_candidate_does_not_block_coverage(self):
        self.ledger({"surface": "internal"})
        self.assert_ok("coverage")

    def test_all_interface_covered_passes(self):
        self.ledger({"status": "covered", "covered_by": ["tests/t.rs::real_test"]})
        self.assert_ok("coverage")

    def test_ledger_errors_still_fail_in_coverage(self):
        self.ledger({"status": "covered", "covered_by": ["tests/nope.rs"]})
        self.assert_fail("missing", "coverage")


class RealLedgerTests(unittest.TestCase):
    def test_repository_ledger_passes_ledger_phase(self):
        p = subprocess.run(
            [sys.executable, "-I", str(GATE), "ledger"],
            capture_output=True,
            text=True,
        )
        self.assertEqual(p.returncode, 0, p.stdout + p.stderr)


if __name__ == "__main__":
    unittest.main()
