import os
import re
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
        (self.root / "marigold-impl/tests").mkdir(parents=True)
        (self.root / "marigold-impl/src/lib.rs").write_text("mod x;\n")
        (self.root / "marigold-impl/tests/t.rs").write_text("#[test]\nfn real_test() {}\n")
        (self.root / "marigold-impl/src/x.rs").write_text("#[cfg(test)]\nmod t {\n    #[tokio::test]\n    async fn other_test() {}\n}\n")
        (self.root / "outside.rs").write_text("#[test]\nfn real_test() {}\n")
        (self.root / "marigold/src").mkdir(parents=True)
        (self.root / "marigold/src/x.rs").write_text("#[test]\nfn real_test() {}\n")

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
            'status="candidate"\nprovable="none"\ncovered_by="marigold-impl/tests/t.rs"\n'
        )
        self.assert_fail("covered_by")

    def test_proof_blocker_accepted(self):
        self.ledger({"proof_blocker": "needs a pure sync core"})
        self.assert_ok()

    def test_proof_blocker_must_be_string(self):
        self.ledger({"proof_blocker": 3})
        self.assert_fail("proof_blocker")

    def test_disputed_status_accepted(self):
        self.ledger({"status": "disputed"})
        self.assert_ok()

    def test_untested_status_rejected(self):
        self.ledger({"status": "untested"})
        self.assert_fail("status")

    def test_list_valued_status_is_clean_error(self):
        (self.root / "tests/invariants.toml").write_text(
            '[[invariant]]\nid="a"\nsurface="interface"\nstatement="s"\n'
            'status=["covered"]\nprovable="none"\ncovered_by=[]\n'
        )
        self.assert_fail("status")

    def test_list_valued_surface_and_provable_are_clean_errors(self):
        for key in ("surface", "provable"):
            self.ledger({})
            path = self.root / "tests/invariants.toml"
            text = path.read_text()
            path.write_text(re.sub(rf'^{key} = ".*"$', f'{key} = ["x"]', text, flags=re.M))
            self.assert_fail(key)

    def test_integer_id_is_clean_error(self):
        (self.root / "tests/invariants.toml").write_text(
            '[[invariant]]\nid=1\nsurface="interface"\nstatement="s"\n'
            'status="candidate"\nprovable="none"\ncovered_by=[]\n'
        )
        self.assert_fail("id")


class CoveredByTests(GateCase):
    def test_covered_requires_entries(self):
        self.ledger({"status": "covered"})
        self.assert_fail("without covered_by")

    def test_covered_with_existing_test_passes(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::real_test"]})
        self.assert_ok()

    def test_covered_requires_a_named_test(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs"]})
        self.assert_fail("path::name")

    def test_candidate_may_omit_name(self):
        self.ledger({"covered_by": ["marigold-impl/tests/t.rs"]})
        self.assert_ok()

    def write_t(self, text):
        (self.root / "marigold-impl/tests/t.rs").write_text(text)

    def covered_t(self, name="real_test"):
        self.ledger({"status": "covered", "covered_by": [f"marigold-impl/tests/t.rs::{name}"]})

    def test_fn_only_in_line_comment_rejected(self):
        self.write_t("// #[test]\n// fn real_test() {}\n")
        self.covered_t()
        self.assert_fail("real_test")

    def test_fn_only_in_block_comment_rejected(self):
        self.write_t("/* #[test]\nfn real_test() {} */\n")
        self.covered_t()
        self.assert_fail("real_test")

    def test_fn_only_in_string_rejected(self):
        self.write_t('const S: &str = "#[test]\\nfn real_test() {}";\n')
        self.covered_t()
        self.assert_fail("real_test")

    def test_fn_only_in_raw_string_rejected(self):
        self.write_t('const S: &str = r#"#[test]\nfn real_test() {}"#;\n')
        self.covered_t()
        self.assert_fail("real_test")

    def test_plain_non_test_fn_rejected(self):
        self.write_t("fn real_test() {}\n")
        self.covered_t()
        self.assert_fail("not a test")

    def test_ignored_test_rejected(self):
        self.write_t("#[test]\n#[ignore]\nfn real_test() {}\n")
        self.covered_t()
        self.assert_fail("ignore")

    def test_ignored_with_reason_rejected(self):
        self.write_t('#[ignore = "slow"]\n#[test]\nfn real_test() {}\n')
        self.covered_t()
        self.assert_fail("ignore")

    def test_tokio_test_with_args_accepted(self):
        self.write_t('#[tokio::test(flavor = "multi_thread", worker_threads = 4)]\nasync fn real_test() {}\n')
        self.covered_t()
        self.assert_ok()

    def test_async_std_test_accepted(self):
        self.write_t("#[async_std::test]\nasync fn real_test() {}\n")
        self.covered_t()
        self.assert_ok()

    def test_cfg_test_alone_is_not_a_test(self):
        self.write_t("#[cfg(test)]\nfn real_test() {}\n")
        self.covered_t()
        self.assert_fail("not a test")

    def test_fn_inside_proptest_macro_accepted(self):
        self.write_t("proptest! {\n    #[test]\n    fn real_test(x in 0..3) {}\n}\n")
        self.covered_t()
        self.assert_ok()

    def test_proptest_fn_without_test_attribute_rejected(self):
        self.write_t("proptest! {\n    fn real_test(x in 0..3) {}\n}\n")
        self.covered_t()
        self.assert_fail("not a test")

    def test_ignored_fn_inside_proptest_rejected(self):
        self.write_t("proptest! {\n    #[ignore]\n    #[test]\n    fn real_test(x in 0..3) {}\n}\n")
        self.covered_t()
        self.assert_fail("ignore")

    def test_fn_after_proptest_macro_is_not_inside_it(self):
        self.write_t("proptest! {\n    #[test]\n    fn other(x in 0..3) {}\n}\nfn real_test() {}\n")
        self.covered_t()
        self.assert_fail("not a test")

    def test_char_literal_quote_does_not_break_stripping(self):
        self.write_t("const Q: char = '\"';\n#[test]\nfn real_test() {}\n")
        self.covered_t()
        self.assert_ok()

    def test_lifetime_does_not_break_stripping(self):
        self.write_t("fn f<'a>(x: &'a str) -> &'a str { x }\n#[test]\nfn real_test() {}\n")
        self.covered_t()
        self.assert_ok()

    def test_production_crate_path_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["marigold/src/x.rs::real_test"]})
        self.assert_fail("not declared")

    def test_marigold_tests_dir_accepted(self):
        (self.root / "marigold/tests").mkdir()
        (self.root / "marigold/tests/y.rs").write_text("#[test]\nfn real_test() {}\n")
        self.ledger({"status": "covered", "covered_by": ["marigold/tests/y.rs::real_test"]})
        self.assert_ok()

    def test_wrong_final_segment_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::m::other_test"]})
        self.assert_fail("other_test")

    def test_crate_dir_path_passes(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/src/x.rs::other_test"]})
        self.assert_ok()

    def test_absolute_path_rejected(self):
        self.ledger({"status": "covered", "covered_by": [str(self.root / "marigold-impl/tests/t.rs")]})
        self.assert_fail("absolute")

    def test_traversal_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/../outside.rs::real_test"]})
        self.assert_fail("..")

    def test_path_outside_allowed_dirs_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["outside.rs::real_test"]})
        self.assert_fail("tests/")

    def test_directory_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests"]})
        self.assert_fail("not a file")

    def test_missing_file_rejected(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/none.rs::real_test"]})
        self.assert_fail("missing")

    def test_symlink_escape_rejected(self):
        (self.root / "marigold-impl/tests/link.rs").symlink_to(self.root / "outside.rs")
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/link.rs::real_test"]})
        self.assert_fail("outside")

    def test_named_test_must_exist_in_file(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::ghost_test"]})
        self.assert_fail("ghost_test")

    def test_name_must_be_a_fn(self):
        (self.root / "marigold-impl/tests/t.rs").write_text("// real_test\nlet real_test_x = 1;\n")
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::real_test"]})
        self.assert_fail("real_test")

    def test_module_path_must_exist_in_file(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::m::real_test"]})
        self.assert_fail("m::real_test")

    def test_module_path_matching_file_accepted(self):
        self.write_t("mod m {\n    #[test]\n    fn real_test() {}\n}\n")
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::m::real_test"]})
        self.assert_ok()

    def test_candidate_with_bad_path_still_rejected(self):
        self.ledger({"covered_by": ["/etc/passwd"]})
        self.assert_fail("absolute")

    def test_candidate_name_not_required_to_exist(self):
        self.ledger({"covered_by": ["marigold-impl/tests/t.rs::ghost_test"]})
        self.assert_ok()


class CfgGatingTests(GateCase):
    def check(self, body, ok, needle=None, name="t"):
        (self.root / "marigold-impl/tests/t.rs").write_text(body)
        self.ledger({"status": "covered", "covered_by": [f"marigold-impl/tests/t.rs::{name}"]})
        if ok:
            self.assert_ok()
        else:
            self.assert_fail(needle or name)

    def test_cfg_not_test_module_rejected(self):
        self.check("#[cfg(not(test))]\nmod m {\n    #[test]\n    fn t() {}\n}\n", False, "cfg")

    def test_nonexistent_feature_module_rejected(self):
        self.check('#[cfg(feature = "nonexistent")]\nmod m {\n    #[test]\n    fn t() {}\n}\n', False, "cfg")

    def test_nonexistent_target_os_on_fn_rejected(self):
        self.check('#[test]\n#[cfg(target_os = "nonexistent")]\nfn t() {}\n', False, "cfg")

    def test_empty_any_rejected(self):
        self.check("#[test]\n#[cfg(any())]\nfn t() {}\n", False, "cfg")

    def test_cfg_attr_ignore_rejected(self):
        self.check("#[test]\n#[cfg_attr(all(), ignore)]\nfn t() {}\n", False, "unsupported attribute form")

    def test_cfg_attr_test_is_unsupported(self):
        self.check("#[cfg_attr(test, test)]\nfn t() {}\n", False, "unsupported attribute form")

    def test_macro_rules_body_rejected(self):
        self.check("macro_rules! m {\n    () => {\n        #[test]\n        fn t() {}\n    };\n}\n", False, "macro")

    def test_fn_nested_in_fn_rejected(self):
        self.check("fn outer() {\n    #[test]\n    fn t() {}\n}\n", False, "nested")

    def test_fn_nested_in_test_fn_rejected(self):
        self.check("#[test]\nfn outer() {\n    #[test]\n    fn t() {}\n}\n", False, "nested")

    def test_cfg_test_module_accepted(self):
        self.check("#[cfg(test)]\nmod m {\n    #[test]\n    fn t() {}\n}\n", True)

    def test_cfg_test_on_fn_accepted(self):
        self.check("#[test]\n#[cfg(test)]\nfn t() {}\n", True)

    def test_feature_tokio_module_accepted(self):
        self.check('#[cfg(feature = "tokio")]\nmod m {\n    #[tokio::test]\n    async fn t() {}\n}\n', True)

    def test_feature_async_std_module_accepted(self):
        self.check('#[cfg(feature = "async-std")]\nmod m {\n    #[async_std::test]\n    async fn t() {}\n}\n', True)

    def test_any_of_runtime_features_accepted(self):
        self.check(
            '#[cfg(any(feature = "tokio", feature = "async-std"))]\nmod m {\n    #[tokio::test]\n    async fn t() {}\n}\n',
            True,
        )

    def test_nested_allowed_modules_accepted(self):
        self.check("#[cfg(test)]\nmod a {\n    mod b {\n        #[test]\n        fn t() {}\n    }\n}\n", True)

    def test_plain_module_accepted(self):
        self.check("mod m {\n    #[test]\n    fn t() {}\n}\n", True)

    def test_should_panic_accepted(self):
        self.check('#[test]\n#[should_panic(expected = "x")]\nfn t() {}\n', True)

    def test_inner_file_cfg_feature_accepted(self):
        self.check('#![cfg(feature = "tokio")]\n#[test]\nfn t() {}\n', True)

    def test_inner_file_cfg_bad_rejected(self):
        self.check('#![cfg(feature = "nonexistent")]\n#[test]\nfn t() {}\n', False, "cfg")

    def test_unknown_attribute_on_fn_rejected(self):
        self.check("#[test]\n#[some_macro]\nfn t() {}\n", False, "unsupported attribute form")

    def test_extern_c_test_fn_accepted(self):
        self.check('#[test]\npub extern "C" fn t() {}\n', True)

    def test_duplicate_name_in_file_rejected(self):
        self.check("#[test]\nfn t() {}\nmod m {\n    fn t() {}\n}\n", False, "ambiguous")

    def test_duplicate_name_disambiguated_by_module_path(self):
        self.check("fn t() {}\nmod m {\n    #[test]\n    fn t() {}\n}\n", True, name="m::t")

    def test_module_path_selecting_non_test_rejected(self):
        self.check("#[test]\nfn t() {}\nmod m {\n    fn t() {}\n}\n", False, "not a test", name="m::t")

    def test_wrong_module_path_rejected(self):
        self.check("mod m {\n    #[test]\n    fn t() {}\n}\n", False, "x::t".split("::")[1], name="x::t")


class ProvenTests(GateCase):
    def proven(self, **kw):
        base = {
            "status": "proven",
            "provable": "kani",
            "covered_by": ["marigold-impl/tests/t.rs::real_test"],
        }
        self.ledger({**base, **kw})

    def test_proven_requires_proof(self):
        self.proven()
        self.assert_fail("proof")

    def test_proven_with_existing_proof_passes(self):
        (self.root / "tests/proofs/kani").mkdir(parents=True)
        (self.root / "tests/proofs/kani/p.rs").write_text("#[kani::proof]\nfn proof() {}\n")
        self.proven(proof="tests/proofs/kani/p.rs")
        self.assert_ok()

    def test_proof_must_be_under_proofs(self):
        self.proven(proof="marigold-impl/tests/t.rs")
        self.assert_fail("tests/proofs/")

    def test_proof_traversal_rejected(self):
        self.proven(proof="tests/proofs/../t.rs")
        self.assert_fail("..")

    def test_proof_absolute_rejected(self):
        self.proven(proof="/tests/proofs/p.rs")
        self.assert_fail("absolute")

    def test_empty_proof_file_rejected(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.rs").write_text("  \n")
        self.proven(proof="tests/proofs/p.rs")
        self.assert_fail("empty")

    def test_non_rs_covered_by_rejected(self):
        (self.root / "tests/proofs").mkdir(exist_ok=True)
        (self.root / "tests/proofs/fake.txt").write_text("#[test]\nfn t() {}\n")
        self.ledger({"status": "covered", "covered_by": ["tests/proofs/fake.txt::t"]})
        self.assert_fail(".rs")

    def test_non_rs_candidate_covered_by_rejected(self):
        (self.root / "marigold-impl/tests/data.txt").write_text("x")
        self.ledger({"covered_by": ["marigold-impl/tests/data.txt"]})
        self.assert_fail(".rs")

    def test_proof_symlink_escaping_proofs_rejected(self):
        (self.root / "Cargo.toml").write_text("kani::proof\n")
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/link.rs").symlink_to(self.root / "Cargo.toml")
        self.proven(proof="tests/proofs/link.rs")
        self.assert_fail("outside")

    def test_proof_suffix_must_be_rs_or_lean(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.txt").write_text("kani::proof\n")
        self.proven(proof="tests/proofs/p.txt")
        self.assert_fail("suffix")

    def test_rs_proof_needs_a_harness(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.rs").write_text("fn helper() {}\n")
        self.proven(proof="tests/proofs/p.rs")
        self.assert_fail("harness")

    def test_rs_proof_with_kani_harness_accepted(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.rs").write_text("#[cfg(kani)]\n#[kani::proof]\nfn check() {}\n")
        self.proven(proof="tests/proofs/p.rs")
        self.assert_ok()

    def test_rs_proof_harness_only_in_comment_rejected(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.rs").write_text("// #[kani::proof]\nfn check() {}\n")
        self.proven(proof="tests/proofs/p.rs")
        self.assert_fail("harness")

    def test_lean_proof_accepted(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.lean").write_text("theorem t : True := trivial\n")
        self.proven(provable="lean", proof="tests/proofs/p.lean")
        self.assert_ok()

    def test_proof_missing_file_rejected(self):
        self.proven(provable="lean", proof="tests/proofs/lean/none.lean")
        self.assert_fail("missing")

    def test_proven_with_provable_none_rejected(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.lean").write_text("theorem t : True := trivial\n")
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
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::real_test"]})
        self.assert_ok("coverage")

    def test_ledger_errors_still_fail_in_coverage(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/nope.rs"]})
        self.assert_fail("missing", "coverage")


class BypassTests(GateCase):
    def check(self, body, ok, needle=None, name="t", path="marigold-impl/tests/t.rs"):
        full = self.root / path
        full.parent.mkdir(parents=True, exist_ok=True)
        full.write_text(body)
        self.ledger({"status": "covered", "covered_by": [f"{path}::{name}"]})
        if ok:
            self.assert_ok()
        else:
            self.assert_fail(needle or name)

    def test_whitespace_inside_feature_string_rejected(self):
        self.check('#[cfg(feature = "to kio")]\nmod m {\n    #[test]\n    fn t() {}\n}\n', False, "cfg")

    def test_whitespace_around_tokens_accepted(self):
        self.check('#[cfg( feature  =  "tokio" )]\nmod m {\n    #[test]\n    fn t() {}\n}\n', True)

    def test_whitespace_before_cfg_paren_accepted(self):
        self.check("#[cfg (test)]\nmod m {\n    #[test]\n    fn t() {}\n}\n", True)

    def test_mod_inner_cfg_not_test_rejected(self):
        self.check("mod tests {\n    #![cfg(not(test))]\n    #[test]\n    fn t() {}\n}\n", False, "cfg")

    def test_cfg_test_mod_inner_any_empty_rejected(self):
        self.check("#[cfg(test)]\nmod tests {\n    #![cfg(any())]\n    #[test]\n    fn t() {}\n}\n", False, "cfg")

    def test_inner_attribute_mid_file_rejected(self):
        self.check("fn a() {}\n#![cfg(not(test))]\n#[test]\nfn t() {}\n", False, "inner attribute")

    def test_inner_attribute_after_item_in_body_rejected(self):
        self.check("mod m {\n    fn a() {}\n    #![cfg(not(test))]\n    #[test]\n    fn t() {}\n}\n", False, "inner attribute")

    def test_proptest_config_inner_attribute_accepted(self):
        self.check("proptest! {\n    #![proptest_config(Default::default())]\n    #[test]\n    fn t(x in 0..3) {}\n}\n", True)

    def test_qualified_proptest_head_accepted(self):
        self.check("proptest::proptest! {\n    #[test]\n    fn t(x in 0..3) {}\n}\n", True)

    def test_other_macro_head_rejected(self):
        self.check("my_proptest! {\n    #[test]\n    fn t(x in 0..3) {}\n}\n", False, "macro")

    def test_raw_byte_string_hides_fn(self):
        self.check('const S: &[u8] = br#"#[test]\nfn t() {}"#;\n', False, "no fn")

    def test_raw_c_string_hides_fn(self):
        self.check('const S: &CStr = cr#"#[test]\nfn t() {}"#;\n', False, "no fn")

    def test_byte_string_hides_fn(self):
        self.check('const S: &[u8] = b"#[test] fn t() {}";\n', False, "no fn")

    def test_c_string_hides_fn(self):
        self.check('const S: &CStr = c"#[test] fn t() {}";\n', False, "no fn")

    def test_byte_char_quote_does_not_desync(self):
        self.check("const Q: u8 = b'\"';\n#[test]\nfn t() {}\n", True)

    def test_many_hash_raw_string_hides_fn(self):
        hashes = "#" * 100
        self.check(f'const S: &str = r{hashes}"#[test] fn t() {{}} "# "{hashes};\n', False, "no fn")

    def test_string_with_hash_quote_inside_many_hashes_resyncs(self):
        hashes = "#" * 100
        self.check(f'const S: &str = r{hashes}"x"{hashes};\n#[test]\nfn t() {{}}\n', True)

    def test_only_listed_test_attributes_accepted(self):
        self.check("#[foo::test]\nfn t() {}\n", False, "unsupported attribute form")

    def test_rstest_accepted(self):
        self.check("#[rstest::rstest]\nfn t() {}\n", True)

    def test_spaced_hash_attribute_still_seen(self):
        self.check("#[test]\n# [cfg(any())]\nfn t() {}\n", False, "cfg")

    def test_orphan_src_file_rejected(self):
        self.check("#[test]\nfn t() {}\n", False, "not declared", path="marigold-impl/src/orphan.rs")

    def test_declared_src_file_accepted(self):
        self.check("#[cfg(test)]\nmod m {\n    #[test]\n    fn t() {}\n}\n", True, path="marigold-impl/src/x.rs")

    def test_cfg_gated_mod_declaration_rejected(self):
        (self.root / "marigold-impl/src/lib.rs").write_text('#[cfg(feature = "nonexistent")]\nmod x;\n')
        self.check("#[test]\nfn t() {}\n", False, "cfg", path="marigold-impl/src/x.rs")

    def test_nested_declared_src_file_accepted(self):
        (self.root / "marigold-impl/src/lib.rs").write_text("mod a;\n")
        (self.root / "marigold-impl/src/a").mkdir()
        (self.root / "marigold-impl/src/a.rs").write_text("mod b;\n")
        self.check("#[test]\nfn t() {}\n", True, path="marigold-impl/src/a/b.rs")

    def test_mod_rs_declared_file_accepted(self):
        (self.root / "marigold-impl/src/lib.rs").write_text("mod a;\n")
        (self.root / "marigold-impl/src/a").mkdir()
        (self.root / "marigold-impl/src/a/mod.rs").write_text("#[test]\nfn t() {}\n")
        self.check("#[test]\nfn t() {}\n", True, path="marigold-impl/src/a/mod.rs")

    def test_benches_rejected(self):
        self.check("#[test]\nfn t() {}\n", False, "compiled", path="marigold-impl/benches/b.rs")

    def test_nested_tests_dir_file_rejected(self):
        self.check("#[test]\nfn t() {}\n", False, "compiled", path="marigold-impl/tests/sub/b.rs")

    def test_proofs_dir_rs_rejected_as_test_citation(self):
        self.check("#[test]\nfn t() {}\n", False, "compiled", path="tests/proofs/p.rs")

    def test_unknown_crate_dir_rejected(self):
        self.check("#[test]\nfn t() {}\n", False, "crate", path="target/tests/b.rs")

    def test_tests_crate_integration_file_accepted(self):
        self.check("#[test]\nfn t() {}\n", True, path="tests/tests/b.rs")

    def test_tests_crate_top_level_file_rejected(self):
        self.check("#[test]\nfn t() {}\n", False, "compiled", path="tests/b.rs")


class ProofStrictnessTests(GateCase):
    def prove(self, rel, text, **kw):
        full = self.root / rel
        full.parent.mkdir(parents=True, exist_ok=True)
        full.write_text(text)
        entry = {
            "status": "proven",
            "provable": "kani",
            "covered_by": ["marigold-impl/tests/t.rs::real_test"],
            "proof": rel,
        }
        self.ledger({**entry, **kw})

    def test_lean_without_theorem_rejected(self):
        self.prove("tests/proofs/p.lean", "def x := 1\n", provable="lean")
        self.assert_fail("theorem")

    def test_lean_with_sorry_rejected(self):
        self.prove("tests/proofs/p.lean", "theorem t : True := by sorry\n", provable="lean")
        self.assert_fail("sorry")

    def test_lean_with_axiom_rejected(self):
        self.prove("tests/proofs/p.lean", "axiom a : True\ntheorem t : True := a\n", provable="lean")
        self.assert_fail("axiom")

    def test_lean_with_admit_rejected(self):
        self.prove("tests/proofs/p.lean", "theorem t : True := by admit\n", provable="lean")
        self.assert_fail("admit")

    def test_lean_sorry_in_comment_ignored(self):
        self.prove("tests/proofs/p.lean", "-- no sorry here\n/- axiom -/\ntheorem t : True := trivial\n", provable="lean")
        self.assert_ok()

    def test_lean_lemma_accepted(self):
        self.prove("tests/proofs/p.lean", "lemma t : True := trivial\n", provable="lean")
        self.assert_ok()

    def test_kani_requires_rs_suffix(self):
        self.prove("tests/proofs/p.lean", "theorem t : True := trivial\n", provable="kani")
        self.assert_fail("suffix")

    def test_lean_requires_lean_suffix(self):
        self.prove("tests/proofs/p.rs", "#[kani::proof]\nfn c() {}\n", provable="lean")
        self.assert_fail("suffix")

    def test_test_attribute_is_not_a_proof_harness(self):
        self.prove("tests/proofs/p.rs", "#[test]\nfn c() {}\n")
        self.assert_fail("harness")

    def test_kani_harness_in_macro_rules_rejected(self):
        self.prove("tests/proofs/p.rs", "macro_rules! m {\n    () => {\n        #[kani::proof]\n        fn c() {}\n    };\n}\n")
        self.assert_fail("harness")

    def test_kani_harness_under_bad_cfg_rejected(self):
        self.prove("tests/proofs/p.rs", "#[cfg(not(kani))]\nmod m {\n    #[kani::proof]\n    fn c() {}\n}\n")
        self.assert_fail("harness")

    def test_kani_harness_under_cfg_kani_accepted(self):
        self.prove("tests/proofs/p.rs", "#[cfg(kani)]\nmod m {\n    #[kani::proof]\n    #[kani::unwind(4)]\n    fn c() {}\n}\n")
        self.assert_ok()

    def test_kani_harness_nested_in_fn_rejected(self):
        self.prove("tests/proofs/p.rs", "fn outer() {\n    #[kani::proof]\n    fn c() {}\n}\n")
        self.assert_fail("harness")

    def test_proof_file_shared_by_two_ids_rejected(self):
        self.prove("tests/proofs/p.rs", "#[kani::proof]\nfn c() {}\n")
        entry = {
            "status": "proven",
            "provable": "kani",
            "covered_by": ["marigold-impl/tests/t.rs::real_test"],
            "proof": "tests/proofs/p.rs",
        }
        self.ledger({"id": "a", **entry}, {"id": "b", **entry})
        self.assert_fail("shared")

    def test_symlinked_proofs_dir_rejected(self):
        (self.root / "elsewhere").mkdir()
        (self.root / "elsewhere/p.rs").write_text("#[kani::proof]\nfn c() {}\n")
        (self.root / "tests/proofs").symlink_to(self.root / "elsewhere")
        entry = {
            "status": "proven",
            "provable": "kani",
            "covered_by": ["marigold-impl/tests/t.rs::real_test"],
            "proof": "tests/proofs/p.rs",
        }
        self.ledger(entry)
        self.assert_fail("symlink")


class RobustnessTests(GateCase):
    def timed(self, body, name="t", path="marigold-impl/tests/t.rs", limit=5.0):
        import time

        (self.root / path).write_text(body)
        self.ledger({"status": "covered", "covered_by": [f"{path}::{name}"]})
        start = time.monotonic()
        result = self.phase()
        elapsed = time.monotonic() - start
        self.assertLess(elapsed, limit, f"took {elapsed:.1f}s")
        return result

    def test_many_qualifiers_is_fast(self):
        code, out, err = self.timed("pub " * 20000 + "fn t() {}\n")
        self.assertEqual(code, 1)
        self.assertNotIn("Traceback", out + err)

    def test_many_raw_strings_is_fast(self):
        code, out, err = self.timed('const A: &str = r#"x"#;\n' * 20000 + "#[test]\nfn t() {}\n")
        self.assertEqual(code, 0, out + err)

    def test_many_duplicate_fn_names_is_fast(self):
        code, out, err = self.timed("#[test]\nfn t() {}\n" * 3000)
        self.assertEqual(code, 1)
        self.assertIn("ambiguous", out)

    def test_many_braces_is_fast(self):
        code, out, err = self.timed("mod m {\n" * 2000 + "#[test]\nfn t() {}\n" + "}\n" * 2000)
        self.assertNotIn("Traceback", out + err)

    def test_id_uniqueness_is_normalised(self):
        self.ledger({"id": "Straße"}, {"id": "STRASSE "})
        self.assert_fail("duplicate id")

    def test_composed_and_decomposed_ids_collide(self):
        self.ledger({"id": "é"}, {"id": "é"})
        self.assert_fail("duplicate id")

    def test_invalid_utf8_ledger_is_clean_error(self):
        (self.root / "tests/invariants.toml").write_bytes(b"\xff\xfe[[invariant]]\n")
        self.assert_fail("UTF-8")


FAKE_CARGO = """#!/bin/sh
echo "$@" >> "$FAKE_CARGO_LOG"
echo "TARGET=$CARGO_TARGET_DIR GIT=${GIT_DIR:-unset}" >> "$FAKE_CARGO_LOG"
case "$*" in
  *--list*) printf '%b' "$FAKE_CARGO_LIST"; exit 0 ;;
  *--exact*) printf '%b' "$FAKE_CARGO_RUN"; exit ${FAKE_CARGO_RC:-0} ;;
esac
exit 3
"""

LIST_ONE = "real_test: test\\n\\n1 test, 0 benchmarks\\n"
RUN_OK = "running 1 test\\ntest real_test ... ok\\n\\ntest result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out\\n"


class RunModeTests(GateCase):
    def setUp(self):
        super().setUp()
        self.shim = self.root / "shim"
        self.shim.mkdir()
        cargo = self.shim / "cargo"
        cargo.write_text(FAKE_CARGO)
        cargo.chmod(0o755)
        self.log = self.root / "cargo.log"
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::real_test"]})

    def run_mode(self, lst=LIST_ONE, run=RUN_OK, rc="0", target="/tmp/fake-target", extra_env=None, phase="ledger", flag="--run"):
        env = {
            "PATH": f"{self.shim}:{os.environ['PATH']}",
            "FAKE_CARGO_LOG": str(self.log),
            "FAKE_CARGO_LIST": lst,
            "FAKE_CARGO_RUN": run,
            "FAKE_CARGO_RC": rc,
            "GIT_DIR": "/nonexistent",
        }
        if target:
            env["CARGO_TARGET_DIR"] = target
        env.update(extra_env or {})
        args = [phase, str(self.root)] + ([flag] if flag else [])
        p = subprocess.run([sys.executable, "-I", str(GATE), *args], capture_output=True, text=True, env=env)
        return p.returncode, p.stdout, p.stderr

    def calls(self):
        return self.log.read_text().splitlines() if self.log.exists() else []

    def test_default_mode_never_invokes_cargo(self):
        code, out, err = self.run_mode(flag=None)
        self.assertEqual(code, 0, out + err)
        self.assertEqual(self.calls(), [])

    def test_run_passes_when_one_test_collected_and_passes(self):
        code, out, err = self.run_mode()
        self.assertEqual(code, 0, out + err)
        calls = self.calls()
        self.assertTrue(any("--list" in c for c in calls))
        self.assertTrue(any("--exact" in c and "real_test" in c for c in calls))

    def test_run_selects_package_and_test_target(self):
        self.run_mode()
        first = self.calls()[0]
        self.assertIn("-p marigold-impl", first)
        self.assertIn("--test t", first)

    def test_run_selects_lib_for_src_files(self):
        (self.root / "marigold-impl/src/x.rs").write_text("#[cfg(test)]\nmod t {\n    #[test]\n    fn real_test() {}\n}\n")
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/src/x.rs::t::real_test"]})
        code, out, err = self.run_mode(lst="x::t::real_test: test\\n", run=RUN_OK)
        self.assertEqual(code, 0, out + err)
        self.assertIn("--lib", self.calls()[0])

    def test_run_passes_features_from_cfg(self):
        (self.root / "marigold-impl/tests/t.rs").write_text('#![cfg(feature = "tokio")]\n#[test]\nfn real_test() {}\n')
        code, out, err = self.run_mode()
        self.assertEqual(code, 0, out + err)
        self.assertIn("--features tokio", self.calls()[0])

    def test_ledger_features_are_passed_to_cargo(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::real_test"], "features": ["tokio", "io"]})
        code, out, err = self.run_mode()
        self.assertEqual(code, 0, out + err)
        self.assertIn("--features io,tokio", self.calls()[0])

    def test_ledger_features_must_be_names(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::real_test"], "features": ["a b; rm"]})
        code, out, err = self.run_mode(flag=None)
        self.assertEqual(code, 1)
        self.assertIn("features", out)

    def test_ledger_features_must_be_a_list(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::real_test"], "features": "tokio"})
        code, out, err = self.run_mode(flag=None)
        self.assertEqual(code, 1)
        self.assertIn("features", out)

    def test_zero_collected_fails(self):
        code, out, err = self.run_mode(lst="\\n0 tests, 0 benchmarks\\n")
        self.assertEqual(code, 1)
        self.assertIn("collected 0", out)

    def test_two_collected_fails(self):
        code, out, err = self.run_mode(lst="a::real_test: test\\nb::real_test: test\\n")
        self.assertEqual(code, 1)
        self.assertIn("collected 2", out)

    def test_failing_test_fails(self):
        code, out, err = self.run_mode(run="test real_test ... FAILED\\n\\ntest result: FAILED. 0 passed; 1 failed\\n", rc="101")
        self.assertEqual(code, 1)
        self.assertIn("did not pass", out)

    def test_exit_zero_without_one_passed_fails(self):
        code, out, err = self.run_mode(run="running 0 tests\\n\\ntest result: ok. 0 passed; 0 failed\\n")
        self.assertEqual(code, 1)
        self.assertIn("did not pass", out)

    def test_candidate_entries_are_not_run(self):
        self.ledger({"covered_by": ["marigold-impl/tests/t.rs::real_test"]})
        code, out, err = self.run_mode()
        self.assertEqual(code, 0, out + err)
        self.assertEqual(self.calls(), [])

    def test_static_errors_skip_the_run(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::ghost"]})
        code, out, err = self.run_mode()
        self.assertEqual(code, 1)
        self.assertEqual(self.calls(), [])

    def test_missing_target_dir_is_usage_error(self):
        code, out, err = self.run_mode(target=None)
        self.assertEqual(code, 2)
        self.assertIn("CARGO_TARGET_DIR", err)
        self.assertEqual(self.calls(), [])

    def test_git_env_is_scrubbed_and_target_is_forwarded(self):
        self.run_mode(target="/tmp/my-target")
        env_line = self.calls()[1]
        self.assertIn("TARGET=/tmp/my-target", env_line)
        self.assertIn("GIT=unset", env_line)

    def test_missing_cargo_is_clean_error(self):
        code, out, err = self.run_mode(extra_env={"PATH": "/nonexistent"})
        self.assertEqual(code, 1)
        self.assertIn("cargo", out)
        self.assertNotIn("Traceback", out + err)

    def test_coverage_run_also_runs(self):
        self.ledger({"status": "covered", "covered_by": ["marigold-impl/tests/t.rs::real_test"]})
        code, out, err = self.run_mode(phase="coverage")
        self.assertEqual(code, 0, out + err)
        self.assertTrue(self.calls())

    def test_proven_citations_are_run(self):
        (self.root / "tests/proofs").mkdir()
        (self.root / "tests/proofs/p.rs").write_text("#[kani::proof]\nfn c() {}\n")
        self.ledger(
            {
                "status": "proven",
                "provable": "kani",
                "covered_by": ["marigold-impl/tests/t.rs::real_test"],
                "proof": "tests/proofs/p.rs",
            }
        )
        code, out, err = self.run_mode()
        self.assertEqual(code, 0, out + err)
        self.assertTrue(self.calls())



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
