import os
import stat
import subprocess
import tempfile
import unittest

SCRIPT = os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
    "nanna_security_review.sh",
)

STUB = r"""#!/bin/bash
prompt=""
while [ $# -gt 0 ]; do
  case "$1" in
    -p) prompt="$2"; shift 2 ;;
    *) args="${args:-} $1"; shift ;;
  esac
done
n=$(ls "$STUB_DIR" | grep -c '\.prompt$' || true)
printf '%s' "$prompt" > "$STUB_DIR/call$n.prompt"
printf '%s' "${args:-}" > "$STUB_DIR/call$n.args"
case "${STUB_MODE:-safe}" in
  safe) printf 'Assistant: VERDICT: SAFE\nLooks fine.\n' ;;
  unsafe) printf 'Assistant: VERDICT: UNSAFE\n- hardcoded credential in config.rs\n' ;;
  garbled) printf 'Assistant: I think this is probably fine.\n' ;;
  both) printf 'VERDICT: SAFE\nVERDICT: UNSAFE\n' ;;
  empty) ;;
  fail) echo "connection refused" >&2; exit 7 ;;
  unsafe_second) if [ "$n" -eq 1 ]; then printf 'VERDICT: UNSAFE\n- bad\n'; else printf 'VERDICT: SAFE\n'; fi ;;
esac
"""

CLEAN_DIFF = "diff --git a/a.rs b/a.rs\n+fn add(a: i32, b: i32) -> i32 { a + b }\n"


def fake_aws_key():
    return "AK" + "IA" + "ABCDEFGHIJKLMNOP"


def fake_private_key_header():
    return "-----BEGIN " + "RSA PRIVATE KEY-----"


class ReviewTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.work = os.path.join(self.tmp.name, "work")
        os.mkdir(self.work)
        self.stub_dir = os.path.join(self.tmp.name, "calls")
        os.mkdir(self.stub_dir)
        self.stub = os.path.join(self.tmp.name, "nanna")
        with open(self.stub, "w") as f:
            f.write(STUB)
        os.chmod(self.stub, os.stat(self.stub).st_mode | stat.S_IXUSR)

    def run_review(self, diff, mode="safe", stub_dir=None, **env):
        e = {
            "PATH": os.environ["PATH"],
            "NANNA_BIN": self.stub,
            "STUB_DIR": stub_dir or self.stub_dir,
            "STUB_MODE": mode,
            "TMPDIR": self.work,
        }
        e.update(env)
        return subprocess.run(
            ["bash", SCRIPT], input=diff, capture_output=True, text=True, env=e
        )

    def calls(self, stub_dir=None):
        d = stub_dir or self.stub_dir
        return sorted(f for f in os.listdir(d) if f.endswith(".prompt"))

    def read_call(self, name):
        with open(os.path.join(self.stub_dir, name)) as f:
            return f.read()

    def test_safe_verdict_passes(self):
        r = self.run_review(CLEAN_DIFF, "safe")
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertIn("VERDICT: SAFE", r.stdout)

    def test_unsafe_verdict_blocks_and_prints_findings(self):
        r = self.run_review(CLEAN_DIFF, "unsafe")
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("hardcoded credential in config.rs", r.stdout + r.stderr)

    def test_missing_verdict_blocks(self):
        r = self.run_review(CLEAN_DIFF, "garbled")
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("verdict", (r.stdout + r.stderr).lower())

    def test_contradictory_verdicts_block(self):
        r = self.run_review(CLEAN_DIFF, "both")
        self.assertNotEqual(r.returncode, 0)

    def test_empty_model_output_blocks(self):
        r = self.run_review(CLEAN_DIFF, "empty")
        self.assertNotEqual(r.returncode, 0)

    def test_model_failure_blocks_with_message(self):
        r = self.run_review(CLEAN_DIFF, "fail")
        self.assertNotEqual(r.returncode, 0)
        out = r.stdout + r.stderr
        self.assertIn("nanna", out.lower())
        self.assertIn("connection refused", out)

    def test_missing_binary_blocks_with_message(self):
        r = self.run_review(CLEAN_DIFF, NANNA_BIN="/nonexistent/nanna-xyz")
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("not found", (r.stdout + r.stderr).lower())

    def test_empty_diff_passes_without_calling_model(self):
        r = self.run_review("", "unsafe")
        self.assertEqual(r.returncode, 0, r.stderr)
        self.assertEqual(self.calls(), [])

    def test_whitespace_only_diff_passes(self):
        r = self.run_review("\n\n", "unsafe")
        self.assertEqual(r.returncode, 0, r.stderr)

    def test_model_and_temperature_flags(self):
        self.run_review(CLEAN_DIFF, "safe")
        args = self.read_call("call0.args")
        self.assertIn("chat", args)
        self.assertIn("-m gemma4:e4b", args)
        self.assertIn("--temperature 0", args)

    def test_model_overridable(self):
        self.run_review(CLEAN_DIFF, "safe", NANNA_REVIEW_MODEL="other:1b")
        self.assertIn("-m other:1b", self.read_call("call0.args"))

    def test_prompt_contains_diff_and_criteria(self):
        self.run_review(CLEAN_DIFF, "safe")
        prompt = self.read_call("call0.prompt")
        self.assertIn("fn add(a: i32", prompt)
        for word in ("secrets", "tokens", "PII", "VERDICT: SAFE", "VERDICT: UNSAFE"):
            self.assertIn(word, prompt)

    def test_large_diff_is_chunked_deterministically(self):
        diff = ("+" + "x" * 99 + "\n") * 100
        env = {"NANNA_REVIEW_CHUNK_BYTES": "2500", "NANNA_REVIEW_MAX_CHUNKS": "10"}
        r = self.run_review(diff, "safe", **env)
        self.assertEqual(r.returncode, 0, r.stderr)
        n = len(self.calls())
        self.assertGreater(n, 1)
        joined = "".join(self.read_call(c) for c in self.calls())
        self.assertEqual(joined.count("x" * 99), 100)
        second = tempfile.mkdtemp(dir=self.tmp.name)
        r2 = self.run_review(diff, "safe", stub_dir=second, **env)
        self.assertEqual(r2.returncode, 0)
        self.assertEqual(len(self.calls(second)), n)

    def test_unsafe_in_any_chunk_blocks(self):
        diff = ("+" + "y" * 99 + "\n") * 100
        r = self.run_review(
            diff,
            "unsafe_second",
            NANNA_REVIEW_CHUNK_BYTES="2500",
            NANNA_REVIEW_MAX_CHUNKS="10",
        )
        self.assertNotEqual(r.returncode, 0)

    def test_diff_over_chunk_limit_fails_closed(self):
        diff = ("+" + "z" * 99 + "\n") * 100
        r = self.run_review(
            diff, "safe", NANNA_REVIEW_CHUNK_BYTES="1000", NANNA_REVIEW_MAX_CHUNKS="2"
        )
        self.assertNotEqual(r.returncode, 0)
        out = (r.stdout + r.stderr).lower()
        self.assertIn("too large", out)
        self.assertEqual(self.calls(), [])

    def test_prescan_blocks_aws_key_even_if_model_says_safe(self):
        diff = '+aws_key = "' + fake_aws_key() + '"\n'
        r = self.run_review(diff, "safe")
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("pre-scan", (r.stdout + r.stderr).lower())

    def test_prescan_blocks_private_key_header(self):
        r = self.run_review("+" + fake_private_key_header() + "\n", "safe")
        self.assertNotEqual(r.returncode, 0)

    def test_prescan_ignores_removed_and_context_lines(self):
        diff = "-key = " + fake_aws_key() + "\n context " + fake_aws_key() + "\n+ok\n"
        r = self.run_review(diff, "safe")
        self.assertEqual(r.returncode, 0, r.stderr)

    def test_prescan_does_not_leak_secret_in_output(self):
        diff = '+aws_key = "' + fake_aws_key() + '"\n'
        r = self.run_review(diff, "safe")
        self.assertNotIn(fake_aws_key(), r.stdout + r.stderr)

    def test_uses_per_run_tempfiles_and_cleans_up(self):
        self.run_review(CLEAN_DIFF, "safe")
        self.run_review(CLEAN_DIFF, "unsafe")
        self.assertEqual(os.listdir(self.work), [])
        with open(SCRIPT) as f:
            src = f.read()
        self.assertIn("mktemp", src)
        self.assertNotIn("/tmp/claude_review", src)


if __name__ == "__main__":
    unittest.main()
