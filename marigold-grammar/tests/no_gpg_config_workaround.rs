//! Regression guard for issue #68 — ensures no source file in the workspace
//! reintroduces the per-repo `commit.gpgsign=false` workaround.
//!
//! The original issue noted that test helpers and shell scripts had to set
//! `git config commit.gpgsign false` on temporary repos so commits would not
//! fail when the developer's global git config insisted on signing. We now
//! pass `--no-gpg-sign` directly to `git commit` instead, which is per-call
//! and requires zero state mutation. This test fails if anyone reintroduces
//! the config workaround in:
//!   - `marigold-grammar/tests/bisect_cardinality.rs`
//!   - `marigold-grammar/tests/bisect_complexity.rs`
//!
//! It runs in `cargo test -p marigold-grammar`, the same workflow that runs
//! the bisect tests, so any reintroduction is caught immediately.

use std::path::PathBuf;

fn workspace_path(rel: &str) -> PathBuf {
    // CARGO_MANIFEST_DIR points at marigold-grammar/. Tests already use this
    // pattern (see e2e_cardinality.rs which reads ./tests/programs/*.marigold).
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR should be set when running cargo test");
    PathBuf::from(manifest_dir).join(rel)
}

/// The workaround string we want to keep out of bisect helpers (issue #68).
/// We assemble it from fragments to avoid the substring trivially appearing
/// in this regression test's own source — otherwise scanners that include
/// this file would always match. Other files do not need such evasion.
fn forbidden_config_key() -> String {
    format!("{}.{}", "commit", "gpgsign")
}

fn assert_no_gpgsign_config_workaround(rel_path: &str) {
    let path = workspace_path(rel_path);
    let contents = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Could not read {}: {e}", path.display()));

    let needle = forbidden_config_key();
    let has_config_workaround = contents.contains(&needle);

    assert!(
        !has_config_workaround,
        "Regression: {rel_path} reintroduced a `{needle}` config workaround \
         (see issue #68). Use `git commit --no-gpg-sign ...` instead so test \
         repos remain free of any global GPG state.",
    );
}

#[test]
fn bisect_cardinality_does_not_use_gpgsign_config_workaround() {
    assert_no_gpgsign_config_workaround("tests/bisect_cardinality.rs");
}

#[test]
fn bisect_complexity_does_not_use_gpgsign_config_workaround() {
    assert_no_gpgsign_config_workaround("tests/bisect_complexity.rs");
}

#[test]
fn bisect_helpers_pass_no_gpg_sign_flag() {
    // Sanity check the *positive* form — both files must opt out of signing
    // via the per-call flag. If a future refactor removes the flag without
    // also reintroducing the config workaround, this test forces the author
    // to think about it.
    for rel in ["tests/bisect_cardinality.rs", "tests/bisect_complexity.rs"] {
        let contents = std::fs::read_to_string(workspace_path(rel))
            .unwrap_or_else(|e| panic!("Could not read {rel}: {e}"));
        assert!(
            contents.contains("--no-gpg-sign"),
            "{rel} must invoke `git commit --no-gpg-sign` (issue #68); use \
             the per-call flag instead of any per-repo gpg config tweak."
        );
    }
}
