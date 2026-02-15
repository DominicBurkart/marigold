use marigold_grammar::complexity::{ComplexityClass, ProgramComplexity};
use std::process::Command;
use tempfile::TempDir;

fn git(dir: &std::path::Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .args(args)
        .current_dir(dir)
        .output()
        .expect("failed to run git");
    if !output.status.success() {
        panic!(
            "git {:?} failed: {}",
            args,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn commit_hash(dir: &std::path::Path) -> String {
    git(dir, &["rev-parse", "HEAD"])
}

fn write_and_commit(dir: &std::path::Path, content: &str, message: &str) -> String {
    std::fs::write(dir.join("program.marigold"), content).unwrap();
    git(dir, &["add", "program.marigold"]);
    git(dir, &["commit", "-m", message]);
    commit_hash(dir)
}

fn analyze_at_commit(dir: &std::path::Path, hash: &str) -> ProgramComplexity {
    git(dir, &["checkout", hash]);
    let source = std::fs::read_to_string(dir.join("program.marigold")).unwrap();
    marigold_grammar::marigold_analyze(&source).unwrap()
}

#[test]
fn test_git_bisect_detects_memory_regression() {
    let tmp = TempDir::new().unwrap();
    let dir = tmp.path();

    git(dir, &["init"]);
    git(dir, &["config", "user.email", "test@test.com"]);
    git(dir, &["config", "user.name", "Test"]);
    git(dir, &["config", "commit.gpgsign", "false"]);

    let commit_a = write_and_commit(
        dir,
        "range(0, 100).map(double).return",
        "good: streaming map",
    );
    let _commit_b = write_and_commit(
        dir,
        "range(0, 100).filter(is_odd).return",
        "good: streaming filter",
    );
    let commit_c = write_and_commit(
        dir,
        "range(0, 100).permutations(3).return",
        "bad: introduced permutations",
    );
    let commit_d = write_and_commit(
        dir,
        "range(0, 100).permutations(3).map(identity).return",
        "bad: still has permutations",
    );

    let analysis_a = analyze_at_commit(dir, &commit_a);
    assert_eq!(analysis_a.program_space, ComplexityClass::O1);

    let analysis_c = analyze_at_commit(dir, &commit_c);
    assert_eq!(analysis_c.program_space, ComplexityClass::ON);

    let analysis_d = analyze_at_commit(dir, &commit_d);
    assert_eq!(analysis_d.program_space, ComplexityClass::ON);

    git(dir, &["checkout", &commit_d]);

    let script = "#!/bin/bash\n! grep -q 'permutations' program.marigold\n";
    let script_path = dir.join("bisect_check.sh");
    std::fs::write(&script_path, script).unwrap();

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&script_path, std::fs::Permissions::from_mode(0o755)).unwrap();
    }

    git(dir, &["bisect", "start", &commit_d, &commit_a]);
    let bisect_output = git(dir, &["bisect", "run", "./bisect_check.sh"]);
    git(dir, &["bisect", "reset"]);

    assert!(
        bisect_output.contains(&commit_c[..8]),
        "Expected bisect to find commit {commit_c}, but got: {bisect_output}"
    );
}
