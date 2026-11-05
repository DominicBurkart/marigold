#![cfg(feature = "cli")]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

struct Sandbox {
    root: PathBuf,
    home: PathBuf,
}

impl Sandbox {
    fn new(name: &str) -> Self {
        let root =
            std::env::temp_dir().join(format!("marigold-run-cli-{name}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        let home = root.join("home");
        std::fs::create_dir_all(&home).unwrap();
        Sandbox { root, home }
    }

    fn write(&self, name: &str, contents: &str) -> PathBuf {
        let path = self.root.join(name);
        std::fs::write(&path, contents).unwrap();
        path
    }

    fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }
}

impl Drop for Sandbox {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.root);
    }
}

fn real_home() -> PathBuf {
    home_from_env().expect("HOME must be set to locate the cargo and rustup homes")
}

fn home_from_env() -> Option<PathBuf> {
    std::env::var_os("HOME").map(PathBuf::from)
}

fn marigold_run(sandbox: &Sandbox, program: &Path) -> Output {
    let cargo_home = std::env::var_os("CARGO_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|| real_home().join(".cargo"));
    let rustup_home = std::env::var_os("RUSTUP_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|| real_home().join(".rustup"));
    let shared_target = Path::new(env!("CARGO_TARGET_TMPDIR")).join("run-cli-target");
    Command::new(env!("CARGO_BIN_EXE_marigold"))
        .arg("run")
        .arg("--unoptimized")
        .arg(program)
        .env("HOME", &sandbox.home)
        .env("CARGO_HOME", cargo_home)
        .env("RUSTUP_HOME", rustup_home)
        .env("CARGO_TARGET_DIR", shared_target)
        .env("MARIGOLD_WORKSPACE_PATH", env!("CARGO_MANIFEST_DIR"))
        .output()
        .unwrap()
}

fn stdout(out: &Output) -> String {
    String::from_utf8_lossy(&out.stdout).into_owned()
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

#[test]
fn run_handles_returning_writing_and_mixed_programs() {
    let sandbox = Sandbox::new("shapes");

    let returning = sandbox.write(
        "returning_program.marigold",
        "fn double(x: i32) -> i32 { x * 2 }\nrange(0, 4).map(double).return",
    );
    let out = marigold_run(&sandbox, &returning);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(stdout(&out), "0\n2\n4\n6\n");

    let written = sandbox.path("written.csv");
    let writing = sandbox.write(
        "writing_program.marigold",
        &format!("range(0, 3).write_file(\"{}\", csv)", written.display()),
    );
    let out = marigold_run(&sandbox, &writing);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(stdout(&out), "");
    assert_eq!(std::fs::read_to_string(&written).unwrap(), "0\n1\n2\n");

    let mixed_written = sandbox.path("mixed.csv");
    let mixed = sandbox.write(
        "mixed_program.marigold",
        &format!(
            "range(0, 3).return\nrange(10, 12).write_file(\"{}\", csv)",
            mixed_written.display()
        ),
    );
    let out = marigold_run(&sandbox, &mixed);
    assert!(out.status.success(), "{}", stderr(&out));
    assert_eq!(stdout(&out), "0\n1\n2\n");
    assert_eq!(std::fs::read_to_string(&mixed_written).unwrap(), "10\n11\n");
}
