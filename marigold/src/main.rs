#[cfg(feature = "cli")]
use anyhow::Result;
#[cfg(feature = "cli")]
use clap::{Parser, Subcommand};

#[cfg(feature = "cli")]
#[derive(Subcommand, Debug)]
enum MarigoldCommand {
    /// Run the program.
    Run {
        /// Disables optimizations to speed up compilation.
        #[arg(short, long, default_value_t = false)]
        unoptimized: bool,

        /// Path of the Marigold file to read
        file: Option<String>,
    },
    /// Install a Marigold program as an executable using Cargo.
    Install {
        /// Path of the Marigold file to read
        file: Option<String>,
    },
    /// Uninstall a Marigold program as an executable using Cargo.
    Uninstall {
        /// Path of the Marigold file to read
        file: Option<String>,
    },
    /// Clean the cache for the passed file.
    Clean {
        /// Path of the Marigold file to read
        file: Option<String>,
    },
    /// Clean all Marigold caches for this user.
    CleanAll,
    /// Statically analyze program complexity.
    Analyze {
        /// Path of the Marigold file to read
        file: Option<String>,
    },
}

#[cfg(feature = "cli")]
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Command to run
    #[command(subcommand)]
    command: Option<MarigoldCommand>,
}

#[cfg(not(feature = "cli"))]
fn main() {
    eprintln!("Marigold needs to be installed with the cli feature (`cargo install --force marigold -F cli`)");
}

#[cfg(feature = "cli")]
fn get_file_name_argument(args: &Args) -> Option<String> {
    use MarigoldCommand::*;

    match &args.command {
        Some(Run {
            unoptimized: _,
            file,
        }) => file.clone(),
        Some(Install { file }) => file.clone(),
        Some(Uninstall { file }) => file.clone(),
        Some(Clean { file }) => file.clone(),
        Some(Analyze { file }) => file.clone(),
        Some(CleanAll) => None,
        None => None,
    }
}

/// Locate the directory in which marigold should cache generated Rust projects.
///
/// Resolution order:
///
/// 1. `$MARIGOLD_CACHE_DIR` if set — opt-in override for tests, CI, sandboxes.
/// 2. The OS-appropriate cache directory provided by [`directories::ProjectDirs`]:
///    * POSIX: `$XDG_CACHE_HOME/marigold` (or `$HOME/.cache/marigold`)
///    * macOS: `$HOME/Library/Caches/computer.dominic.marigold`
///    * Windows: `%LOCALAPPDATA%\dominic\marigold\cache`
/// 3. Legacy fallback: `$HOME/.marigold`. This preserves backwards compatibility
///    on systems where `directories` cannot determine a cache location (e.g.
///    a custom `HOME` with no standard XDG layout, as seen in some sandboxes
///    and the existing test suite).
///
/// Returns an absolute path. Does *not* create the directory; callers are
/// expected to `create_dir_all` lazily so that partial-cache failure modes can
/// be detected and recovered from.
#[cfg(feature = "cli")]
fn cache_root() -> std::path::PathBuf {
    if let Ok(override_path) = std::env::var("MARIGOLD_CACHE_DIR") {
        return std::path::PathBuf::from(override_path);
    }
    if let Some(proj) = directories::ProjectDirs::from("computer", "dominic", "marigold") {
        return proj.cache_dir().to_path_buf();
    }
    home::home_dir()
        .expect("could not locate user's home directory for marigold cache")
        .join(".marigold")
}

/// Verify that a per-program cache directory still contains the files we wrote
/// last time. Returns `false` if any required file is missing (so the caller
/// should rewrite them), `true` if the cache appears complete.
///
/// This handles the "partial cache loss" failure mode flagged in issue #94:
/// users (or external tooling: backup software, disk-cleanup utilities, CI
/// cleaners) may remove individual files inside the cache without removing
/// the whole tree. Rather than crash with an opaque cargo error, we treat
/// any missing file as a cache miss and rewrite it.
#[cfg(feature = "cli")]
fn cache_is_intact(program_project_dir: &std::path::Path) -> bool {
    program_project_dir.join("Cargo.toml").exists()
        && program_project_dir.join("src").join("main.rs").exists()
}

#[cfg(feature = "cli")]
fn main() -> Result<()> {
    use MarigoldCommand::*;

    use convert_case::{Case, Casing};
    use std::io;
    use std::io::Read;
    use std::process::Command;

    const RUST_EDITION: &str = "2021";

    let args = Args::parse();

    let marigold_cache_directory = cache_root();

    let file_name_argument = get_file_name_argument(&args);

    let program_name = {
        let mut file_name = match &file_name_argument {
            Some(path) => {
                let stem = std::path::Path::new(path)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("marigold_program");
                if stem.len() > 1 {
                    stem.to_string()
                } else {
                    "marigold_program".to_string()
                }
            }
            None => "marigold_program".to_string(),
        };

        file_name = file_name.to_case(Case::Snake);
        file_name = file_name
            .strip_prefix("_")
            .map(|s| s.to_string())
            .unwrap_or(file_name);
        file_name = file_name
            .strip_suffix("_")
            .map(|s| s.to_string())
            .unwrap_or(file_name);
        file_name
    };

    let program_project_dir = marigold_cache_directory.join(&program_name);

    let command = match args.command {
        Some(ref command) => match command {
            Run {
                unoptimized: _,
                file: _,
            } => "run",
            Install { file: _ } => "install",
            Uninstall { file: _ } => std::process::exit(
                Command::new("cargo")
                    .args(["uninstall", &program_name])
                    .spawn()?
                    .wait()?
                    .code()
                    .unwrap_or(0),
            ),
            Clean { file: _ } => {
                // Tolerate an already-missing cache: clean is idempotent.
                if program_project_dir.exists() {
                    std::fs::remove_dir_all(&program_project_dir)?;
                }
                std::process::exit(0);
            }
            CleanAll => {
                if marigold_cache_directory.exists() {
                    std::fs::remove_dir_all(&marigold_cache_directory)?;
                }
                std::process::exit(0);
            }
            Analyze { file: _ } => {
                let program_contents = match &file_name_argument {
                    Some(path) => std::fs::read_to_string(path)?.trim().to_string(),
                    None => {
                        let mut stdin = String::new();
                        io::stdin().lock().read_to_string(&mut stdin)?;
                        stdin.trim().to_string()
                    }
                };
                let result = marigold_grammar::marigold_analyze(&program_contents)
                    .map_err(|e| anyhow::anyhow!("{}", e))?;
                let json = serde_json::to_string_pretty(&result)?;
                println!("{json}");
                std::process::exit(0);
            }
        },
        None => "run", // default is run.
    };

    let program_src_dir = program_project_dir.join("src");

    // Partial-cache recovery: if the per-program directory exists but is missing
    // any of the files we expect to find (Cargo.toml, src/main.rs), it was either
    // interrupted mid-write last time or has been partially deleted by an external
    // tool (backup, disk cleaner, etc.). The cheap, correct response is to drop
    // the partial tree and rebuild from scratch, because cargo would otherwise
    // surface confusing errors about an inconsistent project.
    if program_project_dir.exists() && !cache_is_intact(&program_project_dir) {
        // Best-effort: if removal fails (e.g. concurrent access), fall through and
        // let the subsequent create_dir_all / write attempts produce a real error.
        let _ = std::fs::remove_dir_all(&program_project_dir);
    }

    std::fs::create_dir_all(&program_src_dir)?;

    let program_contents = match file_name_argument {
        Some(path) => std::fs::read_to_string(&path)?.trim().to_string(),
        None => {
            let mut stdin = String::new();
            io::stdin().lock().read_to_string(&mut stdin)?;
            stdin.trim().to_string()
        }
    };

    std::fs::write(
        program_src_dir.join("main.rs"),
        format!("#[tokio::main] async fn main() {{ marigold::m!({program_contents}).await }}")
            .as_str(),
    )?;

    const MARIGOLD_VERSION: &str = env!("CARGO_PKG_VERSION");

    let manifest_path = program_project_dir.join("Cargo.toml");

    let marigold_dep = if let Ok(workspace_path) = std::env::var("MARIGOLD_WORKSPACE_PATH") {
        format!(r#"marigold = {{ path = "{workspace_path}", features = ["tokio", "io"]}}"#)
    } else {
        format!(r#"marigold = {{ version = "={MARIGOLD_VERSION}", features = ["tokio", "io"]}}"#)
    };

    std::fs::write(
        &manifest_path,
        format!(
            r#"[package]
name = "{program_name}"
edition = "{RUST_EDITION}"
version = "0.0.1"

[dependencies]
serde = "1"
tokio = {{ version = "1", features = ["full"]}}
{marigold_dep}
        "#
        ),
    )?;

    let exit_status = {
        if command == "run" {
            let unoptimized = {
                if let Some(Run {
                    unoptimized,
                    file: _,
                }) = &args.command
                {
                    *unoptimized
                } else {
                    false
                }
            };
            if unoptimized {
                Command::new("cargo")
                    .args([
                        command,
                        "--manifest-path",
                        manifest_path
                            .to_str()
                            .expect("Marigold could not parse cache manifest path as utf-8"),
                    ])
                    .spawn()?
                    .wait()?
            } else {
                Command::new("cargo")
                    .args([
                        command,
                        "--release",
                        "--manifest-path",
                        manifest_path
                            .to_str()
                            .expect("Marigold could not parse cache manifest path as utf-8"),
                    ])
                    .spawn()?
                    .wait()?
            }
        } else {
            Command::new("cargo")
                .args([
                    command,
                    "--path",
                    program_project_dir
                        .to_str()
                        .expect("Marigold could not parse cache manifest path as utf-8"),
                ])
                .spawn()?
                .wait()?
        }
    };

    std::process::exit(exit_status.code().unwrap_or(0));
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::process::Command;
    use std::sync::LazyLock;

    /// Build the marigold binary exactly once via `cargo build` and return its path.
    /// Unlike `cargo install`, this does not write to `~/.cargo/bin/`
    /// and works in sandboxed environments.
    /// Note: assumes the default debug build profile (target/debug/).
    static BINARY: LazyLock<PathBuf> = LazyLock::new(|| {
        let workspace_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap();
        let status = Command::new("cargo")
            .args(["build", "--features", "cli", "-p", "marigold"])
            .current_dir(workspace_root)
            .status()
            .expect("could not build marigold");
        assert!(status.success(), "failed to build marigold binary");
        workspace_root.join("target/debug/marigold")
    });

    /// Create an isolated temp directory for a test, avoiding any writes
    /// to the working directory or user home.
    fn create_temp_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("marigold_test_{name}_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).expect("could not create temp dir");
        dir
    }

    fn marigold_workspace_path() -> PathBuf {
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf()
    }

    /// Common cache-root override directory used by every test below. We funnel
    /// all cache writes through `MARIGOLD_CACHE_DIR` so the tests are insensitive
    /// to the OS-specific defaults that `directories::ProjectDirs` selects.
    fn cache_override(tmp: &PathBuf) -> PathBuf {
        tmp.join("marigold_cache")
    }

    #[test]
    fn test_run() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("run");
        let csv_file = tmp.join("test_run.csv");

        let marigold_file = tmp.join("test_run.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", cache_override(&tmp))
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold command");
        assert!(status.success(), "marigold run failed");

        assert_eq!(
            fs::read_to_string(&csv_file).expect("could not read CSV"),
            "0\n1\n2\n"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_install_and_uninstall() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("install");
        let install_root = tmp.join("cargo_root");
        fs::create_dir_all(&install_root).expect("could not create install root");

        let csv_file = tmp.join("test_install.csv");
        let marigold_file = tmp.join("test_install.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        // Install the marigold program as a binary
        let status = Command::new(&binary)
            .args(["install", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", cache_override(&tmp))
            .env("CARGO_INSTALL_ROOT", &install_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold install");
        assert!(status.success(), "marigold install failed");

        assert!(
            !csv_file.exists(),
            "csv should not exist before running installed program"
        );

        // Run the installed binary directly from the install root
        let installed_binary = install_root.join("bin/test_install");
        let status = Command::new(&installed_binary)
            .status()
            .expect("could not run installed program");
        assert!(status.success(), "installed program failed");
        assert!(
            csv_file.exists(),
            "csv should exist after running installed program"
        );

        // Uninstall
        let status = Command::new(&binary)
            .args(["uninstall", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", cache_override(&tmp))
            .env("CARGO_INSTALL_ROOT", &install_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold uninstall");
        assert!(status.success(), "marigold uninstall failed");
        assert!(
            !installed_binary.exists(),
            "installed binary should be removed after uninstall"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_clean() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("clean");
        let csv_file = tmp.join("test_clean.csv");

        let marigold_file = tmp.join("test_clean.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let cache_dir_root = cache_override(&tmp);

        // Run first to create cache
        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        let cache_dir = cache_dir_root.join("test_clean");
        assert!(cache_dir.exists(), "cache should exist after run");

        // Clean
        let status = Command::new(&binary)
            .args(["clean", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold clean");
        assert!(status.success(), "marigold clean failed");

        assert!(
            !cache_dir.exists(),
            "cache dir should be removed after clean"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    /// `marigold clean` must be idempotent: invoking it twice (or against a
    /// program whose cache was already wiped by external tooling) should not
    /// error. Regression test for the partial-cache-loss failure mode of #94.
    #[test]
    fn test_clean_is_idempotent_when_cache_missing() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("clean_idempotent");
        let marigold_file = tmp.join("test_clean_idempotent.marigold");
        fs::write(&marigold_file, r#"range(0, 3).return"#).expect("could not write test file");

        let cache_dir_root = cache_override(&tmp);

        // Note: do NOT run first. The cache has never been populated.
        let status = Command::new(&binary)
            .args(["clean", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold clean");
        assert!(
            status.success(),
            "marigold clean on absent cache should succeed (idempotent)"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_clean_all() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("clean_all");
        let csv_file = tmp.join("test_clean_all.csv");

        let marigold_file = tmp.join("test_clean_all.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let cache_dir_root = cache_override(&tmp);

        // Run first to create cache
        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        assert!(cache_dir_root.exists(), "cache should exist after run");

        // Clean all
        let status = Command::new(&binary)
            .args(["clean-all"])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold clean-all");
        assert!(status.success(), "marigold clean-all failed");

        assert!(
            !cache_dir_root.exists(),
            "entire cache should be removed after clean-all"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    // --- Resilience tests for issue #94 (cache directory hardening) ---

    /// Serializes env-var manipulation across the tests below. `cargo test` runs
    /// tests in parallel by default, but `set_var`/`remove_var` mutate process
    /// global state — without a mutex two tests racing on `MARIGOLD_CACHE_DIR`
    /// will see each other's writes and fail intermittently.
    static ENV_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    /// `cache_root()` honors `MARIGOLD_CACHE_DIR` over both `directories` and the
    /// legacy `$HOME/.marigold` fallback. This is the contract the integration
    /// tests above depend on, so we encode it as a direct unit test here.
    #[test]
    fn test_cache_root_respects_explicit_override() {
        let _guard = ENV_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        let prior = std::env::var("MARIGOLD_CACHE_DIR").ok();
        let tmp =
            std::env::temp_dir().join(format!("marigold_root_override_{}", std::process::id()));
        // SAFETY: ENV_LOCK serializes all env-mutating tests in this module.
        unsafe {
            std::env::set_var("MARIGOLD_CACHE_DIR", &tmp);
        }
        let observed = super::cache_root();
        // Restore before asserting so a failure does not leak state to other tests.
        unsafe {
            if let Some(p) = prior {
                std::env::set_var("MARIGOLD_CACHE_DIR", p);
            } else {
                std::env::remove_var("MARIGOLD_CACHE_DIR");
            }
        }
        assert_eq!(observed, tmp);
    }

    /// `cache_root()` returns an OS-cache-shaped path when no override is set.
    /// We don't pin the exact location (it varies per OS) but we do assert it
    /// is absolute and mentions `marigold` somewhere in its tail.
    #[test]
    fn test_cache_root_defaults_to_os_cache() {
        let _guard = ENV_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        let prior = std::env::var("MARIGOLD_CACHE_DIR").ok();
        // SAFETY: ENV_LOCK serializes all env-mutating tests in this module.
        unsafe {
            std::env::remove_var("MARIGOLD_CACHE_DIR");
        }
        let root = super::cache_root();
        unsafe {
            if let Some(p) = prior {
                std::env::set_var("MARIGOLD_CACHE_DIR", p);
            }
        }
        assert!(
            root.is_absolute(),
            "default cache root should be absolute: {root:?}"
        );
        let as_str = root.display().to_string();
        assert!(
            as_str.contains("marigold") || as_str.ends_with(".marigold"),
            "default cache root should mention 'marigold': {as_str}"
        );
    }

    /// `cache_is_intact()` reports `false` when either of the two files we
    /// require (Cargo.toml, src/main.rs) is missing, and `true` only when both
    /// are present. This is the predicate that drives partial-cache recovery.
    #[test]
    fn test_cache_is_intact_detects_missing_files() {
        let tmp = create_temp_dir("intact");
        let proj = tmp.join("program");
        let src = proj.join("src");
        fs::create_dir_all(&src).unwrap();

        // Empty directory: not intact.
        assert!(!super::cache_is_intact(&proj));

        // Only Cargo.toml: not intact.
        fs::write(proj.join("Cargo.toml"), "").unwrap();
        assert!(!super::cache_is_intact(&proj));

        // Both files: intact.
        fs::write(src.join("main.rs"), "fn main(){}").unwrap();
        assert!(super::cache_is_intact(&proj));

        // Removing src/main.rs again breaks the check.
        fs::remove_file(src.join("main.rs")).unwrap();
        assert!(!super::cache_is_intact(&proj));

        let _ = fs::remove_dir_all(&tmp);
    }

    /// Full end-to-end resilience test: corrupt the cache mid-flight by deleting
    /// `src/main.rs` after a successful first run, then re-run. `marigold run`
    /// must rebuild the missing file rather than crash. This exercises the
    /// recovery path added for issue #94.
    #[test]
    fn test_run_recovers_from_partial_cache_loss() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("partial_cache_loss");
        let csv_file = tmp.join("test_partial.csv");

        let marigold_file = tmp.join("test_partial.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let cache_dir_root = cache_override(&tmp);

        // First run populates cache.
        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "initial run failed");

        // Simulate external interference: nuke src/main.rs only.
        let program_main = cache_dir_root
            .join("test_partial")
            .join("src")
            .join("main.rs");
        assert!(
            program_main.exists(),
            "expected main.rs to exist at {program_main:?}"
        );
        fs::remove_file(&program_main).expect("could not delete main.rs");

        // Remove the previous output so we can detect a successful rebuild.
        let _ = fs::remove_file(&csv_file);

        // Second run must recover and produce the output again.
        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not re-run marigold");
        assert!(
            status.success(),
            "marigold should recover from partial cache loss"
        );
        assert!(
            csv_file.exists(),
            "expected output to be regenerated after partial cache loss"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    /// Cache loss "midflight" simulation: wipe the entire per-program cache
    /// between runs. The second invocation must rebuild from scratch without
    /// any user intervention.
    #[test]
    fn test_run_recovers_from_full_cache_loss() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("full_cache_loss");
        let csv_file = tmp.join("test_full.csv");

        let marigold_file = tmp.join("test_full.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let cache_dir_root = cache_override(&tmp);

        // First run populates cache.
        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "initial run failed");

        // Delete the per-program cache tree entirely.
        fs::remove_dir_all(cache_dir_root.join("test_full"))
            .expect("could not delete per-program cache");
        let _ = fs::remove_file(&csv_file);

        // Second run rebuilds from nothing.
        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_CACHE_DIR", &cache_dir_root)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not re-run marigold");
        assert!(
            status.success(),
            "marigold should recover from full cache loss"
        );
        assert!(
            csv_file.exists(),
            "expected output to be regenerated after full cache loss"
        );

        let _ = fs::remove_dir_all(&tmp);
    }
}
