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

/// Resolves the directory used to cache generated Cargo projects.
///
/// The cache holds ephemeral, regenerable build artifacts, so it lives in the
/// OS-conventional cache location rather than the user's home directory.
///
/// Resolution order:
/// - If the `MARIGOLD_CACHE_DIR` environment variable is set, its value is
///   returned verbatim. This is the supported override for power users and is
///   also how the test suite isolates runs.
/// - On Linux/BSD: `$XDG_CACHE_HOME/marigold` when `XDG_CACHE_HOME` is set and
///   absolute, otherwise `~/.cache/marigold`.
/// - On macOS: `~/Library/Caches/marigold`.
/// - On Windows: `%LOCALAPPDATA%\marigold\cache`, falling back to
///   `~\AppData\Local\marigold\cache` when `LOCALAPPDATA` is unset.
///
/// Returns an error (rather than panicking) when no directory can be resolved.
#[cfg(feature = "cli")]
fn resolve_cache_dir() -> anyhow::Result<std::path::PathBuf> {
    use std::path::PathBuf;

    if let Some(dir) = std::env::var_os("MARIGOLD_CACHE_DIR") {
        return Ok(PathBuf::from(dir));
    }

    #[cfg(not(any(target_os = "windows", target_os = "macos")))]
    {
        let base = match std::env::var_os("XDG_CACHE_HOME") {
            Some(value) if std::path::Path::new(&value).is_absolute() => PathBuf::from(value),
            _ => home::home_dir()
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "could not resolve marigold cache directory: \
                         neither XDG_CACHE_HOME nor a home directory is available"
                    )
                })?
                .join(".cache"),
        };
        Ok(base.join("marigold"))
    }

    #[cfg(target_os = "macos")]
    {
        let home = home::home_dir().ok_or_else(|| {
            anyhow::anyhow!(
                "could not resolve marigold cache directory: \
                 user's home directory is not available"
            )
        })?;
        Ok(home.join("Library").join("Caches").join("marigold"))
    }

    #[cfg(target_os = "windows")]
    {
        let base = match std::env::var_os("LOCALAPPDATA") {
            Some(value) => PathBuf::from(value),
            None => home::home_dir()
                .ok_or_else(|| {
                    anyhow::anyhow!(
                        "could not resolve marigold cache directory: \
                         neither LOCALAPPDATA nor a home directory is available"
                    )
                })?
                .join("AppData")
                .join("Local"),
        };
        Ok(base.join("marigold").join("cache"))
    }
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

#[cfg(feature = "cli")]
fn main() -> Result<()> {
    use MarigoldCommand::*;

    use convert_case::{Case, Casing};
    use std::io;
    use std::io::Read;
    use std::process::Command;

    const RUST_EDITION: &str = "2021";

    let args = Args::parse();

    let marigold_cache_directory = resolve_cache_dir()?;

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
                // Treat an already-absent cache dir as success so that
                // `clean` is idempotent and resilient to prior cache loss.
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

    // Recreate the source directory on every run. `create_dir_all` is a no-op
    // when the directory already exists, so this self-heals after a full cache
    // loss (the entire cache dir was deleted) or a partial loss (the project's
    // `src/` directory was removed) without erroring.
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

    #[test]
    fn test_run() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("run");
        let cache_dir = tmp.join("cache");
        let csv_file = tmp.join("test_run.csv");

        let marigold_file = tmp.join("test_run.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
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
        let cache_dir = tmp.join("cache");
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
        let status = Command::new(binary)
            .args(["install", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
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
        let status = Command::new(binary)
            .args(["uninstall", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
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
        let cache_dir = tmp.join("cache");
        let csv_file = tmp.join("test_clean.csv");

        let marigold_file = tmp.join("test_clean.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        // Run first to create cache
        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        let program_dir = cache_dir.join("test_clean");
        assert!(program_dir.exists(), "cache should exist after run");

        // Clean
        let status = Command::new(binary)
            .args(["clean", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold clean");
        assert!(status.success(), "marigold clean failed");

        assert!(
            !program_dir.exists(),
            "cache dir should be removed after clean"
        );

        // `clean` is idempotent: cleaning an already-absent project succeeds.
        let status = Command::new(binary)
            .args(["clean", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold clean");
        assert!(
            status.success(),
            "marigold clean should succeed on an absent cache"
        );

        // A `run` after `clean` recreates the cache cleanly.
        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run after clean failed");
        assert!(
            program_dir.exists(),
            "cache should be recreated after run following clean"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_clean_all() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("clean_all");
        let cache_dir = tmp.join("cache");
        let csv_file = tmp.join("test_clean_all.csv");

        let marigold_file = tmp.join("test_clean_all.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        // Run first to create cache
        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        assert!(cache_dir.exists(), "cache should exist after run");

        // Clean all
        let status = Command::new(binary)
            .args(["clean-all"])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold clean-all");
        assert!(status.success(), "marigold clean-all failed");

        assert!(
            !cache_dir.exists(),
            "the resolved marigold cache dir should be removed after clean-all"
        );
        // `clean-all` removes only the resolved cache dir, not its parent.
        assert!(
            tmp.exists(),
            "clean-all must not remove the parent of the cache dir"
        );

        // A `run` after `clean-all` recreates the cache cleanly.
        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run after clean-all failed");
        assert!(
            cache_dir.join("test_clean_all").exists(),
            "cache should be recreated after run following clean-all"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_cache_dir_env_override() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("cache_override");
        // The cache dir is nested so we can prove nothing is written outside it.
        let cache_dir = tmp.join("custom").join("cache");
        let csv_file = tmp.join("test_cache_override.csv");

        let marigold_file = tmp.join("test_cache_override.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("MARIGOLD_CACHE_DIR", &cache_dir)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold command");
        assert!(status.success(), "marigold run failed");

        let program_dir = cache_dir.join("test_cache_override");
        assert!(
            program_dir.join("Cargo.toml").exists(),
            "generated project should live under MARIGOLD_CACHE_DIR"
        );
        assert!(
            program_dir.join("src").join("main.rs").exists(),
            "generated source should live under MARIGOLD_CACHE_DIR"
        );
        // Nothing should be written to a `.marigold` dir or anywhere else.
        assert!(
            !tmp.join(".marigold").exists(),
            "marigold must not fall back to a ~/.marigold directory"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_cache_recreated_after_full_loss() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("full_loss");
        let cache_dir = tmp.join("cache");
        let csv_file = tmp.join("test_full_loss.csv");

        let marigold_file = tmp.join("test_full_loss.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let run = || {
            Command::new(binary)
                .args(["run", marigold_file.to_str().unwrap()])
                .env("MARIGOLD_CACHE_DIR", &cache_dir)
                .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
                .status()
                .expect("could not run marigold command")
        };

        // First run populates the cache.
        assert!(run().success(), "initial marigold run failed");
        assert!(cache_dir.exists(), "cache should exist after first run");

        // Simulate full cache loss (e.g. OS cleared the temp/cache dir).
        fs::remove_dir_all(&cache_dir).expect("could not remove cache dir");
        assert!(!cache_dir.exists(), "cache dir should be gone");

        // The next run must transparently recreate the cache.
        assert!(run().success(), "marigold run after full cache loss failed");
        assert!(
            cache_dir.join("test_full_loss").join("Cargo.toml").exists(),
            "cache should be recreated after full loss"
        );
        assert_eq!(
            fs::read_to_string(&csv_file).expect("could not read CSV"),
            "0\n1\n2\n"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    #[test]
    fn test_run_recovers_from_partial_cache_loss() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("partial_loss");
        let cache_dir = tmp.join("cache");
        let csv_file = tmp.join("test_partial_loss.csv");

        let marigold_file = tmp.join("test_partial_loss.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let run = || {
            Command::new(binary)
                .args(["run", marigold_file.to_str().unwrap()])
                .env("MARIGOLD_CACHE_DIR", &cache_dir)
                .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
                .status()
                .expect("could not run marigold command")
        };

        // First run populates the cache.
        assert!(run().success(), "initial marigold run failed");

        let program_dir = cache_dir.join("test_partial_loss");
        let generated_main = program_dir.join("src").join("main.rs");
        let manifest = program_dir.join("Cargo.toml");

        // Partial loss: the generated source file is deleted.
        fs::remove_file(&generated_main).expect("could not remove generated main.rs");
        assert!(!generated_main.exists());
        assert!(
            run().success(),
            "marigold run after losing src/main.rs failed"
        );
        assert!(generated_main.exists(), "src/main.rs should be regenerated");

        // Partial loss: the whole src/ directory is deleted.
        fs::remove_dir_all(program_dir.join("src")).expect("could not remove src dir");
        assert!(run().success(), "marigold run after losing src/ dir failed");
        assert!(generated_main.exists(), "src/ should be regenerated");

        // Partial loss: the manifest is deleted.
        fs::remove_file(&manifest).expect("could not remove Cargo.toml");
        assert!(!manifest.exists());
        assert!(
            run().success(),
            "marigold run after losing Cargo.toml failed"
        );
        assert!(manifest.exists(), "Cargo.toml should be regenerated");
        assert_eq!(
            fs::read_to_string(&csv_file).expect("could not read CSV"),
            "0\n1\n2\n"
        );

        let _ = fs::remove_dir_all(&tmp);
    }

    /// On Linux/BSD, with no override set, the cache must land under
    /// `$XDG_CACHE_HOME/marigold`.
    #[cfg(not(any(target_os = "windows", target_os = "macos")))]
    #[test]
    fn test_default_cache_dir_uses_xdg() {
        let binary = &*BINARY;
        let tmp = create_temp_dir("xdg");
        let xdg_cache_home = tmp.join("xdg_cache");
        let csv_file = tmp.join("test_xdg.csv");

        let marigold_file = tmp.join("test_xdg.marigold");
        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env_remove("MARIGOLD_CACHE_DIR")
            .env("XDG_CACHE_HOME", &xdg_cache_home)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold command");
        assert!(status.success(), "marigold run failed");

        let expected = xdg_cache_home.join("marigold").join("test_xdg");
        assert!(
            expected.join("Cargo.toml").exists(),
            "generated project should live under $XDG_CACHE_HOME/marigold"
        );

        let _ = fs::remove_dir_all(&tmp);
    }
}
