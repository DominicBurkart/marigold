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

#[cfg(feature = "cli")]
fn cache_root() -> Result<std::path::PathBuf> {
    dirs::cache_dir()
        .ok_or_else(|| anyhow::anyhow!("could not determine OS cache directory"))
        .map(|d| d.join("marigold"))
}

#[cfg(feature = "cli")]
fn prepare_cache(
    cache_root: &std::path::Path,
    program_name: &str,
    program_contents: &str,
    marigold_version: &str,
    workspace_path: Option<&str>,
) -> Result<std::path::PathBuf> {
    const RUST_EDITION: &str = "2021";

    // Reject program contents that would escape the marigold::m!(...) invocation.
    // The generated main.rs wraps contents as: marigold::m!({program_contents}).await
    // A `})` sequence closes the macro call and ends the async block, allowing
    // arbitrary Rust injection. Reject such input early.
    if program_contents.contains("}") && program_contents.contains(")") {
        // More precise: reject only if `})` appears as a substring.
        if program_contents.contains("})") {
            anyhow::bail!(
                "program contents contain '{}{}' which would escape the macro invocation",
                '}',
                ')'
            );
        }
    }

    let program_project_dir = cache_root.join(program_name);
    let program_src_dir = program_project_dir.join("src");

    std::fs::create_dir_all(&program_src_dir)?;

    std::fs::write(
        program_src_dir.join("main.rs"),
        format!("#[tokio::main] async fn main() {{ marigold::m!({program_contents}).await }}")
            .as_str(),
    )?;

    let manifest_path = program_project_dir.join("Cargo.toml");

    let marigold_dep = if let Some(path) = workspace_path {
        format!("marigold = {{ path = \"{path}\", features = [\"tokio\", \"io\"]}}\n")
    } else {
        format!(
            "marigold = {{ version = \"={marigold_version}\", features = [\"tokio\", \"io\"]}}\n"
        )
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

    Ok(manifest_path)
}

#[cfg(feature = "cli")]
fn clean_program_cache(cache_root: &std::path::Path, program_name: &str) -> Result<()> {
    let program_project_dir = cache_root.join(program_name);
    if program_project_dir.exists() {
        std::fs::remove_dir_all(&program_project_dir)?;
    }
    Ok(())
}

#[cfg(feature = "cli")]
fn clean_all_cache(cache_root: &std::path::Path) -> Result<()> {
    if cache_root.exists() {
        std::fs::remove_dir_all(cache_root)?;
    }
    Ok(())
}

#[cfg(feature = "cli")]
enum CargoInvocation {
    Run { release: bool },
    Install,
}

/// Invoke `cargo run` (with `--manifest-path`) or `cargo install` (with `--path`).
#[cfg(feature = "cli")]
fn invoke_cargo(
    invocation: CargoInvocation,
    path: &std::path::Path,
) -> Result<std::process::ExitStatus> {
    use std::process::Command;

    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("Marigold could not parse cache path as utf-8"))?;

    let status = match invocation {
        CargoInvocation::Run { release } => {
            if release {
                Command::new("cargo")
                    .args(["run", "--release", "--manifest-path", path_str])
                    .spawn()?
                    .wait()?
            } else {
                Command::new("cargo")
                    .args(["run", "--manifest-path", path_str])
                    .spawn()?
                    .wait()?
            }
        }
        CargoInvocation::Install => Command::new("cargo")
            .args(["install", "--path", path_str])
            .spawn()?
            .wait()?,
    };

    Ok(status)
}

#[cfg(feature = "cli")]
fn main() -> Result<()> {
    use MarigoldCommand::*;

    use convert_case::{Case, Casing};
    use std::io;
    use std::io::Read;
    use std::process::Command;

    let args = Args::parse();

    let marigold_cache_directory = cache_root()?;

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

    // Validate program_name against Cargo package name rules: ^[a-z][a-z0-9_]*$
    if !program_name.starts_with(|c: char| c.is_ascii_lowercase())
        || !program_name
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
    {
        anyhow::bail!(
            "invalid program name '{}': must start with a lowercase letter and contain only [a-z0-9_]",
            program_name
        );
    }

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
                clean_program_cache(&marigold_cache_directory, &program_name)?;
                std::process::exit(0);
            }
            CleanAll => {
                clean_all_cache(&marigold_cache_directory)?;
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
        None => "run",
    };

    let program_contents = match file_name_argument {
        Some(path) => std::fs::read_to_string(&path)?.trim().to_string(),
        None => {
            let mut stdin = String::new();
            io::stdin().lock().read_to_string(&mut stdin)?;
            stdin.trim().to_string()
        }
    };

    const MARIGOLD_VERSION: &str = env!("CARGO_PKG_VERSION");

    let workspace_path = std::env::var("MARIGOLD_WORKSPACE_PATH").ok();
    let manifest_path = prepare_cache(
        &marigold_cache_directory,
        &program_name,
        &program_contents,
        MARIGOLD_VERSION,
        workspace_path.as_deref(),
    )?;

    let unoptimized = matches!(
        &args.command,
        Some(Run {
            unoptimized: true,
            file: _,
        })
    );

    let exit_status = if command == "run" {
        invoke_cargo(
            CargoInvocation::Run {
                release: !unoptimized,
            },
            &manifest_path,
        )?
    } else {
        invoke_cargo(
            CargoInvocation::Install,
            &marigold_cache_directory.join(&program_name),
        )?
    };

    std::process::exit(exit_status.code().unwrap_or(0));
}

#[cfg(all(test, feature = "cli"))]
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

        // Run first to create cache
        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("XDG_CACHE_HOME", tmp.join(".cache"))
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        // Derive the expected cache path from dirs::cache_dir() so the assertion
        // is correct on both Linux ($XDG_CACHE_HOME) and macOS ($HOME/Library/Caches).
        // The test process inherits the same env vars as the subprocess, so
        // dirs::cache_dir() resolves to the same root here.
        let cache_dir = dirs::cache_dir()
            .expect("dirs::cache_dir() must be available in test env")
            .join("marigold/test_clean");
        assert!(cache_dir.exists(), "cache should exist after run");

        // Clean
        let status = Command::new(&binary)
            .args(["clean", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("XDG_CACHE_HOME", tmp.join(".cache"))
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

        // Run first to create cache
        let status = Command::new(&binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("XDG_CACHE_HOME", tmp.join(".cache"))
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        // Derive the expected cache root from dirs::cache_dir() so the assertion
        // is correct on both Linux ($XDG_CACHE_HOME) and macOS ($HOME/Library/Caches).
        let cache_root = dirs::cache_dir()
            .expect("dirs::cache_dir() must be available in test env")
            .join("marigold");
        assert!(cache_root.exists(), "cache should exist after run");

        // Clean all
        let status = Command::new(&binary)
            .args(["clean-all"])
            .env("HOME", &tmp)
            .env("XDG_CACHE_HOME", tmp.join(".cache"))
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold clean-all");
        assert!(status.success(), "marigold clean-all failed");

        assert!(
            !cache_root.exists(),
            "entire cache should be removed after clean-all"
        );

        let _ = fs::remove_dir_all(&tmp);
    }
}

#[cfg(all(test, feature = "cli"))]
mod cache_tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_prepare_cache_creates_files() {
        let tmp = tempfile::tempdir().unwrap();
        let manifest = prepare_cache(tmp.path(), "my_prog", "range(0, 1).return", "0.1.0", None)
            .expect("prepare_cache failed");
        assert!(tmp.path().join("my_prog/src/main.rs").exists());
        assert!(manifest.exists());
        let cargo_toml = fs::read_to_string(&manifest).unwrap();
        assert!(cargo_toml.contains("my_prog"));
        assert!(cargo_toml.contains("marigold"));
    }

    #[test]
    fn test_prepare_cache_overwrites_existing() {
        let tmp = tempfile::tempdir().unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None).unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 2).return", "0.1.0", None).unwrap();
        let main_rs = fs::read_to_string(tmp.path().join("prog/src/main.rs")).unwrap();
        assert!(main_rs.contains("range(0, 2).return"));
    }

    #[test]
    fn test_clean_nonexistent_program() {
        let tmp = tempfile::tempdir().unwrap();
        clean_program_cache(tmp.path(), "never_existed").expect("should succeed even if missing");
    }

    #[test]
    fn test_clean_existing_program() {
        let tmp = tempfile::tempdir().unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None).unwrap();
        assert!(tmp.path().join("prog").exists());
        clean_program_cache(tmp.path(), "prog").unwrap();
        assert!(!tmp.path().join("prog").exists());
    }

    #[test]
    fn test_clean_all_empty() {
        let tmp = tempfile::tempdir().unwrap();
        // Take ownership of the path so TempDir::drop does not attempt to
        // remove_dir_all a path that clean_all_cache already removed.
        let path = tmp.into_path();
        clean_all_cache(&path).expect("should succeed on empty dir");
        assert!(!path.exists());
    }

    #[test]
    fn test_clean_all_with_programs() {
        let tmp = tempfile::tempdir().unwrap();
        prepare_cache(tmp.path(), "prog_a", "range(0, 1).return", "0.1.0", None).unwrap();
        prepare_cache(tmp.path(), "prog_b", "range(0, 2).return", "0.1.0", None).unwrap();
        clean_all_cache(tmp.path()).unwrap();
        assert!(!tmp.path().exists());
    }

    #[test]
    fn test_cache_root_returns_os_path() {
        let root = cache_root().expect("cache_root failed");
        assert_eq!(root.file_name().unwrap(), "marigold");
        assert_eq!(root.parent().unwrap(), dirs::cache_dir().unwrap());
    }

    #[cfg(unix)]
    #[test]
    fn test_prepare_cache_readonly_parent() {
        use std::os::unix::fs::PermissionsExt;
        // Root can write to read-only directories, so skip this test when running as root.
        let is_root = std::process::Command::new("id")
            .arg("-u")
            .output()
            .ok()
            .and_then(|o| String::from_utf8(o.stdout).ok())
            .map(|s| s.trim() == "0")
            .unwrap_or(false);
        if is_root {
            return;
        }
        let tmp = tempfile::tempdir().unwrap();
        fs::set_permissions(tmp.path(), fs::Permissions::from_mode(0o444)).unwrap();
        let result = prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None);
        fs::set_permissions(tmp.path(), fs::Permissions::from_mode(0o755)).unwrap();
        assert!(result.is_err(), "expected error writing into read-only dir");
    }

    #[test]
    fn test_partial_loss_main_rs_deleted() {
        let tmp = tempfile::tempdir().unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None).unwrap();
        fs::remove_file(tmp.path().join("prog/src/main.rs")).unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None)
            .expect("should recreate missing main.rs");
        assert!(tmp.path().join("prog/src/main.rs").exists());
    }

    #[test]
    fn test_partial_loss_cargo_toml_deleted() {
        let tmp = tempfile::tempdir().unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None).unwrap();
        fs::remove_file(tmp.path().join("prog/Cargo.toml")).unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None)
            .expect("should recreate missing Cargo.toml");
        assert!(tmp.path().join("prog/Cargo.toml").exists());
    }

    #[test]
    fn test_partial_loss_src_dir_deleted() {
        let tmp = tempfile::tempdir().unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None).unwrap();
        fs::remove_dir_all(tmp.path().join("prog/src")).unwrap();
        prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None)
            .expect("should recreate missing src dir");
        assert!(tmp.path().join("prog/src/main.rs").exists());
    }

    #[test]
    fn test_cache_disappears_after_prepare() {
        let tmp = tempfile::tempdir().unwrap();
        let manifest =
            prepare_cache(tmp.path(), "prog", "range(0, 1).return", "0.1.0", None).unwrap();
        fs::remove_dir_all(tmp.path().join("prog")).unwrap();
        // cargo spawns successfully but exits non-zero because the manifest is missing;
        // invoke_cargo returns Ok(failing_status), not Err.
        let status = invoke_cargo(CargoInvocation::Run { release: false }, &manifest)
            .expect("cargo should spawn successfully");
        assert!(
            !status.success(),
            "cargo should fail when manifest is missing"
        );
    }

    #[test]
    fn test_prepare_cache_workspace_path() {
        let tmp = tempfile::tempdir().unwrap();
        let manifest = prepare_cache(
            tmp.path(),
            "prog",
            "range(0, 1).return",
            "0.1.0",
            Some("/some/path"),
        )
        .unwrap();
        let toml = fs::read_to_string(&manifest).unwrap();
        assert!(
            toml.contains("path = \"/some/path\""),
            "expected path dep in Cargo.toml, got: {toml}"
        );
        assert!(
            !toml.contains("version ="),
            "should not emit version dep when workspace_path is set, got: {toml}"
        );
    }

    #[test]
    fn test_prepare_cache_rejects_injection() {
        let tmp = tempfile::tempdir().unwrap();
        let result = prepare_cache(
            tmp.path(),
            "prog",
            "range(0, 1).return }).await } fn evil() { ",
            "0.1.0",
            None,
        );
        assert!(
            result.is_err(),
            "prepare_cache should reject program_contents containing '{}{}'",
            '}',
            ')'
        );
    }
}
