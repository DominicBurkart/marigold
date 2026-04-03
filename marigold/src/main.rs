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
fn main() -> Result<()> {
    use MarigoldCommand::*;

    use convert_case::{Case, Casing};
    use std::io;
    use std::io::Read;
    use std::process::Command;

    const RUST_EDITION: &str = "2021";

    let args = Args::parse();

    let marigold_cache_directory = home::home_dir()
        .expect("could not locate user's home directory for marigold cache")
        .join(".marigold");

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
                std::fs::remove_dir_all(&program_project_dir)?;
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
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        let cache_dir = tmp.join(".marigold/test_clean");
        assert!(cache_dir.exists(), "cache should exist after run");

        // Clean
        let status = Command::new(&binary)
            .args(["clean", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
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
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        let cache_root = tmp.join(".marigold");
        assert!(cache_root.exists(), "cache should exist after run");

        // Clean all
        let status = Command::new(&binary)
            .args(["clean-all"])
            .env("HOME", &tmp)
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
