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
    use std::sync::Once;

    static BUILD_CLI: Once = Once::new();

    /// Build the CLI binary once (shared across tests) and return its path.
    fn cli_binary() -> PathBuf {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

        BUILD_CLI.call_once(|| {
            assert!(
                Command::new("cargo")
                    .args(["build", "--features", "cli"])
                    .current_dir(&manifest_dir)
                    .status()
                    .expect("could not build marigold cli")
                    .success(),
                "failed to build marigold cli"
            );
        });

        manifest_dir
            .parent()
            .expect("manifest dir has no parent")
            .join("target")
            .join("debug")
            .join("marigold")
    }

    fn test_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("marigold_test_{name}"));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).expect("could not create test dir");
        dir
    }

    /// Create a Command for the marigold CLI with an isolated HOME directory
    /// so each test gets its own `~/.marigold/` cache (no cross-test races).
    /// CARGO_HOME is preserved so cargo can find its registry and config.
    fn marigold_cmd(test_home: &std::path::Path) -> Command {
        let mut cmd = Command::new(cli_binary());
        cmd.env("MARIGOLD_WORKSPACE_PATH", env!("CARGO_MANIFEST_DIR"));
        cmd.env("HOME", test_home);
        // Preserve CARGO_HOME so cargo can find its registry and config
        let cargo_home = std::env::var("CARGO_HOME").unwrap_or_else(|_| {
            std::env::var("HOME")
                .map(|h| format!("{h}/.cargo"))
                .unwrap_or_else(|_| "/root/.cargo".to_string())
        });
        cmd.env("CARGO_HOME", cargo_home);
        cmd
    }

    #[test]
    fn test_run() {
        let dir = test_dir("run");
        let marigold_file = dir.join("test_run.marigold");
        let csv_file = dir.join("test_run.csv");

        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        assert!(marigold_cmd(&dir)
            .args(["run", marigold_file.to_str().unwrap()])
            .status()
            .expect("could not run marigold command")
            .success());

        assert_eq!(
            fs::read_to_string(&csv_file).expect("could not read CSV"),
            "0\n1\n2\n"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_install_and_uninstall() {
        let dir = test_dir("install");
        let install_root = test_dir("install_root");
        let marigold_file = dir.join("test_install.marigold");
        let csv_file = dir.join("test_install.csv");

        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        // Install to a temporary root instead of ~/.cargo/bin/
        assert!(marigold_cmd(&dir)
            .env("CARGO_INSTALL_ROOT", &install_root)
            .args(["install", marigold_file.to_str().unwrap()])
            .status()
            .expect("could not run marigold install")
            .success());

        assert!(!csv_file.exists());

        // Run the installed binary by its full path (no PATH lookup)
        let installed_binary = install_root.join("bin").join("test_install");
        assert!(
            installed_binary.exists(),
            "installed binary not found at {}",
            installed_binary.display()
        );
        assert!(Command::new(&installed_binary)
            .status()
            .expect("could not run installed program")
            .success());

        assert!(csv_file.exists());

        // Uninstall
        assert!(marigold_cmd(&dir)
            .env("CARGO_INSTALL_ROOT", &install_root)
            .args(["uninstall", marigold_file.to_str().unwrap()])
            .status()
            .expect("could not run marigold uninstall")
            .success());

        assert!(
            !installed_binary.exists(),
            "binary should be removed after uninstall"
        );

        let _ = fs::remove_dir_all(&dir);
        let _ = fs::remove_dir_all(&install_root);
    }

    #[test]
    fn test_clean() {
        let dir = test_dir("clean");
        let marigold_file = dir.join("test_clean.marigold");
        let csv_file = dir.join("test_clean.csv");

        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        // Run first to populate the cache
        assert!(marigold_cmd(&dir)
            .args(["run", marigold_file.to_str().unwrap()])
            .status()
            .expect("could not run marigold command")
            .success());

        // Clean the cache for this file
        assert!(marigold_cmd(&dir)
            .args(["clean", marigold_file.to_str().unwrap()])
            .status()
            .expect("could not run marigold clean")
            .success());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_clean_all() {
        let dir = test_dir("clean_all");
        let marigold_file = dir.join("test_clean_all.marigold");
        let csv_file = dir.join("test_clean_all.csv");

        fs::write(
            &marigold_file,
            format!(r#"range(0, 3).write_file("{}", csv)"#, csv_file.display()),
        )
        .expect("could not write test file");

        // Run first to populate the cache
        assert!(marigold_cmd(&dir)
            .args(["run", marigold_file.to_str().unwrap()])
            .status()
            .expect("could not run marigold command")
            .success());

        // Clean all caches
        assert!(marigold_cmd(&dir)
            .args(["clean-all"])
            .status()
            .expect("could not run marigold clean-all")
            .success());

        let _ = fs::remove_dir_all(&dir);
    }
}
