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
impl MarigoldCommand {
    /// Returns the Marigold source file path the command was invoked with, if any.
    /// `CleanAll` does not take a file.
    fn file(&self) -> Option<&str> {
        match self {
            MarigoldCommand::Run { file, .. }
            | MarigoldCommand::Install { file }
            | MarigoldCommand::Uninstall { file }
            | MarigoldCommand::Clean { file }
            | MarigoldCommand::Analyze { file } => file.as_deref(),
            MarigoldCommand::CleanAll => None,
        }
    }
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

/// Read a Marigold program from `path`, or stdin if `path` is `None`. Trims trailing whitespace.
#[cfg(feature = "cli")]
fn read_program_contents(path: Option<&str>) -> std::io::Result<String> {
    use std::io::Read;

    let raw = match path {
        Some(path) => std::fs::read_to_string(path)?,
        None => {
            let mut stdin = String::new();
            std::io::stdin().lock().read_to_string(&mut stdin)?;
            stdin
        }
    };
    Ok(raw.trim().to_string())
}

/// Derive a Cargo-package-friendly program name from the source file path.
/// Falls back to "marigold_program" when no path is supplied or the stem is too short.
#[cfg(feature = "cli")]
fn derive_program_name(file: Option<&str>) -> String {
    use convert_case::{Case, Casing};

    const FALLBACK: &str = "marigold_program";

    let stem = file
        .and_then(|p| std::path::Path::new(p).file_stem().and_then(|s| s.to_str()))
        .filter(|s| s.len() > 1)
        .unwrap_or(FALLBACK);

    let snake = stem.to_case(Case::Snake);
    snake
        .trim_start_matches('_')
        .trim_end_matches('_')
        .to_string()
}

#[cfg(feature = "cli")]
fn main() -> Result<()> {
    use std::process::Command;

    const RUST_EDITION: &str = "2021";
    const MARIGOLD_VERSION: &str = env!("CARGO_PKG_VERSION");

    let args = Args::parse();

    let marigold_cache_directory = home::home_dir()
        .expect("could not locate user's home directory for marigold cache")
        .join(".marigold");

    let file_name_argument = args
        .command
        .as_ref()
        .and_then(|c| c.file())
        .map(String::from);

    let program_name = derive_program_name(file_name_argument.as_deref());
    let program_project_dir = marigold_cache_directory.join(&program_name);

    // Handle commands that exit early without building the cached project.
    match args.command.as_ref() {
        Some(MarigoldCommand::Uninstall { .. }) => {
            std::process::exit(
                Command::new("cargo")
                    .args(["uninstall", &program_name])
                    .spawn()?
                    .wait()?
                    .code()
                    .unwrap_or(0),
            );
        }
        Some(MarigoldCommand::Clean { .. }) => {
            std::fs::remove_dir_all(&program_project_dir)?;
            std::process::exit(0);
        }
        Some(MarigoldCommand::CleanAll) => {
            if marigold_cache_directory.exists() {
                std::fs::remove_dir_all(&marigold_cache_directory)?;
            }
            std::process::exit(0);
        }
        Some(MarigoldCommand::Analyze { .. }) => {
            let program_contents = read_program_contents(file_name_argument.as_deref())?;
            let result = marigold_grammar::marigold_analyze(&program_contents)
                .map_err(|e| anyhow::anyhow!("{}", e))?;
            println!("{}", serde_json::to_string_pretty(&result)?);
            std::process::exit(0);
        }
        Some(MarigoldCommand::Run { .. }) | Some(MarigoldCommand::Install { .. }) | None => {}
    }

    // Default command (no subcommand) is `run`.
    let cargo_subcommand = match args.command {
        Some(MarigoldCommand::Install { .. }) => "install",
        _ => "run",
    };

    let program_src_dir = program_project_dir.join("src");
    std::fs::create_dir_all(&program_src_dir)?;

    let program_contents = read_program_contents(file_name_argument.as_deref())?;

    std::fs::write(
        program_src_dir.join("main.rs"),
        format!("#[tokio::main] async fn main() {{ marigold::m!({program_contents}).await }}"),
    )?;

    let manifest_path = program_project_dir.join("Cargo.toml");

    let marigold_dep = match std::env::var("MARIGOLD_WORKSPACE_PATH") {
        Ok(workspace_path) => {
            format!(r#"marigold = {{ path = "{workspace_path}", features = ["tokio", "io"]}}"#)
        }
        Err(_) => {
            format!(
                r#"marigold = {{ version = "={MARIGOLD_VERSION}", features = ["tokio", "io"]}}"#
            )
        }
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

    // Build the cargo invocation. `run` targets the manifest in place (with optional --release),
    // `install` installs from the cached project path.
    let mut cargo_args: Vec<&str> = vec![cargo_subcommand];
    if cargo_subcommand == "run" {
        let unoptimized = matches!(
            args.command,
            Some(MarigoldCommand::Run {
                unoptimized: true,
                ..
            })
        );
        if !unoptimized {
            cargo_args.push("--release");
        }
        cargo_args.push("--manifest-path");
        cargo_args.push(
            manifest_path
                .to_str()
                .expect("Marigold could not parse cache manifest path as utf-8"),
        );
    } else {
        cargo_args.push("--path");
        cargo_args.push(
            program_project_dir
                .to_str()
                .expect("Marigold could not parse cache manifest path as utf-8"),
        );
    }

    let exit_status = Command::new("cargo").args(&cargo_args).spawn()?.wait()?;
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

    #[test]
    fn derive_program_name_handles_paths_and_fallbacks() {
        use super::derive_program_name;

        // Real source file → snake-cased stem.
        assert_eq!(
            derive_program_name(Some("/tmp/HelloWorld.marigold")),
            "hello_world"
        );
        // No path supplied → stable fallback.
        assert_eq!(derive_program_name(None), "marigold_program");
        // Single-character stems are too short to be useful Cargo package names → fallback.
        assert_eq!(derive_program_name(Some("a.marigold")), "marigold_program");
        // Names that snake-case to leading/trailing underscores are stripped so the
        // resulting Cargo package name is still valid.
        assert_eq!(derive_program_name(Some("_Foo_.marigold")), "foo");
    }

    #[test]
    fn marigold_command_file_extracts_path_for_each_variant() {
        use super::MarigoldCommand;

        let path = Some("prog.marigold".to_string());
        assert_eq!(
            MarigoldCommand::Run {
                unoptimized: false,
                file: path.clone()
            }
            .file(),
            Some("prog.marigold")
        );
        assert_eq!(
            MarigoldCommand::Install { file: path.clone() }.file(),
            Some("prog.marigold")
        );
        assert_eq!(
            MarigoldCommand::Uninstall { file: path.clone() }.file(),
            Some("prog.marigold")
        );
        assert_eq!(
            MarigoldCommand::Clean { file: path.clone() }.file(),
            Some("prog.marigold")
        );
        assert_eq!(
            MarigoldCommand::Analyze { file: path }.file(),
            Some("prog.marigold")
        );
        assert_eq!(MarigoldCommand::CleanAll.file(), None);
    }
}
