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
        format!(r#"marigold = {{ path = "{path}", features = ["tokio", "io"]}}"#)
    } else {
        format!(r#"marigold = {{ version = "={marigold_version}", features = ["tokio", "io"]}}"#)
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

/// Invoke `cargo run` (with `--manifest-path`) or `cargo install` (with `--path`).
/// Only these two commands are supported; the `release` flag is only meaningful for `run`.
#[cfg(feature = "cli")]
fn invoke_cargo_run_or_install(
    command: &str,
    path: &std::path::Path,
    release: bool,
) -> Result<std::process::ExitStatus> {
    use std::process::Command;

    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow::anyhow!("Marigold could not parse cache path as utf-8"))?;

    let status = if command == "run" {
        if release {
            Command::new("cargo")
                .args([command, "--release", "--manifest-path", path_str])
                .spawn()?
                .wait()?
        } else {
            Command::new("cargo")
                .args([command, "--manifest-path", path_str])
                .spawn()?
                .wait()?
        }
    } else {
        Command::new("cargo")
            .args([command, "--path", path_str])
            .spawn()?
            .wait()?
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
        invoke_cargo_run_or_install("run", &manifest_path, !unoptimized)?
    } else {
        invoke_cargo_run_or_install(
            "install",
            &marigold_cache_directory.join(&program_name),
            false,
        )?
    };

    std::process::exit(exit_status.code().unwrap_or(0));
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;
    use std::process::Command;

    #[test]
    fn test_cli() {
        install_marigold_cli();
        test_run();
        test_install();
        test_uninstall();
        test_clean();
        test_clean_all();
        cleanup();
    }

    fn install_marigold_cli() {
        assert!(Command::new("cargo")
            .args(["install", "--force", "--path", ".", "-F", "cli"])
            .spawn()
            .expect("could not install marigold for test")
            .wait()
            .expect("marigold installation process lost")
            .success())
    }

    fn test_run() {
        fs::write(
            "test_run.marigold",
            r#"range(0, 3).write_file("test_run.csv", csv)"#,
        )
        .expect("could not write test file");
        assert!(Command::new("marigold")
            .args(["run", "test_run.marigold"])
            .spawn()
            .expect("could not run marigold command")
            .wait()
            .expect("marigold command lost")
            .success());
        assert_eq!(
            fs::read_to_string("test_run.csv").expect("could not read CSV"),
            "0\n1\n2\n"
        );
    }

    fn test_install() {
        fs::write(
            "test_install.marigold",
            r#"range(0, 3).write_file("test_install.csv", csv)"#,
        )
        .expect("could not write test file");

        assert!(Command::new("marigold")
            .args(["install", "test_install.marigold"])
            .spawn()
            .expect("could not run marigold command")
            .wait()
            .expect("marigold command lost")
            .success());

        assert!(!Path::new("test_install.csv").exists());

        assert!(Command::new("test_install")
            .spawn()
            .expect("could not run marigold command")
            .wait()
            .expect("marigold command lost")
            .success());

        assert!(Path::new("test_install.csv").exists());
    }

    fn test_uninstall() {
        assert!(Command::new("marigold")
            .args(["uninstall", "test_install.marigold"])
            .spawn()
            .expect("could not run marigold command")
            .wait()
            .expect("marigold command lost")
            .success());
    }

    fn test_clean() {
        assert!(Command::new("marigold")
            .args(["clean", "test_run.marigold"])
            .spawn()
            .expect("could not run marigold command")
            .wait()
            .expect("marigold command lost")
            .success());
    }

    fn test_clean_all() {
        assert!(Command::new("marigold")
            .args(["clean-all"])
            .spawn()
            .expect("could not run marigold command")
            .wait()
            .expect("marigold command lost")
            .success());
    }

    fn cleanup() {
        for file in &[
            "test_run.marigold",
            "test_run.csv",
            "test_install.marigold",
            "test_install.csv",
        ] {
            std::fs::remove_file(file).expect("could not delete filee");
        }
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
        clean_all_cache(tmp.path()).expect("should succeed on empty dir");
        assert!(!tmp.path().exists());
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
        let result = invoke_cargo_run_or_install("run", &manifest, false);
        assert!(
            result.is_err(),
            "expected error when cache dir was removed before cargo invocation"
        );
    }
}
