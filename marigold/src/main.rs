#[cfg(feature = "cli")]
use anyhow::Result;
#[cfg(feature = "cli")]
use clap::{Parser, Subcommand};

#[cfg(feature = "cli")]
#[derive(Subcommand, Debug)]
enum MarigoldCommand {
    /// Run the program.
    Run {
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
}

#[cfg(feature = "cli")]
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Command to run
    #[command(subcommand)]
    command: Option<MarigoldCommand>,

    /// Disables optimizations to speed up compilation.
    #[arg(short, long, default_value_t = false)]
    unoptimized: bool,
}

#[cfg(not(feature = "cli"))]
fn main() {
    eprintln!("Marigold needs to be installed with the cli feature (`cargo install --force marigold -F cli`)");
}

#[cfg(feature = "cli")]
fn get_file_name_argument(args: &Args) -> Option<String> {
    use MarigoldCommand::*;

    match &args.command {
        Some(Run { file }) => file.clone(),
        Some(Install { file }) => file.clone(),
        Some(Uninstall { file }) => file.clone(),
        Some(Clean { file }) => file.clone(),
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
                if path.contains('.') {
                    let partial = &path[0..path.find('.').unwrap()];
                    if partial.len() > 1 {
                        partial.to_string()
                    } else {
                        "marigold_program".to_string()
                    }
                } else {
                    path.to_string()
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
        Some(command) => match command {
            Run { file: _ } => "run",
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
marigold = {{ version = "={MARIGOLD_VERSION}", features = ["tokio", "io"]}}
        "#
        ),
    )?;

    let exit_status = {
        if args.unoptimized {
            if command == "install" {
                eprintln!("`install` and `fast` are not compatible.");
                std::process::exit(1);
            }

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
            if command == "install" {
                // `--release` is not accepted with install
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
        }
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
