#[cfg(feature = "cli")]
use anyhow::Result;
#[cfg(feature = "cli")]
use clap::{Parser, Subcommand};

#[cfg(feature = "cli")]
#[derive(Subcommand, Debug)]
enum MarigoldCommand {
    /// Run the program. Default program.
    Run,
    /// Install a Marigold program as an executable using Cargo.
    Install,
    /// Uninstall a Marigold program as an executable using Cargo.
    Uninstall,
    /// Clean the cache for the passed file.
    Clean,
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

    /// Path of the Marigold file to read
    #[arg(short, long)]
    file: Option<String>,

    /// Disables optimizations to speed up compilation.
    #[arg(short, long, default_value_t = false)]
    fast: bool,
}

#[cfg(not(feature = "cli"))]
fn main() {
    eprintln!("Marigold needs to be installed with the cli feature (`cargo install --force marigold -F cli`)");
}

#[cfg(feature = "cli")]
fn main() -> Result<()> {
    use convert_case::{Case, Casing};
    use std::io;
    use std::io::Read;
    use std::process::Command;

    const RUST_EDITION: &str = "2021";

    let args = Args::parse();

    let marigold_cache_directory = home::home_dir()
        .expect("could not locate user's home directory for marigold cache")
        .join(".marigold");

    let program_name = {
        let mut file_name = match &args.file {
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
            MarigoldCommand::Run => "run",
            MarigoldCommand::Install => "install",
            MarigoldCommand::Uninstall => std::process::exit(
                Command::new("cargo")
                    .args(["uninstall", &program_name])
                    .spawn()?
                    .wait()?
                    .code()
                    .unwrap_or(0),
            ),
            MarigoldCommand::Clean => {
                std::fs::remove_dir_all(&program_project_dir)?;
                std::process::exit(0);
            }
            MarigoldCommand::CleanAll => {
                std::fs::remove_dir_all(&marigold_cache_directory)?;
                std::process::exit(0);
            }
        },
        None => "run", // default is run.
    };

    let program_src_dir = program_project_dir.join("src");

    std::fs::create_dir_all(&program_src_dir)?;

    let program_contents = match args.file {
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
tokio = {{ version = "1", features = ["full"]}}
marigold = {{ version = "={MARIGOLD_VERSION}", features = ["tokio", "io"]}}
        "#
        ),
    )?;

    let exit_status = {
        if args.fast {
            if command == "install" {
                eprintln!("`install` and `fast` are not compatible.");
                std::process::exit(1);
            }

            Command::new("cargo")
                .args([
                    command,
                    "--manifest_path",
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
        }
    };

    std::process::exit(exit_status.code().unwrap_or(0));
}
