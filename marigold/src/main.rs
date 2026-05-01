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

    // Reject program contents containing `})` — that sequence closes the
    // `marigold::m!({program_contents}).await` invocation and would allow
    // arbitrary Rust code injection.
    if program_contents.contains("}") {
        anyhow::bail!("program contents contain '})' which would escape the macro invocation");
    }

    let program_project_dir = cache_root.join(program_name);
    let program_src_dir = program_project_dir.join("src");

    std::fs::create_dir_all(&program_src_dir)?;

    std::fs::write(
        program_src_dir.join("main.rs"),
        format!("#[tokio::main] async fn main() {{ marigold::m!({program_contents}).await }}"),
    )?;

    let manifest_path = program_project_dir.join("Cargo.toml");

    let marigold_dep = if let Some(path) = workspace_path {
        format!("marigold = {{ path = \"{path}\", features = [\"tokio\", \"io\"]}}\/n")
    } else {
        format!(
            "marigold = {{ version = \"={marigold_version}\", features = [\"tokio\", \"io\"]}}\/n"
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
