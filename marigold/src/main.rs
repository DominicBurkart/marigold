#[cfg(feature = "cli")]
use anyhow::Result;
#[cfg(feature = "cli")]
use clap::Parser;

#[cfg(feature = "cli")]
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path of the marigold file to read
    #[arg(short, long)]
    file: Option<String>,

    /// Weather to enable compiler optimizations
    #[arg(short, long, default_value_t = false)]
    release: bool,
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
    use std::io::Write;
    use std::process::Command;

    let args = Args::parse();

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
        format!("{file_name}_")
    };

    let program_contents = match args.file {
        Some(path) => std::fs::read_to_string(&path)?.trim().to_string(),
        None => {
            let mut stdin = String::new();
            io::stdin().lock().read_to_string(&mut stdin)?;
            stdin.trim().to_string()
        }
    };

    const MARIGOLD_VERSION: &str = env!("CARGO_PKG_VERSION");

    let mut file = tempfile::Builder::new()
        .prefix(&program_name)
        .suffix(".rs")
        .tempfile()?;

    write!(
        file,
        r#"//#tokio={{ version="1", features=["full"] }}
//# marigold={{ version="={MARIGOLD_VERSION}", features=["tokio", "io"] }}

#[tokio::main]
async fn main() {{ marigold::m!({program_contents}).await }}
"#
    )?;

    let exit_status = Command::new("cargo")
        .args([
            "play",
            "--release",
            "--edition",
            "2021",
            file.path()
                .as_os_str()
                .to_str()
                .expect("marigold failure: generated non-utf8 file descriptor"),
        ])
        .spawn()?
        .wait()?;

    std::process::exit(exit_status.code().unwrap_or(0));
}
