#[cfg(feature = "cli")]
use anyhow::Result;
#[cfg(feature = "cli")]
use clap::Parser;

#[cfg(feature = "cli")]
const MARIGOLD_VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(feature = "cli")]
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Path of the marigold file to read
    #[arg(short, long)]
    file: Option<String>,
}

#[cfg(not(feature = "cli"))]
fn main() {
    eprintln!("marigold needs to be installed with the cli feature (`cargo install --force marigold -F cli`)");
}

#[cfg(feature = "cli")]
fn main() -> Result<()> {
    use std::io;
    use std::io::Read;
    use std::io::Write;
    use std::process::{Command, Stdio};

    let args = Args::parse();

    let program_contents = match args.file {
        Some(path) => std::fs::read_to_string(&path)?,
        None => {
            let mut stdin = String::new();
            io::stdin().lock().read_to_string(&mut stdin)?;
            stdin
        }
    };

    let mut file = tempfile::NamedTempFile::new()?;
    write!(
        file,
        r#"#!/usr/bin/env run-cargo-script
        //! ```cargo
        //! [package]
        //! edition = "2021"
        //!
        //! [dependencies]
        //! tokio = {{ version = "1", features = ["full"] }}
        //! marigold = {{ version="={MARIGOLD_VERSION}", features=["tokio", "io"] }}
        //! ```

        use marigold::m;

        #[tokio::main]
        async fn main() {{
            m!({program_contents}).await;
        }}
    "#
    )?;

    let exit_status = Command::new("cargo")
        .args([
            "script",
            file.path()
                .as_os_str()
                .to_str()
                .expect("marigold failure: generated non-utf8 file descriptor"),
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?
        .wait()?;

    std::process::exit(exit_status.code().unwrap_or(0));
}
