#![forbid(unsafe_code)]

use lsp_server::Connection;
use std::process::ExitCode;

const USAGE: &str = "Usage: marigold-lsp [mcp]

With no arguments, runs the language server over stdio.

Commands:
  mcp          Run a Model Context Protocol server over stdio

Options:
  -h, --help     Print this help
  -V, --version  Print the version";

fn run_lsp() -> Result<(), marigold_lsp::Error> {
    let (connection, io_threads) = Connection::stdio();
    marigold_lsp::serve(&connection)?;
    drop(connection);
    io_threads.join()?;
    Ok(())
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let outcome = match args
        .iter()
        .map(String::as_str)
        .collect::<Vec<_>>()
        .as_slice()
    {
        [] | ["--stdio"] => run_lsp(),
        ["mcp"] => marigold_lsp::mcp::serve_stdio().map_err(Into::into),
        ["--help"] | ["-h"] => {
            println!("{USAGE}");
            Ok(())
        }
        ["--version"] | ["-V"] => {
            println!("marigold-lsp {}", env!("CARGO_PKG_VERSION"));
            Ok(())
        }
        _ => {
            eprintln!("marigold-lsp: unrecognized arguments\n{USAGE}");
            return ExitCode::from(2);
        }
    };
    match outcome {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("marigold-lsp: {e}");
            ExitCode::FAILURE
        }
    }
}
