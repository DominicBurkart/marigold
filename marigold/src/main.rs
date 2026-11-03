#[cfg(feature = "cli")]
use anyhow::Result;
#[cfg(feature = "cli")]
use clap::{Parser, Subcommand, ValueEnum};

#[cfg(feature = "cli")]
#[derive(ValueEnum, Clone, Copy, Debug, Default, PartialEq, Eq)]
enum CheckFormat {
    #[default]
    Human,
    Json,
}

#[cfg(feature = "cli")]
#[derive(ValueEnum, Clone, Copy, Debug, Default, PartialEq, Eq)]
enum EmitFormat {
    #[default]
    Rust,
    Json,
}

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
    /// Check a program for diagnostics without running it. Exits 1 if any
    /// error is found and 2 if the input cannot be read. JSON ranges are byte
    /// offsets into the unmodified input.
    Check {
        /// Output format
        #[arg(long, value_enum, default_value_t = CheckFormat::Human)]
        format: CheckFormat,

        /// Path of the Marigold file to read
        file: Option<String>,
    },
    /// Print the Rust code generated from a program, or that code with a source map.
    ///
    /// With `--format rust` (the default) prints the Rust code the `m!` macro generates
    /// for the program (without the braces `m!` wraps around it). With `--format json`
    /// prints one JSON document with the program, the Rust code and a list of segments,
    /// each mapping an expression of the program to the code generated for it.
    ///
    /// The generated code depends on the build features of this tool. `cargo install
    /// marigold -F cli` builds the grammar with the `io` feature, which is what the `m!`
    /// macro has under `marigold run` (so the output compiles there), and a tool built
    /// with `-F cli,tokio` or `-F cli,async-std` generates different code. The JSON
    /// document lists the features in effect under `features`.
    ///
    /// To compile the output, write it to a file next to the `main.rs` of a project that
    /// depends on marigold with the features `tokio` and `io`, and run it with
    /// `let _ = include!("gen.rs").await;` inside `#[tokio::main] async fn main()`.
    ///
    /// Offsets are byte offsets into the text of FILE as it is on disk (nothing is
    /// trimmed and line endings are kept). They do not refer to the re-spaced
    /// TokenStream that `m!()` sees. Lines and columns are 1-based and columns count
    /// Unicode scalar values, like rustc.
    ///
    /// Stdout is either a complete document or empty. If the program does not compile,
    /// the errors are printed to stderr as `marigold check` prints them (as its JSON
    /// array with `--format json`) and the exit code is 1. Exits 2 if the input cannot
    /// be read. Without FILE, the program is read from stdin; a file named `-` is not
    /// stdin.
    Emit {
        /// Output format
        #[arg(long, value_enum, default_value_t = EmitFormat::Rust)]
        format: EmitFormat,

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
        Some(Check { format: _, file }) => file.clone(),
        Some(Emit { format: _, file }) => file.clone(),
        Some(CleanAll) => None,
        None => None,
    }
}

/// Read the program from `file`, or from stdin if there is none, exactly as it is
/// (no trimming, so byte offsets index the file on disk). On failure, prints the error
/// and returns `None`; the caller exits with status 2.
#[cfg(feature = "cli")]
fn read_source(file: Option<&str>) -> Option<String> {
    use std::io::Read;

    let source = match file {
        Some(path) => std::fs::read_to_string(path),
        None => {
            let mut buf = String::new();
            std::io::stdin()
                .lock()
                .read_to_string(&mut buf)
                .map(|_| buf)
        }
    };
    match source {
        Ok(source) => Some(source),
        Err(e) => {
            eprintln!("error: could not read {}: {e}", file.unwrap_or("<stdin>"));
            None
        }
    }
}

/// One line per diagnostic, `path:line:column: severity[code]: message`, followed by
/// an indented `help:` line if it has one. Columns count Unicode scalar values.
#[cfg(feature = "cli")]
fn render_human(
    path: &str,
    source: &str,
    diagnostics: &[marigold_grammar::diagnostics::Diagnostic],
) -> String {
    use marigold_grammar::line_index::{ColumnUnit, LineIndex};
    use std::fmt::Write;

    let index = LineIndex::new(source);
    let mut out = String::new();
    for d in diagnostics {
        let position = index.line_col(d.range.start, ColumnUnit::Chars);
        let _ = writeln!(
            out,
            "{path}:{}:{}: {}[{}]: {}",
            position.line,
            position.column,
            d.severity.as_str(),
            d.code,
            d.message
        );
        if let Some(help) = &d.help {
            let _ = writeln!(out, "  help: {help}");
        }
    }
    out
}

#[cfg(feature = "cli")]
fn run_check(format: CheckFormat, file: Option<&str>) -> i32 {
    let Some(source) = read_source(file) else {
        return 2;
    };

    let diagnostics = marigold_grammar::marigold_check(&source);

    match format {
        CheckFormat::Json => match serde_json::to_string_pretty(&diagnostics) {
            Ok(json) => println!("{json}"),
            Err(e) => {
                eprintln!("error: could not serialize diagnostics: {e}");
                return 2;
            }
        },
        CheckFormat::Human => {
            let path = file.unwrap_or("<stdin>");
            print!("{}", render_human(path, &source, &diagnostics));
        }
    }

    i32::from(diagnostics.iter().any(|d| d.is_error()))
}

/// Longest message printed for a program that `marigold check` would not look at.
#[cfg(feature = "cli")]
const MAX_SUMMARY_BYTES: usize = 2048;

/// `(line, column, byte offset, message)` for the parse error pest reported as `message`:
/// the position it names (` --> line:column`, columns in characters; `1:1` / 0 if there is
/// none), its last line (`= expected ...`) and no excerpt of the program. At most
/// [`MAX_SUMMARY_BYTES`] long, control characters other than newlines escaped.
#[cfg(feature = "cli")]
fn summarize_parse_error(source: &str, message: &str) -> (usize, usize, usize, String) {
    use marigold_grammar::line_index::{ColumnUnit, LineIndex};

    let position = message.split_once(" --> ").and_then(|(_, rest)| {
        let (line, rest) = rest.split_once(':')?;
        let column = rest.split(|c: char| !c.is_ascii_digit()).next()?;
        Some((line.parse::<usize>().ok()?, column.parse::<usize>().ok()?))
    });
    let (line, column, offset) = position
        .and_then(|(line, column)| {
            let offset = LineIndex::new(source).offset(line, column, ColumnUnit::Chars)?;
            Some((line, column, offset))
        })
        .unwrap_or((1, 1, 0));

    let summary = match message.rsplit_once("\n  = ") {
        Some((_, expected)) => format!("Parse error: {expected}"),
        None => message.to_string(),
    };
    let mut safe = String::new();
    for c in summary.chars() {
        let escaped = if c.is_control() && c != '\n' {
            c.escape_debug().to_string()
        } else {
            c.to_string()
        };
        if safe.len() + escaped.len() > MAX_SUMMARY_BYTES {
            safe.push_str(" [message truncated]");
            break;
        }
        safe.push_str(&escaped);
    }
    (line, column, offset, safe)
}

/// Print why `source` did not compile to stderr, as `marigold check` would (errors
/// only), and return the exit status.
#[cfg(feature = "cli")]
fn report_emit_failure(
    format: EmitFormat,
    path: &str,
    source: &str,
    parse_error: &marigold_grammar::parser::MarigoldParseError,
) -> i32 {
    use marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES;

    let errors: Vec<_> = if source.len() <= MAX_CHECK_INPUT_BYTES {
        marigold_grammar::marigold_check(source)
            .into_iter()
            .filter(|d| d.is_error())
            .collect()
    } else {
        Vec::new()
    };

    let (line, column, offset, message) = summarize_parse_error(source, &parse_error.0);
    let rendered = match format {
        EmitFormat::Rust if errors.is_empty() => {
            format!("{path}:{line}:{column}: error[syntax-error]: {message}\n")
        }
        EmitFormat::Rust => render_human(path, source, &errors),
        EmitFormat::Json => {
            if errors.is_empty() {
                let message = serde_json::Value::String(message).to_string();
                format!(
                    "[\n  {{\n    \"range\": {{\n      \"start\": {offset},\n      \"end\": {offset}\n    }},\n    \
                     \"severity\": \"error\",\n    \"code\": \"syntax-error\",\n    \
                     \"message\": {message}\n  }}\n]\n"
                )
            } else {
                match serde_json::to_string_pretty(&errors) {
                    Ok(json) => format!("{json}\n"),
                    Err(e) => {
                        eprintln!("error: could not serialize diagnostics: {e}");
                        return 2;
                    }
                }
            }
        }
    };
    eprint!("{rendered}");
    1
}

/// Write `bytes` to stdout. A reader that goes away early (`marigold emit f | head`)
/// is not an error.
#[cfg(feature = "cli")]
fn write_stdout(bytes: &[u8]) -> i32 {
    use std::io::Write;

    let mut stdout = std::io::stdout().lock();
    match stdout.write_all(bytes).and_then(|()| stdout.flush()) {
        Ok(()) => 0,
        Err(e) if e.kind() == std::io::ErrorKind::BrokenPipe => 0,
        Err(e) => {
            eprintln!("error: could not write to stdout: {e}");
            2
        }
    }
}

#[cfg(feature = "cli")]
fn run_emit(format: EmitFormat, file: Option<&str>) -> i32 {
    let Some(source) = read_source(file) else {
        return 2;
    };

    let program = match marigold_grammar::marigold_generate(&source) {
        Ok(program) => program,
        Err(e) => {
            return report_emit_failure(format, file.unwrap_or("<stdin>"), &source, &e);
        }
    };

    match format {
        EmitFormat::Rust => write_stdout(program.rust.as_bytes()),
        EmitFormat::Json => {
            let document =
                marigold_grammar::emit::EmitDocument::new(&source, &program.rust, &program.map);
            match serde_json::to_string_pretty(&document) {
                Ok(json) => write_stdout(format!("{json}\n").as_bytes()),
                Err(e) => {
                    eprintln!("error: could not serialize the document: {e}");
                    2
                }
            }
        }
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

    if let Some(Check { format, file }) = &args.command {
        std::process::exit(run_check(*format, file.as_deref()));
    }
    if let Some(Emit { format, file }) = &args.command {
        std::process::exit(run_emit(*format, file.as_deref()));
    }

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
            Check { .. } => unreachable!("check is handled before cache setup"),
            Emit { .. } => unreachable!("emit is handled before cache setup"),
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

#[cfg(all(test, feature = "cli"))]
mod summary_tests {
    use super::{summarize_parse_error, MAX_SUMMARY_BYTES};

    #[test]
    fn keeps_the_position_and_the_expectation_of_a_pest_message() {
        let message =
            "Parse error:  --> 3:9\n  |\n3 | range(0,\n  |         ^---\n  |\n  = expected free_text_literal or inclusive_marker";
        let source = "   \n\nrange(0,";
        assert_eq!(
            summarize_parse_error(source, message),
            (
                3,
                9,
                source.len(),
                "Parse error: expected free_text_literal or inclusive_marker".to_string()
            )
        );
    }

    #[test]
    fn counts_the_column_in_characters_not_bytes() {
        let message = "Parse error:  --> 1:5\n  |\n  = expected x";
        assert_eq!(summarize_parse_error("ééab!", message).2, 6);
    }

    #[test]
    fn a_message_without_a_position_is_reported_at_the_start() {
        let (line, column, offset, text) = summarize_parse_error("abc", "Parse error: odd");
        assert_eq!((line, column, offset), (1, 1, 0));
        assert_eq!(text, "Parse error: odd");
        assert_eq!(
            summarize_parse_error("abc", "Parse error:  --> 9:9\n  = expected x").2,
            0
        );
    }

    #[test]
    fn escapes_control_characters_and_truncates_a_long_message() {
        let message = format!(
            "Parse error: \u{1b}]0;pwned\u{7}\nline two\t{}",
            "x".repeat(10_000)
        );
        let (_, _, _, text) = summarize_parse_error("", &message);
        assert!(
            !text.contains('\u{1b}') && !text.contains('\u{7}'),
            "{text:?}"
        );
        assert!(text.contains("\\u{1b}]0;pwned\\u{7}\nline two"), "{text:?}");
        assert!(text.ends_with(" [message truncated]"), "{text:?}");
        assert!(text.len() <= MAX_SUMMARY_BYTES + " [message truncated]".len());
    }

    #[test]
    fn truncation_never_splits_a_multibyte_character_and_counts_bytes() {
        let message = format!("Parse error: {}", "é".repeat(2000));
        let (_, _, _, text) = summarize_parse_error("", &message);
        assert!(text.ends_with(" [message truncated]"), "{}", text.len());
        assert!(text.len() <= MAX_SUMMARY_BYTES + " [message truncated]".len());
        assert!(text.len() > MAX_SUMMARY_BYTES - 4, "{}", text.len());
        let message = format!("Parse error: {}", "😀".repeat(1000));
        let (_, _, _, text) = summarize_parse_error("", &message);
        assert!(text.ends_with(" [message truncated]"));
        assert!(text.len() <= MAX_SUMMARY_BYTES + " [message truncated]".len());
        assert!(text.len() > MAX_SUMMARY_BYTES - 4, "{}", text.len());
    }

    #[test]
    fn c1_control_characters_are_escaped_too() {
        let (_, _, _, text) = summarize_parse_error("", "Parse error: a\u{9b}31mb\u{85}");
        assert!(
            !text.contains('\u{9b}') && !text.contains('\u{85}'),
            "{text:?}"
        );
    }

    #[test]
    fn the_column_after_astral_characters_is_counted_in_characters() {
        let message = "Parse error:  --> 1:5\n  |\n  = expected x";
        assert_eq!(summarize_parse_error("😀😀ab!", message).2, 10);
    }

    #[test]
    fn escapes_what_the_expectation_line_holds() {
        let message = "Parse error:  --> 1:1\n  |\n  = expected \u{1b}[31m";
        let (_, _, _, text) = summarize_parse_error("", message);
        assert_eq!(text, "Parse error: expected \\u{1b}[31m");
    }
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
    ///
    /// The binary is built with the features these tests were built with. It is the
    /// same file as `CARGO_BIN_EXE_marigold` in the integration tests (`emit_cli`
    /// compares its output with `marigold_parse`, which depends on the features), so a
    /// build with fewer features would replace the binary they test.
    static BINARY: LazyLock<PathBuf> = LazyLock::new(|| {
        let workspace_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap();
        let mut features = vec!["cli"];
        if cfg!(feature = "tokio") {
            features.push("tokio");
        }
        if cfg!(feature = "async-std") {
            features.push("async-std");
        }
        if cfg!(feature = "io") {
            features.push("io");
        }
        let status = Command::new("cargo")
            .args(["build", "--features", &features.join(","), "-p", "marigold"])
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

        let status = Command::new(binary)
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
        let status = Command::new(binary)
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
        let status = Command::new(binary)
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
        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        let cache_dir = tmp.join(".marigold/test_clean");
        assert!(cache_dir.exists(), "cache should exist after run");

        // Clean
        let status = Command::new(binary)
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
        let status = Command::new(binary)
            .args(["run", marigold_file.to_str().unwrap()])
            .env("HOME", &tmp)
            .env("MARIGOLD_WORKSPACE_PATH", marigold_workspace_path())
            .status()
            .expect("could not run marigold");
        assert!(status.success(), "marigold run failed");

        let cache_root = tmp.join(".marigold");
        assert!(cache_root.exists(), "cache should exist after run");

        // Clean all
        let status = Command::new(binary)
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
