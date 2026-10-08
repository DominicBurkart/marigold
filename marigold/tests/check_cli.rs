#![cfg(feature = "cli")]

use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Output, Stdio};

fn marigold() -> Command {
    Command::new(env!("CARGO_BIN_EXE_marigold"))
}

fn write_temp(name: &str, contents: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("marigold-check-cli-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(name);
    std::fs::write(&path, contents).unwrap();
    path
}

fn run_check(args: &[&str], path: &PathBuf) -> Output {
    marigold()
        .arg("check")
        .args(args)
        .arg(path)
        .output()
        .unwrap()
}

#[test]
fn valid_file_exits_zero_with_empty_json_array() {
    let path = write_temp("valid.marigold", "range(0, 10).return");
    let out = run_check(&["--format", "json"], &path);
    assert!(out.status.success(), "{out:?}");
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v, serde_json::json!([]));
}

#[test]
fn broken_file_exits_nonzero_with_positioned_json() {
    let src = "range(Colour).return";
    let path = write_temp("broken.marigold", src);
    let out = run_check(&["--format", "json"], &path);
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    let diags = v.as_array().unwrap();
    assert_eq!(diags.len(), 1, "{v}");
    assert_eq!(diags[0]["code"], "undefined-enum");
    assert_eq!(diags[0]["severity"], "error");
    let start = diags[0]["range"]["start"].as_u64().unwrap() as usize;
    let end = diags[0]["range"]["end"].as_u64().unwrap() as usize;
    assert_eq!(&src[start..end], "Colour");
}

#[test]
fn offsets_are_not_shifted_by_surrounding_whitespace() {
    let src = "\n\n  range(Colour).return\n\n";
    let path = write_temp("padded.marigold", src);
    let out = run_check(&["--format", "json"], &path);
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    let start = v[0]["range"]["start"].as_u64().unwrap() as usize;
    let end = v[0]["range"]["end"].as_u64().unwrap() as usize;
    assert_eq!(&src[start..end], "Colour");
}

#[test]
fn human_format_is_the_default_and_shows_line_and_column() {
    let path = write_temp("human.marigold", "range(0, 1).return\nrange(Colour).return");
    let out = run_check(&[], &path);
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    let stdout = String::from_utf8(out.stdout).unwrap();
    let expected_prefix = format!("{}:2:7: error[undefined-enum]:", path.display());
    assert!(stdout.starts_with(&expected_prefix), "{stdout}");
}

#[test]
fn human_format_columns_count_characters_not_bytes() {
    let path = write_temp("unicode.marigold", "\"ünï\" range(Colour).return");
    let out = run_check(&[], &path);
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(
        stdout.contains(":1:") && stdout.contains("error["),
        "{stdout}"
    );
}

#[test]
fn reads_stdin_when_no_file_is_given() {
    let mut child = marigold()
        .args(["check", "--format", "json"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"range(Colour).return")
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert_eq!(out.status.code(), Some(1));
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v[0]["code"], "undefined-enum");
}

#[test]
fn missing_file_is_a_usage_error_not_a_diagnostic() {
    let path = PathBuf::from("/nonexistent/definitely-missing.marigold");
    let out = run_check(&["--format", "json"], &path);
    assert_eq!(out.status.code(), Some(2), "{out:?}");
    assert!(out.stdout.is_empty());
}

#[test]
fn human_columns_count_characters_after_multibyte_text() {
    let path = write_temp(
        "unicode-col.marigold",
        "range(0, 1).return\n\"éé😀\" range(Colour).return",
    );
    let out = run_check(&[], &path);
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains(":2:"), "{stdout}");
    assert!(stdout.contains("error["), "{stdout}");
}

#[test]
fn crlf_line_endings_report_the_right_line() {
    let src = "range(0, 1).return\r\nrange(Colour).return\r\n";
    let path = write_temp("crlf.marigold", src);
    let out = run_check(&["--format", "json"], &path);
    assert_eq!(out.status.code(), Some(1));
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    let start = v[0]["range"]["start"].as_u64().unwrap() as usize;
    assert_eq!(&src[start..start + 6], "Colour");
    let human = run_check(&[], &path);
    let stdout = String::from_utf8(human.stdout).unwrap();
    assert!(stdout.contains(":2:7: error[undefined-enum]"), "{stdout}");
}

#[test]
fn oversized_input_exits_one_with_input_too_large() {
    let path = write_temp("huge.marigold", &" ".repeat(10 * 1024 * 1024 + 1));
    let out = run_check(&["--format", "json"], &path);
    assert_eq!(out.status.code(), Some(1));
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v[0]["code"], "input-too-large");
}

#[test]
fn warning_only_program_prints_warning_label_and_exits_zero() {
    let path = write_temp("warning.marigold", "a = a\na.return");
    let out = run_check(&[], &path);
    assert_eq!(out.status.code(), Some(0), "{out:?}");
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(
        stdout.contains("warning[undefined-stream-variable]"),
        "{stdout}"
    );

    let out = run_check(&["--format", "json"], &path);
    assert_eq!(out.status.code(), Some(0), "{out:?}");
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v[0]["severity"], "warning");
    assert_eq!(v[0]["code"], "undefined-stream-variable");
}

#[test]
fn undefined_stream_variable_prints_information_label_and_exits_zero() {
    let path = write_temp("undefined_variable.marigold", "xs.return");
    let out = run_check(&[], &path);
    assert_eq!(out.status.code(), Some(0), "{out:?}");
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(
        stdout.contains("information[undefined-stream-variable]"),
        "{stdout}"
    );
    assert!(!stdout.contains("warning["), "{stdout}");

    let out = run_check(&["--format", "json"], &path);
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v[0]["severity"], "information");
}

#[test]
fn information_only_program_prints_information_label_and_exits_zero() {
    let path = write_temp("information.marigold", "range(0, 1).map(f).return");
    let out = run_check(&[], &path);
    assert_eq!(out.status.code(), Some(0), "{out:?}");
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("information[undefined-fn]"), "{stdout}");

    let out = run_check(&["--format", "json"], &path);
    assert_eq!(out.status.code(), Some(0), "{out:?}");
    let v: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(v[0]["severity"], "information");
    assert_eq!(v[0]["code"], "undefined-fn");
}

#[test]
fn warning_alongside_error_still_exits_one() {
    let path = write_temp(
        "mixed.marigold",
        "range(0, 1).map(f).return\nrange(Colour).return",
    );
    let out = run_check(&[], &path);
    assert_eq!(out.status.code(), Some(1), "{out:?}");
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("information[undefined-fn]"), "{stdout}");
    assert!(stdout.contains("error[undefined-enum]"), "{stdout}");
}
