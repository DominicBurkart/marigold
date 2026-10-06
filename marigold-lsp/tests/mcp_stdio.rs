use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};
use std::sync::Mutex;
use std::time::Duration;

const HANG_GUARD: Duration = Duration::from_secs(60);

static LIVE: Mutex<Vec<u32>> = Mutex::new(Vec::new());

fn spawn(args: &[&str]) -> Child {
    let child = Command::new(env!("CARGO_BIN_EXE_marigold-lsp"))
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let pid = child.id();
    LIVE.lock().unwrap().push(pid);
    std::thread::spawn(move || {
        std::thread::sleep(HANG_GUARD);
        if LIVE.lock().unwrap().contains(&pid) {
            let _ = Command::new("kill").arg("-9").arg(pid.to_string()).status();
        }
    });
    child
}

fn finish(child: Child) -> (std::process::ExitStatus, String, String) {
    let pid = child.id();
    let out = child.wait_with_output().unwrap();
    LIVE.lock().unwrap().retain(|p| *p != pid);
    (
        out.status,
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

fn read_json_line(reader: &mut impl BufRead) -> Value {
    let mut line = String::new();
    reader.read_line(&mut line).unwrap();
    assert!(line.ends_with('\n'), "unterminated stdout line: {line:?}");
    serde_json::from_str(line.trim_end())
        .unwrap_or_else(|e| panic!("stdout not JSON: {line:?}: {e}"))
}

fn send(stdin: &mut impl Write, value: Value) {
    writeln!(stdin, "{value}").unwrap();
    stdin.flush().unwrap();
}

#[test]
fn mcp_session_over_pipes() {
    let mut child = spawn(&["mcp"]);
    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());

    send(
        &mut stdin,
        json!({"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}),
    );
    let init = read_json_line(&mut stdout);
    assert_eq!(init["id"], 1);
    assert_eq!(init["result"]["serverInfo"]["name"], "marigold-lsp");
    send(
        &mut stdin,
        json!({"jsonrpc":"2.0","method":"notifications/initialized"}),
    );

    send(
        &mut stdin,
        json!({"jsonrpc":"2.0","id":"list","method":"tools/list"}),
    );
    let list = read_json_line(&mut stdout);
    assert_eq!(list["id"], "list");
    assert_eq!(list["result"]["tools"].as_array().unwrap().len(), 5);

    send(
        &mut stdin,
        json!({"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"marigold_check","arguments":{"source":"range(Colour).return"}}}),
    );
    let called = read_json_line(&mut stdout);
    assert_eq!(called["id"], 3);
    assert_eq!(called["result"]["isError"], false);
    assert_eq!(
        called["result"]["structuredContent"]["diagnostics"][0]["column"],
        7
    );

    stdin.write_all(b"{broken\n").unwrap();
    stdin.flush().unwrap();
    let bad = read_json_line(&mut stdout);
    assert_eq!(bad["error"]["code"], -32700);

    drop(stdin);
    let mut rest = String::new();
    std::io::Read::read_to_string(&mut stdout, &mut rest).unwrap();
    assert_eq!(rest, "");
    drop(stdout);
    let (status, _, stderr) = finish(child);
    assert!(status.success());
    assert_eq!(stderr, "");
}

#[test]
fn mcp_eof_exits_zero_with_empty_stdout() {
    let mut child = spawn(&["mcp"]);
    drop(child.stdin.take());
    let (status, stdout, _) = finish(child);
    assert_eq!(status.code(), Some(0));
    assert_eq!(stdout, "");
}

#[test]
fn version_and_help_print_to_stdout_and_exit_zero() {
    for flag in ["--version", "-V", "--help", "-h"] {
        let child = spawn(&[flag]);
        let (status, stdout, _) = finish(child);
        assert!(status.success(), "{flag}");
        assert!(stdout.contains("marigold-lsp"), "{flag}: {stdout}");
    }
}

#[test]
fn unknown_arguments_exit_two() {
    let child = spawn(&["bogus"]);
    let (status, stdout, stderr) = finish(child);
    assert_eq!(status.code(), Some(2));
    assert_eq!(stdout, "");
    assert!(stderr.contains("Usage"));
}
