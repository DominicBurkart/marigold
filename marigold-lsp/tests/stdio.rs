use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};
use std::sync::Mutex;
use std::time::Duration;

const HANG_GUARD: Duration = Duration::from_secs(60);

fn frame(body: &str) -> String {
    format!("Content-Length: {}\r\n\r\n{body}", body.len())
}

static LIVE: Mutex<Vec<u32>> = Mutex::new(Vec::new());

fn spawn() -> Child {
    let child = Command::new(env!("CARGO_BIN_EXE_marigold-lsp"))
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

fn stderr_of(child: Child) -> (std::process::ExitStatus, String) {
    let pid = child.id();
    let out = child.wait_with_output().unwrap();
    LIVE.lock().unwrap().retain(|p| *p != pid);
    (
        out.status,
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

const INIT: &str = r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#;
const INITIALIZED: &str = r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#;

fn read_message(reader: &mut impl BufRead) -> serde_json::Value {
    let mut len = None;
    loop {
        let mut line = String::new();
        reader.read_line(&mut line).unwrap();
        let line = line.trim_end();
        if line.is_empty() {
            break;
        }
        let (name, value) = line.split_once(": ").expect("header line");
        assert_eq!(name, "Content-Length", "unexpected stdout: {line}");
        len = Some(value.parse::<usize>().unwrap());
    }
    let mut body = vec![0; len.expect("Content-Length header")];
    reader.read_exact(&mut body).unwrap();
    serde_json::from_slice(&body).unwrap()
}

#[test]
fn binary_speaks_only_lsp_on_stdout() {
    let mut child = spawn();
    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());

    for body in [
        r#"{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}"#,
        r#"{"jsonrpc":"2.0","method":"initialized","params":{}}"#,
        r#"{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///x.marigold","languageId":"marigold","version":1,"text":"range(0, 1).retur"}}}"#,
    ] {
        stdin.write_all(frame(body).as_bytes()).unwrap();
    }
    stdin.flush().unwrap();

    let init = read_message(&mut stdout);
    assert_eq!(init["id"], 1);
    assert_eq!(init["result"]["serverInfo"]["name"], "marigold-lsp");

    let published = read_message(&mut stdout);
    assert_eq!(published["method"], "textDocument/publishDiagnostics");
    assert_eq!(
        published["params"]["diagnostics"][0]["code"],
        "syntax-error"
    );

    for body in [
        r#"{"jsonrpc":"2.0","id":2,"method":"shutdown"}"#,
        r#"{"jsonrpc":"2.0","method":"exit"}"#,
    ] {
        stdin.write_all(frame(body).as_bytes()).unwrap();
    }
    stdin.flush().unwrap();
    assert_eq!(read_message(&mut stdout)["id"], 2);
    drop(stdin);
    drop(stdout);
    let (status, stderr) = stderr_of(child);
    assert!(status.success());
    assert_eq!(stderr, "");
}

#[test]
fn eof_without_shutdown_exits_cleanly() {
    let mut child = spawn();
    let mut stdin = child.stdin.take().unwrap();
    for body in [INIT, INITIALIZED] {
        stdin.write_all(frame(body).as_bytes()).unwrap();
    }
    drop(stdin);
    let (status, _) = stderr_of(child);
    assert!(status.success());
}

#[test]
fn exit_without_shutdown_exits_nonzero() {
    let mut child = spawn();
    let mut stdin = child.stdin.take().unwrap();
    for body in [INIT, INITIALIZED, r#"{"jsonrpc":"2.0","method":"exit"}"#] {
        stdin.write_all(frame(body).as_bytes()).unwrap();
    }
    drop(stdin);
    let (status, stderr) = stderr_of(child);
    assert_eq!(status.code(), Some(1));
    assert!(stderr.contains("shutdown"), "{stderr}");
}

#[test]
fn bad_content_length_exits_nonzero_without_hanging() {
    let mut child = spawn();
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(b"Content-Length: abc\r\n\r\n{}").unwrap();
    drop(stdin);
    let (status, _) = stderr_of(child);
    assert_eq!(status.code(), Some(1));
}

#[test]
fn bad_content_length_after_initialize_exits_nonzero() {
    let mut child = spawn();
    let mut stdin = child.stdin.take().unwrap();
    for body in [INIT, INITIALIZED] {
        stdin.write_all(frame(body).as_bytes()).unwrap();
    }
    stdin.write_all(b"Content-Length: -5\r\n\r\n").unwrap();
    drop(stdin);
    let (status, _) = stderr_of(child);
    assert_eq!(status.code(), Some(1));
}
