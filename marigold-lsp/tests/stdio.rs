use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

fn frame(body: &str) -> String {
    format!("Content-Length: {}\r\n\r\n{body}", body.len())
}

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
    let mut child = Command::new(env!("CARGO_BIN_EXE_marigold-lsp"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
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
    assert!(child.wait().unwrap().success());
}
