#![cfg(feature = "datadog")]

use serde_json::{json, Value};
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpListener;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc::{channel, Receiver};
use std::sync::Mutex;
use std::time::Duration;

const API: &str = "SENTINEL_API_KEY_8f3a1c";
const APP: &str = "SENTINEL_APP_KEY_41d9b7";
const BODY: &str = "SENTINEL_BODY_c07e52";
const SITE: &str = "marigold-redaction-test.invalid";
const SRC: &str = "x = range(0, 5).map(f)\nx.return";

static LIVE: Mutex<Vec<u32>> = Mutex::new(Vec::new());

#[derive(Clone, Copy, Debug)]
enum Mode {
    ProxyRefuses,
    GarbageAfterConnect,
    NotHttp,
    HangUp,
}

const MODES: [Mode; 4] = [
    Mode::ProxyRefuses,
    Mode::GarbageAfterConnect,
    Mode::NotHttp,
    Mode::HangUp,
];

struct Proxy {
    url: String,
    seen: Receiver<String>,
}

fn proxy(mode: Mode) -> Proxy {
    let listener = TcpListener::bind("127.0.0.1:0").unwrap();
    let url = format!("http://127.0.0.1:{}", listener.local_addr().unwrap().port());
    let (tx, seen) = channel();
    std::thread::spawn(move || {
        for stream in listener.incoming() {
            let Ok(mut stream) = stream else { return };
            let mut head = Vec::new();
            let mut byte = [0u8; 1];
            while !head.ends_with(b"\r\n\r\n") {
                match stream.read(&mut byte) {
                    Ok(1) => head.push(byte[0]),
                    _ => break,
                }
            }
            let reply = match mode {
                Mode::ProxyRefuses => format!(
                    "HTTP/1.1 500 Oops\r\nContent-Length: {}\r\n\r\n{API} {APP} {BODY}",
                    API.len() + APP.len() + BODY.len() + 2
                ),
                Mode::GarbageAfterConnect => {
                    format!(
                        "HTTP/1.1 200 Connection established\r\n\r\n{API} {APP} {BODY}\u{0}\u{1}"
                    )
                }
                Mode::NotHttp => format!("{API}{APP}{BODY} not http at all\r\n"),
                Mode::HangUp => String::new(),
            };
            let _ = stream.write_all(reply.as_bytes());
            let _ = stream.flush();
            let _ = tx.send(String::from_utf8_lossy(&head).into_owned());
        }
    });
    Proxy { url, seen }
}

fn spawn(args: &[&str], env: &[(&str, &str)]) -> Child {
    let mut command = Command::new(env!("CARGO_BIN_EXE_marigold-lsp"));
    command
        .args(args)
        .env_clear()
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    for (k, v) in env {
        command.env(k, v);
    }
    let child = command.spawn().unwrap();
    let pid = child.id();
    LIVE.lock().unwrap().push(pid);
    std::thread::spawn(move || {
        std::thread::sleep(Duration::from_secs(60));
        if LIVE.lock().unwrap().contains(&pid) {
            let _ = Command::new("kill").arg("-9").arg(pid.to_string()).status();
        }
    });
    child
}

fn env_for(proxy_url: &str, site: &'static str) -> Vec<(&'static str, String)> {
    let mut env = vec![
        ("MARIGOLD_TELEMETRY", "datadog".to_string()),
        ("DD_API_KEY", API.to_string()),
        ("DD_APP_KEY", APP.to_string()),
        ("DD_SITE", site.to_string()),
        ("MARIGOLD_TELEMETRY_ALLOW_CUSTOM_SITE", "1".to_string()),
        ("HTTPS_PROXY", proxy_url.to_string()),
        ("https_proxy", proxy_url.to_string()),
        ("ALL_PROXY", proxy_url.to_string()),
    ];
    env.retain(|(_, v)| !v.is_empty());
    env
}

fn borrowed<'a>(env: &'a [(&'static str, String)]) -> Vec<(&'a str, &'a str)> {
    env.iter().map(|(k, v)| (*k, v.as_str())).collect()
}

fn assert_clean(label: &str, text: &str) {
    for secret in [API, APP, BODY] {
        assert!(!text.contains(secret), "{label} leaked {secret}: {text}");
    }
}

fn finish(child: Child) -> (std::process::ExitStatus, String) {
    let pid = child.id();
    let out = child.wait_with_output().unwrap();
    LIVE.lock().unwrap().retain(|p| *p != pid);
    (
        out.status,
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

fn read_line_json(reader: &mut impl BufRead) -> Option<Value> {
    let mut line = String::new();
    if reader.read_line(&mut line).unwrap() == 0 {
        return None;
    }
    Some(serde_json::from_str(line.trim_end()).unwrap())
}

fn read_framed(reader: &mut impl BufRead) -> Option<Value> {
    let mut len = None;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).unwrap() == 0 {
            return None;
        }
        let line = line.trim_end();
        if line.is_empty() {
            break;
        }
        if let Some(n) = line.strip_prefix("Content-Length: ") {
            len = n.parse::<usize>().ok();
        }
    }
    let mut body = vec![0u8; len.unwrap()];
    reader.read_exact(&mut body).unwrap();
    Some(serde_json::from_slice(&body).unwrap())
}

fn framed(value: Value) -> String {
    let body = value.to_string();
    format!("Content-Length: {}\r\n\r\n{body}", body.len())
}

fn mcp_session(env: &[(&str, &str)], path: &str, calls_tool: bool) -> (Vec<Value>, String) {
    let mut child = spawn(&["mcp"], env);
    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = BufReader::new(child.stdout.take().unwrap());
    let mut send = |v: Value| {
        writeln!(stdin, "{v}").unwrap();
        stdin.flush().unwrap();
    };
    let mut seen = Vec::new();
    send(
        json!({"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"t","version":"0"}}}),
    );
    seen.push(read_line_json(&mut stdout).unwrap());
    send(json!({"jsonrpc":"2.0","method":"notifications/initialized"}));
    send(json!({"jsonrpc":"2.0","id":2,"method":"tools/list"}));
    seen.push(read_line_json(&mut stdout).unwrap());
    if calls_tool {
        send(
            json!({"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"marigold_telemetry","arguments":{"path":path}}}),
        );
        seen.push(read_line_json(&mut stdout).unwrap());
    }
    drop(send);
    drop(stdin);
    while let Some(v) = read_line_json(&mut stdout) {
        seen.push(v);
    }
    let (status, stderr) = finish(child);
    assert!(status.success(), "{status:?} {stderr}");
    (seen, stderr)
}

fn temp_program() -> String {
    let dir = std::env::temp_dir().join(format!("marigold-redaction-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("redaction.marigold");
    std::fs::write(&path, SRC).unwrap();
    path.to_string_lossy().into_owned()
}

#[test]
fn mcp_tool_failures_never_echo_keys_or_backend_bodies() {
    let path = temp_program();
    for mode in MODES {
        let proxy = proxy(mode);
        let env = env_for(&proxy.url, SITE);
        let (seen, stderr) = mcp_session(&borrowed(&env), &path, true);
        let all = serde_json::to_string(&seen).unwrap();
        assert_clean(&format!("{mode:?} stdout"), &all);
        assert_clean(&format!("{mode:?} stderr"), &stderr);
        let call = &seen[2];
        assert_eq!(call["result"]["isError"], true, "{mode:?} {call}");
        let head = proxy.seen.recv_timeout(Duration::from_secs(30)).unwrap();
        assert!(
            head.starts_with(&format!("CONNECT api.{SITE}:443")),
            "{mode:?} {head}"
        );
        assert_clean(&format!("{mode:?} proxy request"), &head);
    }
}

#[test]
fn lsp_failures_never_echo_keys_or_backend_bodies() {
    for mode in MODES {
        let proxy = proxy(mode);
        let env = env_for(&proxy.url, SITE);
        let mut child = spawn(&[], &borrowed(&env));
        let mut stdin = child.stdin.take().unwrap();
        let mut stdout = BufReader::new(child.stdout.take().unwrap());
        let mut messages = Vec::new();
        let mut send = |v: Value| {
            stdin.write_all(framed(v).as_bytes()).unwrap();
            stdin.flush().unwrap();
        };
        let uri = "file:///tmp/redaction.marigold";
        send(
            json!({"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{"workspace":{"inlayHint":{"refreshSupport":true}}}}}),
        );
        messages.push(read_framed(&mut stdout).unwrap());
        send(json!({"jsonrpc":"2.0","method":"initialized","params":{}}));
        send(
            json!({"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":uri,"languageId":"marigold","version":1,"text":SRC}}}),
        );
        send(
            json!({"jsonrpc":"2.0","id":2,"method":"textDocument/hover","params":{"textDocument":{"uri":uri},"position":{"line":0,"character":17}}}),
        );
        send(
            json!({"jsonrpc":"2.0","id":3,"method":"textDocument/inlayHint","params":{"textDocument":{"uri":uri},"range":{"start":{"line":0,"character":0},"end":{"line":9,"character":0}}}}),
        );
        let mut answered = 0;
        while answered < 2 {
            let message = read_framed(&mut stdout).unwrap();
            if message["id"] == 2 || message["id"] == 3 {
                answered += 1;
            }
            messages.push(message);
        }
        let head = proxy.seen.recv_timeout(Duration::from_secs(30)).unwrap();
        assert!(
            head.starts_with(&format!("CONNECT api.{SITE}:443")),
            "{head}"
        );
        send(json!({"jsonrpc":"2.0","id":4,"method":"shutdown"}));
        send(json!({"jsonrpc":"2.0","method":"exit"}));
        drop(send);
        drop(stdin);
        while let Some(message) = read_framed(&mut stdout) {
            messages.push(message);
        }
        let (status, stderr) = finish(child);
        assert!(status.success(), "{mode:?} {status:?} {stderr}");
        let all = serde_json::to_string(&messages).unwrap();
        assert_clean(&format!("{mode:?} lsp stdout"), &all);
        assert_clean(&format!("{mode:?} lsp stderr"), &stderr);
        assert!(all.contains("cardinality:"), "{all}");
        assert!(!all.contains("observed:"));
    }
}

#[test]
fn refused_sites_and_bad_keys_report_without_keys_and_send_nothing() {
    let proxy = proxy(Mode::HangUp);
    let cases: [(&str, &str, &str, &str); 3] = [
        ("evil.com", API, APP, ""),
        ("evil.com/#.datadoghq.com", API, APP, "1"),
        ("datadoghq.com", "SENTINEL_API_KEY_8f3a1c\nline2", APP, ""),
    ];
    let path = temp_program();
    for (site, api, app, custom) in cases {
        let mut env = env_for(&proxy.url, "x");
        env.retain(|(k, _)| {
            !matches!(
                *k,
                "DD_SITE" | "DD_API_KEY" | "DD_APP_KEY" | "MARIGOLD_TELEMETRY_ALLOW_CUSTOM_SITE"
            )
        });
        env.push(("DD_SITE", site.to_string()));
        env.push(("DD_API_KEY", api.to_string()));
        env.push(("DD_APP_KEY", app.to_string()));
        if !custom.is_empty() {
            env.push(("MARIGOLD_TELEMETRY_ALLOW_CUSTOM_SITE", custom.to_string()));
        }
        let (seen, stderr) = mcp_session(&borrowed(&env), &path, false);
        let all = serde_json::to_string(&seen).unwrap();
        assert_clean(&format!("{site} stdout"), &all);
        assert_clean(&format!("{site} stderr"), &stderr);
        assert!(stderr.contains("telemetry disabled"), "{site}: {stderr}");
        assert!(!all.contains("marigold_telemetry"), "{site}");
        assert!(
            proxy.seen.try_recv().is_err(),
            "{site} contacted the network"
        );
    }
}
