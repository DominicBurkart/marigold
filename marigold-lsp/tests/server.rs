use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Exit, Initialized,
    Notification as _, PublishDiagnostics,
};
use lsp_types::request::{Initialize, Shutdown};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, NumberOrString, Position, PublishDiagnosticsParams,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, Uri, VersionedTextDocumentIdentifier,
};
use std::str::FromStr;
use std::thread;
use std::time::Duration;

const JOIN_LIMIT: Duration = Duration::from_secs(10);

fn join_within<T>(handle: thread::JoinHandle<T>, limit: Duration) -> thread::Result<T> {
    let start = std::time::Instant::now();
    while !handle.is_finished() {
        assert!(
            start.elapsed() < limit,
            "thread did not finish within {limit:?}"
        );
        thread::sleep(Duration::from_millis(5));
    }
    handle.join()
}

#[test]
#[should_panic(expected = "did not finish within")]
fn bounded_join_fails_instead_of_hanging_on_a_stuck_thread() {
    let (tx, rx) = std::sync::mpsc::channel::<()>();
    let handle = thread::spawn(move || {
        let _ = rx.recv();
    });
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        join_within(handle, Duration::from_millis(100))
    }));
    drop(tx);
    if let Err(panic) = result {
        std::panic::resume_unwind(panic);
    }
}

struct Client {
    conn: Connection,
    server: Option<thread::JoinHandle<()>>,
    next_id: i32,
}

impl Client {
    fn start() -> (Self, InitializeResult) {
        let (server_conn, conn) = Connection::memory();
        let server = thread::spawn(move || marigold_lsp::serve(&server_conn).unwrap());
        let mut client = Client {
            conn,
            server: Some(server),
            next_id: 0,
        };
        let result = client.request::<Initialize>(InitializeParams::default());
        client.notify::<Initialized>(lsp_types::InitializedParams {});
        (client, serde_json::from_value(result).unwrap())
    }

    fn request<R: lsp_types::request::Request>(&mut self, params: R::Params) -> serde_json::Value {
        self.next_id += 1;
        let id = RequestId::from(self.next_id);
        self.conn
            .sender
            .send(Message::Request(Request::new(
                id.clone(),
                R::METHOD.to_string(),
                params,
            )))
            .unwrap();
        loop {
            match self.recv() {
                Message::Response(r) if r.id == id => {
                    return r.response_result.unwrap();
                }
                _ => {}
            }
        }
    }

    fn notify<N: lsp_types::notification::Notification>(&self, params: N::Params) {
        self.conn
            .sender
            .send(Message::Notification(Notification::new(
                N::METHOD.to_string(),
                params,
            )))
            .unwrap();
    }

    fn recv(&self) -> Message {
        self.conn
            .receiver
            .recv_timeout(Duration::from_secs(10))
            .expect("server did not respond")
    }

    fn diagnostics(&self) -> PublishDiagnosticsParams {
        loop {
            if let Message::Notification(n) = self.recv() {
                if n.method == PublishDiagnostics::METHOD {
                    return serde_json::from_value(n.params).unwrap();
                }
            }
        }
    }

    fn shutdown(mut self) {
        self.request::<Shutdown>(());
        self.notify::<Exit>(());
        join_within(self.server.take().unwrap(), JOIN_LIMIT).unwrap();
    }
}

fn uri() -> Uri {
    Uri::from_str("file:///tmp/example.marigold").unwrap()
}

fn open(client: &Client, text: &str) {
    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem::new(uri(), "marigold".into(), 1, text.into()),
    });
}

#[test]
fn advertises_full_document_sync() {
    let (client, init) = Client::start();
    assert_eq!(
        init.capabilities.text_document_sync,
        Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL))
    );
    assert_eq!(init.server_info.unwrap().name, "marigold-lsp");
    client.shutdown();
}

#[test]
fn publishes_diagnostics_on_open() {
    let (client, _) = Client::start();
    open(&client, "range(0, 1).return\nrange(Colour).return\n");
    let published = client.diagnostics();
    assert_eq!(published.uri, uri());
    assert_eq!(published.version, Some(1));
    let [d] = published.diagnostics.as_slice() else {
        panic!("{:?}", published.diagnostics)
    };
    assert_eq!(d.range.start, Position::new(1, 6));
    assert_eq!(d.range.end, Position::new(1, 12));
    assert_eq!(
        d.code,
        Some(NumberOrString::String("undefined-enum".into()))
    );
    assert_eq!(d.source.as_deref(), Some("marigold"));
    assert_eq!(d.severity, Some(lsp_types::DiagnosticSeverity::ERROR));
    assert!(d.message.contains("no enums are declared"), "{}", d.message);
    client.shutdown();
}

#[test]
fn undefined_stream_variable_is_published_as_warning() {
    let (client, _) = Client::start();
    open(&client, "x = range(0, 3)\nxs.return\n");
    let published = client.diagnostics();
    let [d] = published.diagnostics.as_slice() else {
        panic!("{:?}", published.diagnostics)
    };
    assert_eq!(
        d.code,
        Some(NumberOrString::String("undefined-stream-variable".into()))
    );
    assert_eq!(d.severity, Some(lsp_types::DiagnosticSeverity::WARNING));
    client.shutdown();
}

#[test]
fn undefined_fn_is_published_as_information() {
    let (client, _) = Client::start();
    open(&client, "range(0, 3).map(f).return\n");
    let published = client.diagnostics();
    let [d] = published.diagnostics.as_slice() else {
        panic!("{:?}", published.diagnostics)
    };
    assert_eq!(d.code, Some(NumberOrString::String("undefined-fn".into())));
    assert_eq!(d.severity, Some(lsp_types::DiagnosticSeverity::INFORMATION));
    assert!(
        d.message.contains("Rust item in scope inside m!()"),
        "{}",
        d.message
    );
    client.shutdown();
}

#[test]
fn republishes_on_change_and_clears_when_fixed() {
    let (client, _) = Client::start();
    open(&client, "range(0, 1).retur");
    assert_eq!(client.diagnostics().diagnostics.len(), 1);
    client.notify::<DidChangeTextDocument>(DidChangeTextDocumentParams {
        text_document: VersionedTextDocumentIdentifier::new(uri(), 2),
        content_changes: vec![TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            text: "range(0, 1).return".into(),
        }],
    });
    let published = client.diagnostics();
    assert_eq!(published.version, Some(2));
    assert!(published.diagnostics.is_empty());
    client.shutdown();
}

#[test]
fn clears_diagnostics_on_close() {
    let (client, _) = Client::start();
    open(&client, "range(0, 1).retur");
    assert_eq!(client.diagnostics().diagnostics.len(), 1);
    client.notify::<DidCloseTextDocument>(DidCloseTextDocumentParams {
        text_document: TextDocumentIdentifier::new(uri()),
    });
    assert!(client.diagnostics().diagnostics.is_empty());
    client.shutdown();
}

#[test]
fn unknown_requests_get_method_not_found() {
    let (client, _) = Client::start();
    client
        .conn
        .sender
        .send(Message::Request(Request::new(
            RequestId::from(99),
            "marigold/unknown".into(),
            serde_json::Value::Null,
        )))
        .unwrap();
    loop {
        if let Message::Response(r) = client.recv() {
            assert_eq!(r.id, RequestId::from(99));
            assert_eq!(
                r.response_result.unwrap_err().code,
                lsp_server::ErrorCode::MethodNotFound as i32
            );
            break;
        }
    }
    client.shutdown();
}

fn raw_notify(client: &Client, method: &str, params: serde_json::Value) {
    client
        .conn
        .sender
        .send(Message::Notification(Notification::new(
            method.to_string(),
            params,
        )))
        .unwrap();
}

fn change(client: &Client, version: i32, texts: &[&str]) {
    client.notify::<DidChangeTextDocument>(DidChangeTextDocumentParams {
        text_document: VersionedTextDocumentIdentifier::new(uri(), version),
        content_changes: texts
            .iter()
            .map(|t| TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: (*t).into(),
            })
            .collect(),
    });
}

fn start_raw() -> (
    Connection,
    thread::JoinHandle<Result<(), marigold_lsp::Error>>,
) {
    let (server_conn, conn) = Connection::memory();
    let server = thread::spawn(move || marigold_lsp::serve(&server_conn));
    (conn, server)
}

#[test]
fn malformed_did_open_params_do_not_kill_server() {
    let (client, _) = Client::start();
    raw_notify(
        &client,
        "textDocument/didOpen",
        serde_json::json!({"bogus": 1}),
    );
    raw_notify(&client, "textDocument/didOpen", serde_json::Value::Null);
    raw_notify(&client, "textDocument/didClose", serde_json::json!(42));
    open(&client, "range(0, 1).retur");
    assert_eq!(client.diagnostics().diagnostics.len(), 1);
    client.shutdown();
}

#[test]
fn bad_did_change_does_not_kill_server() {
    let (client, _) = Client::start();
    raw_notify(
        &client,
        "textDocument/didChange",
        serde_json::json!({"textDocument": {"uri": 5}, "contentChanges": "x"}),
    );
    open(&client, "range(0, 1).retur");
    let published = client.diagnostics();
    assert_eq!(published.diagnostics.len(), 1);
    client.shutdown();
}

#[test]
fn exit_without_shutdown_returns_error() {
    let (conn, server) = start_raw();
    conn.sender
        .send(Message::Request(Request::new(
            RequestId::from(1),
            "initialize".into(),
            InitializeParams::default(),
        )))
        .unwrap();
    conn.receiver.recv_timeout(Duration::from_secs(10)).unwrap();
    conn.sender
        .send(Message::Notification(Notification::new(
            "initialized".into(),
            serde_json::json!({}),
        )))
        .unwrap();
    conn.sender
        .send(Message::Notification(Notification::new(
            "exit".into(),
            serde_json::Value::Null,
        )))
        .unwrap();
    assert!(join_within(server, JOIN_LIMIT).unwrap().is_err());
}

#[test]
fn exit_after_shutdown_returns_ok() {
    let (mut client, _) = Client::start();
    client.request::<Shutdown>(());
    client.notify::<Exit>(());
    assert!(join_within(client.server.take().unwrap(), JOIN_LIMIT).is_ok());
}

#[test]
fn shutdown_then_channel_close_returns_ok() {
    let (conn, server) = start_raw();
    let mut client = Client {
        conn,
        server: None,
        next_id: 0,
    };
    client.request::<Initialize>(InitializeParams::default());
    client.notify::<Initialized>(lsp_types::InitializedParams {});
    client.request::<Shutdown>(());
    drop(client);
    assert!(join_within(server, JOIN_LIMIT).unwrap().is_ok());
}

#[test]
fn request_before_initialize_does_not_panic() {
    let (conn, server) = start_raw();
    conn.sender
        .send(Message::Request(Request::new(
            RequestId::from(1),
            "textDocument/hover".into(),
            serde_json::Value::Null,
        )))
        .unwrap();
    match conn.receiver.recv_timeout(Duration::from_secs(10)).unwrap() {
        Message::Response(r) => assert_eq!(r.id, RequestId::from(1)),
        other => panic!("{other:?}"),
    }
    drop(conn);
    assert!(join_within(server, JOIN_LIMIT).unwrap().is_err());
}

#[test]
fn requests_after_shutdown_are_rejected() {
    let (mut client, _) = Client::start();
    client.request::<Shutdown>(());
    client
        .conn
        .sender
        .send(Message::Request(Request::new(
            RequestId::from(77),
            "marigold/unknown".into(),
            serde_json::Value::Null,
        )))
        .unwrap();
    match client.recv() {
        Message::Response(r) => {
            assert_eq!(r.id, RequestId::from(77));
            assert_eq!(
                r.response_result.unwrap_err().code,
                lsp_server::ErrorCode::InvalidRequest as i32
            );
        }
        other => panic!("{other:?}"),
    }
    client.notify::<Exit>(());
    join_within(client.server.take().unwrap(), JOIN_LIMIT).unwrap();
}

#[test]
fn did_change_with_no_content_changes_publishes_nothing() {
    let (client, _) = Client::start();
    change(&client, 2, &[]);
    open(&client, "range(0, 1).retur");
    let published = client.diagnostics();
    assert_eq!(published.version, Some(1));
    client.shutdown();
}

#[test]
fn last_of_multiple_changes_wins_and_version_propagates() {
    let (client, _) = Client::start();
    change(&client, 7, &["range(0, 1).retur", "range(0, 1).return"]);
    let published = client.diagnostics();
    assert_eq!(published.version, Some(7));
    assert!(published.diagnostics.is_empty());
    change(&client, 8, &["range(0, 1).return", "range(0, 1).retur"]);
    let published = client.diagnostics();
    assert_eq!(published.version, Some(8));
    assert_eq!(published.diagnostics.len(), 1);
    client.shutdown();
}

#[test]
fn response_messages_are_ignored() {
    let (client, _) = Client::start();
    client
        .conn
        .sender
        .send(Message::Response(lsp_server::Response::new_ok(
            RequestId::from(1234),
            (),
        )))
        .unwrap();
    open(&client, "range(0, 1).retur");
    assert_eq!(client.diagnostics().diagnostics.len(), 1);
    client.shutdown();
}

#[test]
fn rapid_changes_publish_in_order() {
    let (client, _) = Client::start();
    for v in 1..=1000 {
        change(&client, v, &["range(0, 1).retur"]);
    }
    for v in 1..=1000 {
        assert_eq!(client.diagnostics().version, Some(v));
    }
    client.shutdown();
}

#[test]
fn document_over_the_size_cap_is_rejected_through_did_open() {
    let (client, _) = Client::start();
    let text = " ".repeat(marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES + 1);
    open(&client, &text);
    let published = client.diagnostics();
    assert_eq!(published.diagnostics.len(), 1);
    let d = &published.diagnostics[0];
    assert_eq!(
        d.code,
        Some(lsp_types::NumberOrString::String("input-too-large".into()))
    );
    assert_eq!(d.severity, Some(lsp_types::DiagnosticSeverity::ERROR));
    assert_eq!(d.range.start, lsp_types::Position::new(0, 0));
    client.shutdown();
}

#[test]
fn document_at_the_size_cap_is_still_checked_through_did_open() {
    let (client, _) = Client::start();
    let text = " ".repeat(marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES);
    open(&client, &text);
    assert!(client.diagnostics().diagnostics.is_empty());
    client.shutdown();
}

#[test]
fn large_document_scales_linearly() {
    let line = "range(0, 1).return\n";
    let analyse = |bytes: usize| {
        let text = line.repeat(bytes / line.len());
        (0..3)
            .map(|_| {
                let (client, _) = Client::start();
                let start = std::time::Instant::now();
                open(&client, &text);
                client.diagnostics();
                let elapsed = start.elapsed();
                client.shutdown();
                elapsed
            })
            .min()
            .unwrap()
    };
    let t_small = analyse(250_000);
    let t_large = analyse(500_000);
    let allowed = t_small.mul_f64(3.5) + Duration::from_millis(200);
    assert!(
        t_large < allowed,
        "t(250k)={t_small:?} t(500k)={t_large:?} allowed={allowed:?}"
    );
    assert!(t_large < Duration::from_secs(60), "{t_large:?}");
}

#[test]
fn help_text_is_formatted_into_message() {
    let (client, _) = Client::start();
    let mut found = None;
    for src in [
        "range(Colour).return",
        "range(0, 1).retur",
        "range(0, 1).frobnicate().return",
        "enum A { X }\nrange(B).return",
    ] {
        open(&client, src);
        for d in client.diagnostics().diagnostics {
            if d.message.contains("\nhelp: ") {
                found = Some(d.message);
            }
        }
    }
    let message = found.expect("no diagnostic carried help text");
    let (head, help) = message.split_once("\nhelp: ").unwrap();
    assert!(!head.is_empty() && !help.is_empty());
    client.shutdown();
}
