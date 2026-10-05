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
        self.server.take().unwrap().join().unwrap();
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
