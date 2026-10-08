mod common;

use common::{pos_of, uri};
use lsp_server::{Connection, Message, Notification, Request, RequestId};
use lsp_types::notification::{DidOpenTextDocument, Notification as _, PublishDiagnostics};
use lsp_types::request::{HoverRequest, Request as _};
use serde_json::json;

const TEXT: &str = "x = range(0, 3)\nx.return\n";

fn probe(method: &str) {
    if method == "textDocument/hover" || method == "textDocument/rename" {
        panic!("injected panic for {method}");
    }
    if method == "textDocument/didChange" {
        panic!("injected panic for {method}");
    }
}

struct Peer {
    conn: Connection,
    next: i32,
    handle: Option<std::thread::JoinHandle<()>>,
}

impl Peer {
    fn start() -> Self {
        let (server, conn) = Connection::memory();
        let handle = std::thread::spawn(move || {
            marigold_lsp::serve_with_probe(&server, probe).unwrap();
        });
        let mut peer = Peer {
            conn,
            next: 0,
            handle: Some(handle),
        };
        peer.call("initialize", json!({"capabilities": {}}));
        peer.notify("initialized", json!({}));
        peer
    }

    fn call(&mut self, method: &str, params: serde_json::Value) -> lsp_server::Response {
        self.next += 1;
        let id = RequestId::from(self.next);
        self.conn
            .sender
            .send(Message::Request(Request::new(
                id.clone(),
                method.into(),
                params,
            )))
            .unwrap();
        loop {
            if let Message::Response(r) = self.conn.receiver.recv().unwrap() {
                if r.id == id {
                    return r;
                }
            }
        }
    }

    fn notify(&self, method: &str, params: serde_json::Value) {
        self.conn
            .sender
            .send(Message::Notification(Notification::new(
                method.into(),
                params,
            )))
            .unwrap();
    }

    fn open(&self, u: &lsp_types::Uri, text: &str) {
        self.notify(
            DidOpenTextDocument::METHOD,
            json!({"textDocument": {"uri": u, "languageId": "marigold", "version": 1, "text": text}}),
        );
    }

    fn finish(mut self) {
        self.call("shutdown", serde_json::Value::Null);
        self.notify("exit", serde_json::Value::Null);
        self.handle.take().unwrap().join().unwrap();
    }
}

fn hover_params(u: &lsp_types::Uri) -> serde_json::Value {
    let at = pos_of(TEXT, "x = ");
    json!({"textDocument": {"uri": u}, "position": at})
}

#[test]
fn panicking_request_returns_internal_error_and_server_keeps_serving() {
    let mut peer = Peer::start();
    let u = uri("panic_req");
    peer.open(&u, TEXT);
    let failed = peer.call(HoverRequest::METHOD, hover_params(&u));
    assert_eq!(
        failed.response_result.unwrap_err().code,
        lsp_server::ErrorCode::InternalError as i32
    );
    let ok = peer.call(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": u}}),
    );
    assert!(ok.response_result.is_ok());
    let again = peer.call(HoverRequest::METHOD, hover_params(&u));
    assert!(again.response_result.is_err());
    peer.finish();
}

#[test]
fn panicking_rename_returns_internal_error() {
    let mut peer = Peer::start();
    let u = uri("panic_rename");
    peer.open(&u, TEXT);
    let mut params = hover_params(&u);
    params["newName"] = json!("y");
    let failed = peer.call("textDocument/rename", params);
    assert_eq!(
        failed.response_result.unwrap_err().code,
        lsp_server::ErrorCode::InternalError as i32
    );
    peer.finish();
}

#[test]
fn panicking_did_change_is_survived() {
    let mut peer = Peer::start();
    let u = uri("panic_change");
    peer.open(&u, TEXT);
    loop {
        if let Message::Notification(n) = peer.conn.receiver.recv().unwrap() {
            if n.method == PublishDiagnostics::METHOD {
                break;
            }
        }
    }
    peer.notify(
        "textDocument/didChange",
        json!({"textDocument": {"uri": u, "version": 2}, "contentChanges": [{"text": "range(Colour).return"}]}),
    );
    let ok = peer.call(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": u}}),
    );
    assert!(ok.response_result.is_ok());
    peer.finish();
}
