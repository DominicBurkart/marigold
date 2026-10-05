#![allow(dead_code)]

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{DidOpenTextDocument, Exit, Initialized};
use lsp_types::request::{Initialize, Shutdown};
use lsp_types::{
    DidOpenTextDocumentParams, InitializeParams, InitializeResult, Position, TextDocumentItem, Uri,
};
use std::str::FromStr;
use std::thread;
use std::time::Duration;

pub const HANG_GUARD: Duration = Duration::from_secs(60);

pub struct Client {
    conn: Connection,
    server: Option<thread::JoinHandle<()>>,
    next_id: i32,
}

impl Client {
    pub fn start() -> (Self, InitializeResult) {
        Self::start_with(InitializeParams::default())
    }

    pub fn start_with(params: InitializeParams) -> (Self, InitializeResult) {
        let (server_conn, conn) = Connection::memory();
        let server = thread::spawn(move || marigold_lsp::serve(&server_conn).unwrap());
        let mut client = Client {
            conn,
            server: Some(server),
            next_id: 0,
        };
        let result = client
            .call::<Initialize>(params)
            .response_result
            .expect("initialize failed");
        client.notify::<Initialized>(lsp_types::InitializedParams {});
        (client, serde_json::from_value(result).unwrap())
    }

    #[cfg(feature = "telemetry")]
    pub fn start_telemetry(
        source: Option<Box<dyn marigold_lsp::telemetry::TelemetrySource + Send>>,
    ) -> (Self, InitializeResult) {
        let (server_conn, conn) = Connection::memory();
        let server = thread::spawn(move || {
            marigold_lsp::serve_with_telemetry(&server_conn, source).unwrap()
        });
        let mut client = Client {
            conn,
            server: Some(server),
            next_id: 0,
        };
        let result = client
            .call::<Initialize>(InitializeParams::default())
            .response_result
            .expect("initialize failed");
        client.notify::<Initialized>(lsp_types::InitializedParams {});
        (client, serde_json::from_value(result).unwrap())
    }

    pub fn call<R: lsp_types::request::Request>(&mut self, params: R::Params) -> Response {
        self.call_raw(R::METHOD, serde_json::to_value(params).unwrap())
    }

    pub fn call_raw(&mut self, method: &str, params: serde_json::Value) -> Response {
        self.next_id += 1;
        let id = RequestId::from(self.next_id);
        self.conn
            .sender
            .send(Message::Request(Request::new(
                id.clone(),
                method.to_string(),
                params,
            )))
            .unwrap();
        loop {
            if let Message::Response(r) = self.recv() {
                if r.id == id {
                    return r;
                }
            }
        }
    }

    pub fn request<R: lsp_types::request::Request>(
        &mut self,
        params: R::Params,
    ) -> serde_json::Value {
        self.call::<R>(params)
            .response_result
            .expect("error response")
    }

    pub fn notify<N: lsp_types::notification::Notification>(&self, params: N::Params) {
        self.conn
            .sender
            .send(Message::Notification(Notification::new(
                N::METHOD.to_string(),
                params,
            )))
            .unwrap();
    }

    pub fn recv(&self) -> Message {
        self.conn
            .receiver
            .recv_timeout(HANG_GUARD)
            .expect("server did not respond")
    }

    pub fn open_at(&self, uri: &Uri, text: &str) {
        self.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: TextDocumentItem::new(uri.clone(), "marigold".into(), 1, text.into()),
        });
    }

    pub fn shutdown(mut self) {
        self.request::<Shutdown>(());
        self.notify::<Exit>(());
        self.server.take().unwrap().join().unwrap();
    }
}

pub fn uri(name: &str) -> Uri {
    Uri::from_str(&format!("file:///tmp/{name}.marigold")).unwrap()
}

pub fn pos_of(text: &str, needle: &str) -> Position {
    let byte = text.find(needle).expect("needle not found");
    pos_at(text, byte)
}

pub fn pos_at(text: &str, byte: usize) -> Position {
    let before = &text[..byte];
    let line = before.matches('\n').count();
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let character = text[line_start..byte].encode_utf16().count();
    Position::new(line as u32, character as u32)
}
