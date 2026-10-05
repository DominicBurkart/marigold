#![forbid(unsafe_code)]

//! Language server for the Marigold DSL.
//!
//! The server speaks LSP over any [`lsp_server::Connection`] and publishes
//! [`marigold_grammar::marigold_check`] diagnostics whenever a document is
//! opened or changed.
//!
//! ```
//! let diags = marigold_lsp::lsp_diagnostics("range(Colour).return");
//! assert_eq!(diags.len(), 1);
//! assert_eq!(diags[0].range.start, lsp_types::Position::new(0, 6));
//! ```

pub mod position;

use lsp_server::{Connection, ErrorCode, Message, Notification, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    PublishDiagnostics,
};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, InitializeResult, NumberOrString, PublishDiagnosticsParams,
    Range, ServerCapabilities, ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use position::LineIndex;

pub type Error = Box<dyn std::error::Error + Send + Sync>;

pub fn lsp_diagnostics(text: &str) -> Vec<Diagnostic> {
    let index = LineIndex::new(text);
    marigold_grammar::marigold_check(text)
        .into_iter()
        .map(|d| {
            let message = match &d.help {
                Some(help) => format!("{}\nhelp: {help}", d.message),
                None => d.message.clone(),
            };
            Diagnostic {
                range: Range::new(index.position(d.range.start), index.position(d.range.end)),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(d.code.to_string())),
                source: Some("marigold".to_string()),
                message,
                ..Diagnostic::default()
            }
        })
        .collect()
}

pub fn capabilities() -> InitializeResult {
    InitializeResult {
        capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            ..ServerCapabilities::default()
        },
        server_info: Some(ServerInfo {
            name: "marigold-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    }
}

pub fn serve(connection: &Connection) -> Result<(), Error> {
    let (id, _params) = connection.initialize_start()?;
    connection.initialize_finish(id, serde_json::to_value(capabilities())?)?;
    main_loop(connection)
}

fn main_loop(connection: &Connection) -> Result<(), Error> {
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                connection.sender.send(Message::Response(Response::new_err(
                    req.id,
                    ErrorCode::MethodNotFound as i32,
                    format!("unsupported request: {}", req.method),
                )))?;
            }
            Message::Notification(note) => handle_notification(connection, note)?,
            Message::Response(_) => {}
        }
    }
    Ok(())
}

fn handle_notification(connection: &Connection, note: Notification) -> Result<(), Error> {
    match note.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: lsp_types::DidOpenTextDocumentParams = serde_json::from_value(note.params)?;
            let doc = params.text_document;
            publish(
                connection,
                doc.uri,
                Some(doc.version),
                lsp_diagnostics(&doc.text),
            )
        }
        DidChangeTextDocument::METHOD => {
            let params: lsp_types::DidChangeTextDocumentParams =
                serde_json::from_value(note.params)?;
            match params.content_changes.into_iter().last() {
                Some(change) => publish(
                    connection,
                    params.text_document.uri,
                    Some(params.text_document.version),
                    lsp_diagnostics(&change.text),
                ),
                None => Ok(()),
            }
        }
        DidCloseTextDocument::METHOD => {
            let params: lsp_types::DidCloseTextDocumentParams =
                serde_json::from_value(note.params)?;
            publish(connection, params.text_document.uri, None, Vec::new())
        }
        _ => Ok(()),
    }
}

fn publish(
    connection: &Connection,
    uri: Uri,
    version: Option<i32>,
    diagnostics: Vec<Diagnostic>,
) -> Result<(), Error> {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version,
    };
    connection
        .sender
        .send(Message::Notification(Notification::new(
            PublishDiagnostics::METHOD.to_string(),
            params,
        )))?;
    Ok(())
}
