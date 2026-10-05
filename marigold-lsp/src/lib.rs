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

pub mod nav;
pub mod position;

use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Exit, Notification as _,
    PublishDiagnostics,
};
use lsp_types::request::{
    DocumentSymbolRequest, GotoDefinition, References, Request as _, Shutdown,
};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse,
    GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, InitializeResult,
    NumberOrString, OneOf, PublishDiagnosticsParams, Range, ReferenceParams, ServerCapabilities,
    ServerInfo, SymbolInformation, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use marigold_grammar::diagnostics::Severity;
use position::LineIndex;
use std::collections::HashMap;

pub type Error = Box<dyn std::error::Error + Send + Sync>;

fn lsp_severity(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Information => DiagnosticSeverity::INFORMATION,
        _ => DiagnosticSeverity::ERROR,
    }
}

/// Checks `text` and converts every diagnostic to LSP form with UTF-16 ranges.
///
/// Oversized input yields the single `input-too-large` diagnostic that
/// [`marigold_grammar::marigold_check`] produces.
///
/// ```
/// assert!(marigold_lsp::lsp_diagnostics("range(0, 1).return").is_empty());
/// let big = " ".repeat(marigold_grammar::diagnostics::MAX_CHECK_INPUT_BYTES + 1);
/// let diags = marigold_lsp::lsp_diagnostics(&big);
/// assert_eq!(diags.len(), 1);
/// assert_eq!(
///     diags[0].code,
///     Some(lsp_types::NumberOrString::String("input-too-large".into()))
/// );
/// ```
pub fn lsp_diagnostics(text: &str) -> Vec<Diagnostic> {
    let index = LineIndex::new(text);
    marigold_grammar::marigold_check(text)
        .into_iter()
        .map(|d| {
            let message = match &d.help {
                Some(help) => format!("{}\nhelp: {help}", d.message),
                None => d.message.clone(),
            };
            let severity = lsp_severity(d.severity);
            Diagnostic {
                range: Range::new(index.position(d.range.start), index.position(d.range.end)),
                severity: Some(severity),
                code: Some(NumberOrString::String(d.code.to_string())),
                source: Some("marigold".to_string()),
                message,
                ..Diagnostic::default()
            }
        })
        .collect()
}

/// The `initialize` result: full-document sync, navigation providers and server info.
///
/// ```
/// use lsp_types::OneOf;
/// let caps = marigold_lsp::capabilities();
/// assert_eq!(caps.server_info.unwrap().name, "marigold-lsp");
/// assert!(caps.capabilities.text_document_sync.is_some());
/// assert_eq!(caps.capabilities.definition_provider, Some(OneOf::Left(true)));
/// assert_eq!(caps.capabilities.references_provider, Some(OneOf::Left(true)));
/// assert_eq!(caps.capabilities.document_symbol_provider, Some(OneOf::Left(true)));
/// ```
pub fn capabilities() -> InitializeResult {
    InitializeResult {
        capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            definition_provider: Some(OneOf::Left(true)),
            references_provider: Some(OneOf::Left(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        },
        server_info: Some(ServerInfo {
            name: "marigold-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    }
}

/// Runs the server on `connection` until `shutdown` + `exit`, or until the
/// peer disconnects. Returns an error if `exit` arrives without a prior
/// `shutdown`, as the LSP specification requires a failing exit code then.
///
/// ```
/// use lsp_server::{Connection, Message, Notification, Request, RequestId};
///
/// let (server, client) = Connection::memory();
/// let handle = std::thread::spawn(move || marigold_lsp::serve(&server));
/// client.sender.send(Message::Request(Request::new(
///     RequestId::from(1),
///     "initialize".into(),
///     serde_json::json!({"capabilities": {}}),
/// ))).unwrap();
/// client.receiver.recv().unwrap();
/// client.sender.send(Message::Notification(Notification::new("initialized".into(), serde_json::json!({})))).unwrap();
/// client.sender.send(Message::Request(Request::new(RequestId::from(2), "shutdown".into(), serde_json::Value::Null))).unwrap();
/// client.sender.send(Message::Notification(Notification::new("exit".into(), serde_json::Value::Null))).unwrap();
/// assert!(handle.join().unwrap().is_ok());
/// ```
pub fn serve(connection: &Connection) -> Result<(), Error> {
    let (id, params) = connection.initialize_start()?;
    connection.initialize_finish(id, serde_json::to_value(capabilities())?)?;
    let params: InitializeParams = serde_json::from_value(params).unwrap_or_default();
    let mut server = Server::new(&params);
    main_loop(connection, &mut server)
}

struct Server {
    docs: HashMap<Uri, String>,
    hierarchical_symbols: bool,
}

type Failure = (ErrorCode, String);

impl Server {
    fn new(params: &InitializeParams) -> Self {
        let hierarchical_symbols = params
            .capabilities
            .text_document
            .as_ref()
            .and_then(|t| t.document_symbol.as_ref())
            .and_then(|d| d.hierarchical_document_symbol_support)
            .unwrap_or(false);
        Self {
            docs: HashMap::new(),
            hierarchical_symbols,
        }
    }

    fn handle(&self, req: Request) -> Result<serde_json::Value, Failure> {
        match req.method.as_str() {
            GotoDefinition::METHOD => {
                self.definition(serde_json::from_value(req.params).map_err(invalid_params)?)
            }
            References::METHOD => {
                self.references(serde_json::from_value(req.params).map_err(invalid_params)?)
            }
            DocumentSymbolRequest::METHOD => {
                self.document_symbols(serde_json::from_value(req.params).map_err(invalid_params)?)
            }
            other => Err((
                ErrorCode::MethodNotFound,
                format!("unsupported request: {other}"),
            )),
        }
    }

    fn definition(&self, params: GotoDefinitionParams) -> Result<serde_json::Value, Failure> {
        let at = params.text_document_position_params;
        let uri = at.text_document.uri;
        let found = self
            .docs
            .get(&uri)
            .and_then(|text| nav::definition(&uri, text, at.position))
            .map(GotoDefinitionResponse::Scalar);
        serde_json::to_value(found).map_err(internal)
    }

    fn references(&self, params: ReferenceParams) -> Result<serde_json::Value, Failure> {
        let at = params.text_document_position;
        let uri = at.text_document.uri;
        let found = self.docs.get(&uri).and_then(|text| {
            nav::references(&uri, text, at.position, params.context.include_declaration)
        });
        serde_json::to_value(found).map_err(internal)
    }

    #[allow(deprecated)]
    fn document_symbols(&self, params: DocumentSymbolParams) -> Result<serde_json::Value, Failure> {
        let uri = params.text_document.uri;
        let Some(text) = self.docs.get(&uri) else {
            return Ok(serde_json::Value::Null);
        };
        let outline = nav::outline(text);
        let response = if self.hierarchical_symbols {
            DocumentSymbolResponse::Nested(
                outline
                    .into_iter()
                    .map(|s| DocumentSymbol {
                        name: s.name,
                        detail: None,
                        kind: s.kind,
                        tags: None,
                        deprecated: None,
                        range: s.range,
                        selection_range: s.selection_range,
                        children: None,
                    })
                    .collect(),
            )
        } else {
            DocumentSymbolResponse::Flat(
                outline
                    .into_iter()
                    .map(|s| SymbolInformation {
                        name: s.name,
                        kind: s.kind,
                        tags: None,
                        deprecated: None,
                        location: lsp_types::Location::new(uri.clone(), s.range),
                        container_name: None,
                    })
                    .collect(),
            )
        };
        serde_json::to_value(Some(response)).map_err(internal)
    }
}

fn invalid_params(err: serde_json::Error) -> Failure {
    (ErrorCode::InvalidParams, err.to_string())
}

fn internal(err: serde_json::Error) -> Failure {
    (ErrorCode::InternalError, err.to_string())
}

fn main_loop(connection: &Connection, server: &mut Server) -> Result<(), Error> {
    let mut shutdown = false;
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if req.method == Shutdown::METHOD {
                    connection
                        .sender
                        .send(Message::Response(Response::new_ok(req.id, ())))?;
                    shutdown = true;
                } else if shutdown {
                    connection.sender.send(Message::Response(Response::new_err(
                        req.id,
                        ErrorCode::InvalidRequest as i32,
                        "server is shutting down".to_string(),
                    )))?;
                } else {
                    let id = req.id.clone();
                    let response = match server.handle(req) {
                        Ok(value) => Response::new_ok(id, value),
                        Err((code, message)) => Response::new_err(id, code as i32, message),
                    };
                    connection.sender.send(Message::Response(response))?;
                }
            }
            Message::Notification(note) => {
                if note.method == Exit::METHOD {
                    return if shutdown {
                        Ok(())
                    } else {
                        Err("received exit notification without prior shutdown".into())
                    };
                }
                if !shutdown {
                    handle_notification(connection, server, note)?;
                }
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

fn handle_notification(
    connection: &Connection,
    server: &mut Server,
    note: Notification,
) -> Result<(), Error> {
    let method = note.method;
    match method.as_str() {
        DidOpenTextDocument::METHOD => {
            match serde_json::from_value::<lsp_types::DidOpenTextDocumentParams>(note.params) {
                Ok(params) => {
                    let doc = params.text_document;
                    server.docs.insert(doc.uri.clone(), doc.text.clone());
                    publish(
                        connection,
                        doc.uri,
                        Some(doc.version),
                        lsp_diagnostics(&doc.text),
                    )
                }
                Err(err) => malformed(&method, &err),
            }
        }
        DidChangeTextDocument::METHOD => {
            match serde_json::from_value::<lsp_types::DidChangeTextDocumentParams>(note.params) {
                Ok(params) => match params.content_changes.into_iter().last() {
                    Some(change) => {
                        server
                            .docs
                            .insert(params.text_document.uri.clone(), change.text.clone());
                        publish(
                            connection,
                            params.text_document.uri,
                            Some(params.text_document.version),
                            lsp_diagnostics(&change.text),
                        )
                    }
                    None => Ok(()),
                },
                Err(err) => malformed(&method, &err),
            }
        }
        DidCloseTextDocument::METHOD => {
            match serde_json::from_value::<lsp_types::DidCloseTextDocumentParams>(note.params) {
                Ok(params) => {
                    server.docs.remove(&params.text_document.uri);
                    publish(connection, params.text_document.uri, None, Vec::new())
                }
                Err(err) => malformed(&method, &err),
            }
        }
        _ => Ok(()),
    }
}

fn malformed(method: &str, err: &serde_json::Error) -> Result<(), Error> {
    eprintln!("marigold-lsp: ignoring malformed {method} notification: {err}");
    Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maps_severity() {
        assert_eq!(lsp_severity(Severity::Error), DiagnosticSeverity::ERROR);
        assert_eq!(lsp_severity(Severity::Warning), DiagnosticSeverity::WARNING);
        assert_eq!(
            lsp_severity(Severity::Information),
            DiagnosticSeverity::INFORMATION
        );
    }

    #[test]
    fn error_diagnostics_keep_error_severity() {
        let diags = lsp_diagnostics("range(Colour).return");
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
    }
}
