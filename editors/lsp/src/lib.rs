use franca::find_std_lib;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{DidChangeTextDocument, DidOpenTextDocument};
use lsp_types::request::{Completion, SemanticTokensFullRequest};
use lsp_types::*;
use lsp_types::{InitializeParams, ServerCapabilities};
use std::collections::HashMap;
use std::error::Error;
use std::sync::Arc;

use franca::ast::Flag;
use franca::lex::{Lexer, TokenType};
use franca::pool::StringPool;

macro_rules! notif {
    ($lsp:expr, $not:expr, $ty:ty, $method:ident) => {{
        match castn::<$ty>($not) {
            Ok(params) => {
                $lsp.$method(params);
                continue;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(req)) => req,
        }
    }};
}

macro_rules! request {
    ($lsp:expr, $connection:expr, $not:expr, $ty:ty, $method:ident) => {{
        match cast::<$ty>($not) {
            Ok((id, params)) => {
                let result = $lsp.$method(params);
                let result = serde_json::to_value(&result).unwrap();
                let resp = Response {
                    id,
                    result: Some(result),
                    error: None,
                };
                $connection.sender.send(Message::Response(resp))?;
                continue;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(req)) => req,
        }
    }};
}

pub fn run_lsp_blocking() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");
    find_std_lib();

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![String::from(".")]),
            ..Default::default()
        }),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
            range: Some(false),
            full: Some(SemanticTokensFullOptions::Bool(true)),
            legend: SemanticTokensLegend {
                token_types: vec![
                    SemanticTokenType::NUMBER,
                    SemanticTokenType::STRING,
                    SemanticTokenType::VARIABLE,
                    SemanticTokenType::ENUM,
                    SemanticTokenType::OPERATOR,
                    SemanticTokenType::COMMENT,
                    SemanticTokenType::PROPERTY,
                    SemanticTokenType::KEYWORD,
                ],
                token_modifiers: vec![],
            },
            ..Default::default()
        })),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    let pool = Box::leak(Box::default()); // TODO: why the fuck bro
    main_loop(connection, initialization_params, pool)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop<'p>(connection: Connection, params: serde_json::Value, pool: &'p StringPool<'p>) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut lsp = Lsp::new(pool);
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                let req = request!(lsp, connection, req, SemanticTokensFullRequest, semantic_tokens);
                let _ = request!(lsp, connection, req, Completion, completion);
            }
            Message::Response(_) => {}
            Message::Notification(not) => {
                let not = notif!(lsp, not, DidOpenTextDocument, did_open);
                let _ = notif!(lsp, not, DidChangeTextDocument, did_change);
            }
        }
    }
    Ok(())
}

struct Lsp<'p> {
    files: HashMap<Url, String>,
    pool: &'p StringPool<'p>,
}

impl<'p> Lsp<'p> {
    fn new(pool: &'p StringPool<'p>) -> Self {
        Lsp { files: HashMap::new(), pool }
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) {
        self.files.insert(params.text_document.uri, params.text_document.text);
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) {
        self.files
            .insert(params.text_document.uri, params.content_changes.last().unwrap().text.clone());
    }

    fn semantic_tokens(&mut self, params: SemanticTokensParams) -> Option<SemanticTokensResult> {
        let text = self.files.get(&params.text_document.uri)?;

        let mut lex = Lexer::new(text.clone(), self.pool, text.span);

        let mut symbols = vec![];
        let mut last_line = 0;
        let mut last_col = 0;
        loop {
            let token = lex.next();
            let kind = match &token.kind {
                TokenType::Error(_) | TokenType::Eof => break,
                TokenType::BinaryNum { .. } | TokenType::Number(_) => 0,
                TokenType::Quoted { .. } => 1,
                TokenType::Symbol(i) => {
                    if i.0 < Flag::_Reserved_Count_ as u32 {
                        3
                    } else {
                        2
                    }
                }
                TokenType::At => 3,
                TokenType::Bang => 5,
                TokenType::DoubleSquare => 6,
                TokenType::Fn | TokenType::Qualifier(_) => 7,
                _ => 4,
            };
            let start = text.find_line_col(token.span.low()); // TODO: this has gotta be slow
            let delta_col = if start.line == last_line {
                start.column - last_col
            } else {
                start.column
            };
            symbols.push(SemanticToken {
                delta_line: (start.line - last_line) as u32,
                delta_start: delta_col as u32,
                length: token.span.len() as u32,
                token_type: kind,
                token_modifiers_bitset: 0,
            });

            last_line = start.line;
            last_col = start.column;
        }
        Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: symbols,
        }))
    }
    fn completion(&mut self, params: CompletionParams) -> Option<CompletionResponse> {
        let _ = self.files.get(&params.text_document_position.text_document.uri)?;
        Some(CompletionResponse::List(CompletionList {
            is_incomplete: true,
            items: vec![CompletionItem {
                label: String::from("add"),
                ..Default::default()
            }],
        }))
    }
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn castn<R>(req: Notification) -> Result<R::Params, ExtractError<Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
