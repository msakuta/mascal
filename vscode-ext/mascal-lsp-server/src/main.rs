use dashmap::DashMap;
use log::debug;
use mascal::{nom::Finish, TypeParams};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::{
    jsonrpc::Result, lsp_types::notification::Notification, lsp_types::*, Client, LanguageServer,
    LspService, Server,
};

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: DashMap<String, String>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let res = InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(true),
                        })),
                        ..Default::default()
                    },
                )),

                hover_provider: Some(HoverProviderCapability::Simple(true)),

                ..ServerCapabilities::default()
            },
        };
        Ok(res)
    }

    async fn initialized(&self, _: InitializedParams) {
        debug!("initialized!");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        debug!("file opened");
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: &params.text_document.text,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            text: &params.content_changes[0].text,
            uri: params.text_document.uri,
            version: Some(params.text_document.version),
        })
        .await
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        dbg!(&params.text);
        if let Some(text) = params.text {
            let item = TextDocumentItem {
                uri: params.text_document.uri,
                text: &text,
                version: None,
            };
            self.on_change(item).await;
            _ = self.client.semantic_tokens_refresh().await;
        }
        debug!("file saved!");
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        debug!("file closed!");
    }

    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        debug!("inlay hint");
        let uri = &params.text_document.uri;
        let hashmap = self.type_hints(uri, false).unwrap_or_else(|| vec![]);

        let inlay_hint_list = hashmap
            .into_iter()
            .filter_map(|type_hint| {
                let inlay_hint = InlayHint {
                    text_edits: None,
                    tooltip: None,
                    kind: Some(InlayHintKind::TYPE),
                    padding_left: None,
                    padding_right: None,
                    data: None,
                    position: type_hint.end,
                    label: InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
                        value: if type_hint.literal {
                            type_hint.ty.to_string()
                        } else {
                            format!(": {}", type_hint.ty)
                        },
                        tooltip: None,
                        location: Some(Location {
                            uri: params.text_document.uri.clone(),
                            range: Range {
                                start: Position::new(0, 4),
                                end: Position::new(0, 10),
                            },
                        }),
                        command: None,
                    }]),
                };
                Some(inlay_hint)
            })
            .collect::<Vec<_>>();

        Ok(Some(inlay_hint_list))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        debug!("hover {uri} @ {position:?}");
        let hashmap = self.type_hints(uri, true).unwrap_or_else(|| vec![]);

        if let Some(item) = hashmap
            .into_iter()
            .find(|item| item.start <= position && position < item.end)
        {
            return Ok(Some(Hover {
                contents: HoverContents::Scalar(MarkedString::LanguageString(LanguageString {
                    language: "rpar".to_string(),
                    value: if item.literal {
                        format!("{}{}", item.name, item.ty.to_string())
                    } else {
                        format!("var {}: {}", item.name, item.ty)
                    },
                })),
                range: None,
            }));
        }
        Ok(None)
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        debug!("configuration changed!");
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        debug!("workspace folders changed!");
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        debug!("watched files have changed!");
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        debug!("command executed!");

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}
#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

#[allow(unused)]
enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem<'a> {
    uri: Url,
    text: &'a str,
    version: Option<i32>,
}

impl Backend {
    async fn on_change<'a>(&self, params: TextDocumentItem<'a>) {
        self.document_map
            .insert(params.uri.to_string(), params.text.to_string());

        let diagnostics;
        {
            mascal::DEBUG_STREAM.set(Box::new(std::io::sink()));
            let parse_result = mascal::source(params.text).finish();
            diagnostics = match parse_result {
                Err(e) => {
                    vec![Diagnostic::new_simple(
                        span_to_range(e.input),
                        e.to_string(),
                    )]
                }
                Ok((_, mut stmts)) => {
                    let mut ctx = mascal::TypeCheckContext::new(Some(params.uri.as_str()));
                    if let Err(e) = mascal::type_check(&mut stmts, &mut ctx) {
                        vec![Diagnostic::new_simple(
                            span_to_range(e.span()),
                            e.to_string(),
                        )]
                    } else {
                        vec![]
                    }
                }
            };
        }

        debug!("publishing {} diagnostics", diagnostics.len());
        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, params.version)
            .await;
    }
}

struct TypeHint {
    start: Position,
    end: Position,
    name: String,
    ty: String,
    literal: bool,
}

impl Backend {
    fn type_hints(&self, uri: &Url, include_annotated: bool) -> Option<Vec<TypeHint>> {
        let Some(doc) = self.document_map.get(uri.as_str()) else {
            return None;
        };

        let mut hashmap = vec![];

        let doc_str = doc.value();
        mascal::DEBUG_STREAM.set(Box::new(std::io::sink()));
        let Ok((_, mut ast)) = mascal::source(&doc_str) else {
            debug!("source failed");
            return None;
        };
        if let Err(e) = mascal::type_check(
            &mut ast,
            &mut mascal::TypeCheckContext::new(Some(uri.as_str())),
        ) {
            debug!("type check error: {e}");
            return None;
        }
        mascal::iter_types(&ast, &mut |ty_params| {
            let TypeParams {
                span,
                ty,
                annotated,
                literal,
            } = ty_params;
            let start_pos = Position {
                line: span.location_line().saturating_sub(1),
                character: span.get_column().saturating_sub(1) as u32,
            };
            let end_pos = Position {
                // Offset 1 is difference between nom_locate (assumes line starts with 1) and LSP (with 0)
                line: span.location_line().saturating_sub(1),
                character: span.get_column().saturating_sub(1) as u32 + span.len() as u32,
            };
            if !annotated || include_annotated {
                hashmap.push(TypeHint {
                    start: start_pos,
                    end: end_pos,
                    name: span.to_string(),
                    ty: ty.to_string(),
                    literal,
                });
            }
        });

        Some(hashmap)
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn span_to_range(span: mascal::Span) -> Range {
    Range {
        start: Position {
            line: span.location_line().saturating_sub(1),
            character: span.get_column().saturating_sub(1) as u32,
        },
        end: Position {
            line: span.location_line().saturating_sub(1),
            character: span.get_column().saturating_sub(1) as u32 + span.len() as u32,
        },
    }
}
