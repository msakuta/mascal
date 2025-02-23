"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", { value: true });
exports.activateInlayHints = exports.deactivate = exports.activate = void 0;
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
// type a = Parameters<>;
async function activate(context) {
    console.log("Heeeeey");
    const traceOutputChannel = vscode_1.window.createOutputChannel("Mascal Language Server trace");
    const command = process.env.SERVER_PATH || "mascal-lsp-server";
    const run = {
        command,
        options: {
            env: {
                ...process.env,
                // eslint-disable-next-line @typescript-eslint/naming-convention
                RUST_LOG: "debug",
            },
        },
    };
    const serverOptions = {
        run,
        debug: run,
    };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    // Options to control the language client
    let clientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: "file", language: "rpar" }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: vscode_1.workspace.createFileSystemWatcher("**/.clientrc"),
        },
        traceOutputChannel,
    };
    // Create the language client and start the client.
    client = new node_1.LanguageClient("mascal-lsp-server", "mascal language server", serverOptions, clientOptions);
    // activateInlayHints(context);
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
function activateInlayHints(ctx) {
    const maybeUpdater = {
        hintsProvider: null,
        updateHintsEventEmitter: new vscode_1.EventEmitter(),
        async onConfigChange() {
            this.dispose();
            const event = this.updateHintsEventEmitter.event;
            // this.hintsProvider = languages.registerInlayHintsProvider(
            //   { scheme: "file", language: "nrs" },
            //   // new (class implements InlayHintsProvider {
            //   //   onDidChangeInlayHints = event;
            //   //   resolveInlayHint(hint: InlayHint, token: CancellationToken): ProviderResult<InlayHint> {
            //   //     const ret = {
            //   //       label: hint.label,
            //   //       ...hint,
            //   //     };
            //   //     return ret;
            //   //   }
            //   //   async provideInlayHints(
            //   //     document: TextDocument,
            //   //     range: Range,
            //   //     token: CancellationToken
            //   //   ): Promise<InlayHint[]> {
            //   //     const hints = (await client
            //   //       .sendRequest("custom/inlay_hint", { path: document.uri.toString() })
            //   //       .catch(err => null)) as [number, number, string][];
            //   //     if (hints == null) {
            //   //       return [];
            //   //     } else {
            //   //       return hints.map(item => {
            //   //         const [start, end, label] = item;
            //   //         let startPosition = document.positionAt(start);
            //   //         let endPosition = document.positionAt(end);
            //   //         return {
            //   //           position: endPosition,
            //   //           paddingLeft: true,
            //   //           label: [
            //   //             {
            //   //               value: `${label}`,
            //   //               // location: {
            //   //               //   uri: document.uri,
            //   //               //   range: new Range(1, 0, 1, 0)
            //   //               // }
            //   //               command: {
            //   //                 title: "hello world",
            //   //                 command: "helloworld.helloWorld",
            //   //                 arguments: [document.uri],
            //   //               },
            //   //             },
            //   //           ],
            //   //         };
            //   //       });
            //   //     }
            //   //   }
            //   // })()
            // );
        },
        onDidChangeTextDocument({ contentChanges, document }) {
            // debugger
            // this.updateHintsEventEmitter.fire();
        },
        dispose() {
            var _a;
            (_a = this.hintsProvider) === null || _a === void 0 ? void 0 : _a.dispose();
            this.hintsProvider = null;
            this.updateHintsEventEmitter.dispose();
        },
    };
    vscode_1.workspace.onDidChangeConfiguration(maybeUpdater.onConfigChange, maybeUpdater, ctx.subscriptions);
    vscode_1.workspace.onDidChangeTextDocument(maybeUpdater.onDidChangeTextDocument, maybeUpdater, ctx.subscriptions);
    maybeUpdater.onConfigChange().catch(console.error);
}
exports.activateInlayHints = activateInlayHints;
//# sourceMappingURL=extension.js.map