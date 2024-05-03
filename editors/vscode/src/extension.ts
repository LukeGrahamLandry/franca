// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from "vscode";
import * as path from "path";

import {
  LanguageClient,
  LanguageClientOptions,
  SettingMonitor,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;
// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
  // Use the console to output diagnostic information (console.log) and errors (console.error)
  // This line of code will only be executed once when your extension is activated
  console.log('Congratulations, your extension "franca" is now active.');

  // The command has been defined in the package.json file
  // Now provide the implementation of the command with registerCommand
  // The commandId parameter must match the command field in package.json
  let disposable = vscode.commands.registerCommand("franca.helloWorld", () => {
    // The code you place here will be executed every time your command is executed
    // Display a message box to the user
    vscode.window.showInformationMessage("Hello World from franca.");
  });

  context.subscriptions.push(disposable);

  let opts = {
    command: "/Users/luke/Documents/mods/infered/target/debug/lsp",
    args: ["lsp"],
  };
  let serverOptions: ServerOptions = {
    run: opts,
    debug: opts,
  };

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: 'file', language: 'franca' }],
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "Franca Language Server",
    serverOptions,
    clientOptions,
  );
  client.start();

}

// This method is called when your extension is deactivated
export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}