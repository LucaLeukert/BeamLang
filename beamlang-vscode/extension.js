const vscode = require('vscode');
const path = require('path');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;

function activate(context) {
  const config = vscode.workspace.getConfiguration('beamlang');
  const serverPath = config.get('lsp.serverPath', 'beamlang');
  const serverArgs = config.get('lsp.serverArgs', ['--lsp']);
  const serverCwd = config.get('lsp.serverCwd', '');

  const serverOptions = {
    command: serverPath,
    args: serverArgs,
    transport: TransportKind.stdio,
    options: serverCwd
      ? { cwd: serverCwd }
      : { cwd: path.dirname(serverPath) }
  };

  const clientOptions = {
    documentSelector: [{ scheme: 'file', language: 'beamlang' }]
  };

  client = new LanguageClient(
    'beamlang',
    'BeamLang Language Server',
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(client.start());
}

function deactivate() {
  if (!client) {
    return undefined;
  }
  return client.stop();
}

module.exports = {
  activate,
  deactivate
};
