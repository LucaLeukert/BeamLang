const vscode = require('vscode');
const { LanguageClient } = require('vscode-languageclient/node');

let client;

function activate(context) {
  const config = vscode.workspace.getConfiguration('beamlang');
  const serverPath = config.get('lsp.serverPath', 'beamlang');
  const serverArgs = config.get('lsp.serverArgs', ['--lsp']);

  const serverOptions = {
    command: serverPath,
    args: serverArgs
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
