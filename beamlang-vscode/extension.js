const vscode = require('vscode');
const path = require('path');
const { execFile } = require('child_process');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;

function activate(context) {
  const config = vscode.workspace.getConfiguration('beamlang');
  const serverPath = config.get('lsp.serverPath', 'beamlang');
  const serverArgs = config.get('lsp.serverArgs', ['--lsp']);
  const serverCwd = config.get('lsp.serverCwd', '');
  const debug = config.get('lsp.debug', false);

  const serverOptions = {
    command: serverPath,
    args: serverArgs,
    transport: TransportKind.stdio,
    options: {
      cwd: serverCwd ? serverCwd : path.dirname(serverPath),
      env: Object.assign({}, process.env, debug ? { BEAMLANG_LSP_DEBUG: '1' } : {})
    }
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

  // Register "Format File" command using CLI (works even without the LSP)
  const formatCmd = vscode.commands.registerCommand('beamlang.formatFile', () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== 'beamlang') {
      vscode.window.showWarningMessage('No active BeamLang file to format.');
      return;
    }

    const doc = editor.document;
    const cliPath = config.get('lsp.serverPath', 'beamlang');

    execFile(cliPath, ['format', doc.fileName], (err, stdout) => {
      if (err) {
        vscode.window.showErrorMessage('BeamLang format failed: ' + err.message);
        return;
      }
      const fullRange = new vscode.Range(
        doc.positionAt(0),
        doc.positionAt(doc.getText().length)
      );
      editor.edit(editBuilder => {
        editBuilder.replace(fullRange, stdout);
      });
    });
  });

  context.subscriptions.push(formatCmd);
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
