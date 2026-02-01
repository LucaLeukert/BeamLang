# Quick Installation Guide

## Method 1: Manual Installation (Recommended)

1. **Locate your VS Code extensions folder:**
   - Windows: `C:\Users\YourUsername\.vscode\extensions\`
   - macOS: `~/.vscode/extensions/`
   - Linux: `~/.vscode/extensions/`

2. **Copy the extension:**
   - Copy the entire `beamlang-vscode` folder to the extensions directory
   - Rename it to `beamlang-1.0.0` (optional, but follows VS Code convention)

3. **Restart VS Code:**
   - Close and reopen VS Code
   - Or press `Ctrl+Shift+P` (Cmd+Shift+P on Mac) and run "Developer: Reload Window"

4. **Verify installation:**
   - Open the `example.bl` file
   - You should see syntax highlighting
   - Check the language mode in the bottom-right corner (should say "BeamLang")
   - For LSP features, ensure the `beamlang` CLI is available in your PATH

## Method 2: Build and Install VSIX (Advanced)

If you want to create a proper VSIX package:

1. **Install vsce (Visual Studio Code Extension Manager):**
   ```bash
   npm install -g @vscode/vsce
   ```

2. **Navigate to the extension directory:**
   ```bash
   cd beamlang-vscode
   ```

3. **Package the extension:**
   ```bash
   vsce package
   ```
   This creates a `beamlang-1.0.0.vsix` file.

4. **Install the VSIX:**
   - Open VS Code
   - Go to Extensions (Ctrl+Shift+X)
   - Click the "..." menu
   - Select "Install from VSIX..."
   - Choose the generated `.vsix` file

## Testing the Extension

1. Open `example.bl` in VS Code
2. Verify that you see:
   - Keywords highlighted in purple/pink
   - Types highlighted in blue/green
   - Strings highlighted in orange/red
   - Comments highlighted in gray/green
- Function names highlighted
3. Hover over a function name or trigger completion to verify the language server is running

## Troubleshooting

**Extension not working?**
- Make sure the extension folder is in the correct location
- Ensure VS Code has been restarted
- Check that `.bl` files are associated with "BeamLang" language mode
- Try manually setting the language: Click the language indicator in bottom-right → Select "BeamLang"

**Language server not starting?**
- Ensure the `beamlang` CLI is on your PATH
- Or set `beamlang.lsp.serverPath` in VS Code settings to the full path of the CLI
- If you need custom args, update `beamlang.lsp.serverArgs`

**Syntax highlighting looks wrong?**
- Some themes may display colors differently
- Try a different color theme: `Ctrl+K Ctrl+T` (Cmd+K Cmd+T on Mac)

**Need to modify the extension?**
- Edit `syntaxes/beamlang.tmLanguage.json` for syntax rules
- Edit `language-configuration.json` for bracket/comment behavior
- Reload VS Code window after changes: `Ctrl+R` (Cmd+R on Mac)
