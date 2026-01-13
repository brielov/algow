# Algow for Visual Studio Code

Language support for [Algow](https://github.com/brielov/algow), a statically-typed ML-like language that compiles to JavaScript.

## Features

- **Syntax Highlighting** - Full TextMate grammar for `.alg` files
- **Language Server Protocol** - Powered by the Algow LSP server
  - Diagnostics (errors and warnings)
  - Hover information (types)
  - Go to Definition
  - Find References
  - Auto-completion
  - Document Symbols
  - Formatting

## Requirements

- [Bun](https://bun.sh) runtime installed
- Algow compiler (for the LSP server)

## Setup

### Development (within Algow repo)

The extension automatically detects the LSP server when working within the Algow repository.

### Standalone

1. Clone the Algow repository
2. The extension looks for the LSP server at `src/lsp/index.ts`

## Usage

1. Open a `.alg` file
2. The language server starts automatically
3. Use `Ctrl+Shift+P` → "Algow: Restart Language Server" if needed

## Extension Settings

- `algow.trace.server`: Trace communication with the language server (`off`, `messages`, `verbose`)

## Building the Extension

```bash
cd editors/vscode
npm install
npm run compile
npm run package  # Creates .vsix file
```

## Installing the Extension

```bash
code --install-extension algow-0.1.0.vsix
```

Or use "Extensions: Install from VSIX..." in VS Code.
