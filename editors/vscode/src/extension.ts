import * as path from "path";
import { workspace, window, commands, StatusBarAlignment, type ExtensionContext } from "vscode";
import {
  LanguageClient,
  TransportKind,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export async function activate(context: ExtensionContext): Promise<void> {
  // Find the Algow LSP server
  // First try the workspace, then fall back to global installation
  const workspaceRoot = workspace.workspaceFolders?.[0]?.uri.fsPath;

  // Look for the LSP server in common locations
  const serverPaths = [
    // In the Algow project itself
    workspaceRoot ? path.join(workspaceRoot, "src", "lsp", "index.ts") : null,
    // Parent directory (if editing files in a subdirectory of algow)
    workspaceRoot ? path.join(workspaceRoot, "..", "src", "lsp", "index.ts") : null,
  ].filter((p): p is string => p !== null);

  let serverModule: string | undefined;

  for (const serverPath of serverPaths) {
    try {
      const fs = await import("fs");
      if (fs.existsSync(serverPath)) {
        serverModule = serverPath;
        break;
      }
    } catch {
      // Continue to next path
    }
  }

  if (!serverModule) {
    // Try to use globally installed algow
    serverModule = "algow-lsp";
    window.showWarningMessage(
      "Algow LSP server not found in workspace. Using global installation if available.",
    );
  }

  // Server options - run with bun
  const serverOptions: ServerOptions = {
    run: {
      command: "bun",
      args: ["run", serverModule],
      transport: TransportKind.stdio,
    },
    debug: {
      command: "bun",
      args: ["run", serverModule],
      transport: TransportKind.stdio,
    },
  };

  // Client options
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "algow" }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.alg"),
    },
    outputChannelName: "Algow Language Server",
  };

  // Create the language client
  client = new LanguageClient("algow", "Algow Language Server", serverOptions, clientOptions);

  // Create status bar item
  const statusBarItem = window.createStatusBarItem(StatusBarAlignment.Right, 100);
  statusBarItem.text = "$(check) Algow";
  statusBarItem.tooltip = "Algow Language Server";
  statusBarItem.show();
  context.subscriptions.push(statusBarItem);

  // Register commands
  context.subscriptions.push(
    commands.registerCommand("algow.restartServer", async () => {
      if (client) {
        await client.stop();
        await client.start();
        window.showInformationMessage("Algow Language Server restarted");
      }
    }),
  );

  // Start the client
  try {
    await client.start();
    statusBarItem.text = "$(check) Algow";
  } catch (error) {
    statusBarItem.text = "$(error) Algow";
    statusBarItem.tooltip = `Algow Language Server failed to start: ${error}`;
    window.showErrorMessage(`Failed to start Algow Language Server: ${error}`);
  }
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
  }
}
