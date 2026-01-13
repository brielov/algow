import { execSync } from "child_process";
import * as path from "path";
import { workspace, window, commands, StatusBarAlignment, type ExtensionContext } from "vscode";
import {
  LanguageClient,
  TransportKind,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

/** Check if a command exists in PATH */
const commandExists = (cmd: string): boolean => {
  try {
    execSync(`which ${cmd}`, { stdio: "ignore" });
    return true;
  } catch {
    return false;
  }
};

/** Check if a file exists */
const fileExists = async (filePath: string): Promise<boolean> => {
  try {
    const fs = await import("fs");
    return fs.existsSync(filePath);
  } catch {
    return false;
  }
};

export async function activate(context: ExtensionContext): Promise<void> {
  const config = workspace.getConfiguration("algow");
  const configuredPath = config.get<string>("serverPath");
  const workspaceRoot = workspace.workspaceFolders?.[0]?.uri.fsPath;

  let serverCommand: string;
  let serverArgs: string[];
  let useBun = false;

  if (configuredPath) {
    // Use configured path
    serverCommand = configuredPath;
    serverArgs = ["lsp"];
  } else if (commandExists("algow")) {
    // Found algow in PATH
    serverCommand = "algow";
    serverArgs = ["lsp"];
  } else {
    // Development fallback: look for src/lsp/index.ts in workspace
    const devServerPath = workspaceRoot ? path.join(workspaceRoot, "src", "lsp", "index.ts") : null;

    if (devServerPath && (await fileExists(devServerPath))) {
      serverCommand = "bun";
      serverArgs = ["run", devServerPath];
      useBun = true;
    } else {
      window.showErrorMessage(
        "Algow language server not found. Install algow or set algow.serverPath in settings.",
      );
      return;
    }
  }

  // Server options
  const serverOptions: ServerOptions = {
    run: {
      command: serverCommand,
      args: serverArgs,
      transport: TransportKind.stdio,
    },
    debug: {
      command: serverCommand,
      args: serverArgs,
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
  statusBarItem.text = "$(loading~spin) Algow";
  statusBarItem.tooltip = useBun ? "Algow (dev mode)" : "Algow Language Server";
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
    const errorMsg = error instanceof Error ? error.message : String(error);
    statusBarItem.tooltip = `Algow Language Server failed to start: ${errorMsg}`;
    window.showErrorMessage(`Failed to start Algow Language Server: ${errorMsg}`);
  }
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
  }
}
