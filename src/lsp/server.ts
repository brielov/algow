/**
 * Algow LSP Server
 *
 * Main Language Server Protocol implementation.
 * Handles all LSP methods and coordinates with the analyzer.
 */

import { format as formatSource } from "../format";
import {
  analyze,
  createAnalyzer,
  getCompletionsAtPosition,
  getDefinitionAtPosition,
  getDocumentSymbols,
  getHoverAtPosition,
  getReferencesAtPosition,
  type Analyzer,
} from "./analysis";
import {
  closeDocument,
  createDocumentManager,
  getDocument,
  openDocument,
  updateDocument,
  type DocumentManager,
} from "./documents";
import { spanToRange, type Position, type Range } from "./positions";
import {
  createErrorResponse,
  createNotification,
  createResponse,
  encodeMessage,
  ErrorCodes,
  isNotification,
  isRequest,
  type Message,
  type RequestMessage,
  type NotificationMessage,
} from "./protocol";
import { getFileById, globalToLocal, pathToUri, uriToPath } from "./workspace";

// =============================================================================
// LSP Types
// =============================================================================

type DiagnosticSeverity = 1 | 2 | 3 | 4; // Error, Warning, Information, Hint

type LSPDiagnostic = {
  range: Range;
  message: string;
  severity: DiagnosticSeverity;
  source: string;
};

type CompletionItemKind =
  | 1 // Text
  | 2 // Method
  | 3 // Function
  | 4 // Constructor
  | 5 // Field
  | 6 // Variable
  | 9; // Module

type SymbolKind =
  | 5 // Class (for types)
  | 6 // Method
  | 12 // Function
  | 13 // Variable
  | 22; // Struct (for constructors)

// =============================================================================
// Server State
// =============================================================================

export type LSPServer = {
  readonly documents: DocumentManager;
  readonly analyzer: Analyzer;
  initialized: boolean;
  shutdownRequested: boolean;
  write: (data: Buffer) => void;
};

// =============================================================================
// Factory
// =============================================================================

/** Create a new LSP server */
export const createServer = (write: (data: Buffer) => void): LSPServer => ({
  documents: createDocumentManager(),
  analyzer: createAnalyzer(),
  initialized: false,
  shutdownRequested: false,
  write,
});

// =============================================================================
// Message Handling
// =============================================================================

/** Handle an incoming message */
export const handleMessage = (server: LSPServer, message: Message): void => {
  if (isRequest(message)) {
    handleRequest(server, message);
  } else if (isNotification(message)) {
    handleNotification(server, message);
  }
};

/** Send a response */
const sendResponse = (server: LSPServer, id: number | string, result: unknown): void => {
  const response = createResponse(id, result);
  server.write(encodeMessage(response));
};

/** Send an error response */
const sendError = (server: LSPServer, id: number | string, code: number, message: string): void => {
  const response = createErrorResponse(id, code, message);
  server.write(encodeMessage(response));
};

/** Send a notification */
const sendNotification = (server: LSPServer, method: string, params?: unknown): void => {
  const notification = createNotification(method, params);
  server.write(encodeMessage(notification));
};

// =============================================================================
// Request Handlers
// =============================================================================

const handleRequest = (server: LSPServer, request: RequestMessage): void => {
  const { id, method, params } = request;

  switch (method) {
    case "initialize":
      handleInitialize(server, id, params);
      break;
    case "shutdown":
      handleShutdown(server, id);
      break;
    case "textDocument/hover":
      handleHover(server, id, params);
      break;
    case "textDocument/definition":
      handleDefinition(server, id, params);
      break;
    case "textDocument/references":
      handleReferences(server, id, params);
      break;
    case "textDocument/rename":
      handleRename(server, id, params);
      break;
    case "textDocument/completion":
      handleCompletion(server, id, params);
      break;
    case "textDocument/documentSymbol":
      handleDocumentSymbol(server, id, params);
      break;
    case "textDocument/formatting":
      handleFormatting(server, id, params);
      break;
    default:
      sendError(server, id, ErrorCodes.MethodNotFound, `Unknown method: ${method}`);
  }
};

const handleNotification = (server: LSPServer, notification: NotificationMessage): void => {
  const { method, params } = notification;

  switch (method) {
    case "initialized":
      server.initialized = true;
      break;
    case "exit":
      process.exit(server.shutdownRequested ? 0 : 1);
      break;
    case "textDocument/didOpen":
      handleDidOpen(server, params);
      break;
    case "textDocument/didChange":
      handleDidChange(server, params);
      break;
    case "textDocument/didClose":
      handleDidClose(server, params);
      break;
  }
};

// =============================================================================
// Lifecycle Methods
// =============================================================================

const handleInitialize = (server: LSPServer, id: number | string, _params: unknown): void => {
  sendResponse(server, id, {
    capabilities: {
      textDocumentSync: {
        openClose: true,
        change: 1, // Full sync
      },
      hoverProvider: true,
      definitionProvider: true,
      referencesProvider: true,
      renameProvider: {
        prepareProvider: false,
      },
      completionProvider: {
        triggerCharacters: ["."],
      },
      documentSymbolProvider: true,
      documentFormattingProvider: true,
    },
    serverInfo: {
      name: "algow-lsp",
      version: "0.1.0",
    },
  });
};

const handleShutdown = (server: LSPServer, id: number | string): void => {
  server.shutdownRequested = true;
  sendResponse(server, id, null);
};

// =============================================================================
// Document Sync
// =============================================================================

const handleDidOpen = (server: LSPServer, params: unknown): void => {
  const { textDocument } = params as {
    textDocument: { uri: string; text: string; version: number };
  };
  openDocument(server.documents, textDocument.uri, textDocument.text, textDocument.version);
  reanalyzeAndPublishDiagnostics(server);
};

const handleDidChange = (server: LSPServer, params: unknown): void => {
  const { textDocument, contentChanges } = params as {
    textDocument: { uri: string; version: number };
    contentChanges: { text: string }[];
  };

  // We use full sync, so there's only one content change with the full text
  if (contentChanges.length > 0) {
    updateDocument(
      server.documents,
      textDocument.uri,
      contentChanges[0]!.text,
      textDocument.version,
    );
    reanalyzeAndPublishDiagnostics(server);
  }
};

const handleDidClose = (server: LSPServer, params: unknown): void => {
  const { textDocument } = params as { textDocument: { uri: string } };
  closeDocument(server.documents, textDocument.uri);
  // Clear diagnostics for closed document
  sendNotification(server, "textDocument/publishDiagnostics", {
    uri: textDocument.uri,
    diagnostics: [],
  });
};

// =============================================================================
// Analysis and Diagnostics
// =============================================================================

const reanalyzeAndPublishDiagnostics = (server: LSPServer): void => {
  const result = analyze(server.analyzer, server.documents);

  // Group diagnostics by file
  const diagnosticsByFile = new Map<string, LSPDiagnostic[]>();

  // Initialize for all open documents (to clear old diagnostics)
  for (const doc of server.documents.documents.values()) {
    diagnosticsByFile.set(doc.uri, []);
  }

  // Convert compiler diagnostics to LSP diagnostics
  for (const diag of result.diagnostics) {
    // Map global offset to file-local offset
    const localStart = globalToLocal(result.fileRegistry, diag.start);
    if (!localStart) continue;

    const file = getFileById(result.fileRegistry, localStart.fileId);
    if (!file) continue;

    // Skip diagnostics from prelude
    if (file.path === "<prelude>") continue;

    const lineIndex = result.lineIndices.get(file.path);
    if (!lineIndex) continue;

    // Calculate local end offset
    const localEnd = diag.end - diag.start + localStart.localOffset;

    const range = spanToRange(lineIndex, { start: localStart.localOffset, end: localEnd });
    const severity: DiagnosticSeverity = diag.severity === "error" ? 1 : 2;

    const lspDiag: LSPDiagnostic = {
      range,
      message: diag.message,
      severity,
      source: "algow",
    };

    const uri = pathToUri(file.path);
    const existing = diagnosticsByFile.get(uri) ?? [];
    existing.push(lspDiag);
    diagnosticsByFile.set(uri, existing);
  }

  // Publish diagnostics for each file
  for (const [uri, diagnostics] of diagnosticsByFile) {
    sendNotification(server, "textDocument/publishDiagnostics", { uri, diagnostics });
  }
};

// =============================================================================
// Hover
// =============================================================================

const handleHover = (server: LSPServer, id: number | string, params: unknown): void => {
  const { textDocument, position } = params as {
    textDocument: { uri: string };
    position: Position;
  };

  const result = server.analyzer.lastResult;
  if (!result) {
    sendResponse(server, id, null);
    return;
  }

  const path = uriToPath(textDocument.uri);
  const hover = getHoverAtPosition(result, path, position);

  if (hover) {
    sendResponse(server, id, {
      contents: { kind: "markdown", value: hover.contents },
      range: hover.range,
    });
  } else {
    sendResponse(server, id, null);
  }
};

// =============================================================================
// Go to Definition
// =============================================================================

const handleDefinition = (server: LSPServer, id: number | string, params: unknown): void => {
  const { textDocument, position } = params as {
    textDocument: { uri: string };
    position: Position;
  };

  const result = server.analyzer.lastResult;
  if (!result) {
    sendResponse(server, id, null);
    return;
  }

  const path = uriToPath(textDocument.uri);
  const def = getDefinitionAtPosition(result, path, position);

  if (def) {
    sendResponse(server, id, {
      uri: pathToUri(def.path),
      range: def.range,
    });
  } else {
    sendResponse(server, id, null);
  }
};

// =============================================================================
// Find References
// =============================================================================

const handleReferences = (server: LSPServer, id: number | string, params: unknown): void => {
  const { textDocument, position, context } = params as {
    textDocument: { uri: string };
    position: Position;
    context: { includeDeclaration: boolean };
  };

  const result = server.analyzer.lastResult;
  if (!result) {
    sendResponse(server, id, []);
    return;
  }

  const path = uriToPath(textDocument.uri);
  const refs = getReferencesAtPosition(result, path, position, context.includeDeclaration);

  const locations = refs.map((ref) => ({
    uri: pathToUri(ref.path),
    range: ref.range,
  }));

  sendResponse(server, id, locations);
};

// =============================================================================
// Rename
// =============================================================================

const handleRename = (server: LSPServer, id: number | string, params: unknown): void => {
  const { textDocument, position, newName } = params as {
    textDocument: { uri: string };
    position: Position;
    newName: string;
  };

  const result = server.analyzer.lastResult;
  if (!result) {
    sendResponse(server, id, null);
    return;
  }

  const path = uriToPath(textDocument.uri);
  const refs = getReferencesAtPosition(result, path, position, true);

  if (refs.length === 0) {
    sendResponse(server, id, null);
    return;
  }

  // Group edits by document
  const changes: Record<string, { range: Range; newText: string }[]> = {};

  for (const ref of refs) {
    const uri = pathToUri(ref.path);
    if (!changes[uri]) {
      changes[uri] = [];
    }
    changes[uri].push({ range: ref.range, newText: newName });
  }

  sendResponse(server, id, { changes });
};

// =============================================================================
// Completion
// =============================================================================

const handleCompletion = (server: LSPServer, id: number | string, params: unknown): void => {
  const { textDocument, position } = params as {
    textDocument: { uri: string };
    position: Position;
  };

  const result = server.analyzer.lastResult;
  if (!result) {
    sendResponse(server, id, []);
    return;
  }

  const path = uriToPath(textDocument.uri);
  const completions = getCompletionsAtPosition(result, path, position);

  if (!completions) {
    sendResponse(server, id, []);
    return;
  }

  const items = completions.map((c) => ({
    label: c.label,
    kind: kindToCompletionItemKind(c.kind),
    detail: c.detail,
  }));

  sendResponse(server, id, items);
};

const kindToCompletionItemKind = (kind: string): CompletionItemKind => {
  switch (kind) {
    case "Function":
      return 3; // Function
    case "Variable":
      return 6; // Variable
    case "Constructor":
      return 4; // Constructor
    case "Module":
      return 9; // Module
    default:
      return 6; // Variable as default
  }
};

// =============================================================================
// Document Symbols
// =============================================================================

const handleDocumentSymbol = (server: LSPServer, id: number | string, params: unknown): void => {
  const { textDocument } = params as { textDocument: { uri: string } };

  const result = server.analyzer.lastResult;
  if (!result) {
    sendResponse(server, id, []);
    return;
  }

  const path = uriToPath(textDocument.uri);
  const symbols = getDocumentSymbols(result, path);

  const lspSymbols = symbols.map((s) => ({
    name: s.name,
    kind: kindToSymbolKind(s.kind),
    range: s.range,
    selectionRange: s.range,
    detail: s.detail,
  }));

  sendResponse(server, id, lspSymbols);
};

const kindToSymbolKind = (kind: string): SymbolKind => {
  switch (kind) {
    case "function":
      return 12;
    case "variable":
      return 13;
    case "type":
      return 5;
    case "constructor":
      return 22;
    default:
      return 13;
  }
};

// =============================================================================
// Formatting
// =============================================================================

const handleFormatting = (server: LSPServer, id: number | string, params: unknown): void => {
  const { textDocument } = params as { textDocument: { uri: string } };

  const doc = getDocument(server.documents, textDocument.uri);
  if (!doc) {
    sendResponse(server, id, []);
    return;
  }

  const formatResult = formatSource(doc.content);

  if (formatResult.diagnostics.some((d) => d.severity === "error")) {
    // Formatting failed due to parse errors
    sendResponse(server, id, []);
    return;
  }

  // Return a single edit that replaces the entire document
  const lineCount = doc.lineIndex.lineStarts.length;
  const lastLineStart = doc.lineIndex.lineStarts[lineCount - 1] ?? 0;
  const lastLineLength = doc.content.length - lastLineStart;

  const fullRange: Range = {
    start: { line: 0, character: 0 },
    end: { line: lineCount - 1, character: lastLineLength },
  };

  sendResponse(server, id, [{ range: fullRange, newText: formatResult.formatted }]);
};
