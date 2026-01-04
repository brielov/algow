/**
 * LSP Server Core.
 *
 * Transport-agnostic LSP server implementation.
 * Handles document synchronization, diagnostics, hover, and go-to-definition.
 */

import type {
  Diagnostic as LspDiagnostic,
  DiagnosticSeverity,
  Hover,
  Location,
  MarkupKind,
  Range,
  TextEdit,
  WorkspaceEdit,
} from "vscode-languageserver-types";

// Types not exported from vscode-languageserver-types (they're in -protocol)
type TextDocumentSyncKind = 0 | 1 | 2; // None | Full | Incremental
type InitializeResult = {
  capabilities: {
    textDocumentSync?: TextDocumentSyncKind;
    hoverProvider?: boolean;
    definitionProvider?: boolean;
    renameProvider?: boolean | { prepareProvider?: boolean };
  };
};

// LSP constants
const SYNC_FULL: TextDocumentSyncKind = 1;
const SEVERITY_ERROR = 1;

import {
  bindWithConstructors,
  findAllOccurrences,
  findDefinitionAt,
  findReferenceAt,
  goToDefinition,
  type SymbolTable,
} from "../binder";
import {
  check,
  type ConstructorRegistry,
  processDeclarations,
  type TypeMap,
  typeToString,
} from "../checker";
import type { Diagnostic } from "../diagnostics";
import { createConstructorEnv, evaluate, RuntimeError, valueToString } from "../eval";
import { parse, programToExpr, type Program } from "../parser";
import { declarations as preludeDeclarations } from "../prelude";

import { positionToOffset, spanToRange } from "./positions";
import {
  errorResponse,
  isNotification,
  isRequest,
  type JsonRpcNotification,
  type JsonRpcRequest,
  notification,
  successResponse,
  type Transport,
} from "./transport";

// =============================================================================
// DOCUMENT STATE
// =============================================================================

type DocumentState = {
  readonly uri: string;
  readonly version: number;
  readonly text: string;
  readonly diagnostics: readonly LspDiagnostic[];
  readonly symbols: SymbolTable | null;
  readonly types: TypeMap | null;
  readonly program: Program | null;
  readonly registry: ConstructorRegistry;
};

// =============================================================================
// LSP PARAMETER TYPES
// =============================================================================

type InitializeParams = {
  readonly capabilities: unknown;
  readonly rootUri?: string | null;
};

type TextDocumentItem = {
  readonly uri: string;
  readonly languageId: string;
  readonly version: number;
  readonly text: string;
};

type TextDocumentIdentifier = {
  readonly uri: string;
};

type VersionedTextDocumentIdentifier = TextDocumentIdentifier & {
  readonly version: number;
};

type TextDocumentContentChangeEvent = {
  readonly text: string;
};

type DidOpenTextDocumentParams = {
  readonly textDocument: TextDocumentItem;
};

type DidChangeTextDocumentParams = {
  readonly textDocument: VersionedTextDocumentIdentifier;
  readonly contentChanges: readonly TextDocumentContentChangeEvent[];
};

type DidCloseTextDocumentParams = {
  readonly textDocument: TextDocumentIdentifier;
};

type TextDocumentPositionParams = {
  readonly textDocument: TextDocumentIdentifier;
  readonly position: { readonly line: number; readonly character: number };
};

type RenameParams = TextDocumentPositionParams & {
  readonly newName: string;
};

type EvaluateParams = {
  readonly textDocument: TextDocumentIdentifier;
};

type EvaluateResult = {
  readonly success: boolean;
  readonly value?: string;
  readonly error?: string;
};

// =============================================================================
// SERVER CREATION
// =============================================================================

/**
 * Create an LSP server connected to the given transport.
 */
export const createServer = (transport: Transport): void => {
  const documents = new Map<string, DocumentState>();

  transport.onMessage((message) => {
    if (isRequest(message)) {
      handleRequest(message);
    } else if (isNotification(message)) {
      handleNotification(message);
    }
  });

  transport.onClose(() => {
    // Cleanup if needed
  });

  // Handle requests (need response)
  const handleRequest = (request: JsonRpcRequest): void => {
    const { id, method, params } = request;

    try {
      switch (method) {
        case "initialize":
          transport.send(successResponse(id, handleInitialize(params as InitializeParams)));
          break;
        case "shutdown":
          transport.send(successResponse(id, null));
          break;
        case "textDocument/hover":
          transport.send(successResponse(id, handleHover(params as TextDocumentPositionParams)));
          break;
        case "textDocument/definition":
          transport.send(
            successResponse(id, handleDefinition(params as TextDocumentPositionParams)),
          );
          break;
        case "textDocument/prepareRename":
          transport.send(
            successResponse(id, handlePrepareRename(params as TextDocumentPositionParams)),
          );
          break;
        case "textDocument/rename":
          transport.send(successResponse(id, handleRename(params as RenameParams)));
          break;
        case "algow/evaluate":
          transport.send(successResponse(id, handleEvaluate(params as EvaluateParams)));
          break;
        default:
          transport.send(errorResponse(id, -32601, `Method not found: ${method}`));
      }
    } catch (err) {
      transport.send(errorResponse(id, -32603, (err as Error).message));
    }
  };

  // Handle notifications (no response)
  const handleNotification = (msg: JsonRpcNotification): void => {
    const { method, params } = msg;

    switch (method) {
      case "initialized":
        // Client is ready
        break;
      case "textDocument/didOpen":
        handleDidOpen(params as DidOpenTextDocumentParams);
        break;
      case "textDocument/didChange":
        handleDidChange(params as DidChangeTextDocumentParams);
        break;
      case "textDocument/didClose":
        handleDidClose(params as DidCloseTextDocumentParams);
        break;
      case "exit":
        process.exit(0);
        break;
    }
  };

  const handleInitialize = (_params: InitializeParams): InitializeResult => ({
    capabilities: {
      textDocumentSync: SYNC_FULL,
      hoverProvider: true,
      definitionProvider: true,
      renameProvider: {
        prepareProvider: true,
      },
    },
  });

  const handleDidOpen = (params: DidOpenTextDocumentParams): void => {
    const { uri, version, text } = params.textDocument;
    const state = analyzeDocument(uri, version, text);
    documents.set(uri, state);
    publishDiagnostics(uri, state.diagnostics);
  };

  const handleDidChange = (params: DidChangeTextDocumentParams): void => {
    const { uri, version } = params.textDocument;
    // Full sync: take the last change event's text
    const text = params.contentChanges[params.contentChanges.length - 1]?.text ?? "";
    const state = analyzeDocument(uri, version, text);
    documents.set(uri, state);
    publishDiagnostics(uri, state.diagnostics);
  };

  const handleDidClose = (params: DidCloseTextDocumentParams): void => {
    const { uri } = params.textDocument;
    documents.delete(uri);
    // Clear diagnostics
    publishDiagnostics(uri, []);
  };

  const handleHover = (params: TextDocumentPositionParams): Hover | null => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc?.symbols || !doc.types) return null;

    const offset = positionToOffset(doc.text, params.position);
    const { symbols, types } = doc;

    // Check reference first (more common), then definition
    const ref = findReferenceAt(symbols, offset);
    if (ref?.definition) {
      const type = types.get(ref.definition);
      if (type) {
        return {
          contents: {
            kind: "markdown" as MarkupKind,
            value: `\`\`\`algow\n${ref.name}: ${typeToString(type)}\n\`\`\``,
          },
          range: spanToRange(doc.text, ref.span),
        };
      }
    }

    const def = findDefinitionAt(symbols, offset);
    if (def) {
      const type = types.get(def);
      if (type) {
        return {
          contents: {
            kind: "markdown" as MarkupKind,
            value: `\`\`\`algow\n${def.name}: ${typeToString(type)}\n\`\`\``,
          },
          range: spanToRange(doc.text, def.span),
        };
      }
    }

    return null;
  };

  const handleDefinition = (params: TextDocumentPositionParams): Location | null => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.symbols) return null;

    const offset = positionToOffset(doc.text, params.position);
    const def = goToDefinition(doc.symbols, offset);

    if (!def) return null;

    return {
      uri: params.textDocument.uri,
      range: spanToRange(doc.text, def.span),
    };
  };

  const handlePrepareRename = (params: TextDocumentPositionParams): Range | null => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc?.symbols) return null;

    const offset = positionToOffset(doc.text, params.position);

    // Check reference first, then definition
    const ref = findReferenceAt(doc.symbols, offset);
    if (ref?.definition) return spanToRange(doc.text, ref.span);

    const def = findDefinitionAt(doc.symbols, offset);
    if (def) return spanToRange(doc.text, def.span);

    return null;
  };

  const handleRename = (params: RenameParams): WorkspaceEdit | null => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.symbols) return null;

    const offset = positionToOffset(doc.text, params.position);
    const { definition, references } = findAllOccurrences(doc.symbols, offset);

    if (!definition) return null;

    // Collect all edits: definition + all references
    const edits: TextEdit[] = [];

    // Add edit for the definition
    edits.push({
      range: spanToRange(doc.text, definition.span),
      newText: params.newName,
    });

    // Add edits for all references
    for (const ref of references) {
      edits.push({
        range: spanToRange(doc.text, ref.span),
        newText: params.newName,
      });
    }

    return {
      changes: {
        [params.textDocument.uri]: edits,
      },
    };
  };

  const handleEvaluate = (params: EvaluateParams): EvaluateResult => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }

    // Don't evaluate if there are errors
    if (doc.diagnostics.some((d) => d.severity === SEVERITY_ERROR)) {
      return { success: false, error: "Cannot evaluate: document has errors" };
    }

    const expr = programToExpr(doc.program);
    if (!expr) {
      return { success: false, error: "No expression to evaluate" };
    }

    try {
      // Get constructor names from prelude + user declarations
      const prelude = processDeclarations(preludeDeclarations);
      const { constructorNames } = processDeclarations(doc.program.declarations, prelude);

      const constructorEnv = createConstructorEnv(constructorNames);
      const result = evaluate(constructorEnv, expr);
      return { success: true, value: valueToString(result) };
    } catch (err) {
      if (err instanceof RuntimeError) {
        return { success: false, error: err.message };
      }
      return { success: false, error: (err as Error).message };
    }
  };

  // Analyze document and produce diagnostics
  const analyzeDocument = (uri: string, version: number, text: string): DocumentState => {
    const lspDiagnostics: LspDiagnostic[] = [];

    // Parse
    const parseResult = parse(text);
    for (const diag of parseResult.diagnostics) {
      lspDiagnostics.push(convertDiagnostic(text, diag));
    }

    // Process prelude + user declarations
    const prelude = processDeclarations(preludeDeclarations);
    const { typeEnv, registry, constructorNames } = processDeclarations(
      parseResult.program.declarations,
      prelude,
    );

    // Bind and type check if we have an expression
    const expr = programToExpr(parseResult.program);
    let symbols: SymbolTable | null = null;
    let types: TypeMap | null = null;

    if (expr) {
      // Binding phase: resolve names and build symbol table
      const bindResult = bindWithConstructors(constructorNames, expr);
      symbols = bindResult.symbols;

      for (const diag of bindResult.diagnostics) {
        lspDiagnostics.push(convertDiagnostic(text, diag));
      }

      // Type checking phase: infer types
      const checkResult = check(typeEnv, registry, expr, symbols);
      types = checkResult.types;

      for (const diag of checkResult.diagnostics) {
        lspDiagnostics.push(convertDiagnostic(text, diag));
      }
    }

    return {
      uri,
      version,
      text,
      diagnostics: lspDiagnostics,
      symbols,
      types,
      program: parseResult.program,
      registry,
    };
  };

  const convertDiagnostic = (source: string, diag: Diagnostic): LspDiagnostic => {
    const severity: DiagnosticSeverity =
      diag.severity === "error" ? 1 : diag.severity === "warning" ? 2 : 3;

    return {
      range: spanToRange(source, { start: diag.start, end: diag.end }),
      message: diag.message,
      severity,
      source: "algow",
    };
  };

  const publishDiagnostics = (uri: string, diagnostics: readonly LspDiagnostic[]): void => {
    transport.send(notification("textDocument/publishDiagnostics", { uri, diagnostics }));
  };
};
