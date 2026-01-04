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

import type { Diagnostic } from "../diagnostics";
import { createConstructorEnv, evaluate, RuntimeError, valueToString } from "../eval";
import {
  type ConstructorRegistry,
  infer,
  type InferOutput,
  mergeEnvs,
  mergeRegistries,
  processDataDecl,
  type TypeEnv,
  typeToString,
} from "../infer";
import { parse, programToExpr, type Program } from "../parser";
import { declarations as preludeDeclarations } from "../prelude";
import { findAllOccurrences, findDefinitionAt, findReferenceAt, goToDefinition } from "../symbols";

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
  readonly inferResult: InferOutput | null;
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
      textDocumentSync: 1 as TextDocumentSyncKind, // Full sync
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
    if (!doc || !doc.inferResult) return null;

    const offset = positionToOffset(doc.text, params.position);
    const { symbols } = doc.inferResult;

    // Check if hovering over a definition
    const def = findDefinitionAt(symbols, offset);
    if (def && def.type) {
      return {
        contents: {
          kind: "markdown" as MarkupKind,
          value: `\`\`\`algow\n${def.name}: ${typeToString(def.type)}\n\`\`\``,
        },
        range: spanToRange(doc.text, def.span),
      };
    }

    // Check if hovering over a reference
    const ref = findReferenceAt(symbols, offset);
    if (ref && ref.definition && ref.definition.type) {
      return {
        contents: {
          kind: "markdown" as MarkupKind,
          value: `\`\`\`algow\n${ref.name}: ${typeToString(ref.definition.type)}\n\`\`\``,
        },
        range: spanToRange(doc.text, ref.span),
      };
    }

    return null;
  };

  const handleDefinition = (params: TextDocumentPositionParams): Location | null => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.inferResult) return null;

    const offset = positionToOffset(doc.text, params.position);
    const def = goToDefinition(doc.inferResult.symbols, offset);

    if (!def) return null;

    return {
      uri: params.textDocument.uri,
      range: spanToRange(doc.text, def.span),
    };
  };

  const handlePrepareRename = (params: TextDocumentPositionParams): Range | null => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.inferResult) return null;

    const offset = positionToOffset(doc.text, params.position);
    const { definition } = findAllOccurrences(doc.inferResult.symbols, offset);

    if (!definition) return null;

    // Return the range of the symbol at the cursor position
    // First check if we're on a reference
    const ref = findReferenceAt(doc.inferResult.symbols, offset);
    if (ref) {
      return spanToRange(doc.text, ref.span);
    }

    // Otherwise check if we're on a definition
    const def = findDefinitionAt(doc.inferResult.symbols, offset);
    if (def) {
      return spanToRange(doc.text, def.span);
    }

    return null;
  };

  const handleRename = (params: RenameParams): WorkspaceEdit | null => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.inferResult) return null;

    const offset = positionToOffset(doc.text, params.position);
    const { definition, references } = findAllOccurrences(doc.inferResult.symbols, offset);

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
    if (doc.diagnostics.some((d) => d.severity === 1)) {
      return { success: false, error: "Cannot evaluate: document has errors" };
    }

    const expr = programToExpr(doc.program);
    if (!expr) {
      return { success: false, error: "No expression to evaluate" };
    }

    try {
      // Extract constructor names from prelude + user declarations
      const constructorNames: string[] = [];
      for (const decl of preludeDeclarations) {
        for (const con of decl.constructors) {
          constructorNames.push(con.name);
        }
      }
      for (const decl of doc.program.declarations) {
        for (const con of decl.constructors) {
          constructorNames.push(con.name);
        }
      }

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

    // Type check if we have an expression
    const expr = programToExpr(parseResult.program);
    let inferResult: InferOutput | null = null;

    // Build type environment from prelude + user declarations
    let typeEnv: TypeEnv = new Map();
    let registry: ConstructorRegistry = new Map();

    for (const decl of preludeDeclarations) {
      const [newEnv, newReg] = processDataDecl(decl);
      typeEnv = mergeEnvs(typeEnv, newEnv);
      registry = mergeRegistries(registry, newReg);
    }

    for (const decl of parseResult.program.declarations) {
      const [newEnv, newReg] = processDataDecl(decl);
      typeEnv = mergeEnvs(typeEnv, newEnv);
      registry = mergeRegistries(registry, newReg);
    }

    if (expr) {
      inferResult = infer(typeEnv, registry, expr);

      for (const diag of inferResult.diagnostics) {
        lspDiagnostics.push(convertDiagnostic(text, diag));
      }
    }

    return {
      uri,
      version,
      text,
      diagnostics: lspDiagnostics,
      inferResult,
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
