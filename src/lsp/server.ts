/**
 * LSP Server Core.
 *
 * Transport-agnostic LSP server implementation.
 * Handles document synchronization, diagnostics, hover, and go-to-definition.
 */

import type {
  CompletionItem,
  CompletionItemKind,
  CompletionList,
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
    completionProvider?: {
      triggerCharacters?: string[];
      resolveProvider?: boolean;
    };
  };
};

// LSP constants
const SYNC_FULL: TextDocumentSyncKind = 1;
const SEVERITY_ERROR = 1;

import * as ast from "../ast";
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
  type ForeignMap,
  type ModuleTypeEnv,
  processDeclarations,
  processModules,
  processUseStatements,
  type Type,
  type TypeEnv,
  type TypeMap,
  typeToString,
} from "../checker";
import type { Diagnostic } from "../diagnostics";
import { createConstructorEnv, evaluate, RuntimeError, valueToString } from "../eval";
import { parse, programToExpr, type Program } from "../parser";
import { modules as preludeModules } from "../prelude";
import { lowerToIR } from "../lower";
import { generateJS } from "../backend/js";
import { generateGo } from "../backend/go";
import { printIR } from "../ir";
import { optimize } from "../optimize";

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
// PRELUDE CACHING
// =============================================================================

/** Implicit use statements for prelude modules (import everything) */
const preludeUses: ast.UseDecl[] = preludeModules.map((mod) =>
  ast.useDecl(mod.name, ast.importAll()),
);

/**
 * Cached prelude environment.
 * Computed once at module load time to avoid reprocessing on every keystroke.
 */
const preludeCache = (() => {
  const moduleEnv = processModules(preludeModules);
  const { localEnv, localRegistry, constructorNames, foreignFunctions } = processUseStatements(
    preludeUses,
    moduleEnv,
  );
  return {
    moduleEnv,
    typeEnv: localEnv,
    registry: localRegistry,
    constructorNames: [...constructorNames],
    foreignFunctions,
  };
})();

/** Process a program, reusing cached prelude environment */
const processProgram = (program: Program) => {
  // Start with cached prelude
  const allModules = [...preludeModules, ...program.modules];
  const allUses = [...preludeUses, ...program.uses];

  // Only process user modules (prelude already processed)
  if (program.modules.length > 0) {
    // User has custom modules - need to reprocess all
    const moduleEnv = processModules(allModules);
    // Process prelude first (no collision checking)
    const {
      localEnv: preludeEnv,
      localRegistry: preludeRegistry,
      constructorNames: preludeConstructors,
      foreignFunctions: preludeForeign,
      aliases: preludeAliases,
    } = processUseStatements(preludeUses, moduleEnv);
    // Process user use statements with collision checking against prelude
    const {
      localEnv: userEnv,
      localRegistry: userRegistry,
      constructorNames: userConstructors,
      foreignFunctions: userForeign,
      aliases: userAliases,
      diagnostics: useDiagnostics,
    } = processUseStatements(program.uses, moduleEnv, preludeEnv);
    // Merge environments
    const localEnv = new Map(preludeEnv);
    for (const [k, v] of userEnv) localEnv.set(k, v);
    const localRegistry = new Map(preludeRegistry);
    for (const [k, v] of userRegistry) localRegistry.set(k, v);
    const foreignFunctions = new Map(preludeForeign);
    for (const [k, v] of userForeign) foreignFunctions.set(k, v);
    const aliases = new Map(preludeAliases);
    for (const [k, v] of userAliases) aliases.set(k, v);
    // Process top-level type declarations
    const {
      typeEnv: declEnv,
      registry: declRegistry,
      constructorNames: declConstructors,
    } = processDeclarations(program.declarations);
    // Merge type declarations into the environment
    const typeEnv = new Map(localEnv);
    for (const [k, v] of declEnv) typeEnv.set(k, v);
    const registry = new Map(localRegistry);
    for (const [k, v] of declRegistry) registry.set(k, v);
    return {
      typeEnv,
      registry,
      constructorNames: [...preludeConstructors, ...userConstructors, ...declConstructors],
      allModules,
      allUses,
      moduleEnv,
      aliases,
      foreignFunctions,
      useDiagnostics,
    };
  }

  // No user modules - use cached prelude, only process user use statements
  if (program.uses.length > 0) {
    const {
      localEnv: userEnv,
      localRegistry: userRegistry,
      constructorNames: userConstructors,
      foreignFunctions: userForeign,
      aliases,
      diagnostics: useDiagnostics,
    } = processUseStatements(program.uses, preludeCache.moduleEnv, preludeCache.typeEnv);
    // Merge with prelude
    const localEnv = new Map(preludeCache.typeEnv);
    for (const [k, v] of userEnv) localEnv.set(k, v);
    const localRegistry = new Map(preludeCache.registry);
    for (const [k, v] of userRegistry) localRegistry.set(k, v);
    const foreignFunctions = new Map(preludeCache.foreignFunctions);
    for (const [k, v] of userForeign) foreignFunctions.set(k, v);
    // Process top-level type declarations
    const {
      typeEnv: declEnv,
      registry: declRegistry,
      constructorNames: declConstructors,
    } = processDeclarations(program.declarations);
    // Merge type declarations into the environment
    const typeEnv = new Map(localEnv);
    for (const [k, v] of declEnv) typeEnv.set(k, v);
    const registry = new Map(localRegistry);
    for (const [k, v] of declRegistry) registry.set(k, v);
    return {
      typeEnv,
      registry,
      constructorNames: [
        ...preludeCache.constructorNames,
        ...userConstructors,
        ...declConstructors,
      ],
      allModules,
      allUses,
      moduleEnv: preludeCache.moduleEnv,
      aliases,
      foreignFunctions,
      useDiagnostics,
    };
  }

  // No user modules or uses - but may have type declarations
  // Process top-level type declarations
  const {
    typeEnv: declEnv,
    registry: declRegistry,
    constructorNames: declConstructors,
  } = processDeclarations(program.declarations);
  // Merge type declarations into the environment
  const typeEnv = new Map(preludeCache.typeEnv);
  for (const [k, v] of declEnv) typeEnv.set(k, v);
  const registry = new Map(preludeCache.registry);
  for (const [k, v] of declRegistry) registry.set(k, v);
  return {
    typeEnv,
    registry,
    constructorNames: [...preludeCache.constructorNames, ...declConstructors],
    allModules,
    allUses,
    moduleEnv: preludeCache.moduleEnv,
    aliases: new Map<string, string>(),
    foreignFunctions: preludeCache.foreignFunctions,
    useDiagnostics: [] as Diagnostic[],
  };
};

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
  readonly typeEnv: TypeEnv;
  readonly program: Program | null;
  readonly registry: ConstructorRegistry;
  readonly constructorNames: readonly string[];
  readonly allModules: readonly ast.ModuleDecl[];
  readonly allUses: readonly ast.UseDecl[];
  readonly moduleEnv: ModuleTypeEnv;
  readonly foreignFunctions: ForeignMap;
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

type CompileParams = {
  readonly textDocument: TextDocumentIdentifier;
  readonly optimize?: boolean;
};

type CompileResult = {
  readonly success: boolean;
  readonly code?: string;
  readonly error?: string;
};

type EmitIRParams = {
  readonly textDocument: TextDocumentIdentifier;
  readonly optimize?: boolean;
};

type EmitIRResult = {
  readonly success: boolean;
  readonly ir?: string;
  readonly error?: string;
};

type CompileGoParams = {
  readonly textDocument: TextDocumentIdentifier;
  readonly optimize?: boolean;
};

type CompileGoResult = {
  readonly success: boolean;
  readonly code?: string;
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
        case "textDocument/completion":
          transport.send(
            successResponse(id, handleCompletion(params as TextDocumentPositionParams)),
          );
          break;
        case "algow/evaluate":
          transport.send(successResponse(id, handleEvaluate(params as EvaluateParams)));
          break;
        case "algow/compile":
          transport.send(successResponse(id, handleCompile(params as CompileParams)));
          break;
        case "algow/emitIR":
          transport.send(successResponse(id, handleEmitIR(params as EmitIRParams)));
          break;
        case "algow/compileGo":
          transport.send(successResponse(id, handleCompileGo(params as CompileGoParams)));
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
      completionProvider: {
        triggerCharacters: ["."],
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

  // LSP CompletionItemKind constants
  const COMPLETION_KIND_FUNCTION = 3 as CompletionItemKind;
  const COMPLETION_KIND_CONSTRUCTOR = 4 as CompletionItemKind;
  const COMPLETION_KIND_FIELD = 5 as CompletionItemKind;
  const COMPLETION_KIND_KEYWORD = 14 as CompletionItemKind;

  // LSP CompletionItemKind for modules
  const COMPLETION_KIND_MODULE = 9 as CompletionItemKind;

  const handleCompletion = (params: TextDocumentPositionParams): CompletionList | null => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc) return null;

    const offset = positionToOffset(doc.text, params.position);
    const items: CompletionItem[] = [];

    // Check if we're after a dot (record field access or module member access)
    const beforeCursor = doc.text.slice(0, offset);
    const dotMatch = beforeCursor.match(/(\w+)\s*\.\s*$/);

    if (dotMatch) {
      const name = dotMatch[1]!;

      // Check if this is a module name (starts with uppercase)
      if (name[0] && name[0] === name[0].toUpperCase() && /[A-Z]/.test(name[0])) {
        // Module member completion
        const moduleItems = getModuleMemberCompletions(doc, name);
        if (moduleItems.length > 0) {
          items.push(...moduleItems);
        } else {
          // Fall back to record field completion if no module found
          const fieldItems = getRecordFieldCompletions(doc, name);
          items.push(...fieldItems);
        }
      } else {
        // Record field completion (lowercase identifier)
        const fieldItems = getRecordFieldCompletions(doc, name);
        items.push(...fieldItems);
      }
    } else {
      // General completions from typeEnv (includes prelude), constructors, keywords, modules
      items.push(...getTypeEnvCompletions(doc));
      items.push(...getConstructorCompletions(doc));
      items.push(...getKeywordCompletions());
      items.push(...getModuleCompletions(doc));
    }

    return { isIncomplete: false, items };
  };

  /**
   * Get completions for record fields when cursor is after "varName."
   */
  const getRecordFieldCompletions = (doc: DocumentState, varName: string): CompletionItem[] => {
    if (!doc.symbols || !doc.types) return [];

    // Find the definition of this variable
    for (const def of doc.symbols.definitions) {
      if (def.name === varName) {
        const type = doc.types.get(def);
        if (type) {
          return extractRecordFields(type);
        }
      }
    }

    return [];
  };

  /**
   * Extract field names from a record type, resolving through type variables.
   */
  const extractRecordFields = (type: Type): CompletionItem[] => {
    const items: CompletionItem[] = [];

    // Resolve the type to find TRecord
    const resolved = resolveType(type);
    if (resolved.kind === "TRecord") {
      for (const [fieldName, fieldType] of resolved.fields) {
        items.push({
          label: fieldName,
          kind: COMPLETION_KIND_FIELD,
          detail: typeToString(fieldType),
        });
      }
    } else if (resolved.kind === "TTuple") {
      // For tuples, suggest .0, .1, etc.
      for (let i = 0; i < resolved.elements.length; i++) {
        items.push({
          label: String(i),
          kind: COMPLETION_KIND_FIELD,
          detail: typeToString(resolved.elements[i]!),
        });
      }
    }

    return items;
  };

  /**
   * Resolve type through TVar references if needed.
   * For now, just return the type as-is since we don't have substitution here.
   */
  const resolveType = (type: Type): Type => {
    // In the future, we could apply the final substitution here
    return type;
  };

  /**
   * Get completions from typeEnv (all available functions including prelude).
   */
  const getTypeEnvCompletions = (doc: DocumentState): CompletionItem[] => {
    const items: CompletionItem[] = [];
    for (const [name, scheme] of doc.typeEnv) {
      items.push({
        label: name,
        kind: COMPLETION_KIND_FUNCTION,
        detail: typeToString(scheme.type),
      });
    }
    return items;
  };

  /**
   * Get completions for constructors.
   */
  const getConstructorCompletions = (doc: DocumentState): CompletionItem[] => {
    const items: CompletionItem[] = [];
    for (const [typeName, constructors] of doc.registry) {
      for (const conName of constructors) {
        items.push({
          label: conName,
          kind: COMPLETION_KIND_CONSTRUCTOR,
          detail: typeName,
        });
      }
    }
    return items;
  };

  /**
   * Get keyword completions.
   */
  const getKeywordCompletions = (): CompletionItem[] => {
    const keywords = [
      "let",
      "rec",
      "in",
      "if",
      "then",
      "else",
      "match",
      "when",
      "end",
      "type",
      "module",
      "use",
      "as",
      "and",
      "true",
      "false",
    ];

    return keywords.map((kw) => ({
      label: kw,
      kind: COMPLETION_KIND_KEYWORD,
    }));
  };

  /**
   * Get completions for module names (for use statements or qualified access).
   */
  const getModuleCompletions = (doc: DocumentState): CompletionItem[] => {
    const items: CompletionItem[] = [];
    for (const moduleName of doc.moduleEnv.keys()) {
      items.push({
        label: moduleName,
        kind: COMPLETION_KIND_MODULE,
        detail: "module",
      });
    }
    return items;
  };

  /**
   * Get completions for members of a module (after Module.)
   */
  const getModuleMemberCompletions = (doc: DocumentState, moduleName: string): CompletionItem[] => {
    const items: CompletionItem[] = [];
    const moduleInfo = doc.moduleEnv.get(moduleName);

    if (!moduleInfo) return items;

    // Add all bindings from the module
    for (const [name, scheme] of moduleInfo.typeEnv) {
      items.push({
        label: name,
        kind: COMPLETION_KIND_FUNCTION,
        detail: typeToString(scheme.type),
      });
    }

    // Add all constructors from the module
    for (const [typeName, constructors] of moduleInfo.registry) {
      for (const conName of constructors) {
        items.push({
          label: conName,
          kind: COMPLETION_KIND_CONSTRUCTOR,
          detail: typeName,
        });
      }
    }

    return items;
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

    const expr = programToExpr(doc.program, doc.allModules, doc.allUses);
    if (!expr) {
      return { success: false, error: "No expression to evaluate" };
    }

    try {
      const constructorEnv = createConstructorEnv(doc.constructorNames);
      const result = evaluate(constructorEnv, expr);
      return { success: true, value: valueToString(result) };
    } catch (err) {
      if (err instanceof RuntimeError) {
        return { success: false, error: err.message };
      }
      return { success: false, error: (err as Error).message };
    }
  };

  const handleCompile = (params: CompileParams): CompileResult => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }

    // Don't compile if there are errors
    if (doc.diagnostics.some((d) => d.severity === SEVERITY_ERROR)) {
      return { success: false, error: "Cannot compile: document has errors" };
    }

    const expr = programToExpr(doc.program, doc.allModules, doc.allUses);
    if (!expr) {
      return { success: false, error: "No expression to compile" };
    }

    try {
      // Type check to get CheckOutput (needed for lowering)
      const checkResult = check(doc.typeEnv, doc.registry, expr, doc.symbols!, doc.moduleEnv);

      // Lower to IR
      let ir = lowerToIR(expr, doc.typeEnv, checkResult, doc.foreignFunctions, doc.moduleEnv);

      // Optimize if requested
      if (params.optimize) {
        ir = optimize(ir);
      }

      // Generate JavaScript
      const { code } = generateJS(ir, doc.constructorNames);

      return { success: true, code };
    } catch (err) {
      return { success: false, error: (err as Error).message };
    }
  };

  const handleEmitIR = (params: EmitIRParams): EmitIRResult => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }

    // Don't emit IR if there are errors
    if (doc.diagnostics.some((d) => d.severity === SEVERITY_ERROR)) {
      return { success: false, error: "Cannot emit IR: document has errors" };
    }

    const expr = programToExpr(doc.program, doc.allModules, doc.allUses);
    if (!expr) {
      return { success: false, error: "No expression to compile" };
    }

    try {
      // Type check to get CheckOutput (needed for lowering)
      const checkResult = check(doc.typeEnv, doc.registry, expr, doc.symbols!, doc.moduleEnv);

      // Lower to IR
      let ir = lowerToIR(expr, doc.typeEnv, checkResult, doc.foreignFunctions, doc.moduleEnv);

      // Optimize if requested
      if (params.optimize) {
        ir = optimize(ir);
      }

      // Print IR
      const irText = printIR(ir);

      return { success: true, ir: irText };
    } catch (err) {
      return { success: false, error: (err as Error).message };
    }
  };

  const handleCompileGo = (params: CompileGoParams): CompileGoResult => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }

    // Don't compile if there are errors
    if (doc.diagnostics.some((d) => d.severity === SEVERITY_ERROR)) {
      return { success: false, error: "Cannot compile: document has errors" };
    }

    const expr = programToExpr(doc.program, doc.allModules, doc.allUses);
    if (!expr) {
      return { success: false, error: "No expression to compile" };
    }

    try {
      // Type check to get CheckOutput (needed for lowering)
      const checkResult = check(doc.typeEnv, doc.registry, expr, doc.symbols!, doc.moduleEnv);

      // Lower to IR
      let ir = lowerToIR(expr, doc.typeEnv, checkResult, doc.foreignFunctions, doc.moduleEnv);

      // Optimize if requested
      if (params.optimize) {
        ir = optimize(ir);
      }

      // Generate Go
      const { code } = generateGo(ir, doc.constructorNames);

      return { success: true, code };
    } catch (err) {
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

    // Process prelude + user modules
    const {
      typeEnv,
      registry,
      constructorNames,
      allModules,
      allUses,
      moduleEnv,
      aliases,
      foreignFunctions,
      useDiagnostics,
    } = processProgram(parseResult.program);

    // Add use statement diagnostics (shadowing errors)
    for (const diag of useDiagnostics) {
      lspDiagnostics.push(convertDiagnostic(text, diag));
    }

    // Convert to expression (includes module bindings)
    const expr = programToExpr(parseResult.program, allModules, allUses);
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
      const checkResult = check(typeEnv, registry, expr, symbols, moduleEnv, aliases);
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
      typeEnv,
      program: parseResult.program,
      registry,
      constructorNames,
      allModules,
      allUses,
      moduleEnv,
      foreignFunctions,
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
