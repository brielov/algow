# Chapter 4: Language Server Protocol

The **Language Server Protocol** (LSP) enables rich editor integration—hover info, go-to-definition, autocomplete, and more. This chapter covers Algow's transport-agnostic LSP implementation.

---

## What is LSP?

Before LSP, every editor needed custom plugins for every language. An N×M problem—N editors times M languages. LSP reduces this to N+M: language authors implement one server, editor authors implement one client.

```
┌──────────────┐          ┌──────────────┐
│   VS Code    │◄────────►│              │
├──────────────┤          │              │
│    Neovim    │◄────────►│  Algow LSP   │
├──────────────┤   LSP    │    Server    │
│    Emacs     │◄────────►│              │
├──────────────┤          │              │
│   Monaco     │◄────────►│              │
└──────────────┘          └──────────────┘
```

The protocol uses JSON-RPC messages over some transport (stdio, TCP, WebSocket).

---

## Transport Abstraction

Algow's LSP works in two environments:
- **stdio**: Traditional editors (VS Code, Neovim)
- **Web Worker**: Browser (Monaco playground)

We abstract the transport:

```typescript
export type Transport = {
  /** Send a message from server to client */
  send(message: JsonRpcMessage): void;

  /** Register handler for incoming messages */
  onMessage(handler: (message: JsonRpcMessage) => void): void;

  /** Called when transport should shut down */
  onClose(handler: () => void): void;
};
```

The server doesn't know or care how messages travel—it just sends and receives JSON-RPC.

---

## JSON-RPC Messages

LSP uses JSON-RPC 2.0. Three message types:

### Request

Client asks for something, expects a response:

```typescript
export type JsonRpcRequest = {
  readonly jsonrpc: "2.0";
  readonly id: number | string;
  readonly method: string;
  readonly params?: unknown;
};
```

Example: "Give me hover info at this position"

### Response

Server answers a request:

```typescript
export type JsonRpcResponse = {
  readonly jsonrpc: "2.0";
  readonly id: number | string | null;
  readonly result?: unknown;
  readonly error?: JsonRpcError;
};
```

Example: "Here's the type: `number -> number`"

### Notification

One-way message, no response expected:

```typescript
export type JsonRpcNotification = {
  readonly jsonrpc: "2.0";
  readonly method: string;
  readonly params?: unknown;
};
```

Example: "Document changed" or "Here are new diagnostics"

---

## Server Architecture

The LSP server maintains document state and handles requests:

```typescript
export const createServer = (transport: Transport): void => {
  // Document storage
  const documents = new Map<string, DocumentState>();

  // Wire up message handler
  transport.onMessage((message) => {
    if (isRequest(message)) {
      handleRequest(message);
    } else if (isNotification(message)) {
      handleNotification(message);
    }
  });

  // ... handlers ...
};
```

### Document State

For each open document, we track:

```typescript
type DocumentState = {
  readonly uri: string;           // File path/URL
  readonly version: number;       // Incremented on each edit
  readonly text: string;          // Current content
  readonly diagnostics: readonly LspDiagnostic[];
  readonly symbols: SymbolTable | null;    // From binder
  readonly types: TypeMap | null;          // From type checker
  readonly program: Program | null;        // Parsed AST
  readonly registry: ConstructorRegistry;  // For constructors
};
```

We keep parsed results around for hover, definition lookup, and completion.

---

## Server Capabilities

On initialization, the server advertises what it can do:

```typescript
const handleInitialize = (_params: InitializeParams): InitializeResult => ({
  capabilities: {
    textDocumentSync: 1,          // Full sync (send entire document)
    hoverProvider: true,           // Hover for type info
    definitionProvider: true,      // Go to definition
    renameProvider: {
      prepareProvider: true,       // Validate before rename
    },
    completionProvider: {
      triggerCharacters: ["."],    // Auto-complete on dot
    },
  },
});
```

---

## Document Synchronization

The editor notifies us when documents change.

### Opening a Document

```typescript
const handleDidOpen = (params: DidOpenTextDocumentParams): void => {
  const { uri, version, text } = params.textDocument;
  const state = analyzeDocument(uri, version, text);
  documents.set(uri, state);
  publishDiagnostics(uri, state.diagnostics);
};
```

When a document opens:
1. Parse, bind, and type check
2. Store results for queries
3. Send diagnostics to editor

### Changing a Document

```typescript
const handleDidChange = (params: DidChangeTextDocumentParams): void => {
  const { uri, version } = params.textDocument;
  const text = params.contentChanges[params.contentChanges.length - 1]?.text ?? "";
  const state = analyzeDocument(uri, version, text);
  documents.set(uri, state);
  publishDiagnostics(uri, state.diagnostics);
};
```

Every keystroke triggers re-analysis. This is fast enough for small documents.

### Closing a Document

```typescript
const handleDidClose = (params: DidCloseTextDocumentParams): void => {
  const { uri } = params.textDocument;
  documents.delete(uri);
  publishDiagnostics(uri, []);  // Clear diagnostics
};
```

---

## Analysis Pipeline

When a document changes, we run the full pipeline:

```typescript
const analyzeDocument = (uri: string, version: number, text: string): DocumentState => {
  const lspDiagnostics: LspDiagnostic[] = [];

  // Step 1: Parse
  const parseResult = parse(text);
  for (const diag of parseResult.diagnostics) {
    lspDiagnostics.push(convertDiagnostic(text, diag));
  }

  // Step 2: Process declarations
  const prelude = processDeclarations(preludeDeclarations);
  const { typeEnv, registry, constructorNames } = processDeclarations(
    parseResult.program.declarations,
    prelude,
  );

  const expr = programToExpr(parseResult.program);
  let symbols: SymbolTable | null = null;
  let types: TypeMap | null = null;

  if (expr) {
    // Step 3: Wrap with prelude
    const wrappedExpr = wrapWithPrelude(expr);

    // Step 4: Bind names
    const bindResult = bindWithConstructors(constructorNames, wrappedExpr);
    symbols = bindResult.symbols;
    for (const diag of bindResult.diagnostics) {
      lspDiagnostics.push(convertDiagnostic(text, diag));
    }

    // Step 5: Type check
    const checkResult = check(typeEnv, registry, wrappedExpr, symbols);
    types = checkResult.types;
    for (const diag of checkResult.diagnostics) {
      lspDiagnostics.push(convertDiagnostic(text, diag));
    }
  }

  return { uri, version, text, diagnostics: lspDiagnostics, symbols, types, program: parseResult.program, registry };
};
```

---

## Hover

When the user hovers over code, show type information:

```typescript
const handleHover = (params: TextDocumentPositionParams): Hover | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc?.symbols || !doc.types) return null;

  // Convert LSP position to byte offset
  const offset = positionToOffset(doc.text, params.position);
  const { symbols, types } = doc;

  // Check if hovering over a reference
  const ref = findReferenceAt(symbols, offset);
  if (ref?.definition) {
    const type = types.get(ref.definition);
    if (type) {
      return {
        contents: {
          kind: "markdown",
          value: `\`\`\`algow\n${ref.name}: ${typeToString(type)}\n\`\`\``,
        },
        range: spanToRange(doc.text, ref.span),
      };
    }
  }

  // Check if hovering over a definition
  const def = findDefinitionAt(symbols, offset);
  if (def) {
    const type = types.get(def);
    if (type) {
      return {
        contents: {
          kind: "markdown",
          value: `\`\`\`algow\n${def.name}: ${typeToString(type)}\n\`\`\``,
        },
        range: spanToRange(doc.text, def.span),
      };
    }
  }

  return null;
};
```

Example hover result:

```
┌─────────────────────────┐
│ map: (a -> b) -> List a │
│      -> List b          │
└─────────────────────────┘
```

---

## Go to Definition

Jump from a variable reference to its definition:

```typescript
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
```

The `goToDefinition` function (from the binder) finds the definition for a reference at a given offset.

---

## Rename

Rename a symbol across all occurrences:

### Prepare Rename

First, validate that the position is renameable:

```typescript
const handlePrepareRename = (params: TextDocumentPositionParams): Range | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc?.symbols) return null;

  const offset = positionToOffset(doc.text, params.position);

  // Check if cursor is on a reference
  const ref = findReferenceAt(doc.symbols, offset);
  if (ref?.definition) return spanToRange(doc.text, ref.span);

  // Check if cursor is on a definition
  const def = findDefinitionAt(doc.symbols, offset);
  if (def) return spanToRange(doc.text, def.span);

  return null;  // Not renameable
};
```

### Execute Rename

Apply the rename to all occurrences:

```typescript
const handleRename = (params: RenameParams): WorkspaceEdit | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc || !doc.symbols) return null;

  const offset = positionToOffset(doc.text, params.position);
  const { definition, references } = findAllOccurrences(doc.symbols, offset);

  if (!definition) return null;

  const edits: TextEdit[] = [];

  // Rename the definition
  edits.push({
    range: spanToRange(doc.text, definition.span),
    newText: params.newName,
  });

  // Rename all references
  for (const ref of references) {
    edits.push({
      range: spanToRange(doc.text, ref.span),
      newText: params.newName,
    });
  }

  return {
    changes: { [params.textDocument.uri]: edits },
  };
};
```

---

## Completion

Autocomplete suggestions when typing:

```typescript
const handleCompletion = (params: TextDocumentPositionParams): CompletionList | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return null;

  const offset = positionToOffset(doc.text, params.position);
  const items: CompletionItem[] = [];

  // Check if we're after a dot (record field access)
  const beforeCursor = doc.text.slice(0, offset);
  const dotMatch = beforeCursor.match(/(\w+)\s*\.\s*$/);

  if (dotMatch) {
    // Record/tuple field completion
    const varName = dotMatch[1]!;
    items.push(...getRecordFieldCompletions(doc, varName));
  } else {
    // General completions
    items.push(...getVariableCompletions(doc, offset));
    items.push(...getConstructorCompletions(doc));
    items.push(...getPreludeFunctionCompletions());
    items.push(...getKeywordCompletions());
  }

  return { isIncomplete: false, items };
};
```

### Completion Sources

**Variables in scope**:
```typescript
const getVariableCompletions = (doc: DocumentState, _offset: number): CompletionItem[] => {
  if (!doc.symbols || !doc.types) return [];

  const items: CompletionItem[] = [];
  const seen = new Set<string>();

  for (const def of doc.symbols.definitions) {
    if (seen.has(def.name) || def.kind === "constructor") continue;
    seen.add(def.name);

    const type = doc.types.get(def);
    items.push({
      label: def.name,
      kind: def.kind === "parameter" ? 6 : 3,  // Variable or Function
      detail: type ? typeToString(type) : undefined,
    });
  }

  return items;
};
```

**Constructors**:
```typescript
const getConstructorCompletions = (doc: DocumentState): CompletionItem[] => {
  const items: CompletionItem[] = [];

  for (const [typeName, constructors] of doc.registry) {
    for (const conName of constructors) {
      items.push({
        label: conName,
        kind: 4,  // Constructor
        detail: typeName,
      });
    }
  }

  return items;
};
```

**Prelude functions**:
```typescript
const getPreludeFunctionCompletions = (): CompletionItem[] => {
  return Object.keys(preludeFunctions).map((name) => ({
    label: name,
    kind: 3,  // Function
    detail: "prelude",
  }));
};
```

**Keywords**:
```typescript
const getKeywordCompletions = (): CompletionItem[] => {
  const keywords = ["let", "rec", "in", "if", "then", "else", "match", "with", "end", "data", "true", "false"];
  return keywords.map((kw) => ({ label: kw, kind: 14 }));  // Keyword
};
```

### Record Field Completion

When the user types `point.`, show available fields:

```typescript
const getRecordFieldCompletions = (doc: DocumentState, varName: string): CompletionItem[] => {
  if (!doc.symbols || !doc.types) return [];

  // Find the definition of this variable
  for (const def of doc.symbols.definitions) {
    if (def.name === varName) {
      const type = doc.types.get(def);
      if (type) return extractRecordFields(type);
    }
  }

  return [];
};

const extractRecordFields = (type: Type): CompletionItem[] => {
  const items: CompletionItem[] = [];

  if (type.kind === "TRecord") {
    for (const [fieldName, fieldType] of type.fields) {
      items.push({
        label: fieldName,
        kind: 5,  // Field
        detail: typeToString(fieldType),
      });
    }
  } else if (type.kind === "TTuple") {
    // For tuples, suggest .0, .1, etc.
    for (let i = 0; i < type.elements.length; i++) {
      items.push({
        label: String(i),
        kind: 5,
        detail: typeToString(type.elements[i]!),
      });
    }
  }

  return items;
};
```

---

## Position Conversion

LSP uses 0-based line:character positions. Algow uses byte offsets.

### Offset to Position

```typescript
export const offsetToPosition = (source: string, offset: number): Position => {
  let line = 0;
  let character = 0;

  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === "\n") {
      line++;
      character = 0;
    } else {
      character++;
    }
  }

  return { line, character };
};
```

### Position to Offset

```typescript
export const positionToOffset = (source: string, position: Position): number => {
  let currentLine = 0;
  let offset = 0;

  // Find start of target line
  while (currentLine < position.line && offset < source.length) {
    if (source[offset] === "\n") {
      currentLine++;
    }
    offset++;
  }

  // Add character offset within line
  return offset + position.character;
};
```

### Span to Range

```typescript
export const spanToRange = (source: string, span: Span): Range => ({
  start: offsetToPosition(source, span.start),
  end: offsetToPosition(source, span.end),
});
```

---

## Evaluation in Editor

The playground supports running code directly. We define a custom LSP method:

```typescript
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

  const wrappedExpr = wrapWithPrelude(expr);

  try {
    const prelude = processDeclarations(preludeDeclarations);
    const { constructorNames } = processDeclarations(doc.program.declarations, prelude);
    const constructorEnv = createConstructorEnv(constructorNames);
    const result = evaluate(constructorEnv, wrappedExpr);
    return { success: true, value: valueToString(result) };
  } catch (err) {
    return { success: false, error: (err as Error).message };
  }
};
```

The `algow/evaluate` method returns either a value or an error message.

---

## stdio Transport

For traditional editors:

```typescript
// Simplified version
const createStdioTransport = (): Transport => {
  let messageHandler: ((msg: JsonRpcMessage) => void) | null = null;
  let closeHandler: (() => void) | null = null;

  // Read from stdin
  process.stdin.on("data", (data) => {
    // Parse Content-Length header and JSON body
    const message = parseMessage(data);
    messageHandler?.(message);
  });

  return {
    send: (message) => {
      const json = JSON.stringify(message);
      const header = `Content-Length: ${Buffer.byteLength(json)}\r\n\r\n`;
      process.stdout.write(header + json);
    },
    onMessage: (handler) => { messageHandler = handler; },
    onClose: (handler) => { closeHandler = handler; },
  };
};
```

LSP over stdio uses a simple protocol:
```
Content-Length: 123\r\n
\r\n
{"jsonrpc":"2.0","method":"...","params":{...}}
```

---

## Web Worker Transport

For the browser playground:

```typescript
const createWorkerTransport = (): Transport => {
  let messageHandler: ((msg: JsonRpcMessage) => void) | null = null;

  // Receive messages from main thread
  self.onmessage = (event) => {
    messageHandler?.(event.data as JsonRpcMessage);
  };

  return {
    send: (message) => {
      self.postMessage(message);
    },
    onMessage: (handler) => { messageHandler = handler; },
    onClose: () => {},  // Workers don't close
  };
};
```

The Monaco editor in the main thread communicates with the LSP server running in a Web Worker.

---

## Summary

The LSP integration provides:

1. **Transport abstraction**: Works in CLI and browser
2. **Document synchronization**: Real-time analysis as you type
3. **Hover**: Type information on hover
4. **Go to definition**: Jump to where a name is defined
5. **Rename**: Rename symbols across all occurrences
6. **Completion**: Context-aware suggestions
7. **Diagnostics**: Errors and warnings in the editor
8. **Evaluation**: Run code directly in the playground

The same type checker, binder, and parser power both the command-line compiler and the IDE experience.

---

This concludes Part 5: Putting It Together. The appendices provide reference material: complete language syntax, glossary, and further reading.
