/**
 * Playground Main - Monaco Editor + LSP Bridge
 *
 * Initializes Monaco editor and bridges it to the LSP server
 * running in a web worker via postMessage.
 */

// Monaco and require are loaded from CDN
declare const require: {
  config(options: { paths: Record<string, string> }): void;
  (deps: string[], callback: () => void): void;
};
declare const monaco: typeof import("monaco-editor");

type JsonRpcMessage = {
  readonly jsonrpc: "2.0";
  readonly id?: number | string;
  readonly method?: string;
  readonly params?: unknown;
  readonly result?: unknown;
  readonly error?: { code: number; message: string };
};

// Default code for the playground
const defaultCode = `-- Algow Playground
-- Hover over any identifier to see its inferred type.
-- Ctrl/Cmd+Click to go to definitions.
-- F2 to rename a symbol across all occurrences.

-- Standard types and functions from prelude:
-- type Maybe a = Nothing | Just a
-- type Either a b = Left a | Right b
-- type List a = Nil | Cons a (List a)
-- map, filter, foldr, foldl, head, tail, length, etc.

-- Functions use 'let', recursive ones use 'let rec'
let double = x -> x * 2
let isPositive = x -> x > 0
let add = x y -> x + y

-- Lists use bracket notation
let numbers = [1, 2, 3, 4, 5]

-- Pattern matching uses 'match/when/end'
let rec sum xs = match xs
  when Nil -> 0
  when Cons x rest -> x + sum rest
end

-- Or-patterns for multiple cases
let describe n = match n
  when 0 | 1 -> "small"
  when _ -> "big"
end

-- Try these expressions (change the last line):
-- map double numbers
-- filter isPositive [0, 1, -2, 3]
-- foldr add 0 numbers
-- head numbers
-- length numbers

map double numbers
`;

require.config({
  paths: { vs: "https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs" },
});

require(["vs/editor/editor.main"], () => {
  // Register Algow language
  monaco.languages.register({ id: "algow", extensions: [".alg"] });

  // Syntax highlighting
  monaco.languages.setMonarchTokensProvider("algow", {
    keywords: ["let", "rec", "in", "if", "then", "else", "match", "when", "end", "type", "module", "use", "as", "and"],
    literals: ["true", "false"],

    tokenizer: {
      root: [
        [/--.*$/, "comment"],
        [/\{-/, "comment", "@blockComment"],
        [/\b(let|rec|in|if|then|else|match|when|end|type|module|use|as|and)\b/, "keyword"],
        [/\b(true|false)\b/, "constant"],
        [/\b[A-Z][a-zA-Z0-9_]*\b/, "type.identifier"],
        [/\b[a-z_][a-zA-Z0-9_]*\b/, "identifier"],
        [/\d+(\.\d+)?/, "number"],
        [/"[^"]*"/, "string"],
        [/->/, "operator"],
        [/[+\-*/<>=!|:]+/, "operator"],
        [/[{}()[\],.]/, "delimiter"],
      ],
      blockComment: [
        [/-\}/, "comment", "@pop"],
        [/./, "comment"],
      ],
    },
  });

  // Create editor
  const editor = monaco.editor.create(document.getElementById("editor")!, {
    value: defaultCode,
    language: "algow",
    theme: "vs-dark",
    fontSize: 14,
    minimap: { enabled: false },
    automaticLayout: true,
    tabSize: 2,
  });

  // Create LSP worker
  const worker = new Worker("./worker.js", { type: "module" });

  // Status indicator
  const statusEl = document.getElementById("status")!;

  // Output panel
  const outputEl = document.getElementById("output")!;

  // Create LSP bridge
  const bridge = createLspBridge(worker, editor, statusEl, outputEl);
  void bridge.start();
});

/**
 * Map LSP CompletionItemKind to Monaco CompletionItemKind.
 */
function mapCompletionKind(lspKind?: number): monaco.languages.CompletionItemKind {
  // LSP kinds: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind
  // Monaco kinds: https://microsoft.github.io/monaco-editor/typedoc/enums/languages.CompletionItemKind.html
  switch (lspKind) {
    case 3: // Function
      return monaco.languages.CompletionItemKind.Function;
    case 4: // Constructor
      return monaco.languages.CompletionItemKind.Constructor;
    case 5: // Field
      return monaco.languages.CompletionItemKind.Field;
    case 6: // Variable
      return monaco.languages.CompletionItemKind.Variable;
    case 14: // Keyword
      return monaco.languages.CompletionItemKind.Keyword;
    default:
      return monaco.languages.CompletionItemKind.Text;
  }
}

function createLspBridge(
  worker: Worker,
  editor: ReturnType<typeof monaco.editor.create>,
  statusEl: HTMLElement,
  outputEl: HTMLElement,
) {
  const model = editor.getModel()!;
  const uri = model.uri.toString();
  let requestId = 0;
  const pendingRequests = new Map<
    number,
    { resolve: (value: unknown) => void; reject: (error: Error) => void }
  >();

  // Handle messages from worker
  worker.onmessage = (event: MessageEvent<JsonRpcMessage>) => {
    const message = event.data;

    if (message.id !== undefined && message.id !== null) {
      // Response to a request
      const pending = pendingRequests.get(message.id as number);
      if (pending) {
        pendingRequests.delete(message.id as number);
        if (message.error) {
          pending.reject(new Error(message.error.message));
        } else {
          pending.resolve(message.result);
        }
      }
    } else if (message.method) {
      // Notification from server
      handleNotification(message);
    }
  };

  const sendRequest = (method: string, params: unknown): Promise<unknown> => {
    const id = ++requestId;
    return new Promise((resolve, reject) => {
      pendingRequests.set(id, { resolve, reject });
      worker.postMessage({ jsonrpc: "2.0", id, method, params });
    });
  };

  const sendNotification = (method: string, params: unknown): void => {
    worker.postMessage({ jsonrpc: "2.0", method, params });
  };

  const handleNotification = (notification: JsonRpcMessage): void => {
    if (notification.method === "textDocument/publishDiagnostics") {
      const { diagnostics } = notification.params as {
        uri: string;
        diagnostics: Array<{
          range: {
            start: { line: number; character: number };
            end: { line: number; character: number };
          };
          message: string;
          severity: number;
        }>;
      };

      const markers = diagnostics.map((d) => ({
        severity:
          d.severity === 1
            ? monaco.MarkerSeverity.Error
            : d.severity === 2
              ? monaco.MarkerSeverity.Warning
              : monaco.MarkerSeverity.Info,
        startLineNumber: d.range.start.line + 1,
        startColumn: d.range.start.character + 1,
        endLineNumber: d.range.end.line + 1,
        endColumn: d.range.end.character + 1,
        message: d.message,
        source: "algow",
      }));

      monaco.editor.setModelMarkers(model, "algow", markers);
    }
  };

  return {
    async start() {
      try {
        // Initialize
        await sendRequest("initialize", { capabilities: {} });
        sendNotification("initialized", {});

        statusEl.textContent = "Connected";
        statusEl.classList.add("connected");

        // Open document
        sendNotification("textDocument/didOpen", {
          textDocument: {
            uri,
            languageId: "algow",
            version: 1,
            text: model.getValue(),
          },
        });

        // Evaluate document
        let evaluateTimeout: ReturnType<typeof setTimeout> | null = null;
        const evaluate = async () => {
          try {
            const result = (await sendRequest("algow/evaluate", {
              textDocument: { uri },
            })) as { success: boolean; value?: string; error?: string };

            outputEl.className = "output-content " + (result.success ? "success" : "error");
            outputEl.textContent = result.success ? result.value! : result.error!;
          } catch {
            outputEl.className = "output-content error";
            outputEl.textContent = "Evaluation failed";
          }
        };

        // Evaluate after a short delay
        const scheduleEvaluate = () => {
          if (evaluateTimeout) clearTimeout(evaluateTimeout);
          outputEl.className = "output-content pending";
          outputEl.textContent = "Evaluating...";
          evaluateTimeout = setTimeout(evaluate, 300);
        };

        // Initial evaluation
        scheduleEvaluate();

        // Track changes
        let version = 1;
        model.onDidChangeContent(() => {
          version++;
          sendNotification("textDocument/didChange", {
            textDocument: { uri, version },
            contentChanges: [{ text: model.getValue() }],
          });
          scheduleEvaluate();
        });

        // Register hover provider
        monaco.languages.registerHoverProvider("algow", {
          provideHover: async (_model, position) => {
            try {
              const result = await sendRequest("textDocument/hover", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1,
                },
              });

              if (!result) return null;

              const hover = result as {
                contents: { value: string };
                range?: {
                  start: { line: number; character: number };
                  end: { line: number; character: number };
                };
              };

              return {
                contents: [{ value: hover.contents.value }],
                range: hover.range
                  ? new monaco.Range(
                      hover.range.start.line + 1,
                      hover.range.start.character + 1,
                      hover.range.end.line + 1,
                      hover.range.end.character + 1,
                    )
                  : undefined,
              };
            } catch {
              return null;
            }
          },
        });

        // Register definition provider
        monaco.languages.registerDefinitionProvider("algow", {
          provideDefinition: async (_model, position) => {
            try {
              const result = await sendRequest("textDocument/definition", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1,
                },
              });

              if (!result) return null;

              const loc = result as {
                uri: string;
                range: {
                  start: { line: number; character: number };
                  end: { line: number; character: number };
                };
              };

              return {
                uri: monaco.Uri.parse(loc.uri),
                range: new monaco.Range(
                  loc.range.start.line + 1,
                  loc.range.start.character + 1,
                  loc.range.end.line + 1,
                  loc.range.end.character + 1,
                ),
              };
            } catch {
              return null;
            }
          },
        });

        // Register completion provider
        monaco.languages.registerCompletionItemProvider("algow", {
          triggerCharacters: ["."],
          provideCompletionItems: async (_model, position) => {
            try {
              const result = await sendRequest("textDocument/completion", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1,
                },
              });

              if (!result) return { suggestions: [] };

              const completionList = result as {
                isIncomplete: boolean;
                items: Array<{
                  label: string;
                  kind?: number;
                  detail?: string;
                }>;
              };

              const suggestions = completionList.items.map((item) => ({
                label: item.label,
                kind: mapCompletionKind(item.kind),
                detail: item.detail,
                insertText: item.label,
                range: undefined as unknown as monaco.IRange,
              }));

              return { suggestions };
            } catch {
              return { suggestions: [] };
            }
          },
        });

        // Register rename provider
        monaco.languages.registerRenameProvider("algow", {
          provideRenameEdits: async (_model, position, newName) => {
            try {
              const result = await sendRequest("textDocument/rename", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1,
                },
                newName,
              });

              if (!result) return null;

              const workspaceEdit = result as {
                changes: Record<
                  string,
                  Array<{
                    range: {
                      start: { line: number; character: number };
                      end: { line: number; character: number };
                    };
                    newText: string;
                  }>
                >;
              };

              const edits: monaco.languages.WorkspaceTextEdit[] = [];
              for (const [editUri, textEdits] of Object.entries(workspaceEdit.changes)) {
                for (const edit of textEdits) {
                  edits.push({
                    resource: monaco.Uri.parse(editUri),
                    textEdit: {
                      range: new monaco.Range(
                        edit.range.start.line + 1,
                        edit.range.start.character + 1,
                        edit.range.end.line + 1,
                        edit.range.end.character + 1,
                      ),
                      text: edit.newText,
                    },
                    versionId: undefined,
                  });
                }
              }

              return { edits };
            } catch {
              return null;
            }
          },

          resolveRenameLocation: async (_model, position) => {
            try {
              const result = await sendRequest("textDocument/prepareRename", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1,
                },
              });

              if (!result) return { rejectReason: "Cannot rename this symbol" };

              const range = result as {
                start: { line: number; character: number };
                end: { line: number; character: number };
              };

              return {
                range: new monaco.Range(
                  range.start.line + 1,
                  range.start.character + 1,
                  range.end.line + 1,
                  range.end.character + 1,
                ),
                text: model.getValueInRange(
                  new monaco.Range(
                    range.start.line + 1,
                    range.start.character + 1,
                    range.end.line + 1,
                    range.end.character + 1,
                  ),
                ),
              };
            } catch {
              return { rejectReason: "Cannot rename this symbol" };
            }
          },
        });
      } catch (err) {
        statusEl.textContent = "Error: " + (err as Error).message;
      }
    },
  };
}
