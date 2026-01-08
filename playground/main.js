var __require = /* @__PURE__ */ ((x) => typeof require !== "undefined" ? require : typeof Proxy !== "undefined" ? new Proxy(x, {
  get: (a, b) => (typeof require !== "undefined" ? require : a)[b]
}) : x)(function(x) {
  if (typeof require !== "undefined")
    return require.apply(this, arguments);
  throw Error('Dynamic require of "' + x + '" is not supported');
});

// playground/main.ts
var defaultCode = `-- Algow Playground
-- Hover over identifiers to see inferred types.
-- Ctrl/Cmd+Click to go to definitions.
-- F2 to rename symbols.

-- Standard prelude modules: Maybe, Either, List, Core
-- Type 'List.' to see module member completions!

-- Define a custom module
module Math
  let square x = x * x
  let cube x = x * x * x
end

-- Import module bindings
use Math (..)

-- Functions and pattern matching
let double x = x * 2
let numbers = [1, 2, 3, 4, 5]

let rec sum xs = match xs
  when Nil -> 0
  when Cons x rest -> x + sum rest
end

-- Or-patterns
let describe n = match n
  when 0 | 1 -> "small"
  when _ -> "big"
end

-- Try these (change the last line):
-- map double numbers
-- map square numbers
-- filter (x -> x > 2) numbers
-- sum numbers

map square numbers
`;
__require.config({
  paths: { vs: "https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs" }
});
__require(["vs/editor/editor.main"], () => {
  monaco.languages.register({ id: "algow", extensions: [".alg"] });
  monaco.languages.setMonarchTokensProvider("algow", {
    keywords: [
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
      "and"
    ],
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
        [/[{}()[\],.]/, "delimiter"]
      ],
      blockComment: [
        [/-\}/, "comment", "@pop"],
        [/./, "comment"]
      ]
    }
  });
  const editor = monaco.editor.create(document.getElementById("editor"), {
    value: defaultCode,
    language: "algow",
    theme: "vs-dark",
    fontSize: 14,
    minimap: { enabled: false },
    automaticLayout: true,
    tabSize: 2
  });
  const worker = new Worker("./worker.js", { type: "module" });
  const statusEl = document.getElementById("status");
  const outputEl = document.getElementById("output");
  const jsEl = document.getElementById("js");
  const irEl = document.getElementById("ir");
  const tabs = document.querySelectorAll(".output-tab");
  const contents = document.querySelectorAll(".output-content");
  tabs.forEach((tab) => {
    tab.addEventListener("click", () => {
      const targetId = tab.dataset.tab;
      tabs.forEach((t) => t.classList.remove("active"));
      tab.classList.add("active");
      contents.forEach((c) => c.classList.remove("active"));
      document.getElementById(targetId).classList.add("active");
    });
  });
  const bridge = createLspBridge(worker, editor, statusEl, outputEl, jsEl, irEl);
  bridge.start();
});
function mapCompletionKind(lspKind) {
  switch (lspKind) {
    case 3:
      return monaco.languages.CompletionItemKind.Function;
    case 4:
      return monaco.languages.CompletionItemKind.Constructor;
    case 5:
      return monaco.languages.CompletionItemKind.Field;
    case 6:
      return monaco.languages.CompletionItemKind.Variable;
    case 14:
      return monaco.languages.CompletionItemKind.Keyword;
    default:
      return monaco.languages.CompletionItemKind.Text;
  }
}
function createLspBridge(worker, editor, statusEl, outputEl, jsEl, irEl) {
  const model = editor.getModel();
  const uri = model.uri.toString();
  let requestId = 0;
  const pendingRequests = new Map;
  worker.onmessage = (event) => {
    const message = event.data;
    if (message.id !== undefined && message.id !== null) {
      const pending = pendingRequests.get(message.id);
      if (pending) {
        pendingRequests.delete(message.id);
        if (message.error) {
          pending.reject(new Error(message.error.message));
        } else {
          pending.resolve(message.result);
        }
      }
    } else if (message.method) {
      handleNotification(message);
    }
  };
  const sendRequest = (method, params) => {
    const id = ++requestId;
    return new Promise((resolve, reject) => {
      pendingRequests.set(id, { resolve, reject });
      worker.postMessage({ jsonrpc: "2.0", id, method, params });
    });
  };
  const sendNotification = (method, params) => {
    worker.postMessage({ jsonrpc: "2.0", method, params });
  };
  const handleNotification = (notification) => {
    if (notification.method === "textDocument/publishDiagnostics") {
      const { diagnostics } = notification.params;
      const markers = diagnostics.map((d) => ({
        severity: d.severity === 1 ? monaco.MarkerSeverity.Error : d.severity === 2 ? monaco.MarkerSeverity.Warning : monaco.MarkerSeverity.Info,
        startLineNumber: d.range.start.line + 1,
        startColumn: d.range.start.character + 1,
        endLineNumber: d.range.end.line + 1,
        endColumn: d.range.end.character + 1,
        message: d.message,
        source: "algow"
      }));
      monaco.editor.setModelMarkers(model, "algow", markers);
    }
  };
  return {
    async start() {
      try {
        await sendRequest("initialize", { capabilities: {} });
        sendNotification("initialized", {});
        statusEl.textContent = "Connected";
        statusEl.classList.add("connected");
        sendNotification("textDocument/didOpen", {
          textDocument: {
            uri,
            languageId: "algow",
            version: 1,
            text: model.getValue()
          }
        });
        let evaluateTimeout = null;
        const evaluate = async () => {
          try {
            const result = await sendRequest("algow/evaluate", {
              textDocument: { uri }
            });
            outputEl.className = "output-content " + (outputEl.classList.contains("active") ? "active " : "") + (result.success ? "success" : "error");
            outputEl.textContent = result.success ? result.value : result.error;
          } catch {
            outputEl.className = "output-content " + (outputEl.classList.contains("active") ? "active " : "") + "error";
            outputEl.textContent = "Evaluation failed";
          }
        };
        const compile = async () => {
          try {
            const result = await sendRequest("algow/compile", {
              textDocument: { uri }
            });
            jsEl.className = "output-content " + (jsEl.classList.contains("active") ? "active " : "") + (result.success ? "code" : "error");
            jsEl.textContent = result.success ? result.code : result.error;
          } catch {
            jsEl.className = "output-content " + (jsEl.classList.contains("active") ? "active " : "") + "error";
            jsEl.textContent = "Compilation failed";
          }
        };
        const emitIR = async () => {
          try {
            const result = await sendRequest("algow/emitIR", {
              textDocument: { uri }
            });
            irEl.className = "output-content " + (irEl.classList.contains("active") ? "active " : "") + (result.success ? "code" : "error");
            irEl.textContent = result.success ? result.ir : result.error;
          } catch {
            irEl.className = "output-content " + (irEl.classList.contains("active") ? "active " : "") + "error";
            irEl.textContent = "IR generation failed";
          }
        };
        const updateAll = async () => {
          await Promise.all([evaluate(), compile(), emitIR()]);
        };
        const scheduleEvaluate = () => {
          if (evaluateTimeout)
            clearTimeout(evaluateTimeout);
          if (outputEl.classList.contains("active")) {
            outputEl.className = "output-content active pending";
            outputEl.textContent = "Evaluating...";
          }
          if (jsEl.classList.contains("active")) {
            jsEl.className = "output-content active pending";
            jsEl.textContent = "Compiling...";
          }
          if (irEl.classList.contains("active")) {
            irEl.className = "output-content active pending";
            irEl.textContent = "Generating IR...";
          }
          evaluateTimeout = setTimeout(updateAll, 300);
        };
        scheduleEvaluate();
        let version = 1;
        model.onDidChangeContent(() => {
          version++;
          sendNotification("textDocument/didChange", {
            textDocument: { uri, version },
            contentChanges: [{ text: model.getValue() }]
          });
          scheduleEvaluate();
        });
        monaco.languages.registerHoverProvider("algow", {
          provideHover: async (_model, position) => {
            try {
              const result = await sendRequest("textDocument/hover", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1
                }
              });
              if (!result)
                return null;
              const hover = result;
              return {
                contents: [{ value: hover.contents.value }],
                range: hover.range ? new monaco.Range(hover.range.start.line + 1, hover.range.start.character + 1, hover.range.end.line + 1, hover.range.end.character + 1) : undefined
              };
            } catch {
              return null;
            }
          }
        });
        monaco.languages.registerDefinitionProvider("algow", {
          provideDefinition: async (_model, position) => {
            try {
              const result = await sendRequest("textDocument/definition", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1
                }
              });
              if (!result)
                return null;
              const loc = result;
              return {
                uri: monaco.Uri.parse(loc.uri),
                range: new monaco.Range(loc.range.start.line + 1, loc.range.start.character + 1, loc.range.end.line + 1, loc.range.end.character + 1)
              };
            } catch {
              return null;
            }
          }
        });
        monaco.languages.registerCompletionItemProvider("algow", {
          triggerCharacters: ["."],
          provideCompletionItems: async (_model, position) => {
            try {
              const result = await sendRequest("textDocument/completion", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1
                }
              });
              if (!result)
                return { suggestions: [] };
              const completionList = result;
              const suggestions = completionList.items.map((item) => ({
                label: item.label,
                kind: mapCompletionKind(item.kind),
                detail: item.detail,
                insertText: item.label,
                range: undefined
              }));
              return { suggestions };
            } catch {
              return { suggestions: [] };
            }
          }
        });
        monaco.languages.registerRenameProvider("algow", {
          provideRenameEdits: async (_model, position, newName) => {
            try {
              const result = await sendRequest("textDocument/rename", {
                textDocument: { uri },
                position: {
                  line: position.lineNumber - 1,
                  character: position.column - 1
                },
                newName
              });
              if (!result)
                return null;
              const workspaceEdit = result;
              const edits = [];
              for (const [editUri, textEdits] of Object.entries(workspaceEdit.changes)) {
                for (const edit of textEdits) {
                  edits.push({
                    resource: monaco.Uri.parse(editUri),
                    textEdit: {
                      range: new monaco.Range(edit.range.start.line + 1, edit.range.start.character + 1, edit.range.end.line + 1, edit.range.end.character + 1),
                      text: edit.newText
                    },
                    versionId: undefined
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
                  character: position.column - 1
                }
              });
              if (!result)
                return { rejectReason: "Cannot rename this symbol" };
              const range = result;
              return {
                range: new monaco.Range(range.start.line + 1, range.start.character + 1, range.end.line + 1, range.end.character + 1),
                text: model.getValueInRange(new monaco.Range(range.start.line + 1, range.start.character + 1, range.end.line + 1, range.end.character + 1))
              };
            } catch {
              return { rejectReason: "Cannot rename this symbol" };
            }
          }
        });
      } catch (err) {
        statusEl.textContent = "Error: " + err.message;
      }
    }
  };
}
