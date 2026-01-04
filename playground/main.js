var __require = /* @__PURE__ */ ((x) => typeof require !== "undefined" ? require : typeof Proxy !== "undefined" ? new Proxy(x, {
  get: (a, b) => (typeof require !== "undefined" ? require : a)[b]
}) : x)(function(x) {
  if (typeof require !== "undefined")
    return require.apply(this, arguments);
  throw Error('Dynamic require of "' + x + '" is not supported');
});

// playground/main.ts
var defaultCode = `-- Algow Playground
-- Hover over any identifier to see its inferred type.
-- Ctrl/Cmd+Click to go to definitions.
-- F2 to rename a symbol across all occurrences.

-- Standard data types are available from the prelude:
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b
-- data List a = Nil | Cons a (List a)

-- Define recursive functions with 'let rec'
let rec map f xs = match xs with
| Nil => Nil
| Cons x rest => Cons (f x) (map f rest)
end

let rec filter p xs = match xs with
| Nil => Nil
| Cons x rest => if p x
  then Cons x (filter p rest)
  else filter p rest
end

let rec foldr f z xs = match xs with
| Nil => z
| Cons x rest => f x (foldr f z rest)
end

-- Non-recursive functions use 'let'
let double x = x * 2
let isPositive x = x > 0
let add x y = x + y

-- The :: operator is sugar for Cons
let numbers = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

-- Try these expressions (change the last line):
-- map double numbers
-- filter isPositive (0 :: 1 :: -2 :: 3 :: Nil)
-- foldr add 0 numbers

map double numbers
`;
__require.config({
  paths: { vs: "https://cdn.jsdelivr.net/npm/monaco-editor@0.45.0/min/vs" }
});
__require(["vs/editor/editor.main"], () => {
  monaco.languages.register({ id: "algow", extensions: [".alg"] });
  monaco.languages.setMonarchTokensProvider("algow", {
    keywords: ["let", "rec", "in", "if", "then", "else", "match", "with", "end", "data"],
    literals: ["true", "false"],
    tokenizer: {
      root: [
        [/--.*$/, "comment"],
        [/\{-/, "comment", "@blockComment"],
        [/\b(let|rec|in|if|then|else|match|with|end|data)\b/, "keyword"],
        [/\b(true|false)\b/, "constant"],
        [/\b[A-Z][a-zA-Z0-9_]*\b/, "type.identifier"],
        [/\b[a-z_][a-zA-Z0-9_]*\b/, "identifier"],
        [/\d+(\.\d+)?/, "number"],
        [/"[^"]*"/, "string"],
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
  const bridge = createLspBridge(worker, editor, statusEl, outputEl);
  bridge.start();
});
function createLspBridge(worker, editor, statusEl, outputEl) {
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
            outputEl.className = "output-content " + (result.success ? "success" : "error");
            outputEl.textContent = result.success ? result.value : result.error;
          } catch {
            outputEl.className = "output-content error";
            outputEl.textContent = "Evaluation failed";
          }
        };
        const scheduleEvaluate = () => {
          if (evaluateTimeout)
            clearTimeout(evaluateTimeout);
          outputEl.className = "output-content pending";
          outputEl.textContent = "Evaluating...";
          evaluateTimeout = setTimeout(evaluate, 300);
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
