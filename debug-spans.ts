import { compileForLSP, type SourceFile } from "./src/compile.ts";
import { readFileSync } from "fs";

const preludePath = "./lib/prelude.alg";
const preludeContent = readFileSync(preludePath, "utf-8");

const userPath = "./integration/http/main.alg";
const userContent = readFileSync(userPath, "utf-8");

const sources: SourceFile[] = [
  { path: preludePath, content: preludeContent },
  { path: userPath, content: userContent },
];

const result = compileForLSP(sources);

// Find definitions in user file (fileId = 1)
console.log("User file definitions (fileId=1):");
if (result.symbolTable) {
  for (const [_id, def] of result.symbolTable.definitions) {
    if (def.location.fileId === 1) {
      console.log(
        "  " +
          def.name +
          " (" +
          def.kind +
          "): span " +
          def.location.span.start +
          "-" +
          def.location.span.end,
      );
    }
  }
}
