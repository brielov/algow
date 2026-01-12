import { readdir, stat } from "node:fs/promises";
import { join } from "node:path";

const TIMEOUT_MS = 3000;
const EXAMPLES_DIR = "examples";

async function findAlgFiles(dir: string): Promise<string[]> {
  const files: string[] = [];
  const entries = await readdir(dir);
  for (const entry of entries) {
    const path = join(dir, entry);
    const s = await stat(path);
    if (s.isDirectory()) {
      files.push(...(await findAlgFiles(path)));
    } else if (entry.endsWith(".alg")) {
      files.push(path);
    }
  }
  return files.sort();
}

async function runWithTimeout(file: string): Promise<{ ok: boolean; output: string }> {
  return new Promise((resolve) => {
    const proc = Bun.spawn(["bun", "run", "src/index.ts", "-t", file], {
      stdout: "pipe",
      stderr: "pipe",
    });

    const timeout = setTimeout(() => {
      proc.kill();
      resolve({ ok: false, output: `TIMEOUT after ${TIMEOUT_MS}ms` });
    }, TIMEOUT_MS);

    proc.exited.then(async (code) => {
      clearTimeout(timeout);
      const stderr = await new Response(proc.stderr).text();
      resolve({ ok: code === 0, output: stderr });
    });
  });
}

async function main() {
  const files = await findAlgFiles(EXAMPLES_DIR);
  let passed = 0;
  let failed = 0;

  console.log(`Running ${files.length} examples...\n`);

  for (const file of files) {
    const { ok, output } = await runWithTimeout(file);
    if (ok) {
      console.log(`✓ ${file}`);
      passed++;
    } else {
      console.log(`✗ ${file}`);
      if (output.trim()) {
        console.log(`  ${output.trim().split("\n").join("\n  ")}`);
      }
      failed++;
    }
  }

  console.log(`\n${passed} passed, ${failed} failed`);
  process.exit(failed > 0 ? 1 : 0);
}

main();
