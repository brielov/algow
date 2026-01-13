import { readdir, stat } from "node:fs/promises";
import { join } from "node:path";

const TIMEOUT_MS = 3000;
const INTEGRATION_DIR = "integration";

/** Find all test directories (immediate subdirectories of integration/) */
async function findTestDirs(dir: string): Promise<string[]> {
  const dirs: string[] = [];
  const entries = await readdir(dir);
  for (const entry of entries) {
    const path = join(dir, entry);
    const s = await stat(path);
    if (s.isDirectory()) {
      dirs.push(path);
    }
  }
  return dirs.sort();
}

/** Run check on a directory (multi-file project) */
async function runWithTimeout(dir: string): Promise<{ ok: boolean; output: string }> {
  return new Promise((resolve) => {
    const proc = Bun.spawn(["bun", "run", "src/index.ts", "check", dir], {
      stdout: "pipe",
      stderr: "pipe",
    });

    const timeout = setTimeout(() => {
      proc.kill();
      resolve({ ok: false, output: `TIMEOUT after ${TIMEOUT_MS}ms` });
    }, TIMEOUT_MS);

    void proc.exited.then(async (code) => {
      clearTimeout(timeout);
      const stderr = await new Response(proc.stderr).text();
      resolve({ ok: code === 0, output: stderr });
    });
  });
}

async function main() {
  const dirs = await findTestDirs(INTEGRATION_DIR);
  let passed = 0;
  let failed = 0;

  console.log(`Running ${dirs.length} integration tests...\n`);

  for (const dir of dirs) {
    const { ok, output } = await runWithTimeout(dir);
    if (ok) {
      console.log(`✓ ${dir}`);
      passed++;
    } else {
      console.log(`✗ ${dir}`);
      if (output.trim()) {
        console.log(`  ${output.trim().split("\n").join("\n  ")}`);
      }
      failed++;
    }
  }

  console.log(`\n${passed} passed, ${failed} failed`);
  process.exit(failed > 0 ? 1 : 0);
}

void main();
