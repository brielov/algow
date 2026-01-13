/**
 * Runtime Builder
 *
 * Assembles the runtime from modular components based on the target platform.
 * All runtimes are pre-loaded at module initialization for synchronous access.
 */

export type Target = "node" | "deno" | "browser" | "cloudflare";

export const TARGETS: readonly Target[] = ["node", "deno", "browser", "cloudflare"] as const;

export const DEFAULT_TARGET: Target = "node";

export const isValidTarget = (target: string): target is Target => {
  return TARGETS.includes(target as Target);
};

// Load runtime modules at startup
const loadModule = (path: string): Promise<string> => {
  return Bun.file(new URL(path, import.meta.url)).text();
};

// Pre-load all modules at initialization (top-level await)
const [
  base,
  string,
  char,
  int,
  float,
  debug,
  map,
  set,
  path,
  json,
  nodeTarget,
  denoTarget,
  browserTarget,
  cloudflareTarget,
] = await Promise.all([
  loadModule("./base.js"),
  loadModule("./modules/string.js"),
  loadModule("./modules/char.js"),
  loadModule("./modules/int.js"),
  loadModule("./modules/float.js"),
  loadModule("./modules/debug.js"),
  loadModule("./modules/map.js"),
  loadModule("./modules/set.js"),
  loadModule("./modules/path.js"),
  loadModule("./modules/json.js"),
  loadModule("./targets/node.js"),
  loadModule("./targets/deno.js"),
  loadModule("./targets/browser.js"),
  loadModule("./targets/cloudflare.js"),
]);

// Cross-platform modules shared by all targets
const crossPlatform = [base, string, char, int, float, debug, map, set, path, json].join("\n");

// Pre-built runtimes for each target
const RUNTIMES: Record<Target, string> = {
  node: crossPlatform + "\n" + nodeTarget,
  deno: crossPlatform + "\n" + denoTarget,
  browser: crossPlatform + "\n" + browserTarget,
  cloudflare: crossPlatform + "\n" + cloudflareTarget,
};

/**
 * Get the runtime code for a specific target platform.
 */
export const getRuntime = (target: Target): string => {
  return RUNTIMES[target];
};
