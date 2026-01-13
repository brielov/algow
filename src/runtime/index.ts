/**
 * Runtime Builder
 *
 * Assembles the runtime from modular components based on the target platform.
 * All runtimes are imported as text for bundling compatibility.
 */

// Import runtime modules as text (works with bun build --compile)
import base from "./base.js" with { type: "text" };
import string from "./modules/string.js" with { type: "text" };
import char from "./modules/char.js" with { type: "text" };
import int from "./modules/int.js" with { type: "text" };
import float from "./modules/float.js" with { type: "text" };
import debug from "./modules/debug.js" with { type: "text" };
import map from "./modules/map.js" with { type: "text" };
import set from "./modules/set.js" with { type: "text" };
import path from "./modules/path.js" with { type: "text" };
import json from "./modules/json.js" with { type: "text" };
import http from "./modules/http.js" with { type: "text" };
import nodeTarget from "./targets/node.js" with { type: "text" };
import denoTarget from "./targets/deno.js" with { type: "text" };
import browserTarget from "./targets/browser.js" with { type: "text" };
import cloudflareTarget from "./targets/cloudflare.js" with { type: "text" };

export type Target = "node" | "deno" | "browser" | "cloudflare";

export const TARGETS: readonly Target[] = ["node", "deno", "browser", "cloudflare"] as const;

export const DEFAULT_TARGET: Target = "node";

export const isValidTarget = (target: string): target is Target => {
  return TARGETS.includes(target as Target);
};

// Cross-platform modules shared by all targets
const crossPlatform: string = [
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
  http,
].join("\n");

// Pre-built runtimes for each target
const RUNTIMES: Record<Target, string> = {
  node: `${crossPlatform}\n${nodeTarget}`,
  deno: `${crossPlatform}\n${denoTarget}`,
  browser: `${crossPlatform}\n${browserTarget}`,
  cloudflare: `${crossPlatform}\n${cloudflareTarget}`,
};

/**
 * Get the runtime code for a specific target platform.
 */
export const getRuntime = (target: Target): string => {
  return RUNTIMES[target];
};
