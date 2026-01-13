/**
 * Runtime Builder
 *
 * Assembles the runtime from modular components based on the target platform.
 * All runtimes are imported as text for bundling compatibility.
 */

// Import runtime modules as text (works with bun build --compile)
// Bun's `with { type: "text" }` imports files as strings, but TypeScript
// doesn't recognize this, so we cast to string to satisfy the type checker.
import _base from "./base.js" with { type: "text" };
import _string from "./modules/string.js" with { type: "text" };
import _char from "./modules/char.js" with { type: "text" };
import _int from "./modules/int.js" with { type: "text" };
import _float from "./modules/float.js" with { type: "text" };
import _debug from "./modules/debug.js" with { type: "text" };
import _map from "./modules/map.js" with { type: "text" };
import _set from "./modules/set.js" with { type: "text" };
import _path from "./modules/path.js" with { type: "text" };
import _json from "./modules/json.js" with { type: "text" };
import _http from "./modules/http.js" with { type: "text" };
import _bool from "./modules/bool.js" with { type: "text" };
import _random from "./modules/random.js" with { type: "text" };
import _duration from "./modules/duration.js" with { type: "text" };
import _date from "./modules/date.js" with { type: "text" };
import _time from "./modules/time.js" with { type: "text" };
import _datetime from "./modules/datetime.js" with { type: "text" };
import _test from "./modules/test.js" with { type: "text" };
import _nodeTarget from "./targets/node.js" with { type: "text" };
import _denoTarget from "./targets/deno.js" with { type: "text" };
import _browserTarget from "./targets/browser.js" with { type: "text" };
import _cloudflareTarget from "./targets/cloudflare.js" with { type: "text" };

const base = _base as string;
const string = _string as string;
const char = _char as string;
const int = _int as string;
const float = _float as string;
const debug = _debug as string;
const map = _map as string;
const set = _set as string;
const path = _path as string;
const json = _json as string;
const http = _http as string;
const bool = _bool as string;
const random = _random as string;
const duration = _duration as string;
const date = _date as string;
const time = _time as string;
const datetime = _datetime as string;
const test = _test as string;
const nodeTarget = _nodeTarget as string;
const denoTarget = _denoTarget as string;
const browserTarget = _browserTarget as string;
const cloudflareTarget = _cloudflareTarget as string;

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
  bool,
  random,
  duration,
  date,
  time,
  datetime,
  test,
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
