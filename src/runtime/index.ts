/**
 * Runtime Module
 *
 * Provides runtime code for each target platform.
 * All functions use $-prefixed names for terser DCE compatibility.
 */

// Import runtime modules as text
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
import _regex from "./modules/regex.js" with { type: "text" };
import _nodeTarget from "./targets/node.js" with { type: "text" };
import _denoTarget from "./targets/deno.js" with { type: "text" };
import _browserTarget from "./targets/browser.js" with { type: "text" };
import _cloudflareTarget from "./targets/cloudflare.js" with { type: "text" };

export type Target = "node" | "deno" | "browser" | "cloudflare";

export const TARGETS: readonly Target[] = ["node", "deno", "browser", "cloudflare"] as const;

export const DEFAULT_TARGET: Target = "node";

export const isValidTarget = (target: string): target is Target => {
  return TARGETS.includes(target as Target);
};

// Cross-platform modules (shared across all targets)
const crossPlatform = [
  _base,
  _string,
  _char,
  _int,
  _float,
  _debug,
  _map,
  _set,
  _path,
  _json,
  _http,
  _bool,
  _random,
  _duration,
  _date,
  _time,
  _datetime,
  _test,
  _regex,
].join("\n");

// Full runtime for each target (cross-platform + target-specific)
const RUNTIMES: Record<Target, string> = {
  node: `${crossPlatform}\n${_nodeTarget}`,
  deno: `${crossPlatform}\n${_denoTarget}`,
  browser: `${crossPlatform}\n${_browserTarget}`,
  cloudflare: `${crossPlatform}\n${_cloudflareTarget}`,
};

/**
 * Get the full runtime code for a target.
 * Terser with toplevel=true will eliminate unused functions.
 */
export const getRuntime = (target: Target): string => {
  return RUNTIMES[target];
};
