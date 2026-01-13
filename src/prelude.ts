/**
 * Prelude Module
 *
 * Centralizes prelude loading for use by both CLI and LSP.
 */

import _core from "../lib/prelude/core.alg" with { type: "text" };
import _data from "../lib/prelude/data.alg" with { type: "text" };
import _http from "../lib/prelude/http.alg" with { type: "text" };
import _io from "../lib/prelude/io.alg" with { type: "text" };
import _primitives from "../lib/prelude/primitives.alg" with { type: "text" };
import _random from "../lib/prelude/random.alg" with { type: "text" };
import _test from "../lib/prelude/test.alg" with { type: "text" };
import _time from "../lib/prelude/time.alg" with { type: "text" };
import type { SourceFile } from "./compile";

/** Combined prelude content */
export const preludeContent = [_core, _primitives, _io, _data, _http, _time, _random, _test].join(
  "\n",
);

/** Prelude as a SourceFile */
export const prelude: SourceFile = { path: "<prelude>", content: preludeContent };
