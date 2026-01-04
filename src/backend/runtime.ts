/**
 * JavaScript Runtime for Algow
 *
 * Defines the runtime representation of values and helper functions
 * needed by generated JavaScript code.
 *
 * Value representations:
 * - Primitives: JavaScript primitives (number, string, boolean)
 * - Closures: Native JavaScript closures (no explicit env needed)
 * - Constructors: { $tag: "Name", $args: [...] }
 * - Tuples: JavaScript arrays
 * - Records: Plain JavaScript objects
 */

export const RUNTIME = `// Algow Runtime
"use strict";

/**
 * Apply a function to an argument.
 * Handles both closures and partial constructor application.
 */
const $apply = (fn, arg) => {
  if (typeof fn === "function") {
    return fn(arg);
  }
  // Constructor - partial application
  return { $tag: fn.$tag, $args: [...fn.$args, arg] };
};

/**
 * Create a constructor value.
 */
const $con = (tag, ...args) => ({ $tag: tag, $args: args });

/**
 * Deep structural equality for values.
 */
const $eq = (a, b) => {
  // Same reference or primitive equality
  if (a === b) return true;

  // Different types
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object" || a === null) return false;

  // Constructor equality
  if ("$tag" in a && "$tag" in b) {
    if (a.$tag !== b.$tag) return false;
    if (a.$args.length !== b.$args.length) return false;
    return a.$args.every((x, i) => $eq(x, b.$args[i]));
  }

  // Array (tuple) equality
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((x, i) => $eq(x, b[i]));
  }

  // Object (record) equality
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every(k => k in b && $eq(a[k], b[k]));
};

`;
