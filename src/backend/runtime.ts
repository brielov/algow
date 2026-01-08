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

/**
 * Foreign function registry.
 * Modules provide their foreign function implementations here.
 * Structure: { ModuleName: { functionName: implementation } }
 */
const $foreign = {
  // ==========================================================================
  // String Module
  // ==========================================================================
  String: {
    // length : String -> Int
    length: (s) => s.length,

    // concat : String -> String -> String
    concat: (a) => (b) => a + b,

    // substring : Int -> Int -> String -> String
    substring: (start) => (end) => (s) => s.substring(start, end),

    // charAt : Int -> String -> Maybe Char
    charAt: (i) => (s) => {
      if (i < 0 || i >= s.length) return $con("Nothing");
      return $con("Just", s[i]);
    },

    // toList : String -> List Char
    toList: (s) => {
      let result = $con("Nil");
      for (let i = s.length - 1; i >= 0; i--) {
        result = $con("Cons", s[i], result);
      }
      return result;
    },

    // fromList : List Char -> String
    fromList: (list) => {
      let result = "";
      let current = list;
      while (current.$tag === "Cons") {
        result += current.$args[0];
        current = current.$args[1];
      }
      return result;
    },

    // eq : String -> String -> Bool
    eq: (a) => (b) => a === b,

    // lt : String -> String -> Bool
    lt: (a) => (b) => a < b,

    // split : String -> String -> List String
    split: (delimiter) => (s) => {
      const parts = s.split(delimiter);
      let result = $con("Nil");
      for (let i = parts.length - 1; i >= 0; i--) {
        result = $con("Cons", parts[i], result);
      }
      return result;
    },

    // join : String -> List String -> String
    join: (separator) => (list) => {
      const parts = [];
      let current = list;
      while (current.$tag === "Cons") {
        parts.push(current.$args[0]);
        current = current.$args[1];
      }
      return parts.join(separator);
    },

    // trim : String -> String
    trim: (s) => s.trim(),

    // toUpper : String -> String
    toUpper: (s) => s.toUpperCase(),

    // toLower : String -> String
    toLower: (s) => s.toLowerCase(),

    // contains : String -> String -> Bool
    contains: (needle) => (haystack) => haystack.includes(needle),

    // startsWith : String -> String -> Bool
    startsWith: (prefix) => (s) => s.startsWith(prefix),

    // endsWith : String -> String -> Bool
    endsWith: (suffix) => (s) => s.endsWith(suffix),

    // replace : String -> String -> String -> String
    replace: (search) => (replacement) => (s) => s.replaceAll(search, replacement),
  },

  // ==========================================================================
  // Char Module
  // ==========================================================================
  Char: {
    // toInt : Char -> Int
    toInt: (c) => c.charCodeAt(0),

    // fromInt : Int -> Maybe Char
    fromInt: (n) => {
      if (n < 0 || n > 0x10FFFF) return $con("Nothing");
      return $con("Just", String.fromCodePoint(n));
    },

    // toString : Char -> String
    toString: (c) => c,

    // eq : Char -> Char -> Bool
    eq: (a) => (b) => a === b,

    // lt : Char -> Char -> Bool
    lt: (a) => (b) => a < b,

    // isDigit : Char -> Bool
    isDigit: (c) => c >= "0" && c <= "9",

    // isAlpha : Char -> Bool
    isAlpha: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z"),

    // isAlphaNum : Char -> Bool
    isAlphaNum: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9"),

    // isSpace : Char -> Bool
    isSpace: (c) => c === " " || c === "\\t" || c === "\\n" || c === "\\r",

    // isUpper : Char -> Bool
    isUpper: (c) => c >= "A" && c <= "Z",

    // isLower : Char -> Bool
    isLower: (c) => c >= "a" && c <= "z",

    // toUpper : Char -> Char
    toUpper: (c) => c.toUpperCase(),

    // toLower : Char -> Char
    toLower: (c) => c.toLowerCase(),
  },
};

`;
