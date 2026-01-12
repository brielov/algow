// Algow Runtime - Base
"use strict";

// Deep structural equality
const $eq = (a, b) => {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object" || a === null || b === null) return false;

  // List equality (objects with h/t fields)
  if ("h" in a && "t" in a && "h" in b && "t" in b) {
    return $eq(a.h, b.h) && $eq(a.t, b.t);
  }

  // ADT equality (arrays with numeric tag at index 0)
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((x, i) => $eq(x, b[i]));
  }

  // Record equality
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every((k) => k in b && $eq(a[k], b[k]));
};

// Map errno codes to IOError constructors
// IOError = FileNotFound(0) | PermissionDenied(1) | IsDirectory(2) | NotDirectory(3) | AlreadyExists(4) | NotEmpty(5) | UnknownError(6)
const $ioError = (err, path) => {
  const code = err.code;
  if (code === "ENOENT") return [0, path]; // FileNotFound
  if (code === "EACCES" || code === "EPERM") return [1, path]; // PermissionDenied
  if (code === "EISDIR") return [2, path]; // IsDirectory
  if (code === "ENOTDIR") return [3, path]; // NotDirectory
  if (code === "EEXIST") return [4, path]; // AlreadyExists
  if (code === "ENOTEMPTY") return [5, path]; // NotEmpty
  return [6, err.message]; // UnknownError
};

// Stub generator for unavailable features
const $unavailable = (target, module, fn) => () => {
  throw new Error(`${module}.${fn} is not available in target "${target}"`);
};

// Foreign function registry
const $foreign = {};
