// Algow Runtime - Base Functions
// Each function is a standalone const for tree-shaking

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

const $showInner = (v) => {
  if (typeof v === "string") {
    // Use split/join instead of regex to avoid parser issues
    return '"' + v.split("\\").join("\\\\").split('"').join('\\"') + '"';
  }
  return $show(v);
};

const $show = (v) => {
  // Primitives
  if (typeof v === "string") return v;
  if (typeof v === "number") return String(v);
  if (typeof v === "boolean") return v ? "true" : "false";
  if (v === null) return "()";

  // Non-object primitives
  if (typeof v !== "object") return String(v);

  // List (Cons cells with h/t fields)
  if ("h" in v && "t" in v) {
    const items = [];
    let cur = v;
    while (cur !== null) {
      items.push($showInner(cur.h));
      cur = cur.t;
    }
    return "[" + items.join(", ") + "]";
  }

  // ADT or Tuple (arrays)
  if (Array.isArray(v)) {
    if (v.length > 0 && typeof v[0] === "number" && Number.isInteger(v[0])) {
      // ADT: [tag, arg1, arg2, ...]
      const tag = v[0];
      if (v.length === 1) return "<" + tag + ">";
      const args = v.slice(1).map($showInner).join(", ");
      return "<" + tag + ": " + args + ">";
    }
    // Tuple
    return "(" + v.map($showInner).join(", ") + ")";
  }

  // Record
  const entries = Object.entries(v);
  if (entries.length === 0) return "{}";
  const fields = entries.map(([k, val]) => k + ": " + $showInner(val)).join(", ");
  return "{ " + fields + " }";
};

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

const $unavailable = (target, module, fn) => () => {
  throw new Error(`${module}.${fn} is not available in target "${target}"`);
};
