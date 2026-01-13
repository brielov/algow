// Assert module - testing utilities

const fail = (msg) => {
  console.error(`\x1b[31mAssertion failed:\x1b[0m ${msg}`);
  process.exit(1);
};

// User-friendly stringification for assertion output
const stringify = (value) => {
  if (value === null) return "Nil";
  if (typeof value === "object" && "h" in value && "t" in value) {
    const items = [];
    let current = value;
    while (current !== null) {
      items.push(stringify(current.h));
      current = current.t;
    }
    return `[${items.join(", ")}]`;
  }
  if (Array.isArray(value)) {
    if (value.length === 1 && value[0] === 0) return "Nothing";
    if (value.length === 2 && value[0] === 0) return `Left(${stringify(value[1])})`;
    if (value.length === 2 && value[0] === 1) return `Just(${stringify(value[1])})`;
    return `(${value.map(stringify).join(", ")})`;
  }
  if (typeof value === "string") return `"${value}"`;
  return String(value);
};

$foreign.Assert = {
  equal: (expected) => (actual) => (msg) => {
    if (!$eq(expected, actual)) {
      fail(`${msg}\n  expected: ${stringify(expected)}\n  actual:   ${stringify(actual)}`);
    }
  },
  notEqual: (a) => (b) => (msg) => {
    if ($eq(a, b)) {
      fail(`${msg}\n  values should differ: ${stringify(a)}`);
    }
  },
  ok: (cond) => (msg) => {
    if (!cond) fail(msg);
  },
  fail: (msg) => {
    fail(msg);
  },
};
