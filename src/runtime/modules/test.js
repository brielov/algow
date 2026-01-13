// Test module - collects results and reports at the end

// User-friendly stringification for test output
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

// ADT: TestPass string = [0, name], TestFail string string = [1, name, message]
$foreign.Test = {
  equal: (name) => (expected) => (actual) => {
    if ($eq(expected, actual)) {
      return [0, name];
    }
    return [1, name, `expected: ${stringify(expected)}\n  actual:   ${stringify(actual)}`];
  },
  notEqual: (name) => (a) => (b) => {
    if (!$eq(a, b)) {
      return [0, name];
    }
    return [1, name, `values should differ: ${stringify(a)}`];
  },
  ok: (name) => (cond) => {
    if (cond) {
      return [0, name];
    }
    return [1, name, "condition was false"];
  },
  fail: (name) => {
    return [1, name, "explicit failure"];
  },
  run: (results) => {
    const failures = [];
    let total = 0;

    // Iterate through the linked list
    let current = results;
    while (current !== null) {
      total++;
      const result = current.h;
      if (result[0] === 1) {
        failures.push({ name: result[1], message: result[2] });
      }
      current = current.t;
    }

    // Report failures
    for (const { name, message } of failures) {
      console.error(`\x1b[31m✗ ${name}\x1b[0m`);
      console.error(`  ${message}`);
    }

    // Summary
    const passed = total - failures.length;
    if (failures.length === 0) {
      console.log(`\x1b[32m${passed}/${total} tests passed\x1b[0m`);
    } else {
      console.log(`\n\x1b[31m${passed}/${total} tests passed\x1b[0m`);
      process.exit(1);
    }
  },
};
