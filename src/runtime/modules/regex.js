// Regex module - cross-platform

// Parse pattern string into RegExp
// Supports: "pattern" or "/pattern/flags"
const parsePattern = (pattern, defaultFlags = "") => {
  if (pattern.startsWith("/")) {
    const lastSlash = pattern.lastIndexOf("/");
    if (lastSlash > 0) {
      const p = pattern.slice(1, lastSlash);
      const f = pattern.slice(lastSlash + 1);
      return new RegExp(p, f);
    }
  }
  return new RegExp(pattern, defaultFlags);
};

// Convert JS match result to Algow Match record
const toMatch = (m) => ({
  value: m[0],
  index: m.index,
  groups: (() => {
    const groups = m.slice(1);
    let result = null;
    for (let i = groups.length - 1; i >= 0; i--) {
      result = { h: groups[i] ?? "", t: result };
    }
    return result;
  })(),
});

// Convert JS array to Algow list
const toList = (arr) => {
  let result = null;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = { h: arr[i], t: result };
  }
  return result;
};

$foreign.Regex = {
  test: (pattern) => (s) => parsePattern(pattern).test(s),

  find: (pattern) => (s) => {
    const m = s.match(parsePattern(pattern));
    return m === null ? [0] : [1, toMatch(m)];
  },

  findAll: (pattern) => (s) => {
    const re = parsePattern(pattern, "g");
    // Ensure global flag for matchAll
    const globalRe = re.global ? re : new RegExp(re.source, re.flags + "g");
    const matches = [...s.matchAll(globalRe)].map(toMatch);
    return toList(matches);
  },

  replace: (pattern) => (replacement) => (s) => s.replace(parsePattern(pattern), replacement),

  replaceAll: (pattern) => (replacement) => (s) =>
    s.replaceAll(parsePattern(pattern, "g"), replacement),

  split: (pattern) => (s) => toList(s.split(parsePattern(pattern))),

  splitLimit: (pattern) => (limit) => (s) => {
    const parts = s.split(parsePattern(pattern));
    if (limit <= 0) return toList(parts);
    if (limit >= parts.length) return toList(parts);
    const head = parts.slice(0, limit - 1);
    const tail = parts.slice(limit - 1).join(
      // Re-match to get the actual delimiter for rejoining
      s.match(parsePattern(pattern))?.[0] ?? "",
    );
    return toList([...head, tail]);
  },
};
