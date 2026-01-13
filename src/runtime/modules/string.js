// String module - cross-platform
$foreign.String = {
  length: (s) => s.length,
  concat: (a) => (b) => a + b,
  substring: (start) => (end) => (s) => s.substring(start, end),
  charAt: (i) => (s) => (i < 0 || i >= s.length ? [0] : [1, s[i]]),
  head: (s) => (s.length === 0 ? [0] : [1, s[0]]),
  tail: (s) => (s.length === 0 ? "" : s.slice(1)),
  drop: (n) => (s) => s.slice(n),
  take: (n) => (s) => s.slice(0, n),
  isEmpty: (s) => s.length === 0,
  toList: (s) => {
    let result = null;
    for (let i = s.length - 1; i >= 0; i--) result = { h: s[i], t: result };
    return result;
  },
  fromList: (list) => {
    let result = "";
    let current = list;
    while (current !== null) {
      result += current.h;
      current = current.t;
    }
    return result;
  },
  eq: (a) => (b) => a === b,
  lt: (a) => (b) => a < b,
  split: (delimiter) => (s) => {
    const parts = s.split(delimiter);
    let result = null;
    for (let i = parts.length - 1; i >= 0; i--) result = { h: parts[i], t: result };
    return result;
  },
  join: (separator) => (list) => {
    const parts = [];
    let current = list;
    while (current !== null) {
      parts.push(current.h);
      current = current.t;
    }
    return parts.join(separator);
  },
  trim: (s) => s.trim(),
  toUpper: (s) => s.toUpperCase(),
  toLower: (s) => s.toLowerCase(),
  contains: (needle) => (haystack) => haystack.includes(needle),
  startsWith: (prefix) => (s) => s.startsWith(prefix),
  endsWith: (suffix) => (s) => s.endsWith(suffix),
  replace: (search) => (replacement) => (s) => s.replaceAll(search, replacement),
  reverse: (s) => [...s].reverse().join(""),
  repeat: (n) => (s) => s.repeat(Math.max(0, n)),
  indexOf: (needle) => (s) => {
    const i = s.indexOf(needle);
    return i === -1 ? [0] : [1, i];
  },
  lastIndexOf: (needle) => (s) => {
    const i = s.lastIndexOf(needle);
    return i === -1 ? [0] : [1, i];
  },
  padLeft: (len) => (ch) => (s) => s.padStart(len, ch),
  padRight: (len) => (ch) => (s) => s.padEnd(len, ch),
  lines: (s) => {
    const parts = s.split(/\r?\n/);
    let result = null;
    for (let i = parts.length - 1; i >= 0; i--) result = { h: parts[i], t: result };
    return result;
  },
  unlines: (list) => {
    const parts = [];
    let cur = list;
    while (cur !== null) {
      parts.push(cur.h);
      cur = cur.t;
    }
    return parts.join("\n");
  },
  words: (s) => {
    const parts = s
      .trim()
      .split(/\s+/)
      .filter((x) => x.length > 0);
    let result = null;
    for (let i = parts.length - 1; i >= 0; i--) result = { h: parts[i], t: result };
    return result;
  },
  unwords: (list) => {
    const parts = [];
    let cur = list;
    while (cur !== null) {
      parts.push(cur.h);
      cur = cur.t;
    }
    return parts.join(" ");
  },
};
