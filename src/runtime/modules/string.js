// String module - cross-platform

const $String_length = (s) => s.length;

const $String_concat = (a) => (b) => a + b;

const $String_substring = (start) => (end) => (s) => s.substring(start, end);

const $String_charAt = (i) => (s) => (i < 0 || i >= s.length ? [0] : [1, s[i]]);

const $String_head = (s) => (s.length === 0 ? [0] : [1, s[0]]);

const $String_tail = (s) => (s.length === 0 ? "" : s.slice(1));

const $String_drop = (n) => (s) => s.slice(n);

const $String_take = (n) => (s) => s.slice(0, n);

const $String_isEmpty = (s) => s.length === 0;

const $String_toList = (s) => {
  let result = null;
  for (let i = s.length - 1; i >= 0; i--) result = { h: s[i], t: result };
  return result;
};

const $String_fromList = (list) => {
  let result = "";
  let current = list;
  while (current !== null) {
    result += current.h;
    current = current.t;
  }
  return result;
};

const $String_eq = (a) => (b) => a === b;

const $String_lt = (a) => (b) => a < b;

const $String_split = (delimiter) => (s) => {
  const parts = s.split(delimiter);
  let result = null;
  for (let i = parts.length - 1; i >= 0; i--) result = { h: parts[i], t: result };
  return result;
};

const $String_join = (separator) => (list) => {
  const parts = [];
  let current = list;
  while (current !== null) {
    parts.push(current.h);
    current = current.t;
  }
  return parts.join(separator);
};

const $String_trim = (s) => s.trim();

const $String_toUpper = (s) => s.toUpperCase();

const $String_toLower = (s) => s.toLowerCase();

const $String_contains = (needle) => (haystack) => haystack.includes(needle);

const $String_startsWith = (prefix) => (s) => s.startsWith(prefix);

const $String_endsWith = (suffix) => (s) => s.endsWith(suffix);

const $String_replace = (search) => (replacement) => (s) => s.replaceAll(search, replacement);

const $String_reverse = (s) => [...s].reverse().join("");

const $String_repeat = (n) => (s) => s.repeat(Math.max(0, n));

const $String_indexOf = (needle) => (s) => {
  const i = s.indexOf(needle);
  return i === -1 ? [0] : [1, i];
};

const $String_lastIndexOf = (needle) => (s) => {
  const i = s.lastIndexOf(needle);
  return i === -1 ? [0] : [1, i];
};

const $String_padLeft = (len) => (ch) => (s) => s.padStart(len, ch);

const $String_padRight = (len) => (ch) => (s) => s.padEnd(len, ch);

const $String_lines = (s) => {
  const parts = s.split(/\r?\n/);
  let result = null;
  for (let i = parts.length - 1; i >= 0; i--) result = { h: parts[i], t: result };
  return result;
};

const $String_unlines = (list) => {
  const parts = [];
  let cur = list;
  while (cur !== null) {
    parts.push(cur.h);
    cur = cur.t;
  }
  return parts.join("\n");
};

const $String_words = (s) => {
  const parts = s
    .trim()
    .split(/\s+/)
    .filter((x) => x.length > 0);
  let result = null;
  for (let i = parts.length - 1; i >= 0; i--) result = { h: parts[i], t: result };
  return result;
};

const $String_unwords = (list) => {
  const parts = [];
  let cur = list;
  while (cur !== null) {
    parts.push(cur.h);
    cur = cur.t;
  }
  return parts.join(" ");
};
