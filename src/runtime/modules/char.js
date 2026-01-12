// Char module - cross-platform
$foreign.Char = {
  toInt: (c) => c.charCodeAt(0),
  fromInt: (n) => (n < 0 || n > 0x10ffff ? null : [0, String.fromCodePoint(n)]),
  toString: (c) => c,
  eq: (a) => (b) => a === b,
  lt: (a) => (b) => a < b,
  isDigit: (c) => c >= "0" && c <= "9",
  isAlpha: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z"),
  isAlphaNum: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9"),
  isSpace: (c) => c === " " || c === "\t" || c === "\n" || c === "\r",
  isUpper: (c) => c >= "A" && c <= "Z",
  isLower: (c) => c >= "a" && c <= "z",
  toUpper: (c) => c.toUpperCase(),
  toLower: (c) => c.toLowerCase(),
  isIdentStart: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c === "_",
  isIdentChar: (c) =>
    (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9") || c === "_",
};
