// Char module - cross-platform

const $Char_toInt = (c) => c.charCodeAt(0);

const $Char_fromInt = (n) => (n < 0 || n > 0x10ffff ? [0] : [1, String.fromCodePoint(n)]);

const $Char_toString = (c) => c;

const $Char_eq = (a) => (b) => a === b;

const $Char_lt = (a) => (b) => a < b;

const $Char_isDigit = (c) => c >= "0" && c <= "9";

const $Char_isAlpha = (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z");

const $Char_isAlphaNum = (c) =>
  (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9");

const $Char_isSpace = (c) => c === " " || c === "\t" || c === "\n" || c === "\r";

const $Char_isUpper = (c) => c >= "A" && c <= "Z";

const $Char_isLower = (c) => c >= "a" && c <= "z";

const $Char_toUpper = (c) => c.toUpperCase();

const $Char_toLower = (c) => c.toLowerCase();

const $Char_isIdentStart = (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c === "_";

const $Char_isIdentChar = (c) =>
  (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9") || c === "_";
