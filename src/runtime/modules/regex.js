// Regex module - cross-platform

// Parse pattern string into RegExp
// Supports: "pattern" or "/pattern/flags"
const $Regex_parsePattern = (pattern, defaultFlags = "") => {
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
const $Regex_toMatch = (m) => ({
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
const $Regex_toList = (arr) => {
  let result = null;
  for (let i = arr.length - 1; i >= 0; i--) {
    result = { h: arr[i], t: result };
  }
  return result;
};

const $Regex_test = (pattern) => (s) => $Regex_parsePattern(pattern).test(s);

const $Regex_find = (pattern) => (s) => {
  const m = s.match($Regex_parsePattern(pattern));
  return m === null ? [0] : [1, $Regex_toMatch(m)];
};

const $Regex_findAll = (pattern) => (s) => {
  const re = $Regex_parsePattern(pattern, "g");
  // Ensure global flag for matchAll
  const globalRe = re.global ? re : new RegExp(re.source, re.flags + "g");
  const matches = [...s.matchAll(globalRe)].map($Regex_toMatch);
  return $Regex_toList(matches);
};

const $Regex_replace = (pattern) => (replacement) => (s) =>
  s.replace($Regex_parsePattern(pattern), replacement);

const $Regex_replaceAll = (pattern) => (replacement) => (s) =>
  s.replaceAll($Regex_parsePattern(pattern, "g"), replacement);

const $Regex_split = (pattern) => (s) => $Regex_toList(s.split($Regex_parsePattern(pattern)));

const $Regex_splitLimit = (pattern) => (limit) => (s) => {
  const parts = s.split($Regex_parsePattern(pattern));
  if (limit <= 0) return $Regex_toList(parts);
  if (limit >= parts.length) return $Regex_toList(parts);
  const head = parts.slice(0, limit - 1);
  const tail = parts.slice(limit - 1).join(
    // Re-match to get the actual delimiter for rejoining
    s.match($Regex_parsePattern(pattern))?.[0] ?? "",
  );
  return $Regex_toList([...head, tail]);
};
