// Path module - cross-platform (pure JS implementation)

const $Path_join = (a) => (b) => {
  if (!a) return b;
  if (!b) return a;
  const aEnd = a.endsWith("/");
  const bStart = b.startsWith("/");
  if (aEnd && bStart) return a + b.slice(1);
  if (!aEnd && !bStart) return a + "/" + b;
  return a + b;
};

const $Path_dirname = (p) => {
  if (!p) return ".";
  const lastSlash = p.lastIndexOf("/");
  if (lastSlash === -1) return ".";
  if (lastSlash === 0) return "/";
  return p.slice(0, lastSlash);
};

const $Path_basename = (p) => {
  if (!p) return "";
  const lastSlash = p.lastIndexOf("/");
  return lastSlash === -1 ? p : p.slice(lastSlash + 1);
};

const $Path_extension = (p) => {
  const base = $Path_basename(p);
  const lastDot = base.lastIndexOf(".");
  if (lastDot <= 0) return [0]; // Nothing - no extension or hidden file
  return [1, base.slice(lastDot)]; // Just extension
};

const $Path_isAbsolute = (p) => p.startsWith("/");
