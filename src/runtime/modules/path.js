// Path module - cross-platform (pure JS implementation)
$foreign.Path = {
  join: (a) => (b) => {
    if (!a) return b;
    if (!b) return a;
    const aEnd = a.endsWith("/");
    const bStart = b.startsWith("/");
    if (aEnd && bStart) return a + b.slice(1);
    if (!aEnd && !bStart) return a + "/" + b;
    return a + b;
  },
  dirname: (p) => {
    if (!p) return ".";
    const lastSlash = p.lastIndexOf("/");
    if (lastSlash === -1) return ".";
    if (lastSlash === 0) return "/";
    return p.slice(0, lastSlash);
  },
  basename: (p) => {
    if (!p) return "";
    const lastSlash = p.lastIndexOf("/");
    return lastSlash === -1 ? p : p.slice(lastSlash + 1);
  },
  extension: (p) => {
    const base = $foreign.Path.basename(p);
    const lastDot = base.lastIndexOf(".");
    if (lastDot <= 0) return null; // No extension or hidden file without extension
    return [0, base.slice(lastDot)];
  },
  isAbsolute: (p) => p.startsWith("/"),
};
