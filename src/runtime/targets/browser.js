// Target: Browser
// IO with console, File/Dir as stubs

$foreign.IO = {
  print: (v) => {
    // Browser has no stdout, use console
    console.log($show(v));
    return null;
  },
  printLine: (v) => {
    console.log($show(v));
    return null;
  },
  exit: $unavailable("browser", "IO", "exit"),
  getEnv: $unavailable("browser", "IO", "getEnv"),
  getArgs: null,
};

// File system is not available in browser
$foreign.File = {
  read: $unavailable("browser", "File", "read"),
  write: $unavailable("browser", "File", "write"),
  append: $unavailable("browser", "File", "append"),
  remove: $unavailable("browser", "File", "remove"),
  copy: $unavailable("browser", "File", "copy"),
  rename: $unavailable("browser", "File", "rename"),
  touch: $unavailable("browser", "File", "touch"),
  exists: $unavailable("browser", "File", "exists"),
  size: $unavailable("browser", "File", "size"),
};

$foreign.Dir = {
  create: $unavailable("browser", "Dir", "create"),
  remove: $unavailable("browser", "Dir", "remove"),
  list: $unavailable("browser", "Dir", "list"),
  exists: $unavailable("browser", "Dir", "exists"),
};
