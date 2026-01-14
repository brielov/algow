// Target: Browser
// IO with console, File/Dir as stubs

// IO module
const $IO_print = (v) => {
  console.log($show(v));
  return null;
};

const $IO_printLine = (v) => {
  console.log($show(v));
  return null;
};

const $IO_exit = $unavailable("browser", "IO", "exit");

const $IO_getEnv = $unavailable("browser", "IO", "getEnv");

const $IO_getArgs = null;

// File module - not available in browser
const $File_read = $unavailable("browser", "File", "read");
const $File_write = $unavailable("browser", "File", "write");
const $File_append = $unavailable("browser", "File", "append");
const $File_remove = $unavailable("browser", "File", "remove");
const $File_copy = $unavailable("browser", "File", "copy");
const $File_rename = $unavailable("browser", "File", "rename");
const $File_touch = $unavailable("browser", "File", "touch");
const $File_exists = $unavailable("browser", "File", "exists");
const $File_size = $unavailable("browser", "File", "size");

// Dir module - not available in browser
const $Dir_create = $unavailable("browser", "Dir", "create");
const $Dir_remove = $unavailable("browser", "Dir", "remove");
const $Dir_list = $unavailable("browser", "Dir", "list");
const $Dir_exists = $unavailable("browser", "Dir", "exists");
