// Target: Cloudflare Workers
// Limited IO, no File/Dir access

// IO module
const $IO_print = (v) => {
  console.log($show(v));
  return null;
};

const $IO_printLine = (v) => {
  console.log($show(v));
  return null;
};

const $IO_exit = $unavailable("cloudflare", "IO", "exit");

const $IO_getEnv = (name) => {
  // Cloudflare Workers can access env via globalThis
  // This requires the env to be bound in wrangler.toml
  const value = globalThis[name];
  return value === undefined ? [0] : [1, String(value)];
};

const $IO_getArgs = null;

// File module - not available in Cloudflare Workers
const $File_read = $unavailable("cloudflare", "File", "read");
const $File_write = $unavailable("cloudflare", "File", "write");
const $File_append = $unavailable("cloudflare", "File", "append");
const $File_remove = $unavailable("cloudflare", "File", "remove");
const $File_copy = $unavailable("cloudflare", "File", "copy");
const $File_rename = $unavailable("cloudflare", "File", "rename");
const $File_touch = $unavailable("cloudflare", "File", "touch");
const $File_exists = $unavailable("cloudflare", "File", "exists");
const $File_size = $unavailable("cloudflare", "File", "size");

// Dir module - not available in Cloudflare Workers
const $Dir_create = $unavailable("cloudflare", "Dir", "create");
const $Dir_remove = $unavailable("cloudflare", "Dir", "remove");
const $Dir_list = $unavailable("cloudflare", "Dir", "list");
const $Dir_exists = $unavailable("cloudflare", "Dir", "exists");
