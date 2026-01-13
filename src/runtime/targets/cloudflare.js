// Target: Cloudflare Workers
// Limited IO, no File/Dir access

$foreign.IO = {
  print: (s) => {
    console.log(s);
    return null;
  },
  printLine: (s) => {
    console.log(s);
    return null;
  },
  exit: $unavailable("cloudflare", "IO", "exit"),
  getEnv: (name) => {
    // Cloudflare Workers can access env via globalThis
    // This requires the env to be bound in wrangler.toml
    const value = globalThis[name];
    return value === undefined ? null : [0, String(value)];
  },
};

// File system is not available in Cloudflare Workers
$foreign.File = {
  read: $unavailable("cloudflare", "File", "read"),
  write: $unavailable("cloudflare", "File", "write"),
  append: $unavailable("cloudflare", "File", "append"),
  remove: $unavailable("cloudflare", "File", "remove"),
  copy: $unavailable("cloudflare", "File", "copy"),
  rename: $unavailable("cloudflare", "File", "rename"),
  touch: $unavailable("cloudflare", "File", "touch"),
  exists: $unavailable("cloudflare", "File", "exists"),
  size: $unavailable("cloudflare", "File", "size"),
};

$foreign.Dir = {
  create: $unavailable("cloudflare", "Dir", "create"),
  remove: $unavailable("cloudflare", "Dir", "remove"),
  list: $unavailable("cloudflare", "Dir", "list"),
  exists: $unavailable("cloudflare", "Dir", "exists"),
};
