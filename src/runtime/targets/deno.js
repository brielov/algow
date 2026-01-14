// Target: Deno
// IO, File, Dir modules using Deno APIs
// File and Dir operations are async for transparent async/await support

$foreign.IO = {
  print: (v) => {
    Deno.stdout.writeSync(new TextEncoder().encode($show(v)));
    return null;
  },
  printLine: (v) => {
    console.log($show(v));
    return null;
  },
  exit: (code) => {
    Deno.exit(code);
  },
  getEnv: (name) => {
    const value = Deno.env.get(name);
    return value === undefined ? [0] : [1, value];
  },
  getArgs: Deno.args.reduceRight((t, h) => ({ h, t }), null),
};

// Map Deno errors to IOError
const $denoIoError = (err, path) => {
  if (err instanceof Deno.errors.NotFound) return [0, path];
  if (err instanceof Deno.errors.PermissionDenied) return [1, path];
  if (err instanceof Deno.errors.IsADirectory) return [2, path];
  if (err instanceof Deno.errors.NotADirectory) return [3, path];
  if (err instanceof Deno.errors.AlreadyExists) return [4, path];
  if (err instanceof Deno.errors.NotEmpty) return [5, path];
  return [6, err.message];
};

$foreign.File = {
  read: async (path) => {
    try {
      const content = await Deno.readTextFile(path);
      return [1, content];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  write: (path) => async (content) => {
    try {
      await Deno.writeTextFile(path, content);
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  append: (path) => async (content) => {
    try {
      await Deno.writeTextFile(path, content, { append: true });
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  remove: async (path) => {
    try {
      await Deno.remove(path);
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  copy: (src) => async (dest) => {
    try {
      await Deno.copyFile(src, dest);
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, src)];
    }
  },
  rename: (src) => async (dest) => {
    try {
      await Deno.rename(src, dest);
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, src)];
    }
  },
  touch: async (path) => {
    try {
      const now = new Date();
      try {
        await Deno.utime(path, now, now);
      } catch {
        await Deno.writeTextFile(path, "", { create: true });
      }
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  exists: async (path) => {
    try {
      const stat = await Deno.stat(path);
      return stat.isFile;
    } catch {
      return false;
    }
  },
  size: async (path) => {
    try {
      const stat = await Deno.stat(path);
      return [1, stat.size];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
};

$foreign.Dir = {
  create: async (path) => {
    try {
      await Deno.mkdir(path, { recursive: true });
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  remove: async (path) => {
    try {
      await Deno.remove(path, { recursive: true });
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  list: async (path) => {
    try {
      const entries = [];
      for await (const entry of Deno.readDir(path)) {
        entries.push(entry.name);
      }
      let result = null;
      for (let i = entries.length - 1; i >= 0; i--) result = { h: entries[i], t: result };
      return [1, result];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  exists: async (path) => {
    try {
      const stat = await Deno.stat(path);
      return stat.isDirectory;
    } catch {
      return false;
    }
  },
};
