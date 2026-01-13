// Target: Deno
// IO, File, Dir modules using Deno APIs

$foreign.IO = {
  print: (s) => {
    Deno.stdout.writeSync(new TextEncoder().encode(s));
    return null;
  },
  printLine: (s) => {
    console.log(s);
    return null;
  },
  exit: (code) => {
    Deno.exit(code);
  },
  getEnv: (name) => {
    const value = Deno.env.get(name);
    return value === undefined ? null : [0, value];
  },
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
  read: (path) => {
    try {
      return [1, Deno.readTextFileSync(path)];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  write: (path) => (content) => {
    try {
      Deno.writeTextFileSync(path, content);
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  append: (path) => (content) => {
    try {
      Deno.writeTextFileSync(path, content, { append: true });
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  remove: (path) => {
    try {
      Deno.removeSync(path);
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  copy: (src) => (dest) => {
    try {
      Deno.copyFileSync(src, dest);
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, src)];
    }
  },
  rename: (src) => (dest) => {
    try {
      Deno.renameSync(src, dest);
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, src)];
    }
  },
  touch: (path) => {
    try {
      const now = new Date();
      try {
        Deno.utimeSync(path, now, now);
      } catch {
        Deno.writeTextFileSync(path, "", { create: true });
      }
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  exists: (path) => {
    try {
      return Deno.statSync(path).isFile;
    } catch {
      return false;
    }
  },
  size: (path) => {
    try {
      return [1, Deno.statSync(path).size];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
};

$foreign.Dir = {
  create: (path) => {
    try {
      Deno.mkdirSync(path, { recursive: true });
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  remove: (path) => {
    try {
      Deno.removeSync(path, { recursive: true });
      return [1, null];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  list: (path) => {
    try {
      const entries = [...Deno.readDirSync(path)].map((e) => e.name);
      let result = null;
      for (let i = entries.length - 1; i >= 0; i--) result = { h: entries[i], t: result };
      return [1, result];
    } catch (err) {
      return [0, $denoIoError(err, path)];
    }
  },
  exists: (path) => {
    try {
      return Deno.statSync(path).isDirectory;
    } catch {
      return false;
    }
  },
};
