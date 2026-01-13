// Target: Node.js / Bun
// IO, File, Dir modules using Node.js APIs
// File and Dir operations are async for transparent async/await support

const fs = require("fs").promises;

$foreign.IO = {
  print: (s) => {
    process.stdout.write(s);
    return null;
  },
  printLine: (s) => {
    console.log(s);
    return null;
  },
  exit: (code) => {
    process.exit(code);
  },
  getEnv: (name) => {
    const value = process.env[name];
    return value === undefined ? null : [0, value];
  },
};

$foreign.File = {
  read: async (path) => {
    try {
      const content = await fs.readFile(path, "utf8");
      return [1, content];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  write: (path) => async (content) => {
    try {
      await fs.writeFile(path, content, "utf8");
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  append: (path) => async (content) => {
    try {
      await fs.appendFile(path, content, "utf8");
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  remove: async (path) => {
    try {
      await fs.unlink(path);
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  copy: (src) => async (dest) => {
    try {
      await fs.copyFile(src, dest);
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, src)];
    }
  },
  rename: (src) => async (dest) => {
    try {
      await fs.rename(src, dest);
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, src)];
    }
  },
  touch: async (path) => {
    try {
      const now = new Date();
      try {
        await fs.utimes(path, now, now);
      } catch {
        const handle = await fs.open(path, "w");
        await handle.close();
      }
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  exists: async (path) => {
    try {
      const stat = await fs.stat(path);
      return stat.isFile();
    } catch {
      return false;
    }
  },
  size: async (path) => {
    try {
      const stat = await fs.stat(path);
      return [1, stat.size];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
};

$foreign.Dir = {
  create: async (path) => {
    try {
      await fs.mkdir(path, { recursive: true });
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  remove: async (path) => {
    try {
      await fs.rm(path, { recursive: true, force: true });
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  list: async (path) => {
    try {
      const files = await fs.readdir(path);
      let result = null;
      for (let i = files.length - 1; i >= 0; i--) result = { h: files[i], t: result };
      return [1, result];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  exists: async (path) => {
    try {
      const stat = await fs.stat(path);
      return stat.isDirectory();
    } catch {
      return false;
    }
  },
};
