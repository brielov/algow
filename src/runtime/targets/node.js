// Target: Node.js / Bun
// IO, File, Dir modules using Node.js APIs

$foreign.IO = {
  print: (s) => {
    process.stdout.write(s);
    return null;
  },
  printLine: (s) => {
    console.log(s);
    return null;
  },
  args: (_) => {
    const args = process.argv.slice(2);
    let result = null;
    for (let i = args.length - 1; i >= 0; i--) result = { h: args[i], t: result };
    return result;
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
  read: (path) => {
    try {
      const fs = require("fs");
      return [1, fs.readFileSync(path, "utf8")];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  write: (path) => (content) => {
    try {
      const fs = require("fs");
      fs.writeFileSync(path, content, "utf8");
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  append: (path) => (content) => {
    try {
      const fs = require("fs");
      fs.appendFileSync(path, content, "utf8");
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  remove: (path) => {
    try {
      const fs = require("fs");
      fs.unlinkSync(path);
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  copy: (src) => (dest) => {
    try {
      const fs = require("fs");
      fs.copyFileSync(src, dest);
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, src)];
    }
  },
  rename: (src) => (dest) => {
    try {
      const fs = require("fs");
      fs.renameSync(src, dest);
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, src)];
    }
  },
  touch: (path) => {
    try {
      const fs = require("fs");
      const now = new Date();
      try {
        fs.utimesSync(path, now, now);
      } catch {
        fs.closeSync(fs.openSync(path, "w"));
      }
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  exists: (path) => {
    try {
      const fs = require("fs");
      return fs.statSync(path).isFile();
    } catch {
      return false;
    }
  },
  size: (path) => {
    try {
      const fs = require("fs");
      return [1, fs.statSync(path).size];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
};

$foreign.Dir = {
  create: (path) => {
    try {
      const fs = require("fs");
      fs.mkdirSync(path, { recursive: true });
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  remove: (path) => {
    try {
      const fs = require("fs");
      fs.rmSync(path, { recursive: true, force: true });
      return [1, null];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  list: (path) => {
    try {
      const fs = require("fs");
      const files = fs.readdirSync(path);
      let result = null;
      for (let i = files.length - 1; i >= 0; i--) result = { h: files[i], t: result };
      return [1, result];
    } catch (err) {
      return [0, $ioError(err, path)];
    }
  },
  exists: (path) => {
    try {
      const fs = require("fs");
      return fs.statSync(path).isDirectory();
    } catch {
      return false;
    }
  },
};
