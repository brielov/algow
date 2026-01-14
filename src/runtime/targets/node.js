// Target: Node.js / Bun
// IO, File, Dir modules using Node.js APIs

const $node_fs = /*#__PURE__*/ require("fs").promises;

// IO module
const $IO_print = (v) => {
  process.stdout.write($show(v));
  return null;
};

const $IO_printLine = (v) => {
  console.log($show(v));
  return null;
};

const $IO_exit = (code) => {
  process.exit(code);
};

const $IO_getEnv = (name) => {
  const value = process.env[name];
  return value === undefined ? [0] : [1, value];
};

const $IO_getArgs = /*#__PURE__*/ process.argv.slice(2).reduceRight((t, h) => ({ h, t }), null);

// File module
const $File_read = async (path) => {
  try {
    const content = await $node_fs.readFile(path, "utf8");
    return [1, content];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

const $File_write = (path) => async (content) => {
  try {
    await $node_fs.writeFile(path, content, "utf8");
    return [1, null];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

const $File_append = (path) => async (content) => {
  try {
    await $node_fs.appendFile(path, content, "utf8");
    return [1, null];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

const $File_remove = async (path) => {
  try {
    await $node_fs.unlink(path);
    return [1, null];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

const $File_copy = (src) => async (dest) => {
  try {
    await $node_fs.copyFile(src, dest);
    return [1, null];
  } catch (err) {
    return [0, $ioError(err, src)];
  }
};

const $File_rename = (src) => async (dest) => {
  try {
    await $node_fs.rename(src, dest);
    return [1, null];
  } catch (err) {
    return [0, $ioError(err, src)];
  }
};

const $File_touch = async (path) => {
  try {
    const now = new Date();
    try {
      await $node_fs.utimes(path, now, now);
    } catch {
      const handle = await $node_fs.open(path, "w");
      await handle.close();
    }
    return [1, null];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

const $File_exists = async (path) => {
  try {
    const stat = await $node_fs.stat(path);
    return stat.isFile();
  } catch {
    return false;
  }
};

const $File_size = async (path) => {
  try {
    const stat = await $node_fs.stat(path);
    return [1, stat.size];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

// Dir module
const $Dir_create = async (path) => {
  try {
    await $node_fs.mkdir(path, { recursive: true });
    return [1, null];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

const $Dir_remove = async (path) => {
  try {
    await $node_fs.rm(path, { recursive: true, force: true });
    return [1, null];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

const $Dir_list = async (path) => {
  try {
    const files = await $node_fs.readdir(path);
    let result = null;
    for (let i = files.length - 1; i >= 0; i--) result = { h: files[i], t: result };
    return [1, result];
  } catch (err) {
    return [0, $ioError(err, path)];
  }
};

const $Dir_exists = async (path) => {
  try {
    const stat = await $node_fs.stat(path);
    return stat.isDirectory();
  } catch {
    return false;
  }
};
