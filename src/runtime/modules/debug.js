// Debug module - cross-platform
$foreign.Debug = {
  log: (x) => {
    console.log(x);
    return x;
  },
  trace: (label) => (x) => {
    console.log(label + ":", x);
    return x;
  },
  panic: (msg) => {
    throw new Error(msg);
  },
};
