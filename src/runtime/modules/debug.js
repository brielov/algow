// Debug module - cross-platform

const $Debug_log = (x) => {
  console.log(x);
  return x;
};

const $Debug_trace = (label) => (x) => {
  console.log(label + ":", x);
  return x;
};

const $Debug_panic = (msg) => {
  throw new Error(msg);
};
