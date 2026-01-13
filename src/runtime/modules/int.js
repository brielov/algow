// Int module - cross-platform
$foreign.Int = {
  add: (a) => (b) => a + b,
  sub: (a) => (b) => a - b,
  mul: (a) => (b) => a * b,
  div: (a) => (b) => Math.trunc(a / b),
  mod: (a) => (b) => a % b,
  neg: (a) => -a,
  abs: (a) => Math.abs(a),
  eq: (a) => (b) => a === b,
  lt: (a) => (b) => a < b,
  le: (a) => (b) => a <= b,
  gt: (a) => (b) => a > b,
  ge: (a) => (b) => a >= b,
  toFloat: (a) => a,
  toString: (a) => String(a),
  fromString: (s) => {
    const n = parseInt(s, 10);
    return Number.isNaN(n) ? [0] : [1, n];
  },
};
