// Float module - cross-platform
$foreign.Float = {
  add: (a) => (b) => a + b,
  sub: (a) => (b) => a - b,
  mul: (a) => (b) => a * b,
  div: (a) => (b) => a / b,
  neg: (a) => -a,
  abs: (a) => Math.abs(a),
  eq: (a) => (b) => a === b,
  lt: (a) => (b) => a < b,
  le: (a) => (b) => a <= b,
  gt: (a) => (b) => a > b,
  ge: (a) => (b) => a >= b,
  floor: (a) => Math.floor(a),
  ceil: (a) => Math.ceil(a),
  round: (a) => Math.round(a),
  sqrt: (a) => Math.sqrt(a),
  pow: (a) => (b) => Math.pow(a, b),
  sin: (a) => Math.sin(a),
  cos: (a) => Math.cos(a),
  tan: (a) => Math.tan(a),
  log: (a) => Math.log(a),
  exp: (a) => Math.exp(a),
  toString: (a) => String(a),
  fromString: (s) => {
    const n = parseFloat(s);
    return Number.isNaN(n) ? [0] : [1, n];
  },
};
