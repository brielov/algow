// Float module - cross-platform

const $Float_add = (a) => (b) => a + b;

const $Float_sub = (a) => (b) => a - b;

const $Float_mul = (a) => (b) => a * b;

const $Float_div = (a) => (b) => a / b;

const $Float_neg = (a) => -a;

const $Float_abs = (a) => Math.abs(a);

const $Float_eq = (a) => (b) => a === b;

const $Float_lt = (a) => (b) => a < b;

const $Float_le = (a) => (b) => a <= b;

const $Float_gt = (a) => (b) => a > b;

const $Float_ge = (a) => (b) => a >= b;

const $Float_floor = (a) => Math.floor(a);

const $Float_ceil = (a) => Math.ceil(a);

const $Float_round = (a) => Math.round(a);

const $Float_sqrt = (a) => Math.sqrt(a);

const $Float_pow = (a) => (b) => Math.pow(a, b);

const $Float_sin = (a) => Math.sin(a);

const $Float_cos = (a) => Math.cos(a);

const $Float_tan = (a) => Math.tan(a);

const $Float_log = (a) => Math.log(a);

const $Float_exp = (a) => Math.exp(a);

const $Float_toString = (a) => String(a);

const $Float_fromString = (s) => {
  const n = parseFloat(s);
  return Number.isNaN(n) ? [0] : [1, n];
};
