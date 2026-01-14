// Int module - cross-platform

const $Int_add = (a) => (b) => a + b;

const $Int_sub = (a) => (b) => a - b;

const $Int_mul = (a) => (b) => a * b;

const $Int_div = (a) => (b) => Math.trunc(a / b);

const $Int_mod = (a) => (b) => a % b;

const $Int_neg = (a) => -a;

const $Int_abs = (a) => Math.abs(a);

const $Int_eq = (a) => (b) => a === b;

const $Int_lt = (a) => (b) => a < b;

const $Int_le = (a) => (b) => a <= b;

const $Int_gt = (a) => (b) => a > b;

const $Int_ge = (a) => (b) => a >= b;

const $Int_toFloat = (a) => a;

const $Int_toString = (a) => String(a);

const $Int_fromString = (s) => {
  const n = parseInt(s, 10);
  return Number.isNaN(n) ? [0] : [1, n];
};
