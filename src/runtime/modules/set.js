// Set module - cross-platform

const $Set_empty = /*#__PURE__*/ new Set();

const $Set_singleton = (v) => new Set([v]);

const $Set_insert = (v) => (s) => new Set(s).add(v);

const $Set_delete = (v) => (s) => {
  const s2 = new Set(s);
  s2.delete(v);
  return s2;
};

const $Set_member = (v) => (s) => s.has(v);

const $Set_size = (s) => s.size;

const $Set_toList = (s) => {
  let result = null;
  for (const v of [...s].reverse()) result = { h: v, t: result };
  return result;
};

const $Set_fromList = (list) => {
  const s = new Set();
  let current = list;
  while (current !== null) {
    s.add(current.h);
    current = current.t;
  }
  return s;
};

const $Set_union = (s1) => (s2) => s1.union(s2);

const $Set_intersect = (s1) => (s2) => s1.intersection(s2);

const $Set_difference = (s1) => (s2) => s1.difference(s2);
