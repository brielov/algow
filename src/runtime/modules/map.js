// Map module - cross-platform

const $Map_empty = /*#__PURE__*/ new Map();

const $Map_singleton = (k) => (v) => new Map([[k, v]]);

const $Map_insert = (k) => (v) => (m) => new Map(m).set(k, v);

const $Map_lookup = (k) => (m) => (m.has(k) ? [1, m.get(k)] : [0]);

const $Map_delete = (k) => (m) => {
  const m2 = new Map(m);
  m2.delete(k);
  return m2;
};

const $Map_member = (k) => (m) => m.has(k);

const $Map_size = (m) => m.size;

const $Map_keys = (m) => {
  let result = null;
  for (const k of [...m.keys()].reverse()) result = { h: k, t: result };
  return result;
};

const $Map_values = (m) => {
  let result = null;
  for (const v of [...m.values()].reverse()) result = { h: v, t: result };
  return result;
};

const $Map_toList = (m) => {
  let result = null;
  for (const [k, v] of [...m.entries()].reverse()) result = { h: [k, v], t: result };
  return result;
};

const $Map_fromList = (list) => {
  const m = new Map();
  let current = list;
  while (current !== null) {
    const [k, v] = current.h;
    m.set(k, v);
    current = current.t;
  }
  return m;
};
