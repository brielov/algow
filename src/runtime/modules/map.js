// Map module - cross-platform
$foreign.Map = {
  empty: new Map(),
  singleton: (k) => (v) => new Map([[k, v]]),
  insert: (k) => (v) => (m) => new Map(m).set(k, v),
  lookup: (k) => (m) => (m.has(k) ? [1, m.get(k)] : [0]),
  delete: (k) => (m) => {
    const m2 = new Map(m);
    m2.delete(k);
    return m2;
  },
  member: (k) => (m) => m.has(k),
  size: (m) => m.size,
  keys: (m) => {
    let result = null;
    for (const k of [...m.keys()].reverse()) result = { h: k, t: result };
    return result;
  },
  values: (m) => {
    let result = null;
    for (const v of [...m.values()].reverse()) result = { h: v, t: result };
    return result;
  },
  toList: (m) => {
    let result = null;
    for (const [k, v] of [...m.entries()].reverse()) result = { h: [k, v], t: result };
    return result;
  },
  fromList: (list) => {
    const m = new Map();
    let current = list;
    while (current !== null) {
      const [k, v] = current.h;
      m.set(k, v);
      current = current.t;
    }
    return m;
  },
};
