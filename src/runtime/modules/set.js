// Set module - cross-platform
$foreign.Set = {
  empty: new Set(),
  singleton: (v) => new Set([v]),
  insert: (v) => (s) => new Set(s).add(v),
  delete: (v) => (s) => {
    const s2 = new Set(s);
    s2.delete(v);
    return s2;
  },
  member: (v) => (s) => s.has(v),
  size: (s) => s.size,
  toList: (s) => {
    let result = null;
    for (const v of [...s].reverse()) result = { h: v, t: result };
    return result;
  },
  fromList: (list) => {
    const s = new Set();
    let current = list;
    while (current !== null) {
      s.add(current.h);
      current = current.t;
    }
    return s;
  },
  union: (s1) => (s2) => s1.union(s2),
  intersect: (s1) => (s2) => s1.intersection(s2),
  difference: (s1) => (s2) => s1.difference(s2),
};
