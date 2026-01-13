// Random module - cross-platform
$foreign.Random = {
  int: (min) => (max) => Math.floor(Math.random() * (max - min + 1)) + min,
  get float() {
    return Math.random();
  },
  get bool() {
    return Math.random() < 0.5;
  },
  pick: (list) => {
    if (list === null) return [0]; // Nothing
    const items = [];
    let cur = list;
    while (cur !== null) {
      items.push(cur.h);
      cur = cur.t;
    }
    if (items.length === 0) return [0]; // Nothing
    return [1, items[Math.floor(Math.random() * items.length)]]; // Just
  },
  shuffle: (list) => {
    if (list === null) return null;
    const items = [];
    let cur = list;
    while (cur !== null) {
      items.push(cur.h);
      cur = cur.t;
    }
    // Fisher-Yates shuffle
    for (let i = items.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [items[i], items[j]] = [items[j], items[i]];
    }
    let result = null;
    for (let i = items.length - 1; i >= 0; i--) {
      result = { h: items[i], t: result };
    }
    return result;
  },
};
