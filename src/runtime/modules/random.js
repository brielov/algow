// Random module - cross-platform

const $Random_int = (min) => (max) => Math.floor(Math.random() * (max - min + 1)) + min;

const $Random_float = () => Math.random();

const $Random_bool = () => Math.random() < 0.5;

const $Random_pick = (list) => {
  if (list === null) return [0]; // Nothing
  const items = [];
  let cur = list;
  while (cur !== null) {
    items.push(cur.h);
    cur = cur.t;
  }
  if (items.length === 0) return [0]; // Nothing
  return [1, items[Math.floor(Math.random() * items.length)]]; // Just
};

const $Random_shuffle = (list) => {
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
};
