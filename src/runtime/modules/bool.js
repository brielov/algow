// Bool module - cross-platform

const $Bool_negate = (a) => !a;

const $Bool_both = (a) => (b) => a && b;

const $Bool_either = (a) => (b) => a || b;

const $Bool_xor = (a) => (b) => (a && !b) || (!a && b);
