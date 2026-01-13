// Bool module - cross-platform
$foreign.Bool = {
  negate: (a) => !a,
  both: (a) => (b) => a && b,
  either: (a) => (b) => a || b,
  xor: (a) => (b) => (a && !b) || (!a && b),
};
