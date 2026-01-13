-- =============================================================================
-- Tuple Index Out of Bounds Examples (These produce errors!)
-- =============================================================================
-- Uncomment these to see the compiler errors.

let pair = (1, 2)

-- ERROR: Tuple index 2 out of bounds. Tuple has 2 element(s) (indices 0-1)
-- let invalid1 = pair.2

-- ERROR: Tuple index 3 out of bounds. Tuple has 2 element(s) (indices 0-1)
-- let invalid2 = pair.3

let triple = (1, 2, 3)

-- ERROR: Tuple index 3 out of bounds. Tuple has 3 element(s) (indices 0-2)
-- let invalid3 = triple.3

-- ERROR: Tuple index 10 out of bounds. Tuple has 3 element(s) (indices 0-2)
-- let invalid4 = triple.10

-- This file demonstrates what NOT to do.
-- Always use valid indices for tuple access!
-- For a tuple with N elements, valid indices are 0 to N-1.

42
