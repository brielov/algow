-- =============================================================================
-- Division by Zero Example (This produces an error!)
-- =============================================================================
-- Uncomment this to see the compiler error.

-- ERROR: Division by zero
-- let crash = 10 / 0

-- The compiler detects division by literal zero at compile time.
-- For dynamic divisors, use safeDivide pattern:
--
--   let safeDivide = x y ->
--     if y == 0
--       then Nothing
--       else Just (x / y)

42
