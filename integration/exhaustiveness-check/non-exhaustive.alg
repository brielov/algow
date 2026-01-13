-- =============================================================================
-- Non-Exhaustive Pattern Examples (These produce warnings!)
-- =============================================================================
-- Uncomment these to see the compiler warnings.

type Status = Pending | Running | Completed | Failed

-- WARNING: Non-exhaustive - missing Failed
-- let describe1 = s ->
--   match s
--     when Pending -> "waiting"
--     when Running -> "in progress"
--     when Completed -> "done"
--   end

-- WARNING: Non-exhaustive - missing Nothing
-- let unsafeUnwrap = opt ->
--   match opt
--     when Just x -> x
--   end

-- WARNING: Non-exhaustive - missing false
-- let trueOnly = b ->
--   match b
--     when true -> 1
--   end

-- WARNING: Non-exhaustive - missing Cons
-- let emptyOnly = xs ->
--   match xs
--     when Nil -> 0
--   end

-- This file demonstrates what NOT to do.
-- Always ensure your pattern matches cover all cases!
-- Use wildcards (_) if you don't need to handle specific cases differently.

42
