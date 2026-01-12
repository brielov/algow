-- =============================================================================
-- Invalid Or-Pattern Examples (These produce errors!)
-- =============================================================================
-- Uncomment these to see the compiler errors.

-- ERROR: Or-pattern alternatives must bind the same variables.
--        Left side binds: x
--        Right side binds: (none)
-- let invalid1 = opt ->
--   match opt
--     when Just x | Nothing -> 0
--   end

-- ERROR: Or-pattern alternatives must bind the same variables.
--        Left side binds: a
--        Right side binds: b
-- let invalid2 = opt ->
--   match opt
--     when Just a | Just b -> 0
--     when Nothing -> 1
--   end

-- ERROR: Or-pattern alternatives must bind the same variables.
--        Left side binds: x, y
--        Right side binds: x
-- let invalid3 = pair ->
--   match pair
--     when (x, y) | (x, 0) -> x
--   end

-- VALID: Both sides bind the same variable 'x'
let valid1 = opt ->
  match opt
    when Just (Just x) | Just (Right x) -> x
    when _ -> 0
  end

-- VALID: Neither side binds any variables
let valid2 = opt ->
  match opt
    when Nothing | Just Nothing -> 0
    when Just (Just _) -> 1
  end

-- Test
valid1 (Just (Just 42)) + valid2 Nothing  -- 42 + 0 = 42
