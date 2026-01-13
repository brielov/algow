-- =============================================================================
-- Or-Pattern Examples
-- =============================================================================
-- Or-patterns allow matching multiple patterns with the same result.
-- All alternatives must bind the same variables with the same types.

type Color = Red | Green | Blue | RGB int int int

-- Simple or-pattern (no bindings)
let isPrimary = c ->
  match c
    when Red | Green | Blue -> true
    when RGB _ _ _ -> false
  end

-- Or-pattern with Maybe
let isEmptyOrNothing = opt ->
  match opt
    when Nothing | Just Nil -> true
    when _ -> false
  end

-- Or-pattern with nested constructors
let isSpecialValue = opt ->
  match opt
    when Nothing | Just Nothing -> true
    when Just (Just _) -> false
  end

-- Or-pattern with nested patterns and same variable binding
-- Extracts value from deeply nested Maybe structures
let extractValue = opt ->
  match opt
    when Just (Just x) -> x
    when Just Nothing -> 0
    when Nothing -> 0
  end

-- Or-pattern with literals
let isZeroOrOne = n ->
  match n
    when 0 | 1 -> true
    when _ -> false
  end

-- Complex or-pattern with lists
let startsWithZeroOrOne = xs ->
  match xs
    when Cons 0 _ | Cons 1 _ -> true
    when _ -> false
  end

-- Test results
let result =
  (if isPrimary Red then 1 else 0) +              -- 1
  (if isPrimary (RGB 1 2 3) then 1 else 0) +      -- 0
  (if isEmptyOrNothing Nothing then 1 else 0) +   -- 1
  (if isEmptyOrNothing (Just Nil) then 1 else 0) + -- 1
  (if isEmptyOrNothing (Just [1]) then 1 else 0) + -- 0
  (if isSpecialValue Nothing then 1 else 0) +     -- 1
  (if isSpecialValue (Just Nothing) then 1 else 0) + -- 1
  extractValue (Just (Just 42)) +                 -- 42
  extractValue (Just Nothing) +                   -- 0
  extractValue Nothing +                          -- 0
  (if isZeroOrOne 0 then 1 else 0) +              -- 1
  (if isZeroOrOne 1 then 1 else 0) +              -- 1
  (if isZeroOrOne 5 then 1 else 0) +              -- 0
  (if startsWithZeroOrOne [0, 2, 3] then 1 else 0) + -- 1
  (if startsWithZeroOrOne [1, 2, 3] then 1 else 0) + -- 1
  (if startsWithZeroOrOne [5, 2, 3] then 1 else 0)   -- 0
  -- Total: 1 + 0 + 1 + 1 + 0 + 1 + 1 + 42 + 0 + 0 + 1 + 1 + 0 + 1 + 1 + 0 = 51

let main = args -> IO.printLine (Int.toString result)
