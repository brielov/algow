-- =============================================================================
-- Exhaustiveness Checking Examples
-- =============================================================================
-- The compiler warns when pattern matches don't cover all possible cases.

type Color = Red | Green | Blue

-- GOOD: Exhaustive match - covers all constructors
let describeColor = c ->
  match c
    when Red -> "red"
    when Green -> "green"
    when Blue -> "blue"
  end

-- GOOD: Exhaustive with wildcard
let isRed = c ->
  match c
    when Red -> true
    when _ -> false
  end

-- GOOD: Exhaustive Maybe match
let unwrapOr = default opt ->
  match opt
    when Nothing -> default
    when Just x -> x
  end

-- GOOD: Exhaustive bool match
let boolToInt = b ->
  match b
    when true -> 1
    when false -> 0
  end

-- GOOD: Exhaustive List match
let safeHead = xs ->
  match xs
    when Nil -> Nothing
    when Cons x _ -> Just x
  end

-- GOOD: Exhaustive with as-pattern
let headAndTail = xs ->
  match xs
    when Nil -> Nothing
    when Cons x rest as whole -> Just (x, rest, whole)
  end

-- GOOD: Exhaustive tuple match with wildcard fallback
let classifyPair = pair ->
  match pair
    when (0, 0) -> "origin"
    when (0, _) -> "on y-axis"
    when (_, 0) -> "on x-axis"
    when _ -> "general point"
  end

-- Test the functions
let result =
  let c1 = String.length (describeColor Red) in      -- 3
  let c2 = String.length (describeColor Green) in    -- 5
  let c3 = String.length (describeColor Blue) in     -- 4
  let r1 = if isRed Red then 1 else 0 in             -- 1
  let r2 = if isRed Blue then 1 else 0 in            -- 0
  let u1 = unwrapOr 0 (Just 42) in                   -- 42
  let u2 = unwrapOr 99 Nothing in                    -- 99
  let b1 = boolToInt true in                         -- 1
  let b2 = boolToInt false in                        -- 0
  let h1 = unwrapOr 0 (safeHead [10, 20]) in         -- 10
  let h2 = unwrapOr 0 (safeHead Nil) in              -- 0
  let p1 = String.length (classifyPair (0, 0)) in    -- 6
  let p2 = String.length (classifyPair (5, 0)) in    -- 9
  c1 + c2 + c3 + r1 + r2 + u1 + u2 + b1 + b2 + h1 + h2 + p1 + p2
  -- Expected: 3 + 5 + 4 + 1 + 0 + 42 + 99 + 1 + 0 + 10 + 0 + 6 + 9 = 180

let main = args -> IO.printLine (Int.toString result)
