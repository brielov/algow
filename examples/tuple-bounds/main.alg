-- =============================================================================
-- Tuple Index Bounds Checking Examples
-- =============================================================================
-- The compiler checks tuple index access at compile time.

-- Tuple indexing uses zero-based indices
let pair = (10, "hello")
let first = pair.0   -- 10 (valid: index 0 of 2-tuple)
let second = pair.1  -- "hello" (valid: index 1 of 2-tuple)

let triple = (1, 2, 3)
let a = triple.0  -- 1 (valid)
let b = triple.1  -- 2 (valid)
let c = triple.2  -- 3 (valid)

-- Nested tuple access
let nested = ((1, 2), (3, 4))
let innerFirst = nested.0   -- (1, 2)
let innerSecond = nested.1  -- (3, 4)
let deepValue = innerFirst.1  -- 2 (from (1, 2))

-- Mixed tuple with different types
let mixed = (42, "answer", true)
let num = mixed.0      -- 42
let str = mixed.1      -- "answer"
let flag = mixed.2     -- true

-- Polymorphic tuple swap function
-- Works thanks to deferred tuple constraint resolution
let swap = p ->
  let x = p.0 in
  let y = p.1 in
  (y, x)

let swapped = swap (1, 2)
let newFirst = swapped.0   -- 2
let newSecond = swapped.1  -- 1

-- Test results
let result =
  first +                           -- 10
  String.length second +            -- 5
  a + b + c +                       -- 1 + 2 + 3 = 6
  deepValue +                       -- 2
  num +                             -- 42
  String.length str +               -- 6
  (if flag then 1 else 0) +         -- 1
  newFirst + newSecond              -- 2 + 1 = 3
  -- Total: 10 + 5 + 6 + 2 + 42 + 6 + 1 + 3 = 75

let main = args -> IO.printLine (Int.toString result)
