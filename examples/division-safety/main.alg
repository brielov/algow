-- =============================================================================
-- Division Safety Examples
-- =============================================================================
-- The compiler detects division by literal zero at compile time.

-- Safe division examples
let a = Int.div 10 2   -- 5
let b = Int.div 100 10 -- 10
let c = Int.div 7 3    -- 2 (integer division, truncated)
let d = Int.div 15 4   -- 3 (integer division, truncated)

-- Safe division with variables (runtime check needed)
let safeDivide = x y ->
  if y == 0
    then Nothing
    else Just (Int.div x y)

-- Using safe division
let result1 = match safeDivide 10 2
  when Just x -> x
  when Nothing -> 0
end  -- 5

let result2 = match safeDivide 10 0
  when Just x -> x
  when Nothing -> -1
end  -- -1 (avoided division by zero)

-- Division in complex expressions
let average = xs ->
  let sum = List.foldl (acc x -> acc + x) 0 xs in
  let len = List.length xs in
  if len == 0
    then Nothing
    else Just (Int.div sum len)

let avg1 = match average [10, 20, 30]
  when Just x -> x
  when Nothing -> 0
end  -- 20

let avg2 = match average Nil
  when Just x -> x
  when Nothing -> 0
end  -- 0 (empty list handled safely)

-- Test results
let result =
  a + b + c + d +           -- 5 + 10 + 2 + 3 = 20
  result1 + result2 +       -- 5 + (-1) = 4
  avg1 + avg2               -- 20 + 0 = 20
  -- Total: 20 + 4 + 20 = 44

result
