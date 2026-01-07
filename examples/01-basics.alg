-- Basic Examples
-- Demonstrates fundamental language features

-- =============================================================================
-- ARITHMETIC AND COMPARISONS
-- =============================================================================

let arithmetic = {
  add = 2 + 3,
  sub = 10 - 4,
  mul = 6 * 7,
  div = 15 / 2,
  nested = (2 + 3) * (4 - 1)
}

let comparisons = {
  lt = 1 < 2,
  gt = 5 > 3,
  lte = 3 <= 3,
  gte = 4 >= 4,
  eq = 5 == 5,
  neq = 3 != 4
}

-- =============================================================================
-- FUNCTIONS
-- =============================================================================

let double x = x * 2
let square x = x * x
let add x y = x + y

-- Function composition
let doubleSquare x = double (square x)

let functions = {
  doubled = double 5,
  squared = square 4,
  added = add 3 7,
  composed = doubleSquare 3
}

-- =============================================================================
-- CONDITIONALS
-- =============================================================================

let abs x = if x < 0 then 0 - x else x
let max a b = if a > b then a else b
let min a b = if a < b then a else b
let sign x = if x > 0 then 1 else if x < 0 then 0 - 1 else 0

let conditionals = {
  absNeg = abs (0 - 5),
  absPos = abs 5,
  maximum = max 3 7,
  minimum = min 3 7,
  signPos = sign 10,
  signNeg = sign (0 - 10),
  signZero = sign 0
}

-- =============================================================================
-- TUPLES AND RECORDS
-- =============================================================================

let point = (3, 4)
let fst pair = match pair when (a, b) -> a end
let snd pair = match pair when (a, b) -> b end

let person = { name = "Alice", age = 30 }

let tuples_records = {
  point = point,
  x = fst point,
  y = snd point,
  person = person,
  personName = person.name,
  personAge = person.age
}

-- =============================================================================
-- PATTERN MATCHING
-- =============================================================================

let describe n = match n
  when 0 -> "zero"
  when 1 -> "one"
  when 2 -> "two"
  when x -> "many"
end

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let patterns = {
  zero = describe 0,
  one = describe 1,
  many = describe 100,
  fact5 = factorial 5,
  fact10 = factorial 10
}

-- =============================================================================
-- MAYBE TYPE
-- =============================================================================

let safeDivide a b =
  if b == 0
  then Nothing
  else Just (a / b)

let maybeDouble m = match m
  when Nothing -> Nothing
  when Just x -> Just (x * 2)
end

let getOrDefault def m = match m
  when Nothing -> def
  when Just x -> x
end

let maybes = {
  safeDiv = safeDivide 10 2,
  safeDivZero = safeDivide 10 0,
  doubled = maybeDouble (Just 5),
  doubledNothing = maybeDouble Nothing,
  withDefault = getOrDefault 0 Nothing,
  withValue = getOrDefault 0 (Just 42)
}

-- =============================================================================
-- LISTS
-- =============================================================================

let nums = [1, 2, 3, 4, 5]

let rec sumList xs = match xs
  when Nil -> 0
  when Cons x rest -> x + sumList rest
end

let lists = {
  numbers = nums,
  sum = sumList nums,
  mapped = map (x -> x * 2) nums,
  filtered = filter (x -> x > 2) nums,
  len = length nums
}

-- =============================================================================
-- RESULT
-- =============================================================================

in {
  arithmetic = arithmetic,
  comparisons = comparisons,
  functions = functions,
  conditionals = conditionals,
  tuples_records = tuples_records,
  patterns = patterns,
  maybes = maybes,
  lists = lists
}
