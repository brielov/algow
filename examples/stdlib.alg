-- Standard Library Examples for Algow
-- Demonstrates pattern matching, recursion, higher-order functions,
-- algebraic data types, and the full expressiveness of the language.

-- =============================================================================
-- LIST UTILITIES
-- =============================================================================

-- Length of a list
let rec length xs = match xs
  when Nil -> 0
  when Cons _ rest -> 1 + length rest
end

-- Sum all numbers in a list
let rec sum xs = match xs
  when Nil -> 0
  when Cons x rest -> x + sum rest
end

-- Product of all numbers in a list
let rec product xs = match xs
  when Nil -> 1
  when Cons x rest -> x * product rest
end

-- Append two lists
let rec append xs ys = match xs
  when Nil -> ys
  when Cons x rest -> Cons x (append rest ys)
end

-- Reverse a list (tail-recursive)
let rec reverse xs =
  let rec go acc xs = match xs
    when Nil -> acc
    when Cons x rest -> go (Cons x acc) rest
  end
  in go Nil xs

-- Map a function over a list
let rec map f xs = match xs
  when Nil -> Nil
  when Cons x rest -> Cons (f x) (map f rest)
end

-- Filter elements that satisfy a predicate
let rec filter p xs = match xs
  when Nil -> Nil
  when Cons x rest ->
    if p x
    then Cons x (filter p rest)
    else filter p rest
end

-- Fold right
let rec foldr f z xs = match xs
  when Nil -> z
  when Cons x rest -> f x (foldr f z rest)
end

-- Fold left (tail recursive)
let rec foldl f z xs = match xs
  when Nil -> z
  when Cons x rest -> foldl f (f z x) rest
end

-- Take first n elements
let rec take n xs =
  if n <= 0
  then Nil
  else match xs
    when Nil -> Nil
    when Cons x rest -> Cons x (take (n - 1) rest)
  end

-- Drop first n elements
let rec drop n xs =
  if n <= 0
  then xs
  else match xs
    when Nil -> Nil
    when Cons _ rest -> drop (n - 1) rest
  end

-- Zip two lists into a list of tuples
let rec zip xs ys = match xs
  when Nil -> Nil
  when Cons x xrest -> match ys
    when Nil -> Nil
    when Cons y yrest -> Cons (x, y) (zip xrest yrest)
  end
end

-- Find first element satisfying predicate
let rec find p xs = match xs
  when Nil -> Nothing
  when Cons x rest ->
    if p x
    then Just x
    else find p rest
end

-- Check if any element satisfies predicate
let rec any p xs = match xs
  when Nil -> false
  when Cons x rest ->
    if p x
    then true
    else any p rest
end

-- Check if all elements satisfy predicate
let rec all p xs = match xs
  when Nil -> true
  when Cons x rest ->
    if p x
    then all p rest
    else false
end

-- Get element at index (0-based)
let rec nth n xs = match xs
  when Nil -> Nothing
  when Cons x rest ->
    if n == 0
    then Just x
    else nth (n - 1) rest
end

-- =============================================================================
-- SORTING
-- =============================================================================

-- Quicksort
let rec quicksort xs = match xs
  when Nil -> Nil
  when Cons pivot rest ->
    let smaller = filter (x -> x < pivot) rest in
    let larger = filter (x -> x >= pivot) rest in
    append (quicksort smaller) (Cons pivot (quicksort larger))
end

-- Insert into sorted list
let rec insert x xs = match xs
  when Nil -> Cons x Nil
  when Cons y rest ->
    if x <= y
    then Cons x xs
    else Cons y (insert x rest)
end

-- Insertion sort
let rec insertionSort xs = match xs
  when Nil -> Nil
  when Cons x rest -> insert x (insertionSort rest)
end

-- =============================================================================
-- MAYBE UTILITIES
-- =============================================================================

-- Map over Maybe
let mapMaybe f m = match m
  when Nothing -> Nothing
  when Just x -> Just (f x)
end

-- Get value or default
let fromMaybe def m = match m
  when Nothing -> def
  when Just x -> x
end

-- Check if Maybe has value
let isJust m = match m
  when Nothing -> false
  when Just _ -> true
end

-- =============================================================================
-- NUMERIC UTILITIES
-- =============================================================================

-- Absolute value
let abs x = if x < 0 then 0 - x else x

-- Maximum of two numbers
let max a b = if a > b then a else b

-- Minimum of two numbers
let min a b = if a < b then a else b

-- Factorial
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

-- Fibonacci (efficient with accumulator)
let rec fibFast n =
  let rec go a b n =
    if n == 0 then a
    else go b (a + b) (n - 1)
  in go 0 1 n

-- Power function
let rec pow base exp =
  if exp == 0 then 1
  else base * pow base (exp - 1)

-- Check if number is even (for non-negative integers)
let rec isEven n =
  if n == 0 then true
  else if n == 1 then false
  else isEven (n - 2)

-- =============================================================================
-- FUNCTION UTILITIES
-- =============================================================================

-- Identity function
let id x = x

-- Function composition
let compose f g x = f (g x)

-- Flip argument order
let flip f a b = f b a

-- =============================================================================
-- RANGE AND ENUMERATION
-- =============================================================================

-- Generate range [start, stop)
let rec range start stop =
  if start >= stop
  then Nil
  else Cons start (range (start + 1) stop)

-- Replicate value n times
let rec replicate n x =
  if n <= 0
  then Nil
  else Cons x (replicate (n - 1) x)

-- =============================================================================
-- DEMONSTRATION
-- =============================================================================

-- Sample data
let nums = [5, 2, 8, 1, 9, 3, 7, 4, 6]

-- Run all the functions and collect results
let result = {
  -- Basic stats
  original = nums,
  len = length nums,
  total = sum nums,
  prod = product nums,

  -- Transformations
  sorted = quicksort nums,
  sortedIns = insertionSort nums,
  reversed = reverse nums,
  doubled = map (x -> x * 2) nums,
  evens = filter isEven nums,

  -- Slicing
  first3 = take 3 nums,
  last3 = drop 6 nums,

  -- Generation
  oneToFive = range 1 6,
  squares = map (x -> x * x) (range 1 6),
  fives = replicate 5 5,

  -- Searching
  found = find (x -> x > 7) nums,
  at3 = nth 3 nums,

  -- Predicates
  hasLarge = any (x -> x > 100) nums,
  allPositive = all (x -> x > 0) nums,

  -- Numeric
  factorial10 = factorial 10,
  fib20 = fibFast 20,
  pow2_10 = pow 2 10,

  -- Zipping
  zipped = zip (range 1 4) (Cons "a" (Cons "b" (Cons "c" Nil)))
}

in result
