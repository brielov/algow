-- Recursion Examples
-- Demonstrates recursive functions and tail recursion

-- =============================================================================
-- BASIC RECURSION
-- =============================================================================

-- Factorial (simple recursion)
let rec fact n =
  if n <= 1 then 1
  else n * fact (n - 1)

-- Fibonacci (naive recursion - exponential time)
let rec fibSlow n =
  if n <= 1 then n
  else fibSlow (n - 1) + fibSlow (n - 2)

-- Factorial with accumulator (tail recursive)
let rec factTail n =
  let rec go acc n =
    if n <= 1 then acc
    else go (acc * n) (n - 1)
  in go 1 n

-- Fibonacci with accumulators (tail recursive, linear time)
let rec fibFast n =
  let rec go a b n =
    if n == 0 then a
    else go b (a + b) (n - 1)
  in go 0 1 n

-- Sum from 1 to n (tail recursive)
let rec sumTo n =
  let rec go acc n =
    if n <= 0 then acc
    else go (acc + n) (n - 1)
  in go 0 n

-- =============================================================================
-- LIST RECURSION
-- =============================================================================

-- Sum of list elements
let rec sumList xs = match xs
  when Nil -> 0
  when Cons x rest -> x + sumList rest
end

-- Product of list elements
let rec productList xs = match xs
  when Nil -> 1
  when Cons x rest -> x * productList rest
end

-- Count elements in list
let rec countList xs = match xs
  when Nil -> 0
  when Cons h rest -> 1 + countList rest
end

-- Append two lists
let rec appendList xs ys = match xs
  when Nil -> ys
  when Cons x rest -> Cons x (appendList rest ys)
end

-- Reverse list (tail recursive)
let rec reverseList xs =
  let rec go acc xs = match xs
    when Nil -> acc
    when Cons x rest -> go (Cons x acc) rest
  end
  in go Nil xs

-- =============================================================================
-- MUTUAL RECURSION
-- =============================================================================

-- Check if number is even/odd using mutual recursion
let rec isEvenMut n =
  if n == 0 then true
  else isOddMut (n - 1)
and isOddMut n =
  if n == 0 then false
  else isEvenMut (n - 1)

in

-- =============================================================================
-- RESULTS
-- =============================================================================

let testNums = [1, 2, 3, 4, 5]

in {
  -- Basic recursion
  fact5 = fact 5,
  fact10 = fact 10,
  fibSlow10 = fibSlow 10,

  -- Tail recursion
  factTail10 = factTail 10,
  fibFast20 = fibFast 20,
  fibFast30 = fibFast 30,
  sumTo100 = sumTo 100,

  -- List recursion
  sum = sumList testNums,
  product = productList testNums,
  count = countList testNums,
  appended = appendList [1, 2, 3] [4, 5, 6],
  reversed = reverseList testNums,

  -- Mutual recursion
  isEven10 = isEvenMut 10,
  isEven11 = isEvenMut 11,
  isOdd10 = isOddMut 10,
  isOdd11 = isOddMut 11
}
