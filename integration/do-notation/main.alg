-- =============================================================================
-- Do-Notation Examples
-- =============================================================================
-- Demonstrates do-notation for monadic composition

-- Bring Maybe's flatMap into scope for do-notation
let flatMap = Maybe.flatMap
let pure = x -> Just x

-- Basic do-notation: sequence Maybe computations
let testBasic =
  let result = do
    x <- Just 1
    y <- Just 2
    z <- Just 3
    pure (x + y + z)
  end in
  match result
    when Just n -> n
    when Nothing -> 0
  end

-- Short-circuit on Nothing
let testShortCircuit =
  let result = do
    x <- Just 10
    y <- Nothing
    z <- Just 20
    pure (x + y + z)
  end in
  match result
    when Just _ -> 0
    when Nothing -> 1
  end

-- Pattern matching in bind
let testPatternBind =
  let result = do
    (a, b) <- Just (1, 2)
    (c, d) <- Just (3, 4)
    pure (a + b + c + d)
  end in
  match result
    when Just n -> n
    when Nothing -> 0
  end

-- Let bindings inside do
let testDoLet =
  let result = do
    x <- Just 5
    let doubled = x * 2
    y <- Just doubled
    pure (x + y)
  end in
  match result
    when Just n -> n
    when Nothing -> 0
  end

-- Nested do blocks
let testNested =
  let inner = x -> do
    y <- Just (x * 2)
    pure (y + 1)
  end in
  let result = do
    a <- Just 5
    b <- inner a
    pure b
  end in
  match result
    when Just n -> n
    when Nothing -> 0
  end

-- Chaining multiple operations
let safeDivide = x y -> if y == 0 then Nothing else Just (Int.div x y)

let testChain =
  let result = do
    a <- safeDivide 100 2
    b <- safeDivide a 5
    c <- safeDivide b 2
    pure c
  end in
  match result
    when Just n -> n
    when Nothing -> 0
  end

-- Division by zero short-circuits
let testDivByZero =
  let result = do
    a <- safeDivide 100 2
    b <- safeDivide a 0
    c <- safeDivide b 2
    pure c
  end in
  match result
    when Just _ -> 0
    when Nothing -> 1
  end

-- List lookup helper
let rec listNth = n xs ->
  match xs
    when Nil -> Nothing
    when Cons x rest -> if n == 0 then Just x else listNth (n - 1) rest
  end

let testListLookup =
  let items = [10, 20, 30, 40, 50] in
  let result = do
    first <- listNth 0 items
    third <- listNth 2 items
    last <- listNth 4 items
    pure (first + third + last)
  end in
  match result
    when Just n -> n
    when Nothing -> 0
  end

let total = testBasic + testShortCircuit + testPatternBind + testDoLet + testNested + testChain + testDivByZero + testListLookup

let main = args ->
  let a = IO.printLine (String.concat "Basic: " (Int.toString testBasic)) in
  let b = IO.printLine (String.concat "Short-circuit: " (Int.toString testShortCircuit)) in
  let c = IO.printLine (String.concat "Pattern bind: " (Int.toString testPatternBind)) in
  let d = IO.printLine (String.concat "Do let: " (Int.toString testDoLet)) in
  let e = IO.printLine (String.concat "Nested: " (Int.toString testNested)) in
  let f = IO.printLine (String.concat "Chain: " (Int.toString testChain)) in
  let g = IO.printLine (String.concat "Div by zero: " (Int.toString testDivByZero)) in
  let h = IO.printLine (String.concat "List lookup: " (Int.toString testListLookup)) in
  let i = IO.printLine (String.concat "Total: " (Int.toString total)) in
  IO.printLine "Do-notation tests passed!"
