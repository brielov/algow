-- Do-Notation Examples

let testBasic =
  let result = do[Maybe]
    x <- Just 1
    y <- Just 2
    z <- Just 3
    Just (x + y + z)
  end in
  Assert.equal (Just 6) result "basic do"

let testShortCircuit =
  let result = do[Maybe]
    x <- Just 10
    y <- Nothing
    z <- Just 20
    Just (x + y + z)
  end in
  Assert.equal Nothing result "short-circuit"

let testPatternBind =
  let result = do[Maybe]
    (a, b) <- Just (1, 2)
    (c, d) <- Just (3, 4)
    Just (a + b + c + d)
  end in
  Assert.equal (Just 10) result "pattern bind"

let testDoLet =
  let result = do[Maybe]
    x <- Just 5
    let doubled = x * 2
    y <- Just doubled
    Just (x + y)
  end in
  Assert.equal (Just 15) result "do let"

let testNested =
  let inner = x -> do[Maybe]
    y <- Just (x * 2)
    Just (y + 1)
  end in
  let result = do[Maybe]
    a <- Just 5
    b <- inner a
    Just b
  end in
  Assert.equal (Just 11) result "nested"

let safeDivide = x y -> if y == 0 then Nothing else Just (Int.div x y)

let testChain =
  let result = do[Maybe]
    a <- safeDivide 100 2
    b <- safeDivide a 5
    c <- safeDivide b 2
    Just c
  end in
  Assert.equal (Just 5) result "chain"

let testDivByZero =
  let result = do[Maybe]
    a <- safeDivide 100 2
    b <- safeDivide a 0
    c <- safeDivide b 2
    Just c
  end in
  Assert.equal Nothing result "div by zero"

let rec listNth = n xs -> match xs
  when Nil -> Nothing
  when Cons x rest -> if n == 0 then Just x else listNth (n - 1) rest
end

let testListLookup =
  let items = [10, 20, 30, 40, 50] in
  let result = do[Maybe]
    first <- listNth 0 items
    third <- listNth 2 items
    last <- listNth 4 items
    Just (first + third + last)
  end in
  Assert.equal (Just 90) result "list lookup"

let main = args ->
  let a = testBasic in
  let b = testShortCircuit in
  let c = testPatternBind in
  let d = testDoLet in
  let e = testNested in
  let f = testChain in
  let g = testDivByZero in
  let h = testListLookup in
  IO.printLine "do-notation: ok"
