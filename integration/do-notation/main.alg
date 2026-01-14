-- Do-Notation Examples
let safeDivide =
  x y -> if y == 0
    then Nothing
    else Just (Int.div x y)

let rec listNth = n xs ->
  match xs
    when Nil -> Nothing
    when Cons x rest -> if n == 0
      then Just x
      else listNth (n - 1) rest
  end

let tests =
  [ Test.equal "basic do" (Just 6) (do[Maybe]
      x <- Just 1
      y <- Just 2
      z <- Just 3
      Just (x + y + z)
    end)
    , Test.equal "short-circuit" Nothing (do[Maybe]
      x <- Just 10
      y <- Nothing
      z <- Just 20
      Just (x + y + z)
    end)
    , Test.equal "pattern bind" (Just 10) (do[Maybe]
      (a, b) <- Just (1, 2)
      (c, d) <- Just (3, 4)
      Just (a + b + c + d)
    end)
    , Test.equal "do let" (Just 15) (do[Maybe]
      x <- Just 5
      let doubled = x * 2
      y <- Just doubled
      Just (x + y)
    end)
    , Test.equal "nested" (Just 11) (let inner = x ->
      do[Maybe]
        y <- Just (x * 2)
        Just (y + 1)
      end in
    do[Maybe]
      a <- Just 5
      b <- inner a
      Just b
    end)
    , Test.equal "chain" (Just 5) (do[Maybe]
      a <- safeDivide 100 2
      b <- safeDivide a 5
      c <- safeDivide b 2
      Just c
    end)
    , Test.equal "div by zero" Nothing (do[Maybe]
      a <- safeDivide 100 2
      b <- safeDivide a 0
      c <- safeDivide b 2
      Just c
    end)
    , Test.equal "list lookup" (Just 90) (let items = [10, 20, 30, 40, 50] in
    do[Maybe]
      first <- listNth 0 items
      third <- listNth 2 items
      last <- listNth 4 items
      Just (first + third + last)
    end)
    ]

let main =
  args -> Test.run tests
