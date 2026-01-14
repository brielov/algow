-- Closures: Capture, Nested, Higher-Order, State

-- Basic capture at top level
let x_ = 10
let addX = y -> x_ + y
let mulX = y -> x_ * y

-- Multiple captures
let a_ = 1
let b_ = 2
let c_ = 3
let sumABC = d -> a_ + b_ + c_ + d

-- Nested closures - single line style
let outer = x -> (y -> (z -> x + y + z))

-- Closure factory
let makeAdder = n -> (x -> x + n)

-- Closure over function
let f_ = x -> x * 2
let applyF = y -> f_ y

-- Multiplier closure
let multiplier = n -> (x -> x * n)

-- Closure returning closure
let compose = f g -> (x -> f (g x))

-- Counter using recursion
let rec countdown = n acc ->
  if n <= 0 then acc
  else countdown (n - 1) (n :: acc)

-- Closure over Maybe
let defaultVal_ = 42
let getOrDefault = opt -> match opt when Nothing -> defaultVal_ when Just x -> x end

-- Closure over record
let config_ = { multiplier = 10, offset = 5 }
let transform = x -> x * config_.multiplier + config_.offset

-- Closure in recursion
let rec sumWithBase = base xs ->
  match xs
    when Nil -> base
    when Cons x rest -> x + sumWithBase base rest
  end

-- Multiple levels of closure - single line
let fn1 = x -> (y -> (z -> 1 + 2 + 3 + x + y + z))

-- Closure capturing tuple
let point_ = (10, 20)
let addToPoint = p -> match p when (dx, dy) -> (point_.0 + dx, point_.1 + dy) end

-- Shadowing in closures
let shadowTest =
  let shadow = 50 in
  let inner = x -> x + shadow in
  inner 10

let tests =
  let add5 = makeAdder 5 in
  let add10 = makeAdder 10 in
  let double = multiplier 2 in
  let triple = multiplier 3 in
  let xs = [1, 2, 3, 4, 5] in
  let sumWithXs = y -> y + List.foldl (acc x -> acc + x) 0 xs in
  let offset = 100 in
  let addOffset = List.map (x -> x + offset) [1, 2, 3] in
  let threshold = 3 in
  let aboveThreshold = List.filter (x -> x > threshold) [1, 2, 3, 4, 5] in
  let factor = 2 in
  let scaledSum = List.foldl (acc x -> acc + x * factor) 0 [1, 2, 3] in
  let addOne = x -> x + 1 in
  let timesTwo = x -> x * 2 in
  let addOneThenDouble = compose timesTwo addOne in
  let doubleThenAddOne = compose addOne timesTwo in
  let sumWith100 = sumWithBase 100 in
  let scale = 10 in
  let pipeResult = [1, 2, 3] |> List.map (x -> x * scale) |> List.foldl (acc x -> acc + x) 0 in
  [ Test.equal "basic capture addX" 15 (addX 5)
  , Test.equal "basic capture mulX" 50 (mulX 5)
  , Test.equal "multiple captures" 10 (sumABC 4)
  , Test.equal "nested closures" 60 (outer 10 20 30)
  , Test.equal "makeAdder 5" 12 (add5 7)
  , Test.equal "makeAdder 10" 17 (add10 7)
  , Test.equal "add5 and add10 independent" 27 (add5 10 + add10 2)
  , Test.equal "closure over function" 20 (applyF 10)
  , Test.equal "multiplier double" 10 (double 5)
  , Test.equal "multiplier triple" 15 (triple 5)
  , Test.equal "closure over list" 25 (sumWithXs 10)
  , Test.equal "closure in map" [101, 102, 103] addOffset
  , Test.equal "closure in filter" [4, 5] aboveThreshold
  , Test.equal "closure in fold" 12 scaledSum
  , Test.equal "compose addOne then double" 12 (addOneThenDouble 5)
  , Test.equal "compose double then addOne" 11 (doubleThenAddOne 5)
  , Test.equal "countdown" [1, 2, 3, 4, 5] (countdown 5 [])
  , Test.equal "closure over Maybe Nothing" 42 (getOrDefault Nothing)
  , Test.equal "closure over Maybe Just" 10 (getOrDefault (Just 10))
  , Test.equal "closure over record" 55 (transform 5)
  , Test.equal "closure in recursion" 106 (sumWith100 [1, 2, 3])
  , Test.equal "multiple levels" 111 (fn1 10 20 30)
  , Test.equal "closure over tuple" (15, 25) (addToPoint (5, 5))
  , Test.equal "closure in pipeline" 60 pipeResult
  , Test.equal "shadowing in closures" 60 shadowTest
    -- Closure independence
  , Test.equal "closures are independent" 30 (
      let make = n -> (x -> x + n) in
      let f1 = make 10 in
      let f2 = make 20 in
      f1 5 + f2 (-5)
    )
    -- Closure captures value
  , Test.equal "capture by value" 10 (
      let val = 10 in
      let captured = x -> x + val in
      captured 0
    )
    -- Complex closure chain
  , Test.equal "closure chain" 42 (
      let step1 = x -> x + 1 in
      let step2 = x -> step1 x * 2 in
      let step3 = x -> step2 x + step1 x in
      step3 10
    )
    -- Closure in list operations
  , Test.equal "closure all operations" 45 (
      let base = 5 in
      [1, 2, 3]
        |> List.map (x -> x + base)
        |> List.map (x -> x * 2)
        |> List.foldl (acc x -> acc + x) 0
    )
    -- Recursive with closure
  , Test.equal "recursive with closure" 55 (
      let target = 10 in
      let rec sumTo = n -> if n <= 0 then 0 else n + sumTo (n - 1) in
      sumTo target
    )
  ]

let main = args -> Test.run tests
