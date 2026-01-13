-- =============================================================================
-- Functions: Currying, Higher-Order Functions, Composition, Pipes
-- =============================================================================

-- =============================================================================
-- Basic Curried Functions
-- =============================================================================

let testCurrying =
  let add = x y -> x + y in
  let add5 = add 5 in
  let a = add5 10 in                         -- 15
  let b = add5 20 in                         -- 25
  let c = add 3 4 in                         -- 7
  -- Multi-param currying
  let add3 = x y z -> x + y + z in
  let partial1 = add3 1 in
  let partial2 = partial1 2 in
  let d = partial2 3 in                      -- 6
  let e = add3 10 20 30 in                   -- 60
  -- Deeper currying
  let add4 = a b c d -> a + b + c + d in
  let f = add4 1 2 3 4 in                    -- 10
  a + b + c + d + e + f                      -- 123

-- =============================================================================
-- Higher-Order Functions
-- =============================================================================

let testHigherOrder =
  let apply = f x -> f x in
  let double = x -> x * 2 in
  let increment = x -> x + 1 in
  let a = apply double 5 in                  -- 10
  let b = apply increment 5 in               -- 6

  -- Apply twice
  let applyTwice = f x -> f (f x) in
  let c = applyTwice double 3 in             -- 12
  let d = applyTwice increment 0 in          -- 2

  -- Apply n times
  let rec applyN = n f x ->
    if n <= 0 then x else applyN (n - 1) f (f x)
  in
  let e = applyN 5 double 1 in               -- 32
  let f = applyN 10 increment 0 in           -- 10

  -- Functions returning functions
  let makeAdder = n -> (x -> n + x) in
  let add10 = makeAdder 10 in
  let g = add10 5 in                         -- 15
  let add100 = makeAdder 100 in
  let h = add100 23 in                       -- 123

  a + b + c + d + e + f + g + h              -- 210

-- =============================================================================
-- Function Composition
-- =============================================================================

let testComposition =
  let compose = f g x -> f (g x) in
  let double = x -> x * 2 in
  let increment = x -> x + 1 in
  let square = x -> x * x in

  -- Two-function composition
  let doubleIncrement = compose increment double in
  let incrementDouble = compose double increment in
  let a = doubleIncrement 5 in               -- 11 (5*2 + 1)
  let b = incrementDouble 5 in               -- 12 ((5+1)*2)

  -- Compose with square
  let squareDouble = compose double square in
  let doubleSquare = compose square double in
  let c = squareDouble 3 in                  -- 18 (3*3 * 2)
  let d = doubleSquare 3 in                  -- 36 ((3*2)^2)

  -- Three-function composition
  let compose3 = f g h x -> f (g (h x)) in
  let process = compose3 double increment square in
  let e = process 3 in                       -- 20 ((3*3 + 1) * 2)

  -- Chain of operations
  let pipeline = compose3 (x -> x + 1) (x -> x * 2) (x -> x - 1) in
  let f = pipeline 10 in                     -- 19 (((10-1)*2)+1)

  a + b + c + d + e + f                      -- 116

-- =============================================================================
-- Flip, Const, Identity
-- =============================================================================

let testFlipConstId =
  -- Identity
  let id_ = x -> x in
  let a = id_ 42 in                          -- 42
  let b = id_ (id_ 10) in                    -- 10

  -- Const
  let const_ = x y -> x in
  let c = const_ 5 10 in                     -- 5
  let always42 = const_ 42 in
  let d = always42 "ignored" in              -- 42
  let e = always42 999 in                    -- 42

  -- Flip
  let flip_ = f a b -> f b a in
  let subtract = a b -> a - b in
  let subtractFrom = flip_ subtract in
  let f = subtract 10 3 in                   -- 7
  let g = subtractFrom 10 3 in               -- -7 (3 - 10)

  -- Flip with division
  let divide = a b -> Int.div a b in
  let divideInto = flip_ divide in
  let h = divide 10 2 in                     -- 5
  let i = divideInto 10 2 in                 -- 0 (2 / 10)

  a + b + c + d + e + f + g + h + i          -- 146

-- =============================================================================
-- Pipe Operator
-- =============================================================================

let testPipe =
  -- Basic piping
  let double = x -> x * 2 in
  let increment = x -> x + 1 in
  let square = x -> x * x in

  let a = 5 |> double in                     -- 10
  let b = 5 |> increment in                  -- 6
  let c = 5 |> double |> increment in        -- 11
  let d = 5 |> increment |> double in        -- 12

  -- Longer pipelines
  let e = 3 |> double |> double |> double in -- 24
  let f = 2 |> square |> square in           -- 16
  let g = 3 |> increment |> square |> double in  -- 32

  -- Pipeline with partial application
  let add = x y -> x + y in
  let h = 10 |> add 5 in                     -- 15
  let i = 10 |> add 5 |> double in           -- 30

  -- Complex pipeline
  let j = 1 |> add 1 |> double |> add 3 |> square in  -- 49

  a + b + c + d + e + f + g + h + i + j      -- 205

-- =============================================================================
-- Functions on Lists
-- =============================================================================

let testListFunctions =
  let nums = [1, 2, 3, 4, 5] in

  -- Sum using foldl
  let sum = xs -> List.foldl (acc x -> acc + x) 0 xs in
  let a = sum nums in                        -- 15

  -- Product using foldl
  let product = xs -> List.foldl (acc x -> acc * x) 1 xs in
  let b = product nums in                    -- 120

  -- Map with partial application
  let addN = n -> (x -> x + n) in
  let doubled = List.map (addN 0 |> (f -> x -> f x * 2)) nums in
  let c = sum (List.map (x -> x * 2) nums) in  -- 30

  -- Filter with predicate
  let isEven = x -> Int.mod x 2 == 0 in
  let evens = List.filter isEven nums in
  let d = sum evens in                       -- 6

  -- Combined operations
  let e = nums
    |> List.filter (x -> x > 2)
    |> List.map (x -> x * x)
    |> sum
  in                                         -- 50

  a + b + c + d + e                          -- 221

-- =============================================================================
-- Recursive Higher-Order Functions
-- =============================================================================

let testRecursiveHOF =
  -- Fix point combinator (Y combinator style)
  let rec fix = f x -> f (fix f) x in

  -- Factorial using fix
  let factStep = self n ->
    if n <= 1 then 1 else n * self (n - 1)
  in
  let a = fix factStep 5 in                  -- 120

  -- Fibonacci using fix
  let fibStep = self n ->
    if n <= 1 then n else self (n - 1) + self (n - 2)
  in
  let b = fix fibStep 10 in                  -- 55

  -- Simple recursion
  let rec sumTo = n ->
    if n <= 0 then 0 else n + sumTo (n - 1)
  in
  let c = sumTo 10 in                        -- 55

  -- Recursion with accumulator
  let rec sumToAcc = acc n ->
    if n <= 0 then acc else sumToAcc (acc + n) (n - 1)
  in
  let d = sumToAcc 0 10 in                   -- 55

  a + b + c + d                              -- 285

-- =============================================================================
-- Closures
-- =============================================================================

let testClosures =
  -- Closure capturing local variable
  let makeCounter = start ->
    let inc = x -> x + 1 in
    let result = inc start in
    result
  in
  let a = makeCounter 10 in                  -- 11

  -- Nested closures
  let outer = x ->
    let middle = y ->
      let inner = z -> x + y + z in
      inner
    in
    middle
  in
  let b = outer 1 2 3 in                     -- 6

  -- Closure over function
  let makeMultiplier = factor ->
    (x -> x * factor)
  in
  let times3 = makeMultiplier 3 in
  let times5 = makeMultiplier 5 in
  let c = times3 10 in                       -- 30
  let d = times5 10 in                       -- 50

  -- Closure with pattern matching
  let makeProcessor = default_ ->
    (opt -> match opt
      when Just x -> x + default_
      when Nothing -> default_
    end)
  in
  let proc = makeProcessor 100 in
  let e = proc (Just 50) in                  -- 150
  let f = proc Nothing in                    -- 100

  a + b + c + d + e + f                      -- 347

-- =============================================================================
-- Lambda Syntax Variations
-- =============================================================================

let testLambdas =
  -- Single param
  let f1 = x -> x + 1 in
  let a = f1 10 in                           -- 11

  -- Multiple params
  let f2 = x y -> x + y in
  let b = f2 5 3 in                          -- 8

  -- Nested lambdas (equivalent to multi-param)
  let f3 = x -> y -> z -> x + y + z in
  let c = f3 1 2 3 in                        -- 6

  -- Lambda in let binding body
  let result = (x -> x * 2) 10 in
  let d = result in                          -- 20

  -- Lambda with match
  let f4 = opt -> match opt when Just x -> x when Nothing -> 0 end in
  let e = f4 (Just 42) in                    -- 42
  let f = f4 Nothing in                      -- 0

  -- Lambda in list operations
  let nums = [1, 2, 3] in
  let g = List.foldl (acc x -> acc + x * x) 0 nums in  -- 14

  a + b + c + d + e + f + g                  -- 101

-- =============================================================================
-- Final Result
-- =============================================================================

let result =
  testCurrying +                             -- 123
  testHigherOrder +                          -- 210
  testComposition +                          -- 116
  testFlipConstId +                          -- 146
  testPipe +                                 -- 205
  testListFunctions +                        -- 221
  testRecursiveHOF +                         -- 285
  testClosures +                             -- 347
  testLambdas                                -- 101
                                             -- Total: 1754

let main = args -> IO.printLine (Int.toString result)
