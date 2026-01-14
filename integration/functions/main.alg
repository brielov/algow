-- Functions: Currying, Higher-Order Functions, Composition, Pipes
let add =
  x y -> x + y

let add3 =
  x y z -> x + y + z

let add4 =
  a b c d -> a + b + c + d

let apply =
  f x -> f x

let double =
  x -> x * 2

let increment =
  x -> x + 1

let square =
  x -> x * x

let applyTwice =
  f x -> f (f x)

let rec applyN = n f x -> if n <= 0
  then x
  else applyN (n - 1) f (f x)

let makeAdder =
  n -> x -> n + x

let compose =
  f g x -> f (g x)

let compose3 =
  f g h x -> f (g (h x))

let id_ =
  x -> x

let const_ =
  x y -> x

let flip_ =
  f a b -> f b a

let subtract =
  a b -> a - b

let divide =
  a b -> Int.div a b

let sum =
  xs -> List.foldl (acc x -> acc + x) 0 xs

let product =
  xs -> List.foldl (acc x -> acc * x) 1 xs

let isEven =
  x -> Int.mod x 2 == 0

let rec fix = f x -> f (fix f) x

let factStep =
  self n -> if n <= 1
    then 1
    else n * self (n - 1)

let fibStep =
  self n -> if n <= 1
    then n
    else self (n - 1) + self (n - 2)

let rec sumTo = n -> if n <= 0
  then 0
  else n + sumTo (n - 1)

let rec sumToAcc = acc n -> if n <= 0
  then acc
  else sumToAcc (acc + n) (n - 1)

let makeCounter =
  start -> let inc = x -> x + 1 in
  inc start

let outer =
  x -> let middle = y -> let inner = z -> x + y + z in
  inner in
  middle

let makeMultiplier =
  factor -> x -> x * factor

let makeProcessor =
  default_ -> opt ->
    match opt
      when Just x -> x + default_
      when Nothing -> default_
    end

let tests =
  let add5 = add 5 in
  let times3 = makeMultiplier 3 in
  let times5 = makeMultiplier 5 in
  let proc = makeProcessor 100 in
  let nums = [1, 2, 3, 4, 5] in
  [ Test.equal "add5 10" 15 (add5 10)
    , Test.equal "add5 20" 25 (add5 20)
    , Test.equal "add 3 4" 7 (add 3 4)
    , Test.equal "add3 1 2 3" 6 (add3 1 2 3)
    , Test.equal "add3 10 20 30" 60 (add3 10 20 30)
    , Test.equal "add4" 10 (add4 1 2 3 4)
    , Test.equal "apply double" 10 (apply double 5)
    , Test.equal "apply increment" 6 (apply increment 5)
    , Test.equal "applyTwice double" 12 (applyTwice double 3)
    , Test.equal "applyTwice increment" 2 (applyTwice increment 0)
    , Test.equal "applyN 5 double" 32 (applyN 5 double 1)
    , Test.equal "applyN 10 increment" 10 (applyN 10 increment 0)
    , Test.equal "makeAdder 10" 15 (makeAdder 10 5)
    , Test.equal "makeAdder 100" 123 (makeAdder 100 23)
    , Test.equal "inc . double" 11 (compose increment double 5)
    , Test.equal "double . inc" 12 (compose double increment 5)
    , Test.equal "double . square" 18 (compose double square 3)
    , Test.equal "square . double" 36 (compose square double 3)
    , Test.equal "compose3" 20 (compose3 double increment square 3)
    , Test.equal "pipeline" 19 (compose3 (x -> x + 1) (x -> x * 2) (x -> x - 1) 10)
    , Test.equal "id 42" 42 (id_ 42)
    , Test.equal "id id 10" 10 (id_ (id_ 10))
    , Test.equal "const 5 10" 5 (const_ 5 10)
    , Test.equal "always42" 42 (const_ 42 999)
    , Test.equal "10 - 3" 7 (subtract 10 3)
    , Test.equal "3 - 10" (0 - 7) (flip_ subtract 10 3)
    , Test.equal "10 / 2" 5 (divide 10 2)
    , Test.equal "2 / 10" 0 (flip_ divide 10 2)
    , Test.equal "5 |> double" 10 (5 |> double)
    , Test.equal "5 |> inc" 6 (5 |> increment)
    , Test.equal "5 |> double |> inc" 11 (5 |> double |> increment)
    , Test.equal "5 |> inc |> double" 12 (5 |> increment |> double)
    , Test.equal "3x double" 24 (3 |> double |> double |> double)
    , Test.equal "2x square" 16 (2 |> square |> square)
    , Test.equal "10 |> add 5" 15 (10 |> add 5)
    , Test.equal "pipe chain" 30 (10 |> add 5 |> double)
    , Test.equal "complex pipe" 49 (1 |> add 1 |> double |> add 3 |> square)
    , Test.equal "sum" 15 (sum nums)
    , Test.equal "product" 120 (product nums)
    , Test.equal "map sum" 30 (sum (List.map (x -> x * 2) nums))
    , Test.equal "filter sum" 6 (sum (List.filter isEven nums))
    , Test.equal "pipe list" 50 (nums |> List.filter (x -> x > 2) |> List.map (x -> x * x) |> sum)
    , Test.equal "fix factorial 5" 120 (fix factStep 5)
    , Test.equal "fix fib 10" 55 (fix fibStep 10)
    , Test.equal "sumTo 10" 55 (sumTo 10)
    , Test.equal "sumToAcc 10" 55 (sumToAcc 0 10)
    , Test.equal "makeCounter 10" 11 (makeCounter 10)
    , Test.equal "nested closures" 6 (outer 1 2 3)
    , Test.equal "times3 10" 30 (times3 10)
    , Test.equal "times5 10" 50 (times5 10)
    , Test.equal "proc Just" 150 (proc (Just 50))
    , Test.equal "proc Nothing" 100 (proc Nothing)
    , Test.equal "inline lambda" 11 ((x -> x + 1) 10)
    , Test.equal "multi-param lambda" 8 ((x y -> x + y) 5 3)
    , Test.equal "nested lambdas" 6 ((x -> y -> z -> x + y + z) 1 2 3)
    , Test.equal "lambda in let body" 20 ((x -> x * 2) 10)
    , Test.equal "lambda match Just" 42 ((opt ->
      match opt
        when Just x -> x
        when Nothing -> 0
      end) (Just 42))
    , Test.equal "lambda match Nothing" 0 ((opt ->
      match opt
        when Just x -> x
        when Nothing -> 0
      end) Nothing)
    , Test.equal "lambda in foldl" 14 (List.foldl (acc x -> acc + x * x) 0 [1, 2, 3])
    ]

let main =
  args -> Test.run tests
