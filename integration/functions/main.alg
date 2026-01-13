-- Functions: Currying, Higher-Order Functions, Composition, Pipes

let testCurrying =
  let add = x y -> x + y in
  let add5 = add 5 in
  let a = Assert.equal 15 (add5 10) "add5 10" in
  let b = Assert.equal 25 (add5 20) "add5 20" in
  let c = Assert.equal 7 (add 3 4) "add 3 4" in
  let add3 = x y z -> x + y + z in
  let d = Assert.equal 6 (add3 1 2 3) "add3 1 2 3" in
  let e = Assert.equal 60 (add3 10 20 30) "add3 10 20 30" in
  let add4 = a b c d -> a + b + c + d in
  let f = Assert.equal 10 (add4 1 2 3 4) "add4" in
  ()

let testHigherOrder =
  let apply = f x -> f x in
  let double = x -> x * 2 in
  let increment = x -> x + 1 in
  let a = Assert.equal 10 (apply double 5) "apply double" in
  let b = Assert.equal 6 (apply increment 5) "apply increment" in
  let applyTwice = f x -> f (f x) in
  let c = Assert.equal 12 (applyTwice double 3) "applyTwice double" in
  let d = Assert.equal 2 (applyTwice increment 0) "applyTwice increment" in
  let rec applyN = n f x -> if n <= 0 then x else applyN (n - 1) f (f x) in
  let e = Assert.equal 32 (applyN 5 double 1) "applyN 5 double" in
  let f = Assert.equal 10 (applyN 10 increment 0) "applyN 10 increment" in
  let makeAdder = n -> (x -> n + x) in
  let add10 = makeAdder 10 in
  let g = Assert.equal 15 (add10 5) "makeAdder 10" in
  let h = Assert.equal 123 (makeAdder 100 23) "makeAdder 100" in
  ()

let testComposition =
  let compose = f g x -> f (g x) in
  let double = x -> x * 2 in
  let increment = x -> x + 1 in
  let square = x -> x * x in
  let a = Assert.equal 11 (compose increment double 5) "inc . double" in
  let b = Assert.equal 12 (compose double increment 5) "double . inc" in
  let c = Assert.equal 18 (compose double square 3) "double . square" in
  let d = Assert.equal 36 (compose square double 3) "square . double" in
  let compose3 = f g h x -> f (g (h x)) in
  let e = Assert.equal 20 (compose3 double increment square 3) "compose3" in
  let f = Assert.equal 19 (compose3 (x -> x + 1) (x -> x * 2) (x -> x - 1) 10) "pipeline" in
  ()

let testFlipConstId =
  let id_ = x -> x in
  let a = Assert.equal 42 (id_ 42) "id 42" in
  let b = Assert.equal 10 (id_ (id_ 10)) "id id 10" in
  let const_ = x y -> x in
  let c = Assert.equal 5 (const_ 5 10) "const 5 10" in
  let always42 = const_ 42 in
  let d = Assert.equal 42 (always42 999) "always42" in
  let flip_ = f a b -> f b a in
  let subtract = a b -> a - b in
  let e = Assert.equal 7 (subtract 10 3) "10 - 3" in
  let f = Assert.equal (-7) (flip_ subtract 10 3) "3 - 10" in
  let divide = a b -> Int.div a b in
  let g = Assert.equal 5 (divide 10 2) "10 / 2" in
  let h = Assert.equal 0 (flip_ divide 10 2) "2 / 10" in
  ()

let testPipe =
  let double = x -> x * 2 in
  let increment = x -> x + 1 in
  let square = x -> x * x in
  let a = Assert.equal 10 (5 |> double) "5 |> double" in
  let b = Assert.equal 6 (5 |> increment) "5 |> inc" in
  let c = Assert.equal 11 (5 |> double |> increment) "5 |> double |> inc" in
  let d = Assert.equal 12 (5 |> increment |> double) "5 |> inc |> double" in
  let e = Assert.equal 24 (3 |> double |> double |> double) "3x double" in
  let f = Assert.equal 16 (2 |> square |> square) "2x square" in
  let add = x y -> x + y in
  let g = Assert.equal 15 (10 |> add 5) "10 |> add 5" in
  let h = Assert.equal 30 (10 |> add 5 |> double) "pipe chain" in
  let i = Assert.equal 49 (1 |> add 1 |> double |> add 3 |> square) "complex pipe" in
  ()

let testListFunctions =
  let nums = [1, 2, 3, 4, 5] in
  let sum = xs -> List.foldl (acc x -> acc + x) 0 xs in
  let product = xs -> List.foldl (acc x -> acc * x) 1 xs in
  let a = Assert.equal 15 (sum nums) "sum" in
  let b = Assert.equal 120 (product nums) "product" in
  let c = Assert.equal 30 (sum (List.map (x -> x * 2) nums)) "map sum" in
  let isEven = x -> Int.mod x 2 == 0 in
  let d = Assert.equal 6 (sum (List.filter isEven nums)) "filter sum" in
  let e = Assert.equal 50 (nums |> List.filter (x -> x > 2) |> List.map (x -> x * x) |> sum) "pipe list" in
  ()

let testRecursiveHOF =
  let rec fix = f x -> f (fix f) x in
  let factStep = self n -> if n <= 1 then 1 else n * self (n - 1) in
  let a = Assert.equal 120 (fix factStep 5) "fix factorial 5" in
  let fibStep = self n -> if n <= 1 then n else self (n - 1) + self (n - 2) in
  let b = Assert.equal 55 (fix fibStep 10) "fix fib 10" in
  let rec sumTo = n -> if n <= 0 then 0 else n + sumTo (n - 1) in
  let c = Assert.equal 55 (sumTo 10) "sumTo 10" in
  let rec sumToAcc = acc n -> if n <= 0 then acc else sumToAcc (acc + n) (n - 1) in
  let d = Assert.equal 55 (sumToAcc 0 10) "sumToAcc 10" in
  ()

let testClosures =
  let makeCounter = start -> let inc = x -> x + 1 in inc start in
  let a = Assert.equal 11 (makeCounter 10) "makeCounter 10" in
  let outer = x -> let middle = y -> let inner = z -> x + y + z in inner in middle in
  let b = Assert.equal 6 (outer 1 2 3) "nested closures" in
  let makeMultiplier = factor -> (x -> x * factor) in
  let times3 = makeMultiplier 3 in
  let times5 = makeMultiplier 5 in
  let c = Assert.equal 30 (times3 10) "times3 10" in
  let d = Assert.equal 50 (times5 10) "times5 10" in
  let makeProcessor = default_ -> (opt -> match opt when Just x -> x + default_ when Nothing -> default_ end) in
  let proc = makeProcessor 100 in
  let e = Assert.equal 150 (proc (Just 50)) "proc Just" in
  let f = Assert.equal 100 (proc Nothing) "proc Nothing" in
  ()

let testLambdas =
  let a = Assert.equal 11 ((x -> x + 1) 10) "inline lambda" in
  let b = Assert.equal 8 ((x y -> x + y) 5 3) "multi-param lambda" in
  let c = Assert.equal 6 ((x -> y -> z -> x + y + z) 1 2 3) "nested lambdas" in
  let d = Assert.equal 20 ((x -> x * 2) 10) "lambda in let body" in
  let e = Assert.equal 42 ((opt -> match opt when Just x -> x when Nothing -> 0 end) (Just 42)) "lambda match Just" in
  let f = Assert.equal 0 ((opt -> match opt when Just x -> x when Nothing -> 0 end) Nothing) "lambda match Nothing" in
  let g = Assert.equal 14 (List.foldl (acc x -> acc + x * x) 0 [1, 2, 3]) "lambda in foldl" in
  ()

let main = args ->
  let a = testCurrying in
  let b = testHigherOrder in
  let c = testComposition in
  let d = testFlipConstId in
  let e = testPipe in
  let f = testListFunctions in
  let g = testRecursiveHOF in
  let h = testClosures in
  let i = testLambdas in
  IO.printLine "functions: ok"
