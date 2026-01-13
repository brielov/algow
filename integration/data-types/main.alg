-- Data Types: ADTs, Polymorphism, Recursive Types, Records, Tuples

type Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let testEnumTypes =
  let isWeekend = day -> match day when Saturday | Sunday -> true when _ -> false end in
  let weekdayNumber = day -> match day
    when Monday -> 1 when Tuesday -> 2 when Wednesday -> 3 when Thursday -> 4
    when Friday -> 5 when Saturday -> 6 when Sunday -> 7
  end in
  let a = Assert.equal 1 (weekdayNumber Monday) "Monday = 1" in
  let b = Assert.equal 5 (weekdayNumber Friday) "Friday = 5" in
  let c = Assert.equal 7 (weekdayNumber Sunday) "Sunday = 7" in
  let d = Assert.ok (isWeekend Saturday) "Saturday is weekend" in
  let e = Assert.ok (Bool.negate (isWeekend Monday)) "Monday not weekend" in
  ()

type Shape = Circle int | Rectangle int int | Triangle int int int

let testDataADTs =
  let area = shape -> match shape
    when Circle r -> r * r * 3
    when Rectangle w h -> w * h
    when Triangle a b c -> Int.div (a + b + c) 2
  end in
  let perimeter = shape -> match shape
    when Circle r -> 2 * 3 * r
    when Rectangle w h -> 2 * (w + h)
    when Triangle a b c -> a + b + c
  end in
  let a = Assert.equal 75 (area (Circle 5)) "Circle area" in
  let b = Assert.equal 24 (area (Rectangle 4 6)) "Rectangle area" in
  let c = Assert.equal 6 (area (Triangle 3 4 5)) "Triangle area" in
  let d = Assert.equal 42 (perimeter (Circle 7)) "Circle perimeter" in
  let e = Assert.equal 16 (perimeter (Rectangle 3 5)) "Rectangle perimeter" in
  let f = Assert.equal 12 (perimeter (Triangle 3 4 5)) "Triangle perimeter" in
  ()

type Box a = EmptyBox | FilledBox a

let testPolymorphicADTs =
  let unbox = default_ box -> match box when EmptyBox -> default_ when FilledBox x -> x end in
  let a = Assert.equal 42 (unbox 0 (FilledBox 42)) "unbox FilledBox" in
  let b = Assert.equal 0 (unbox 0 EmptyBox) "unbox EmptyBox" in
  let c = Assert.equal "hello" (unbox "" (FilledBox "hello")) "unbox string" in
  let d = Assert.equal "default" (unbox "default" EmptyBox) "unbox string default" in
  let doubleUnbox = outer -> match outer
    when EmptyBox -> 0
    when FilledBox inner -> match inner when EmptyBox -> -1 when FilledBox x -> x end
  end in
  let e = Assert.equal 100 (doubleUnbox (FilledBox (FilledBox 100))) "nested FilledBox" in
  let f = Assert.equal (-1) (doubleUnbox (FilledBox EmptyBox)) "nested EmptyBox" in
  let g = Assert.equal 0 (doubleUnbox EmptyBox) "outer EmptyBox" in
  ()

type Nat = Zero | Succ Nat

let testRecursiveTypes =
  let rec natToInt = n -> match n when Zero -> 0 when Succ m -> 1 + natToInt m end in
  let rec intToNat = n -> if n <= 0 then Zero else Succ (intToNat (n - 1)) in
  let rec addNat = a b -> match a when Zero -> b when Succ m -> Succ (addNat m b) end in
  let three = Succ (Succ (Succ Zero)) in
  let two = Succ (Succ Zero) in
  let a = Assert.equal 3 (natToInt three) "natToInt 3" in
  let b = Assert.equal 2 (natToInt two) "natToInt 2" in
  let c = Assert.equal 5 (natToInt (addNat three two)) "addNat 3 2" in
  let d = Assert.equal 7 (natToInt (intToNat 7)) "intToNat 7" in
  ()

type Tree a = Leaf | Node a (Tree a) (Tree a)

let testBinaryTrees =
  let rec size = tree -> match tree when Leaf -> 0 when Node _ left right -> 1 + size left + size right end in
  let rec depth = tree -> match tree
    when Leaf -> 0
    when Node _ left right -> 1 + (if depth left > depth right then depth left else depth right)
  end in
  let rec sumTree = tree -> match tree when Leaf -> 0 when Node x left right -> x + sumTree left + sumTree right end in
  let rec treeMap = f tree -> match tree
    when Leaf -> Leaf
    when Node x left right -> Node (f x) (treeMap f left) (treeMap f right)
  end in
  let tree = Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 Leaf (Node 10 Leaf Leaf)) in
  let a = Assert.equal 6 (size tree) "tree size" in
  let b = Assert.equal 3 (depth tree) "tree depth" in
  let c = Assert.equal 31 (sumTree tree) "tree sum" in
  let d = Assert.equal 62 (sumTree (treeMap (x -> x * 2) tree)) "doubled tree sum" in
  ()

let testMaybeType =
  let safeHead = xs -> match xs when Nil -> Nothing when Cons x _ -> Just x end in
  let maybeOr = default_ opt -> match opt when Nothing -> default_ when Just x -> x end in
  let a = Assert.equal (Just 10) (safeHead [10, 20, 30]) "safeHead list" in
  let b = Assert.equal Nothing (safeHead Nil) "safeHead Nil" in
  let c = Assert.equal 10 (maybeOr 0 (safeHead [10, 20, 30])) "maybeOr Just" in
  let d = Assert.equal 0 (maybeOr 0 (safeHead Nil)) "maybeOr Nothing" in
  ()

let testEitherType =
  let safeDivide = a b -> if b == 0 then Left "division by zero" else Right (Int.div a b) in
  let eitherOr = default_ either_ -> match either_ when Left _ -> default_ when Right x -> x end in
  let a = Assert.equal (Right 5) (safeDivide 10 2) "safeDivide ok" in
  let b = Assert.ok (Either.isLeft (safeDivide 10 0)) "safeDivide by zero" in
  let c = Assert.equal 5 (eitherOr 0 (safeDivide 10 2)) "eitherOr Right" in
  let d = Assert.equal (-1) (eitherOr (-1) (safeDivide 10 0)) "eitherOr Left" in
  ()

let testListType =
  let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil in
  let ys = [6, 7, 8, 9, 10] in
  let a = Assert.equal 5 (List.length xs) "xs length" in
  let b = Assert.equal 5 (List.length ys) "ys length" in
  let c = Assert.equal 15 (List.foldl (acc x -> acc + x) 0 xs) "xs sum" in
  let d = Assert.equal 40 (List.foldl (acc x -> acc + x) 0 ys) "ys sum" in
  let e = Assert.equal 30 (List.foldl (acc x -> acc + x) 0 (List.map (x -> x * 2) xs)) "doubled sum" in
  let f = Assert.equal [8, 9, 10] (List.filter (x -> x > 7) ys) "filter > 7" in
  let g = Assert.equal 10 (List.length (List.concat [xs, ys])) "concat length" in
  ()

let testTupleTypes =
  let a = Assert.equal 1 (match (1, "one") when (n, _) -> n end) "pair fst" in
  let b = Assert.equal 60 (match (10, 20, 30) when (x, y, z) -> x + y + z end) "triple sum" in
  let c = Assert.equal 10 (match ((1, 2), (3, 4)) when ((a, b), (c, d)) -> a + b + c + d end) "nested tuple" in
  let pairs = [(1, 2), (3, 4), (5, 6)] in
  let d = Assert.equal 21 (List.foldl (acc p -> match p when (x, y) -> acc + x + y end) 0 pairs) "pairs sum" in
  let swap = p -> match p when (a, b) -> (b, a) end in
  let e = Assert.equal (20, 10) (swap (10, 20)) "swap" in
  ()

let testRecordTypes =
  let point = { x = 10, y = 20 } in
  let a = Assert.equal 30 (point.x + point.y) "point sum" in
  let rect = { topLeft = { x = 0, y = 0 }, bottomRight = { x = 100, y = 50 } } in
  let b = Assert.equal 150 (rect.bottomRight.x + rect.bottomRight.y) "nested record" in
  let p1 = { x = 5, y = 10 } in
  let p2 = { p1 | x = 100 } in
  let c = Assert.equal 110 (p2.x + p2.y) "record update" in
  let points = [{ x = 1, y = 2 }, { x = 3, y = 4 }, { x = 5, y = 6 }] in
  let d = Assert.equal 21 (List.foldl (acc p -> acc + p.x + p.y) 0 points) "records list" in
  ()

type Expr = Const int | Add Expr Expr | Mul Expr Expr | Neg Expr

let testComplexTypes =
  let rec eval = expr -> match expr
    when Const n -> n
    when Add left right -> eval left + eval right
    when Mul left right -> eval left * eval right
    when Neg e -> 0 - eval e
  end in
  let rec countOps = expr -> match expr
    when Const _ -> 0
    when Add left right -> 1 + countOps left + countOps right
    when Mul left right -> 1 + countOps left + countOps right
    when Neg e -> 1 + countOps e
  end in
  let expr1 = Add (Mul (Add (Const 3) (Const 4)) (Const 2)) (Neg (Const 5)) in
  let a = Assert.equal 9 (eval expr1) "(3+4)*2 + (-5) = 9" in
  let expr2 = Mul (Mul (Const 2) (Const 3)) (Const 4) in
  let b = Assert.equal 24 (eval expr2) "2*3*4 = 24" in
  let c = Assert.equal 4 (countOps expr1) "expr1 ops" in
  let d = Assert.equal 2 (countOps expr2) "expr2 ops" in
  let e = Assert.equal 5 (eval (Neg (Neg (Const 5)))) "--5 = 5" in
  ()

let main = args ->
  let a = testEnumTypes in
  let b = testDataADTs in
  let c = testPolymorphicADTs in
  let d = testRecursiveTypes in
  let e = testBinaryTrees in
  let f = testMaybeType in
  let g = testEitherType in
  let h = testListType in
  let i = testTupleTypes in
  let j = testRecordTypes in
  let k = testComplexTypes in
  IO.printLine "data-types: ok"
