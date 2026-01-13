-- Data Types: ADTs, Polymorphism, Recursive Types, Records, Tuples

type Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

type Shape = Circle int | Rectangle int int | Triangle int int int

type Box a = EmptyBox | FilledBox a

type Nat = Zero | Succ Nat

type Tree a = Leaf | Node a (Tree a) (Tree a)

type Expr = Const int | Add Expr Expr | Mul Expr Expr | Neg Expr

let isWeekend = day -> match day when Saturday | Sunday -> true when _ -> false end

let weekdayNumber = day -> match day
  when Monday -> 1 when Tuesday -> 2 when Wednesday -> 3 when Thursday -> 4
  when Friday -> 5 when Saturday -> 6 when Sunday -> 7
end

let area = shape -> match shape
  when Circle r -> r * r * 3
  when Rectangle w h -> w * h
  when Triangle a b c -> Int.div (a + b + c) 2
end

let perimeter = shape -> match shape
  when Circle r -> 2 * 3 * r
  when Rectangle w h -> 2 * (w + h)
  when Triangle a b c -> a + b + c
end

let unbox = default_ box -> match box when EmptyBox -> default_ when FilledBox x -> x end

let doubleUnbox = outer -> match outer
  when EmptyBox -> 0
  when FilledBox inner -> match inner when EmptyBox -> -1 when FilledBox x -> x end
end

let rec natToInt = n -> match n when Zero -> 0 when Succ m -> 1 + natToInt m end

let rec intToNat = n -> if n <= 0 then Zero else Succ (intToNat (n - 1))

let rec addNat = a b -> match a when Zero -> b when Succ m -> Succ (addNat m b) end

let rec treeSize = tree -> match tree when Leaf -> 0 when Node _ left right -> 1 + treeSize left + treeSize right end

let rec treeDepth = tree -> match tree
  when Leaf -> 0
  when Node _ left right -> 1 + (if treeDepth left > treeDepth right then treeDepth left else treeDepth right)
end

let rec sumTree = tree -> match tree when Leaf -> 0 when Node x left right -> x + sumTree left + sumTree right end

let rec treeMap = f tree -> match tree
  when Leaf -> Leaf
  when Node x left right -> Node (f x) (treeMap f left) (treeMap f right)
end

let safeHead = xs -> match xs when Nil -> Nothing when Cons x _ -> Just x end

let maybeOr = default_ opt -> match opt when Nothing -> default_ when Just x -> x end

let safeDivide = a b -> if b == 0 then Left "division by zero" else Right (Int.div a b)

let eitherOr = default_ either_ -> match either_ when Left _ -> default_ when Right x -> x end

let rec eval = expr -> match expr
  when Const n -> n
  when Add left right -> eval left + eval right
  when Mul left right -> eval left * eval right
  when Neg e -> 0 - eval e
end

let rec countOps = expr -> match expr
  when Const _ -> 0
  when Add left right -> 1 + countOps left + countOps right
  when Mul left right -> 1 + countOps left + countOps right
  when Neg e -> 1 + countOps e
end

let tests =
  let three = Succ (Succ (Succ Zero)) in
  let two = Succ (Succ Zero) in
  let tree = Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 Leaf (Node 10 Leaf Leaf)) in
  let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil in
  let ys = [6, 7, 8, 9, 10] in
  let pairs = [(1, 2), (3, 4), (5, 6)] in
  let swap = p -> match p when (a, b) -> (b, a) end in
  let point = { x = 10, y = 20 } in
  let rect = { topLeft = { x = 0, y = 0 }, bottomRight = { x = 100, y = 50 } } in
  let p1 = { x = 5, y = 10 } in
  let p2 = { p1 | x = 100 } in
  let points = [{ x = 1, y = 2 }, { x = 3, y = 4 }, { x = 5, y = 6 }] in
  let expr1 = Add (Mul (Add (Const 3) (Const 4)) (Const 2)) (Neg (Const 5)) in
  let expr2 = Mul (Mul (Const 2) (Const 3)) (Const 4) in
  [ Test.equal "Monday = 1" 1 (weekdayNumber Monday)
  , Test.equal "Friday = 5" 5 (weekdayNumber Friday)
  , Test.equal "Sunday = 7" 7 (weekdayNumber Sunday)
  , Test.ok "Saturday is weekend" (isWeekend Saturday)
  , Test.ok "Monday not weekend" (Bool.negate (isWeekend Monday))
  , Test.equal "Circle area" 75 (area (Circle 5))
  , Test.equal "Rectangle area" 24 (area (Rectangle 4 6))
  , Test.equal "Triangle area" 6 (area (Triangle 3 4 5))
  , Test.equal "Circle perimeter" 42 (perimeter (Circle 7))
  , Test.equal "Rectangle perimeter" 16 (perimeter (Rectangle 3 5))
  , Test.equal "Triangle perimeter" 12 (perimeter (Triangle 3 4 5))
  , Test.equal "unbox FilledBox" 42 (unbox 0 (FilledBox 42))
  , Test.equal "unbox EmptyBox" 0 (unbox 0 EmptyBox)
  , Test.equal "unbox string" "hello" (unbox "" (FilledBox "hello"))
  , Test.equal "unbox string default" "default" (unbox "default" EmptyBox)
  , Test.equal "nested FilledBox" 100 (doubleUnbox (FilledBox (FilledBox 100)))
  , Test.equal "nested EmptyBox" (-1) (doubleUnbox (FilledBox EmptyBox))
  , Test.equal "outer EmptyBox" 0 (doubleUnbox EmptyBox)
  , Test.equal "natToInt 3" 3 (natToInt three)
  , Test.equal "natToInt 2" 2 (natToInt two)
  , Test.equal "addNat 3 2" 5 (natToInt (addNat three two))
  , Test.equal "intToNat 7" 7 (natToInt (intToNat 7))
  , Test.equal "tree size" 6 (treeSize tree)
  , Test.equal "tree depth" 3 (treeDepth tree)
  , Test.equal "tree sum" 31 (sumTree tree)
  , Test.equal "doubled tree sum" 62 (sumTree (treeMap (x -> x * 2) tree))
  , Test.equal "safeHead list" (Just 10) (safeHead [10, 20, 30])
  , Test.equal "safeHead Nil" Nothing (safeHead Nil)
  , Test.equal "maybeOr Just" 10 (maybeOr 0 (safeHead [10, 20, 30]))
  , Test.equal "maybeOr Nothing" 0 (maybeOr 0 (safeHead Nil))
  , Test.equal "safeDivide ok" (Right 5) (safeDivide 10 2)
  , Test.ok "safeDivide by zero" (Either.isLeft (safeDivide 10 0))
  , Test.equal "eitherOr Right" 5 (eitherOr 0 (safeDivide 10 2))
  , Test.equal "eitherOr Left" (-1) (eitherOr (-1) (safeDivide 10 0))
  , Test.equal "xs length" 5 (List.length xs)
  , Test.equal "ys length" 5 (List.length ys)
  , Test.equal "xs sum" 15 (List.foldl (acc x -> acc + x) 0 xs)
  , Test.equal "ys sum" 40 (List.foldl (acc x -> acc + x) 0 ys)
  , Test.equal "doubled sum" 30 (List.foldl (acc x -> acc + x) 0 (List.map (x -> x * 2) xs))
  , Test.equal "filter > 7" [8, 9, 10] (List.filter (x -> x > 7) ys)
  , Test.equal "concat length" 10 (List.length (List.concat [xs, ys]))
  , Test.equal "pair fst" 1 (match (1, "one") when (n, _) -> n end)
  , Test.equal "triple sum" 60 (match (10, 20, 30) when (x, y, z) -> x + y + z end)
  , Test.equal "nested tuple" 10 (match ((1, 2), (3, 4)) when ((a, b), (c, d)) -> a + b + c + d end)
  , Test.equal "pairs sum" 21 (List.foldl (acc p -> match p when (x, y) -> acc + x + y end) 0 pairs)
  , Test.equal "swap" (20, 10) (swap (10, 20))
  , Test.equal "point sum" 30 (point.x + point.y)
  , Test.equal "nested record" 150 (rect.bottomRight.x + rect.bottomRight.y)
  , Test.equal "record update" 110 (p2.x + p2.y)
  , Test.equal "records list" 21 (List.foldl (acc p -> acc + p.x + p.y) 0 points)
  , Test.equal "(3+4)*2 + (-5) = 9" 9 (eval expr1)
  , Test.equal "2*3*4 = 24" 24 (eval expr2)
  , Test.equal "expr1 ops" 4 (countOps expr1)
  , Test.equal "expr2 ops" 2 (countOps expr2)
  , Test.equal "--5 = 5" 5 (eval (Neg (Neg (Const 5))))
  ]

let main = args -> Test.run tests
