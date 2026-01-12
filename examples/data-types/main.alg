-- =============================================================================
-- Data Types: ADTs, Polymorphism, Recursive Types, Records, Tuples
-- =============================================================================

-- =============================================================================
-- Simple Enumeration Types (Sum Types)
-- =============================================================================

type Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let testEnumTypes =
  let isWeekend = day ->
    match day
      when Saturday -> true
      when Sunday -> true
      when _ -> false
    end
  in
  let weekdayNumber = day ->
    match day
      when Monday -> 1
      when Tuesday -> 2
      when Wednesday -> 3
      when Thursday -> 4
      when Friday -> 5
      when Saturday -> 6
      when Sunday -> 7
    end
  in
  let a = weekdayNumber Monday in           -- 1
  let b = weekdayNumber Friday in           -- 5
  let c = weekdayNumber Sunday in           -- 7
  let d = if isWeekend Saturday then 10 else 0 in  -- 10
  let e = if isWeekend Monday then 10 else 0 in    -- 0

  a + b + c + d + e                         -- 23

-- =============================================================================
-- ADTs with Data Fields
-- =============================================================================

type Shape = Circle int | Rectangle int int | Triangle int int int

let testDataADTs =
  let area = shape ->
    match shape
      when Circle r -> r * r * 3
      when Rectangle w h -> w * h
      when Triangle a b c -> (a + b + c) / 2
    end
  in
  let perimeter = shape ->
    match shape
      when Circle r -> 2 * 3 * r
      when Rectangle w h -> 2 * (w + h)
      when Triangle a b c -> a + b + c
    end
  in
  let a = area (Circle 5) in                -- 75
  let b = area (Rectangle 4 6) in           -- 24
  let c = area (Triangle 3 4 5) in          -- 6
  let d = perimeter (Circle 7) in           -- 42
  let e = perimeter (Rectangle 3 5) in      -- 16
  let f = perimeter (Triangle 3 4 5) in     -- 12

  a + b + c + d + e + f                     -- 175

-- =============================================================================
-- Polymorphic ADTs (Generic Types)
-- =============================================================================

type Box a = EmptyBox | FilledBox a

let testPolymorphicADTs =
  let unbox = default_ box ->
    match box
      when EmptyBox -> default_
      when FilledBox x -> x
    end
  in
  -- Box of int
  let a = unbox 0 (FilledBox 42) in         -- 42
  let b = unbox 0 EmptyBox in               -- 0

  -- Box of string (using length)
  let c = String.length (unbox "" (FilledBox "hello")) in  -- 5
  let d = String.length (unbox "default" EmptyBox) in      -- 7

  -- Nested boxes
  let doubleUnbox = outer ->
    match outer
      when EmptyBox -> 0
      when FilledBox inner -> match inner
        when EmptyBox -> -1
        when FilledBox x -> x
      end
    end
  in
  let e = doubleUnbox (FilledBox (FilledBox 100)) in  -- 100
  let f = doubleUnbox (FilledBox EmptyBox) in         -- -1
  let g = doubleUnbox EmptyBox in                     -- 0

  a + b + c + d + e + f + g                -- 153

-- =============================================================================
-- Recursive Data Types
-- =============================================================================

type Nat = Zero | Succ Nat

let testRecursiveTypes =
  let rec natToInt = n ->
    match n
      when Zero -> 0
      when Succ m -> 1 + natToInt m
    end
  in
  let rec intToNat = n ->
    if n <= 0 then Zero else Succ (intToNat (n - 1))
  in
  let rec addNat = a b ->
    match a
      when Zero -> b
      when Succ m -> Succ (addNat m b)
    end
  in
  let three = Succ (Succ (Succ Zero)) in
  let two = Succ (Succ Zero) in

  let a = natToInt three in                 -- 3
  let b = natToInt two in                   -- 2
  let c = natToInt (addNat three two) in    -- 5
  let d = natToInt (intToNat 7) in          -- 7

  a + b + c + d                             -- 17

-- =============================================================================
-- Binary Trees
-- =============================================================================

type Tree a = Leaf | Node a (Tree a) (Tree a)

let testBinaryTrees =
  let rec size = tree ->
    match tree
      when Leaf -> 0
      when Node _ left right -> 1 + size left + size right
    end
  in
  let rec depth = tree ->
    match tree
      when Leaf -> 0
      when Node _ left right ->
        let leftD = depth left in
        let rightD = depth right in
        1 + (if leftD > rightD then leftD else rightD)
    end
  in
  let rec sumTree = tree ->
    match tree
      when Leaf -> 0
      when Node x left right -> x + sumTree left + sumTree right
    end
  in
  let rec treeMap = f tree ->
    match tree
      when Leaf -> Leaf
      when Node x left right -> Node (f x) (treeMap f left) (treeMap f right)
    end
  in
  -- Build a tree:     5
  --                  / \
  --                 3   8
  --                / \   \
  --               1   4   10
  let tree = Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 8 Leaf (Node 10 Leaf Leaf)) in
  let a = size tree in                      -- 6
  let b = depth tree in                     -- 3
  let c = sumTree tree in                   -- 31
  let doubled = treeMap (x -> x * 2) tree in
  let d = sumTree doubled in                -- 62

  a + b + c + d                             -- 102

-- =============================================================================
-- Using Built-in Maybe Type
-- =============================================================================

let testMaybeType =
  let safeHead = xs ->
    match xs
      when Nil -> Nothing
      when Cons x _ -> Just x
    end
  in
  let safeTail = xs ->
    match xs
      when Nil -> Nothing
      when Cons _ rest -> Just rest
    end
  in
  let maybeMap = f opt ->
    match opt
      when Nothing -> Nothing
      when Just x -> Just (f x)
    end
  in
  let maybeOr = default_ opt ->
    match opt
      when Nothing -> default_
      when Just x -> x
    end
  in

  let nums = [10, 20, 30] in
  let a = maybeOr 0 (safeHead nums) in      -- 10
  let b = maybeOr 0 (safeHead Nil) in       -- 0
  let c = maybeOr 0 (maybeMap (x -> x * 2) (safeHead nums)) in  -- 20
  let d = match safeTail nums
    when Nothing -> 0
    when Just rest -> List.length rest
  end in                                     -- 2

  -- Maybe chain
  let e = maybeOr 0 (maybeMap (x -> x + 5) (maybeMap (x -> x * 3) (safeHead [4, 5, 6]))) in  -- 17

  a + b + c + d + e                         -- 49

-- =============================================================================
-- Using Built-in Either Type
-- =============================================================================

let testEitherType =
  let safeDivide = a b ->
    if b == 0 then Left "division by zero" else Right (a / b)
  in
  let eitherMap = f either_ ->
    match either_
      when Left err -> Left err
      when Right x -> Right (f x)
    end
  in
  let eitherOr = default_ either_ ->
    match either_
      when Left _ -> default_
      when Right x -> x
    end
  in

  let a = eitherOr 0 (safeDivide 10 2) in   -- 5
  let b = eitherOr (-1) (safeDivide 10 0) in -- -1
  let c = eitherOr 0 (eitherMap (x -> x * 2) (safeDivide 15 3)) in  -- 10

  -- Chain operations
  let result = match safeDivide 100 5
    when Left _ -> 0
    when Right x -> match safeDivide x 2
      when Left _ -> 0
      when Right y -> y
    end
  end in
  let d = result in                         -- 10

  a + b + c + d                             -- 24

-- =============================================================================
-- Using Built-in List Type
-- =============================================================================

let testListType =
  -- List construction
  let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil in
  let ys = [6, 7, 8, 9, 10] in

  -- Basic operations
  let a = List.length xs in                 -- 5
  let b = List.length ys in                 -- 5

  -- List operations with prelude
  let c = List.foldl (acc x -> acc + x) 0 xs in  -- 15
  let d = List.foldl (acc x -> acc + x) 0 ys in  -- 40

  -- Map
  let doubled = List.map (x -> x * 2) xs in
  let e = List.foldl (acc x -> acc + x) 0 doubled in  -- 30

  -- Filter
  let evens = List.filter (x -> x / 2 * 2 == x) ys in
  let f = List.foldl (acc x -> acc + x) 0 evens in    -- 24 (6 + 8 + 10)

  -- Concat
  let combined = List.concat [xs, ys] in
  let g = List.length combined in           -- 10

  a + b + c + d + e + f + g                 -- 129

-- =============================================================================
-- Tuple Types
-- =============================================================================

let testTupleTypes =
  -- Pairs
  let pair = (1, "one") in
  let a = match pair when (n, _) -> n end in  -- 1

  -- Triples
  let triple = (10, 20, 30) in
  let b = match triple when (x, y, z) -> x + y + z end in  -- 60

  -- Nested tuples
  let nested = ((1, 2), (3, 4)) in
  let c = match nested when ((a, b), (c, d)) -> a + b + c + d end in  -- 10

  -- Tuple in list
  let pairs = [(1, 2), (3, 4), (5, 6)] in
  let d = List.foldl (acc p -> match p when (x, y) -> acc + x + y end) 0 pairs in  -- 21

  -- Swap function
  let swap = p -> match p when (a, b) -> (b, a) end in
  let (y, x) = swap (10, 20) in
  let e = x + y in                          -- 30

  a + b + c + d + e                         -- 122

-- =============================================================================
-- Record Types
-- =============================================================================

let testRecordTypes =
  -- Simple record
  let point = { x = 10, y = 20 } in
  let a = point.x + point.y in              -- 30

  -- Nested records
  let rect = {
    topLeft = { x = 0, y = 0 },
    bottomRight = { x = 100, y = 50 }
  } in
  let width = rect.bottomRight.x - rect.topLeft.x in
  let height = rect.bottomRight.y - rect.topLeft.y in
  let b = width + height in                 -- 150

  -- Record with function field
  let counter = {
    value = 0,
    increment = c -> { value = c.value + 1, increment = c.increment }
  } in
  let c1 = counter.increment counter in
  let c2 = c1.increment c1 in
  let c = c2.value in                       -- 2

  -- Record update
  let p1 = { x = 5, y = 10 } in
  let p2 = { p1 | x = 100 } in
  let d = p2.x + p2.y in                    -- 110

  -- List of records
  let points = [
    { x = 1, y = 2 },
    { x = 3, y = 4 },
    { x = 5, y = 6 }
  ] in
  let e = List.foldl (acc p -> acc + p.x + p.y) 0 points in  -- 21

  a + b + c + d + e                         -- 313

-- =============================================================================
-- Complex Type Hierarchies
-- =============================================================================

type Expr = Const int | Add Expr Expr | Mul Expr Expr | Neg Expr

let testComplexTypes =
  let rec eval = expr ->
    match expr
      when Const n -> n
      when Add left right -> eval left + eval right
      when Mul left right -> eval left * eval right
      when Neg e -> 0 - eval e
    end
  in
  let rec countOps = expr ->
    match expr
      when Const _ -> 0
      when Add left right -> 1 + countOps left + countOps right
      when Mul left right -> 1 + countOps left + countOps right
      when Neg e -> 1 + countOps e
    end
  in

  -- (3 + 4) * 2 - 5 = 14 - 5 = 9
  -- Actually: (3 + 4) * 2 = 14, then negate 5 and add = 14 + (-5) = 9
  -- Let's do: Add (Mul (Add (Const 3) (Const 4)) (Const 2)) (Neg (Const 5))
  let expr1 = Add (Mul (Add (Const 3) (Const 4)) (Const 2)) (Neg (Const 5)) in
  let a = eval expr1 in                     -- 9

  -- 2 * 3 * 4 = 24
  let expr2 = Mul (Mul (Const 2) (Const 3)) (Const 4) in
  let b = eval expr2 in                     -- 24

  -- Count operations
  let c = countOps expr1 in                 -- 4 (Add, Mul, Add, Neg)
  let d = countOps expr2 in                 -- 2 (Mul, Mul)

  -- Nested negation: --5 = 5
  let expr3 = Neg (Neg (Const 5)) in
  let e = eval expr3 in                     -- 5

  a + b + c + d + e                         -- 44

-- =============================================================================
-- Final Result
-- =============================================================================

let result =
  testEnumTypes +                           -- 23
  testDataADTs +                            -- 175
  testPolymorphicADTs +                     -- 153
  testRecursiveTypes +                      -- 17
  testBinaryTrees +                         -- 102
  testMaybeType +                           -- 49
  testEitherType +                          -- 24
  testListType +                            -- 129
  testTupleTypes +                          -- 122
  testRecordTypes +                         -- 313
  testComplexTypes                          -- 44
                                            -- Total: 1151

result
