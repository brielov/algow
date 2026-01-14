-- ADT: Algebraic Data Types (Enums, Parameterized, Recursive, Constructors)
-- Simple enums
type Color
  = Red
  | Green
  | Blue

type Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

type Direction
  = North
  | South
  | East
  | West

type Ordering
  = LT
  | EQ
  | GT

-- Parameterized types
type Box a
  = EmptyBox
  | FilledBox a

type Pair a b = Pair a b

type Triple a b c = Triple a b c

type Result a e
  = Ok a
  | Err e

-- Recursive types
type Nat
  = Zero
  | Succ Nat

type MyList a
  = Empty
  | Node a (MyList a)

type Tree a
  = Leaf
  | Branch a (Tree a) (Tree a)

type BinaryTree a
  = BLeaf a
  | BNode (BinaryTree a) (BinaryTree a)

type Rose a = Rose a (List (Rose a))

-- Multi-arity constructors
type Shape
  = Circle int
  | Rectangle int int
  | Triangle int int int
  | Point

type Expr
  = Const int
  | Var string
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Let string Expr Expr

-- Complex nested types
type Nested a = Nested (Box (Box a))

type ListOfMaybe a = LoM (List (Maybe a))

-- Color tests
let colorToInt =
  c ->
    match c
      when Red -> 0
      when Green -> 1
      when Blue -> 2
    end

let intToColor =
  n ->
    match n
      when 0 -> Red
      when 1 -> Green
      when _ -> Blue
    end

let nextColor =
  c ->
    match c
      when Red -> Green
      when Green -> Blue
      when Blue -> Red
    end

-- Weekday tests
let isWeekend =
  d ->
    match d
      when Saturday | Sunday -> true
      when _ -> false
    end

let dayNumber =
  d ->
    match d
      when Monday -> 1
      when Tuesday -> 2
      when Wednesday -> 3
      when Thursday -> 4
      when Friday -> 5
      when Saturday -> 6
      when Sunday -> 7
    end

-- Direction tests
let opposite =
  d ->
    match d
      when North -> South
      when South -> North
      when East -> West
      when West -> East
    end

let turnRight =
  d ->
    match d
      when North -> East
      when East -> South
      when South -> West
      when West -> North
    end

-- Ordering tests
let compare_ =
  a b -> if a < b
    then LT
    else if a > b
      then GT
      else EQ

let orderingToInt =
  o ->
    match o
      when LT -> 0 - 1
      when EQ -> 0
      when GT -> 1
    end

-- Box tests
let unbox =
  default_ b ->
    match b
      when EmptyBox -> default_
      when FilledBox x -> x
    end

let mapBox =
  f b ->
    match b
      when EmptyBox -> EmptyBox
      when FilledBox x -> FilledBox (f x)
    end

let flatMapBox =
  f b ->
    match b
      when EmptyBox -> EmptyBox
      when FilledBox x -> f x
    end

let isEmptyBox =
  b ->
    match b
      when EmptyBox -> true
      when FilledBox _ -> false
    end

-- Pair/Triple tests
let fst_ =
  p ->
    match p
      when Pair a _ -> a
    end

let snd_ =
  p ->
    match p
      when Pair _ b -> b
    end

let first =
  t ->
    match t
      when Triple a _ _ -> a
    end

let second =
  t ->
    match t
      when Triple _ b _ -> b
    end

let third =
  t ->
    match t
      when Triple _ _ c -> c
    end

let swapPair =
  p ->
    match p
      when Pair a b -> Pair b a
    end

-- Result tests
let isOk =
  r ->
    match r
      when Ok _ -> true
      when Err _ -> false
    end

let isErr =
  r ->
    match r
      when Ok _ -> false
      when Err _ -> true
    end

let mapResult =
  f r ->
    match r
      when Ok x -> Ok (f x)
      when Err e -> Err e
    end

let mapErr =
  f r ->
    match r
      when Ok x -> Ok x
      when Err e -> Err (f e)
    end

let withDefaultResult =
  d r ->
    match r
      when Ok x -> x
      when Err _ -> d
    end

-- Nat tests
let rec natToInt = n ->
  match n
    when Zero -> 0
    when Succ m -> 1 + natToInt m
  end

let rec intToNat = n -> if n <= 0
  then Zero
  else Succ (intToNat (n - 1))

let rec addNat = a b ->
  match a
    when Zero -> b
    when Succ m -> Succ (addNat m b)
  end

let rec mulNat = a b ->
  match a
    when Zero -> Zero
    when Succ m -> addNat b (mulNat m b)
  end

let rec eqNat = a b ->
  match (a, b)
    when (Zero, Zero) -> true
    when (Succ m, Succ n) -> eqNat m n
    when _ -> false
  end

-- MyList tests
let rec myListLength = xs ->
  match xs
    when Empty -> 0
    when Node _ rest -> 1 + myListLength rest
  end

let rec myListSum = xs ->
  match xs
    when Empty -> 0
    when Node x rest -> x + myListSum rest
  end

let rec myListMap = f xs ->
  match xs
    when Empty -> Empty
    when Node x rest -> Node (f x) (myListMap f rest)
  end

let rec myListAppend = xs ys ->
  match xs
    when Empty -> ys
    when Node x rest -> Node x (myListAppend rest ys)
  end

-- Tree tests
let rec treeSize = t ->
  match t
    when Leaf -> 0
    when Branch _ l r -> 1 + treeSize l + treeSize r
  end

let rec treeDepth = t ->
  match t
    when Leaf -> 0
    when Branch _ l r -> 1 + (if treeDepth l > treeDepth r
      then treeDepth l
      else treeDepth r)
  end

let rec treeSum = t ->
  match t
    when Leaf -> 0
    when Branch x l r -> x + treeSum l + treeSum r
  end

let rec treeMap = f t ->
  match t
    when Leaf -> Leaf
    when Branch x l r -> Branch (f x) (treeMap f l) (treeMap f r)
  end

let rec treeContains = x t ->
  match t
    when Leaf -> false
    when Branch v l r -> v == x || treeContains x l || treeContains x r
  end

-- BinaryTree tests
let rec binaryTreeSize = t ->
  match t
    when BLeaf _ -> 1
    when BNode l r -> binaryTreeSize l + binaryTreeSize r
  end

let rec binaryTreeLeaves = t ->
  match t
    when BLeaf x -> [x]
    when BNode l r -> List.append (binaryTreeLeaves l) (binaryTreeLeaves r)
  end

-- Rose tree tests
let rec roseValue = r ->
  match r
    when Rose x _ -> x
  end

let rec roseChildren = r ->
  match r
    when Rose _ cs -> cs
  end

-- Expr tests
let rec eval = e ->
  match e
    when Const n -> n
    when Var _ -> 0
    when Add l r -> eval l + eval r
    when Mul l r -> eval l * eval r
    when Neg x -> 0 - eval x
    when Let _ v body -> eval body + eval v - eval v
  end

let rec countNodes = e ->
  match e
    when Const _ -> 1
    when Var _ -> 1
    when Add l r -> 1 + countNodes l + countNodes r
    when Mul l r -> 1 + countNodes l + countNodes r
    when Neg x -> 1 + countNodes x
    when Let _ v b -> 1 + countNodes v + countNodes b
  end

-- Shape tests
let area =
  s ->
    match s
      when Point -> 0
      when Circle r -> r * r * 3
      when Rectangle w h -> w * h
      when Triangle a b c -> Int.div ((a + b + c) * (a + b - c)) 4
    end

let perimeter =
  s ->
    match s
      when Point -> 0
      when Circle r -> 6 * r
      when Rectangle w h -> 2 * (w + h)
      when Triangle a b c -> a + b + c
    end

-- Nested type tests
let unwrapNested =
  n ->
    match n
      when Nested outer -> match outer
        when EmptyBox -> 0
        when FilledBox inner -> match inner
          when EmptyBox -> 0 - 1
          when FilledBox x -> x
        end
      end
    end

-- Constructor as first-class function tests
let constructorAsFunc =
  FilledBox

let partiallyApplied =
  Pair 10

let tests =
  let one = Succ Zero in
  let two = Succ (Succ Zero) in
  let three = Succ (Succ (Succ Zero)) in
  let myList = Node 1 (Node 2 (Node 3 Empty)) in
  let tree = Branch 5 (Branch 3 (Branch 1 Leaf Leaf) (Branch 4 Leaf Leaf)) (Branch 8 Leaf (Branch 10 Leaf Leaf)) in
  let expr = Add (Mul (Const 3) (Const 4)) (Neg (Const 2)) in
  let bTree = BNode (BNode (BLeaf 1) (BLeaf 2)) (BLeaf 3) in
  let rose = Rose 1 [Rose 2 [], Rose 3 [Rose 4 []]] in
  [ Test.equal "color Red" 0 (colorToInt Red)
    , Test.equal "color Green" 1 (colorToInt Green)
    , Test.equal "color Blue" 2 (colorToInt Blue)
    , Test.equal "intToColor 0" Red (intToColor 0)
    , Test.equal "nextColor cycle" Red (nextColor (nextColor (nextColor Red)))
    , Test.ok "Saturday weekend" (isWeekend Saturday)
    , Test.ok "Sunday weekend" (isWeekend Sunday)
    , Test.ok "Monday not weekend" (Bool.negate (isWeekend Monday))
    , Test.equal "Monday = 1" 1 (dayNumber Monday)
    , Test.equal "Friday = 5" 5 (dayNumber Friday)
    , Test.equal "opposite North" South (opposite North)
    , Test.equal "opposite East" West (opposite East)
    , Test.equal "turnRight North" East (turnRight North)
    , Test.equal "turnRight 4x" North (turnRight (turnRight (turnRight (turnRight North))))
    , Test.equal "compare LT" LT (compare_ 1 5)
    , Test.equal "compare GT" GT (compare_ 10 5)
    , Test.equal "compare EQ" EQ (compare_ 5 5)
    , Test.equal "orderingToInt LT" (0 - 1) (orderingToInt LT)
    , Test.equal "unbox FilledBox" 42 (unbox 0 (FilledBox 42))
    , Test.equal "unbox EmptyBox" 99 (unbox 99 EmptyBox)
    , Test.equal "mapBox" (FilledBox 10) (mapBox (x -> x * 2) (FilledBox 5))
    , Test.equal "mapBox empty" EmptyBox (mapBox (x -> x * 2) EmptyBox)
    , Test.equal "flatMapBox" (FilledBox 100) (flatMapBox (x -> FilledBox (x * x)) (FilledBox 10))
    , Test.ok "isEmptyBox true" (isEmptyBox EmptyBox)
    , Test.ok "isEmptyBox false" (Bool.negate (isEmptyBox (FilledBox 1)))
    , Test.equal "fst" 10 (fst_ (Pair 10 20))
    , Test.equal "snd" 20 (snd_ (Pair 10 20))
    , Test.equal "first" 1 (first (Triple 1 2 3))
    , Test.equal "second" 2 (second (Triple 1 2 3))
    , Test.equal "third" 3 (third (Triple 1 2 3))
    , Test.equal "swapPair" (Pair 2 1) (swapPair (Pair 1 2))
    , Test.ok "isOk Ok" (isOk (Ok 42))
    , Test.ok "isOk Err" (Bool.negate (isOk (Err "error")))
    , Test.ok "isErr Err" (isErr (Err "error"))
    , Test.equal "mapResult Ok" (Ok 84) (mapResult (x -> x * 2) (Ok 42))
    , Test.equal "mapErr" (Err "ERROR") (mapErr String.toUpper (Err "error"))
    , Test.equal "withDefaultResult Ok" 42 (withDefaultResult 0 (Ok 42))
    , Test.equal "withDefaultResult Err" 0 (withDefaultResult 0 (Err "error"))
    , Test.equal "natToInt 0" 0 (natToInt Zero)
    , Test.equal "natToInt 3" 3 (natToInt three)
    , Test.equal "intToNat 5" 5 (natToInt (intToNat 5))
    , Test.equal "addNat 2 3" 5 (natToInt (addNat two three))
    , Test.equal "mulNat 2 3" 6 (natToInt (mulNat two three))
    , Test.ok "eqNat same" (eqNat two two)
    , Test.ok "eqNat diff" (Bool.negate (eqNat one two))
    , Test.equal "myListLength" 3 (myListLength myList)
    , Test.equal "myListSum" 6 (myListSum myList)
    , Test.equal "myListMap" 12 (myListSum (myListMap (x -> x * 2) myList))
    , Test.equal "myListAppend" 5 (myListLength (myListAppend myList (Node 4 (Node 5 Empty))))
    , Test.equal "treeSize" 6 (treeSize tree)
    , Test.equal "treeDepth" 3 (treeDepth tree)
    , Test.equal "treeSum" 31 (treeSum tree)
    , Test.equal "treeMap sum" 62 (treeSum (treeMap (x -> x * 2) tree))
    , Test.ok "treeContains 5" (treeContains 5 tree)
    , Test.ok "treeContains 10" (treeContains 10 tree)
    , Test.ok "treeContains 99 false" (Bool.negate (treeContains 99 tree))
    , Test.equal "binaryTreeSize" 3 (binaryTreeSize bTree)
    , Test.equal "binaryTreeLeaves" [1, 2, 3] (binaryTreeLeaves bTree)
    , Test.equal "roseValue" 1 (roseValue rose)
    , Test.equal "roseChildren count" 2 (List.length (roseChildren rose))
    , Test.equal "eval expr" 10 (eval expr)
    , Test.equal "eval const" 42 (eval (Const 42))
    , Test.equal "eval neg" (0 - 5) (eval (Neg (Const 5)))
    , Test.equal "countNodes" 5 (countNodes expr)
    , Test.equal "Point area" 0 (area Point)
    , Test.equal "Circle area" 48 (area (Circle 4))
    , Test.equal "Rectangle area" 24 (area (Rectangle 4 6))
    , Test.equal "Point perimeter" 0 (perimeter Point)
    , Test.equal "Circle perimeter" 30 (perimeter (Circle 5))
    , Test.equal "Rectangle perimeter" 20 (perimeter (Rectangle 4 6))
    , Test.equal "nested empty outer" 0 (unwrapNested (Nested EmptyBox))
    , Test.equal "nested empty inner" (0 - 1) (unwrapNested (Nested (FilledBox EmptyBox)))
    , Test.equal "nested filled" 42 (unwrapNested (Nested (FilledBox (FilledBox 42))))
    , Test.equal "constructor as func" (FilledBox 10) (constructorAsFunc 10)
    , Test.equal "partially applied constructor" (Pair 10 20) (partiallyApplied 20)
    ]

-- Enum tests
-- Box tests
-- Pair/Triple tests
-- Result tests
-- Nat tests
-- MyList tests
-- Tree tests
-- BinaryTree tests
-- Rose tree tests
-- Expr tests
-- Shape tests
-- Nested tests
-- Constructor as function
let main =
  args -> Test.run tests
