-- Constraints: Eq, Ord, Num testing
-- Eq constraint tests - types that support ==
let eqInt =
  a b -> a == b

let eqFloat =
  a b -> a == b

let eqString =
  a b -> a == b

let eqBool =
  a b -> a == b

let eqChar =
  a b -> a == b

-- Generic equality (polymorphic, requires Eq)
let equals =
  a b -> a == b

let notEquals =
  a b -> a != b

-- Ord constraint tests - types that support <, <=, >, >=
let lessThan =
  a b -> a < b

let lessOrEqual =
  a b -> a <= b

let greaterThan =
  a b -> a > b

let greaterOrEqual =
  a b -> a >= b

-- Generic comparison
let compare_ =
  a b -> if a < b
    then 0 - 1
    else if a > b
      then 1
      else 0

let minOf =
  a b -> if a < b
    then a
    else b

let maxOf =
  a b -> if a > b
    then a
    else b

let clamp =
  lo hi x -> maxOf lo (minOf hi x)

-- Num constraint tests - types that support +, -, *
let add =
  a b -> a + b

let subtract =
  a b -> a - b

let multiply =
  a b -> a * b

-- Generic numeric operations
let double =
  x -> x + x

let square =
  x -> x * x

let sumList =
  xs -> List.foldl (acc x -> acc + x) 0 xs

let productList =
  xs -> List.foldl (acc x -> acc * x) 1 xs

-- Combined constraints
let inRange =
  lo hi x -> x >= lo && x <= hi

let sortTwo =
  a b -> if a <= b
    then (a, b)
    else (b, a)

-- Equality on compound types
let eqTuple2 =
  t1 t2 -> t1 == t2

let eqList =
  l1 l2 -> l1 == l2

let eqMaybe =
  m1 m2 -> m1 == m2

let eqEither =
  e1 e2 -> e1 == e2

let eqRecord =
  r1 r2 -> r1 == r2

-- List operations using constraints
let rec contains = x xs ->
  match xs
    when Nil -> false
    when Cons y ys -> x == y || contains x ys
  end

let rec removeFirst = x xs ->
  match xs
    when Nil -> Nil
    when Cons y ys -> if x == y
      then ys
      else Cons y (removeFirst x ys)
  end

let rec countOccurrences = x xs ->
  match xs
    when Nil -> 0
    when Cons y ys -> (if x == y
      then 1
      else 0) + countOccurrences x ys
  end

-- Sorting using Ord
let rec insertSorted = x xs ->
  match xs
    when Nil -> [x]
    when Cons y ys -> if x <= y
      then x :: xs
      else y :: insertSorted x ys
  end

let rec insertionSort = xs ->
  match xs
    when Nil -> Nil
    when Cons x rest -> insertSorted x (insertionSort rest)
  end

-- Finding min/max in lists
let rec minList = xs ->
  match xs
    when Nil -> Nothing
    when Cons x Nil -> Just x
    when Cons x rest -> match minList rest
      when Nothing -> Just x
      when Just m -> Just (minOf x m)
    end
  end

let rec maxList = xs ->
  match xs
    when Nil -> Nothing
    when Cons x Nil -> Just x
    when Cons x rest -> match maxList rest
      when Nothing -> Just x
      when Just m -> Just (maxOf x m)
    end
  end

let tests =
  [ Test.ok "int eq true" (eqInt 5 5)
    , Test.ok "int eq false" (Bool.negate (eqInt 5 6))
    , Test.ok "int neq true" (notEquals 5 6)
    , Test.ok "int neq false" (Bool.negate (notEquals 5 5))
    , Test.ok "float eq true" (eqFloat 3.14 3.14)
    , Test.ok "float eq false" (Bool.negate (eqFloat 3.14 2.71))
    , Test.ok "string eq true" (eqString "hello" "hello")
    , Test.ok "string eq false" (Bool.negate (eqString "hello" "world"))
    , Test.ok "string eq empty" (eqString "" "")
    , Test.ok "bool eq tt" (eqBool true true)
    , Test.ok "bool eq ff" (eqBool false false)
    , Test.ok "bool eq tf" (Bool.negate (eqBool true false))
    , Test.ok "char eq true" (eqChar 'a' 'a')
    , Test.ok "char eq false" (Bool.negate (eqChar 'a' 'b'))
    , Test.ok "generic equals int" (equals 42 42)
    , Test.ok "generic equals string" (equals "test" "test")
    , Test.ok "generic equals tuple" (equals (1, 2) (1, 2))
    , Test.ok "generic equals list" (equals [1, 2, 3] [1, 2, 3])
    , Test.ok "int < true" (lessThan 5 10)
    , Test.ok "int < false" (Bool.negate (lessThan 10 5))
    , Test.ok "int <= true eq" (lessOrEqual 5 5)
    , Test.ok "int <= true lt" (lessOrEqual 5 10)
    , Test.ok "int > true" (greaterThan 10 5)
    , Test.ok "int >= true eq" (greaterOrEqual 5 5)
    , Test.ok "float < true" (lessThan 1.5 2.5)
    , Test.ok "float > true" (greaterThan 2.5 1.5)
    , Test.ok "string < true" (lessThan "apple" "banana")
    , Test.ok "string > true" (greaterThan "zebra" "apple")
    , Test.ok "string <= prefix" (lessOrEqual "app" "apple")
    , Test.ok "char < true" (lessThan 'a' 'z')
    , Test.ok "char > true" (greaterThan 'z' 'a')
    , Test.equal "compare lt" (0 - 1) (compare_ 5 10)
    , Test.equal "compare gt" 1 (compare_ 10 5)
    , Test.equal "compare eq" 0 (compare_ 5 5)
    , Test.equal "minOf int" 5 (minOf 5 10)
    , Test.equal "maxOf int" 10 (maxOf 5 10)
    , Test.equal "minOf string" "apple" (minOf "apple" "banana")
    , Test.equal "maxOf string" "banana" (maxOf "apple" "banana")
    , Test.equal "clamp within" 50 (clamp 0 100 50)
    , Test.equal "clamp below" 0 (clamp 0 100 (0 - 50))
    , Test.equal "clamp above" 100 (clamp 0 100 150)
    , Test.equal "add int" 15 (add 10 5)
    , Test.equal "subtract int" 5 (subtract 10 5)
    , Test.equal "multiply int" 50 (multiply 10 5)
    , Test.equal "double int" 20 (double 10)
    , Test.equal "square int" 25 (square 5)
    , Test.equal "add float" 5.5 (add 2.5 3.0)
    , Test.equal "subtract float" 1.5 (subtract 3.5 2.0)
    , Test.equal "multiply float" 6.0 (multiply 2.0 3.0)
    , Test.equal "double float" 5.0 (double 2.5)
    , Test.equal "sumList" 15 (sumList [1, 2, 3, 4, 5])
    , Test.equal "productList" 120 (productList [1, 2, 3, 4, 5])
    , Test.equal "sumList empty" 0 (sumList [])
    , Test.equal "productList empty" 1 (productList [])
    , Test.ok "inRange true" (inRange 0 100 50)
    , Test.ok "inRange false below" (Bool.negate (inRange 0 100 (0 - 1)))
    , Test.ok "inRange false above" (Bool.negate (inRange 0 100 101))
    , Test.ok "inRange boundary lo" (inRange 0 100 0)
    , Test.ok "inRange boundary hi" (inRange 0 100 100)
    , Test.equal "sortTwo ordered" (1, 5) (sortTwo 1 5)
    , Test.equal "sortTwo unordered" (1, 5) (sortTwo 5 1)
    , Test.equal "sortTwo equal" (3, 3) (sortTwo 3 3)
    , Test.ok "tuple equality" (eqTuple2 (1, 2) (1, 2))
    , Test.ok "tuple inequality" (Bool.negate (eqTuple2 (1, 2) (1, 3)))
    , Test.ok "list equality" (eqList [1, 2, 3] [1, 2, 3])
    , Test.ok "list inequality" (Bool.negate (eqList [1, 2, 3] [1, 2, 4]))
    , Test.ok "maybe equality Just" (eqMaybe (Just 42) (Just 42))
    , Test.ok "maybe equality Nothing" (eqMaybe Nothing Nothing)
    , Test.ok "maybe inequality" (Bool.negate (eqMaybe (Just 1) Nothing))
    , Test.ok "either equality Right" (eqEither (Right 10) (Right 10))
    , Test.ok "either equality Left" (eqEither (Left "err") (Left "err"))
    , Test.ok "record equality" (eqRecord { x = 1, y = 2 } { x = 1, y = 2 })
    , Test.ok "contains true" (contains 3 [1, 2, 3, 4, 5])
    , Test.ok "contains false" (Bool.negate (contains 10 [1, 2, 3, 4, 5]))
    , Test.ok "contains string" (contains "b" ["a", "b", "c"])
    , Test.equal "removeFirst" [1, 3, 4, 5] (removeFirst 2 [1, 2, 3, 4, 5])
    , Test.equal "removeFirst not found" [1, 2, 3] (removeFirst 10 [1, 2, 3])
    , Test.equal "countOccurrences" 3 (countOccurrences 2 [1, 2, 2, 3, 2, 4])
    , Test.equal "countOccurrences zero" 0 (countOccurrences 10 [1, 2, 3])
    , Test.equal "insertionSort int" [1, 2, 3, 4, 5] (insertionSort [3, 1, 4, 1, 5, 2])
    , Test.equal "insertionSort already sorted" [1, 2, 3] (insertionSort [1, 2, 3])
    , Test.equal "insertionSort reverse" [1, 2, 3, 4, 5] (insertionSort [5, 4, 3, 2, 1])
    , Test.equal "insertionSort string" ["apple", "banana", "cherry"] (insertionSort ["cherry", "apple", "banana"])
    , Test.equal "minList" (Just 1) (minList [3, 1, 4, 1, 5])
    , Test.equal "maxList" (Just 5) (maxList [3, 1, 4, 1, 5])
    , Test.equal "minList empty" Nothing (minList [])
    , Test.equal "maxList single" (Just 42) (maxList [42])
    , Test.equal "minList string" (Just "apple") (minList ["cherry", "apple", "banana"])
    ]

-- Int equality
-- Float equality
-- String equality
-- Bool equality
-- Char equality
-- Generic equals
-- Int ordering
-- Float ordering
-- String ordering (lexicographic)
-- Char ordering
-- Compare function
-- Min/max
-- Clamp
-- Numeric operations on int
-- Numeric operations on float
-- Sum/product of lists
-- Combined constraints
-- Compound type equality
-- List operations with Eq
-- Sorting with Ord
-- Min/max in lists
let main =
  args -> Test.run tests
