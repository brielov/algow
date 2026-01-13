-- Pattern Matching: All Pattern Types, Guards, Nested, Or-Patterns

type Color = Red | Green | Blue | RGB int int int

let colorValue = c -> match c when Red -> 1 when Green -> 2 when Blue -> 3 when RGB r g b -> r + g + b end

let describeInt = n -> match n when 0 -> "zero" when 1 -> "one" when 2 -> "two" when _ -> "many" end

let describeBool = b -> match b when true -> "yes" when false -> "no" end

let extractValue = opt -> match opt when Nothing -> 0 when Just x -> x end

let sumTuple = t -> match t when (a, b) -> a + b end

let extractNested = opt -> match opt when Nothing -> 0 when Just (a, b) -> a * b end

let isJust_ = opt -> match opt when Nothing -> false when Just _ -> true end

let getFirst = t -> match t when (x, _) -> x end

let headOrZero = xs -> match xs when Nil -> 0 when Cons x _ -> x end

let classify = n -> match n
  when x if x < 0 -> -1
  when x if x == 0 -> 0
  when x if x < 10 -> 1
  when x if x < 100 -> 2
  when _ -> 3
end

let safeDiv = pair -> match pair when (_, 0) -> Nothing when (a, b) -> Just (Int.div a b) end

let headAndAll = xs -> match xs when Nil -> Nothing when Cons x _ as whole -> Just (x, whole) end

let swapPair = p -> match p when (a, b) -> (b, a) end

let sumTriple = t -> match t when (a, b, c) -> a + b + c end

let flattenPairs = t -> match t when ((a, b), (c, d)) -> a + b + c + d end

let extractMiddle = t -> match t when (_, x, _) -> x end

let isEmpty_ = xs -> match xs when Nil -> true when Cons _ _ -> false end

let isSingleton = xs -> match xs when Cons _ Nil -> true when _ -> false end

let sumFirstTwo = xs -> match xs
  when Cons a (Cons b _) -> a + b
  when Cons a Nil -> a
  when Nil -> 0
end

let getX = p -> match p when { x = x, y = _ } -> x end

let sumCoords = p -> match p when { x = x, y = y } -> x + y end

let getInnerX = p -> match p when { inner = { x = x, y = _ }, value = _ } -> x end

let isRedOrBlue = c -> match c when Red | Blue -> true when _ -> false end

let isNothingOrZero = opt -> match opt when Nothing | Just 0 -> true when _ -> false end

let extractDeep = opt -> match opt
  when Just (Just (Just x)) -> x
  when Just (Just Nothing) -> -1
  when Just Nothing -> -2
  when Nothing -> -3
end

let tests =
  [ Test.equal "Red" 1 (colorValue Red)
  , Test.equal "Green" 2 (colorValue Green)
  , Test.equal "Blue" 3 (colorValue Blue)
  , Test.equal "RGB" 60 (colorValue (RGB 10 20 30))
  , Test.equal "0 -> zero" "zero" (describeInt 0)
  , Test.equal "1 -> one" "one" (describeInt 1)
  , Test.equal "99 -> many" "many" (describeInt 99)
  , Test.equal "true -> yes" "yes" (describeBool true)
  , Test.equal "false -> no" "no" (describeBool false)
  , Test.equal "Nothing" 0 (extractValue Nothing)
  , Test.equal "Just 42" 42 (extractValue (Just 42))
  , Test.equal "tuple sum" 30 (sumTuple (10, 20))
  , Test.equal "nested extract" 12 (extractNested (Just (3, 4)))
  , Test.ok "Nothing not Just" (Bool.negate (isJust_ Nothing))
  , Test.ok "Just _ is Just" (isJust_ (Just "ignored"))
  , Test.equal "getFirst" 42 (getFirst (42, "ignored"))
  , Test.equal "headOrZero list" 10 (headOrZero [10, 20, 30])
  , Test.equal "headOrZero Nil" 0 (headOrZero Nil)
  , Test.equal "classify -5" (-1) (classify (-5))
  , Test.equal "classify 0" 0 (classify 0)
  , Test.equal "classify 5" 1 (classify 5)
  , Test.equal "classify 50" 2 (classify 50)
  , Test.equal "classify 500" 3 (classify 500)
  , Test.equal "safeDiv ok" (Just 5) (safeDiv (10, 2))
  , Test.equal "safeDiv by 0" Nothing (safeDiv (10, 0))
  , Test.equal "headAndAll" (Just (1, [1, 2, 3])) (headAndAll [1, 2, 3])
  , Test.equal "swap" (2, 1) (swapPair (1, 2))
  , Test.equal "triple sum" 60 (sumTriple (10, 20, 30))
  , Test.equal "flatten pairs" 10 (flattenPairs ((1, 2), (3, 4)))
  , Test.equal "extract middle" 42 (extractMiddle ("a", 42, "b"))
  , Test.ok "Nil isEmpty" (isEmpty_ Nil)
  , Test.ok "[1,2] not empty" (Bool.negate (isEmpty_ [1, 2]))
  , Test.ok "[42] singleton" (isSingleton [42])
  , Test.ok "[1,2] not singleton" (Bool.negate (isSingleton [1, 2]))
  , Test.equal "sumFirstTwo" 30 (sumFirstTwo [10, 20, 30])
  , Test.equal "sumFirstTwo single" 5 (sumFirstTwo [5])
  , Test.equal "sumFirstTwo Nil" 0 (sumFirstTwo Nil)
  , Test.equal "getX" 10 (getX { x = 10, y = 20 })
  , Test.equal "sumCoords" 7 (sumCoords { x = 3, y = 4 })
  , Test.equal "getInnerX" 100 (getInnerX { inner = { x = 100, y = 200 }, value = 0 })
  , Test.ok "Red is red or blue" (isRedOrBlue Red)
  , Test.ok "Blue is red or blue" (isRedOrBlue Blue)
  , Test.ok "Green not red or blue" (Bool.negate (isRedOrBlue Green))
  , Test.ok "Nothing" (isNothingOrZero Nothing)
  , Test.ok "Just 0" (isNothingOrZero (Just 0))
  , Test.ok "Just 5 not match" (Bool.negate (isNothingOrZero (Just 5)))
  , Test.equal "deep Just" 42 (extractDeep (Just (Just (Just 42))))
  , Test.equal "JJN" (-1) (extractDeep (Just (Just Nothing)))
  , Test.equal "JN" (-2) (extractDeep (Just Nothing))
  , Test.equal "N" (-3) (extractDeep Nothing)
  , Test.equal "match as value" 20 (match Just 10 when Nothing -> 0 when Just n -> n * 2 end)
  , Test.equal "map with match" [100, 1, 2, 100, 3] (List.map (n -> match n when 0 -> 100 when _ -> n end) [0, 1, 2, 0, 3])
  , Test.equal "nested match" 50 (match Just (Just 5)
      when Just inner -> match inner when Just n -> n * 10 when Nothing -> 0 end
      when Nothing -> -1
    end)
  ]

let main = args -> Test.run tests
