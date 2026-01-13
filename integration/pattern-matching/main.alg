-- Pattern Matching: All Pattern Types, Guards, Nested, Or-Patterns

type Color = Red | Green | Blue | RGB int int int

let testADTPatterns =
  let colorValue = c -> match c when Red -> 1 when Green -> 2 when Blue -> 3 when RGB r g b -> r + g + b end in
  let a = Assert.equal 1 (colorValue Red) "Red" in
  let b = Assert.equal 2 (colorValue Green) "Green" in
  let c = Assert.equal 3 (colorValue Blue) "Blue" in
  let d = Assert.equal 60 (colorValue (RGB 10 20 30)) "RGB" in
  ()

let testLiteralPatterns =
  let describeInt = n -> match n when 0 -> "zero" when 1 -> "one" when 2 -> "two" when _ -> "many" end in
  let a = Assert.equal "zero" (describeInt 0) "0 -> zero" in
  let b = Assert.equal "one" (describeInt 1) "1 -> one" in
  let c = Assert.equal "many" (describeInt 99) "99 -> many" in
  let describeBool = b -> match b when true -> "yes" when false -> "no" end in
  let d = Assert.equal "yes" (describeBool true) "true -> yes" in
  let e = Assert.equal "no" (describeBool false) "false -> no" in
  ()

let testVariablePatterns =
  let extractValue = opt -> match opt when Nothing -> 0 when Just x -> x end in
  let a = Assert.equal 0 (extractValue Nothing) "Nothing" in
  let b = Assert.equal 42 (extractValue (Just 42)) "Just 42" in
  let sumTuple = t -> match t when (a, b) -> a + b end in
  let c = Assert.equal 30 (sumTuple (10, 20)) "tuple sum" in
  let extractNested = opt -> match opt when Nothing -> 0 when Just (a, b) -> a * b end in
  let d = Assert.equal 12 (extractNested (Just (3, 4))) "nested extract" in
  ()

let testWildcardPatterns =
  let isJust_ = opt -> match opt when Nothing -> false when Just _ -> true end in
  let a = Assert.ok (Bool.negate (isJust_ Nothing)) "Nothing not Just" in
  let b = Assert.ok (isJust_ (Just "ignored")) "Just _ is Just" in
  let getFirst = t -> match t when (x, _) -> x end in
  let c = Assert.equal 42 (getFirst (42, "ignored")) "getFirst" in
  let headOrZero = xs -> match xs when Nil -> 0 when Cons x _ -> x end in
  let d = Assert.equal 10 (headOrZero [10, 20, 30]) "headOrZero list" in
  let e = Assert.equal 0 (headOrZero Nil) "headOrZero Nil" in
  ()

let testGuardPatterns =
  let classify = n -> match n
    when x if x < 0 -> -1
    when x if x == 0 -> 0
    when x if x < 10 -> 1
    when x if x < 100 -> 2
    when _ -> 3
  end in
  let a = Assert.equal (-1) (classify (-5)) "classify -5" in
  let b = Assert.equal 0 (classify 0) "classify 0" in
  let c = Assert.equal 1 (classify 5) "classify 5" in
  let d = Assert.equal 2 (classify 50) "classify 50" in
  let e = Assert.equal 3 (classify 500) "classify 500" in
  let safeDiv = pair -> match pair when (_, 0) -> Nothing when (a, b) -> Just (Int.div a b) end in
  let f = Assert.equal (Just 5) (safeDiv (10, 2)) "safeDiv ok" in
  let g = Assert.equal Nothing (safeDiv (10, 0)) "safeDiv by 0" in
  ()

let testAsPatterns =
  let headAndAll = xs -> match xs when Nil -> Nothing when Cons x _ as whole -> Just (x, whole) end in
  let result = headAndAll [1, 2, 3] in
  let a = Assert.equal (Just (1, [1, 2, 3])) result "headAndAll" in
  ()

let testTuplePatterns =
  let swapPair = p -> match p when (a, b) -> (b, a) end in
  let a = Assert.equal (2, 1) (swapPair (1, 2)) "swap" in
  let sumTriple = t -> match t when (a, b, c) -> a + b + c end in
  let b = Assert.equal 60 (sumTriple (10, 20, 30)) "triple sum" in
  let flattenPairs = t -> match t when ((a, b), (c, d)) -> a + b + c + d end in
  let c = Assert.equal 10 (flattenPairs ((1, 2), (3, 4))) "flatten pairs" in
  let extractMiddle = t -> match t when (_, x, _) -> x end in
  let d = Assert.equal 42 (extractMiddle ("a", 42, "b")) "extract middle" in
  ()

let testListPatterns =
  let isEmpty_ = xs -> match xs when Nil -> true when Cons _ _ -> false end in
  let a = Assert.ok (isEmpty_ Nil) "Nil isEmpty" in
  let b = Assert.ok (Bool.negate (isEmpty_ [1, 2])) "[1,2] not empty" in
  let isSingleton = xs -> match xs when Cons _ Nil -> true when _ -> false end in
  let c = Assert.ok (isSingleton [42]) "[42] singleton" in
  let d = Assert.ok (Bool.negate (isSingleton [1, 2])) "[1,2] not singleton" in
  let sumFirstTwo = xs -> match xs
    when Cons a (Cons b _) -> a + b
    when Cons a Nil -> a
    when Nil -> 0
  end in
  let e = Assert.equal 30 (sumFirstTwo [10, 20, 30]) "sumFirstTwo" in
  let f = Assert.equal 5 (sumFirstTwo [5]) "sumFirstTwo single" in
  let g = Assert.equal 0 (sumFirstTwo Nil) "sumFirstTwo Nil" in
  ()

let testRecordPatterns =
  let getX = p -> match p when { x = x, y = _ } -> x end in
  let a = Assert.equal 10 (getX { x = 10, y = 20 }) "getX" in
  let sumCoords = p -> match p when { x = x, y = y } -> x + y end in
  let b = Assert.equal 7 (sumCoords { x = 3, y = 4 }) "sumCoords" in
  let getInnerX = p -> match p when { inner = { x = x, y = _ }, value = _ } -> x end in
  let c = Assert.equal 100 (getInnerX { inner = { x = 100, y = 200 }, value = 0 }) "getInnerX" in
  ()

let testOrPatterns =
  let isRedOrBlue = c -> match c when Red | Blue -> true when _ -> false end in
  let a = Assert.ok (isRedOrBlue Red) "Red is red or blue" in
  let b = Assert.ok (isRedOrBlue Blue) "Blue is red or blue" in
  let c = Assert.ok (Bool.negate (isRedOrBlue Green)) "Green not red or blue" in
  let isNothingOrZero = opt -> match opt when Nothing | Just 0 -> true when _ -> false end in
  let d = Assert.ok (isNothingOrZero Nothing) "Nothing" in
  let e = Assert.ok (isNothingOrZero (Just 0)) "Just 0" in
  let f = Assert.ok (Bool.negate (isNothingOrZero (Just 5))) "Just 5 not match" in
  ()

let testNestedPatterns =
  let extractDeep = opt -> match opt
    when Just (Just (Just x)) -> x
    when Just (Just Nothing) -> -1
    when Just Nothing -> -2
    when Nothing -> -3
  end in
  let a = Assert.equal 42 (extractDeep (Just (Just (Just 42)))) "deep Just" in
  let b = Assert.equal (-1) (extractDeep (Just (Just Nothing))) "JJN" in
  let c = Assert.equal (-2) (extractDeep (Just Nothing)) "JN" in
  let d = Assert.equal (-3) (extractDeep Nothing) "N" in
  ()

let testMatchAsValue =
  let x = match Just 10 when Nothing -> 0 when Just n -> n * 2 end in
  let a = Assert.equal 20 x "match as value" in
  let result = List.map (n -> match n when 0 -> 100 when _ -> n end) [0, 1, 2, 0, 3] in
  let b = Assert.equal [100, 1, 2, 100, 3] result "map with match" in
  let z = match Just (Just 5)
    when Just inner -> match inner when Just n -> n * 10 when Nothing -> 0 end
    when Nothing -> -1
  end in
  let c = Assert.equal 50 z "nested match" in
  ()

let main = args ->
  let a = testADTPatterns in
  let b = testLiteralPatterns in
  let c = testVariablePatterns in
  let d = testWildcardPatterns in
  let e = testGuardPatterns in
  let f = testAsPatterns in
  let g = testTuplePatterns in
  let h = testListPatterns in
  let i = testRecordPatterns in
  let j = testOrPatterns in
  let k = testNestedPatterns in
  let l = testMatchAsValue in
  IO.printLine "pattern-matching: ok"
