-- Basics: Literals, Operators, Strings, Booleans, Tuples

let testIntegers =
  let a = Assert.equal 25 (42 + (-17) + 0) "sum" in
  let b = Assert.equal 59 (42 - (-17)) "diff" in
  let c = Assert.equal 42 (6 * 7) "product" in
  let d = Assert.equal 33 (Int.div 100 3) "division" in
  let e = Assert.equal (-5) (0 - 5) "negation" in
  ()

let testComparisons =
  let a = Assert.ok (5 < 10) "5 < 10" in
  let b = Assert.ok (10 <= 10) "10 <= 10" in
  let c = Assert.ok (15 > 10) "15 > 10" in
  let d = Assert.ok (10 >= 10) "10 >= 10" in
  let e = Assert.ok (5 == 5) "5 == 5" in
  let f = Assert.ok (5 != 6) "5 != 6" in
  let g = Assert.ok (Bool.negate (5 == 6)) "not 5 == 6" in
  let h = Assert.ok (Bool.negate (10 < 5)) "not 10 < 5" in
  ()

let testBooleans =
  let a = Assert.equal true (true && true) "&& tt" in
  let b = Assert.equal false (true && false) "&& tf" in
  let c = Assert.equal false (false && true) "&& ft" in
  let d = Assert.equal false (false && false) "&& ff" in
  let e = Assert.equal true (true || true) "|| tt" in
  let f = Assert.equal true (true || false) "|| tf" in
  let g = Assert.equal true (false || true) "|| ft" in
  let h = Assert.equal false (false || false) "|| ff" in
  ()

let testShortCircuit =
  let x = 0 in
  let a = Assert.equal false (false && (Int.div 1 x == 0)) "&& short-circuit" in
  let b = Assert.equal true (true || (Int.div 1 x == 0)) "|| short-circuit" in
  ()

let testStrings =
  let a = Assert.equal 5 (String.length "hello") "length hello" in
  let b = Assert.equal "hello world" (String.concat (String.concat "hello" " ") "world") "concat" in
  let c = Assert.ok (String.isEmpty "") "isEmpty empty" in
  let d = Assert.ok (Bool.negate (String.isEmpty "x")) "not isEmpty x" in
  let e = Assert.ok (String.eq "hello" "hello") "eq same" in
  let f = Assert.ok (Bool.negate (String.eq "hello" "Hello")) "eq different" in
  ()

let testStringManip =
  let s = "hello world" in
  let a = Assert.equal "hello" (String.take 5 s) "take 5" in
  let b = Assert.equal "world" (String.drop 6 s) "drop 6" in
  let c = Assert.equal "hello" (String.substring 0 5 s) "substring" in
  let d = Assert.equal "ABC" (String.toUpper "abc") "toUpper" in
  let e = Assert.equal "xyz" (String.toLower "XYZ") "toLower" in
  let f = Assert.equal "hi" (String.trim "  hi  ") "trim" in
  let g = Assert.ok (String.contains "ell" "hello") "contains" in
  let h = Assert.ok (String.startsWith "he" "hello") "startsWith" in
  let i = Assert.ok (String.endsWith "lo" "hello") "endsWith" in
  ()

let testTuples =
  let a = Assert.equal 3 (match (1, 2) when (x, y) -> x + y end) "pair" in
  let b = Assert.equal 6 (match (1, 2, 3) when (x, y, z) -> x + y + z end) "triple" in
  let c = Assert.equal 10 (match (1, 2, 3, 4) when (a, b, c, d) -> a + b + c + d end) "quad" in
  let d = Assert.equal 10 (match ((1, 2), (3, 4)) when ((a, b), (c, d)) -> a + b + c + d end) "nested" in
  let e = Assert.equal 47 (match (42, "hello", true) when (n, s, b) -> if b then n + String.length s else 0 end) "mixed" in
  ()

let testBindings =
  let x = 1 in
  let y = 2 in
  let z = x + y in
  let a = Assert.equal 3 z "z = x + y" in
  let x = 10 in
  let w = x + y in
  let b = Assert.equal 12 w "w = shadowed x + y" in
  let nested =
    let a = 1 in
    let b = 2 in
    let c = let a = 10 in a + b in
    a + b + c
  in
  let c = Assert.equal 15 nested "nested shadowing" in
  ()

let testConditionals =
  let a = Assert.equal 1 (if true then 1 else 2) "if true" in
  let b = Assert.equal 2 (if false then 1 else 2) "if false" in
  let c = Assert.equal 10 (if 5 > 3 then 10 else 20) "if 5 > 3" in
  let d = Assert.equal 20 (if 5 < 3 then 10 else 20) "if 5 < 3" in
  let e = Assert.equal 2 (if true then if false then 1 else 2 else 3) "nested if" in
  let classify = n -> if n < 0 then -1 else if n == 0 then 0 else 1 in
  let f = Assert.equal (-1) (classify (-5)) "classify -5" in
  let g = Assert.equal 0 (classify 0) "classify 0" in
  let h = Assert.equal 1 (classify 10) "classify 10" in
  ()

let testMaybe =
  let a = Assert.equal 42 (match Just 42 when Just x -> x when Nothing -> 0 end) "Just 42" in
  let b = Assert.equal 0 (match Nothing when Just x -> x when Nothing -> 0 end) "Nothing" in
  let c = Assert.ok (Maybe.isJust (Just 1)) "isJust Just" in
  let d = Assert.ok (Bool.negate (Maybe.isJust Nothing)) "not isJust Nothing" in
  let e = Assert.ok (Maybe.isNothing Nothing) "isNothing Nothing" in
  let f = Assert.ok (Bool.negate (Maybe.isNothing (Just 1))) "not isNothing Just" in
  let g = Assert.equal 99 (Maybe.withDefault 99 Nothing) "withDefault Nothing" in
  let h = Assert.equal 42 (Maybe.withDefault 99 (Just 42)) "withDefault Just" in
  let i = Assert.equal (Just 20) (Maybe.map (x -> x * 2) (Just 10)) "map Just" in
  let j = Assert.equal Nothing (Maybe.map (x -> x * 2) Nothing) "map Nothing" in
  ()

let testEither =
  let a = Assert.equal 42 (match Right 42 when Right x -> x when Left e -> 0 end) "Right 42" in
  let b = Assert.equal (-1) (match Left "error" when Right x -> x when Left e -> -1 end) "Left error" in
  let c = Assert.ok (Either.isRight (Right 1)) "isRight Right" in
  let d = Assert.ok (Either.isLeft (Left "x")) "isLeft Left" in
  let e = Assert.equal 42 (Either.withDefault 99 (Right 42)) "withDefault Right" in
  let f = Assert.equal 99 (Either.withDefault 99 (Left "err")) "withDefault Left" in
  let g = Assert.equal (Right 11) (Either.map (x -> x + 1) (Right 10)) "map Right" in
  ()

let testLists =
  let xs = [1, 2, 3, 4, 5] in
  let a = Assert.equal 5 (List.length xs) "length" in
  let b = Assert.equal 0 (List.length Nil) "length Nil" in
  let c = Assert.ok (List.isEmpty Nil) "isEmpty Nil" in
  let d = Assert.ok (Bool.negate (List.isEmpty xs)) "not isEmpty xs" in
  let e = Assert.equal (Just 1) (List.head xs) "head" in
  let f = Assert.equal 4 (Maybe.withDefault 0 (Maybe.map List.length (List.tail xs))) "tail length" in
  let g = Assert.equal 15 (List.foldl (acc x -> acc + x) 0 xs) "foldl sum" in
  let h = Assert.equal 120 (List.foldl (acc x -> acc * x) 1 xs) "foldl product" in
  ()

let testListOps =
  let xs = [1, 2, 3, 4, 5] in
  let a = Assert.equal [2, 4, 6, 8, 10] (List.map (x -> x * 2) xs) "map double" in
  let b = Assert.equal [2, 4] (List.filter (x -> Int.mod x 2 == 0) xs) "filter even" in
  let c = Assert.equal 8 (List.length (List.append xs [6, 7, 8])) "append length" in
  let d = Assert.equal (Just 5) (List.head (List.reverse xs)) "reverse head" in
  let e = Assert.equal 3 (List.length (List.take 3 xs)) "take 3" in
  let f = Assert.equal 3 (List.length (List.drop 2 xs)) "drop 2" in
  ()

let testCons =
  let xs = 1 :: 2 :: 3 :: Nil in
  let a = Assert.equal 3 (List.length xs) "cons length" in
  let b = Assert.equal 4 (List.length (0 :: xs)) "prepend length" in
  let c = Assert.equal (Just 0) (List.head (0 :: xs)) "prepend head" in
  let d = Assert.equal 3 (List.foldl (acc x -> acc + x) 0 (0 :: [1, 2])) "cons with list" in
  ()

let testChars =
  let a = Assert.ok (Char.isAlpha 'a') "isAlpha a" in
  let b = Assert.ok (Bool.negate (Char.isAlpha '5')) "not isAlpha 5" in
  let c = Assert.ok (Char.isDigit '5') "isDigit 5" in
  let d = Assert.ok (Bool.negate (Char.isDigit 'a')) "not isDigit a" in
  let e = Assert.ok (Char.isUpper 'Z') "isUpper Z" in
  let f = Assert.ok (Char.isLower 'a') "isLower a" in
  let g = Assert.ok (Char.isSpace ' ') "isSpace" in
  let h = Assert.equal 97 (Char.toInt 'a') "toInt a" in
  ()

let testIntConversion =
  let a = Assert.equal "42" (Int.toString 42) "toString 42" in
  let b = Assert.equal "-17" (Int.toString (-17)) "toString -17" in
  let c = Assert.equal (Just 123) (Int.fromString "123") "fromString 123" in
  let d = Assert.equal Nothing (Int.fromString "abc") "fromString abc" in
  let e = Assert.equal 50 (Int.abs (-50)) "abs -50" in
  ()

let main = args ->
  let a = testIntegers in
  let b = testComparisons in
  let c = testBooleans in
  let d = testShortCircuit in
  let e = testStrings in
  let f = testStringManip in
  let g = testTuples in
  let h = testBindings in
  let i = testConditionals in
  let j = testMaybe in
  let k = testEither in
  let l = testLists in
  let m = testListOps in
  let n = testCons in
  let o = testChars in
  let p = testIntConversion in
  IO.printLine "basics: ok"
