-- Edge Cases: Empty structures, Zero values, Negative numbers, Unicode, Escapes, Shadowing
let tests =
  let emptyList = [] in
  -- Empty list operations
  [ Test.equal "empty list length" 0 (List.length emptyList)
    , Test.ok "empty list isEmpty" (List.isEmpty emptyList)
    , Test.ok "empty list head is Nothing" (match List.head emptyList
      when Nothing -> true
      when _ -> false
    end)
    , Test.ok "empty list tail is Nothing" (match List.tail emptyList
      when Nothing -> true
      when _ -> false
    end)
    , Test.equal "empty list map length" 0 (List.length (List.map (x -> x + 1) emptyList))
    , Test.equal "empty list filter length" 0 (List.length (List.filter (x -> true) emptyList))
    , Test.equal "empty list foldl" 0 (List.foldl (acc x -> acc + x) 0 emptyList)
    , Test.equal "empty list foldr" 0 (List.foldr (x acc -> x + acc) 0 emptyList)
    , Test.equal "empty list reverse length" 0 (List.length (List.reverse emptyList))
    , Test.equal "empty list append length" 2 (List.length (List.append emptyList [1, 2]))
    , Test.equal "append empty length" 2 (List.length (List.append [1, 2] emptyList))
    , Test.equal "empty list take length" 0 (List.length (List.take 5 emptyList))
    , Test.equal "empty list drop length" 0 (List.length (List.drop 5 emptyList))
    , Test.equal "empty list concat length" 0 (List.length (List.concat []))
    , Test.equal "concat empty lists length" 0 (List.length (List.concat [[], [], []]))
    , Test.equal "empty string length" 0 (String.length "")
    , Test.ok "empty string isEmpty" (String.isEmpty "")
    , Test.equal "empty string concat" "" (String.concat "" "")
    , Test.equal "empty string append" "hello" (String.concat "" "hello")
    , Test.equal "concat empty" "hello" (String.concat "hello" "")
    , Test.equal "empty string take" "" (String.take 5 "")
    , Test.equal "empty string drop" "" (String.drop 5 "")
    , Test.equal "empty string toUpper" "" (String.toUpper "")
    , Test.equal "empty string toLower" "" (String.toLower "")
    , Test.equal "empty string trim" "" (String.trim "")
    , Test.ok "empty string contains empty" (String.contains "" "hello")
    , Test.ok "empty string startsWith empty" (String.startsWith "" "hello")
    , Test.ok "empty string endsWith empty" (String.endsWith "" "hello")
    , Test.equal "zero addition" 0 (0 + 0)
    , Test.equal "zero multiplication" 0 (5 * 0)
    , Test.equal "zero subtraction" 0 (0 - 0)
    , Test.equal "mod by larger" 5 (Int.mod 5 10)
    , Test.equal "mod zero" 0 (Int.mod 0 5)
    , Test.equal "div zero by nonzero" 0 (Int.div 0 5)
    , Test.ok "zero float addition" (Float.abs (0.0 + 0.0) < 0.001)
    , Test.ok "zero float multiplication" (Float.abs (5.0 * 0.0) < 0.001)
    , Test.equal "negative addition" (0 - 15) (0 - 10 + (0 - 5))
    , Test.equal "negative subtraction" (0 - 5) (0 - 10 - (0 - 5))
    , Test.equal "negative multiplication pos" (0 - 50) ((0 - 10) * 5)
    , Test.equal "negative multiplication neg" 50 ((0 - 10) * (0 - 5))
    , Test.equal "negative division" (0 - 2) (Int.div (0 - 10) 5)
    , Test.equal "division negative" (0 - 2) (Int.div 10 (0 - 5))
    , Test.equal "both negative division" 2 (Int.div (0 - 10) (0 - 5))
    , Test.equal "negative mod" (0 - 1) (Int.mod (0 - 11) 5)
    , Test.ok "negative less than" (0 - 10 < 0 - 5)
    , Test.ok "negative greater than" (0 - 5 > 0 - 10)
    , Test.equal "negative abs" 42 (Int.abs (0 - 42))
    , Test.equal "zero abs" 0 (Int.abs 0)
    , Test.equal "positive abs" 42 (Int.abs 42)
    , Test.ok "negative float" (Float.abs (0.0 - 3.14 - (0 - 3.14)) < 0.001)
    , Test.ok "negative float compare" (0 - 3.14 < 0.0)
    , Test.equal "unicode length" 5 (String.length "hello")
    , Test.ok "unicode equality" ("cafe" == "cafe")
    , Test.equal "newline escape length" 1 (String.length "\n")
    , Test.equal "tab escape length" 1 (String.length "\t")
    , Test.equal "backslash escape length" 1 (String.length "\\")
    , Test.equal "double quote escape length" 1 (String.length "\"")
    , Test.equal "carriage return length" 1 (String.length "\r")
    , Test.equal "multiple escapes" 5 (String.length "a\nb\tc")
    , Test.equal "escaped in middle" 5 (String.length "he\nlo")
    , Test.equal "basic shadowing" 20 (let x = 10 in
    -- Empty string operations
    -- Zero values
    -- Negative numbers
    -- Negative float
    -- Unicode in strings
    -- Escape sequences in strings
    -- Variable shadowing
    let x = 20 in
    x)
    , Test.equal "nested shadowing" 30 (let x = 10 in
    let y = let x = 20 in
    x + x in
    x + y)
    , Test.equal "function param shadowing" 100 (let x = 10 in
    let f = x -> x * x in
    f x)
    , Test.equal "shadowing in pattern" 3 (let x = 10 in
    match (1, 2)
      when (x, y) -> x + y
    end)
    , Test.equal "multiple level shadowing" 60 (let x = 10 in
    let y = let x = 20 in
    let z = let x = 30 in
    x in
    z + x in
    x + y)
    , Test.equal "match on empty list" "empty" (match []
      when Nil -> "empty"
      when Cons _ _ -> "nonempty"
    end)
    , Test.equal "match on singleton" "one" (match [42]
      when Nil -> "none"
      when Cons _ Nil -> "one"
      when _ -> "many"
    end)
    , Test.equal "match on Nothing" 0 (match Nothing
      when Nothing -> 0
      when Just x -> x
    end)
    , Test.equal "match on Just Nothing" "just nothing" (match Just Nothing
      when Just Nothing -> "just nothing"
      when Just (Just _) -> "just just"
      when Nothing -> "nothing"
    end)
    , Test.equal "deeply nested arithmetic" 100 (10 + 10 + 10 + 10 + 10 + 50)
    , Test.equal "deeply nested function calls" 11 (let f = x -> x + 1 in
    -- Edge cases in pattern matching
    -- Deeply nested expressions
    f (f (f (f (f (f (f (f (f (f 1))))))))))
    , Test.equal "deeply nested if" 5 (if true
      then if true
        then if true
          then if true
            then if true
              then 5
              else 4
            else 3
          else 2
        else 1
      else 0)
    , Test.equal "deeply nested let" 15 (let a = 1 in
    let b = 2 in
    let c = 3 in
    let d = 4 in
    let e = 5 in
    a + b + c + d + e)
    , Test.equal "take 0 length" 0 (List.length (List.take 0 [1, 2, 3]))
    , Test.equal "take more than length" 3 (List.length (List.take 100 [1, 2, 3]))
    , Test.equal "drop 0 length" 3 (List.length (List.drop 0 [1, 2, 3]))
    , Test.equal "drop more than length" 0 (List.length (List.drop 100 [1, 2, 3]))
    , Test.equal "take negative length" 0 (List.length (List.take (0 - 1) [1, 2, 3]))
    , Test.equal "drop negative length" 3 (List.length (List.drop (0 - 1) [1, 2, 3]))
    , Test.equal "string take 0" "" (String.take 0 "hello")
    , Test.equal "string take more" "hello" (String.take 100 "hello")
    , Test.equal "string drop 0" "hello" (String.drop 0 "hello")
    , Test.equal "string drop more" "" (String.drop 100 "hello")
    , Test.equal "substring empty" "" (String.substring 0 0 "hello")
    , Test.equal "substring full" "hello" (String.substring 0 5 "hello")
    , Test.ok "singleton list head is Just" (match List.head [42]
      when Just 42 -> true
      when _ -> false
    end)
    , Test.ok "singleton list tail is empty" (match List.tail [42]
      when Just t -> List.isEmpty t
      when Nothing -> false
    end)
    , Test.equal "singleton reverse length" 1 (List.length (List.reverse [42]))
    , Test.ok "singleton map check" (match List.head (List.map (x -> x * 2) [42])
      when Just 84 -> true
      when _ -> false
    end)
    , Test.equal "single char string" 1 (String.length "x")
    , Test.ok "compare equal strings" ("hello" == "hello")
    , Test.ok "compare equal empty strings" ("" == "")
    , Test.ok "compare equal tuples" ((1, 2) == (1, 2))
    , Test.ok "compare equal unit" (() == ())
    , Test.equal "large number" 1000000000 1000000000
    , Test.equal "large arithmetic" 2000000000 (1000000000 + 1000000000)
    , Test.ok "chained and" (1 < 2 && 2 < 3 && 3 < 4)
    , Test.ok "chained or" (1 > 10 || 2 > 10 || 3 < 10)
    ]

-- Boundary values for take/drop
-- String boundary cases
-- Single element operations
-- Comparison edge cases
-- Large numbers
-- Chained comparisons
let main =
  args -> Test.run tests
