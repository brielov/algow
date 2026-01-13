-- =============================================================================
-- Basics: Literals, Operators, Strings, Booleans, Tuples, Basic Operations
-- =============================================================================

-- =============================================================================
-- Integer Literals and Arithmetic
-- =============================================================================

let testIntegers =
  let a = 42 in
  let b = -17 in
  let c = 0 in
  let sum = a + b + c in           -- 25
  let diff = a - b in              -- 59
  let prod = 6 * 7 in              -- 42
  let quot = Int.div 100 3 in      -- 33 (integer division)
  let neg = 0 - 5 in               -- -5
  sum + diff + prod + quot + neg   -- 25 + 59 + 42 + 33 + (-5) = 154

-- =============================================================================
-- Comparison Operators
-- =============================================================================

let testComparisons =
  let a = if 5 < 10 then 1 else 0 in         -- 1
  let b = if 10 <= 10 then 1 else 0 in       -- 1
  let c = if 15 > 10 then 1 else 0 in        -- 1
  let d = if 10 >= 10 then 1 else 0 in       -- 1
  let e = if 5 == 5 then 1 else 0 in         -- 1
  let f = if 5 != 6 then 1 else 0 in         -- 1
  let g = if 5 == 6 then 1 else 0 in         -- 0
  let h = if 10 < 5 then 1 else 0 in         -- 0
  a + b + c + d + e + f + g + h              -- 6

-- =============================================================================
-- Boolean Operations
-- =============================================================================

let testBooleans =
  let t = true in
  let f = false in
  let a = if t && t then 1 else 0 in         -- 1
  let b = if t && f then 1 else 0 in         -- 0
  let c = if f && t then 1 else 0 in         -- 0
  let d = if f && f then 1 else 0 in         -- 0
  let e = if t || t then 1 else 0 in         -- 1
  let f_ = if t || f then 1 else 0 in        -- 1
  let g = if f || t then 1 else 0 in         -- 1
  let h = if f || f then 1 else 0 in         -- 0
  a + b + c + d + e + f_ + g + h             -- 4

-- =============================================================================
-- Short-circuit Evaluation
-- =============================================================================

let testShortCircuit =
  -- && should not evaluate second arg if first is false
  let x = 0 in
  let a = if false && (Int.div 1 x == 0) then 1 else 0 in  -- 0 (doesn't crash)
  -- || should not evaluate second arg if first is true
  let b = if true || (Int.div 1 x == 0) then 1 else 0 in   -- 1 (doesn't crash)
  a + b                                               -- 1

-- =============================================================================
-- String Literals and Operations
-- =============================================================================

let testStrings =
  let s1 = "hello" in
  let s2 = "world" in
  let a = String.length s1 in                -- 5
  let b = String.length s2 in                -- 5
  let s3 = String.concat s1 " " in
  let s4 = String.concat s3 s2 in            -- "hello world"
  let c = String.length s4 in                -- 11
  let d = if String.isEmpty "" then 1 else 0 in      -- 1
  let e = if String.isEmpty "x" then 1 else 0 in     -- 0
  let f = if String.eq s1 "hello" then 1 else 0 in   -- 1
  let g = if String.eq s1 "Hello" then 1 else 0 in   -- 0
  a + b + c + d + e + f + g                  -- 23

-- =============================================================================
-- String Manipulation
-- =============================================================================

let testStringManip =
  let s = "hello world" in
  let a = String.length (String.take 5 s) in         -- 5
  let b = String.length (String.drop 6 s) in         -- 5
  let sub = String.substring 0 5 s in
  let c = if String.eq sub "hello" then 1 else 0 in  -- 1
  let upper = String.toUpper "abc" in
  let d = if String.eq upper "ABC" then 1 else 0 in  -- 1
  let lower = String.toLower "XYZ" in
  let e = if String.eq lower "xyz" then 1 else 0 in  -- 1
  let trimmed = String.trim "  hi  " in
  let f = if String.eq trimmed "hi" then 1 else 0 in -- 1
  let g = if String.contains "ell" "hello" then 1 else 0 in    -- 1
  let h = if String.startsWith "he" "hello" then 1 else 0 in   -- 1
  let i = if String.endsWith "lo" "hello" then 1 else 0 in     -- 1
  a + b + c + d + e + f + g + h + i          -- 17

-- =============================================================================
-- Tuple Operations
-- =============================================================================

let testTuples =
  let pair = (1, 2) in
  let triple = (1, 2, 3) in
  let quad = (1, 2, 3, 4) in
  let nested = ((1, 2), (3, 4)) in
  let a = match pair when (x, y) -> x + y end in          -- 3
  let b = match triple when (x, y, z) -> x + y + z end in -- 6
  let c = match quad when (a, b, c, d) -> a + b + c + d end in  -- 10
  let d = match nested when ((a, b), (c, d)) -> a + b + c + d end in  -- 10
  -- Mixed tuples
  let mixed = (42, "hello", true) in
  let e = match mixed when (n, s, b) ->
    if b then n + String.length s else 0
  end in  -- 47
  a + b + c + d + e                          -- 76

-- =============================================================================
-- Let Bindings and Shadowing
-- =============================================================================

let testBindings =
  let x = 1 in
  let y = 2 in
  let z = x + y in                           -- 3
  let x = 10 in                              -- shadow x
  let w = x + y in                           -- 12
  let result = z + w in                      -- 15
  -- Nested let
  let nested =
    let a = 1 in
    let b = 2 in
    let c =
      let a = 10 in                          -- shadow a
      a + b                                  -- 12
    in
    a + b + c                                -- 1 + 2 + 12 = 15
  in
  result + nested                            -- 30

-- =============================================================================
-- Conditionals
-- =============================================================================

let testConditionals =
  let a = if true then 1 else 2 in           -- 1
  let b = if false then 1 else 2 in          -- 2
  let c = if 5 > 3 then 10 else 20 in        -- 10
  let d = if 5 < 3 then 10 else 20 in        -- 20
  -- Nested if
  let e = if true then
    if false then 1 else 2
  else
    if true then 3 else 4
  in                                         -- 2
  -- Chained conditions
  let classify = n ->
    if n < 0 then -1
    else if n == 0 then 0
    else 1
  in
  let f = classify (-5) in                   -- -1
  let g = classify 0 in                      -- 0
  let h = classify 10 in                     -- 1
  a + b + c + d + e + f + g + h              -- 35

-- =============================================================================
-- Maybe Operations (from prelude)
-- =============================================================================

let testMaybe =
  let a = match Just 42 when Just x -> x when Nothing -> 0 end in  -- 42
  let b = match Nothing when Just x -> x when Nothing -> 0 end in  -- 0
  let c = if Maybe.isJust (Just 1) then 1 else 0 in                -- 1
  let d = if Maybe.isJust Nothing then 1 else 0 in                 -- 0
  let e = if Maybe.isNothing Nothing then 1 else 0 in              -- 1
  let f = if Maybe.isNothing (Just 1) then 1 else 0 in             -- 0
  let g = Maybe.withDefault 99 Nothing in                          -- 99
  let h = Maybe.withDefault 99 (Just 42) in                        -- 42
  let mapped = Maybe.map (x -> x * 2) (Just 10) in
  let i = Maybe.withDefault 0 mapped in                            -- 20
  a + b + c + d + e + f + g + h + i          -- 206

-- =============================================================================
-- Either Operations (from prelude)
-- =============================================================================

let testEither =
  let a = match Right 42 when Right x -> x when Left e -> 0 end in   -- 42
  let b = match Left "error" when Right x -> x when Left e -> -1 end in  -- -1
  let c = if Either.isRight (Right 1) then 1 else 0 in               -- 1
  let d = if Either.isLeft (Left "x") then 1 else 0 in               -- 1
  let e = Either.withDefault 99 (Right 42) in                        -- 42
  let f = Either.withDefault 99 (Left "err") in                      -- 99
  let mapped = Either.map (x -> x + 1) (Right 10) in
  let g = Either.withDefault 0 mapped in                             -- 11
  a + b + c + d + e + f + g                  -- 195

-- =============================================================================
-- List Basics (from prelude)
-- =============================================================================

let testLists =
  let xs = [1, 2, 3, 4, 5] in
  let a = List.length xs in                  -- 5
  let b = List.length Nil in                 -- 0
  let c = if List.isEmpty Nil then 1 else 0 in    -- 1
  let d = if List.isEmpty xs then 1 else 0 in     -- 0
  let e = match List.head xs when Just x -> x when Nothing -> 0 end in  -- 1
  let f = match List.tail xs when Just t -> List.length t when Nothing -> 0 end in  -- 4
  let sum = List.foldl (acc x -> acc + x) 0 xs in  -- 15
  let prod = List.foldl (acc x -> acc * x) 1 xs in -- 120
  a + b + c + d + e + f + sum + prod         -- 146

-- =============================================================================
-- List Operations
-- =============================================================================

let testListOps =
  let xs = [1, 2, 3, 4, 5] in
  let doubled = List.map (x -> x * 2) xs in
  let a = List.foldl (acc x -> acc + x) 0 doubled in   -- 30
  let evens = List.filter (x -> Int.mod x 2 == 0) xs in
  let b = List.length evens in                          -- 2
  let ys = [6, 7, 8] in
  let zs = List.append xs ys in
  let c = List.length zs in                             -- 8
  let rev = List.reverse xs in
  let d = match List.head rev when Just x -> x when Nothing -> 0 end in  -- 5
  let taken = List.take 3 xs in
  let e = List.length taken in                          -- 3
  let dropped = List.drop 2 xs in
  let f = List.length dropped in                        -- 3
  a + b + c + d + e + f                      -- 51

-- =============================================================================
-- Cons Operator and List Construction
-- =============================================================================

let testCons =
  let xs = 1 :: 2 :: 3 :: Nil in
  let a = List.length xs in                  -- 3
  let ys = 0 :: xs in
  let b = List.length ys in                  -- 4
  let c = match ys when Cons x rest -> x when Nil -> -1 end in  -- 0
  let zs = [1, 2] in
  let ws = 0 :: zs in
  let d = List.foldl (acc x -> acc + x) 0 ws in  -- 3
  a + b + c + d                              -- 10

-- =============================================================================
-- Character Operations
-- =============================================================================

let testChars =
  let c1 = 'a' in
  let c2 = 'Z' in
  let c3 = '5' in
  let c4 = ' ' in
  let a = if Char.isAlpha c1 then 1 else 0 in        -- 1
  let b = if Char.isAlpha c3 then 1 else 0 in        -- 0
  let c = if Char.isDigit c3 then 1 else 0 in        -- 1
  let d = if Char.isDigit c1 then 1 else 0 in        -- 0
  let e = if Char.isUpper c2 then 1 else 0 in        -- 1
  let f = if Char.isLower c1 then 1 else 0 in        -- 1
  let g = if Char.isSpace c4 then 1 else 0 in        -- 1
  let h = Char.toInt c1 in                           -- 97
  a + b + c + d + e + f + g + h              -- 102

-- =============================================================================
-- Integer Conversion
-- =============================================================================

let testIntConversion =
  let s1 = Int.toString 42 in
  let a = String.length s1 in                -- 2
  let s2 = Int.toString (-17) in
  let b = String.length s2 in                -- 3
  let parsed = Int.fromString "123" in
  let c = match parsed when Just n -> n when Nothing -> 0 end in  -- 123
  let invalid = Int.fromString "abc" in
  let d = match invalid when Just n -> n when Nothing -> -1 end in  -- -1
  let e = Int.abs (-50) in                   -- 50
  a + b + c + d + e                          -- 177

-- =============================================================================
-- Final Result
-- =============================================================================

let result =
  testIntegers +           -- 154
  testComparisons +        -- 6
  testBooleans +           -- 4
  testShortCircuit +       -- 1
  testStrings +            -- 23
  testStringManip +        -- 17
  testTuples +             -- 76
  testBindings +           -- 30
  testConditionals +       -- 35
  testMaybe +              -- 206
  testEither +             -- 195
  testLists +              -- 146
  testListOps +            -- 51
  testCons +               -- 10
  testChars +              -- 102
  testIntConversion        -- 177
                           -- Total: 1233

let main = args -> IO.printLine (Int.toString result)
