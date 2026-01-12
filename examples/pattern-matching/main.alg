-- =============================================================================
-- Pattern Matching: All Pattern Types, Guards, Nested, Or-Patterns
-- =============================================================================

-- =============================================================================
-- ADT Pattern Matching
-- =============================================================================

type Color = Red | Green | Blue | RGB int int int

let testADTPatterns =
  let colorValue = c ->
    match c
      when Red -> 1
      when Green -> 2
      when Blue -> 3
      when RGB r g b -> r + g + b
    end
  in
  let a = colorValue Red in                  -- 1
  let b = colorValue Green in                -- 2
  let c = colorValue Blue in                 -- 3
  let d = colorValue (RGB 10 20 30) in       -- 60
  a + b + c + d                              -- 66

-- =============================================================================
-- Literal Patterns
-- =============================================================================

let testLiteralPatterns =
  let describeInt = n ->
    match n
      when 0 -> "zero"
      when 1 -> "one"
      when 2 -> "two"
      when _ -> "many"
    end
  in
  let lenZero = String.length (describeInt 0) in   -- 4
  let lenOne = String.length (describeInt 1) in    -- 3
  let lenTwo = String.length (describeInt 2) in    -- 3
  let lenMany = String.length (describeInt 99) in  -- 4

  let describeBool = b ->
    match b
      when true -> "yes"
      when false -> "no"
    end
  in
  let lenYes = String.length (describeBool true) in   -- 3
  let lenNo = String.length (describeBool false) in   -- 2

  lenZero + lenOne + lenTwo + lenMany + lenYes + lenNo  -- 19

-- =============================================================================
-- Variable Patterns
-- =============================================================================

let testVariablePatterns =
  -- Simple variable binding
  let extractValue = opt ->
    match opt
      when Nothing -> 0
      when Just x -> x
    end
  in
  let a = extractValue Nothing in            -- 0
  let b = extractValue (Just 42) in          -- 42

  -- Multiple variable extraction
  let sumTuple = t ->
    match t
      when (a, b) -> a + b
    end
  in
  let c = sumTuple (10, 20) in               -- 30

  -- Nested variable extraction
  let extractNested = opt ->
    match opt
      when Nothing -> 0
      when Just (a, b) -> a * b
    end
  in
  let d = extractNested (Just (3, 4)) in     -- 12

  a + b + c + d                              -- 84

-- =============================================================================
-- Wildcard Patterns
-- =============================================================================

let testWildcardPatterns =
  -- Basic wildcard
  let isJust_ = opt ->
    match opt
      when Nothing -> 0
      when Just _ -> 1
    end
  in
  let a = isJust_ Nothing in                 -- 0
  let b = isJust_ (Just "ignored") in        -- 1

  -- Wildcard in tuple
  let getFirst = t ->
    match t
      when (x, _) -> x
    end
  in
  let c = getFirst (42, "ignored") in        -- 42

  -- Wildcard in list
  let headOrZero = xs ->
    match xs
      when Nil -> 0
      when Cons x _ -> x
    end
  in
  let d = headOrZero [10, 20, 30] in         -- 10
  let e = headOrZero Nil in                  -- 0

  a + b + c + d + e                          -- 53

-- =============================================================================
-- Guard Patterns
-- =============================================================================

let testGuardPatterns =
  -- Basic guards
  let classify = n ->
    match n
      when x if x < 0 -> -1
      when x if x == 0 -> 0
      when x if x < 10 -> 1
      when x if x < 100 -> 2
      when _ -> 3
    end
  in
  let a = classify (-5) in                   -- -1
  let b = classify 0 in                      -- 0
  let c = classify 5 in                      -- 1
  let d = classify 50 in                     -- 2
  let e = classify 500 in                    -- 3

  -- Guards with destructuring
  let safeDiv = pair ->
    match pair
      when (_, 0) -> Nothing
      when (a, b) if b != 0 -> Just (a / b)
      when _ -> Nothing
    end
  in
  let f = match safeDiv (10, 2) when Just x -> x when Nothing -> -1 end in  -- 5
  let g = match safeDiv (10, 0) when Just x -> x when Nothing -> -1 end in  -- -1

  -- Chained guards
  let grade = score ->
    match score
      when n if n >= 90 -> 4
      when n if n >= 80 -> 3
      when n if n >= 70 -> 2
      when n if n >= 60 -> 1
      when _ -> 0
    end
  in
  let h = grade 95 in                        -- 4
  let i = grade 75 in                        -- 2
  let j = grade 55 in                        -- 0

  a + b + c + d + e + f + g + h + i + j      -- 15

-- =============================================================================
-- As-Patterns
-- =============================================================================

let testAsPatterns =
  -- Simple as-pattern
  let headAndAll = xs ->
    match xs
      when Nil -> Nothing
      when Cons x _ as whole -> Just (x, whole)
    end
  in
  let result = headAndAll [1, 2, 3] in
  let a = match result
    when Nothing -> 0
    when Just (h, list) -> h + List.length list
  end in                                     -- 1 + 3 = 4

  -- As-pattern with nested destructuring
  let firstTwoAndAll = xs ->
    match xs
      when Cons a (Cons b rest) as whole -> Just (a, b, whole)
      when _ -> Nothing
    end
  in
  let result2 = firstTwoAndAll [10, 20, 30, 40] in
  let b = match result2
    when Nothing -> 0
    when Just (x, y, list) -> x + y + List.length list
  end in                                     -- 10 + 20 + 4 = 34

  a + b                                      -- 38

-- =============================================================================
-- Tuple Patterns
-- =============================================================================

let testTuplePatterns =
  -- Pair matching
  let swapPair = p ->
    match p
      when (a, b) -> (b, a)
    end
  in
  let (x, y) = swapPair (1, 2) in
  let a = x + y in                           -- 3

  -- Triple matching
  let sumTriple = t ->
    match t
      when (a, b, c) -> a + b + c
    end
  in
  let b = sumTriple (10, 20, 30) in          -- 60

  -- Nested tuple matching
  let flattenPairs = t ->
    match t
      when ((a, b), (c, d)) -> a + b + c + d
    end
  in
  let c = flattenPairs ((1, 2), (3, 4)) in   -- 10

  -- Mixed tuple matching
  let extractMiddle = t ->
    match t
      when (_, x, _) -> x
    end
  in
  let d = extractMiddle ("a", 42, "b") in    -- 42

  a + b + c + d                              -- 115

-- =============================================================================
-- List Patterns
-- =============================================================================

let testListPatterns =
  -- Empty list
  let isEmpty_ = xs ->
    match xs
      when Nil -> 1
      when Cons _ _ -> 0
    end
  in
  let a = isEmpty_ Nil in                    -- 1
  let b = isEmpty_ [1, 2] in                 -- 0

  -- Single element list
  let isSingleton = xs ->
    match xs
      when Cons _ Nil -> 1
      when _ -> 0
    end
  in
  let c = isSingleton [42] in                -- 1
  let d = isSingleton [1, 2] in              -- 0

  -- First two elements
  let sumFirstTwo = xs ->
    match xs
      when Cons a (Cons b _) -> a + b
      when Cons a Nil -> a
      when Nil -> 0
    end
  in
  let e = sumFirstTwo [10, 20, 30] in        -- 30
  let f = sumFirstTwo [5] in                 -- 5
  let g = sumFirstTwo Nil in                 -- 0

  -- Cons pattern with ::
  let describeList = xs ->
    match xs
      when Nil -> 0
      when Cons x Nil -> x
      when Cons x (Cons y Nil) -> x + y
      when Cons x (Cons y (Cons z _)) -> x + y + z
    end
  in
  let h = describeList Nil in                -- 0
  let i = describeList [5] in                -- 5
  let j = describeList [3, 4] in             -- 7
  let k = describeList [1, 2, 3, 4, 5] in    -- 6

  a + b + c + d + e + f + g + h + i + j + k  -- 55

-- =============================================================================
-- Record Patterns
-- =============================================================================

let testRecordPatterns =
  -- Simple record matching
  let getX = p ->
    match p
      when { x = x, y = _ } -> x
    end
  in
  let a = getX { x = 10, y = 20 } in         -- 10

  -- Multiple field extraction
  let sumCoords = p ->
    match p
      when { x = x, y = y } -> x + y
    end
  in
  let b = sumCoords { x = 3, y = 4 } in      -- 7

  -- Nested record matching
  let getInnerX = p ->
    match p
      when { inner = { x = x, y = _ }, value = _ } -> x
    end
  in
  let c = getInnerX { inner = { x = 100, y = 200 }, value = 0 } in  -- 100

  a + b + c                                  -- 117

-- =============================================================================
-- Or-Patterns
-- =============================================================================

let testOrPatterns =
  -- Simple or-pattern
  let isRedOrBlue = c ->
    match c
      when Red | Blue -> 1
      when _ -> 0
    end
  in
  let a = isRedOrBlue Red in                 -- 1
  let b = isRedOrBlue Blue in                -- 1
  let c = isRedOrBlue Green in               -- 0

  -- Or-pattern with Maybe
  let isNothingOrZero = opt ->
    match opt
      when Nothing | Just 0 -> 1
      when _ -> 0
    end
  in
  let d = isNothingOrZero Nothing in         -- 1
  let e = isNothingOrZero (Just 0) in        -- 1
  let f = isNothingOrZero (Just 5) in        -- 0

  -- Or-pattern with nested constructors
  let isSpecial = opt ->
    match opt
      when Nothing | Just Nothing -> 1
      when _ -> 0
    end
  in
  let g = isSpecial Nothing in               -- 1
  let h = isSpecial (Just Nothing) in        -- 1
  let i = isSpecial (Just (Just 1)) in       -- 0

  a + b + c + d + e + f + g + h + i          -- 6

-- =============================================================================
-- Nested Patterns
-- =============================================================================

let testNestedPatterns =
  -- Deeply nested ADT
  let extractDeep = opt ->
    match opt
      when Just (Just (Just x)) -> x
      when Just (Just Nothing) -> -1
      when Just Nothing -> -2
      when Nothing -> -3
    end
  in
  let a = extractDeep (Just (Just (Just 42))) in    -- 42
  let b = extractDeep (Just (Just Nothing)) in      -- -1
  let c = extractDeep (Just Nothing) in             -- -2
  let d = extractDeep Nothing in                    -- -3

  -- Nested list pattern
  let sumFirstOfFirst = xss ->
    match xss
      when Cons (Cons x _) _ -> x
      when _ -> 0
    end
  in
  let e = sumFirstOfFirst [[10, 20], [30, 40]] in   -- 10
  let f = sumFirstOfFirst [Nil, [1, 2]] in          -- 0

  -- Nested tuple in list
  let sumPairs = xs ->
    match xs
      when Cons (a, b) rest ->
        a + b + (match rest
          when Cons (c, d) _ -> c + d
          when _ -> 0
        end)
      when _ -> 0
    end
  in
  let g = sumPairs [(1, 2), (3, 4), (5, 6)] in      -- 10

  a + b + c + d + e + f + g                  -- 56

-- =============================================================================
-- Match Expression as Value
-- =============================================================================

let testMatchAsValue =
  -- Match in let binding
  let x = match Just 10
    when Nothing -> 0
    when Just n -> n * 2
  end in                                     -- 20

  -- Match in function argument
  let result = List.map (n ->
    match n
      when 0 -> 100
      when _ -> n
    end
  ) [0, 1, 2, 0, 3] in
  let y = List.foldl (acc x -> acc + x) 0 result in  -- 206

  -- Nested match expressions
  let z = match Just (Just 5)
    when Just inner -> match inner
      when Just n -> n * 10
      when Nothing -> 0
    end
    when Nothing -> -1
  end in                                     -- 50

  x + y + z                                  -- 276

-- =============================================================================
-- Final Result
-- =============================================================================

let result =
  testADTPatterns +                          -- 66
  testLiteralPatterns +                      -- 19
  testVariablePatterns +                     -- 84
  testWildcardPatterns +                     -- 53
  testGuardPatterns +                        -- 15
  testAsPatterns +                           -- 38
  testTuplePatterns +                        -- 115
  testListPatterns +                         -- 55
  testRecordPatterns +                       -- 117
  testOrPatterns +                           -- 6
  testNestedPatterns +                       -- 56
  testMatchAsValue                           -- 276
                                             -- Total: 900

result
