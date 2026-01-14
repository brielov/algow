-- Tuples: Creation, Indexing, Patterns, Nested, Large

let tests =
  let pair = (10, 20) in
  let triple = (1, 2, 3) in
  let quad = (1, 2, 3, 4) in
  let quint = (1, 2, 3, 4, 5) in
  let mixed = (42, "hello", true, 3.14) in
  let nested = ((1, 2), (3, 4)) in
  let deepNested = (((1, 2), (3, 4)), ((5, 6), (7, 8))) in
  let withList = ([1, 2], [3, 4, 5]) in
  let withMaybe = (Just 10, Nothing, Just 30) in
  let swap = p -> match p when (a, b) -> (b, a) end in
  let fst_ = p -> match p when (a, _) -> a end in
  let snd_ = p -> match p when (_, b) -> b end in
  let sumPair = p -> match p when (a, b) -> a + b end in
  let sumTriple = t -> match t when (a, b, c) -> a + b + c end in
  let sumQuad = q -> match q when (a, b, c, d) -> a + b + c + d end in
  let first3 = t -> match t when (a, _, _) -> a end in
  let second3 = t -> match t when (_, b, _) -> b end in
  let third3 = t -> match t when (_, _, c) -> c end in
  let rotateLeft = t -> match t when (a, b, c) -> (b, c, a) end in
  let rotateRight = t -> match t when (a, b, c) -> (c, a, b) end in
  let flattenPairs = p -> match p when ((a, b), (c, d)) -> (a, b, c, d) end in
  let mapPair = f p -> match p when (a, b) -> (f a, f b) end in
  let zipPair = p1 p2 -> match (p1, p2) when ((a, b), (c, d)) -> ((a, c), (b, d)) end in
  [ -- Basic creation and access via patterns
    Test.equal "pair fst" 10 (fst_ pair)
  , Test.equal "pair snd" 20 (snd_ pair)
  , Test.equal "triple first" 1 (first3 triple)
  , Test.equal "triple second" 2 (second3 triple)
  , Test.equal "triple third" 3 (third3 triple)
    -- Indexing via .0, .1, .2
  , Test.equal "pair.0" 10 pair.0
  , Test.equal "pair.1" 20 pair.1
  , Test.equal "triple.0" 1 triple.0
  , Test.equal "triple.1" 2 triple.1
  , Test.equal "triple.2" 3 triple.2
  , Test.equal "quad.0" 1 quad.0
  , Test.equal "quad.1" 2 quad.1
  , Test.equal "quad.2" 3 quad.2
  , Test.equal "quad.3" 4 quad.3
  , Test.equal "quint.4" 5 quint.4
    -- Mixed type tuple indexing
  , Test.equal "mixed.0" 42 mixed.0
  , Test.equal "mixed.1" "hello" mixed.1
  , Test.equal "mixed.2" true mixed.2
  , Test.equal "mixed.3" 3.14 mixed.3
    -- Nested tuple indexing
  , Test.equal "nested.0" (1, 2) nested.0
  , Test.equal "nested.1" (3, 4) nested.1
  , Test.equal "nested.0.0" 1 (nested.0).0
  , Test.equal "nested.0.1" 2 (nested.0).1
  , Test.equal "nested.1.0" 3 (nested.1).0
  , Test.equal "nested.1.1" 4 (nested.1).1
    -- Deep nested indexing
  , Test.equal "deep.0.0.0" 1 ((deepNested.0).0).0
  , Test.equal "deep.1.1.1" 8 ((deepNested.1).1).1
    -- Tuple with collections
  , Test.equal "withList.0 length" 2 (List.length withList.0)
  , Test.equal "withList.1 length" 3 (List.length withList.1)
  , Test.equal "withMaybe.0" (Just 10) withMaybe.0
  , Test.equal "withMaybe.1" Nothing withMaybe.1
  , Test.equal "withMaybe.2" (Just 30) withMaybe.2
    -- Swap operation
  , Test.equal "swap" (20, 10) (swap pair)
  , Test.equal "swap swap identity" pair (swap (swap pair))
    -- Sum operations
  , Test.equal "sumPair" 30 (sumPair pair)
  , Test.equal "sumTriple" 6 (sumTriple triple)
  , Test.equal "sumQuad" 10 (sumQuad quad)
    -- Rotation
  , Test.equal "rotateLeft" (2, 3, 1) (rotateLeft triple)
  , Test.equal "rotateRight" (3, 1, 2) (rotateRight triple)
  , Test.equal "rotateLeft 3x" triple (rotateLeft (rotateLeft (rotateLeft triple)))
  , Test.equal "rotateRight 3x" triple (rotateRight (rotateRight (rotateRight triple)))
    -- Flatten
  , Test.equal "flattenPairs" (1, 2, 3, 4) (flattenPairs nested)
    -- Map over pair
  , Test.equal "mapPair double" (20, 40) (mapPair (x -> x * 2) pair)
  , Test.equal "mapPair negate" ((-10), (-20)) (mapPair (x -> 0 - x) pair)
    -- Zip pairs
  , Test.equal "zipPair" ((1, 3), (2, 4)) (zipPair (1, 2) (3, 4))
    -- Pattern matching
  , Test.equal "match pair" 30 (match pair when (x, y) -> x + y end)
  , Test.equal "match triple" 6 (match triple when (x, y, z) -> x + y + z end)
  , Test.equal "match quad" 10 (match quad when (a, b, c, d) -> a + b + c + d end)
  , Test.equal "match nested" 10 (match nested when ((a, b), (c, d)) -> a + b + c + d end)
  , Test.equal "match deep" 36 (match deepNested when (((a, b), (c, d)), ((e, f), (g, h))) -> a + b + c + d + e + f + g + h end)
    -- Wildcards in patterns
  , Test.equal "wildcard first" 20 (match pair when (_, y) -> y end)
  , Test.equal "wildcard second" 10 (match pair when (x, _) -> x end)
  , Test.equal "wildcard middle" 4 (match (1, 2, 3) when (a, _, c) -> a + c end)
    -- Tuple equality
  , Test.ok "pair equal" (pair == (10, 20))
  , Test.ok "pair not equal" (pair != (10, 21))
  , Test.ok "triple equal" (triple == (1, 2, 3))
  , Test.ok "nested equal" (nested == ((1, 2), (3, 4)))
    -- Tuples in lists
  , Test.equal "list of pairs sum" 21 (List.foldl (acc p -> acc + p.0 + p.1) 0 [(1, 2), (3, 4), (5, 6)])
    -- Tuples as function return
  , Test.equal "return tuple" (100, 200) (let makePair = x -> (x, x * 2) in makePair 100)
    -- Unit tuple
  , Test.equal "unit" () ()
  , Test.ok "unit equal" (() == ())
    -- Large tuple
  , Test.equal "6-tuple.5" 6 ((1, 2, 3, 4, 5, 6).5)
  , Test.equal "7-tuple sum" 28 (match (1, 2, 3, 4, 5, 6, 7) when (a, b, c, d, e, f, g) -> a + b + c + d + e + f + g end)
    -- Tuple in Maybe
  , Test.equal "Just tuple" (Just (1, 2)) (Just (1, 2))
  , Test.equal "extract from Just tuple" 3 (match Just (1, 2) when Just (a, b) -> a + b when Nothing -> 0 end)
    -- Tuple in Either
  , Test.equal "Right tuple" (Right (10, 20)) (Right (10, 20))
  , Test.equal "extract from Right tuple" 30 (match Right (10, 20) when Right (a, b) -> a + b when Left _ -> 0 end)
  ]

let main = args -> Test.run tests
