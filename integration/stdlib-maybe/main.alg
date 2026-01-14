-- Stdlib: Maybe module

-- Helper to check Maybe equality
let maybeEq = m1 m2 -> match (m1, m2)
  when (Nothing, Nothing) -> true
  when (Just a, Just b) -> a == b
  when _ -> false
end

-- do-notation values (must be defined before use in tests)
let doSuccess = do[Maybe]
  x <- Just 10
  y <- Just 20
  z <- Just 30
  Just (x + y + z)
end

let doShortCircuit = do[Maybe]
  x <- Just 10
  y <- Nothing
  z <- Just 30
  Just (x + y + z)
end

let tests =
  let just42 = Just 42 in
  let nothing = Nothing in
  [ -- isJust / isNothing
    Test.ok "isJust Just" (Maybe.isJust just42)
  , Test.ok "isJust Nothing" (Bool.negate (Maybe.isJust nothing))
  , Test.ok "isNothing Nothing" (Maybe.isNothing nothing)
  , Test.ok "isNothing Just" (Bool.negate (Maybe.isNothing just42))
    -- map
  , Test.ok "map Just" (maybeEq (Maybe.map (x -> x * 2) just42) (Just 84))
  , Test.ok "map Nothing" (Maybe.isNothing (Maybe.map (x -> x * 2) nothing))
  , Test.ok "map composition" (maybeEq (Maybe.map (x -> x + 2) (Maybe.map (x -> x * 2) just42)) (Just 86))
    -- flatMap
  , Test.ok "flatMap Just -> Just" (maybeEq (Maybe.flatMap (x -> Just (x * 2)) just42) (Just 84))
  , Test.ok "flatMap Just -> Nothing" (Maybe.isNothing (Maybe.flatMap (x -> Nothing) just42))
  , Test.ok "flatMap Nothing" (Maybe.isNothing (Maybe.flatMap (x -> Just (x * 2)) nothing))
  , Test.ok "flatMap chain" (maybeEq (Maybe.flatMap (x -> if x > 5 then Nothing else Just (x * 2)) (Just 5)) (Just 10))
  , Test.ok "flatMap chain fail" (Maybe.isNothing (Maybe.flatMap (x -> if x > 5 then Nothing else Just (x * 2)) (Just 10)))
    -- pure
  , Test.ok "pure" (maybeEq (Maybe.pure 42) (Just 42))
  , Test.ok "pure string" (match Maybe.pure "hello" when Just "hello" -> true when _ -> false end)
    -- withDefault
  , Test.equal "withDefault Just" 42 (Maybe.withDefault 0 just42)
  , Test.equal "withDefault Nothing" 0 (Maybe.withDefault 0 nothing)
  , Test.equal "withDefault string" "default" (Maybe.withDefault "default" Nothing)
    -- toList
  , Test.equal "toList Just length" 1 (List.length (Maybe.toList just42))
  , Test.equal "toList Nothing length" 0 (List.length (Maybe.toList nothing))
  , Test.ok "toList Just value" (match List.head (Maybe.toList just42) when Just 42 -> true when _ -> false end)
    -- Equality via pattern matching
  , Test.ok "Just == Just same" (match (Just 42, Just 42) when (Just a, Just b) -> a == b when _ -> false end)
  , Test.ok "Just != Just diff" (match (Just 42, Just 43) when (Just a, Just b) -> a != b when _ -> false end)
  , Test.ok "Nothing == Nothing" (match (Nothing, Nothing) when (Nothing, Nothing) -> true when _ -> false end)
  , Test.ok "Just != Nothing" (match (Just 42, Nothing) when (Just _, Nothing) -> true when _ -> false end)
  , Test.ok "Nothing != Just" (match (Nothing, Just 42) when (Nothing, Just _) -> true when _ -> false end)
    -- Chaining
  , Test.ok "chain multiple maps" (maybeEq (Just 10 |> Maybe.map (x -> x + 2) |> Maybe.map (x -> x * 2) |> Maybe.map (x -> x + 4) |> Maybe.map (x -> x * 3)) (Just 168))
  , Test.ok "chain with Nothing" (Maybe.isNothing (Just 10 |> Maybe.flatMap (x -> Just (x + 2)) |> Maybe.flatMap (x -> Nothing) |> Maybe.flatMap (x -> Just (x + 4))))
    -- Nested Maybe
  , Test.ok "nested Just Just" (match Just (Just 42) when Just (Just 42) -> true when _ -> false end)
  , Test.ok "nested Just Nothing" (match Just Nothing when Just Nothing -> true when _ -> false end)
  , Test.ok "flatten nested" (maybeEq (Maybe.flatMap (x -> x) (Just (Just 42))) (Just 42))
  , Test.ok "flatten nested Nothing" (Maybe.isNothing (Maybe.flatMap (x -> x) (Just Nothing)))
    -- Maybe with other types (check structure, not equality)
  , Test.ok "Maybe string" (match Just "hello" when Just "hello" -> true when _ -> false end)
  , Test.ok "Maybe list" (match Just [1, 2, 3] when Just xs -> List.length xs == 3 when _ -> false end)
  , Test.ok "Maybe tuple" (match Just (1, 2) when Just (a, b) -> a == 1 && b == 2 when _ -> false end)
    -- Common patterns
  , Test.ok "safe division" (maybeEq (let safeDivide = a b -> if b == 0 then Nothing else Just (Int.div a b) in safeDivide 10 2) (Just 5))
  , Test.ok "safe division by zero" (Maybe.isNothing (let safeDivide = a b -> if b == 0 then Nothing else Just (Int.div a b) in safeDivide 10 0))
    -- do-notation with Maybe
  , Test.ok "do notation" (maybeEq doSuccess (Just 60))
  , Test.ok "do notation short circuit" (Maybe.isNothing doShortCircuit)
  ]

let main = args -> Test.run tests
