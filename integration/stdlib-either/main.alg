-- Stdlib: Either module

-- Helper to check Either equality
let eitherEq = e1 e2 -> match (e1, e2)
  when (Right a, Right b) -> a == b
  when (Left a, Left b) -> a == b
  when _ -> false
end

let tests =
  let right42 = Right 42 in
  let leftErr = Left "error" in
  [ -- isLeft / isRight
    Test.ok "isRight Right" (Either.isRight right42)
  , Test.ok "isRight Left" (Bool.negate (Either.isRight leftErr))
  , Test.ok "isLeft Left" (Either.isLeft leftErr)
  , Test.ok "isLeft Right" (Bool.negate (Either.isLeft right42))
    -- map (maps over Right)
  , Test.ok "map Right" (eitherEq (Either.map (x -> x * 2) right42) (Right 84))
  , Test.ok "map Left unchanged" (Either.isLeft (Either.map (x -> x * 2) leftErr))
  , Test.ok "map composition" (eitherEq (Either.map (x -> x + 2) (Either.map (x -> x * 2) right42)) (Right 86))
    -- mapLeft (maps over Left)
  , Test.ok "mapLeft Left" (match Either.mapLeft String.toUpper leftErr when Left "ERROR" -> true when _ -> false end)
  , Test.ok "mapLeft Right unchanged" (eitherEq (Either.mapLeft String.toUpper right42) (Right 42))
    -- flatMap
  , Test.ok "flatMap Right -> Right" (eitherEq (Either.flatMap (x -> Right (x * 2)) right42) (Right 84))
  , Test.ok "flatMap Right -> Left" (Either.isLeft (Either.flatMap (x -> Left "failed") right42))
  , Test.ok "flatMap Left" (Either.isLeft (Either.flatMap (x -> Right (x * 2)) leftErr))
  , Test.ok "flatMap chain" (eitherEq (Either.flatMap (x -> if x > 5 then Left "too big" else Right (x * 2)) (Right 5)) (Right 10))
  , Test.ok "flatMap chain fail" (Either.isLeft (Either.flatMap (x -> if x > 5 then Left "too big" else Right (x * 2)) (Right 10)))
    -- pure
  , Test.ok "pure" (eitherEq (Either.pure 42) (Right 42))
  , Test.ok "pure string" (match Either.pure "hello" when Right "hello" -> true when _ -> false end)
    -- withDefault
  , Test.equal "withDefault Right" 42 (Either.withDefault 0 right42)
  , Test.equal "withDefault Left" 0 (Either.withDefault 0 leftErr)
    -- fromMaybe
  , Test.ok "fromMaybe Just" (eitherEq (Either.fromMaybe "not found" (Just 42)) (Right 42))
  , Test.ok "fromMaybe Nothing" (Either.isLeft (Either.fromMaybe "not found" Nothing))
    -- Equality via pattern matching
  , Test.ok "Right == Right same" (match (Right 42, Right 42) when (Right a, Right b) -> a == b when _ -> false end)
  , Test.ok "Right != Right diff" (match (Right 42, Right 43) when (Right a, Right b) -> a != b when _ -> false end)
  , Test.ok "Left == Left same" (match (Left "error", Left "error") when (Left a, Left b) -> a == b when _ -> false end)
  , Test.ok "Left != Left diff" (match (Left "error", Left "other") when (Left a, Left b) -> a != b when _ -> false end)
  , Test.ok "Right != Left" (match (Right 42, Left 42) when (Right _, Left _) -> true when _ -> false end)
  , Test.ok "Left != Right" (match (Left 42, Right 42) when (Left _, Right _) -> true when _ -> false end)
    -- Chaining (single line to avoid parsing issues)
  , Test.ok "chain multiple maps" (eitherEq (Right 10 |> Either.map (x -> x + 2) |> Either.map (x -> x * 2) |> Either.map (x -> x + 4) |> Either.map (x -> x * 3)) (Right 168))
  , Test.ok "chain with Left" (Either.isLeft (Right 10 |> Either.flatMap (x -> Right (x + 2)) |> Either.flatMap (x -> Left "failed") |> Either.flatMap (x -> Right (x + 4))))
    -- Common error handling patterns
  , Test.ok "safe division" (eitherEq (let safeDivide = a b -> if b == 0 then Left "division by zero" else Right (Int.div a b) in safeDivide 10 2) (Right 5))
  , Test.ok "safe division error" (Either.isLeft (let safeDivide = a b -> if b == 0 then Left "division by zero" else Right (Int.div a b) in safeDivide 10 0))
    -- Validation pattern
  , Test.ok "validation success" (eitherEq (let validate = x -> if x >= 0 && x <= 100 then Right x else Left "out of range" in Either.flatMap validate (Right 25)) (Right 25))
  , Test.ok "validation failure" (Either.isLeft (let validate = x -> if x >= 0 && x <= 100 then Right x else Left "out of range" in Either.flatMap validate (Right 150)))
    -- Nested Either
  , Test.ok "nested Right Right" (match Right (Right 42) when Right (Right 42) -> true when _ -> false end)
  , Test.ok "nested Right Left" (match Right (Left "inner") when Right (Left "inner") -> true when _ -> false end)
  , Test.ok "flatten nested" (eitherEq (Either.flatMap (x -> x) (Right (Right 42))) (Right 42))
    -- Pattern matching
  , Test.equal "match Right" 42 (match right42 when Right x -> x when Left _ -> 0 end)
  , Test.equal "match Left" "error" (match leftErr when Right _ -> "" when Left e -> e end)
  ]

let main = args -> Test.run tests
