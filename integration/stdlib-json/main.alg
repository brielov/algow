-- Stdlib: Json module

let tests =
  [ -- encode primitives
    Test.equal "encode int" "42" (Json.encode 42)
  , Test.equal "encode float" "3.14" (Json.encode 3.14)
  , Test.equal "encode string" "\"hello\"" (Json.encode "hello")
  , Test.equal "encode bool true" "true" (Json.encode true)
  , Test.equal "encode bool false" "false" (Json.encode false)
    -- encode collections
  , Test.ok "encode list contains 1" (String.contains "1" (Json.encode [1, 2, 3]))
  , Test.equal "encode empty list" "[]" (Json.encode [])
    -- decode basic
  , Test.ok "decode valid json" (Either.isRight (Json.decode "42"))
  , Test.ok "decode invalid json" (Either.isLeft (Json.decode "invalid"))
    -- int decoder
  , Test.ok "int decoder" (
      match Json.decode "42"
        when Left e -> false
        when Right json -> match Json.int json when Just 42 -> true when _ -> false end
      end
    )
  , Test.ok "int decoder string" (
      match Json.decode "\"hello\""
        when Left e -> false
        when Right json -> Maybe.isNothing (Json.int json)
      end
    )
    -- float decoder
  , Test.ok "float decoder" (
      match Json.decode "3.14"
        when Left e -> false
        when Right json -> match Json.float json when Just n -> Float.abs (n - 3.14) < 0.001 when Nothing -> false end
      end
    )
    -- string decoder
  , Test.ok "string decoder" (
      match Json.decode "\"hello\""
        when Left e -> false
        when Right json -> match Json.string json when Just "hello" -> true when _ -> false end
      end
    )
  , Test.ok "string decoder int" (
      match Json.decode "42"
        when Left e -> false
        when Right json -> Maybe.isNothing (Json.string json)
      end
    )
    -- bool decoder
  , Test.ok "bool decoder true" (
      match Json.decode "true"
        when Left e -> false
        when Right json -> match Json.bool json when Just true -> true when _ -> false end
      end
    )
  , Test.ok "bool decoder false" (
      match Json.decode "false"
        when Left e -> false
        when Right json -> match Json.bool json when Just false -> true when _ -> false end
      end
    )
    -- null decoder
  , Test.ok "null decoder" (
      match Json.decode "null"
        when Left e -> false
        when Right json -> Maybe.isJust (Json.null json)
      end
    )
    -- field decoder
  , Test.ok "field decoder" (
      match Json.decode "{\"x\": 42}"
        when Left e -> false
        when Right json -> Maybe.isJust (Json.field "x" json)
      end
    )
  , Test.ok "field decoder missing" (
      match Json.decode "{\"x\": 42}"
        when Left e -> false
        when Right json -> Maybe.isNothing (Json.field "y" json)
      end
    )
    -- at decoder (field + decoder)
  , Test.ok "at decoder" (
      match Json.decode "{\"x\": 42}"
        when Left e -> false
        when Right json -> match Json.at "x" Json.int json when Just 42 -> true when _ -> false end
      end
    )
    -- list decoder
  , Test.ok "list int decoder" (
      match Json.decode "[1, 2, 3]"
        when Left e -> false
        when Right json -> match Json.list Json.int json when Just xs -> List.length xs == 3 when Nothing -> false end
      end
    )
  , Test.ok "list empty" (
      match Json.decode "[]"
        when Left e -> false
        when Right json -> match Json.list Json.int json when Just xs -> List.isEmpty xs when Nothing -> false end
      end
    )
    -- nullable decoder
  , Test.ok "nullable null" (
      match Json.decode "null"
        when Left e -> false
        when Right json -> match Json.nullable Json.int json when Just Nothing -> true when _ -> false end
      end
    )
  , Test.ok "nullable value" (
      match Json.decode "42"
        when Left e -> false
        when Right json -> match Json.nullable Json.int json when Just (Just 42) -> true when _ -> false end
      end
    )
    -- map
  , Test.ok "map decoder" (
      match Json.decode "42"
        when Left e -> false
        when Right json -> match Json.map (x -> x * 2) (Json.int json) when Just 84 -> true when _ -> false end
      end
    )
    -- map2
  , Test.ok "map2 decoder" (
      match Json.decode "{\"x\": 10, \"y\": 20}"
        when Left e -> false
        when Right json ->
          let result = Json.map2 (a b -> a + b) (Json.at "x" Json.int json) (Json.at "y" Json.int json) in
          match result when Just 30 -> true when _ -> false end
      end
    )
    -- map3
  , Test.ok "map3 decoder" (
      match Json.decode "{\"a\": 10, \"b\": 20, \"c\": 30}"
        when Left e -> false
        when Right json ->
          let result = Json.map3 (a b c -> a + b + c) (Json.at "a" Json.int json) (Json.at "b" Json.int json) (Json.at "c" Json.int json) in
          match result when Just 60 -> true when _ -> false end
      end
    )
    -- Nested object
  , Test.ok "nested object" (
      match Json.decode "{\"point\": {\"x\": 10, \"y\": 20}}"
        when Left e -> false
        when Right json ->
          let result = Maybe.flatMap (p -> Json.at "x" Json.int p) (Json.field "point" json) in
          match result when Just 10 -> true when _ -> false end
      end
    )
  ]

let main = args -> Test.run tests
