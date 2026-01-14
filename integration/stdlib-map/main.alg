-- Stdlib: Map module
let tests =
  let empty = Map.empty in
  let single = Map.singleton "a" 1 in
  let multi = Map.fromList [("a", 1), ("b", 2), ("c", 3)] in
  [ Test.equal "empty size" 0 (Map.size empty)
    , Test.ok "empty lookup Nothing" (Maybe.isNothing (Map.lookup "x" empty))
    , Test.equal "singleton size" 1 (Map.size single)
    , Test.equal "singleton lookup" (Just 1) (Map.lookup "a" single)
    , Test.equal "singleton lookup missing" Nothing (Map.lookup "b" single)
    , Test.equal "insert new" (Just 10) (Map.lookup "x" (Map.insert "x" 10 empty))
    , Test.equal "insert size" 1 (Map.size (Map.insert "x" 10 empty))
    , Test.equal "insert overwrite" (Just 99) (Map.lookup "a" (Map.insert "a" 99 single))
    , Test.equal "insert overwrite size" 1 (Map.size (Map.insert "a" 99 single))
    , Test.equal "insert multiple" 3 (Map.size multi)
    , Test.equal "lookup found" (Just 2) (Map.lookup "b" multi)
    , Test.equal "lookup not found" Nothing (Map.lookup "x" multi)
    , Test.equal "delete existing" Nothing (Map.lookup "a" (Map.delete "a" multi))
    , Test.equal "delete size" 2 (Map.size (Map.delete "a" multi))
    , Test.equal "delete non-existing" 3 (Map.size (Map.delete "x" multi))
    , Test.equal "delete from empty" 0 (Map.size (Map.delete "x" empty))
    , Test.ok "member true" (Map.member "a" multi)
    , Test.ok "member false" (Bool.negate (Map.member "x" multi))
    , Test.ok "member empty" (Bool.negate (Map.member "a" empty))
    , Test.equal "size empty" 0 (Map.size empty)
    , Test.equal "size single" 1 (Map.size single)
    , Test.equal "size multi" 3 (Map.size multi)
    , Test.equal "keys length" 3 (List.length (Map.keys multi))
    , Test.ok "keys contains a" (List.elem "a" (Map.keys multi))
    , Test.ok "keys contains b" (List.elem "b" (Map.keys multi))
    , Test.ok "keys contains c" (List.elem "c" (Map.keys multi))
    , Test.equal "keys empty" [] (Map.keys empty)
    , Test.equal "values length" 3 (List.length (Map.values multi))
    , Test.equal "values sum" 6 (List.sum (Map.values multi))
    , Test.equal "values empty" [] (Map.values empty)
    , Test.equal "toList length" 3 (List.length (Map.toList multi))
    , Test.equal "fromList size" 3 (Map.size (Map.fromList [("x", 1), ("y", 2), ("z", 3)]))
    , Test.equal "fromList lookup" (Just 2) (Map.lookup "y" (Map.fromList [("x", 1), ("y", 2), ("z", 3)]))
    , Test.equal "fromList duplicate key" (Just 99) (Map.lookup "a" (Map.fromList [("a", 1), ("a", 99)]))
    , Test.equal "fromList empty" 0 (Map.size (Map.fromList []))
    , Test.equal "fromList toList size" 3 (Map.size (Map.fromList (Map.toList multi)))
    , Test.equal "update chain" (Just 30) (empty |> Map.insert "a" 10 |> Map.insert "b" 20 |> Map.insert "a" 30 |> Map.lookup "a")
    , Test.equal "large map" 100 (Map.size (Map.fromList (List.map (i -> (Int.toString i, i)) (List.range 0 100))))
    ]

-- empty
-- singleton
-- insert
-- lookup
-- delete
-- member
-- size
-- keys
-- values
-- toList / fromList
-- Roundtrip
-- Update chain
-- Large map
let main =
  args -> Test.run tests
