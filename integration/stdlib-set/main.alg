-- Stdlib: Set module
let tests =
  let empty = Set.empty in
  let single = Set.singleton "a" in
  let multi = Set.fromList ["a", "b", "c"] in
  let multi2 = Set.fromList ["b", "c", "d"] in
  [ Test.equal "empty size" 0 (Set.size empty)
    , Test.ok "empty member false" (Bool.negate (Set.member "x" empty))
    , Test.equal "singleton size" 1 (Set.size single)
    , Test.ok "singleton member" (Set.member "a" single)
    , Test.ok "singleton member false" (Bool.negate (Set.member "b" single))
    , Test.equal "insert new size" 1 (Set.size (Set.insert "x" empty))
    , Test.ok "insert member" (Set.member "x" (Set.insert "x" empty))
    , Test.equal "insert duplicate size" 1 (Set.size (Set.insert "a" single))
    , Test.equal "insert multiple size" 3 (Set.size multi)
    , Test.ok "delete existing" (Bool.negate (Set.member "a" (Set.delete "a" multi)))
    , Test.equal "delete size" 2 (Set.size (Set.delete "a" multi))
    , Test.equal "delete non-existing size" 3 (Set.size (Set.delete "x" multi))
    , Test.equal "delete from empty" 0 (Set.size (Set.delete "x" empty))
    , Test.ok "member true" (Set.member "a" multi)
    , Test.ok "member false" (Bool.negate (Set.member "x" multi))
    , Test.equal "size empty" 0 (Set.size empty)
    , Test.equal "size single" 1 (Set.size single)
    , Test.equal "size multi" 3 (Set.size multi)
    , Test.equal "toList length" 3 (List.length (Set.toList multi))
    , Test.equal "fromList size" 3 (Set.size (Set.fromList ["x", "y", "z"]))
    , Test.equal "fromList duplicates" 2 (Set.size (Set.fromList ["a", "a", "b"]))
    , Test.equal "fromList empty" 0 (Set.size (Set.fromList []))
    , Test.equal "union size" 4 (Set.size (Set.union multi multi2))
    , Test.ok "union member a" (Set.member "a" (Set.union multi multi2))
    , Test.ok "union member d" (Set.member "d" (Set.union multi multi2))
    , Test.equal "union same" 3 (Set.size (Set.union multi multi))
    , Test.equal "union empty" 3 (Set.size (Set.union multi empty))
    , Test.equal "intersect size" 2 (Set.size (Set.intersect multi multi2))
    , Test.ok "intersect member b" (Set.member "b" (Set.intersect multi multi2))
    , Test.ok "intersect member c" (Set.member "c" (Set.intersect multi multi2))
    , Test.ok "intersect not member a" (Bool.negate (Set.member "a" (Set.intersect multi multi2)))
    , Test.ok "intersect not member d" (Bool.negate (Set.member "d" (Set.intersect multi multi2)))
    , Test.equal "intersect same" 3 (Set.size (Set.intersect multi multi))
    , Test.equal "intersect empty" 0 (Set.size (Set.intersect multi empty))
    , Test.equal "difference size" 1 (Set.size (Set.difference multi multi2))
    , Test.ok "difference member a" (Set.member "a" (Set.difference multi multi2))
    , Test.ok "difference not member b" (Bool.negate (Set.member "b" (Set.difference multi multi2)))
    , Test.equal "difference same" 0 (Set.size (Set.difference multi multi))
    , Test.equal "difference empty" 3 (Set.size (Set.difference multi empty))
    , Test.equal "difference from empty" 0 (Set.size (Set.difference empty multi))
    , Test.equal "fromList toList size" 3 (Set.size (Set.fromList (Set.toList multi)))
    , Test.equal "chain insert" 4 (Set.size (empty |> Set.insert "a" |> Set.insert "b" |> Set.insert "c" |> Set.insert "d"))
    , Test.equal "large set" 100 (Set.size (Set.fromList (List.map Int.toString (List.range 0 100))))
    ]

-- empty
-- singleton
-- insert
-- delete
-- member
-- size
-- toList / fromList
-- union
-- intersect
-- difference
-- Roundtrip
-- Chain operations
-- Large set
let main =
  args -> Test.run tests
