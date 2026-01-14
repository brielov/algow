-- Stdlib: Random module
-- Note: Random tests verify the output is within expected ranges

let tests =
  [ -- int generates within range
    Test.ok "int in range 0-10" (
      let n = Random.int 0 10 in
      n >= 0 && n < 10
    )
  , Test.ok "int in range 100-200" (
      let n = Random.int 100 200 in
      n >= 100 && n < 200
    )
  , Test.ok "int negative range" (
      let n = Random.int (-100) 0 in
      n >= (-100) && n < 0
    )
    -- float generates 0-1
  , Test.ok "float in range" (
      let f = Random.float in
      f >= 0.0 && f < 1.0
    )
    -- bool returns true or false
  , Test.ok "bool is boolean" (
      let b = Random.bool in
      b == true || b == false
    )
    -- pick from list
  , Test.ok "pick returns Just for non-empty" (
      Maybe.isJust (Random.pick [1, 2, 3, 4, 5])
    )
  , Test.equal "pick from empty" Nothing (Random.pick [])
  , Test.ok "pick value in list" (
      match Random.pick [1, 2, 3]
        when Nothing -> false
        when Just x -> x == 1 || x == 2 || x == 3
      end
    )
    -- shuffle preserves length
  , Test.equal "shuffle length" 5 (List.length (Random.shuffle [1, 2, 3, 4, 5]))
  , Test.equal "shuffle empty" [] (Random.shuffle [])
  , Test.equal "shuffle singleton" [42] (Random.shuffle [42])
    -- shuffle preserves elements
  , Test.equal "shuffle preserves sum" 15 (List.sum (Random.shuffle [1, 2, 3, 4, 5]))
  , Test.ok "shuffle preserves elements" (
      let original = [1, 2, 3, 4, 5] in
      let shuffled = Random.shuffle original in
      List.all (x -> List.elem x shuffled) original &&
      List.all (x -> List.elem x original) shuffled
    )
    -- Multiple calls produce (potentially) different results
    -- Note: This could theoretically fail if RNG produces same value twice
  , Test.ok "multiple int calls" (
      let n1 = Random.int 0 1000000 in
      let n2 = Random.int 0 1000000 in
      let n3 = Random.int 0 1000000 in
      -- At least 2 of 3 should differ with high probability
      n1 != n2 || n2 != n3 || n1 != n3
    )
  ]

let main = args -> Test.run tests
