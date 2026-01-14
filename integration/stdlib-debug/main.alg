-- Stdlib: Debug module

let tests =
  [ -- log returns its argument unchanged
    Test.equal "log int" 42 (Debug.log 42)
  , Test.equal "log string" "hello" (Debug.log "hello")
  , Test.equal "log list" [1, 2, 3] (Debug.log [1, 2, 3])
  , Test.equal "log tuple" (1, 2) (Debug.log (1, 2))
  , Test.equal "log record" { x = 1 } (Debug.log { x = 1 })
    -- trace returns its second argument unchanged, logs the first
  , Test.equal "trace int" 42 (Debug.trace "value" 42)
  , Test.equal "trace string" "world" (Debug.trace "greeting" "world")
  , Test.equal "trace in expression" 84 (Debug.trace "doubling" 42 * 2)
    -- chained tracing
  , Test.equal "chained trace" 15 (
      let a = Debug.trace "a" 5 in
      let b = Debug.trace "b" 10 in
      a + b
    )
    -- trace with different message types
  , Test.equal "trace computation" 100 (
      [1, 2, 3, 4, 5]
        |> Debug.trace "input list"
        |> List.map (x -> x * 2)
        |> Debug.trace "after map"
        |> List.foldl (acc x -> acc + x) 0
        |> Debug.trace "final sum"
    )
  ]

let main = args -> Test.run tests
