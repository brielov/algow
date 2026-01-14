-- Stdlib: IO module (limited tests since IO has side effects)

-- Note: Most IO functions have side effects and are difficult to test.
-- We test what we can without affecting the environment significantly.

let tests =
  [ -- getEnv returns Maybe (environment variable may or may not exist)
    Test.ok "getEnv returns Maybe" (
      let result = IO.getEnv "PATH" in
      Maybe.isJust result || Maybe.isNothing result
    )
  , Test.ok "getEnv nonexistent" (Maybe.isNothing (IO.getEnv "ALGOW_NONEXISTENT_VAR_12345"))
    -- print and printLine - just verify they execute without crashing
  , Test.ok "print executes" (
      let result = IO.print "" in
      true
    )
  , Test.ok "printLine executes" (
      let result = IO.printLine "" in
      true
    )
  ]

let main = args -> Test.run tests
