-- Stdlib: Int module
let tests =
  [ Test.equal "add" 15 (Int.add 10 5)
    , Test.equal "sub" 5 (Int.sub 10 5)
    , Test.equal "mul" 50 (Int.mul 10 5)
    , Test.equal "div" 2 (Int.div 10 5)
    , Test.equal "mod" 1 (Int.mod 10 3)
    , Test.equal "add negative" 5 (Int.add 10 (0 - 5))
    , Test.equal "sub negative" 15 (Int.sub 10 (0 - 5))
    , Test.equal "mul negative" (0 - 50) (Int.mul 10 (0 - 5))
    , Test.equal "div negative" (0 - 2) (Int.div 10 (0 - 5))
    , Test.equal "neg positive" (0 - 10) (Int.neg 10)
    , Test.equal "neg negative" 10 (Int.neg (0 - 10))
    , Test.equal "neg zero" 0 (Int.neg 0)
    , Test.equal "abs positive" 42 (Int.abs 42)
    , Test.equal "abs negative" 42 (Int.abs (0 - 42))
    , Test.equal "abs zero" 0 (Int.abs 0)
    , Test.ok "eq true" (Int.eq 5 5)
    , Test.ok "eq false" (Bool.negate (Int.eq 5 6))
    , Test.ok "lt true" (Int.lt 5 10)
    , Test.ok "lt false" (Bool.negate (Int.lt 10 5))
    , Test.ok "lt equal" (Bool.negate (Int.lt 5 5))
    , Test.ok "le true <" (Int.le 5 10)
    , Test.ok "le true =" (Int.le 5 5)
    , Test.ok "le false" (Bool.negate (Int.le 10 5))
    , Test.ok "gt true" (Int.gt 10 5)
    , Test.ok "gt false" (Bool.negate (Int.gt 5 10))
    , Test.ok "gt equal" (Bool.negate (Int.gt 5 5))
    , Test.ok "ge true >" (Int.ge 10 5)
    , Test.ok "ge true =" (Int.ge 5 5)
    , Test.ok "ge false" (Bool.negate (Int.ge 5 10))
    , Test.equal "toFloat 42" 42.0 (Int.toFloat 42)
    , Test.equal "toFloat negative" (0 - 42.0) (Int.toFloat (0 - 42))
    , Test.equal "toFloat zero" 0.0 (Int.toFloat 0)
    , Test.equal "toString 42" "42" (Int.toString 42)
    , Test.equal "toString negative" "-42" (Int.toString (0 - 42))
    , Test.equal "toString zero" "0" (Int.toString 0)
    , Test.equal "toString large" "1000000" (Int.toString 1000000)
    , Test.equal "fromString 42" (Just 42) (Int.fromString "42")
    , Test.equal "fromString negative" (Just (0 - 42)) (Int.fromString "-42")
    , Test.equal "fromString zero" (Just 0) (Int.fromString "0")
    , Test.equal "fromString invalid" Nothing (Int.fromString "abc")
    , Test.equal "fromString empty" Nothing (Int.fromString "")
    , Test.equal "fromString mixed" Nothing (Int.fromString "12abc")
    , Test.equal "fromString spaces" Nothing (Int.fromString " 42 ")
    , Test.equal "fromString float" Nothing (Int.fromString "3.14")
    , Test.equal "mod zero dividend" 0 (Int.mod 0 5)
    , Test.equal "div zero dividend" 0 (Int.div 0 5)
    , Test.equal "add zeros" 0 (Int.add 0 0)
    , Test.equal "mul by zero" 0 (Int.mul 42 0)
    , Test.equal "mul by one" 42 (Int.mul 42 1)
    , Test.equal "div by one" 42 (Int.div 42 1)
    , Test.equal "add large" 2000000000 (Int.add 1000000000 1000000000)
    , Test.equal "mul large" 1000000 (Int.mul 1000 1000)
    ]

-- Basic arithmetic
-- Negative arithmetic
-- abs
-- Comparisons
-- Conversion: toFloat
-- Conversion: toString
-- Conversion: fromString
-- Edge cases
-- Large numbers
let main =
  args -> Test.run tests
