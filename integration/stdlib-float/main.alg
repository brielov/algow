-- Stdlib: Float module

let tests =
  [ -- Basic arithmetic
    Test.equal "add" 5.5 (Float.add 2.5 3.0)
  , Test.equal "sub" 1.5 (Float.sub 3.5 2.0)
  , Test.equal "mul" 6.0 (Float.mul 2.0 3.0)
  , Test.equal "div" 2.5 (Float.div 5.0 2.0)
    -- Negative arithmetic
  , Test.equal "add negative" 0.5 (Float.add 2.5 (-2.0))
  , Test.equal "sub negative" 4.5 (Float.sub 2.5 (-2.0))
  , Test.equal "mul negative" (-6.0) (Float.mul 2.0 (-3.0))
  , Test.equal "div negative" (-2.5) (Float.div 5.0 (-2.0))
  , Test.equal "neg positive" (-3.14) (Float.neg 3.14)
  , Test.equal "neg negative" 3.14 (Float.neg (-3.14))
  , Test.equal "neg zero" 0.0 (Float.neg 0.0)
    -- abs
  , Test.equal "abs positive" 3.14 (Float.abs 3.14)
  , Test.equal "abs negative" 3.14 (Float.abs (-3.14))
  , Test.equal "abs zero" 0.0 (Float.abs 0.0)
    -- Comparisons
  , Test.ok "eq true" (Float.eq 3.14 3.14)
  , Test.ok "eq false" (Bool.negate (Float.eq 3.14 2.71))
  , Test.ok "lt true" (Float.lt 2.5 3.5)
  , Test.ok "lt false" (Bool.negate (Float.lt 3.5 2.5))
  , Test.ok "lt equal" (Bool.negate (Float.lt 3.5 3.5))
  , Test.ok "le true <" (Float.le 2.5 3.5)
  , Test.ok "le true =" (Float.le 3.5 3.5)
  , Test.ok "gt true" (Float.gt 3.5 2.5)
  , Test.ok "gt false" (Bool.negate (Float.gt 2.5 3.5))
  , Test.ok "ge true >" (Float.ge 3.5 2.5)
  , Test.ok "ge true =" (Float.ge 3.5 3.5)
    -- floor / ceil / round
  , Test.equal "floor 3.7" 3 (Float.floor 3.7)
  , Test.equal "floor 3.2" 3 (Float.floor 3.2)
  , Test.equal "floor negative" (-4) (Float.floor (-3.2))
  , Test.equal "ceil 3.2" 4 (Float.ceil 3.2)
  , Test.equal "ceil 3.7" 4 (Float.ceil 3.7)
  , Test.equal "ceil negative" (-3) (Float.ceil (-3.2))
  , Test.equal "round 3.4" 3 (Float.round 3.4)
  , Test.equal "round 3.5" 4 (Float.round 3.5)
  , Test.equal "round 3.6" 4 (Float.round 3.6)
  , Test.equal "round negative" (-3) (Float.round (-3.4))
    -- sqrt
  , Test.equal "sqrt 4" 2.0 (Float.sqrt 4.0)
  , Test.equal "sqrt 9" 3.0 (Float.sqrt 9.0)
  , Test.equal "sqrt 16" 4.0 (Float.sqrt 16.0)
  , Test.equal "sqrt 0" 0.0 (Float.sqrt 0.0)
  , Test.equal "sqrt 1" 1.0 (Float.sqrt 1.0)
    -- pow
  , Test.equal "pow 2^3" 8.0 (Float.pow 2.0 3.0)
  , Test.equal "pow 3^2" 9.0 (Float.pow 3.0 2.0)
  , Test.equal "pow x^0" 1.0 (Float.pow 5.0 0.0)
  , Test.equal "pow x^1" 5.0 (Float.pow 5.0 1.0)
  , Test.equal "pow 0^n" 0.0 (Float.pow 0.0 5.0)
    -- sin / cos / tan (basic checks)
  , Test.equal "sin 0" 0.0 (Float.sin 0.0)
  , Test.equal "cos 0" 1.0 (Float.cos 0.0)
  , Test.equal "tan 0" 0.0 (Float.tan 0.0)
    -- log / exp
  , Test.equal "exp 0" 1.0 (Float.exp 0.0)
  , Test.equal "log 1" 0.0 (Float.log 1.0)
    -- toString / fromString
  , Test.equal "toString" "3.14" (Float.toString 3.14)
  , Test.equal "toString negative" "-2.5" (Float.toString (-2.5))
  , Test.equal "toString zero" "0" (Float.toString 0.0)
  , Test.equal "fromString 3.14" (Just 3.14) (Float.fromString "3.14")
  , Test.equal "fromString negative" (Just (-2.5)) (Float.fromString "-2.5")
  , Test.equal "fromString int" (Just 42.0) (Float.fromString "42")
  , Test.equal "fromString invalid" Nothing (Float.fromString "abc")
  , Test.equal "fromString empty" Nothing (Float.fromString "")
    -- Edge cases
  , Test.equal "add zeros" 0.0 (Float.add 0.0 0.0)
  , Test.equal "mul by zero" 0.0 (Float.mul 42.0 0.0)
  , Test.equal "mul by one" 42.0 (Float.mul 42.0 1.0)
  , Test.equal "div by one" 42.0 (Float.div 42.0 1.0)
  ]

let main = args -> Test.run tests
