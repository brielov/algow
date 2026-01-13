-- Algorithm W - Test Suite
-- Entry point that uses types.alg and infer.alg

-- Test expressions
let idFn = EAbs "x" (EVar "x")
let constFn = EAbs "x" (EAbs "y" (EVar "x"))
let applyFn = EAbs "f" (EAbs "x" (EApp (EVar "f") (EVar "x")))
let composeFn = EAbs "f" (EAbs "g" (EAbs "x" (EApp (EVar "f") (EApp (EVar "g") (EVar "x")))))
let flipFn = EAbs "f" (EAbs "a" (EAbs "b" (EApp (EApp (EVar "f") (EVar "b")) (EVar "a"))))

let letPoly = ELet "id" (EAbs "x" (EVar "x")) (EApp (EVar "id") (EInt 42))
let letPoly2 = ELet "f" (EAbs "x" (EVar "x")) (EIf (EApp (EVar "f") (EBool true)) (EApp (EVar "f") (EInt 1)) (EInt 0))
let ifExpr = EIf (EBool true) (EInt 1) (EInt 2)
let higherOrder = EAbs "f" (EApp (EVar "f") (EApp (EVar "f") (EInt 0)))

-- Church numerals
let zero = EAbs "f" (EAbs "x" (EVar "x"))
let succ = EAbs "n" (EAbs "f" (EAbs "x" (EApp (EVar "f") (EApp (EApp (EVar "n") (EVar "f")) (EVar "x")))))

-- SKI combinators
let sCombi = EAbs "x" (EAbs "y" (EAbs "z" (EApp (EApp (EVar "x") (EVar "z")) (EApp (EVar "y") (EVar "z")))))
let kCombi = EAbs "x" (EAbs "y" (EVar "x"))
let iCombi = EAbs "x" (EVar "x")

-- Should fail: omega (self-application)
let omega = EAbs "x" (EApp (EVar "x") (EVar "x"))

-- Test helper: returns true if inference matches expectation
let test = name expr shouldSucceed ->
  match typeOf expr
    when Left err -> Test.ok name (shouldSucceed == false)
    when Right ty -> Test.ok name (shouldSucceed == true)
  end

let tests =
  [ test "id" idFn true
  , test "const" constFn true
  , test "apply" applyFn true
  , test "compose" composeFn true
  , test "flip" flipFn true
  , test "letPoly" letPoly true
  , test "letPoly2" letPoly2 true
  , test "if" ifExpr true
  , test "higherOrder" higherOrder true
  , test "zero" zero true
  , test "succ" succ true
  , test "S" sCombi true
  , test "K" kCombi true
  , test "I" iCombi true
  , test "omega" omega false
  ]

let main = args -> Test.run tests
