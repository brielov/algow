-- Or-Pattern Examples

type Color = Red | Green | Blue | RGB int int int

let isPrimary = c -> match c when Red | Green | Blue -> true when RGB _ _ _ -> false end

let isEmptyOrNothing = opt -> match opt when Nothing | Just Nil -> true when _ -> false end

let isSpecialValue = opt -> match opt when Nothing | Just Nothing -> true when Just (Just _) -> false end

let extractValue = opt -> match opt when Just (Just x) -> x when Just Nothing | Nothing -> 0 end

let isZeroOrOne = n -> match n when 0 | 1 -> true when _ -> false end

let startsWithZeroOrOne = xs -> match xs when Cons 0 _ | Cons 1 _ -> true when _ -> false end

let tests =
  [ Test.ok "Red is primary" (isPrimary Red)
  , Test.ok "Green is primary" (isPrimary Green)
  , Test.ok "Blue is primary" (isPrimary Blue)
  , Test.ok "RGB not primary" (Bool.negate (isPrimary (RGB 1 2 3)))
  , Test.ok "Nothing" (isEmptyOrNothing Nothing)
  , Test.ok "Just Nil" (isEmptyOrNothing (Just Nil))
  , Test.ok "Just [1]" (Bool.negate (isEmptyOrNothing (Just [1])))
  , Test.ok "special Nothing" (isSpecialValue Nothing)
  , Test.ok "special Just Nothing" (isSpecialValue (Just Nothing))
  , Test.ok "not special Just Just" (Bool.negate (isSpecialValue (Just (Just 1))))
  , Test.equal "extract Just Just" 42 (extractValue (Just (Just 42)))
  , Test.equal "extract Just Nothing" 0 (extractValue (Just Nothing))
  , Test.equal "extract Nothing" 0 (extractValue Nothing)
  , Test.ok "0 is 0 or 1" (isZeroOrOne 0)
  , Test.ok "1 is 0 or 1" (isZeroOrOne 1)
  , Test.ok "5 not 0 or 1" (Bool.negate (isZeroOrOne 5))
  , Test.ok "starts with 0" (startsWithZeroOrOne [0, 2, 3])
  , Test.ok "starts with 1" (startsWithZeroOrOne [1, 2, 3])
  , Test.ok "not starts with 0 or 1" (Bool.negate (startsWithZeroOrOne [5, 2, 3]))
  ]

let main = args -> Test.run tests
