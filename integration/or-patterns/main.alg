-- Or-Pattern Examples

type Color = Red | Green | Blue | RGB int int int

let isPrimary = c -> match c when Red | Green | Blue -> true when RGB _ _ _ -> false end

let isEmptyOrNothing = opt -> match opt when Nothing | Just Nil -> true when _ -> false end

let isSpecialValue = opt -> match opt when Nothing | Just Nothing -> true when Just (Just _) -> false end

let extractValue = opt -> match opt when Just (Just x) -> x when Just Nothing | Nothing -> 0 end

let isZeroOrOne = n -> match n when 0 | 1 -> true when _ -> false end

let startsWithZeroOrOne = xs -> match xs when Cons 0 _ | Cons 1 _ -> true when _ -> false end

let main = args ->
  let a = Assert.ok (isPrimary Red) "Red is primary" in
  let b = Assert.ok (isPrimary Green) "Green is primary" in
  let c = Assert.ok (isPrimary Blue) "Blue is primary" in
  let d = Assert.ok (Bool.negate (isPrimary (RGB 1 2 3))) "RGB not primary" in
  let e = Assert.ok (isEmptyOrNothing Nothing) "Nothing" in
  let f = Assert.ok (isEmptyOrNothing (Just Nil)) "Just Nil" in
  let g = Assert.ok (Bool.negate (isEmptyOrNothing (Just [1]))) "Just [1]" in
  let h = Assert.ok (isSpecialValue Nothing) "special Nothing" in
  let i = Assert.ok (isSpecialValue (Just Nothing)) "special Just Nothing" in
  let j = Assert.ok (Bool.negate (isSpecialValue (Just (Just 1)))) "not special Just Just" in
  let k = Assert.equal 42 (extractValue (Just (Just 42))) "extract Just Just" in
  let l = Assert.equal 0 (extractValue (Just Nothing)) "extract Just Nothing" in
  let m = Assert.equal 0 (extractValue Nothing) "extract Nothing" in
  let n = Assert.ok (isZeroOrOne 0) "0 is 0 or 1" in
  let o = Assert.ok (isZeroOrOne 1) "1 is 0 or 1" in
  let p = Assert.ok (Bool.negate (isZeroOrOne 5)) "5 not 0 or 1" in
  let q = Assert.ok (startsWithZeroOrOne [0, 2, 3]) "starts with 0" in
  let r = Assert.ok (startsWithZeroOrOne [1, 2, 3]) "starts with 1" in
  let s = Assert.ok (Bool.negate (startsWithZeroOrOne [5, 2, 3])) "not starts with 0 or 1" in
  IO.printLine "or-patterns: ok"
