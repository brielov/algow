-- Operators: Arithmetic, Comparison, Logical, Precedence, Associativity, Pipe, Cons
let tests =
  let add = a b -> a + b in
  -- Arithmetic: +, -, *, /, mod
  let sub = a b -> a - b in
  let mul = a b -> a * b in
  let div_ = a b -> Int.div a b in
  let mod_ = a b -> Int.mod a b in
  [ Test.equal "addition" 15 (10 + 5)
    , Test.equal "subtraction" 5 (10 - 5)
    , Test.equal "multiplication" 50 (10 * 5)
    , Test.equal "division" 3 (Int.div 10 3)
    , Test.equal "modulo" 1 (Int.mod 10 3)
    , Test.equal "negative addition" 5 (10 + (0 - 5))
    , Test.equal "negative subtraction" 15 (10 - (0 - 5))
    , Test.equal "negative multiplication" (0 - 50) (10 * (0 - 5))
    , Test.equal "negative division" (0 - 3) (Int.div (0 - 10) 3)
    , Test.equal "zero addition" 10 (10 + 0)
    , Test.equal "zero multiplication" 0 (10 * 0)
    , Test.equal "identity subtraction" 0 (10 - 10)
    , Test.ok "float addition" (Float.abs (1.5 + 2.0 - 3.5) < 0.001)
    , Test.ok "float subtraction" (Float.abs (3.5 - 2.0 - 1.5) < 0.001)
    , Test.ok "float multiplication" (Float.abs (2.0 * 3.0 - 6.0) < 0.001)
    , Test.ok "float division" (Float.abs (5.0 / 2.0 - 2.5) < 0.001)
    , Test.equal "unary negation" (0 - 10) (0 - 10)
    , Test.equal "double negation" 10 (0 - (0 - 10))
    , Test.ok "less than true" (5 < 10)
    , Test.ok "less than false" (Bool.negate (10 < 5))
    , Test.ok "less than equal edge" (Bool.negate (5 < 5))
    , Test.ok "less or equal true <" (5 <= 10)
    , Test.ok "less or equal true =" (5 <= 5)
    , Test.ok "less or equal false" (Bool.negate (10 <= 5))
    , Test.ok "greater than true" (10 > 5)
    , Test.ok "greater than false" (Bool.negate (5 > 10))
    , Test.ok "greater than equal edge" (Bool.negate (5 > 5))
    , Test.ok "greater or equal true >" (10 >= 5)
    , Test.ok "greater or equal true =" (5 >= 5)
    , Test.ok "greater or equal false" (Bool.negate (5 >= 10))
    , Test.ok "equal true int" (5 == 5)
    , Test.ok "equal false int" (Bool.negate (5 == 6))
    , Test.ok "equal true string" ("hello" == "hello")
    , Test.ok "equal false string" (Bool.negate ("hello" == "world"))
    , Test.ok "not equal true" (5 != 6)
    , Test.ok "not equal false" (Bool.negate (5 != 5))
    , Test.ok "negative less than" (0 - 10 < 5)
    , Test.ok "negative greater than" (5 > 0 - 10)
    , Test.ok "both negative compare" (0 - 10 < 0 - 5)
    , Test.ok "float less than" (1.5 < 2.5)
    , Test.ok "float equal" (2.5 == 2.5)
    , Test.ok "string less than" ("apple" < "banana")
    , Test.ok "string greater than" ("zebra" > "apple")
    , Test.ok "char less than" ('a' < 'b')
    , Test.ok "char equal" ('x' == 'x')
    , Test.equal "and tt" true (true && true)
    , Test.equal "and tf" false (true && false)
    , Test.equal "and ft" false (false && true)
    , Test.equal "and ff" false (false && false)
    , Test.equal "or tt" true (true || true)
    , Test.equal "or tf" true (true || false)
    , Test.equal "or ft" true (false || true)
    , Test.equal "or ff" false (false || false)
    , Test.equal "and short-circuit" false (false && Int.div 1 0 == 0)
    , Test.equal "or short-circuit" true (true || Int.div 1 0 == 0)
    , Test.equal "negate true" false (Bool.negate true)
    , Test.equal "negate false" true (Bool.negate false)
    , Test.equal "double negate" true (Bool.negate (Bool.negate true))
    , Test.equal "mul before add" 14 (2 + 3 * 4)
    , Test.equal "mul before sub" (0 - 10) (2 - 3 * 4)
    , Test.equal "parens override" 20 ((2 + 3) * 4)
    , Test.equal "complex precedence" 17 (1 + 2 * 3 + 4 * 2 + 2)
    , Test.ok "compare after arithmetic" (2 + 3 < 10)
    , Test.ok "compare after arithmetic 2" (2 * 5 == 10)
    , Test.equal "and before or 1" true (true || false && false)
    , Test.equal "and before or 2" false (false || false && true)
    , Test.equal "and before or 3" true ((true || false) && true)
    , Test.equal "complex logical" true (5 > 3 && 10 < 20 || false)
    , Test.equal "complex logical 2" false (5 > 10 && true || 3 > 5)
    , Test.equal "sub left assoc" (0 - 3) (10 - 5 - 8)
    , Test.equal "sub left assoc parens" 13 (10 - (5 - 8))
    , Test.equal "div left assoc" 2 (Int.div (Int.div 100 5) 10)
    , Test.equal "cons basic length" 3 (List.length (1 :: [2, 3]))
    , Test.equal "cons chain length" 4 (List.length (1 :: 2 :: 3 :: 4 :: []))
    , Test.equal "cons to Nil length" 1 (List.length (42 :: Nil))
    , Test.ok "cons head" (match List.head (1 :: [2, 3])
      when Just 1 -> true
      when _ -> false
    end)
    , Test.equal "pipe basic" 10 (5 |> (x -> x * 2))
    , Test.equal "pipe chain" 30 (5 |> (x -> x * 2) |> (x -> x + 20))
    , Test.equal "pipe with list" 15 ([1, 2, 3, 4, 5] |> List.foldl (acc x -> acc + x) 0)
    , Test.equal "pipe three" 42 (2 |> (x -> x * 3) |> (x -> x + 1) |> (x -> x * 6))
    , Test.equal "long pipe" 100 (1 |> (x -> x + 1) |> (x -> x * 2) |> (x -> x + 6) |> (x -> x * 10))
    , Test.ok "mixed 1" (1 + 2 * 3 > 5 && 10 < 20)
    , Test.ok "mixed 2" (Bool.negate (1 + 2 > 10 || 5 * 2 < 5))
    , Test.equal "mixed arithmetic" 23 (1 + 2 * 3 + 4 * 4)
    , Test.ok "tuple equality" ((1, 2) == (1, 2))
    , Test.ok "tuple inequality" ((1, 2) != (1, 3))
    , Test.ok "nested tuple equality" (((1, 2), (3, 4)) == ((1, 2), (3, 4)))
    ]

-- Basic arithmetic
-- Float arithmetic
-- Unary negation
-- Comparison operators: <, <=, >, >=, ==, !=
-- Comparison with negative numbers
-- Float comparisons
-- String comparisons (lexicographic)
-- Char comparisons
-- Logical operators: &&, ||
-- Short-circuit evaluation
-- Boolean negate
-- Precedence tests
-- Comparison precedence
-- Logical precedence (and binds tighter than or)
-- Complex logical
-- Associativity tests
-- Cons operator :: (test lengths instead of direct list equality)
-- Pipe operator |>
-- Mixed operators
-- Equality on tuples (tuples do satisfy Eq)
let main =
  args -> Test.run tests
