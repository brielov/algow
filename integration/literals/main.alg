-- Literals: Integers, Floats, Strings, Chars, Booleans, Lists

let tests =
  -- Integer literals
  [ Test.equal "int zero" 0 0
  , Test.equal "int positive" 42 42
  , Test.equal "int negative" (-42) (0 - 42)
  , Test.equal "int large positive" 1000000 1000000
  , Test.equal "int large negative" (-1000000) (0 - 1000000)
  , Test.equal "int one" 1 1
  , Test.equal "int negative one" (-1) (0 - 1)
  , Test.equal "int max safe" 9007199254740991 9007199254740991
    -- Float literals
  , Test.equal "float zero" 0.0 0.0
  , Test.equal "float positive" 3.14 3.14
  , Test.equal "float negative" (-3.14) (0.0 - 3.14)
  , Test.equal "float whole" 42.0 42.0
  , Test.equal "float small" 0.001 0.001
  , Test.equal "float large" 1000000.0 1000000.0
    -- String literals
  , Test.equal "string empty" "" ""
  , Test.equal "string simple" "hello" "hello"
  , Test.equal "string with spaces" "hello world" "hello world"
  , Test.equal "string single char" "x" "x"
  , Test.equal "string digits" "12345" "12345"
  , Test.equal "string mixed" "abc123" "abc123"
    -- String escape sequences
  , Test.equal "string newline length" 1 (String.length "\n")
  , Test.equal "string tab length" 1 (String.length "\t")
  , Test.equal "string backslash length" 1 (String.length "\\")
  , Test.equal "string quote length" 1 (String.length "\"")
  , Test.equal "string carriage return length" 1 (String.length "\r")
    -- Multiple escapes in one string
  , Test.equal "multiple escapes length" 3 (String.length "\n\t\\")
  , Test.equal "mixed content" 6 (String.length "a\nb\tc")
    -- Char literals
  , Test.equal "char lowercase" 'a' 'a'
  , Test.equal "char uppercase" 'Z' 'Z'
  , Test.equal "char digit" '5' '5'
  , Test.equal "char space" ' ' ' '
    -- Char escape sequences
  , Test.equal "char newline" '\n' '\n'
  , Test.equal "char tab" '\t' '\t'
  , Test.equal "char backslash" '\\' '\\'
  , Test.equal "char single quote" '\'' '\''
    -- Char toInt
  , Test.equal "char a toInt" 97 (Char.toInt 'a')
  , Test.equal "char A toInt" 65 (Char.toInt 'A')
  , Test.equal "char 0 toInt" 48 (Char.toInt '0')
  , Test.equal "char space toInt" 32 (Char.toInt ' ')
    -- Boolean literals
  , Test.equal "bool true" true true
  , Test.equal "bool false" false false
  , Test.ok "true is truthy" true
  , Test.ok "false negated" (Bool.negate false)
    -- Boolean operations
  , Test.ok "true == true" (true == true)
  , Test.ok "false == false" (false == false)
  , Test.ok "true != false" (true != false)
    -- List literals
  , Test.equal "list empty" [] Nil
  , Test.equal "list singleton" [42] (42 :: Nil)
  , Test.equal "list multiple" [1, 2, 3] (1 :: 2 :: 3 :: Nil)
  , Test.equal "list length 0" 0 (List.length [])
  , Test.equal "list length 1" 1 (List.length [42])
  , Test.equal "list length 5" 5 (List.length [1, 2, 3, 4, 5])
    -- List with various types
  , Test.equal "list of strings" ["a", "b", "c"] ("a" :: "b" :: "c" :: Nil)
  , Test.equal "list of bools" [true, false, true] (true :: false :: true :: Nil)
  , Test.equal "list of tuples" [(1, 2), (3, 4)] ((1, 2) :: (3, 4) :: Nil)
    -- Nested lists
  , Test.equal "nested list" [[1, 2], [3, 4]] ([1, 2] :: [3, 4] :: Nil)
  , Test.equal "nested list length" 2 (List.length [[1], [2], [3]])
    -- List head/tail
  , Test.equal "head of list" (Just 1) (List.head [1, 2, 3])
  , Test.equal "head of empty" Nothing (List.head [])
  , Test.equal "tail length" 2 (Maybe.withDefault 0 (Maybe.map List.length (List.tail [1, 2, 3])))
    -- Unit literal
  , Test.equal "unit" () ()
    -- Tuple literals
  , Test.equal "pair literal" (1, 2) (1, 2)
  , Test.equal "triple literal" (1, 2, 3) (1, 2, 3)
  , Test.equal "quad literal" (1, 2, 3, 4) (1, 2, 3, 4)
  , Test.equal "mixed tuple" (1, "hello", true) (1, "hello", true)
  , Test.equal "nested tuple" ((1, 2), (3, 4)) ((1, 2), (3, 4))
    -- Complex nested literals
  , Test.equal "list of Maybe" [Just 1, Nothing, Just 3] (Just 1 :: Nothing :: Just 3 :: Nil)
  , Test.equal "tuple with list" ([1, 2], [3, 4]) ([1, 2], [3, 4])
  , Test.equal "record literal" { x = 1, y = 2 } { x = 1, y = 2 }
  , Test.equal "nested record" { a = { b = 42 } } { a = { b = 42 } }
  ]

let main = args -> Test.run tests
