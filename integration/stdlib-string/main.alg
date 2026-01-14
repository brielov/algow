-- Stdlib: String module
let tests =
  let s = "hello world" in
  let empty = "" in
  [ Test.equal "length hello" 5 (String.length "hello")
    , Test.equal "length empty" 0 (String.length empty)
    , Test.equal "length space" 11 (String.length s)
    , Test.ok "isEmpty empty" (String.isEmpty empty)
    , Test.ok "isEmpty not empty" (Bool.negate (String.isEmpty "x"))
    , Test.equal "concat" "helloworld" (String.concat "hello" "world")
    , Test.equal "concat empty left" "hello" (String.concat "" "hello")
    , Test.equal "concat empty right" "hello" (String.concat "hello" "")
    , Test.equal "concat empty both" "" (String.concat "" "")
    , Test.equal "substring hello" "hello" (String.substring 0 5 s)
    , Test.equal "substring world" "world" (String.substring 6 11 s)
    , Test.equal "substring middle" "lo wo" (String.substring 3 8 s)
    , Test.equal "substring empty" "" (String.substring 0 0 s)
    , Test.equal "charAt 0" (Just 'h') (String.charAt 0 s)
    , Test.equal "charAt 4" (Just 'o') (String.charAt 4 s)
    , Test.equal "charAt out of bounds" Nothing (String.charAt 100 s)
    , Test.equal "charAt negative" Nothing (String.charAt (0 - 1) s)
    , Test.equal "head" (Just 'h') (String.head s)
    , Test.equal "head empty" Nothing (String.head empty)
    , Test.equal "tail hello" "ello" (String.tail "hello")
    , Test.equal "tail single" "" (String.tail "x")
    , Test.equal "tail empty" "" (String.tail empty)
    , Test.equal "take 5" "hello" (String.take 5 s)
    , Test.equal "take 0" "" (String.take 0 s)
    , Test.equal "take more" "hello" (String.take 100 "hello")
    , Test.equal "take negative" "" (String.take (0 - 1) s)
    , Test.equal "drop 6" "world" (String.drop 6 s)
    , Test.equal "drop 0" s (String.drop 0 s)
    , Test.equal "drop more" "" (String.drop 100 s)
    , Test.equal "drop negative" s (String.drop (0 - 1) s)
    , Test.equal "toList" ['h', 'i'] (String.toList "hi")
    , Test.equal "fromList" "hi" (String.fromList ['h', 'i'])
    , Test.equal "toList empty" [] (String.toList empty)
    , Test.equal "fromList empty" "" (String.fromList [])
    , Test.equal "split comma" ["a", "b", "c"] (String.split "," "a,b,c")
    , Test.equal "split space" ["hello", "world"] (String.split " " s)
    , Test.equal "split not found" ["hello"] (String.split "x" "hello")
    , Test.equal "join comma" "a,b,c" (String.join "," ["a", "b", "c"])
    , Test.equal "join space" "hello world" (String.join " " ["hello", "world"])
    , Test.equal "join empty list" "" (String.join "," [])
    , Test.equal "trim spaces" "hello" (String.trim "  hello  ")
    , Test.equal "trim tabs" "hello" (String.trim "\thello\t")
    , Test.equal "trim mixed" "hello" (String.trim "  \n hello \t ")
    , Test.equal "trim empty" "" (String.trim "   ")
    , Test.equal "toUpper" "HELLO" (String.toUpper "hello")
    , Test.equal "toUpper mixed" "HELLO WORLD" (String.toUpper "Hello World")
    , Test.equal "toLower" "hello" (String.toLower "HELLO")
    , Test.equal "toLower mixed" "hello world" (String.toLower "Hello World")
    , Test.ok "contains true" (String.contains "ell" s)
    , Test.ok "contains false" (Bool.negate (String.contains "xyz" s))
    , Test.ok "contains empty" (String.contains "" s)
    , Test.ok "contains self" (String.contains "hello" "hello")
    , Test.ok "startsWith true" (String.startsWith "hello" s)
    , Test.ok "startsWith false" (Bool.negate (String.startsWith "world" s))
    , Test.ok "startsWith empty" (String.startsWith "" s)
    , Test.ok "endsWith true" (String.endsWith "world" s)
    , Test.ok "endsWith false" (Bool.negate (String.endsWith "hello" s))
    , Test.ok "endsWith empty" (String.endsWith "" s)
    , Test.equal "replace" "hi world" (String.replace "hello" "hi" s)
    , Test.equal "replace not found" "hello" (String.replace "xyz" "abc" "hello")
    , Test.equal "replace all" "h-ll-" (String.replace "o" "-" "hello")
    , Test.equal "reverse" "olleh" (String.reverse "hello")
    , Test.equal "reverse empty" "" (String.reverse "")
    , Test.equal "reverse single" "x" (String.reverse "x")
    , Test.equal "repeat 3" "abcabcabc" (String.repeat 3 "abc")
    , Test.equal "repeat 0" "" (String.repeat 0 "abc")
    , Test.equal "repeat 1" "abc" (String.repeat 1 "abc")
    , Test.equal "indexOf" (Just 2) (String.indexOf "l" "hello")
    , Test.equal "indexOf not found" Nothing (String.indexOf "x" "hello")
    , Test.equal "lastIndexOf" (Just 3) (String.lastIndexOf "l" "hello")
    , Test.equal "lastIndexOf not found" Nothing (String.lastIndexOf "x" "hello")
    , Test.equal "padLeft" "00042" (String.padLeft 5 '0' "42")
    , Test.equal "padLeft no change" "hello" (String.padLeft 3 '0' "hello")
    , Test.equal "padRight" "42000" (String.padRight 5 '0' "42")
    , Test.equal "padRight no change" "hello" (String.padRight 3 '0' "hello")
    , Test.equal "lines" ["a", "b", "c"] (String.lines "a\nb\nc")
    , Test.equal "unlines" "a\nb\nc" (String.unlines ["a", "b", "c"])
    , Test.equal "words" ["hello", "world"] (String.words "hello world")
    , Test.equal "words multiple spaces" ["a", "b", "c"] (String.words "a  b   c")
    , Test.equal "unwords" "hello world" (String.unwords ["hello", "world"])
    , Test.ok "eq true" (String.eq "hello" "hello")
    , Test.ok "eq false" (Bool.negate (String.eq "hello" "world"))
    , Test.ok "lt true" (String.lt "apple" "banana")
    , Test.ok "lt false" (Bool.negate (String.lt "zebra" "apple"))
    ]

-- length
-- isEmpty
-- concat
-- substring
-- charAt
-- head
-- tail
-- take
-- drop
-- toList / fromList
-- split / join
-- trim
-- toUpper / toLower
-- contains
-- startsWith / endsWith
-- replace
-- reverse
-- repeat
-- indexOf / lastIndexOf
-- padLeft / padRight
-- lines / unlines
-- words / unwords
-- eq / lt (ordering)
let main =
  args -> Test.run tests
