-- Stdlib: Regex module
let tests =
  [ Test.ok "test digits" (Regex.test "\\d+" "abc123")
    , Test.ok "test no match" (Bool.negate (Regex.test "\\d+" "abc"))
    , Test.ok "test case sensitive" (Bool.negate (Regex.test "HELLO" "hello"))
    , Test.ok "test case insensitive" (Regex.test "/hello/i" "HELLO")
    , Test.ok "test anchors" (Regex.test "^hello" "hello world")
    , Test.ok "test anchors no match" (Bool.negate (Regex.test "^world" "hello world"))
    , Test.ok "test word boundary" (Regex.test "\\bfoo\\b" "a foo b")
    , Test.ok "test multiline" (Regex.test "/^bar/m" "foo\nbar")
    , Test.ok "find found" (Maybe.isJust (Regex.find "foo" "foobar"))
    , Test.ok "find not found" (Maybe.isNothing (Regex.find "xyz" "foobar"))
    , Test.ok "find value" (match Regex.find "\\d+" "abc123def"
      when Just m -> m.value == "123"
      when Nothing -> false
    end)
    , Test.ok "find index" (match Regex.find "\\d+" "abc123def"
      when Just m -> m.index == 3
      when Nothing -> false
    end)
    , Test.ok "find groups" (match Regex.find "(\\w+)@(\\w+)" "foo@bar.com"
      when Just m -> List.length m.groups == 2
      when Nothing -> false
    end)
    , Test.ok "find group values" (match Regex.find "(\\w+)@(\\w+)" "foo@bar.com"
      when Just m -> match m.groups
        when Cons g1 (Cons g2 Nil) -> g1 == "foo" && g2 == "bar"
        when _ -> false
      end
      when Nothing -> false
    end)
    , Test.ok "findAll count" (List.length (Regex.findAll "\\d" "a1b2c3") == 3)
    , Test.ok "findAll empty" (List.isEmpty (Regex.findAll "\\d" "abc"))
    , Test.ok "findAll values" (let matches = Regex.findAll "\\d+" "a12b34c56" in
    -- test: basic matching
    -- find: single match
    -- findAll: find all matches
    let getValue = m -> m.value in
    let values = List.map getValue matches in
    match values
      when Cons "12" (Cons "34" (Cons "56" Nil)) -> true
      when _ -> false
    end)
    , Test.ok "findAll indices" (let matches = Regex.findAll "\\d" "a1b2c3" in
    let getIndex = m -> m.index in
    let indices = List.map getIndex matches in
    match indices
      when Cons 1 (Cons 3 (Cons 5 Nil)) -> true
      when _ -> false
    end)
    , Test.equal "replace first" "hi world hello" (Regex.replace "hello" "hi" "hello world hello")
    , Test.equal "replace pattern" "X23" (Regex.replace "\\d" "X" "123")
    , Test.equal "replace no match" "hello" (Regex.replace "xyz" "abc" "hello")
    , Test.equal "replace with capture" "world hello" (Regex.replace "(\\w+) (\\w+)" "$2 $1" "hello world")
    , Test.equal "replaceAll" "X-X-X" (Regex.replaceAll "\\d" "X" "1-2-3")
    , Test.equal "replaceAll pattern" "a#b#c#" (Regex.replaceAll "\\d" "#" "a1b2c3")
    , Test.equal "replaceAll spaces" "hello-world-foo" (Regex.replaceAll "\\s+" "-" "hello world foo")
    , Test.equal "replaceAll with flag" "X-X-X" (Regex.replaceAll "/\\d/g" "X" "1-2-3")
    , Test.equal "split comma" ["a", "b", "c"] (Regex.split "," "a,b,c")
    , Test.equal "split whitespace" ["hello", "world"] (Regex.split "\\s+" "hello   world")
    , Test.equal "split multi-char" ["a", "b", "c"] (Regex.split "\\s*,\\s*" "a , b,  c")
    , Test.equal "split no match" ["hello"] (Regex.split "x" "hello")
    , Test.equal "splitLimit basic" ["a", "b,c,d"] (Regex.splitLimit "," 2 "a,b,c,d")
    , Test.equal "splitLimit all" ["a", "b", "c"] (Regex.splitLimit "," 10 "a,b,c")
    , Test.equal "splitLimit one" ["a,b,c"] (Regex.splitLimit "," 1 "a,b,c")
    , Test.equal "splitLimit zero" ["a", "b", "c"] (Regex.splitLimit "," 0 "a,b,c")
    ]

-- replace: first occurrence
-- replaceAll: all occurrences
-- split: by pattern
-- splitLimit: with max parts
let main =
  args -> Test.run tests
