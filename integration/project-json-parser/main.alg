-- Project: JSON Parser from scratch
-- A complete JSON parser implementing the JSON specification
-- JSON Value type
type JValue
  = JNull
  | JBool bool
  | JNumber float
  | JString string
  | JArray (List JValue)
  | JObject (List (string, JValue))

-- Parser type with remaining input
type ParseResult a
  = Success a string
  | Failure string

-- Basic parser combinators
let succeed =
  v input -> Success v input

let fail =
  msg input -> Failure msg

let char =
  c input -> if String.isEmpty input
    then Failure "unexpected end of input"
    else match String.head input
      when Nothing -> Failure "unexpected end of input"
      when Just ch -> if ch == c
        then Success ch (String.tail input)
        else Failure (String.concat "expected '" (String.concat (Char.toString c) "'"))
    end

let satisfies =
  pred input -> if String.isEmpty input
    then Failure "unexpected end of input"
    else match String.head input
      when Nothing -> Failure "unexpected end of input"
      when Just ch -> if pred ch
        then Success ch (String.tail input)
        else Failure "character did not satisfy predicate"
    end

let rec skipWhile = pred input -> if String.isEmpty input
  then input
  else match String.head input
    when Nothing -> input
    when Just ch -> if pred ch
      then skipWhile pred (String.tail input)
      else input
  end

let skipSpaces =
  input -> skipWhile Char.isSpace input

-- String parsing with escape handling
let rec parseStringContent = acc input -> if String.isEmpty input
  then Failure "unterminated string"
  else match String.head input
    when Nothing -> Failure "unterminated string"
    when Just '"' -> Success (String.fromList (List.reverse acc)) (String.tail input)
    when Just '\\' -> let rest = String.tail input in
    if String.isEmpty rest
      then Failure "unterminated escape"
      else match String.head rest
        when Nothing -> Failure "unterminated escape"
        when Just 'n' -> parseStringContent ('\n' :: acc) (String.tail rest)
        when Just 't' -> parseStringContent ('\t' :: acc) (String.tail rest)
        when Just 'r' -> parseStringContent ('\r' :: acc) (String.tail rest)
        when Just '\\' -> parseStringContent ('\\' :: acc) (String.tail rest)
        when Just '"' -> parseStringContent ('"' :: acc) (String.tail rest)
        when Just c -> parseStringContent (c :: acc) (String.tail rest)
      end
    when Just c -> parseStringContent (c :: acc) (String.tail input)
  end

let parseString =
  input -> let input_ = skipSpaces input in
  match char '"' input_
    when Failure e -> Failure e
    when Success _ rest -> parseStringContent [] rest
  end

-- Number parsing
let rec collectDigits = acc input -> if String.isEmpty input
  then (acc, input)
  else match String.head input
    when Nothing -> (acc, input)
    when Just c -> if Char.isDigit c
      then collectDigits (c :: acc) (String.tail input)
      else (acc, input)
  end

let parseNumber =
  input -> let input_ = skipSpaces input in
  match match String.head input_
    when Nothing -> (false, input_)
    when Just '-' -> (true, String.tail input_)
    when Just _ -> (false, input_)
  end
    when (isNeg, rest1) -> match collectDigits [] rest1
      when ([], _) -> Failure "expected number"
      when (digits, rest2) -> let intPart = String.fromList (List.reverse digits) in
      match String.head rest2
        when Just '.' -> let rest3 = String.tail rest2 in
        match collectDigits [] rest3
          when (fracDigits, rest4) -> let fracPart = String.fromList (List.reverse fracDigits) in
          let numStr = String.concat intPart (String.concat "." fracPart) in
          let fullStr = if isNeg
            then String.concat "-" numStr
            else numStr in
          match Float.fromString fullStr
            when Nothing -> Failure "invalid number"
            when Just n -> Success n rest4
          end
        end
        when _ -> let fullStr = if isNeg
          then String.concat "-" intPart
          else intPart in
        match Float.fromString fullStr
          when Nothing -> Failure "invalid number"
          when Just n -> Success n rest2
        end
      end
    end
  end

-- Keyword parsing
let keyword =
  word input -> let input_ = skipSpaces input in
  let len = String.length word in
  if String.take len input_ == word
    then Success word (String.drop len input_)
    else Failure (String.concat "expected " word)

-- Forward declarations via mutual recursion
let rec parseValue = input -> let input_ = skipSpaces input in
if String.isEmpty input_
  then Failure "unexpected end of input"
  else match String.head input_
    when Nothing -> Failure "unexpected end of input"
    when Just '"' -> match parseString input_
      when Success s rest -> Success (JString s) rest
      when Failure e -> Failure e
    end
    when Just '[' -> parseArray input_
    when Just '{' -> parseObject input_
    when Just 'n' -> match keyword "null" input_
      when Success _ rest -> Success JNull rest
      when Failure e -> Failure e
    end
    when Just 't' -> match keyword "true" input_
      when Success _ rest -> Success (JBool true) rest
      when Failure e -> Failure e
    end
    when Just 'f' -> match keyword "false" input_
      when Success _ rest -> Success (JBool false) rest
      when Failure e -> Failure e
    end
    when Just c -> if c == '-' || Char.isDigit c
      then match parseNumber input_
        when Success n rest -> Success (JNumber n) rest
        when Failure e -> Failure e
      end
      else Failure "unexpected character"
  end
and parseArray = input -> let input_ = skipSpaces input in
match char '[' input_
  when Failure e -> Failure e
  when Success _ rest -> let rest_ = skipSpaces rest in
  match String.head rest_
    when Just ']' -> Success (JArray []) (String.tail rest_)
    when _ -> parseArrayElements [] rest_
  end
end
and parseArrayElements = acc input ->
  match parseValue input
    when Failure e -> Failure e
    when Success v rest -> let rest_ = skipSpaces rest in
    match String.head rest_
      when Just ',' -> parseArrayElements (v :: acc) (String.tail rest_)
      when Just ']' -> Success (JArray (List.reverse (v :: acc))) (String.tail rest_)
      when _ -> Failure "expected ',' or ']'"
    end
  end
and parseObject = input -> let input_ = skipSpaces input in
match char '{' input_
  when Failure e -> Failure e
  when Success _ rest -> let rest_ = skipSpaces rest in
  match String.head rest_
    when Just '}' -> Success (JObject []) (String.tail rest_)
    when _ -> parseObjectMembers [] rest_
  end
end
and parseObjectMembers = acc input ->
  match parseString input
    when Failure e -> Failure e
    when Success key rest -> let rest_ = skipSpaces rest in
    match char ':' rest_
      when Failure e -> Failure e
      when Success _ rest2 -> match parseValue rest2
        when Failure e -> Failure e
        when Success value rest3 -> let rest3_ = skipSpaces rest3 in
        match String.head rest3_
          when Just ',' -> parseObjectMembers ((key, value) :: acc) (String.tail rest3_)
          when Just '}' -> Success (JObject (List.reverse ((key, value) :: acc))) (String.tail rest3_)
          when _ -> Failure "expected ',' or '}'"
        end
      end
    end
  end

-- Main parse function
let parse =
  input ->
    match parseValue input
      when Failure e -> Left e
      when Success v rest -> let rest_ = skipSpaces rest in
      if String.isEmpty rest_
        then Right v
        else Left "unexpected trailing characters"
    end

-- Accessors
let getString =
  v ->
    match v
      when JString s -> Just s
      when _ -> Nothing
    end

let getNumber =
  v ->
    match v
      when JNumber n -> Just n
      when _ -> Nothing
    end

let getBool =
  v ->
    match v
      when JBool b -> Just b
      when _ -> Nothing
    end

let getArray =
  v ->
    match v
      when JArray a -> Just a
      when _ -> Nothing
    end

let getObject =
  v ->
    match v
      when JObject o -> Just o
      when _ -> Nothing
    end

let isNull =
  v ->
    match v
      when JNull -> true
      when _ -> false
    end

let rec getField = key obj ->
  match obj
    when Nil -> Nothing
    when Cons (k, v) rest -> if k == key
      then Just v
      else getField key rest
  end

let at =
  key v ->
    match v
      when JObject pairs -> getField key pairs
      when _ -> Nothing
    end

-- Tests
let tests =
  [ Test.equal "parse null" (Right JNull) (parse "null")
    , Test.equal "parse null spaces" (Right JNull) (parse "  null  ")
    , Test.equal "parse true" (Right (JBool true)) (parse "true")
    , Test.equal "parse false" (Right (JBool false)) (parse "false")
    , Test.equal "parse int" (Right (JNumber 42.0)) (parse "42")
    , Test.equal "parse negative" (Right (JNumber (0 - 42.0))) (parse "-42")
    , Test.equal "parse float" (Right (JNumber 3.14)) (parse "3.14")
    , Test.equal "parse negative float" (Right (JNumber (0 - 3.14))) (parse "-3.14")
    , Test.equal "parse zero" (Right (JNumber 0.0)) (parse "0")
    , Test.equal "parse string" (Right (JString "hello")) (parse "\"hello\"")
    , Test.equal "parse empty string" (Right (JString "")) (parse "\"\"")
    , Test.equal "parse string escapes" (Right (JString "a\nb")) (parse "\"a\\nb\"")
    , Test.equal "parse string tab" (Right (JString "a\tb")) (parse "\"a\\tb\"")
    , Test.equal "parse string quote" (Right (JString "a\"b")) (parse "\"a\\\"b\"")
    , Test.equal "parse empty array" (Right (JArray [])) (parse "[]")
    , Test.equal "parse array one" (Right (JArray [JNumber 1.0])) (parse "[1]")
    , Test.equal "parse array nums" (Right (JArray [JNumber 1.0, JNumber 2.0, JNumber 3.0])) (parse "[1, 2, 3]")
    , Test.equal "parse array mixed" (Right (JArray [JNumber 1.0, JString "two", JBool true])) (parse "[1, \"two\", true]")
    , Test.equal "parse nested array" (Right (JArray [JArray [JNumber 1.0, JNumber 2.0]])) (parse "[[1, 2]]")
    , Test.equal "parse empty object" (Right (JObject [])) (parse "{}")
    , Test.equal "parse object one" (Right (JObject [("x", JNumber 1.0)])) (parse "{\"x\": 1}")
    , Test.equal "parse object two" (Right (JObject [("x", JNumber 1.0), ("y", JNumber 2.0)])) (parse "{\"x\": 1, \"y\": 2}")
    , Test.equal "parse nested object" (Right (JObject [("p", JObject [("x", JNumber 10.0)])])) (parse "{\"p\": {\"x\": 10}}")
    , Test.ok "parse complex" (Either.isRight (parse "{\"name\": \"Alice\", \"age\": 30, \"active\": true, \"tags\": [\"a\", \"b\"]}"))
    , Test.equal "parse with whitespace" (Right (JArray [JNumber 1.0, JNumber 2.0])) (parse "  [  1  ,  2  ]  ")
    , Test.equal "getString" (Just "hello") (match parse "\"hello\""
      when Right v -> getString v
      when Left _ -> Nothing
    end)
    , Test.equal "getNumber" (Just 42.0) (match parse "42"
      when Right v -> getNumber v
      when Left _ -> Nothing
    end)
    , Test.equal "getBool" (Just true) (match parse "true"
      when Right v -> getBool v
      when Left _ -> Nothing
    end)
    , Test.ok "isNull" (match parse "null"
      when Right v -> isNull v
      when Left _ -> false
    end)
    , Test.equal "at field" (Just (JNumber 10.0)) (match parse "{\"x\": 10}"
      when Right v -> at "x" v
      when Left _ -> Nothing
    end)
    , Test.ok "error unterminated string" (Either.isLeft (parse "\"hello"))
    , Test.ok "error invalid keyword" (Either.isLeft (parse "tru"))
    , Test.ok "error trailing comma" (Either.isLeft (parse "[1,]"))
    , Test.ok "error missing colon" (Either.isLeft (parse "{\"x\" 1}"))
    ]

-- Null
-- Booleans
-- Numbers
-- Strings
-- Arrays
-- Objects
-- Complex JSON
-- Whitespace handling
-- Accessors
-- Error cases
let main =
  args -> Test.run tests
