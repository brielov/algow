-- Project: Mini Lisp Interpreter
-- A complete S-expression interpreter with primitives and special forms

-- S-expression type (no closure to avoid circular type)
type SExpr
  = SNum int
  | SSymbol string
  | SNil
  | SCons SExpr SExpr
  | SPrimitive string

-- Environment is a simple list of bindings
type Env = Env (List (string, SExpr))

-- Closure stored separately with environment reference
type Closure = Closure (List string) SExpr Env

-- Result type
type Result a = Ok a | Err string

-- Environment operations
let emptyEnv = Env []

let rec lookupEnv = name env -> match env when Env pairs ->
  let rec find = ps -> match ps
    when Nil -> Nothing
    when Cons (k, v) rest -> if k == name then Just v else find rest
  end in find pairs
end

let extendEnv = name value env -> match env when Env pairs ->
  Env (Cons (name, value) pairs)
end

let rec extendEnvMany = names values env -> match (names, values)
  when (Nil, _) -> env
  when (_, Nil) -> env
  when (Cons n ns, Cons v vs) -> extendEnvMany ns vs (extendEnv n v env)
end

-- Parser helpers
let isDigitChar = c -> Char.isDigit c
let isSymbolStartChar = c -> Char.isAlpha c || c == '+' || c == '-' || c == '*' || c == '/' || c == '=' || c == '<' || c == '>' || c == '?' || c == '!'
let isSymbolChar = c -> isSymbolStartChar c || Char.isDigit c
let isSpaceChar = c -> Char.isSpace c

let rec skipWhitespace = chars -> match chars
  when Nil -> Nil
  when Cons c rest -> if isSpaceChar c then skipWhitespace rest else chars
end

let rec readNumber = acc chars -> match chars
  when Nil -> (acc, Nil)
  when Cons c rest ->
    if isDigitChar c
    then readNumber (acc * 10 + Char.toInt c - 48) rest
    else (acc, chars)
end

let rec readSymbol = acc chars -> match chars
  when Nil -> (String.fromList (List.reverse acc), Nil)
  when Cons c rest ->
    if isSymbolChar c
    then readSymbol (Cons c acc) rest
    else (String.fromList (List.reverse acc), chars)
end

-- Parser (forward declarations via mutual recursion)
let rec parseExpr = chars ->
  let chars_ = skipWhitespace chars in
  match chars_
    when Nil -> Err "unexpected end of input"
    when Cons c rest ->
      if isDigitChar c then
        match readNumber 0 chars_ when (n, rest_) -> Ok (SNum n, rest_) end
      else if c == '-' && Bool.negate (List.isEmpty rest) then
        match String.head (String.fromList rest)
          when Just c2 ->
            if isDigitChar c2 then
              match readNumber 0 rest when (n, rest_) -> Ok (SNum (0 - n), rest_) end
            else
              match readSymbol [c] rest when (sym, rest_) -> Ok (SSymbol sym, rest_) end
          when Nothing -> match readSymbol [c] rest when (sym, rest_) -> Ok (SSymbol sym, rest_) end
        end
      else if isSymbolStartChar c then
        match readSymbol [] chars_ when (sym, rest_) ->
          if sym == "nil" then Ok (SNil, rest_)
          else Ok (SSymbol sym, rest_)
        end
      else if c == '(' then
        parseList rest
      else if c == '\'' then
        match parseExpr rest
          when Err e -> Err e
          when Ok (expr, rest_) -> Ok (SCons (SSymbol "quote") (SCons expr SNil), rest_)
        end
      else Err "unexpected character"
  end

and parseList = chars ->
  let chars_ = skipWhitespace chars in
  match chars_
    when Nil -> Err "unexpected end of list"
    when Cons ')' rest -> Ok (SNil, rest)
    when Cons c_ rest_ ->
      match parseExpr chars_
        when Err e -> Err e
        when Ok (first, rest) ->
          match parseListTail rest
            when Err e -> Err e
            when Ok (tail, rest2) -> Ok (SCons first tail, rest2)
          end
      end
  end

and parseListTail = chars ->
  let chars_ = skipWhitespace chars in
  match chars_
    when Nil -> Err "unexpected end of list"
    when Cons ')' rest -> Ok (SNil, rest)
    when Cons c_ rest_ ->
      match parseExpr chars_
        when Err e -> Err e
        when Ok (expr, rest) ->
          match parseListTail rest
            when Err e -> Err e
            when Ok (tail, rest2) -> Ok (SCons expr tail, rest2)
          end
      end
  end

let parse = s ->
  match parseExpr (String.toList s)
    when Err e -> Err e
    when Ok (expr, rest) ->
      let rest_ = skipWhitespace rest in
      if List.isEmpty rest_ then Ok expr
      else Err "trailing characters"
  end

-- Helper to check truthiness
let isTrue = expr -> match expr when SNil -> false when _ -> true end

-- Convert sexpr list to algow list
let rec listToAlgowList = sexpr ->
  let rec go = s acc -> match s
    when SNil -> List.reverse acc
    when SCons h t -> go t (Cons h acc)
    when _ -> List.reverse (Cons s acc)
  end in go sexpr []

-- Extract parameter names from sexpr
let rec extractParams = params ->
  let rec go = ps acc -> match ps
    when SNil -> Ok (List.reverse acc)
    when SCons (SSymbol name) rest -> go rest (Cons name acc)
    when _ -> Err "invalid parameter"
  end in go params []

-- Primitive operations
let applyPrimitive = name args -> match name
  when "+" -> match args
    when SCons (SNum a) (SCons (SNum b) SNil) -> Ok (SNum (a + b))
    when _ -> Err "+ requires two numbers"
  end
  when "-" -> match args
    when SCons (SNum a) (SCons (SNum b) SNil) -> Ok (SNum (a - b))
    when _ -> Err "- requires two numbers"
  end
  when "*" -> match args
    when SCons (SNum a) (SCons (SNum b) SNil) -> Ok (SNum (a * b))
    when _ -> Err "* requires two numbers"
  end
  when "/" -> match args
    when SCons (SNum a) (SCons (SNum b) SNil) ->
      if b == 0 then Err "division by zero"
      else Ok (SNum (Int.div a b))
    when _ -> Err "/ requires two numbers"
  end
  when "=" -> match args
    when SCons (SNum a) (SCons (SNum b) SNil) -> Ok (if a == b then SSymbol "t" else SNil)
    when _ -> Err "= requires two numbers"
  end
  when "<" -> match args
    when SCons (SNum a) (SCons (SNum b) SNil) -> Ok (if a < b then SSymbol "t" else SNil)
    when _ -> Err "< requires two numbers"
  end
  when ">" -> match args
    when SCons (SNum a) (SCons (SNum b) SNil) -> Ok (if a > b then SSymbol "t" else SNil)
    when _ -> Err "> requires two numbers"
  end
  when "cons" -> match args
    when SCons a (SCons b SNil) -> Ok (SCons a b)
    when _ -> Err "cons requires two arguments"
  end
  when "car" -> match args
    when SCons (SCons a d) SNil -> Ok a
    when _ -> Err "car requires a cons"
  end
  when "cdr" -> match args
    when SCons (SCons a d) SNil -> Ok d
    when _ -> Err "cdr requires a cons"
  end
  when "null?" -> match args
    when SCons SNil SNil -> Ok (SSymbol "t")
    when SCons _ SNil -> Ok SNil
    when _ -> Err "null? requires one argument"
  end
  when "number?" -> match args
    when SCons (SNum _) SNil -> Ok (SSymbol "t")
    when SCons _ SNil -> Ok SNil
    when _ -> Err "number? requires one argument"
  end
  when _ -> Err (String.concat "unknown primitive: " name)
end

-- Standard environment with primitives
let standardEnv =
  let prims = ["+", "-", "*", "/", "=", "<", ">", "cons", "car", "cdr", "null?", "number?"] in
  List.foldl (env name -> extendEnv name (SPrimitive name) env) emptyEnv prims

-- Simple evaluator (no closures for simplicity)
let rec eval = env expr -> match expr
  when SNum n -> Ok (SNum n)
  when SSymbol name ->
    if name == "nil" then Ok SNil
    else if name == "t" then Ok (SSymbol "t")
    else match lookupEnv name env
      when Nothing -> Err (String.concat "unbound symbol: " name)
      when Just v -> Ok v
    end
  when SNil -> Ok SNil
  when SCons head tail -> evalForm env head tail
  when SPrimitive _ -> Ok expr
end

and evalForm = env head tail -> match head
  when SSymbol "quote" -> match tail
    when SCons arg SNil -> Ok arg
    when _ -> Err "quote requires one argument"
  end

  when SSymbol "if" -> match tail
    when SCons cond (SCons then_ (SCons else_ SNil)) ->
      match eval env cond
        when Err e -> Err e
        when Ok condVal ->
          if isTrue condVal
          then eval env then_
          else eval env else_
      end
    when _ -> Err "if requires three arguments"
  end

  when SSymbol "begin" -> evalBegin env tail

  when SSymbol "let" -> match tail
    when SCons bindings (SCons body SNil) ->
      match evalLetBindings env bindings
        when Err e -> Err e
        when Ok env_ -> eval env_ body
      end
    when _ -> Err "let requires bindings and body"
  end

  when _ ->
    match eval env head
      when Err e -> Err e
      when Ok func -> match evalList env tail
        when Err e -> Err e
        when Ok args -> apply func args
      end
    end
end

and evalList = env exprs -> match exprs
  when SNil -> Ok SNil
  when SCons first rest ->
    match eval env first
      when Err e -> Err e
      when Ok firstVal ->
        match evalList env rest
          when Err e -> Err e
          when Ok restVals -> Ok (SCons firstVal restVals)
        end
    end
  when _ -> Err "invalid list"
end

and evalBegin = env exprs -> match exprs
  when SNil -> Ok SNil
  when SCons last SNil -> eval env last
  when SCons first rest ->
    match eval env first
      when Err e -> Err e
      when Ok _ -> evalBegin env rest
    end
  when _ -> Err "invalid begin"
end

and evalLetBindings = env bindings -> match bindings
  when SNil -> Ok env
  when SCons (SCons (SSymbol name) (SCons value SNil)) rest ->
    match eval env value
      when Err e -> Err e
      when Ok val -> evalLetBindings (extendEnv name val env) rest
    end
  when _ -> Err "invalid let bindings"
end

and apply = func args -> match func
  when SPrimitive name -> applyPrimitive name args
  when _ -> Err "not a function"
end

-- Run function
let run = code ->
  match parse code
    when Err e -> Err e
    when Ok expr -> eval standardEnv expr
  end

-- Get number from result
let getNum = result -> match result
  when Ok (SNum n) -> Just n
  when _ -> Nothing
end

-- Check for true result
let isOk = result -> match result when Ok _ -> true when Err _ -> false end

-- Tests
let tests =
  [ -- Numbers
    Test.ok "number" (match getNum (run "42") when Just 42 -> true when _ -> false end)
  , Test.ok "negative" (match getNum (run "-5") when Just n -> n == 0 - 5 when _ -> false end)

    -- Arithmetic
  , Test.ok "add" (match getNum (run "(+ 2 3)") when Just 5 -> true when _ -> false end)
  , Test.ok "sub" (match getNum (run "(- 5 2)") when Just 3 -> true when _ -> false end)
  , Test.ok "mul" (match getNum (run "(* 3 4)") when Just 12 -> true when _ -> false end)
  , Test.ok "div" (match getNum (run "(/ 10 3)") when Just 3 -> true when _ -> false end)
  , Test.ok "nested arith" (match getNum (run "(+ (* 2 3) (* 2 4))") when Just 14 -> true when _ -> false end)

    -- Comparisons
  , Test.ok "eq true" (isTrue (match run "(= 5 5)" when Ok v -> v when Err _ -> SNil end))
  , Test.ok "eq false" (Bool.negate (isTrue (match run "(= 5 6)" when Ok v -> v when Err _ -> SSymbol "t" end)))
  , Test.ok "lt true" (isTrue (match run "(< 3 5)" when Ok v -> v when Err _ -> SNil end))
  , Test.ok "gt true" (isTrue (match run "(> 5 3)" when Ok v -> v when Err _ -> SNil end))

    -- Quote
  , Test.ok "quote symbol" (match run "'x" when Ok (SSymbol "x") -> true when _ -> false end)
  , Test.ok "quote list" (match run "'(1 2 3)" when Ok (SCons _ _) -> true when _ -> false end)

    -- If
  , Test.ok "if true" (match getNum (run "(if t 1 2)") when Just 1 -> true when _ -> false end)
  , Test.ok "if false" (match getNum (run "(if nil 1 2)") when Just 2 -> true when _ -> false end)
  , Test.ok "if compare" (match getNum (run "(if (< 1 2) 10 20)") when Just 10 -> true when _ -> false end)

    -- Let
  , Test.ok "let simple" (match getNum (run "(let ((x 42)) x)") when Just 42 -> true when _ -> false end)
  , Test.ok "let multiple" (match getNum (run "(let ((x 3) (y 4)) (+ x y))") when Just 7 -> true when _ -> false end)
  , Test.ok "let nested" (match getNum (run "(let ((x 5)) (let ((y 10)) (+ x y)))") when Just 15 -> true when _ -> false end)

    -- Cons, car, cdr
  , Test.ok "cons" (match run "(cons 1 2)" when Ok (SCons (SNum 1) (SNum 2)) -> true when _ -> false end)
  , Test.ok "car" (match getNum (run "(car (cons 1 2))") when Just 1 -> true when _ -> false end)
  , Test.ok "cdr" (match getNum (run "(cdr (cons 1 2))") when Just 2 -> true when _ -> false end)
  , Test.ok "car list" (match getNum (run "(car '(1 2 3))") when Just 1 -> true when _ -> false end)

    -- null?
  , Test.ok "null? nil" (isTrue (match run "(null? nil)" when Ok v -> v when Err _ -> SNil end))
  , Test.ok "null? non-nil" (Bool.negate (isTrue (match run "(null? 1)" when Ok v -> v when Err _ -> SSymbol "t" end)))

    -- number?
  , Test.ok "number? true" (isTrue (match run "(number? 42)" when Ok v -> v when Err _ -> SNil end))
  , Test.ok "number? false" (Bool.negate (isTrue (match run "(number? 'x)" when Ok v -> v when Err _ -> SSymbol "t" end)))

    -- Begin
  , Test.ok "begin" (match getNum (run "(begin 1 2 3)") when Just 3 -> true when _ -> false end)

    -- Error cases
  , Test.ok "unbound var" (Bool.negate (isOk (run "x")))
  , Test.ok "div by zero" (Bool.negate (isOk (run "(/ 10 0)")))
  , Test.ok "car non-cons" (Bool.negate (isOk (run "(car 5)")))
  ]

let main = args -> Test.run tests
