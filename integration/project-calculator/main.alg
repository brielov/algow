-- Project: Expression Calculator
-- A complete expression evaluator with AST, parser, and evaluator
-- Features: arithmetic, variables, let bindings

-- Token types for lexer
type Token
  = TokNum int
  | TokIdent string
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokLParen
  | TokRParen
  | TokEquals
  | TokLet
  | TokIn
  | TokEOF

-- AST for expressions
type Expr
  = Num int
  | Var string
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  | Let string Expr Expr

-- Environment for variable bindings
type Env = Env (List (string, int))

let emptyEnv = Env []

let lookupEnv = name env ->
  match env when Env pairs ->
    let rec find = ps -> match ps
      when Nil -> Nothing
      when Cons (k, v) rest -> if k == name then Just v else find rest
    end in find pairs
  end

let extendEnv = name value env ->
  match env when Env pairs -> Env (Cons (name, value) pairs) end

-- Lexer: converts string to tokens
let isDigitChar = c -> Char.isDigit c
let isAlphaChar = c -> Char.isAlpha c
let isAlphaNumChar = c -> Char.isAlphaNum c || c == '_'
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

let rec readIdent = acc chars -> match chars
  when Nil -> (String.fromList (List.reverse acc), Nil)
  when Cons c rest ->
    if isAlphaNumChar c
    then readIdent (Cons c acc) rest
    else (String.fromList (List.reverse acc), chars)
end

let rec tokenize = chars ->
  let chars_ = skipWhitespace chars in
  match chars_
    when Nil -> [TokEOF]
    when Cons c rest ->
      if isDigitChar c then
        match readNumber 0 chars_ when (n, rest_) -> Cons (TokNum n) (tokenize rest_) end
      else if isAlphaChar c then
        match readIdent [] chars_ when (ident, rest_) ->
          let tok = if ident == "let" then TokLet
                    else if ident == "in" then TokIn
                    else TokIdent ident
          in Cons tok (tokenize rest_)
        end
      else if c == '+' then Cons TokPlus (tokenize rest)
      else if c == '-' then Cons TokMinus (tokenize rest)
      else if c == '*' then Cons TokStar (tokenize rest)
      else if c == '/' then Cons TokSlash (tokenize rest)
      else if c == '(' then Cons TokLParen (tokenize rest)
      else if c == ')' then Cons TokRParen (tokenize rest)
      else if c == '=' then Cons TokEquals (tokenize rest)
      else tokenize rest
  end

let lex = s -> tokenize (String.toList s)

-- Parser: converts tokens to AST
-- Grammar:
--   expr    = let | additive
--   let     = "let" IDENT "=" expr "in" expr
--   additive = multiplicative (('+' | '-') multiplicative)*
--   multiplicative = unary (('*' | '/') unary)*
--   unary   = '-' unary | primary
--   primary = NUM | IDENT | '(' expr ')'

type ParseResult a = ParseOk a (List Token) | ParseErr string

let parseExpr = tokens -> parseLet tokens

and parseLet = tokens -> match tokens
  when Cons TokLet (Cons (TokIdent name) (Cons TokEquals rest)) ->
    match parseExpr rest
      when ParseErr e -> ParseErr e
      when ParseOk value rest2 -> match rest2
        when Cons TokIn rest3 ->
          match parseExpr rest3
            when ParseErr e -> ParseErr e
            when ParseOk body rest4 -> ParseOk (Let name value body) rest4
          end
        when _ -> ParseErr "expected 'in'"
      end
    end
  when _ -> parseAdditive tokens
end

and parseAdditive = tokens ->
  match parseMultiplicative tokens
    when ParseErr e -> ParseErr e
    when ParseOk left rest -> parseAdditiveCont left rest
  end

and parseAdditiveCont = left tokens -> match tokens
  when Cons TokPlus rest ->
    match parseMultiplicative rest
      when ParseErr e -> ParseErr e
      when ParseOk right rest2 -> parseAdditiveCont (Add left right) rest2
    end
  when Cons TokMinus rest ->
    match parseMultiplicative rest
      when ParseErr e -> ParseErr e
      when ParseOk right rest2 -> parseAdditiveCont (Sub left right) rest2
    end
  when _ -> ParseOk left tokens
end

and parseMultiplicative = tokens ->
  match parseUnary tokens
    when ParseErr e -> ParseErr e
    when ParseOk left rest -> parseMultiplicativeCont left rest
  end

and parseMultiplicativeCont = left tokens -> match tokens
  when Cons TokStar rest ->
    match parseUnary rest
      when ParseErr e -> ParseErr e
      when ParseOk right rest2 -> parseMultiplicativeCont (Mul left right) rest2
    end
  when Cons TokSlash rest ->
    match parseUnary rest
      when ParseErr e -> ParseErr e
      when ParseOk right rest2 -> parseMultiplicativeCont (Div left right) rest2
    end
  when _ -> ParseOk left tokens
end

and parseUnary = tokens -> match tokens
  when Cons TokMinus rest ->
    match parseUnary rest
      when ParseErr e -> ParseErr e
      when ParseOk e rest2 -> ParseOk (Neg e) rest2
    end
  when _ -> parsePrimary tokens
end

and parsePrimary = tokens -> match tokens
  when Cons (TokNum n) rest -> ParseOk (Num n) rest
  when Cons (TokIdent name) rest -> ParseOk (Var name) rest
  when Cons TokLParen rest ->
    match parseExpr rest
      when ParseErr e -> ParseErr e
      when ParseOk e rest2 -> match rest2
        when Cons TokRParen rest3 -> ParseOk e rest3
        when _ -> ParseErr "expected ')'"
      end
    end
  when _ -> ParseErr "unexpected token"
end

let parse = s ->
  let tokens = lex s in
  match parseExpr tokens
    when ParseErr e -> Left e
    when ParseOk expr rest -> match rest
      when Cons TokEOF Nil -> Right expr
      when Cons TokEOF _ -> Right expr
      when _ -> Left "unexpected tokens after expression"
    end
  end

-- Evaluator: evaluates AST to int
let rec eval = env expr -> match expr
  when Num n -> Right n
  when Var name -> match lookupEnv name env
    when Nothing -> Left (String.concat "undefined variable: " name)
    when Just v -> Right v
  end
  when Add l r -> match (eval env l, eval env r)
    when (Right a, Right b) -> Right (a + b)
    when (Left e, _) -> Left e
    when (_, Left e) -> Left e
  end
  when Sub l r -> match (eval env l, eval env r)
    when (Right a, Right b) -> Right (a - b)
    when (Left e, _) -> Left e
    when (_, Left e) -> Left e
  end
  when Mul l r -> match (eval env l, eval env r)
    when (Right a, Right b) -> Right (a * b)
    when (Left e, _) -> Left e
    when (_, Left e) -> Left e
  end
  when Div l r -> match (eval env l, eval env r)
    when (Right a, Right b) ->
      if b == 0 then Left "division by zero" else Right (Int.div a b)
    when (Left e, _) -> Left e
    when (_, Left e) -> Left e
  end
  when Neg e -> match eval env e
    when Right n -> Right (0 - n)
    when Left err -> Left err
  end
  when Let name value body ->
    match eval env value
      when Left err -> Left err
      when Right v -> eval (extendEnv name v env) body
    end
end

-- Main calculate function
let calculate = s ->
  match parse s
    when Left err -> Left err
    when Right expr -> eval emptyEnv expr
  end

-- Tests
let tests =
  [ -- Basic arithmetic
    Test.equal "number" (Right 42) (calculate "42")
  , Test.equal "addition" (Right 15) (calculate "10 + 5")
  , Test.equal "subtraction" (Right 5) (calculate "10 - 5")
  , Test.equal "multiplication" (Right 50) (calculate "10 * 5")
  , Test.equal "division" (Right 2) (calculate "10 / 5")
  , Test.equal "negation" (Right (-5)) (calculate "-5")
  , Test.equal "double negation" (Right 5) (calculate "--5")
    -- Precedence
  , Test.equal "mul before add" (Right 14) (calculate "2 + 3 * 4")
  , Test.equal "parens override" (Right 20) (calculate "(2 + 3) * 4")
  , Test.equal "complex precedence" (Right 23) (calculate "1 + 2 * 3 + 4 * 4")
    -- Let bindings
  , Test.equal "let simple" (Right 42) (calculate "let x = 42 in x")
  , Test.equal "let with expr" (Right 12) (calculate "let x = 5 in x + 7")
  , Test.equal "let nested" (Right 30) (calculate "let x = 10 in let y = 20 in x + y")
  , Test.equal "let shadowing" (Right 99) (calculate "let x = 10 in let x = 99 in x")
  , Test.equal "let in expr" (Right 100) (calculate "let x = 10 in x * x")
    -- Complex expressions
  , Test.equal "complex 1" (Right 35) (calculate "let a = 5 in let b = 7 in a * b")
  , Test.equal "complex 2" (Right 15) (calculate "(2 + 3) * (4 - 1)")
  , Test.equal "complex 3" (Right 13) (calculate "let x = 3 in let y = x + 1 in x * y + 1")
    -- Error cases
  , Test.ok "undefined var" (Either.isLeft (calculate "x + 1"))
  , Test.ok "division by zero" (Either.isLeft (calculate "10 / 0"))
    -- Whitespace handling
  , Test.equal "extra spaces" (Right 5) (calculate "  2   +   3  ")
  , Test.equal "no spaces" (Right 5) (calculate "2+3")
    -- Edge cases
  , Test.equal "zero" (Right 0) (calculate "0")
  , Test.equal "negative result" (Right (-5)) (calculate "5 - 10")
  , Test.equal "chained ops" (Right 10) (calculate "1 + 2 + 3 + 4")
  , Test.equal "nested parens" (Right 6) (calculate "((((1 + 2) + 3)))")
  ]

let main = args -> Test.run tests
