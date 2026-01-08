-- Self-Hosted Lexer
-- A lexer for the Algow language, written in Algow
-- Demonstrates: ADTs, pattern matching, string manipulation, recursion
--
-- Note: Use `List.map` explicitly since multiple modules export `map`.

-- =============================================================================
-- TOKEN TYPES
-- =============================================================================

type Token
  = TkInt Int
  | TkFloat Float
  | TkString string
  | TkChar char
  | TkLower string      -- lowercase identifier
  | TkUpper string      -- uppercase identifier (constructor)
  | TkLet | TkRec | TkIn | TkIf | TkThen | TkElse
  | TkMatch | TkWhen | TkEnd | TkType | TkAs | TkAnd
  | TkTrue | TkFalse
  | TkModule | TkUse | TkForeign
  | TkPlus | TkMinus | TkStar | TkSlash
  | TkLt | TkLe | TkGt | TkGe | TkEqEq | TkNe
  | TkPipe | TkArrow | TkEq | TkBar
  | TkComma | TkDot | TkUnderscore
  | TkColon | TkColonColon
  | TkLParen | TkRParen
  | TkLBrace | TkRBrace
  | TkLBracket | TkRBracket
  | TkEof
  | TkError string

-- Lexer result: current token + remaining source
type LexResult = LexResult Token string

-- =============================================================================
-- CHARACTER HELPERS
-- =============================================================================

-- Get first character of string as Maybe Char
let stringHead s =
  if String.length s == 0
  then Nothing
  else String.charAt 0 s

-- Get string without first character
let stringTail s =
  if String.length s == 0
  then ""
  else String.substring 1 (String.length s) s

-- Check if string is empty
let stringIsEmpty s = String.length s == 0

-- Check if character is whitespace
let isWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- Check if character can start an identifier
let isIdentStart c = Char.isAlpha c || c == '_'

-- Check if character can continue an identifier
let isIdentChar c = Char.isAlphaNum c || c == '_'

-- =============================================================================
-- SKIP WHITESPACE AND COMMENTS
-- =============================================================================

let rec skipWhitespace s =
  match stringHead s
    when Just c if isWhitespace c -> skipWhitespace (stringTail s)
    when _ -> s
  end

-- Skip line comment (starts with --)
let rec skipLineComment s =
  match stringHead s
    when Just '\n' -> stringTail s
    when Just _ -> skipLineComment (stringTail s)
    when Nothing -> s
  end

-- Skip block comment (nested {- -})
let rec skipBlockComment depth s =
  if depth == 0
  then s
  else if String.length s < 2
  then s  -- Unclosed comment, return what's left
  else
    let c1 = String.substring 0 1 s in
    let c2 = String.substring 1 2 s in
    if c1 + c2 == "{-"
    then skipBlockComment (depth + 1) (String.substring 2 (String.length s) s)
    else if c1 + c2 == "-}"
    then skipBlockComment (depth - 1) (String.substring 2 (String.length s) s)
    else skipBlockComment depth (stringTail s)

-- Skip all whitespace and comments
let rec skipWsAndComments s =
  let s2 = skipWhitespace s in
  if String.length s2 >= 2 then
    let prefix = String.substring 0 2 s2 in
    if prefix == "--"
    then skipWsAndComments (skipLineComment (String.substring 2 (String.length s2) s2))
    else if prefix == "{-"
    then skipWsAndComments (skipBlockComment 1 (String.substring 2 (String.length s2) s2))
    else s2
  else s2

-- =============================================================================
-- IDENTIFIER AND KEYWORD LEXING
-- =============================================================================

-- Collect identifier characters
let rec collectIdent acc s =
  match stringHead s
    when Just c if isIdentChar c -> collectIdent (acc + Char.toString c) (stringTail s)
    when _ -> (acc, s)
  end

-- Check if identifier is a keyword and return appropriate token
let identToToken ident =
  if ident == "let" then TkLet
  else if ident == "rec" then TkRec
  else if ident == "in" then TkIn
  else if ident == "if" then TkIf
  else if ident == "then" then TkThen
  else if ident == "else" then TkElse
  else if ident == "match" then TkMatch
  else if ident == "when" then TkWhen
  else if ident == "end" then TkEnd
  else if ident == "type" then TkType
  else if ident == "as" then TkAs
  else if ident == "and" then TkAnd
  else if ident == "true" then TkTrue
  else if ident == "false" then TkFalse
  else if ident == "module" then TkModule
  else if ident == "use" then TkUse
  else if ident == "foreign" then TkForeign
  else TkLower ident

-- =============================================================================
-- NUMBER LEXING
-- =============================================================================

-- Collect digits
let rec collectDigits acc s =
  match stringHead s
    when Just c if Char.isDigit c -> collectDigits (acc + Char.toString c) (stringTail s)
    when _ -> (acc, s)
  end

-- Check if string starts with a dot followed by a digit
let startsWithDotDigit s =
  if String.length s < 2 then false
  else
    match String.charAt 0 s
      when Just c1 ->
        match String.charAt 1 s
          when Just c2 -> c1 == '.' && Char.isDigit c2
          when Nothing -> false
        end
      when Nothing -> false
    end

-- Lex a number (int or float)
let lexNumber s =
  match collectDigits "" s
    when (digits, rest) ->
      if startsWithDotDigit rest then
        -- It's a float
        let rest2 = stringTail rest in
        match collectDigits "" rest2
          when (frac, rest3) ->
            match Float.fromString (digits + "." + frac)
              when Just f -> LexResult (TkFloat f) rest3
              when Nothing -> LexResult (TkError "Invalid float") rest3
            end
        end
      else
        -- It's an integer
        match Int.fromString digits
          when Just n -> LexResult (TkInt n) rest
          when Nothing -> LexResult (TkError "Invalid int") rest
        end
  end

-- =============================================================================
-- STRING LEXING
-- =============================================================================

-- Lex escape sequence, returns the escaped character
let lexEscape s =
  match stringHead s
    when Nothing -> (Nothing, s)
    when Just c ->
      if c == 'n' then (Just '\n', stringTail s)
      else if c == 't' then (Just '\t', stringTail s)
      else if c == 'r' then (Just '\r', stringTail s)
      else if c == '\\' then (Just '\\', stringTail s)
      else if c == '"' then (Just '"', stringTail s)
      else if c == '\'' then (Just '\'', stringTail s)
      else (Nothing, s)
  end

-- Collect string contents until closing quote
let rec collectString acc s =
  match stringHead s
    when Nothing -> (Nothing, s)  -- Unterminated string
    when Just '"' -> (Just acc, stringTail s)
    when Just '\\' ->
      match lexEscape (stringTail s)
        when (Just escaped, rest) -> collectString (acc + Char.toString escaped) rest
        when (Nothing, rest) -> (Nothing, rest)  -- Invalid escape
      end
    when Just c -> collectString (acc + Char.toString c) (stringTail s)
  end

-- =============================================================================
-- CHAR LEXING
-- =============================================================================

-- Check if next char is closing single quote
let nextIsClosingQuote s =
  match stringHead s
    when Just c -> c == '\''
    when Nothing -> false
  end

let lexChar s =
  match stringHead s
    when Nothing -> LexResult (TkError "Unterminated char") s
    when Just '\\' ->
      match lexEscape (stringTail s)
        when (Just escaped, rest) if nextIsClosingQuote rest ->
          LexResult (TkChar escaped) (stringTail rest)
        when (Just _, rest) -> LexResult (TkError "Unterminated char") rest
        when (Nothing, rest) -> LexResult (TkError "Invalid escape in char") rest
      end
    when Just c ->
      let rest = stringTail s in
      if nextIsClosingQuote rest
      then LexResult (TkChar c) (stringTail rest)
      else LexResult (TkError "Unterminated char") rest
  end

-- =============================================================================
-- MAIN LEXER
-- =============================================================================

-- Helper to lex two-char operator or fallback to single-char
let lexTwoCharOp secondChar twoCharTok oneCharTok rest =
  match stringHead rest
    when Just c ->
      if c == secondChar
      then LexResult twoCharTok (stringTail rest)
      else LexResult oneCharTok rest
    when Nothing -> LexResult oneCharTok rest
  end

let lexToken s =
  let s2 = skipWsAndComments s in
  match stringHead s2
    when Nothing -> LexResult TkEof s2
    when Just c ->
      -- Identifiers and keywords (lowercase or underscore)
      if Char.isLower c || c == '_' then
        match collectIdent "" s2
          when (ident, rest) -> LexResult (identToToken ident) rest
        end
      -- Uppercase identifiers (constructors)
      else if Char.isUpper c then
        match collectIdent "" s2
          when (ident, rest) -> LexResult (TkUpper ident) rest
        end
      -- Numbers
      else if Char.isDigit c then lexNumber s2
      -- Strings
      else if c == '"' then
        match collectString "" (stringTail s2)
          when (Just str, rest) -> LexResult (TkString str) rest
          when (Nothing, rest) -> LexResult (TkError "Unterminated string") rest
        end
      -- Characters
      else if c == '\'' then lexChar (stringTail s2)
      -- Two-character operators
      else if c == '-' then lexTwoCharOp '>' TkArrow TkMinus (stringTail s2)
      else if c == '=' then lexTwoCharOp '=' TkEqEq TkEq (stringTail s2)
      else if c == '!' then lexTwoCharOp '=' TkNe (TkError "Unexpected '!'") (stringTail s2)
      else if c == '<' then lexTwoCharOp '=' TkLe TkLt (stringTail s2)
      else if c == '>' then lexTwoCharOp '=' TkGe TkGt (stringTail s2)
      else if c == '|' then lexTwoCharOp '>' TkPipe TkBar (stringTail s2)
      else if c == ':' then lexTwoCharOp ':' TkColonColon TkColon (stringTail s2)
      -- Single-character tokens
      else if c == '+' then LexResult TkPlus (stringTail s2)
      else if c == '*' then LexResult TkStar (stringTail s2)
      else if c == '/' then LexResult TkSlash (stringTail s2)
      else if c == ',' then LexResult TkComma (stringTail s2)
      else if c == '.' then LexResult TkDot (stringTail s2)
      else if c == '(' then LexResult TkLParen (stringTail s2)
      else if c == ')' then LexResult TkRParen (stringTail s2)
      else if c == '{' then LexResult TkLBrace (stringTail s2)
      else if c == '}' then LexResult TkRBrace (stringTail s2)
      else if c == '[' then LexResult TkLBracket (stringTail s2)
      else if c == ']' then LexResult TkRBracket (stringTail s2)
      -- Unknown character
      else LexResult (TkError ("Unexpected character: " + Char.toString c)) (stringTail s2)
  end

-- =============================================================================
-- TOKENIZE ENTIRE STRING
-- =============================================================================

let rec tokenize s =
  match lexToken s
    when LexResult TkEof _ -> [TkEof]
    when LexResult tok rest -> Cons tok (tokenize rest)
  end

-- =============================================================================
-- TOKEN DISPLAY
-- =============================================================================

let tokenToString tok = match tok
  when TkInt n -> "Int(" + Int.toString n + ")"
  when TkFloat f -> "Float(" + Float.toString f + ")"
  when TkString s -> "String(\"" + s + "\")"
  when TkChar c -> "Char('" + Char.toString c + "')"
  when TkLower s -> "Lower(" + s + ")"
  when TkUpper s -> "Upper(" + s + ")"
  when TkLet -> "let"
  when TkRec -> "rec"
  when TkIn -> "in"
  when TkIf -> "if"
  when TkThen -> "then"
  when TkElse -> "else"
  when TkMatch -> "match"
  when TkWhen -> "when"
  when TkEnd -> "end"
  when TkType -> "type"
  when TkAs -> "as"
  when TkAnd -> "and"
  when TkTrue -> "true"
  when TkFalse -> "false"
  when TkModule -> "module"
  when TkUse -> "use"
  when TkForeign -> "foreign"
  when TkPlus -> "+"
  when TkMinus -> "-"
  when TkStar -> "*"
  when TkSlash -> "/"
  when TkLt -> "<"
  when TkLe -> "<="
  when TkGt -> ">"
  when TkGe -> ">="
  when TkEqEq -> "=="
  when TkNe -> "!="
  when TkPipe -> "|>"
  when TkArrow -> "->"
  when TkEq -> "="
  when TkBar -> "|"
  when TkComma -> ","
  when TkDot -> "."
  when TkUnderscore -> "_"
  when TkColon -> ":"
  when TkColonColon -> "::"
  when TkLParen -> "("
  when TkRParen -> ")"
  when TkLBrace -> "{"
  when TkRBrace -> "}"
  when TkLBracket -> "["
  when TkRBracket -> "]"
  when TkEof -> "EOF"
  when TkError msg -> "Error(" + msg + ")"
end

-- =============================================================================
-- TESTING
-- =============================================================================

-- Test source code (a small Algow program)
let testSource = "
let rec factorial n =
  if n <= 1
  then 1
  else n * factorial (n - 1)

-- Test the factorial
let result = factorial 5
in result
"

-- Tokenize a simple expression
let simpleExpr = "let x = 42 in x + 1"

-- Tokenize with various literals
let literalTest = "123 3.14 \"hello\" 'a' True False"

-- Tokenize with operators
let opTest = "a + b - c * d / e == f != g < h <= i > j >= k"

-- Tokenize with all delimiters
let delimTest = "( ) { } [ ] , . : :: -> |> |"

in {
  -- Show tokenized simple expression
  simpleTokens = List.map tokenToString (tokenize simpleExpr),

  -- Show tokenized literals
  literalTokens = List.map tokenToString (tokenize literalTest),

  -- Show tokenized operators
  opTokens = List.map tokenToString (tokenize opTest),

  -- Show tokenized delimiters
  delimTokens = List.map tokenToString (tokenize delimTest),

  -- Count tokens in factorial program
  factorialTokenCount = List.length (tokenize testSource),

  -- First few tokens of factorial
  factorialPreview = List.take 15 (List.map tokenToString (tokenize testSource))
}
