-- =============================================================================
-- Lexer Module
-- Tokenization using maximal munch algorithm
-- Reference: SPEC.md Section 3
-- =============================================================================

module Lexer
  use Types (..)

  -- ===========================================================================
  -- Whitespace and Comment Handling
  -- ===========================================================================

  -- Skip whitespace characters, tracking if newline encountered
  -- Returns (remaining string, sawNewline)
  let rec skipWhitespaceWithNewline sawNewline s =
    match String.head s
      when Just '\n' -> skipWhitespaceWithNewline true (String.tail s)
      when Just c if Char.isSpace c -> skipWhitespaceWithNewline sawNewline (String.tail s)
      when _ -> (s, sawNewline)
    end

  -- Skip whitespace characters (backwards compatible)
  let rec skipWhitespace s =
    match String.head s
      when Just c if Char.isSpace c -> skipWhitespace (String.tail s)
      when _ -> s
    end

  -- Skip line comment (starts with --)
  let rec skipLineComment s =
    match String.head s
      when Just '\n' -> String.tail s
      when Just _ -> skipLineComment (String.tail s)
      when Nothing -> s
    end

  -- Skip block comment (nested {- -})
  let rec skipBlockComment depth s =
    if depth == 0 then s
    else if String.length s < 2 then s
    else
      let c1 = String.charAt 0 s in
      let c2 = String.charAt 1 s in
      match (c1, c2)
        when (Just '{', Just '-') ->
          skipBlockComment (depth + 1) (String.drop 2 s)
        when (Just '-', Just '}') ->
          skipBlockComment (depth - 1) (String.drop 2 s)
        when _ -> skipBlockComment depth (String.tail s)
      end

  -- Skip all whitespace and comments
  let rec skipTrivia s =
    let s2 = skipWhitespace s in
    if String.length s2 >= 2 then
      let c1 = String.charAt 0 s2 in
      let c2 = String.charAt 1 s2 in
      match (c1, c2)
        when (Just '-', Just '-') ->
          skipTrivia (skipLineComment (String.drop 2 s2))
        when (Just '{', Just '-') ->
          skipTrivia (skipBlockComment 1 (String.drop 2 s2))
        when _ -> s2
      end
    else s2

  -- Skip all whitespace and comments, tracking newlines
  -- Returns (remaining string, sawNewline)
  let rec skipTriviaWithNewline sawNewline s =
    let (s2, newNewline) = skipWhitespaceWithNewline sawNewline s in
    if String.length s2 >= 2 then
      let c1 = String.charAt 0 s2 in
      let c2 = String.charAt 1 s2 in
      match (c1, c2)
        when (Just '-', Just '-') ->
          -- Line comment implies newline after
          skipTriviaWithNewline true (skipLineComment (String.drop 2 s2))
        when (Just '{', Just '-') ->
          skipTriviaWithNewline newNewline (skipBlockComment 1 (String.drop 2 s2))
        when _ -> (s2, newNewline)
      end
    else (s2, newNewline)

  -- ===========================================================================
  -- Character Classification
  -- ===========================================================================

  let isIdentStart c = Char.isLower c || c == '_'
  let isIdentChar c = Char.isAlphaNum c || c == '_' || c == '\''

  -- ===========================================================================
  -- Identifier and Keyword Lexing
  -- ===========================================================================

  -- Collect identifier characters
  let rec collectIdent acc s =
    match String.head s
      when Just c if isIdentChar c ->
        collectIdent (acc + Char.toString c) (String.tail s)
      when _ -> (acc, s)
    end

  -- Keyword lookup table
  let keywords = Map.fromList [
    ("let", TokLet),
    ("rec", TokRec),
    ("and", TokAnd),
    ("in", TokIn),
    ("if", TokIf),
    ("then", TokThen),
    ("else", TokElse),
    ("match", TokMatch),
    ("when", TokWhen),
    ("end", TokEnd),
    ("type", TokType),
    ("module", TokModule),
    ("use", TokUse),
    ("foreign", TokForeign),
    ("as", TokAs),
    ("do", TokDo),
    ("true", TokTrue),
    ("false", TokFalse)
  ]

  -- Check if identifier is a keyword or underscore
  let identToToken ident =
    if ident == "_" then TokUnderscore
    else match Map.lookup ident keywords
      when Just tok -> tok
      when Nothing -> TokLower ident
    end

  -- ===========================================================================
  -- Number Lexing
  -- ===========================================================================

  -- Collect digit characters
  let rec collectDigits acc s =
    match String.head s
      when Just c if Char.isDigit c ->
        collectDigits (acc + Char.toString c) (String.tail s)
      when _ -> (acc, s)
    end

  -- Check if string starts with a dot followed by a digit (for floats)
  let startsWithDotDigit s =
    if String.length s < 2 then false
    else
      match (String.charAt 0 s, String.charAt 1 s)
        when (Just '.', Just c) -> Char.isDigit c
        when _ -> false
      end

  -- Lex a number (int or float)
  let lexNumber s pos =
    match collectDigits "" s
      when (digits, rest) ->
        if startsWithDotDigit rest then
          -- It's a float: collect fractional part
          let rest2 = String.tail rest in
          match collectDigits "" rest2
            when (frac, rest3) ->
              let numStr = digits + "." + frac in
              let newPos = pos + String.length numStr in
              match Float.fromString numStr
                when Just f -> LexResult (TokFloat f) rest3 newPos
                when Nothing -> LexResult (TokError "Invalid float") rest3 newPos
              end
          end
        else
          -- It's an integer
          let newPos = pos + String.length digits in
          match Int.fromString digits
            when Just n -> LexResult (TokInt n) rest newPos
            when Nothing -> LexResult (TokError "Invalid int") rest newPos
          end
    end

  -- ===========================================================================
  -- String Lexing
  -- ===========================================================================

  -- Lex escape sequence
  let lexEscape s =
    match String.head s
      when Nothing -> (Nothing, s)
      when Just 'n' -> (Just '\n', String.tail s)
      when Just 't' -> (Just '\t', String.tail s)
      when Just 'r' -> (Just '\r', String.tail s)
      when Just '\\' -> (Just '\\', String.tail s)
      when Just '"' -> (Just '"', String.tail s)
      when Just '\'' -> (Just '\'', String.tail s)
      when Just '0' -> (Char.fromInt 0, String.tail s)
      when _ -> (Nothing, s)
    end

  -- Collect string contents until closing quote
  let rec collectString acc s =
    match String.head s
      when Nothing -> (Nothing, s)
      when Just '"' -> (Just acc, String.tail s)
      when Just '\\' ->
        match lexEscape (String.tail s)
          when (Just escaped, rest) ->
            collectString (acc + Char.toString escaped) rest
          when (Nothing, rest) -> (Nothing, rest)
        end
      when Just c -> collectString (acc + Char.toString c) (String.tail s)
    end

  -- ===========================================================================
  -- Character Lexing
  -- ===========================================================================

  let lexChar s pos =
    match String.head s
      when Nothing -> LexResult (TokError "Unterminated char") s pos
      when Just '\\' ->
        match lexEscape (String.tail s)
          when (Just escaped, rest) ->
            match String.head rest
              when Just '\'' -> LexResult (TokChar escaped) (String.tail rest) (pos + 4)
              when _ -> LexResult (TokError "Unterminated char") rest pos
            end
          when (Nothing, rest) -> LexResult (TokError "Invalid escape") rest pos
        end
      when Just c ->
        let rest = String.tail s in
        match String.head rest
          when Just '\'' -> LexResult (TokChar c) (String.tail rest) (pos + 3)
          when _ -> LexResult (TokError "Unterminated char") rest pos
        end
    end

  -- ===========================================================================
  -- Operator Lexing
  -- ===========================================================================

  -- Helper: try to lex two-char operator, fallback to single-char
  let lexTwoChar secondChar twoTok oneTok rest pos =
    match String.head rest
      when Just c ->
        if c == secondChar
        then LexResult twoTok (String.tail rest) (pos + 2)
        else LexResult oneTok rest (pos + 1)
      when Nothing -> LexResult oneTok rest (pos + 1)
    end

  -- ===========================================================================
  -- Main Lexer
  -- ===========================================================================

  let rec lexToken s pos =
    let s2 = skipTrivia s in
    let skipped = String.length s - String.length s2 in
    let pos2 = pos + skipped in
    match String.head s2
      when Nothing -> LexResult TokEof s2 pos2

      when Just c ->
        let rest = String.tail s2 in

        -- Lowercase identifier or keyword
        if isIdentStart c then
          match collectIdent "" s2
            when (ident, rest2) ->
              LexResult (identToToken ident) rest2 (pos2 + String.length ident)
          end

        -- Uppercase identifier (constructor/type)
        else if Char.isUpper c then
          match collectIdent "" s2
            when (ident, rest2) ->
              LexResult (TokUpper ident) rest2 (pos2 + String.length ident)
          end

        -- Number
        else if Char.isDigit c then
          lexNumber s2 pos2

        -- String
        else if c == '"' then
          match collectString "" rest
            when (Just str, rest2) ->
              LexResult (TokString str) rest2 (pos2 + String.length str + 2)
            when (Nothing, rest2) ->
              LexResult (TokError "Unterminated string") rest2 pos2
          end

        -- Character
        else if c == '\'' then
          lexChar rest (pos2 + 1)

        -- Two-character operators
        else if c == '-' then
          match String.head rest
            when Just '>' -> LexResult TokArrow (String.tail rest) (pos2 + 2)
            when _ -> LexResult TokMinus rest (pos2 + 1)
          end

        else if c == '<' then
          match String.head rest
            when Just '-' -> LexResult TokLeftArrow (String.tail rest) (pos2 + 2)
            when Just '=' -> LexResult TokLe (String.tail rest) (pos2 + 2)
            when _ -> LexResult TokLt rest (pos2 + 1)
          end

        else if c == '>' then
          lexTwoChar '=' TokGe TokGt rest pos2

        else if c == '=' then
          lexTwoChar '=' TokEqEq TokEq rest pos2

        else if c == '!' then
          match String.head rest
            when Just '=' -> LexResult TokNe (String.tail rest) (pos2 + 2)
            when _ -> LexResult (TokError "Unexpected '!'") rest (pos2 + 1)
          end

        else if c == '|' then
          match String.head rest
            when Just '>' -> LexResult TokPipe (String.tail rest) (pos2 + 2)
            when Just '|' -> LexResult TokOr2 (String.tail rest) (pos2 + 2)
            when _ -> LexResult TokBar rest (pos2 + 1)
          end

        else if c == '&' then
          match String.head rest
            when Just '&' -> LexResult TokAnd2 (String.tail rest) (pos2 + 2)
            when _ -> LexResult (TokError "Unexpected '&'") rest (pos2 + 1)
          end

        else if c == ':' then
          lexTwoChar ':' TokColon2 TokColon rest pos2

        -- Single-character tokens
        else if c == '+' then LexResult TokPlus rest (pos2 + 1)
        else if c == '*' then LexResult TokStar rest (pos2 + 1)
        else if c == '/' then LexResult TokSlash rest (pos2 + 1)
        else if c == ',' then LexResult TokComma rest (pos2 + 1)
        else if c == '.' then LexResult TokDot rest (pos2 + 1)
        else if c == '(' then LexResult TokLParen rest (pos2 + 1)
        else if c == ')' then LexResult TokRParen rest (pos2 + 1)
        else if c == '[' then LexResult TokLBracket rest (pos2 + 1)
        else if c == ']' then LexResult TokRBracket rest (pos2 + 1)
        else if c == '{' then LexResult TokLBrace rest (pos2 + 1)
        else if c == '}' then LexResult TokRBrace rest (pos2 + 1)

        else LexResult (TokError ("Unexpected: " + Char.toString c)) rest (pos2 + 1)
    end

  -- ===========================================================================
  -- Tokenize Entire String
  -- ===========================================================================

  -- Tail-recursive tokenization using accumulator
  -- This avoids stack overflow on large files (6000+ tokens)
  let rec tokenizeAcc acc s pos =
    match lexToken s pos
      when LexResult TokEof _ _ -> List.reverse (TokEof :: acc)
      when LexResult tok rest newPos -> tokenizeAcc (tok :: acc) rest newPos
    end

  let tokenize s = tokenizeAcc Nil s 0

  -- ===========================================================================
  -- Tokenization with Line Start Tracking
  -- Returns List (Token, bool) where bool indicates if token is at line start
  -- ===========================================================================

  -- Lex a single token, tracking if it's at line start
  -- Returns (token, remaining source, new position, atLineStart)
  let rec lexTokenWithLineInfo atLineStart s pos =
    let (s2, sawNewline) = skipTriviaWithNewline false s in
    let isAtLineStart = atLineStart || sawNewline in
    let skipped = String.length s - String.length s2 in
    let pos2 = pos + skipped in
    match String.head s2
      when Nothing -> (TokEof, s2, pos2, isAtLineStart)
      when Just c ->
        let rest = String.tail s2 in

        -- Lowercase identifier or keyword
        if isIdentStart c then
          match collectIdent "" s2
            when (ident, rest2) ->
              (identToToken ident, rest2, pos2 + String.length ident, isAtLineStart)
          end

        -- Uppercase identifier (constructor/type)
        else if Char.isUpper c then
          match collectIdent "" s2
            when (ident, rest2) ->
              (TokUpper ident, rest2, pos2 + String.length ident, isAtLineStart)
          end

        -- Number
        else if Char.isDigit c then
          match lexNumber s2 pos2
            when LexResult tok rest2 newPos -> (tok, rest2, newPos, isAtLineStart)
          end

        -- String
        else if c == '"' then
          match collectString "" rest
            when (Just str, rest2) ->
              (TokString str, rest2, pos2 + String.length str + 2, isAtLineStart)
            when (Nothing, rest2) ->
              (TokError "Unterminated string", rest2, pos2, isAtLineStart)
          end

        -- Character
        else if c == '\'' then
          match lexChar rest (pos2 + 1)
            when LexResult tok rest2 newPos -> (tok, rest2, newPos, isAtLineStart)
          end

        -- Two-character operators
        else if c == '-' then
          match String.head rest
            when Just '>' -> (TokArrow, String.tail rest, pos2 + 2, isAtLineStart)
            when _ -> (TokMinus, rest, pos2 + 1, isAtLineStart)
          end

        else if c == '<' then
          match String.head rest
            when Just '-' -> (TokLeftArrow, String.tail rest, pos2 + 2, isAtLineStart)
            when Just '=' -> (TokLe, String.tail rest, pos2 + 2, isAtLineStart)
            when _ -> (TokLt, rest, pos2 + 1, isAtLineStart)
          end

        else if c == '>' then
          match String.head rest
            when Just '=' -> (TokGe, String.tail rest, pos2 + 2, isAtLineStart)
            when _ -> (TokGt, rest, pos2 + 1, isAtLineStart)
          end

        else if c == '=' then
          match String.head rest
            when Just '=' -> (TokEqEq, String.tail rest, pos2 + 2, isAtLineStart)
            when _ -> (TokEq, rest, pos2 + 1, isAtLineStart)
          end

        else if c == '!' then
          match String.head rest
            when Just '=' -> (TokNe, String.tail rest, pos2 + 2, isAtLineStart)
            when _ -> (TokError "Unexpected '!'", rest, pos2 + 1, isAtLineStart)
          end

        else if c == '|' then
          match String.head rest
            when Just '>' -> (TokPipe, String.tail rest, pos2 + 2, isAtLineStart)
            when Just '|' -> (TokOr2, String.tail rest, pos2 + 2, isAtLineStart)
            when _ -> (TokBar, rest, pos2 + 1, isAtLineStart)
          end

        else if c == '&' then
          match String.head rest
            when Just '&' -> (TokAnd2, String.tail rest, pos2 + 2, isAtLineStart)
            when _ -> (TokError "Unexpected '&'", rest, pos2 + 1, isAtLineStart)
          end

        else if c == ':' then
          match String.head rest
            when Just ':' -> (TokColon2, String.tail rest, pos2 + 2, isAtLineStart)
            when _ -> (TokColon, rest, pos2 + 1, isAtLineStart)
          end

        -- Single-character tokens
        else if c == '+' then (TokPlus, rest, pos2 + 1, isAtLineStart)
        else if c == '*' then (TokStar, rest, pos2 + 1, isAtLineStart)
        else if c == '/' then (TokSlash, rest, pos2 + 1, isAtLineStart)
        else if c == ',' then (TokComma, rest, pos2 + 1, isAtLineStart)
        else if c == '.' then (TokDot, rest, pos2 + 1, isAtLineStart)
        else if c == '(' then (TokLParen, rest, pos2 + 1, isAtLineStart)
        else if c == ')' then (TokRParen, rest, pos2 + 1, isAtLineStart)
        else if c == '[' then (TokLBracket, rest, pos2 + 1, isAtLineStart)
        else if c == ']' then (TokRBracket, rest, pos2 + 1, isAtLineStart)
        else if c == '{' then (TokLBrace, rest, pos2 + 1, isAtLineStart)
        else if c == '}' then (TokRBrace, rest, pos2 + 1, isAtLineStart)

        else (TokError ("Unexpected: " + Char.toString c), rest, pos2 + 1, isAtLineStart)
    end

  -- Tokenize with line start info
  let rec tokenizeWithLineInfoAcc acc s pos =
    match lexTokenWithLineInfo true s pos
      when (TokEof, _, _, atLine) ->
        List.reverse ((TokEof, atLine) :: acc)
      when (tok, rest, newPos, atLine) ->
        tokenizeWithLineInfoAcc ((tok, atLine) :: acc) rest newPos
    end

  let tokenizeWithLineInfo s = tokenizeWithLineInfoAcc Nil s 0

end
