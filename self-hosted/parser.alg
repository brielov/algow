-- =============================================================================
-- Parser Module
-- Pratt parsing (Top-Down Operator Precedence)
-- Reference: SPEC.md Section 4, Pratt (1973)
-- =============================================================================

module Parser
  use Types (..)

  -- ===========================================================================
  -- Helper: append single element to list
  -- ===========================================================================

  let snoc xs x = List.append xs [x]

  -- ===========================================================================
  -- Parser State
  -- ===========================================================================

  -- Parser state: tokens and current position
  type PState = PState (List Token) int

  -- Parser result
  type PResult a
    = POk a PState
    | PErr string int

  -- ===========================================================================
  -- Basic Combinators
  -- ===========================================================================

  let peek state =
    match state
      when PState tokens _ ->
        match tokens
          when Nil -> TokEof
          when Cons t _ -> t
        end
    end

  let advance state =
    match state
      when PState tokens pos ->
        match tokens
          when Nil -> PState Nil pos
          when Cons _ rest -> PState rest (pos + 1)
        end
    end

  let getPos state =
    match state when PState _ pos -> pos end

  let expect tok state =
    if peek state == tok
    then POk tok (advance state)
    else PErr ("Expected " + tokenName tok) (getPos state)

  let tokenName tok = match tok
    when TokLet -> "'let'"
    when TokRec -> "'rec'"
    when TokAnd -> "'and'"
    when TokIn -> "'in'"
    when TokIf -> "'if'"
    when TokThen -> "'then'"
    when TokElse -> "'else'"
    when TokMatch -> "'match'"
    when TokWhen -> "'when'"
    when TokEnd -> "'end'"
    when TokType -> "'type'"
    when TokModule -> "'module'"
    when TokUse -> "'use'"
    when TokForeign -> "'foreign'"
    when TokAs -> "'as'"
    when TokDo -> "'do'"
    when TokTrue -> "'true'"
    when TokFalse -> "'false'"
    when TokArrow -> "'->'"
    when TokLeftArrow -> "'<-'"
    when TokEq -> "'='"
    when TokBar -> "'|'"
    when TokColon -> "':'"
    when TokComma -> "','"
    when TokDot -> "'.'"
    when TokLParen -> "'('"
    when TokRParen -> "')'"
    when TokLBracket -> "'['"
    when TokRBracket -> "']'"
    when TokLBrace -> "'{'"
    when TokRBrace -> "'}'"
    when TokEof -> "end of input"
    when _ -> "token"
  end

  -- ===========================================================================
  -- Operator Precedence (for Pratt parsing)
  -- ===========================================================================

  -- Precedence levels (higher = binds tighter)
  let precOr = 1        -- ||
  let precAnd = 2       -- &&
  let precCompare = 3   -- ==, !=, <, <=, >, >=
  let precCons = 4      -- ::
  let precAdd = 5       -- +, -
  let precMul = 6       -- *, /, %
  let precPipe = 0      -- |> (lowest, left-to-right)

  let infixPrec tok = match tok
    when TokOr2 -> Just precOr
    when TokAnd2 -> Just precAnd
    when TokEqEq -> Just precCompare
    when TokNe -> Just precCompare
    when TokLt -> Just precCompare
    when TokLe -> Just precCompare
    when TokGt -> Just precCompare
    when TokGe -> Just precCompare
    when TokColon2 -> Just precCons
    when TokPlus -> Just precAdd
    when TokMinus -> Just precAdd
    when TokStar -> Just precMul
    when TokSlash -> Just precMul
    when TokPercent -> Just precMul
    when TokPipe -> Just precPipe
    when _ -> Nothing
  end

  let tokToOp tok = match tok
    when TokOr2 -> "||"
    when TokAnd2 -> "&&"
    when TokEqEq -> "=="
    when TokNe -> "!="
    when TokLt -> "<"
    when TokLe -> "<="
    when TokGt -> ">"
    when TokGe -> ">="
    when TokPlus -> "+"
    when TokMinus -> "-"
    when TokStar -> "*"
    when TokSlash -> "/"
    when TokPercent -> "%"
    when _ -> "?"
  end

  -- Right-associative operators
  let isRightAssoc tok = match tok
    when TokColon2 -> true
    when _ -> false
  end

  -- ===========================================================================
  -- Expression Parsing (Pratt)
  -- ===========================================================================

  -- Check if token can start an atom
  -- Tokens that can be function arguments (without parentheses)
  -- Note: Let, If, Match, Do require parentheses to be used as arguments
  let canBeArgument tok = match tok
    when TokInt _ -> true
    when TokFloat _ -> true
    when TokString _ -> true
    when TokChar _ -> true
    when TokLower _ -> true
    when TokUpper _ -> true
    when TokTrue -> true
    when TokFalse -> true
    when TokLParen -> true
    when TokLBracket -> true
    when TokLBrace -> true
    when _ -> false
  end

  -- Tokens that can start an expression (including keywords)
  let canStartAtom tok = match tok
    when TokMinus -> true
    when TokIf -> true
    when TokMatch -> true
    when TokLet -> true
    when TokDo -> true
    when _ -> canBeArgument tok
  end

  -- Parse atom (highest precedence)
  let rec parseAtom state = match peek state
    -- Literals
    when TokInt n -> POk (SLit (LInt n)) (advance state)
    when TokFloat f -> POk (SLit (LFloat f)) (advance state)
    when TokString s -> POk (SLit (LString s)) (advance state)
    when TokChar c -> POk (SLit (LChar c)) (advance state)
    when TokTrue -> POk (SLit (LBool true)) (advance state)
    when TokFalse -> POk (SLit (LBool false)) (advance state)

    -- Variables
    when TokLower x -> POk (SVar x) (advance state)

    -- Constructors
    when TokUpper c -> POk (SCon c) (advance state)

    -- Parenthesized expression, tuple, or unit
    when TokLParen -> parseParenExpr (advance state)

    -- List literal
    when TokLBracket -> parseListExpr (advance state)

    -- Record literal
    when TokLBrace -> parseRecordExpr (advance state)

    -- If expression
    when TokIf -> parseIfExpr (advance state)

    -- Match expression
    when TokMatch -> parseMatchExpr (advance state)

    -- Let expression
    when TokLet -> parseLetExpr (advance state)

    -- Do notation
    when TokDo -> parseDoExpr (advance state)

    -- Unary minus
    when TokMinus ->
      match parseAtom (advance state)
        when PErr e p -> PErr e p
        when POk expr s2 -> POk (SUnaryMinus expr) s2
      end

    when _ -> PErr "Expected expression" (getPos state)
  end

  -- Parse parenthesized expression, tuple, or lambda
  and parseParenExpr state =
    -- Check for unit ()
    if peek state == TokRParen then
      POk (STuple Nil) (advance state)
    -- Check for lambda: (x y -> e) or (x -> e)
    else if isLambdaStart state then
      parseLambda state
    else
      -- Parse first expression
      match parseExpr 0 state
        when PErr e p -> PErr e p
        when POk expr s2 ->
          match peek s2
            -- Just parenthesized expression
            when TokRParen -> POk expr (advance s2)
            -- Tuple
            when TokComma -> parseTupleRest [expr] (advance s2)
            -- Type annotation
            when TokColon ->
              match parseType (advance s2)
                when PErr e p -> PErr e p
                when POk ty s3 ->
                  match expect TokRParen s3
                    when PErr e p -> PErr e p
                    when POk _ s4 -> POk (SAnnot expr ty) s4
                  end
              end
            when _ -> PErr "Expected ')', ',', or ':'" (getPos s2)
          end
      end

  -- Check if we're at the start of a lambda
  and isLambdaStart state =
    match peek state
      when TokLower _ -> hasArrowAfterParams state
      when TokUnderscore -> hasArrowAfterParams state
      when _ -> false
    end

  -- Look ahead to see if there's an arrow after parameters
  and hasArrowAfterParams state =
    let rec skip s =
      match peek s
        when TokLower _ -> skip (advance s)
        when TokUnderscore -> skip (advance s)
        when TokArrow -> true
        when _ -> false
      end
    in skip state

  -- Parse lambda in parens: (x y -> e)
  and parseLambda state =
    match parseParams Nil state
      when PErr e p -> PErr e p
      when POk params s2 ->
        match expect TokArrow s2
          when PErr e p -> PErr e p
          when POk _ s3 ->
            match parseExpr 0 s3
              when PErr e p -> PErr e p
              when POk body s4 ->
                match expect TokRParen s4
                  when PErr e p -> PErr e p
                  when POk _ s5 -> POk (SAbs params body) s5
                end
            end
        end
    end

  -- Parse bare lambda: x y -> e (without parens)
  and parseBareLambda state =
    match parseParams Nil state
      when PErr e p -> PErr e p
      when POk params s2 ->
        match expect TokArrow s2
          when PErr e p -> PErr e p
          when POk _ s3 ->
            match parseExpr 0 s3
              when PErr e p -> PErr e p
              when POk body s4 -> POk (SAbs params body) s4
            end
        end
    end

  -- Parse parameters (list of identifiers)
  and parseParams acc state =
    match peek state
      when TokLower x -> parseParams (snoc acc x) (advance state)
      when TokUnderscore -> parseParams (snoc acc "_") (advance state)
      when _ -> POk acc state
    end

  -- Parse tuple rest after first element
  and parseTupleRest acc state =
    match parseExpr 0 state
      when PErr e p -> PErr e p
      when POk expr s2 ->
        let newAcc = snoc acc expr in
        match peek s2
          when TokComma -> parseTupleRest newAcc (advance s2)
          when TokRParen -> POk (STuple newAcc) (advance s2)
          when _ -> PErr "Expected ',' or ')'" (getPos s2)
        end
    end

  -- Parse list literal
  and parseListExpr state =
    if peek state == TokRBracket then
      POk (SList Nil) (advance state)
    else
      parseListItems Nil state

  and parseListItems acc state =
    match parseExpr 0 state
      when PErr e p -> PErr e p
      when POk expr s2 ->
        let newAcc = snoc acc expr in
        match peek s2
          when TokComma -> parseListItems newAcc (advance s2)
          when TokRBracket -> POk (SList newAcc) (advance s2)
          when _ -> PErr "Expected ',' or ']'" (getPos s2)
        end
    end

  -- Parse record literal or record update
  and parseRecordExpr state =
    match peek state
      when TokRBrace -> POk (SRecord Nil) (advance state)
      when TokLower x ->
        let s2 = advance state in
        match peek s2
          -- Record update: { r | x = 1 }
          when TokBar -> parseRecordUpdate (SVar x) (advance s2)
          -- Regular record field
          when TokEq ->
            match parseExpr 0 (advance s2)
              when PErr e p -> PErr e p
              when POk v s3 -> parseRecordFieldsRest [SField x v] s3
            end
          -- Punned field: { x } means { x = x }
          when TokComma ->
            parseRecordFieldsRest [SField x (SVar x)] (advance s2)
          when TokRBrace ->
            POk (SRecord [SField x (SVar x)]) (advance s2)
          when _ -> PErr "Expected '=', '|', ',', or '}'" (getPos s2)
        end
      when _ -> PErr "Expected field name or '}'" (getPos state)
    end

  and parseRecordUpdate base state =
    match parseRecordUpdateFields Nil state
      when PErr e p -> PErr e p
      when POk fields s -> POk (SRecordUpdate base fields) s
    end

  and parseRecordUpdateFields acc state =
    match peek state
      when TokLower x ->
        let s2 = advance state in
        match peek s2
          when TokEq ->
            match parseExpr 0 (advance s2)
              when PErr e p -> PErr e p
              when POk v s3 ->
                let newAcc = snoc acc (SField x v) in
                match peek s3
                  when TokComma -> parseRecordUpdateFields newAcc (advance s3)
                  when TokRBrace -> POk newAcc (advance s3)
                  when _ -> PErr "Expected ',' or '}'" (getPos s3)
                end
            end
          when _ -> PErr "Expected '='" (getPos s2)
        end
      when TokRBrace -> POk acc (advance state)
      when _ -> PErr "Expected field name" (getPos state)
    end

  and parseRecordFieldsRest acc state =
    match peek state
      when TokComma ->
        match peek (advance state)
          when TokLower x ->
            let s2 = advance (advance state) in
            match peek s2
              when TokEq ->
                match parseExpr 0 (advance s2)
                  when PErr e p -> PErr e p
                  when POk v s3 -> parseRecordFieldsRest (snoc acc (SField x v)) s3
                end
              -- Punned field
              when TokComma ->
                parseRecordFieldsRest (snoc acc (SField x (SVar x))) (advance s2)
              when TokRBrace ->
                POk (SRecord (snoc acc (SField x (SVar x)))) (advance s2)
              when _ -> PErr "Expected '=' or ','" (getPos s2)
            end
          when _ -> PErr "Expected field name" (getPos (advance state))
        end
      when TokRBrace -> POk (SRecord acc) (advance state)
      when _ -> PErr "Expected ',' or '}'" (getPos state)
    end

  -- Parse if expression
  and parseIfExpr state =
    match parseExpr 0 state
      when PErr e p -> PErr e p
      when POk cond s2 ->
        match expect TokThen s2
          when PErr e p -> PErr e p
          when POk _ s3 ->
            match parseExpr 0 s3
              when PErr e p -> PErr e p
              when POk thenBr s4 ->
                match expect TokElse s4
                  when PErr e p -> PErr e p
                  when POk _ s5 ->
                    match parseExpr 0 s5
                      when PErr e p -> PErr e p
                      when POk elseBr s6 -> POk (SIf cond thenBr elseBr) s6
                    end
                end
            end
        end
    end

  -- Parse match expression
  and parseMatchExpr state =
    match parseExpr 0 state
      when PErr e p -> PErr e p
      when POk scrutinee s2 -> parseCases scrutinee Nil s2
    end

  and parseCases scrutinee acc state =
    match peek state
      when TokWhen ->
        match parseCase (advance state)
          when PErr e p -> PErr e p
          when POk c s2 -> parseCases scrutinee (snoc acc c) s2
        end
      when TokEnd -> POk (SMatch scrutinee acc) (advance state)
      when _ -> PErr "Expected 'when' or 'end'" (getPos state)
    end

  and parseCase state =
    match parsePattern state
      when PErr e p -> PErr e p
      when POk pat s2 ->
        -- Check for guard
        match peek s2
          when TokIf ->
            -- Use parseExprNoLambda for guard to avoid confusing 'guard ->' with bare lambda
            match parseExprNoLambda 0 (advance s2)
              when PErr e p -> PErr e p
              when POk guard s3 ->
                match expect TokArrow s3
                  when PErr e p -> PErr e p
                  when POk _ s4 ->
                    match parseExpr 0 s4
                      when PErr e p -> PErr e p
                      when POk body s5 -> POk (SCase pat (Just guard) body) s5
                    end
                end
            end
          when TokArrow ->
            match parseExpr 0 (advance s2)
              when PErr e p -> PErr e p
              when POk body s3 -> POk (SCase pat Nothing body) s3
            end
          when _ -> PErr "Expected 'if' or '->'" (getPos s2)
        end
    end

  -- Check if token starts a destructuring pattern (not a simple variable binding)
  and isPatternStart tok = match tok
    when TokLParen -> true
    when TokLBrace -> true
    when TokUnderscore -> true
    when TokUpper _ -> true
    when _ -> false
  end

  -- Parse let expression
  and parseLetExpr state =
    match peek state
      when TokRec -> parseLetRec (advance state)
      when TokLower x -> parseLetSimple x (advance state)
      when _ ->
        -- Check for destructuring pattern: let (a, b) = expr in body
        if isPatternStart (peek state) then
          parseLetDestructure state
        else
          PErr "Expected 'rec' or identifier" (getPos state)
    end

  -- Parse let destructuring: let pattern = expr in body
  -- Desugars to: match expr when pattern -> body end
  and parseLetDestructure state =
    match parsePattern state
      when PErr e p -> PErr e p
      when POk pat s2 ->
        match expect TokEq s2
          when PErr e p -> PErr e p
          when POk _ s3 ->
            match parseExpr 0 s3
              when PErr e p -> PErr e p
              when POk value s4 ->
                match expect TokIn s4
                  when PErr e p -> PErr e p
                  when POk _ s5 ->
                    match parseExpr 0 s5
                      when PErr e p -> PErr e p
                      when POk body s6 ->
                        -- Desugar to match
                        POk (SMatch value [SCase pat Nothing body]) s6
                    end
                end
            end
        end
    end

  and parseLetSimple name state =
    -- Check for parameters
    match parseParams Nil state
      when PErr e p -> PErr e p
      when POk params s2 ->
        match expect TokEq s2
          when PErr e p -> PErr e p
          when POk _ s3 ->
            match parseExpr 0 s3
              when PErr e p -> PErr e p
              when POk value s4 ->
                let boundExpr = if List.isEmpty params then value else SAbs params value in
                match expect TokIn s4
                  when PErr e p -> PErr e p
                  when POk _ s5 ->
                    match parseExpr 0 s5
                      when PErr e p -> PErr e p
                      when POk body s6 -> POk (SLet name boundExpr body) s6
                    end
                end
            end
        end
    end

  and parseLetRec state =
    match parseRecBinding state
      when PErr e p -> PErr e p
      when POk firstBind s2 -> parseLetRecRest [firstBind] s2
    end

  and parseLetRecRest acc state =
    match peek state
      when TokAnd ->
        match parseRecBinding (advance state)
          when PErr e p -> PErr e p
          when POk bind s2 -> parseLetRecRest (snoc acc bind) s2
        end
      when TokIn ->
        match parseExpr 0 (advance state)
          when PErr e p -> PErr e p
          when POk body s2 -> POk (SLetRec acc body) s2
        end
      when _ -> PErr "Expected 'and' or 'in'" (getPos state)
    end

  and parseRecBinding state =
    match peek state
      when TokLower name ->
        match parseParams Nil (advance state)
          when PErr e p -> PErr e p
          when POk params s2 ->
            match expect TokEq s2
              when PErr e p -> PErr e p
              when POk _ s3 ->
                match parseExpr 0 s3
                  when PErr e p -> PErr e p
                  when POk value s4 ->
                    let boundExpr = if List.isEmpty params then value else SAbs params value in
                    POk (SBinding name boundExpr) s4
                end
            end
        end
      when _ -> PErr "Expected identifier" (getPos state)
    end

  -- Parse do expression
  and parseDoExpr state =
    parseDoStmts Nil state

  and parseDoStmts acc state =
    match peek state
      when TokEnd -> POk (SDo acc) (advance state)
      when TokLet ->
        match parseDoLet (advance state)
          when PErr e p -> PErr e p
          when POk stmt s2 -> parseDoStmts (snoc acc stmt) s2
        end
      when _ ->
        match parseDoStmt state
          when PErr e p -> PErr e p
          when POk stmt s2 -> parseDoStmts (snoc acc stmt) s2
        end
    end

  and parseDoLet state =
    match parsePattern state
      when PErr e p -> PErr e p
      when POk pat s2 ->
        match expect TokEq s2
          when PErr e p -> PErr e p
          when POk _ s3 ->
            match parseExpr 0 s3
              when PErr e p -> PErr e p
              when POk expr s4 -> POk (DoLet pat expr) s4
            end
        end
    end

  and parseDoStmt state =
    -- Try to parse pattern <- expr first
    match parsePattern state
      when PErr _ _ ->
        -- Fall back to just expression
        match parseExpr 0 state
          when PErr e p -> PErr e p
          when POk expr s2 -> POk (DoExpr expr) s2
        end
      when POk pat s2 ->
        match peek s2
          when TokLeftArrow ->
            match parseExpr 0 (advance s2)
              when PErr e p -> PErr e p
              when POk expr s3 -> POk (DoBindPattern pat expr) s3
            end
          when _ ->
            -- Pattern was actually an expression
            match patternToExpr pat
              when Just expr -> POk (DoExpr expr) s2
              when Nothing -> PErr "Expected '<-' after pattern" (getPos s2)
            end
        end
    end

  -- Convert simple patterns back to expressions
  and patternToExpr pat = match pat
    when SPVar x -> Just (SVar x)
    when SPLit l -> Just (SLit l)
    when SPCon c args ->
      if List.isEmpty args then Just (SCon c)
      else Nothing
    when _ -> Nothing
  end

  -- ===========================================================================
  -- Pratt Expression Parser
  -- ===========================================================================

  and parseExpr minPrec state =
    -- At the start of an expression, check for bare lambda: x y -> e
    if minPrec == 0 && isLambdaStart state then
      parseBareLambda state
    else
      parseExprCore minPrec state

  -- Parse expression without bare lambda check (for guard expressions)
  and parseExprNoLambda minPrec state =
    parseExprCore minPrec state

  and parseExprCore minPrec state =
    match parseAtom state
      when PErr e p -> PErr e p
      when POk left s2 -> parseExprRest minPrec left s2
    end

  and parseExprRest minPrec left state =
    let tok = peek state in
    -- Check for field access
    if tok == TokDot then
      match peek (advance state)
        when TokLower field ->
          parseExprRest minPrec (SFieldAccess left field) (advance (advance state))
        when TokInt n ->
          -- Tuple index access: x.0, x.1, etc.
          parseExprRest minPrec (SFieldAccess left (Int.toString n)) (advance (advance state))
        when _ -> PErr "Expected field name" (getPos (advance state))
      end
    -- Check for infix operator
    else match infixPrec tok
      when Just prec ->
        if prec >= minPrec then
          let nextPrec = if isRightAssoc tok then prec else prec + 1 in
          let op = tokToOp tok in
          match parseExpr nextPrec (advance state)
            when PErr e p -> PErr e p
            when POk right s2 ->
              -- Handle special operators
              let expr = if tok == TokColon2 then SConsOp left right
                         else if tok == TokPipe then SPipeOp left right
                         else SBinOp op left right
              in
              parseExprRest minPrec expr s2
          end
        else POk left state
      when Nothing ->
        -- Check for function application (only simple tokens, not keywords)
        if canBeArgument tok then
          match parseAtom state
            when PErr e p -> PErr e p
            when POk arg s2 -> parseExprRest minPrec (SApp left arg) s2
          end
        else POk left state
    end

  -- ===========================================================================
  -- Pattern Parsing
  -- ===========================================================================

  and parsePattern state = parsePatternOr state

  and parsePatternOr state =
    match parsePatternAs state
      when PErr e p -> PErr e p
      when POk left s2 ->
        match peek s2
          when TokBar ->
            match parsePatternOr (advance s2)
              when PErr e p -> PErr e p
              when POk right s3 -> POk (SPOr left right) s3
            end
          when _ -> POk left s2
        end
    end

  and parsePatternAs state =
    match parsePatternCons state
      when PErr e p -> PErr e p
      when POk pat s2 ->
        match peek s2
          when TokAs ->
            match peek (advance s2)
              when TokLower name -> POk (SPAs name pat) (advance (advance s2))
              when _ -> PErr "Expected identifier after 'as'" (getPos (advance s2))
            end
          when _ -> POk pat s2
        end
    end

  and parsePatternCons state =
    match parsePatternAtom state
      when PErr e p -> PErr e p
      when POk left s2 ->
        match peek s2
          when TokColon2 ->
            match parsePatternCons (advance s2)
              when PErr e p -> PErr e p
              when POk right s3 -> POk (SPCons left right) s3
            end
          when _ -> POk left s2
        end
    end

  and parsePatternAtom state = match peek state
    -- Wildcard
    when TokUnderscore -> POk SPWild (advance state)

    -- Literals
    when TokInt n -> POk (SPLit (LInt n)) (advance state)
    when TokFloat f -> POk (SPLit (LFloat f)) (advance state)
    when TokString s -> POk (SPLit (LString s)) (advance state)
    when TokChar c -> POk (SPLit (LChar c)) (advance state)
    when TokTrue -> POk (SPLit (LBool true)) (advance state)
    when TokFalse -> POk (SPLit (LBool false)) (advance state)

    -- Variable
    when TokLower x -> POk (SPVar x) (advance state)

    -- Constructor (with optional arguments)
    when TokUpper c -> parseConPattern c (advance state)

    -- Parenthesized pattern or tuple
    when TokLParen -> parseParenPattern (advance state)

    -- List pattern
    when TokLBracket -> parseListPattern (advance state)

    -- Record pattern
    when TokLBrace -> parseRecordPattern (advance state)

    -- Negative number pattern
    when TokMinus ->
      match peek (advance state)
        when TokInt n -> POk (SPLit (LInt (0 - n))) (advance (advance state))
        when TokFloat f -> POk (SPLit (LFloat (0.0 - f))) (advance (advance state))
        when _ -> PErr "Expected number after '-'" (getPos (advance state))
      end

    when _ -> PErr "Expected pattern" (getPos state)
  end

  and parseConPattern name state =
    -- Parse constructor arguments (if any)
    match parseConArgs Nil state
      when PErr e p -> PErr e p
      when POk args s -> POk (SPCon name args) s
    end

  and parseConArgs acc state =
    -- Constructor arguments must be atomic patterns
    let tok = peek state in
    if canStartPatternAtom tok then
      match parsePatternAtom state
        when PErr e p -> PErr e p
        when POk pat s2 -> parseConArgs (snoc acc pat) s2
      end
    else POk acc state

  and canStartPatternAtom tok = match tok
    when TokUnderscore -> true
    when TokInt _ -> true
    when TokFloat _ -> true
    when TokString _ -> true
    when TokChar _ -> true
    when TokTrue -> true
    when TokFalse -> true
    when TokLower _ -> true
    when TokUpper _ -> true
    when TokLParen -> true
    when TokLBracket -> true
    when TokLBrace -> true
    when _ -> false
  end

  and parseParenPattern state =
    if peek state == TokRParen then
      POk (SPTuple Nil) (advance state)
    else
      match parsePattern state
        when PErr e p -> PErr e p
        when POk pat s2 ->
          match peek s2
            when TokRParen -> POk pat (advance s2)
            when TokComma -> parseTuplePatternRest [pat] (advance s2)
            when _ -> PErr "Expected ')' or ','" (getPos s2)
          end
      end

  and parseTuplePatternRest acc state =
    match parsePattern state
      when PErr e p -> PErr e p
      when POk pat s2 ->
        let newAcc = snoc acc pat in
        match peek s2
          when TokComma -> parseTuplePatternRest newAcc (advance s2)
          when TokRParen -> POk (SPTuple newAcc) (advance s2)
          when _ -> PErr "Expected ',' or ')'" (getPos s2)
        end
    end

  and parseListPattern state =
    if peek state == TokRBracket then
      POk (SPList Nil) (advance state)
    else
      parseListPatternItems Nil state

  and parseListPatternItems acc state =
    match parsePattern state
      when PErr e p -> PErr e p
      when POk pat s2 ->
        let newAcc = snoc acc pat in
        match peek s2
          when TokComma -> parseListPatternItems newAcc (advance s2)
          when TokRBracket -> POk (SPList newAcc) (advance s2)
          when _ -> PErr "Expected ',' or ']'" (getPos s2)
        end
    end

  and parseRecordPattern state =
    if peek state == TokRBrace then
      POk (SPRecord Nil) (advance state)
    else
      parseRecordPatternFields Nil state

  and parseRecordPatternFields acc state =
    match peek state
      when TokLower x ->
        let s2 = advance state in
        match peek s2
          when TokEq ->
            match parsePattern (advance s2)
              when PErr e p -> PErr e p
              when POk pat s3 ->
                let newAcc = snoc acc (SPatternField x pat) in
                parseRecordPatternFieldsRest newAcc s3
            end
          -- Punned field: { x } means { x = x }
          when TokComma ->
            let newAcc = snoc acc (SPatternField x (SPVar x)) in
            parseRecordPatternFields newAcc (advance s2)
          when TokRBrace ->
            POk (SPRecord (snoc acc (SPatternField x (SPVar x)))) (advance s2)
          when _ -> PErr "Expected '=', ',', or '}'" (getPos s2)
        end
      when TokRBrace -> POk (SPRecord acc) (advance state)
      when _ -> PErr "Expected field name or '}'" (getPos state)
    end

  and parseRecordPatternFieldsRest acc state =
    match peek state
      when TokComma -> parseRecordPatternFields acc (advance state)
      when TokRBrace -> POk (SPRecord acc) (advance state)
      when _ -> PErr "Expected ',' or '}'" (getPos state)
    end

  -- ===========================================================================
  -- Type Parsing
  -- ===========================================================================

  and parseType state = parseTypeFun state

  and parseTypeFun state =
    match parseTypeApp state
      when PErr e p -> PErr e p
      when POk left s2 ->
        match peek s2
          when TokArrow ->
            match parseTypeFun (advance s2)
              when PErr e p -> PErr e p
              when POk right s3 -> POk (STFun left right) s3
            end
          when _ -> POk left s2
        end
    end

  and parseTypeApp state =
    match parseTypeAtom state
      when PErr e p -> PErr e p
      when POk left s2 -> parseTypeAppRest left s2
    end

  and parseTypeAppRest left state =
    let tok = peek state in
    if canStartTypeAtom tok then
      match parseTypeAtom state
        when PErr e p -> PErr e p
        when POk arg s2 -> parseTypeAppRest (STApp left arg) s2
      end
    else POk left state

  and canStartTypeAtom tok = match tok
    when TokLower _ -> true
    when TokUpper _ -> true
    when TokLParen -> true
    when TokLBrace -> true
    when _ -> false
  end

  and parseTypeAtom state = match peek state
    -- Type variable
    when TokLower x -> POk (STVar x) (advance state)

    -- Type constructor
    when TokUpper c -> POk (STCon c) (advance state)

    -- Parenthesized type or tuple type
    when TokLParen -> parseParenType (advance state)

    -- Record type
    when TokLBrace -> parseRecordType (advance state)

    when _ -> PErr "Expected type" (getPos state)
  end

  and parseParenType state =
    if peek state == TokRParen then
      POk (STTuple Nil) (advance state)
    else
      match parseType state
        when PErr e p -> PErr e p
        when POk ty s2 ->
          match peek s2
            when TokRParen -> POk ty (advance s2)
            when TokComma -> parseTupleTypeRest [ty] (advance s2)
            when _ -> PErr "Expected ')' or ','" (getPos s2)
          end
      end

  and parseTupleTypeRest acc state =
    match parseType state
      when PErr e p -> PErr e p
      when POk ty s2 ->
        let newAcc = snoc acc ty in
        match peek s2
          when TokComma -> parseTupleTypeRest newAcc (advance s2)
          when TokRParen -> POk (STTuple newAcc) (advance s2)
          when _ -> PErr "Expected ',' or ')'" (getPos s2)
        end
    end

  and parseRecordType state =
    if peek state == TokRBrace then
      POk (STRecord Nil) (advance state)
    else
      parseRecordTypeFields Nil state

  and parseRecordTypeFields acc state =
    match peek state
      when TokLower x ->
        match expect TokColon (advance state)
          when PErr e p -> PErr e p
          when POk _ s2 ->
            match parseType s2
              when PErr e p -> PErr e p
              when POk ty s3 ->
                let newAcc = snoc acc (STypeField x ty) in
                match peek s3
                  when TokComma -> parseRecordTypeFields newAcc (advance s3)
                  when TokRBrace -> POk (STRecord newAcc) (advance s3)
                  when _ -> PErr "Expected ',' or '}'" (getPos s3)
                end
            end
        end
      when TokRBrace -> POk (STRecord acc) (advance state)
      when _ -> PErr "Expected field name or '}'" (getPos state)
    end

  -- ===========================================================================
  -- Declaration Parsing
  -- ===========================================================================

  and parseDecl state = match peek state
    when TokType -> parseTypeDecl (advance state)
    when TokLet -> parseLetDecl (advance state)
    when TokForeign -> parseForeignDecl (advance state)
    when TokModule -> parseModuleDecl (advance state)
    when _ -> PErr "Expected declaration" (getPos state)
  end

  and parseTypeDecl state =
    match peek state
      when TokUpper name ->
        match parseTypeParams Nil (advance state)
          when PErr e p -> PErr e p
          when POk params s2 ->
            match expect TokEq s2
              when PErr e p -> PErr e p
              when POk _ s3 -> parseConstructors name params Nil s3
            end
        end
      when _ -> PErr "Expected type name" (getPos state)
    end

  and parseTypeParams acc state =
    match peek state
      when TokLower x -> parseTypeParams (snoc acc x) (advance state)
      when _ -> POk acc state
    end

  and parseConstructors typeName typeParams acc state =
    match peek state
      when TokUpper conName ->
        match parseConDeclFields Nil (advance state)
          when PErr e p -> PErr e p
          when POk fields s2 ->
            let newAcc = snoc acc (SConDecl conName fields) in
            match peek s2
              when TokBar -> parseConstructors typeName typeParams newAcc (advance s2)
              when _ -> POk (SDeclType typeName typeParams newAcc) s2
            end
        end
      when _ -> PErr "Expected constructor name" (getPos state)
    end

  and parseConDeclFields acc state =
    let tok = peek state in
    if canStartTypeAtom tok then
      match parseTypeAtom state
        when PErr e p -> PErr e p
        when POk ty s2 -> parseConDeclFields (snoc acc ty) s2
      end
    else POk acc state

  and parseLetDecl state =
    match peek state
      when TokRec -> parseLetRecDecl (advance state)
      when TokLower name -> parseLetSimpleDecl name (advance state)
      when _ -> PErr "Expected 'rec' or identifier" (getPos state)
    end

  and parseLetSimpleDecl name state =
    match parseParams Nil state
      when PErr e p -> PErr e p
      when POk params s2 ->
        -- Check for type annotation
        match peek s2
          when TokColon ->
            match parseType (advance s2)
              when PErr e p -> PErr e p
              when POk ty s3 ->
                match expect TokEq s3
                  when PErr e p -> PErr e p
                  when POk _ s4 ->
                    match parseExpr 0 s4
                      when PErr e p -> PErr e p
                      when POk value s5 ->
                        let boundExpr = if List.isEmpty params then value else SAbs params value in
                        POk (SDeclLet name (SAnnot boundExpr ty)) s5
                    end
                end
            end
          when TokEq ->
            match parseExpr 0 (advance s2)
              when PErr e p -> PErr e p
              when POk value s3 ->
                let boundExpr = if List.isEmpty params then value else SAbs params value in
                POk (SDeclLet name boundExpr) s3
            end
          when _ -> PErr "Expected '=' or ':'" (getPos s2)
        end
    end

  and parseLetRecDecl state =
    match parseRecDeclBinding state
      when PErr e p -> PErr e p
      when POk firstBind s2 -> parseLetRecDeclRest [firstBind] s2
    end

  and parseLetRecDeclRest acc state =
    match peek state
      when TokAnd ->
        match parseRecDeclBinding (advance state)
          when PErr e p -> PErr e p
          when POk bind s2 -> parseLetRecDeclRest (snoc acc bind) s2
        end
      when _ -> POk (SDeclLetRec acc) state
    end

  and parseRecDeclBinding state =
    match peek state
      when TokLower name ->
        match parseParams Nil (advance state)
          when PErr e p -> PErr e p
          when POk params s2 ->
            match expect TokEq s2
              when PErr e p -> PErr e p
              when POk _ s3 ->
                match parseExpr 0 s3
                  when PErr e p -> PErr e p
                  when POk value s4 ->
                    let boundExpr = if List.isEmpty params then value else SAbs params value in
                    POk (SBinding name boundExpr) s4
                end
            end
        end
      when _ -> PErr "Expected identifier" (getPos state)
    end

  and parseForeignDecl state =
    match peek state
      when TokLower name ->
        match expect TokColon (advance state)
          when PErr e p -> PErr e p
          when POk _ s2 ->
            match parseType s2
              when PErr e p -> PErr e p
              when POk ty s3 -> POk (SDeclForeign name ty) s3
            end
        end
      when _ -> PErr "Expected identifier" (getPos state)
    end

  and parseModuleDecl state =
    match peek state
      when TokUpper name ->
        parseModuleBody name Nil Nil (advance state)
      when _ -> PErr "Expected module name" (getPos state)
    end

  and parseModuleBody name uses decls state =
    match peek state
      when TokUse ->
        match parseUseDecl (advance state)
          when PErr e p -> PErr e p
          when POk u s2 -> parseModuleBody name (snoc uses u) decls s2
        end
      when TokType ->
        match parseTypeDecl (advance state)
          when PErr e p -> PErr e p
          when POk d s2 -> parseModuleBody name uses (snoc decls d) s2
        end
      when TokLet ->
        match parseLetDecl (advance state)
          when PErr e p -> PErr e p
          when POk d s2 ->
            -- Handle 'and' for mutually recursive bindings
            parseModuleBodyAndBindings name uses (snoc decls d) s2
        end
      when TokAnd ->
        -- Handle 'and' as continuation of previous let rec
        match parseAndBinding (advance state)
          when PErr e p -> PErr e p
          when POk d s2 -> parseModuleBodyAndBindings name uses (snoc decls d) s2
        end
      when TokForeign ->
        match parseForeignDecl (advance state)
          when PErr e p -> PErr e p
          when POk d s2 -> parseModuleBody name uses (snoc decls d) s2
        end
      when TokEnd -> POk (SDeclModule name uses decls) (advance state)
      when _ -> PErr "Expected 'use', 'type', 'let', 'foreign', or 'end'" (getPos state)
    end

  -- After parsing a let/and binding, check for more 'and' bindings
  and parseModuleBodyAndBindings name uses decls state =
    match peek state
      when TokAnd ->
        match parseAndBinding (advance state)
          when PErr e p -> PErr e p
          when POk d s2 -> parseModuleBodyAndBindings name uses (snoc decls d) s2
        end
      when _ -> parseModuleBody name uses decls state
    end

  -- Parse an 'and' binding (like a let rec binding but without 'let rec')
  and parseAndBinding state =
    match peek state
      when TokLower name ->
        match parseParams Nil (advance state)
          when PErr e p -> PErr e p
          when POk params s2 ->
            match expect TokEq s2
              when PErr e p -> PErr e p
              when POk _ s3 ->
                match parseExpr 0 s3
                  when PErr e p -> PErr e p
                  when POk value s4 ->
                    let boundExpr = if List.isEmpty params then value else SAbs params value in
                    POk (SDeclLetRec [SBinding name boundExpr]) s4
                end
            end
        end
      when _ -> PErr "Expected identifier after 'and'" (getPos state)
    end

  and parseUseDecl state =
    match peek state
      when TokUpper modName ->
        let s2 = advance state in
        match peek s2
          when TokLParen ->
            let s3 = advance s2 in
            match peek s3
              when TokDot ->
                match peek (advance s3)
                  when TokDot ->
                    match expect TokRParen (advance (advance s3))
                      when PErr e p -> PErr e p
                      when POk _ s4 -> POk (SUseAll modName) s4
                    end
                  when _ -> PErr "Expected '..' for import all" (getPos (advance s3))
                end
              when _ -> parseUseItems modName Nil s3
            end
          when _ -> POk (SUseQualified modName) s2
        end
      when _ -> PErr "Expected module name" (getPos state)
    end

  and parseUseItems modName acc state =
    match peek state
      when TokLower name ->
        let newAcc = snoc acc name in
        let s2 = advance state in
        match peek s2
          when TokComma -> parseUseItems modName newAcc (advance s2)
          when TokRParen -> POk (SUseItems modName newAcc) (advance s2)
          when _ -> PErr "Expected ',' or ')'" (getPos s2)
        end
      when TokUpper name ->
        let newAcc = snoc acc name in
        let s2 = advance state in
        match peek s2
          when TokComma -> parseUseItems modName newAcc (advance s2)
          when TokRParen -> POk (SUseItems modName newAcc) (advance s2)
          when _ -> PErr "Expected ',' or ')'" (getPos s2)
        end
      when TokRParen -> POk (SUseItems modName acc) (advance state)
      when _ -> PErr "Expected identifier or ')'" (getPos state)
    end

  -- ===========================================================================
  -- Program Parsing
  -- ===========================================================================

  and parseProgram state =
    parseDeclsAndExpr Nil state

  and parseDeclsAndExpr decls state =
    match peek state
      when TokEof -> POk (SProgram decls Nothing) state
      when TokUse ->
        match parseUseDecl (advance state)
          when PErr e p -> PErr e p
          when POk u s2 -> parseDeclsAndExpr (snoc decls (SDeclUse u)) s2
        end
      when TokType ->
        match parseTypeDecl (advance state)
          when PErr e p -> PErr e p
          when POk d s2 -> parseDeclsAndExpr (snoc decls d) s2
        end
      when TokModule ->
        match parseModuleDecl (advance state)
          when PErr e p -> PErr e p
          when POk d s2 -> parseDeclsAndExpr (snoc decls d) s2
        end
      when TokForeign ->
        match parseForeignDecl (advance state)
          when PErr e p -> PErr e p
          when POk d s2 -> parseDeclsAndExpr (snoc decls d) s2
        end
      when TokLet ->
        -- Could be declaration or expression
        parseLetDeclOrExpr decls (advance state)
      when TokIn ->
        -- trailing 'in expr' for the program
        match parseExpr 0 (advance state)
          when PErr e p -> PErr e p
          when POk expr s2 -> POk (SProgram decls (Just expr)) s2
        end
      when _ ->
        -- Try to parse as expression
        match parseExpr 0 state
          when PErr e p -> PErr e p
          when POk expr s2 -> POk (SProgram decls (Just expr)) s2
        end
    end

  and parseLetDeclOrExpr decls state =
    match peek state
      when TokRec ->
        match parseLetRecDecl (advance state)
          when PErr e p -> PErr e p
          when POk d s2 ->
            -- Check if followed by 'in' (expression) or not (declaration)
            match peek s2
              when TokIn ->
                -- Convert to expression
                match parseExpr 0 (advance s2)
                  when PErr e p -> PErr e p
                  when POk body s3 ->
                    let bindings = match d when SDeclLetRec bs -> bs when _ -> Nil end in
                    let expr = SLetRec bindings body in
                    POk (SProgram decls (Just expr)) s3
                end
              when _ -> parseDeclsAndExpr (snoc decls d) s2
            end
        end
      when TokLParen ->
        -- Destructuring pattern: let (a, b) = expr in body
        -- Always produces an expression, not a binding
        match parseLetDestructure state
          when PErr e p -> PErr e p
          when POk expr s2 -> POk (SProgram decls (Just expr)) s2
        end
      when TokLBrace ->
        -- Record destructuring pattern: let {x, y} = expr in body
        match parseLetDestructure state
          when PErr e p -> PErr e p
          when POk expr s2 -> POk (SProgram decls (Just expr)) s2
        end
      when TokUnderscore ->
        -- Wildcard pattern: let _ = expr in body
        match parseLetDestructure state
          when PErr e p -> PErr e p
          when POk expr s2 -> POk (SProgram decls (Just expr)) s2
        end
      when TokUpper _ ->
        -- Constructor pattern: let Just x = expr in body
        match parseLetDestructure state
          when PErr e p -> PErr e p
          when POk expr s2 -> POk (SProgram decls (Just expr)) s2
        end
      when TokLower name ->
        match parseParams Nil (advance state)
          when PErr e p -> PErr e p
          when POk params s2 ->
            match peek s2
              when TokColon ->
                -- Type annotation - definitely a declaration
                match parseType (advance s2)
                  when PErr e p -> PErr e p
                  when POk ty s3 ->
                    match expect TokEq s3
                      when PErr e p -> PErr e p
                      when POk _ s4 ->
                        match parseExpr 0 s4
                          when PErr e p -> PErr e p
                          when POk value s5 ->
                            let boundExpr = if List.isEmpty params then value else SAbs params value in
                            let d = SDeclLet name (SAnnot boundExpr ty) in
                            parseDeclsAndExpr (snoc decls d) s5
                        end
                    end
                end
              when TokEq ->
                match parseExpr 0 (advance s2)
                  when PErr e p -> PErr e p
                  when POk value s3 ->
                    let boundExpr = if List.isEmpty params then value else SAbs params value in
                    -- Check if followed by 'in' (expression) or not (declaration)
                    match peek s3
                      when TokIn ->
                        match parseExpr 0 (advance s3)
                          when PErr e p -> PErr e p
                          when POk body s4 ->
                            let expr = SLet name boundExpr body in
                            POk (SProgram decls (Just expr)) s4
                        end
                      when _ ->
                        let d = SDeclLet name boundExpr in
                        parseDeclsAndExpr (snoc decls d) s3
                    end
                end
              when _ -> PErr "Expected '=' or ':'" (getPos s2)
            end
        end
      when _ -> PErr "Expected 'rec' or identifier" (getPos state)
    end

  -- ===========================================================================
  -- Entry Point
  -- ===========================================================================

  let parse tokens =
    let state = PState tokens 0 in
    match parseProgram state
      when PErr msg pos -> ParseErr msg pos
      when POk program _ -> ParseOk program Nil
    end

end
