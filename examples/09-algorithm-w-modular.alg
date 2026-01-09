-- =============================================================================
-- Algorithm W: Hindley-Milner Type Inference
-- A clean, modular implementation
-- =============================================================================

-- =============================================================================
-- Module: Types
-- Core type definitions for the language
-- =============================================================================

module Types
  -- Tokens produced by the lexer
  type Token
    = TkInt Int
    | TkBool bool
    | TkIdent string
    | TkLambda
    | TkArrow
    | TkLet
    | TkIn
    | TkIf
    | TkThen
    | TkElse
    | TkLParen
    | TkRParen
    | TkEq
    | TkEof

  -- Abstract syntax tree
  type Expr
    = EInt Int
    | EBool bool
    | EVar string
    | EAbs string Expr
    | EApp Expr Expr
    | ELet string Expr Expr
    | EIf Expr Expr Expr

  -- Types in our type system
  type Type
    = TVar string
    | TInt
    | TBool
    | TFun Type Type

  -- Type scheme: quantified type (forall vars. type)
  type Scheme = Scheme (List string) Type

  -- Result of parsing
  type ParseResult
    = ParseOk Expr (List Token)
    | ParseErr string
end

-- =============================================================================
-- Module: Lexer
-- Tokenization
-- =============================================================================

module Lexer
  use Types (..)

  let rec skipWhitespace s =
    match String.head s
      when Just c if Char.isSpace c -> skipWhitespace (String.tail s)
      when _ -> s
    end

  let rec collectIdent acc s =
    match String.head s
      when Just c if Char.isIdentChar c -> collectIdent (acc + Char.toString c) (String.tail s)
      when _ -> (acc, s)
    end

  let rec collectDigits acc s =
    match String.head s
      when Just c if Char.isDigit c -> collectDigits (acc + Char.toString c) (String.tail s)
      when _ -> (acc, s)
    end

  let keywords = Map.fromList [
    ("let", TkLet), ("in", TkIn), ("if", TkIf), ("then", TkThen), ("else", TkElse),
    ("true", TkBool true), ("false", TkBool false)
  ]

  let identToToken ident =
    match Map.lookup ident keywords
      when Just tok -> tok
      when Nothing -> TkIdent ident
    end

  let rec lexToken s =
    let s2 = skipWhitespace s in
    match String.head s2
      when Nothing -> (TkEof, s2)
      when Just c ->
        let rest = String.tail s2 in
        if Char.isLower c || c == '_' then
          match collectIdent "" s2 when (id, r) -> (identToToken id, r) end
        else if Char.isDigit c then
          match collectDigits "" s2 when (d, r) ->
            match Int.fromString d
              when Just n -> (TkInt n, r)
              when Nothing -> (TkEof, r)  -- fallback
            end
          end
        else if c == '-' then
          match String.head rest
            when Just '>' -> (TkArrow, String.tail rest)
            when _ -> (TkEof, rest)
          end
        else if c == '\\' then (TkLambda, rest)
        else if c == '=' then (TkEq, rest)
        else if c == '(' then (TkLParen, rest)
        else if c == ')' then (TkRParen, rest)
        else (TkEof, rest)
    end

  let rec tokenize s =
    match lexToken s
      when (TkEof, _) -> [TkEof]
      when (t, r) -> t :: tokenize r
    end
end

-- =============================================================================
-- Module: Parser
-- Recursive descent parser
-- =============================================================================

module Parser
  use Types (..)

  let peek ts =
    match ts
      when Nil -> TkEof
      when Cons t _ -> t
    end

  let advance ts =
    match ts
      when Nil -> Nil
      when Cons _ r -> r
    end

  let rec parseAtom ts =
    match peek ts
      when TkInt n -> ParseOk (EInt n) (advance ts)
      when TkBool b -> ParseOk (EBool b) (advance ts)
      when TkIdent x -> ParseOk (EVar x) (advance ts)
      when TkLParen ->
        match parseExpr (advance ts)
          when ParseErr e -> ParseErr e
          when ParseOk e r ->
            match peek r
              when TkRParen -> ParseOk e (advance r)
              when _ -> ParseErr "expected )"
            end
        end
      when TkLambda -> parseLambda (advance ts)
      when TkLet -> parseLet (advance ts)
      when TkIf -> parseIf (advance ts)
      when _ -> ParseErr "unexpected token"
    end

  and parseLambda ts =
    match peek ts
      when TkIdent p ->
        let r = advance ts in
        match peek r
          when TkArrow ->
            match parseExpr (advance r)
              when ParseErr e -> ParseErr e
              when ParseOk b r2 -> ParseOk (EAbs p b) r2
            end
          when _ -> ParseErr "expected ->"
        end
      when _ -> ParseErr "expected parameter"
    end

  and parseLet ts =
    match peek ts
      when TkIdent n ->
        let r = advance ts in
        match peek r
          when TkEq ->
            match parseExpr (advance r)
              when ParseErr e -> ParseErr e
              when ParseOk e1 r2 ->
                match peek r2
                  when TkIn ->
                    match parseExpr (advance r2)
                      when ParseErr e -> ParseErr e
                      when ParseOk e2 r3 -> ParseOk (ELet n e1 e2) r3
                    end
                  when _ -> ParseErr "expected in"
                end
            end
          when _ -> ParseErr "expected ="
        end
      when _ -> ParseErr "expected identifier"
    end

  and parseIf ts =
    match parseExpr ts
      when ParseErr e -> ParseErr e
      when ParseOk c r ->
        match peek r
          when TkThen ->
            match parseExpr (advance r)
              when ParseErr e -> ParseErr e
              when ParseOk t r2 ->
                match peek r2
                  when TkElse ->
                    match parseExpr (advance r2)
                      when ParseErr e -> ParseErr e
                      when ParseOk f r3 -> ParseOk (EIf c t f) r3
                    end
                  when _ -> ParseErr "expected else"
                end
            end
          when _ -> ParseErr "expected then"
        end
    end

  and parseApp ts =
    match parseAtom ts
      when ParseErr e -> ParseErr e
      when ParseOk l r -> parseAppRest l r
    end

  and parseAppRest l ts =
    match peek ts
      when TkInt _ -> applyArg l ts
      when TkBool _ -> applyArg l ts
      when TkIdent _ -> applyArg l ts
      when TkLParen -> applyArg l ts
      when _ -> ParseOk l ts
    end

  and applyArg l ts =
    match parseAtom ts
      when ParseErr e -> ParseErr e
      when ParseOk r rest -> parseAppRest (EApp l r) rest
    end

  and parseExpr ts = parseApp ts

  let parse ts =
    match parseExpr ts
      when ParseErr e -> Left e
      when ParseOk e r ->
        match peek r
          when TkEof -> Right e
          when _ -> Left "unexpected tokens after expression"
        end
    end
end

-- =============================================================================
-- Module: Subst
-- Substitution operations
-- =============================================================================

module Subst
  use Types (..)

  let rec applySubst sub ty =
    match ty
      when TVar n ->
        match Map.lookup n sub
          when Just t -> applySubst sub t
          when Nothing -> ty
        end
      when TInt -> TInt
      when TBool -> TBool
      when TFun a b -> TFun (applySubst sub a) (applySubst sub b)
    end

  let applySubstScheme sub sch =
    match sch
      when Scheme vs ty ->
        let filtered = List.foldl (s v -> Map.delete v s) sub vs in
        Scheme vs (applySubst filtered ty)
    end

  let applySubstEnv sub env =
    Map.fromList (List.map (p -> match p when (k, v) -> (k, applySubstScheme sub v) end) (Map.toList env))

  let composeSubst s1 s2 =
    let s1Applied = Map.fromList (List.map (p -> match p when (k, v) -> (k, applySubst s2 v) end) (Map.toList s1)) in
    List.foldl (acc p -> match p when (k, v) -> if Map.member k acc then acc else Map.insert k v acc end) s1Applied (Map.toList s2)
end

-- =============================================================================
-- Module: Infer
-- Type inference (Algorithm W)
-- =============================================================================

module Infer
  use Types (..)
  use Subst (..)

  -- Free type variables
  let rec freeVarsType ty =
    match ty
      when TVar n -> Set.singleton n
      when TInt -> Set.empty
      when TBool -> Set.empty
      when TFun a b -> Set.union (freeVarsType a) (freeVarsType b)
    end

  let freeVarsScheme sch =
    match sch
      when Scheme vs ty -> Set.difference (freeVarsType ty) (Set.fromList vs)
    end

  let freeVarsEnv env =
    List.foldl (acc p -> match p when (_, s) -> Set.union acc (freeVarsScheme s) end) Set.empty (Map.toList env)

  -- Generalization and instantiation
  let generalize env ty =
    Scheme (Set.toList (Set.difference (freeVarsType ty) (freeVarsEnv env))) ty

  let freshVar n = "t" + Int.toString n

  let instantiate cnt sch =
    match sch
      when Scheme vs ty ->
        let rec mkSubst n vlist =
          match vlist
            when Nil -> (n, Map.empty)
            when Cons v rest ->
              match mkSubst (n + 1) rest
                when (n2, s) -> (n2, Map.insert v (TVar (freshVar n)) s)
              end
          end
        in
        match mkSubst cnt vs
          when (n, sub) -> (n, applySubst sub ty)
        end
    end

  -- Occurs check (prevents infinite types)
  let rec occursIn n ty =
    match ty
      when TVar m -> n == m
      when TInt -> false
      when TBool -> false
      when TFun a b -> occursIn n a || occursIn n b
    end

  -- Unification
  let rec unify t1 t2 =
    match (t1, t2)
      when (TInt, TInt) -> Right Map.empty
      when (TBool, TBool) -> Right Map.empty
      when (TVar a, TVar b) ->
        if a == b then Right Map.empty
        else Right (Map.singleton a (TVar b))
      when (TVar a, t) ->
        if occursIn a t then Left "infinite type"
        else Right (Map.singleton a t)
      when (t, TVar a) ->
        if occursIn a t then Left "infinite type"
        else Right (Map.singleton a t)
      when (TFun a1 b1, TFun a2 b2) ->
        match unify a1 a2
          when Left e -> Left e
          when Right s1 ->
            match unify (applySubst s1 b1) (applySubst s1 b2)
              when Left e -> Left e
              when Right s2 -> Right (composeSubst s1 s2)
            end
        end
      when _ -> Left "type mismatch"
    end

  -- Main inference function
  let rec infer cnt env expr =
    match expr
      when EInt _ -> Right (cnt, Map.empty, TInt)
      when EBool _ -> Right (cnt, Map.empty, TBool)
      when EVar x ->
        match Map.lookup x env
          when Nothing -> Left ("unbound variable: " + x)
          when Just sch ->
            match instantiate cnt sch
              when (n, ty) -> Right (n, Map.empty, ty)
            end
        end
      when EAbs p body ->
        let tv = TVar (freshVar cnt) in
        match infer (cnt + 1) (Map.insert p (Scheme Nil tv) env) body
          when Left e -> Left e
          when Right (n, s, bty) -> Right (n, s, TFun (applySubst s tv) bty)
        end
      when EApp f arg ->
        match infer cnt env f
          when Left e -> Left e
          when Right (n1, s1, fty) ->
            match infer n1 (applySubstEnv s1 env) arg
              when Left e -> Left e
              when Right (n2, s2, aty) ->
                let tv = TVar (freshVar n2) in
                match unify (applySubst s2 fty) (TFun aty tv)
                  when Left e -> Left e
                  when Right s3 -> Right (n2 + 1, composeSubst (composeSubst s1 s2) s3, applySubst s3 tv)
                end
            end
        end
      when ELet x e1 e2 ->
        match infer cnt env e1
          when Left e -> Left e
          when Right (n1, s1, t1) ->
            let env2 = applySubstEnv s1 env in
            match infer n1 (Map.insert x (generalize env2 t1) env2) e2
              when Left e -> Left e
              when Right (n2, s2, t2) -> Right (n2, composeSubst s1 s2, t2)
            end
        end
      when EIf c t f ->
        match infer cnt env c
          when Left e -> Left e
          when Right (n1, s1, cty) ->
            match unify cty TBool
              when Left e -> Left e
              when Right s2 ->
                let env2 = applySubstEnv (composeSubst s1 s2) env in
                match infer n1 env2 t
                  when Left e -> Left e
                  when Right (n2, s3, tty) ->
                    match infer n2 (applySubstEnv s3 env2) f
                      when Left e -> Left e
                      when Right (n3, s4, fty) ->
                        match unify (applySubst s4 tty) fty
                          when Left e -> Left e
                          when Right s5 ->
                            Right (n3, composeSubst (composeSubst (composeSubst (composeSubst s1 s2) s3) s4) s5, applySubst s5 fty)
                        end
                    end
                end
            end
        end
    end

  let inferType e =
    match infer 0 Map.empty e
      when Left e -> Left e
      when Right (_, s, t) -> Right (applySubst s t)
    end
end

-- =============================================================================
-- Module: Pretty
-- Pretty printing
-- =============================================================================

module Pretty
  use Types (..)

  let rec showType ty =
    match ty
      when TVar n -> n
      when TInt -> "Int"
      when TBool -> "Bool"
      when TFun a b ->
        let left = match a
          when TFun _ _ -> "(" + showType a + ")"
          when _ -> showType a
        end in
        left + " -> " + showType b
    end
end

-- =============================================================================
-- Main: Test runner
-- =============================================================================

let runInfer input =
  let tokens = Lexer.tokenize input in
  match Parser.parse tokens
    when Left e -> "Parse error: " + e
    when Right expr ->
      match Infer.inferType expr
        when Left e -> "Type error: " + e
        when Right t -> Pretty.showType t
      end
  end

let test name input =
  let _ = Debug.log (name + ": " + input) in
  Debug.log ("  => " + runInfer input)

in let _ = test "int" "42"
in let _ = test "bool" "true"
in let _ = test "id" "\\x -> x"
in let _ = test "const" "\\x -> \\y -> x"
in let _ = test "app" "(\\x -> x) 42"
in let _ = test "let" "let id = \\x -> x in id 42"
in let _ = test "poly" "let id = \\x -> x in id"
in let _ = test "if" "if true then 1 else 2"
in let _ = test "compose" "\\f -> \\g -> \\x -> f (g x)"
in let _ = test "err:app" "1 1"
in let _ = test "err:if" "if 1 then 2 else 3"
in "Done!"
