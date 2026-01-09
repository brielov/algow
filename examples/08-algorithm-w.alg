-- Self-Hosted Algorithm W (Hindley-Milner Type Inference)
-- A minimal type checker for lambda calculus

-- =============================================================================
-- TYPES
-- =============================================================================

type Token
  = TkInt int | TkBool bool | TkIdent string
  | TkLambda | TkArrow | TkLet | TkIn | TkIf | TkThen | TkElse
  | TkLParen | TkRParen | TkEq | TkEof | TkError string

type Expr
  = EInt int | EBool bool | EVar string
  | EAbs string Expr | EApp Expr Expr
  | ELet string Expr Expr | EIf Expr Expr Expr

type Type = TVar string | TInt | TBool | TFun Type Type

type Scheme = Scheme (List string) Type

type ParseResult = ParseOk Expr (List Token) | ParseErr string

-- =============================================================================
-- IMPLEMENTATION (all functions in one mutual recursion block)
-- =============================================================================

let rec skipWhitespace s =
  match String.head s
    when Just c if Char.isSpace c -> skipWhitespace (String.tail s)
    when _ -> s
  end

and collectIdent acc s =
  match String.head s
    when Just c if Char.isIdentChar c -> collectIdent (acc + Char.toString c) (String.tail s)
    when _ -> (acc, s)
  end

and collectDigits acc s =
  match String.head s
    when Just c if Char.isDigit c -> collectDigits (acc + Char.toString c) (String.tail s)
    when _ -> (acc, s)
  end

and keywords u = Map.fromList [
  ("let", TkLet), ("in", TkIn), ("if", TkIf), ("then", TkThen), ("else", TkElse),
  ("true", TkBool true), ("false", TkBool false)
]

and identToToken ident =
  match Map.lookup ident (keywords 0)
    when Just tok -> tok
    when Nothing -> TkIdent ident
  end

and lexToken s =
  let s2 = skipWhitespace s in
  match String.head s2
    when Nothing -> (TkEof, s2)
    when Just c ->
      let rest = String.tail s2 in
      if Char.isLower c || c == '_' then
        match collectIdent "" s2 when (id, r) -> (identToToken id, r) end
      else if Char.isDigit c then
        match collectDigits "" s2 when (d, r) ->
          match Int.fromString d when Just n -> (TkInt n, r) when Nothing -> (TkError "bad", r) end
        end
      else if c == '-' then
        match String.head rest when Just '>' -> (TkArrow, String.tail rest) when _ -> (TkError "-", rest) end
      else if c == '\\' then (TkLambda, rest)
      else if c == '=' then (TkEq, rest)
      else if c == '(' then (TkLParen, rest)
      else if c == ')' then (TkRParen, rest)
      else (TkError "?", rest)
  end

and tokenize s =
  match lexToken s when (TkEof, _) -> [TkEof] when (t, r) -> t :: tokenize r end

-- Parser helpers
and peek ts = match ts when Nil -> TkEof when Cons t _ -> t end
and advance ts = match ts when Nil -> Nil when Cons _ r -> r end

-- Parser (mutually recursive)
and parseAtom ts = match peek ts
  when TkInt n -> ParseOk (EInt n) (advance ts)
  when TkBool b -> ParseOk (EBool b) (advance ts)
  when TkIdent x -> ParseOk (EVar x) (advance ts)
  when TkLParen ->
    match parseExpr (advance ts)
      when ParseErr e -> ParseErr e
      when ParseOk e r -> match peek r when TkRParen -> ParseOk e (advance r) when _ -> ParseErr ")" end
    end
  when TkLambda -> parseLambda (advance ts)
  when TkLet -> parseLet (advance ts)
  when TkIf -> parseIf (advance ts)
  when _ -> ParseErr "token"
end

and parseLambda ts = match peek ts
  when TkIdent p -> let r = advance ts in match peek r
    when TkArrow -> match parseExpr (advance r) when ParseErr e -> ParseErr e when ParseOk b r2 -> ParseOk (EAbs p b) r2 end
    when _ -> ParseErr "->"
  end
  when _ -> ParseErr "param"
end

and parseLet ts = match peek ts
  when TkIdent n -> let r = advance ts in match peek r
    when TkEq -> match parseExpr (advance r)
      when ParseErr e -> ParseErr e
      when ParseOk e1 r2 -> match peek r2
        when TkIn -> match parseExpr (advance r2) when ParseErr e -> ParseErr e when ParseOk e2 r3 -> ParseOk (ELet n e1 e2) r3 end
        when _ -> ParseErr "in"
      end
    end
    when _ -> ParseErr "="
  end
  when _ -> ParseErr "id"
end

and parseIf ts = match parseExpr ts
  when ParseErr e -> ParseErr e
  when ParseOk c r -> match peek r
    when TkThen -> match parseExpr (advance r)
      when ParseErr e -> ParseErr e
      when ParseOk t r2 -> match peek r2
        when TkElse -> match parseExpr (advance r2) when ParseErr e -> ParseErr e when ParseOk f r3 -> ParseOk (EIf c t f) r3 end
        when _ -> ParseErr "else"
      end
    end
    when _ -> ParseErr "then"
  end
end

and parseApp ts = match parseAtom ts when ParseErr e -> ParseErr e when ParseOk l r -> parseAppRest l r end

and parseAppRest l ts = match peek ts
  when TkInt _ -> applyC l ts
  when TkBool _ -> applyC l ts
  when TkIdent _ -> applyC l ts
  when TkLParen -> applyC l ts
  when _ -> ParseOk l ts
end

and applyC l ts = match parseAtom ts when ParseErr e -> ParseErr e when ParseOk r rest -> parseAppRest (EApp l r) rest end

and parseExpr ts = parseApp ts

and parse ts = match parseExpr ts
  when ParseErr e -> Left e
  when ParseOk e r -> match peek r when TkEof -> Right e when _ -> Left "extra" end
end

-- Type Inference helpers
and freshVar n = "t" + Int.toString n

and applySubst sub ty = match ty
  when TVar n -> match Map.lookup n sub when Just t -> applySubst sub t when Nothing -> ty end
  when TInt -> TInt
  when TBool -> TBool
  when TFun a b -> TFun (applySubst sub a) (applySubst sub b)
end

and applySubstScheme sub sch = match sch
  when Scheme vs ty -> Scheme vs (applySubst (List.foldl (s v -> Map.delete v s) sub vs) ty)
end

and applySubstEnv sub env =
  Map.fromList (List.map (p -> match p when (k, v) -> (k, applySubstScheme sub v) end) (Map.toList env))

and composeSubst s1 s2 =
  let s1a = Map.fromList (List.map (p -> match p when (k, v) -> (k, applySubst s2 v) end) (Map.toList s1)) in
  List.foldl (a p -> match p when (k, v) -> if Map.member k a then a else Map.insert k v a end) s1a (Map.toList s2)

and freeVarsType ty = match ty
  when TVar n -> Set.singleton n
  when TInt -> Set.empty
  when TBool -> Set.empty
  when TFun a b -> Set.union (freeVarsType a) (freeVarsType b)
end

and freeVarsScheme sch = match sch when Scheme vs ty -> Set.difference (freeVarsType ty) (Set.fromList vs) end

and freeVarsEnv env =
  List.foldl (a p -> match p when (_, s) -> Set.union a (freeVarsScheme s) end) Set.empty (Map.toList env)

and generalize env ty = Scheme (Set.toList (Set.difference (freeVarsType ty) (freeVarsEnv env))) ty

and instantiate cnt sch = match sch
  when Scheme vs ty ->
    let rec mk n vlist = match vlist when Nil -> (n, Map.empty) when Cons v r -> match mk (n+1) r when (n2, s) -> (n2, Map.insert v (TVar (freshVar n)) s) end end in
    match mk cnt vs when (n, sub) -> (n, applySubst sub ty) end
end

and occursIn n ty = match ty
  when TVar m -> n == m
  when TInt -> false
  when TBool -> false
  when TFun a b -> occursIn n a || occursIn n b
end

and unify t1 t2 = match (t1, t2)
  when (TInt, TInt) -> Right Map.empty
  when (TBool, TBool) -> Right Map.empty
  when (TVar a, TVar b) -> if a == b then Right Map.empty else Right (Map.singleton a (TVar b))
  when (TVar a, t) -> if occursIn a t then Left "inf" else Right (Map.singleton a t)
  when (t, TVar a) -> if occursIn a t then Left "inf" else Right (Map.singleton a t)
  when (TFun a1 b1, TFun a2 b2) -> match unify a1 a2
    when Left e -> Left e
    when Right s1 -> match unify (applySubst s1 b1) (applySubst s1 b2) when Left e -> Left e when Right s2 -> Right (composeSubst s1 s2) end
  end
  when _ -> Left "mismatch"
end

and infer cnt env expr = match expr
  when EInt _ -> Right (cnt, Map.empty, TInt)
  when EBool _ -> Right (cnt, Map.empty, TBool)
  when EVar x -> match Map.lookup x env
    when Nothing -> Left ("unbound: " + x)
    when Just sch -> match instantiate cnt sch when (n, ty) -> Right (n, Map.empty, ty) end
  end
  when EAbs p body ->
    let tv = TVar (freshVar cnt) in
    match infer (cnt+1) (Map.insert p (Scheme Nil tv) env) body
      when Left e -> Left e
      when Right (n, s, bty) -> Right (n, s, TFun (applySubst s tv) bty)
    end
  when EApp f arg ->
    match infer cnt env f when Left e -> Left e when Right (n1, s1, fty) ->
      match infer n1 (applySubstEnv s1 env) arg when Left e -> Left e when Right (n2, s2, aty) ->
        let tv = TVar (freshVar n2) in
        match unify (applySubst s2 fty) (TFun aty tv)
          when Left e -> Left e
          when Right s3 -> Right (n2+1, composeSubst (composeSubst s1 s2) s3, applySubst s3 tv)
        end
      end
    end
  when ELet x e1 e2 ->
    match infer cnt env e1 when Left e -> Left e when Right (n1, s1, t1) ->
      let env2 = applySubstEnv s1 env in
      match infer n1 (Map.insert x (generalize env2 t1) env2) e2
        when Left e -> Left e
        when Right (n2, s2, t2) -> Right (n2, composeSubst s1 s2, t2)
      end
    end
  when EIf c t f ->
    match infer cnt env c when Left e -> Left e when Right (n1, s1, cty) ->
      match unify cty TBool when Left e -> Left e when Right s2 ->
        let env2 = applySubstEnv (composeSubst s1 s2) env in
        match infer n1 env2 t when Left e -> Left e when Right (n2, s3, tty) ->
          match infer n2 (applySubstEnv s3 env2) f when Left e -> Left e when Right (n3, s4, fty) ->
            match unify (applySubst s4 tty) fty
              when Left e -> Left e
              when Right s5 -> Right (n3, composeSubst (composeSubst (composeSubst (composeSubst s1 s2) s3) s4) s5, applySubst s5 fty)
            end
          end
        end
      end
    end
end

and inferType e = match infer 0 Map.empty e when Left e -> Left e when Right (_, s, t) -> Right (applySubst s t) end

and showType ty = match ty
  when TVar n -> n
  when TInt -> "Int"
  when TBool -> "Bool"
  when TFun a b -> let l = match a when TFun _ _ -> "(" + showType a + ")" when _ -> showType a end in l + " -> " + showType b
end

-- =============================================================================
-- TESTS
-- =============================================================================

and runInfer i = let ts = tokenize i in match parse ts
  when Left e -> "Parse: " + e
  when Right e -> match inferType e when Left err -> "Type: " + err when Right t -> showType t end
end

and test n i = let _ = Debug.log (n + ": " + i) in Debug.log ("  => " + runInfer i)

in let _ = test "int" "42"
in let _ = test "bool" "true"
in let _ = test "id" "\\x -> x"
in let _ = test "const" "\\x -> \\y -> x"
in let _ = test "app" "(\\x -> x) 42"
in let _ = test "let" "let id = \\x -> x in id 42"
in let _ = test "poly" "let id = \\x -> x in id"
in let _ = test "if" "if true then 1 else 2"
in let _ = test "compose" "\\f -> \\g -> \\x -> f (g x)"
in let _ = test "err1" "1 1"
in let _ = test "err2" "if 1 then 2 else 3"
in "Done!"
