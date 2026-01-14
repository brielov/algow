-- Project: Type Checker (Algorithm W Implementation)
-- A complete Hindley-Milner type inference implementation
-- Types
type Type
  = TVar string
  | TInt
  | TBool
  | TFun Type Type

-- Type Scheme (for let-polymorphism)
type Scheme = Forall (List string) Type

-- Expressions
type Expr
  = EVar string
  | EInt int
  | EBool bool
  | ELam string Expr
  | EApp Expr Expr
  | ELet string Expr Expr
  | EIf Expr Expr Expr

-- Substitution (mapping from type variable to type)
type Subst = Subst (List (string, Type))

-- Type environment
type TypeEnv = TypeEnv (List (string, Scheme))

-- Fresh variable generation state
type Fresh = Fresh int

-- Result type for type inference
type InferResult a
  = InferOk a Fresh
  | InferErr string

-- Substitution operations
let emptySubst =
  Subst []

let singletonSubst =
  v t -> Subst [(v, t)]

let rec lookupSubst = v subst ->
  match subst
    when Subst pairs -> let rec find = ps ->
      match ps
        when Nil -> Nothing
        when Cons (k, t) rest -> if k == v
          then Just t
          else find rest
      end
    in
    find pairs
  end

-- Apply substitution to type
let rec applySubst = subst ty ->
  match ty
    when TVar v -> match lookupSubst v subst
      when Nothing -> TVar v
      when Just t -> t
    end
    when TInt -> TInt
    when TBool -> TBool
    when TFun t1 t2 -> TFun (applySubst subst t1) (applySubst subst t2)
  end

-- Apply substitution to scheme
let applySubstScheme =
  subst scheme ->
    match scheme
      when Forall vars t -> let subst_ = match subst
        when Subst pairs -> Subst (List.filter (p ->
          match p
            when (k, _) -> Bool.negate (List.elem k vars)
          end) pairs)
      end in
      Forall vars (applySubst subst_ t)
    end

-- Apply substitution to environment
let applySubstEnv =
  subst env ->
    match env
      when TypeEnv bindings -> TypeEnv (List.map (b ->
        match b
          when (n, s) -> (n, applySubstScheme subst s)
        end) bindings)
    end

-- Compose substitutions
let composeSubst =
  s1 s2 ->
    match (s1, s2)
      when (Subst p1, Subst p2) -> let applied = List.map (p ->
        match p
          when (k, t) -> (k, applySubst s1 t)
        end) p2 in
      Subst (List.append p1 applied)
    end

-- Free type variables in a type
let rec ftvType = ty ->
  match ty
    when TVar v -> [v]
    when TInt -> []
    when TBool -> []
    when TFun t1 t2 -> List.union (ftvType t1) (ftvType t2)
  end

-- Free type variables in a scheme
let ftvScheme =
  scheme ->
    match scheme
      when Forall vars t -> List.filter (v -> Bool.negate (List.elem v vars)) (ftvType t)
    end

-- Free type variables in environment
let ftvEnv =
  env ->
    match env
      when TypeEnv bindings -> List.concat (List.map (b ->
        match b
          when (_, s) -> ftvScheme s
        end) bindings)
    end

-- Fresh variable generation
let freshVar =
  fresh ->
    match fresh
      when Fresh n -> InferOk (TVar (String.concat "t" (Int.toString n))) (Fresh (n + 1))
    end

-- Environment operations
let emptyEnv =
  TypeEnv []

let extendEnv =
  name scheme env ->
    match env
      when TypeEnv bindings -> TypeEnv (Cons (name, scheme) bindings)
    end

let rec lookupEnv = name env ->
  match env
    when TypeEnv bindings -> let rec find = bs ->
      match bs
        when Nil -> Nothing
        when Cons (n, s) rest -> if n == name
          then Just s
          else find rest
      end
    in
    find bindings
  end

-- Generalize a type to a scheme
let generalize =
  env ty -> let envFtv = ftvEnv env in
  let vars = List.filter (v -> Bool.negate (List.elem v envFtv)) (ftvType ty) in
  Forall vars ty

-- Instantiate a scheme to a type
let rec instantiate = scheme fresh ->
  match scheme
    when Forall vars ty -> let rec mkSubst = vs subst f ->
      match vs
        when Nil -> InferOk subst f
        when Cons v rest -> match freshVar f
          when InferErr e -> InferErr e
          when InferOk newVar f2 -> mkSubst rest (composeSubst (singletonSubst v newVar) subst) f2
        end
      end
    in
    match mkSubst vars emptySubst fresh
      when InferErr e -> InferErr e
      when InferOk subst f2 -> InferOk (applySubst subst ty) f2
    end
  end

-- Occurs check
let rec occurs = v ty ->
  match ty
    when TVar v2 -> v == v2
    when TInt -> false
    when TBool -> false
    when TFun t1 t2 -> occurs v t1 || occurs v t2
  end

-- Check if types are identical (for var-var case)
let rec typesIdentical = t1 t2 ->
  match (t1, t2)
    when (TVar v1, TVar v2) -> v1 == v2
    when (TInt, TInt) -> true
    when (TBool, TBool) -> true
    when (TFun a1 r1, TFun a2 r2) -> typesIdentical a1 a2 && typesIdentical r1 r2
    when _ -> false
  end

-- Unification
let rec unify = t1 t2 ->
  match (t1, t2)
    when (TInt, TInt) -> Right emptySubst
    when (TBool, TBool) -> Right emptySubst
    when (TVar v1, TVar v2) -> if v1 == v2
      then Right emptySubst
      else Right (singletonSubst v1 (TVar v2))
    when (TVar v, t) -> if occurs v t
      then Left "infinite type"
      else Right (singletonSubst v t)
    when (t, TVar v) -> if occurs v t
      then Left "infinite type"
      else Right (singletonSubst v t)
    when (TFun a1 r1, TFun a2 r2) -> match unify a1 a2
      when Left e -> Left e
      when Right s1 -> match unify (applySubst s1 r1) (applySubst s1 r2)
        when Left e -> Left e
        when Right s2 -> Right (composeSubst s2 s1)
      end
    end
    when _ -> Left "type mismatch"
  end

-- Type inference (Algorithm W)
let rec infer = env expr fresh ->
  match expr
    when EInt _ -> InferOk (emptySubst, TInt) fresh
    when EBool _ -> InferOk (emptySubst, TBool) fresh
    when EVar name -> match lookupEnv name env
      when Nothing -> InferErr (String.concat "unbound variable: " name)
      when Just scheme -> match instantiate scheme fresh
        when InferErr e -> InferErr e
        when InferOk ty f2 -> InferOk (emptySubst, ty) f2
      end
    end
    when ELam param body -> match freshVar fresh
      when InferErr e -> InferErr e
      when InferOk paramTy f2 -> let env_ = extendEnv param (Forall [] paramTy) env in
      match infer env_ body f2
        when InferErr e -> InferErr e
        when InferOk (s, bodyTy) f3 -> InferOk (s, TFun (applySubst s paramTy) bodyTy) f3
      end
    end
    when EApp func arg -> match freshVar fresh
      when InferErr e -> InferErr e
      when InferOk resTy f2 -> match infer env func f2
        when InferErr e -> InferErr e
        when InferOk (s1, funcTy) f3 -> match infer (applySubstEnv s1 env) arg f3
          when InferErr e -> InferErr e
          when InferOk (s2, argTy) f4 -> match unify (applySubst s2 funcTy) (TFun argTy resTy)
            when Left e -> InferErr e
            when Right s3 -> InferOk (composeSubst s3 (composeSubst s2 s1), applySubst s3 resTy) f4
          end
        end
      end
    end
    when ELet name value body -> match infer env value fresh
      when InferErr e -> InferErr e
      when InferOk (s1, valueTy) f2 -> let env_ = applySubstEnv s1 env in
      let scheme = generalize env_ valueTy in
      let env__ = extendEnv name scheme env_ in
      match infer env__ body f2
        when InferErr e -> InferErr e
        when InferOk (s2, bodyTy) f3 -> InferOk (composeSubst s2 s1, bodyTy) f3
      end
    end
    when EIf cond then_ else_ -> match infer env cond fresh
      when InferErr e -> InferErr e
      when InferOk (s1, condTy) f2 -> match unify condTy TBool
        when Left e -> InferErr e
        when Right s2 -> let s12 = composeSubst s2 s1 in
        match infer (applySubstEnv s12 env) then_ f2
          when InferErr e -> InferErr e
          when InferOk (s3, thenTy) f3 -> match infer (applySubstEnv (composeSubst s3 s12) env) else_ f3
            when InferErr e -> InferErr e
            when InferOk (s4, elseTy) f4 -> match unify (applySubst s4 thenTy) elseTy
              when Left e -> InferErr e
              when Right s5 -> InferOk (composeSubst s5 (composeSubst s4 (composeSubst s3 s12)), applySubst s5 elseTy) f4
            end
          end
        end
      end
    end
  end

-- Top-level inference
let typeOf =
  expr ->
    match infer emptyEnv expr (Fresh 0)
      when InferErr e -> Left e
      when InferOk (subst, ty) _ -> Right (applySubst subst ty)
    end

-- Type equality (ignoring variable names)
let rec typeEq = t1 t2 ->
  match (t1, t2)
    when (TInt, TInt) -> true
    when (TBool, TBool) -> true
    when (TVar _, TVar _) -> true
    when (TFun a1 r1, TFun a2 r2) -> typeEq a1 a2 && typeEq r1 r2
    when _ -> false
  end

let typeMatches =
  expected actual ->
    match actual
      when Left _ -> false
      when Right t -> typeEq expected t
    end

-- Tests
let tests =
  [ Test.ok "int literal" (typeMatches TInt (typeOf (EInt 42)))
    , Test.ok "bool literal true" (typeMatches TBool (typeOf (EBool true)))
    , Test.ok "bool literal false" (typeMatches TBool (typeOf (EBool false)))
    , Test.ok "unbound var error" (Either.isLeft (typeOf (EVar "x")))
    , Test.ok "identity fn" (typeMatches (TFun (TVar "a") (TVar "a")) (typeOf (ELam "x" (EVar "x"))))
    , Test.ok "const fn" (typeMatches (TFun (TVar "a") (TFun (TVar "b") (TVar "a"))) (typeOf (ELam "x" (ELam "y" (EVar "x")))))
    , Test.ok "simple app" (typeMatches TInt (typeOf (EApp (ELam "x" (EVar "x")) (EInt 42))))
    , Test.ok "let id" (typeMatches TInt (typeOf (ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EInt 42)))))
    , Test.ok "let poly" (typeMatches TBool (typeOf (ELet "id" (ELam "x" (EVar "x")) (ELet "a" (EApp (EVar "id") (EInt 1)) (EApp (EVar "id") (EBool true))))))
    , Test.ok "if int" (typeMatches TInt (typeOf (EIf (EBool true) (EInt 1) (EInt 2))))
    , Test.ok "if bool" (typeMatches TBool (typeOf (EIf (EBool true) (EBool true) (EBool false))))
    , Test.ok "if cond not bool" (Either.isLeft (typeOf (EIf (EInt 1) (EInt 2) (EInt 3))))
    , Test.ok "if branches differ" (Either.isLeft (typeOf (EIf (EBool true) (EInt 1) (EBool false))))
    , Test.ok "apply non-function" (Either.isLeft (typeOf (EApp (EInt 42) (EInt 1))))
    , Test.ok "nested let" (typeMatches TInt (typeOf (ELet "f" (ELam "x" (EVar "x")) (ELet "g" (ELam "y" (EApp (EVar "f") (EVar "y"))) (EApp (EVar "g") (EInt 42))))))
    , Test.ok "compose pattern" (typeMatches (TFun (TVar "a") (TVar "a")) (typeOf (ELam "f" (ELam "g" (ELam "x" (EApp (EVar "f") (EApp (EVar "g") (EVar "x"))))))))
    , Test.ok "S combinator" (Either.isRight (typeOf (ELam "f" (ELam "g" (ELam "x" (EApp (EApp (EVar "f") (EVar "x")) (EApp (EVar "g") (EVar "x"))))))))
    , Test.ok "K combinator" (Either.isRight (typeOf (ELam "x" (ELam "y" (EVar "x")))))
    , Test.ok "I combinator" (Either.isRight (typeOf (ELam "x" (EVar "x"))))
    , Test.ok "shadow" (typeMatches TInt (typeOf (ELet "x" (EBool true) (ELet "x" (EInt 42) (EVar "x")))))
    ]

-- Literals
-- Variables
-- Lambda
-- Application
-- Let polymorphism
-- If expressions
-- Type errors
-- Complex expressions
-- SKI combinators
-- Shadowing
let main =
  args -> Test.run tests
