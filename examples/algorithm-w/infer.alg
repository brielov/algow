-- =============================================================================
-- Algorithm W - Type Inference
-- =============================================================================
-- Implementation of Hindley-Milner type inference (Algorithm W).

-- =============================================================================
-- Type Definitions (inline for self-containment)
-- =============================================================================

type Type
  = TVar string
  | TCon string
  | TFun Type Type

type Scheme = Forall (List string) Type

type Expr
  = EVar string
  | ELit Lit
  | EAbs string Expr
  | EApp Expr Expr
  | ELet string Expr Expr

type Lit = LInt int | LBool bool

type TypeEnv = TypeEnv (List (string, Scheme))
type Subst = Subst (List (string, Type))

-- Result type for inference
type InferResult a = InferErr string | InferOk a

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- String equality
let strEq = String.eq

-- List membership
let rec member = x xs ->
  match xs
    when Nil -> false
    when Cons y rest -> if strEq x y then true else member x rest
  end

-- Remove duplicates
let rec nub = xs ->
  match xs
    when Nil -> Nil
    when Cons x rest ->
      if member x rest
        then nub rest
        else Cons x (nub rest)
  end

-- Set difference (xs - ys)
let diff = xs ys -> List.filter (x -> member x ys == false) xs

-- =============================================================================
-- Type Environment Operations
-- =============================================================================

let envEmpty = TypeEnv Nil

let envExtend = name scheme env ->
  match env
    when TypeEnv bindings -> TypeEnv (Cons (name, scheme) bindings)
  end

let rec envLookup = name env ->
  match env
    when TypeEnv Nil -> Nothing
    when TypeEnv (Cons (n, s) rest) ->
      if strEq name n
        then Just s
        else envLookup name (TypeEnv rest)
  end

-- =============================================================================
-- Substitution Operations
-- =============================================================================

let substEmpty = Subst Nil
let substSingleton = var ty -> Subst (Cons (var, ty) Nil)

let rec substLookup = var subst ->
  match subst
    when Subst Nil -> Nothing
    when Subst (Cons (v, t) rest) ->
      if strEq var v
        then Just t
        else substLookup var (Subst rest)
  end

-- Apply substitution to a type
let rec applySubstType = subst ty ->
  match ty
    when TVar v ->
      match substLookup v subst
        when Nothing -> TVar v
        when Just t -> t
      end
    when TCon c -> TCon c
    when TFun t1 t2 -> TFun (applySubstType subst t1) (applySubstType subst t2)
  end

-- Apply substitution to a scheme
let applySubstScheme = subst scheme ->
  match scheme
    when Forall vars ty ->
      let filtered = List.foldl (acc var ->
        match acc
          when Subst bindings ->
            Subst (List.filter (p ->
              match p when (v, t) -> strEq v var == false end
            ) bindings)
        end
      ) subst vars in
      Forall vars (applySubstType filtered ty)
  end

-- Apply substitution to environment
let applySubstEnv = subst env ->
  match env
    when TypeEnv bindings ->
      TypeEnv (List.map (p ->
        match p when (name, scheme) -> (name, applySubstScheme subst scheme) end
      ) bindings)
  end

-- Compose substitutions: apply s1 first, then s2
let composeSubst = s2 s1 ->
  match (s1, s2)
    when (Subst bindings1, Subst bindings2) ->
      let applied = List.map (p ->
        match p when (v, t) -> (v, applySubstType s2 t) end
      ) bindings1 in
      Subst (List.append bindings2 applied)
  end

-- =============================================================================
-- Free Type Variables
-- =============================================================================

let rec freeVarsType = ty ->
  match ty
    when TVar v -> Cons v Nil
    when TCon c -> Nil
    when TFun t1 t2 -> List.append (freeVarsType t1) (freeVarsType t2)
  end

let freeVarsScheme = scheme ->
  match scheme
    when Forall vars ty -> diff (freeVarsType ty) vars
  end

let freeVarsEnv = env ->
  match env
    when TypeEnv bindings ->
      nub (List.concat (List.map (p ->
        match p when (name, scheme) -> freeVarsScheme scheme end
      ) bindings))
  end

-- =============================================================================
-- Occurs Check
-- =============================================================================

let rec occursIn = var ty ->
  match ty
    when TVar v -> strEq var v
    when TCon c -> false
    when TFun t1 t2 -> occursIn var t1 || occursIn var t2
  end

-- =============================================================================
-- Unification
-- =============================================================================

let rec unify = t1 t2 ->
  match (t1, t2)
    when (TVar v1, TVar v2) ->
      if strEq v1 v2
        then InferOk substEmpty
        else InferOk (substSingleton v1 t2)

    when (TVar v, t) ->
      if occursIn v t
        then InferErr "occurs check failed"
        else InferOk (substSingleton v t)

    when (t, TVar v) ->
      if occursIn v t
        then InferErr "occurs check failed"
        else InferOk (substSingleton v t)

    when (TCon c1, TCon c2) ->
      if strEq c1 c2
        then InferOk substEmpty
        else InferErr "type mismatch"

    when (TFun l1 r1, TFun l2 r2) ->
      match unify l1 l2
        when InferErr e -> InferErr e
        when InferOk s1 ->
          match unify (applySubstType s1 r1) (applySubstType s1 r2)
            when InferErr e -> InferErr e
            when InferOk s2 -> InferOk (composeSubst s2 s1)
          end
      end

    when _ -> InferErr "type mismatch"
  end

-- =============================================================================
-- Fresh Type Variables
-- =============================================================================

-- Counter state passed through inference
type InferState = InferState int

let freshVar = state ->
  match state
    when InferState n ->
      let name = String.concat "t" (Int.toString n) in
      (TVar name, InferState (n + 1))
  end

-- =============================================================================
-- Instantiation and Generalization
-- =============================================================================

-- Instantiate a scheme with fresh type variables
let instantiate = scheme state ->
  match scheme
    when Forall vars ty ->
      let rec go = vars subst state ->
        match vars
          when Nil -> (applySubstType subst ty, state)
          when Cons v rest ->
            let pair = freshVar state in
            match pair
              when (freshTy, newState) ->
                go rest (composeSubst (substSingleton v freshTy) subst) newState
            end
        end
      in go vars substEmpty state
  end

-- Generalize a type over free variables not in the environment
let generalize = env ty ->
  let envFree = freeVarsEnv env in
  let tyFree = freeVarsType ty in
  let vars = diff tyFree envFree in
  Forall (nub vars) ty

-- =============================================================================
-- Type Inference (Algorithm W)
-- =============================================================================

let rec infer = env expr state ->
  match expr
    when EVar x ->
      match envLookup x env
        when Nothing -> (InferErr (String.concat "unbound variable: " x), state)
        when Just scheme ->
          let pair = instantiate scheme state in
          match pair
            when (ty, newState) -> (InferOk (substEmpty, ty), newState)
          end
      end

    when ELit lit ->
      match lit
        when LInt n -> (InferOk (substEmpty, TCon "Int"), state)
        when LBool b -> (InferOk (substEmpty, TCon "Bool"), state)
      end

    when EAbs x body ->
      let pair = freshVar state in
      match pair
        when (paramTy, state1) ->
          let newEnv = envExtend x (Forall Nil paramTy) env in
          match infer newEnv body state1
            when (InferErr e, state2) -> (InferErr e, state2)
            when (InferOk (s, bodyTy), state2) ->
              let resultTy = TFun (applySubstType s paramTy) bodyTy in
              (InferOk (s, resultTy), state2)
          end
      end

    when EApp fun arg ->
      let pair = freshVar state in
      match pair
        when (resultTy, state1) ->
          match infer env fun state1
            when (InferErr e, state2) -> (InferErr e, state2)
            when (InferOk (s1, funTy), state2) ->
              match infer (applySubstEnv s1 env) arg state2
                when (InferErr e, state3) -> (InferErr e, state3)
                when (InferOk (s2, argTy), state3) ->
                  let expectedFunTy = TFun argTy (applySubstType s2 resultTy) in
                  match unify (applySubstType s2 funTy) expectedFunTy
                    when InferErr e -> (InferErr e, state3)
                    when InferOk s3 ->
                      let finalSubst = composeSubst s3 (composeSubst s2 s1) in
                      let finalTy = applySubstType s3 (applySubstType s2 resultTy) in
                      (InferOk (finalSubst, finalTy), state3)
                  end
              end
          end
      end

    when ELet x defn body ->
      match infer env defn state
        when (InferErr e, state1) -> (InferErr e, state1)
        when (InferOk (s1, defnTy), state1) ->
          let newEnv = applySubstEnv s1 env in
          let scheme = generalize newEnv defnTy in
          let extendedEnv = envExtend x scheme newEnv in
          match infer extendedEnv body state1
            when (InferErr e, state2) -> (InferErr e, state2)
            when (InferOk (s2, bodyTy), state2) ->
              (InferOk (composeSubst s2 s1, bodyTy), state2)
          end
      end
  end

-- =============================================================================
-- Top-level Inference
-- =============================================================================

let inferExpr = expr ->
  let initialEnv = envEmpty in
  let initialState = InferState 0 in
  match infer initialEnv expr initialState
    when (InferErr e, _) -> Left e
    when (InferOk (subst, ty), _) -> Right ty
  end

-- =============================================================================
-- Type Pretty Printing
-- =============================================================================

let rec typeToString = ty ->
  match ty
    when TVar v -> v
    when TCon c -> c
    when TFun t1 t2 ->
      let left = match t1
        when TFun _ _ -> String.concat "(" (String.concat (typeToString t1) ")")
        when _ -> typeToString t1
      end in
      String.concat left (String.concat " -> " (typeToString t2))
  end

-- =============================================================================
-- Example Expressions
-- =============================================================================

-- Identity function: \x -> x
let idExpr = EAbs "x" (EVar "x")

-- Constant function: \x -> \y -> x
let constExpr = EAbs "x" (EAbs "y" (EVar "x"))

-- Apply: \f -> \x -> f x
let applyExpr = EAbs "f" (EAbs "x" (EApp (EVar "f") (EVar "x")))

-- Compose: \f -> \g -> \x -> f (g x)
let composeExpr = EAbs "f" (EAbs "g" (EAbs "x" (EApp (EVar "f") (EApp (EVar "g") (EVar "x")))))

-- Let polymorphism: let id = \x -> x in (id 1, id true)
-- (Simplified since we don't have tuples - just apply id twice)
let letPolyExpr = ELet "id" (EAbs "x" (EVar "x")) (EApp (EVar "id") (ELit (LInt 42)))

-- Self application attempt (should fail or give complex type)
let selfAppExpr = EAbs "x" (EApp (EVar "x") (EVar "x"))

-- =============================================================================
-- Testing
-- =============================================================================

let testInfer = expr name ->
  match inferExpr expr
    when Left err ->
      IO.printLine (String.concat name (String.concat ": Error - " err))
    when Right ty ->
      IO.printLine (String.concat name (String.concat ": " (typeToString ty)))
  end

let runInferTests =
  let r1 = testInfer idExpr "id" in
  let r2 = testInfer constExpr "const" in
  let r3 = testInfer applyExpr "apply" in
  let r4 = testInfer composeExpr "compose" in
  let r5 = testInfer letPolyExpr "letPoly" in
  let r6 = testInfer selfAppExpr "selfApp" in
  0
