-- =============================================================================
-- TYPE INFERENCE
-- =============================================================================
--
-- Hindley-Milner type inference using Algorithm W.
--
-- Reference: SPEC.md Section 8
--            Damas & Milner, "Principal type-schemes for functional programs" (1982)
--
-- =============================================================================

module Infer

  use Common (..)
  use Resolve (
    Name, RExpr, RPattern, RCase, RDecl, RProgram,
    -- Expression constructors
    RVar, RLit, RApp, RAbs, RLet, RLetRec, RMatch, RCon, RTuple,
    RRecord, RRecordUpdate, RField,
    -- Pattern constructors
    RPWild, RPVar, RPLit, RPCon, RPTuple, RPRecord, RPAs, RPOr,
    -- Declaration constructors
    RDeclType, RDeclLet, RDeclLetRec, RDeclForeign, RDeclModule
  )

  -- =========================================================================
  -- TYPE REPRESENTATION
  -- =========================================================================
  --
  -- Types used during inference. Row polymorphism for records.
  --
  -- =========================================================================

  type Type
    = TVar string                          -- Type variable (α, β, ...)
    | TCon string                          -- Type constructor (int, string, ...)
    | TApp Type Type                       -- Type application (Maybe int)
    | TFun Type Type                       -- Function type (a -> b)
    | TTuple (List Type)                   -- Tuple type (int, string)
    | TRecord (List (string, Type)) (Maybe Type)  -- Record with optional row variable

  -- Polymorphic type scheme: ∀α₁...αₙ. τ
  type Scheme = Scheme (List string) Type

  -- =========================================================================
  -- TYPE CONSTRUCTORS (helpers)
  -- =========================================================================

  let tInt = TCon "int"
  let tFloat = TCon "float"
  let tString = TCon "string"
  let tChar = TCon "char"
  let tBool = TCon "boolean"
  let tUnit = TCon "Unit"

  let tList = t -> TApp (TCon "List") t
  let tMaybe = t -> TApp (TCon "Maybe") t

  -- =========================================================================
  -- SUBSTITUTION
  -- =========================================================================
  --
  -- A substitution maps type variables to types.
  -- Implemented as association list.
  --
  -- =========================================================================

  type Subst = List (string, Type)

  let emptySubst = Nil

  let singleSubst = var typ -> [(var, typ)]

  -- Look up a type variable in substitution
  let rec lookupSubst = var subst -> match subst
    when Nil -> Nothing
    when Cons (v, t) rest ->
      if v == var then Just t
      else lookupSubst var rest
  end

  -- Apply substitution to a type
  let rec applySubst = subst typ -> match typ
    when TVar x ->
      match lookupSubst x subst
        when Just t -> t
        when Nothing -> TVar x
      end
    when TCon c -> TCon c
    when TApp t1 t2 -> TApp (applySubst subst t1) (applySubst subst t2)
    when TFun t1 t2 -> TFun (applySubst subst t1) (applySubst subst t2)
    when TTuple ts -> TTuple (List.map (applySubst subst) ts)
    when TRecord fields row ->
      TRecord (List.map (applySubstField subst) fields) (Maybe.map (applySubst subst) row)
  end

  and applySubstField = subst pair -> match pair
    when (name, typ) -> (name, applySubst subst typ)
  end

  -- Apply substitution to a scheme
  let applySubstScheme = subst scheme -> match scheme
    when Scheme vars typ ->
      -- Don't substitute bound variables
      let subst2 = List.filter (notBound vars) subst in
      Scheme vars (applySubst subst2 typ)
  end

  and notBound = vars pair -> match pair
    when (v, _) -> not (List.elem v vars)
  end

  -- Compose two substitutions: (s1 ∘ s2)(t) = s1(s2(t))
  let composeSubst = s1 s2 ->
    let s2Applied = List.map (applyToSnd s1) s2 in
    List.concat s2Applied (List.filter (notInS2 s2) s1)

  and applyToSnd = s1 pair -> match pair
    when (v, t) -> (v, applySubst s1 t)
  end

  and notInS2 = s2 pair -> match pair
    when (v, _) -> match lookupSubst v s2
      when Nothing -> true
      when Just _ -> false
    end
  end

  -- =========================================================================
  -- FREE TYPE VARIABLES
  -- =========================================================================

  -- Free type variables in a type
  let rec ftv = typ -> match typ
    when TVar x -> [x]
    when TCon _ -> Nil
    when TApp t1 t2 -> List.union (ftv t1) (ftv t2)
    when TFun t1 t2 -> List.union (ftv t1) (ftv t2)
    when TTuple ts -> List.foldl (acc t -> List.union acc (ftv t)) Nil ts
    when TRecord fields row ->
      let fieldVars = List.foldl (acc pair -> match pair
        when (_, t) -> List.union acc (ftv t)
      end) Nil fields in
      match row
        when Nothing -> fieldVars
        when Just r -> List.union fieldVars (ftv r)
      end
  end

  -- Free type variables in a scheme
  let ftvScheme = scheme -> match scheme
    when Scheme vars typ -> List.filter (v -> not (List.elem v vars)) (ftv typ)
  end

  -- Free type variables in a type environment
  let ftvEnv = env ->
    List.foldl (acc pair -> match pair
      when (_, scheme) -> List.union acc (ftvScheme scheme)
    end) Nil env

  -- =========================================================================
  -- TYPE ENVIRONMENT
  -- =========================================================================
  --
  -- Maps Names to type schemes.
  --
  -- =========================================================================

  type TypeEnv = List (int, Scheme)  -- Use Name.id as key

  let emptyTypeEnv = Nil

  let lookupEnv = nameId env -> match env
    when Nil -> Nothing
    when Cons (id, scheme) rest ->
      if id == nameId then Just scheme
      else lookupEnv nameId rest
  end

  let extendEnv = nameId scheme env -> Cons (nameId, scheme) env

  let applySubstEnv = subst env ->
    List.map (pair -> match pair
      when (id, scheme) -> (id, applySubstScheme subst scheme)
    end) env

  -- =========================================================================
  -- INFERENCE STATE
  -- =========================================================================

  type InferState = {
    nextVar : int,
    subst : Subst,
    errors : List string,
    constructors : List string    -- Collected constructor names
  }

  let initInferState = { nextVar = 0, subst = emptySubst, errors = Nil, constructors = Nil }

  -- Generate a fresh type variable
  let freshTVar = state ->
    let name = String.concat "t" (Int.toString state.nextVar) in
    let newState = { state | nextVar = state.nextVar + 1 } in
    (TVar name, newState)

  -- Add an error
  let addInferError = msg state ->
    { state | errors = Cons msg state.errors }

  -- Add a constructor name
  let addConstructor = name state ->
    { state | constructors = Cons name state.constructors }

  -- Add multiple constructor names
  let addConstructors = names state ->
    { state | constructors = List.concat names state.constructors }

  -- Update substitution
  let extendSubst = s state ->
    { state | subst = composeSubst s state.subst }

  -- =========================================================================
  -- OCCURS CHECK
  -- =========================================================================
  --
  -- Check if a type variable occurs in a type (prevents infinite types).
  --
  -- =========================================================================

  let rec occurs = var typ -> match typ
    when TVar x -> x == var
    when TCon _ -> false
    when TApp t1 t2 -> occurs var t1 || occurs var t2
    when TFun t1 t2 -> occurs var t1 || occurs var t2
    when TTuple ts -> List.any (occurs var) ts
    when TRecord fields row ->
      let inFields = List.any (pair -> match pair
        when (_, t) -> occurs var t
      end) fields in
      let inRow = match row
        when Nothing -> false
        when Just r -> occurs var r
      end in
      inFields || inRow
  end

  -- =========================================================================
  -- UNIFICATION
  -- =========================================================================
  --
  -- Find a substitution that makes two types equal.
  --
  -- =========================================================================

  let rec unify = t1 t2 state -> match (t1, t2)
    -- Same type variable
    when (TVar x, TVar y) if x == y -> (emptySubst, state)

    -- Type variable: bind (with occurs check)
    when (TVar x, t) ->
      if occurs x t then
        let st = addInferError "infinite type" state in
        (emptySubst, st)
      else (singleSubst x t, state)

    when (t, TVar x) ->
      if occurs x t then
        let st = addInferError "infinite type" state in
        (emptySubst, st)
      else (singleSubst x t, state)

    -- Same type constructor
    when (TCon a, TCon b) ->
      if a == b then (emptySubst, state)
      else
        let st = addInferError (String.concat "type mismatch: " (String.concat a (String.concat " vs " b))) state in
        (emptySubst, st)

    -- Type application
    when (TApp a1 b1, TApp a2 b2) ->
      let (s1, st1) = unify a1 a2 state in
      let (s2, st2) = unify (applySubst s1 b1) (applySubst s1 b2) st1 in
      (composeSubst s2 s1, st2)

    -- Function types
    when (TFun a1 b1, TFun a2 b2) ->
      let (s1, st1) = unify a1 a2 state in
      let (s2, st2) = unify (applySubst s1 b1) (applySubst s1 b2) st1 in
      (composeSubst s2 s1, st2)

    -- Tuple types
    when (TTuple ts1, TTuple ts2) ->
      unifyMany ts1 ts2 state

    -- Record types (simplified - no row polymorphism for now)
    when (TRecord fs1 _, TRecord fs2 _) ->
      unifyRecordFields fs1 fs2 state

    -- Mismatch
    when _ ->
      let st = addInferError "type mismatch" state in
      (emptySubst, st)
  end

  -- Unify lists of types
  and unifyMany = ts1 ts2 state -> match (ts1, ts2)
    when (Nil, Nil) -> (emptySubst, state)
    when (Cons t1 rest1, Cons t2 rest2) ->
      let (s1, st1) = unify t1 t2 state in
      let (s2, st2) = unifyMany (List.map (applySubst s1) rest1) (List.map (applySubst s1) rest2) st1 in
      (composeSubst s2 s1, st2)
    when _ ->
      let st = addInferError "tuple length mismatch" state in
      (emptySubst, st)
  end

  -- Unify record fields (simplified)
  and unifyRecordFields = fs1 fs2 state ->
    -- Sort fields by name and unify pairwise
    let sorted1 = List.sortBy fst fs1 in
    let sorted2 = List.sortBy fst fs2 in
    unifyFieldPairs sorted1 sorted2 state

  and unifyFieldPairs = fs1 fs2 state -> match (fs1, fs2)
    when (Nil, Nil) -> (emptySubst, state)
    when (Cons (n1, t1) rest1, Cons (n2, t2) rest2) ->
      if n1 == n2 then
        let (s1, st1) = unify t1 t2 state in
        let (s2, st2) = unifyFieldPairs (List.map (applySubstField s1) rest1) (List.map (applySubstField s1) rest2) st1 in
        (composeSubst s2 s1, st2)
      else
        let msg = String.concat "field mismatch: " (String.concat n1 (String.concat " vs " n2)) in
        let st = addInferError msg state in
        (emptySubst, st)
    when _ ->
      let st = addInferError "record field count mismatch" state in
      (emptySubst, st)
  end

  -- =========================================================================
  -- GENERALIZATION & INSTANTIATION
  -- =========================================================================

  -- Generalize: create a polymorphic scheme from a type
  -- Quantify over free variables not in the environment
  let generalize = env typ ->
    let envVars = ftvEnv env in
    let typeVars = ftv typ in
    let freeVars = List.filter (v -> not (List.elem v envVars)) typeVars in
    Scheme freeVars typ

  -- Instantiate: replace quantified variables with fresh ones
  let instantiate = scheme state -> match scheme
    when Scheme vars typ ->
      let (subst, st1) = freshSubstForVars vars state in
      (applySubst subst typ, st1)
  end

  and freshSubstForVars = vars state -> match vars
    when Nil -> (emptySubst, state)
    when Cons v rest ->
      let (tv, st1) = freshTVar state in
      let (restSubst, st2) = freshSubstForVars rest st1 in
      (Cons (v, tv) restSubst, st2)
  end

  -- =========================================================================
  -- ALGORITHM W - EXPRESSION INFERENCE
  -- =========================================================================
  --
  -- W : TypeEnv -> RExpr -> (Subst, Type)
  --
  -- =========================================================================

  let rec infer = env expr state -> match expr
    -- Variable: look up and instantiate
    when RVar name ->
      match lookupEnv name.id env
        when Just scheme ->
          let (typ, st1) = instantiate scheme state in
          (typ, st1)
        when Nothing ->
          let st1 = addInferError (String.concat "unbound variable: " name.original) state in
          let (tv, st2) = freshTVar st1 in
          (tv, st2)
      end

    -- Literal
    when RLit lit -> (inferLit lit, state)

    -- Application: f x
    when RApp e1 e2 ->
      let (t1, st1) = infer env e1 state in
      let (t2, st2) = infer (applySubstEnv st1.subst env) e2 st1 in
      let (tv, st3) = freshTVar st2 in
      let (s, st4) = unify (applySubst st3.subst t1) (TFun t2 tv) st3 in
      let st5 = extendSubst s st4 in
      (applySubst s tv, st5)

    -- Lambda: λx. e
    when RAbs name body ->
      let (tv, st1) = freshTVar state in
      let env2 = extendEnv name.id (Scheme Nil tv) env in
      let (t, st2) = infer env2 body st1 in
      (TFun (applySubst st2.subst tv) t, st2)

    -- Let: let x = e1 in e2
    when RLet name e1 e2 ->
      let (t1, st1) = infer env e1 state in
      let env2 = applySubstEnv st1.subst env in
      let scheme = generalize env2 t1 in
      let env3 = extendEnv name.id scheme env2 in
      let (t2, st2) = infer env3 e2 st1 in
      (t2, st2)

    -- Let rec: let rec bindings in body
    when RLetRec bindings body ->
      let (env2, st1) = inferRecBindings env bindings state in
      let (t, st2) = infer env2 body st1 in
      (t, st2)

    -- Match
    when RMatch scrutinee cases ->
      let (tScrutinee, st1) = infer env scrutinee state in
      let (tResult, st2) = freshTVar st1 in
      let (st3) = inferCases env tScrutinee tResult cases st2 in
      (applySubst st3.subst tResult, st3)

    -- Constructor
    when RCon name ->
      -- Look up constructor type (should be in env or prelude)
      match lookupEnv 0 env  -- Placeholder: constructors need special handling
        when _ ->
          let (tv, st1) = freshTVar state in
          (tv, st1)
      end

    -- Tuple
    when RTuple elems ->
      let (ts, st1) = inferExprs env elems state in
      (TTuple ts, st1)

    -- Record
    when RRecord fields ->
      let (fieldTypes, st1) = inferFields env fields state in
      (TRecord fieldTypes Nothing, st1)

    -- Record update
    when RRecordUpdate base updates ->
      let (tBase, st1) = infer env base state in
      let (updateTypes, st2) = inferFields env updates st1 in
      -- Base must be a record containing all updated fields
      let (tv, st3) = freshTVar st2 in
      let expectedRecord = TRecord updateTypes (Just tv) in
      let (s, st4) = unify tBase expectedRecord st3 in
      let st5 = extendSubst s st4 in
      (applySubst s tBase, st5)

    -- Field access
    when RField e field ->
      let (tRecord, st1) = infer env e state in
      let (tField, st2) = freshTVar st1 in
      let (tRow, st3) = freshTVar st2 in
      let expectedRecord = TRecord [(field, tField)] (Just tRow) in
      let (s, st4) = unify tRecord expectedRecord st3 in
      let st5 = extendSubst s st4 in
      (applySubst s tField, st5)
  end

  -- Infer type of literal
  and inferLit = lit -> match lit
    when LInt _ -> tInt
    when LFloat _ -> tFloat
    when LString _ -> tString
    when LChar _ -> tChar
    when LBool _ -> tBool
  end

  -- Infer types of multiple expressions
  and inferExprs = env exprs state -> match exprs
    when Nil -> (Nil, state)
    when Cons e rest ->
      let (t, st1) = infer env e state in
      let (ts, st2) = inferExprs (applySubstEnv st1.subst env) rest st1 in
      (Cons t ts, st2)
  end

  -- Infer types of record fields
  and inferFields = env fields state -> match fields
    when Nil -> (Nil, state)
    when Cons (name, e) rest ->
      let (t, st1) = infer env e state in
      let (ts, st2) = inferFields (applySubstEnv st1.subst env) rest st1 in
      (Cons (name, t) ts, st2)
  end

  -- Infer recursive bindings
  and inferRecBindings = env bindings state ->
    -- First, assign fresh type variables to all bindings
    let (env2, tvars, st1) = assignFreshVars env bindings state in
    -- Then infer each binding's body and unify with its type variable
    let (st2) = inferAndUnifyRec env2 bindings tvars st1 in
    -- Generalize all bindings
    let env3 = generalizeRec env2 bindings tvars st2 in
    (env3, st2)

  and assignFreshVars = env bindings state -> match bindings
    when Nil -> (env, Nil, state)
    when Cons (name, _) rest ->
      let (tv, st1) = freshTVar state in
      let env2 = extendEnv name.id (Scheme Nil tv) env in
      let (env3, tvs, st2) = assignFreshVars env2 rest st1 in
      (env3, Cons tv tvs, st2)
  end

  and inferAndUnifyRec = env bindings tvars state -> match (bindings, tvars)
    when (Nil, _) -> state
    when (_, Nil) -> state
    when (Cons (_, e) restB, Cons tv restT) ->
      let (t, st1) = infer env e state in
      let (s, st2) = unify tv t st1 in
      let st3 = extendSubst s st2 in
      inferAndUnifyRec (applySubstEnv s env) restB restT st3
  end

  and generalizeRec = env bindings tvars state -> match (bindings, tvars)
    when (Nil, _) -> env
    when (_, Nil) -> env
    when (Cons (name, _) restB, Cons tv restT) ->
      let finalType = applySubst state.subst tv in
      let scheme = generalize env finalType in
      let env2 = extendEnv name.id scheme env in
      generalizeRec env2 restB restT state
  end

  -- =========================================================================
  -- CASE INFERENCE
  -- =========================================================================

  and inferCases = env tScrutinee tResult cases state -> match cases
    when Nil -> state
    when Cons c rest ->
      let (st1) = inferCase env tScrutinee tResult c state in
      inferCases (applySubstEnv st1.subst env) tScrutinee tResult rest st1
  end

  and inferCase = env tScrutinee tResult c state ->
    -- Infer pattern and get bindings
    let (tPat, bindings, st1) = inferPattern c.pattern state in
    -- Unify pattern type with scrutinee
    let (s1, st2) = unify tPat (applySubst st1.subst tScrutinee) st1 in
    let st3 = extendSubst s1 st2 in
    -- Extend environment with pattern bindings
    let env2 = List.foldl (e pair -> match pair
      when (nameId, typ) -> extendEnv nameId (Scheme Nil typ) e
    end) (applySubstEnv s1 env) bindings in
    -- Infer guard if present
    let st4 = match c.guard
      when Nothing -> st3
      when Just g ->
        let (tGuard, st) = infer env2 g st3 in
        let (s, st2) = unify tGuard tBool st in
        extendSubst s st2
    end in
    -- Infer body
    let (tBody, st5) = infer (applySubstEnv st4.subst env2) c.body st4 in
    -- Unify body with result type
    let (s2, st6) = unify tBody (applySubst st5.subst tResult) st5 in
    extendSubst s2 st6

  -- =========================================================================
  -- PATTERN INFERENCE
  -- =========================================================================
  --
  -- Returns (patternType, bindings, state)
  -- where bindings is List (nameId, Type)
  --
  -- =========================================================================

  and inferPattern = pat state -> match pat
    when RPWild ->
      let (tv, st1) = freshTVar state in
      (tv, Nil, st1)

    when RPVar name ->
      let (tv, st1) = freshTVar state in
      (tv, [(name.id, tv)], st1)

    when RPLit lit ->
      (inferLit lit, Nil, state)

    when RPCon conName pats ->
      -- Constructor patterns need type info from registry
      -- For now, infer component types and return fresh var
      let (tPats, bindings, st1) = inferPatterns pats state in
      let (tv, st2) = freshTVar st1 in
      (tv, bindings, st2)

    when RPTuple pats ->
      let (tPats, bindings, st1) = inferPatterns pats state in
      (TTuple tPats, bindings, st1)

    when RPRecord fields ->
      let (fieldTypes, bindings, st1) = inferPatternFields fields state in
      (TRecord fieldTypes Nothing, bindings, st1)

    when RPAs name pat ->
      let (tPat, bindings, st1) = inferPattern pat state in
      (tPat, Cons (name.id, tPat) bindings, st1)

    when RPOr p1 p2 ->
      let (t1, bindings1, st1) = inferPattern p1 state in
      let (t2, _, st2) = inferPattern p2 st1 in
      let (s, st3) = unify t1 t2 st2 in
      let st4 = extendSubst s st3 in
      (applySubst s t1, bindings1, st4)
  end

  and inferPatterns = pats state -> match pats
    when Nil -> (Nil, Nil, state)
    when Cons p rest ->
      let (t, bindings1, st1) = inferPattern p state in
      let (ts, bindings2, st2) = inferPatterns rest st1 in
      (Cons t ts, List.concat bindings1 bindings2, st2)
  end

  and inferPatternFields = fields state -> match fields
    when Nil -> (Nil, Nil, state)
    when Cons (name, p) rest ->
      let (t, bindings1, st1) = inferPattern p state in
      let (ts, bindings2, st2) = inferPatternFields rest st1 in
      (Cons (name, t) ts, List.concat bindings1 bindings2, st2)
  end

  -- =========================================================================
  -- DECLARATION INFERENCE
  -- =========================================================================

  let rec inferDecl = env decl state -> match decl
    when RDeclType _ _ conDecls ->
      -- Type declarations: collect constructor names
      let conNames = List.map (cd -> cd.name) conDecls in
      let st1 = addConstructors conNames state in
      (env, st1)

    when RDeclLet name expr ->
      let (t, st1) = infer env expr state in
      let scheme = generalize (applySubstEnv st1.subst env) t in
      let env2 = extendEnv name.id scheme env in
      (env2, st1)

    when RDeclLetRec bindings ->
      let (env2, st1) = inferRecBindings env bindings state in
      (env2, st1)

    when RDeclForeign name typ ->
      -- Foreign declarations have explicit types
      let scheme = Scheme Nil (coreTypeToType typ) in
      let env2 = extendEnv name.id scheme env in
      (env2, state)

    when RDeclModule _ decls ->
      inferDecls env decls state
  end

  and inferDecls = env decls state -> match decls
    when Nil -> (env, state)
    when Cons d rest ->
      let (env2, st1) = inferDecl env d state in
      inferDecls env2 rest st1
  end

  -- Convert CoreType to internal Type
  and coreTypeToType = ct -> match ct
    when Desugar.CTVar x -> TVar x
    when Desugar.CTCon c -> TCon c
    when Desugar.CTApp t1 t2 -> TApp (coreTypeToType t1) (coreTypeToType t2)
    when Desugar.CTFun t1 t2 -> TFun (coreTypeToType t1) (coreTypeToType t2)
    when Desugar.CTTuple ts -> TTuple (List.map coreTypeToType ts)
    when Desugar.CTRecord fs -> TRecord (List.map coreTypeField fs) Nothing
  end

  and coreTypeField = pair -> match pair
    when (name, t) -> (name, coreTypeToType t)
  end

  -- =========================================================================
  -- PROGRAM INFERENCE
  -- =========================================================================

  -- Result type for inference
  type InferResult = {
    exprType : Maybe Type,
    errors : List string,
    constructors : List string
  }

  let inferProgram = program initialEnv ->
    let state = initInferState in
    let (env, st1) = inferDecls initialEnv program.decls state in
    let (exprType, st2) = match program.expr
      when Nothing -> (Nothing, st1)
      when Just e ->
        let (t, st) = infer env e st1 in
        (Just t, st)
    end in
    {
      exprType = exprType,
      errors = st2.errors,
      constructors = st2.constructors
    }

end
