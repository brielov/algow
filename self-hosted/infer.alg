-- =============================================================================
-- Infer Module
-- Type inference using Algorithm W (Damas-Milner)
-- Reference: SPEC.md Section 8, Damas & Milner (1982)
-- =============================================================================

module Infer
  use Types (..)

  -- ===========================================================================
  -- Type Environment
  -- ===========================================================================

  -- Type environment: maps Names to type Schemes
  type TypeEnv = TypeEnv (List (int, Scheme))

  let emptyTypeEnv = TypeEnv Nil

  let lookupTypeEnv name env =
    match env
      when TypeEnv entries ->
        match name
          when Name id _ ->
            let rec find es = match es
              when Nil -> Nothing
              when Cons (i, scheme) rest ->
                if i == id then Just scheme
                else find rest
            end
            in find entries
        end
    end

  let extendTypeEnv name scheme env =
    match (name, env)
      when (Name id _, TypeEnv entries) ->
        TypeEnv (Cons (id, scheme) entries)
    end

  -- ===========================================================================
  -- Substitution
  -- ===========================================================================

  -- Substitution: maps type variable names to types
  type Subst = Subst (List (string, Type))

  let emptySubst = Subst Nil

  let singletonSubst var ty = Subst [(var, ty)]

  let lookupSubst var subst =
    match subst
      when Subst entries ->
        let rec find es = match es
          when Nil -> Nothing
          when Cons (v, ty) rest ->
            if v == var then Just ty
            else find rest
        end
        in find entries
    end

  -- Apply substitution to a type
  let rec applySubst subst ty = match ty
    when TVar x ->
      match lookupSubst x subst
        when Just t -> applySubst subst t
        when Nothing -> ty
      end
    when TCon c -> TCon c
    when TApp t1 t2 -> TApp (applySubst subst t1) (applySubst subst t2)
    when TFun t1 t2 -> TFun (applySubst subst t1) (applySubst subst t2)
    when TTuple ts -> TTuple (List.map (applySubst subst) ts)
    when TRecord fields ->
      TRecord (List.map (f -> match f when TField n t -> TField n (applySubst subst t) end) fields)
  end

  let filterSubst subst vars =
    match subst
      when Subst entries ->
        let rec filter es = match es
          when Nil -> Nil
          when Cons (v, ty) rest ->
            if List.any (x -> x == v) vars
            then filter rest
            else Cons (v, ty) (filter rest)
        end
        in Subst (filter entries)
    end

  -- Apply substitution to a scheme
  let applySubstScheme subst scheme =
    match scheme
      when Forall vars ty ->
        -- Remove bound variables from substitution
        let filteredSubst = filterSubst subst vars in
        Forall vars (applySubst filteredSubst ty)
    end

  -- Apply substitution to environment
  let applySubstEnv subst env =
    match env
      when TypeEnv entries ->
        TypeEnv (List.map (e -> match e when (id, scheme) -> (id, applySubstScheme subst scheme) end) entries)
    end

  -- Compose two substitutions: s1 ∘ s2
  let composeSubst s1 s2 =
    match (s1, s2)
      when (Subst e1, Subst e2) ->
        let e1Applied = List.map (e -> match e when (v, ty) -> (v, applySubst s2 ty) end) e1 in
        let combined = List.foldl (acc e ->
          match e
            when (v, ty) ->
              if List.any (x -> match x when (v2, _) -> v2 == v end) acc
              then acc
              else Cons e acc
          end
        ) e1Applied e2 in
        Subst combined
    end

  -- ===========================================================================
  -- Free Type Variables
  -- ===========================================================================

  let rec freeVarsType ty = match ty
    when TVar x -> Set.singleton x
    when TCon _ -> Set.empty
    when TApp t1 t2 -> Set.union (freeVarsType t1) (freeVarsType t2)
    when TFun t1 t2 -> Set.union (freeVarsType t1) (freeVarsType t2)
    when TTuple ts -> List.foldl (acc t -> Set.union acc (freeVarsType t)) Set.empty ts
    when TRecord fields ->
      List.foldl (acc f -> match f when TField _ t -> Set.union acc (freeVarsType t) end) Set.empty fields
  end

  let freeVarsScheme scheme =
    match scheme
      when Forall vars ty ->
        Set.difference (freeVarsType ty) (Set.fromList vars)
    end

  let freeVarsEnv env =
    match env
      when TypeEnv entries ->
        List.foldl (acc e -> match e when (_, scheme) -> Set.union acc (freeVarsScheme scheme) end) Set.empty entries
    end

  -- ===========================================================================
  -- Generalization and Instantiation
  -- ===========================================================================

  let generalize env ty =
    let envFree = freeVarsEnv env in
    let tyFree = freeVarsType ty in
    let vars = Set.toList (Set.difference tyFree envFree) in
    Forall vars ty

  -- Fresh type variable generation
  let freshVar n = "t" + Int.toString n

  let instantiate counter scheme =
    match scheme
      when Forall vars ty ->
        let rec mkSubst n vlist = match vlist
          when Nil -> (n, emptySubst)
          when Cons v rest ->
            let (n2, s) = mkSubst (n + 1) rest in
            (n2, composeSubst (singletonSubst v (TVar (freshVar n))) s)
        end in
        let (newCounter, subst) = mkSubst counter vars in
        (newCounter, applySubst subst ty)
    end

  -- ===========================================================================
  -- Unification
  -- ===========================================================================

  -- Occurs check: does variable v occur in type t?
  let rec occursIn v ty = match ty
    when TVar x -> v == x
    when TCon _ -> false
    when TApp t1 t2 -> occursIn v t1 || occursIn v t2
    when TFun t1 t2 -> occursIn v t1 || occursIn v t2
    when TTuple ts -> List.any (occursIn v) ts
    when TRecord fields ->
      List.any (f -> match f when TField _ t -> occursIn v t end) fields
  end

  -- Unify two types
  let rec unify t1 t2 = match (t1, t2)
    -- Same type constructor
    when (TCon a, TCon b) ->
      if a == b then Right emptySubst
      else Left ("Type mismatch: " + a + " vs " + b)

    -- Type variable on left
    when (TVar a, TVar b) ->
      if a == b then Right emptySubst
      else Right (singletonSubst a (TVar b))

    when (TVar a, t) ->
      if occursIn a t then Left "Infinite type"
      else Right (singletonSubst a t)

    when (t, TVar a) ->
      if occursIn a t then Left "Infinite type"
      else Right (singletonSubst a t)

    -- Function types
    when (TFun a1 b1, TFun a2 b2) ->
      match unify a1 a2
        when Left e -> Left e
        when Right s1 ->
          match unify (applySubst s1 b1) (applySubst s1 b2)
            when Left e -> Left e
            when Right s2 -> Right (composeSubst s1 s2)
          end
      end

    -- Type application
    when (TApp a1 b1, TApp a2 b2) ->
      match unify a1 a2
        when Left e -> Left e
        when Right s1 ->
          match unify (applySubst s1 b1) (applySubst s1 b2)
            when Left e -> Left e
            when Right s2 -> Right (composeSubst s1 s2)
          end
      end

    -- Tuples
    when (TTuple ts1, TTuple ts2) ->
      unifyMany ts1 ts2

    -- Records (simplified: same fields in same order)
    when (TRecord fs1, TRecord fs2) ->
      unifyRecordFields fs1 fs2

    when _ -> Left "Type mismatch"
  end

  and unifyMany ts1 ts2 = match (ts1, ts2)
    when (Nil, Nil) -> Right emptySubst
    when (Cons t1 rest1, Cons t2 rest2) ->
      match unify t1 t2
        when Left e -> Left e
        when Right s1 ->
          match unifyMany (List.map (applySubst s1) rest1) (List.map (applySubst s1) rest2)
            when Left e -> Left e
            when Right s2 -> Right (composeSubst s1 s2)
          end
      end
    when _ -> Left "Tuple arity mismatch"
  end

  and unifyRecordFields fs1 fs2 = match (fs1, fs2)
    when (Nil, Nil) -> Right emptySubst
    when (Cons (TField n1 t1) rest1, Cons (TField n2 t2) rest2) ->
      if n1 == n2 then
        match unify t1 t2
          when Left e -> Left e
          when Right s1 ->
            let applyToField f = match f when TField n t -> TField n (applySubst s1 t) end in
            match unifyRecordFields (List.map applyToField rest1) (List.map applyToField rest2)
              when Left e -> Left e
              when Right s2 -> Right (composeSubst s1 s2)
            end
        end
      else Left ("Record field mismatch: " + n1 + " vs " + n2)
    when _ -> Left "Record field count mismatch"
  end

  -- ===========================================================================
  -- Algorithm W
  -- W : TypeEnv -> Core.Expr -> Either Error (Subst, Type)
  -- ===========================================================================

  let rec infer counter env expr = match expr
    -- Literal
    when CLit l -> Right (counter, emptySubst, typeOfLit l)

    -- Variable
    when CVar name ->
      match lookupTypeEnv name env
        when Nothing ->
          match name when Name _ orig -> Left ("Unbound variable: " + orig) end
        when Just scheme ->
          let (n, ty) = instantiate counter scheme in
          Right (n, emptySubst, ty)
      end

    -- Lambda
    when CAbs param body ->
      let tv = TVar (freshVar counter) in
      let env2 = extendTypeEnv param (Forall Nil tv) env in
      match infer (counter + 1) env2 body
        when Left e -> Left e
        when Right (n, s, bodyTy) ->
          Right (n, s, TFun (applySubst s tv) bodyTy)
      end

    -- Application
    when CApp func arg ->
      match infer counter env func
        when Left e -> Left e
        when Right (n1, s1, funcTy) ->
          match infer n1 (applySubstEnv s1 env) arg
            when Left e -> Left e
            when Right (n2, s2, argTy) ->
              let tv = TVar (freshVar n2) in
              match unify (applySubst s2 funcTy) (TFun argTy tv)
                when Left e -> Left e
                when Right s3 ->
                  Right (n2 + 1, composeSubst (composeSubst s1 s2) s3, applySubst s3 tv)
              end
          end
      end

    -- Let
    when CLet name value body ->
      match infer counter env value
        when Left e -> Left e
        when Right (n1, s1, valueTy) ->
          let env2 = applySubstEnv s1 env in
          let scheme = generalize env2 valueTy in
          match infer n1 (extendTypeEnv name scheme env2) body
            when Left e -> Left e
            when Right (n2, s2, bodyTy) ->
              Right (n2, composeSubst s1 s2, bodyTy)
          end
      end

    -- Let rec (simplified: single binding)
    when CLetRec bindings body ->
      inferLetRec counter env bindings body

    -- Match
    when CMatch scrutinee cases ->
      match infer counter env scrutinee
        when Left e -> Left e
        when Right (n1, s1, scrutTy) ->
          let env2 = applySubstEnv s1 env in
          inferCases n1 s1 env2 scrutTy cases
      end

    -- Constructor
    when CCon name ->
      -- Constructors are looked up from constructor environment
      -- For now, return a fresh type variable (should be looked up)
      let tv = TVar (freshVar counter) in
      Right (counter + 1, emptySubst, tv)

    -- Tuple
    when CTuple exprs ->
      inferTuple counter env exprs

    -- Record
    when CRecord fields ->
      inferRecord counter env fields

    -- Record update
    when CRecordUpdate base updates ->
      match infer counter env base
        when Left e -> Left e
        when Right (n1, s1, baseTy) ->
          -- Simplified: base must be a record, updates must match
          inferRecordUpdate n1 s1 env baseTy updates
      end

    -- Field access
    when CFieldAccess e field ->
      match infer counter env e
        when Left e -> Left e
        when Right (n1, s1, ty) ->
          -- Simplified: ty must be a record with the field
          let tv = TVar (freshVar n1) in
          Right (n1 + 1, s1, tv)
      end

    -- Foreign
    when CForeign _ _ ->
      -- Foreign functions need their type from declarations
      let tv = TVar (freshVar counter) in
      Right (counter + 1, emptySubst, tv)
  end

  -- ===========================================================================
  -- Helper Inference Functions
  -- ===========================================================================

  and typeOfLit l = match l
    when LInt _ -> TCon "int"
    when LFloat _ -> TCon "float"
    when LString _ -> TCon "string"
    when LChar _ -> TCon "char"
    when LBool _ -> TCon "bool"
  end

  and inferLetRec counter env bindings body =
    -- Create fresh type variables for all bindings
    let (env2, counter2, tvs) = createRecBindingTypes env counter bindings in
    -- Infer types for all binding bodies
    match inferRecBindings counter2 env2 bindings tvs
      when Left e -> Left e
      when Right (n1, s1) ->
        -- Generalize and add to environment
        let env3 = applySubstEnv s1 env2 in
        let env4 = addGeneralizedBindings env3 bindings tvs s1 in
        -- Infer body
        match infer n1 env4 body
          when Left e -> Left e
          when Right (n2, s2, bodyTy) ->
            Right (n2, composeSubst s1 s2, bodyTy)
        end
    end

  and createRecBindingTypes env counter bindings =
    match bindings
      when Nil -> (env, counter, Nil)
      when Cons (CBinding name _) rest ->
        let tv = TVar (freshVar counter) in
        let env2 = extendTypeEnv name (Forall Nil tv) env in
        let (env3, n, tvs) = createRecBindingTypes env2 (counter + 1) rest in
        (env3, n, Cons tv tvs)
    end

  and inferRecBindings counter env bindings tvs =
    match (bindings, tvs)
      when (Nil, Nil) -> Right (counter, emptySubst)
      when (Cons (CBinding _ expr) restB, Cons tv restT) ->
        match infer counter env expr
          when Left e -> Left e
          when Right (n1, s1, ty) ->
            match unify (applySubst s1 tv) ty
              when Left e -> Left e
              when Right s2 ->
                let s3 = composeSubst s1 s2 in
                let env2 = applySubstEnv s3 env in
                match inferRecBindings n1 env2 restB (List.map (applySubst s3) restT)
                  when Left e -> Left e
                  when Right (n2, s4) -> Right (n2, composeSubst s3 s4)
                end
            end
        end
      when _ -> Left "Internal error in inferRecBindings"
    end

  and addGeneralizedBindings env bindings tvs subst =
    match (bindings, tvs)
      when (Nil, Nil) -> env
      when (Cons (CBinding name _) restB, Cons tv restT) ->
        let ty = applySubst subst tv in
        let scheme = generalize env ty in
        let env2 = extendTypeEnv name scheme env in
        addGeneralizedBindings env2 restB restT subst
      when _ -> env
    end

  and inferCases counter subst env scrutTy cases =
    match cases
      when Nil -> Left "Empty match"
      when Cons (CCase pat guard body) Nil ->
        inferCase counter subst env scrutTy pat guard body
      when Cons (CCase pat guard body) rest ->
        match inferCase counter subst env scrutTy pat guard body
          when Left e -> Left e
          when Right (n1, s1, ty1) ->
            match inferCases n1 s1 (applySubstEnv s1 env) (applySubst s1 scrutTy) rest
              when Left e -> Left e
              when Right (n2, s2, ty2) ->
                match unify (applySubst s2 ty1) ty2
                  when Left e -> Left e
                  when Right s3 ->
                    Right (n2, composeSubst (composeSubst s1 s2) s3, applySubst s3 ty2)
                end
            end
        end
    end

  and inferCase counter subst env scrutTy pat guard body =
    -- Infer pattern type and get bindings
    let (n1, s1, patTy, patEnv) = inferPattern counter env pat in
    -- Unify pattern type with scrutinee type
    match unify (applySubst s1 scrutTy) patTy
      when Left e -> Left e
      when Right s2 ->
        let s3 = composeSubst (composeSubst subst s1) s2 in
        let env2 = applySubstEnv s3 (mergeEnv env patEnv) in
        -- Check guard if present
        match guard
          when Nothing ->
            match infer n1 env2 body
              when Left e -> Left e
              when Right (n2, s4, bodyTy) ->
                Right (n2, composeSubst s3 s4, bodyTy)
            end
          when Just guardExpr ->
            match infer n1 env2 guardExpr
              when Left e -> Left e
              when Right (n2, s4, guardTy) ->
                match unify guardTy (TCon "bool")
                  when Left e -> Left e
                  when Right s5 ->
                    let env3 = applySubstEnv (composeSubst s4 s5) env2 in
                    match infer n2 env3 body
                      when Left e -> Left e
                      when Right (n3, s6, bodyTy) ->
                        Right (n3, composeSubst (composeSubst (composeSubst s3 s4) s5) s6, bodyTy)
                    end
                end
            end
        end
    end

  and mergeEnv env1 env2 =
    match env2
      when TypeEnv entries ->
        List.foldl (acc e -> match e when (id, scheme) -> extendTypeEnv (Name id "") scheme acc end) env1 entries
    end

  and inferPattern counter env pat = match pat
    when CPWild ->
      let tv = TVar (freshVar counter) in
      (counter + 1, emptySubst, tv, emptyTypeEnv)

    when CPVar name ->
      let tv = TVar (freshVar counter) in
      (counter + 1, emptySubst, tv, extendTypeEnv name (Forall Nil tv) emptyTypeEnv)

    when CPLit l ->
      (counter, emptySubst, typeOfLit l, emptyTypeEnv)

    when CPCon conName pats ->
      -- Simplified: return fresh type for constructor result
      let (n1, s1, patTys, patEnv) = inferPatterns counter env pats in
      let tv = TVar (freshVar n1) in
      (n1 + 1, s1, tv, patEnv)

    when CPTuple pats ->
      let (n1, s1, patTys, patEnv) = inferPatterns counter env pats in
      (n1, s1, TTuple patTys, patEnv)

    when CPRecord fields ->
      inferRecordPattern counter env fields

    when CPAs name pat ->
      let (n1, s1, patTy, patEnv) = inferPattern counter env pat in
      let patEnv2 = extendTypeEnv name (Forall Nil patTy) patEnv in
      (n1, s1, patTy, patEnv2)

    when CPOr p1 p2 ->
      -- Both patterns must have same type
      let (n1, s1, ty1, env1) = inferPattern counter env p1 in
      let (n2, s2, ty2, env2) = inferPattern n1 env p2 in
      match unify (applySubst s2 ty1) ty2
        when Left _ -> (n2, composeSubst s1 s2, ty2, mergeEnv env1 env2)
        when Right s3 -> (n2, composeSubst (composeSubst s1 s2) s3, applySubst s3 ty2, mergeEnv env1 env2)
      end
  end

  and inferPatterns counter env pats =
    match pats
      when Nil -> (counter, emptySubst, Nil, emptyTypeEnv)
      when Cons p rest ->
        let (n1, s1, ty, patEnv1) = inferPattern counter env p in
        let (n2, s2, tys, patEnv2) = inferPatterns n1 env rest in
        (n2, composeSubst s1 s2, Cons (applySubst s2 ty) tys, mergeEnv patEnv1 patEnv2)
    end

  and inferRecordPattern counter env fields =
    match fields
      when Nil -> (counter, emptySubst, TRecord Nil, emptyTypeEnv)
      when Cons (CPatternField name pat) rest ->
        let (n1, s1, ty, patEnv1) = inferPattern counter env pat in
        let (n2, s2, TRecord restFields, patEnv2) = inferRecordPattern n1 env rest in
        (n2, composeSubst s1 s2, TRecord (Cons (TField name (applySubst s2 ty)) restFields), mergeEnv patEnv1 patEnv2)
    end

  and inferTuple counter env exprs =
    match exprs
      when Nil -> Right (counter, emptySubst, TTuple Nil)
      when Cons e rest ->
        match infer counter env e
          when Left err -> Left err
          when Right (n1, s1, ty) ->
            match inferTuple n1 (applySubstEnv s1 env) rest
              when Left err -> Left err
              when Right (n2, s2, TTuple tys) ->
                Right (n2, composeSubst s1 s2, TTuple (Cons (applySubst s2 ty) tys))
              when Right (n2, s2, _) ->
                Right (n2, composeSubst s1 s2, TTuple [applySubst s2 ty])
            end
        end
    end

  and inferRecord counter env fields =
    match fields
      when Nil -> Right (counter, emptySubst, TRecord Nil)
      when Cons (CField name expr) rest ->
        match infer counter env expr
          when Left err -> Left err
          when Right (n1, s1, ty) ->
            match inferRecord n1 (applySubstEnv s1 env) rest
              when Left err -> Left err
              when Right (n2, s2, TRecord restFields) ->
                Right (n2, composeSubst s1 s2, TRecord (Cons (TField name (applySubst s2 ty)) restFields))
              when Right (n2, s2, _) ->
                Right (n2, composeSubst s1 s2, TRecord [TField name (applySubst s2 ty)])
            end
        end
    end

  and inferRecordUpdate counter subst env baseTy updates =
    -- Simplified: just return the base type
    Right (counter, subst, baseTy)

  -- ===========================================================================
  -- Entry Point
  -- ===========================================================================

  let inferType expr =
    match infer 0 emptyTypeEnv expr
      when Left e -> Left e
      when Right (_, s, ty) -> Right (applySubst s ty)
    end

  let inferExpr env expr =
    match infer 0 env expr
      when Left e -> Left e
      when Right (_, s, ty) -> Right (applySubst s ty)
    end

end
