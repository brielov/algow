-- =============================================================================
-- Resolve Module
-- Name resolution: assigns unique identifiers to all bindings
-- Reference: SPEC.md Section 7
-- =============================================================================

module Resolve
  use Types (..)

  -- ===========================================================================
  -- Environment and State
  -- ===========================================================================

  -- Resolution state: next unique ID
  type ResolveState = ResolveState int

  -- Result type for resolution
  type ResolveResult a
    = ResOk a ResolveState
    | ResErr string

  -- Environment: maps original names to resolved Names
  -- Using association list for simplicity
  type Env = Env (List (string, Name))

  let emptyEnv = Env Nil

  let lookupEnv name env =
    match env
      when Env entries ->
        let rec find es = match es
          when Nil -> Nothing
          when Cons (n, resolved) rest ->
            if n == name then Just resolved
            else find rest
        end
        in find entries
    end

  let extendEnv name resolved env =
    match env when Env entries -> Env (Cons (name, resolved) entries) end

  let initState = ResolveState 0

  let freshId state =
    match state
      when ResolveState n -> (n, ResolveState (n + 1))
    end

  let freshName original state =
    let (id, newState) = freshId state in
    (Name id original, newState)

  -- ===========================================================================
  -- Expression Resolution
  -- ===========================================================================

  let rec resolveExpr env state expr = match expr
    -- Variable: look up in environment
    when CVar (Name _ original) ->
      match lookupEnv original env
        when Nothing -> ResErr ("Unbound variable: " + original)
        when Just resolved -> ResOk (CVar resolved) state
      end

    -- Literal: pass through
    when CLit l -> ResOk (CLit l) state

    -- Application
    when CApp e1 e2 ->
      match resolveExpr env state e1
        when ResErr e -> ResErr e
        when ResOk c1 s1 ->
          match resolveExpr env s1 e2
            when ResErr e -> ResErr e
            when ResOk c2 s2 -> ResOk (CApp c1 c2) s2
          end
      end

    -- Lambda: bind parameter
    when CAbs (Name _ param) body ->
      let (resolvedParam, s1) = freshName param state in
      let env2 = extendEnv param resolvedParam env in
      match resolveExpr env2 s1 body
        when ResErr e -> ResErr e
        when ResOk cBody s2 -> ResOk (CAbs resolvedParam cBody) s2
      end

    -- Let: bind variable
    when CLet (Name _ name) value body ->
      match resolveExpr env state value
        when ResErr e -> ResErr e
        when ResOk cValue s1 ->
          let (resolvedName, s2) = freshName name s1 in
          let env2 = extendEnv name resolvedName env in
          match resolveExpr env2 s2 body
            when ResErr e -> ResErr e
            when ResOk cBody s3 -> ResOk (CLet resolvedName cValue cBody) s3
          end
      end

    -- Let rec: bind all names first, then resolve bodies
    when CLetRec bindings body ->
      -- First, create fresh names for all bindings
      let (env2, s1, names) = bindRecNames env state bindings in
      -- Then resolve all binding bodies with the extended environment
      match resolveBindingBodies env2 s1 bindings names
        when ResErr e -> ResErr e
        when ResOk cBindings s2 ->
          -- Finally resolve the body
          match resolveExpr env2 s2 body
            when ResErr e -> ResErr e
            when ResOk cBody s3 -> ResOk (CLetRec cBindings cBody) s3
          end
      end

    -- Match
    when CMatch scrutinee cases ->
      match resolveExpr env state scrutinee
        when ResErr e -> ResErr e
        when ResOk cScrutinee s1 ->
          match resolveCases env s1 cases
            when ResErr e -> ResErr e
            when ResOk cCases s2 -> ResOk (CMatch cScrutinee cCases) s2
          end
      end

    -- Constructor: pass through (constructors are global)
    when CCon c -> ResOk (CCon c) state

    -- Tuple
    when CTuple exprs ->
      match resolveExprs env state exprs
        when ResErr e -> ResErr e
        when ResOk cExprs s1 -> ResOk (CTuple cExprs) s1
      end

    -- Record
    when CRecord fields ->
      match resolveFields env state fields
        when ResErr e -> ResErr e
        when ResOk cFields s1 -> ResOk (CRecord cFields) s1
      end

    -- Record update
    when CRecordUpdate base fields ->
      match resolveExpr env state base
        when ResErr e -> ResErr e
        when ResOk cBase s1 ->
          match resolveFields env s1 fields
            when ResErr e -> ResErr e
            when ResOk cFields s2 -> ResOk (CRecordUpdate cBase cFields) s2
          end
      end

    -- Field access
    when CFieldAccess e field ->
      match resolveExpr env state e
        when ResErr e -> ResErr e
        when ResOk cE s1 -> ResOk (CFieldAccess cE field) s1
      end

    -- Foreign: pass through
    when CForeign mod name -> ResOk (CForeign mod name) state
  end

  -- ===========================================================================
  -- Helper Functions
  -- ===========================================================================

  and bindRecNames env state bindings =
    match bindings
      when Nil -> (env, state, Nil)
      when Cons (CBinding (Name _ name) _) rest ->
        let (resolvedName, s1) = freshName name state in
        let env2 = extendEnv name resolvedName env in
        let (env3, s2, restNames) = bindRecNames env2 s1 rest in
        (env3, s2, Cons resolvedName restNames)
    end

  and resolveBindingBodies env state bindings names =
    match (bindings, names)
      when (Nil, Nil) -> ResOk Nil state
      when (Cons (CBinding _ body) restBindings, Cons resolvedName restNames) ->
        match resolveExpr env state body
          when ResErr e -> ResErr e
          when ResOk cBody s1 ->
            match resolveBindingBodies env s1 restBindings restNames
              when ResErr e -> ResErr e
              when ResOk cRest s2 ->
                ResOk (Cons (CBinding resolvedName cBody) cRest) s2
            end
        end
      when _ -> ResErr "Internal error: binding/name mismatch"
    end

  and resolveExprs env state exprs =
    match exprs
      when Nil -> ResOk Nil state
      when Cons e rest ->
        match resolveExpr env state e
          when ResErr err -> ResErr err
          when ResOk cE s1 ->
            match resolveExprs env s1 rest
              when ResErr err -> ResErr err
              when ResOk cRest s2 -> ResOk (Cons cE cRest) s2
            end
        end
    end

  and resolveFields env state fields =
    match fields
      when Nil -> ResOk Nil state
      when Cons (CField name expr) rest ->
        match resolveExpr env state expr
          when ResErr e -> ResErr e
          when ResOk cExpr s1 ->
            match resolveFields env s1 rest
              when ResErr e -> ResErr e
              when ResOk cRest s2 ->
                ResOk (Cons (CField name cExpr) cRest) s2
            end
        end
    end

  and resolveCases env state cases =
    match cases
      when Nil -> ResOk Nil state
      when Cons (CCase pat guard body) rest ->
        -- Pattern introduces bindings
        let (env2, s1, cPat) = resolvePattern env state pat in
        -- Resolve guard in extended environment
        match resolveMaybeExpr env2 s1 guard
          when ResErr e -> ResErr e
          when ResOk cGuard s2 ->
            -- Resolve body in extended environment
            match resolveExpr env2 s2 body
              when ResErr e -> ResErr e
              when ResOk cBody s3 ->
                match resolveCases env s3 rest
                  when ResErr e -> ResErr e
                  when ResOk cRest s4 ->
                    ResOk (Cons (CCase cPat cGuard cBody) cRest) s4
                end
            end
        end
    end

  and resolveMaybeExpr env state maybeExpr =
    match maybeExpr
      when Nothing -> ResOk Nothing state
      when Just e ->
        match resolveExpr env state e
          when ResErr err -> ResErr err
          when ResOk cE s1 -> ResOk (Just cE) s1
        end
    end

  -- ===========================================================================
  -- Pattern Resolution
  -- Patterns introduce bindings into the environment
  -- ===========================================================================

  and resolvePattern env state pat = match pat
    -- Wildcard: no bindings
    when CPWild -> (env, state, CPWild)

    -- Variable: bind
    when CPVar (Name _ name) ->
      let (resolvedName, s1) = freshName name state in
      let env2 = extendEnv name resolvedName env in
      (env2, s1, CPVar resolvedName)

    -- Literal: no bindings
    when CPLit l -> (env, state, CPLit l)

    -- Constructor: resolve nested patterns
    when CPCon c pats ->
      let (env2, s1, cPats) = resolvePatterns env state pats in
      (env2, s1, CPCon c cPats)

    -- Tuple
    when CPTuple pats ->
      let (env2, s1, cPats) = resolvePatterns env state pats in
      (env2, s1, CPTuple cPats)

    -- Record
    when CPRecord fields ->
      let (env2, s1, cFields) = resolvePatternFields env state fields in
      (env2, s1, CPRecord cFields)

    -- As pattern: bind the name and resolve the inner pattern
    when CPAs (Name _ name) p ->
      let (resolvedName, s1) = freshName name state in
      let env2 = extendEnv name resolvedName env in
      let (env3, s2, cP) = resolvePattern env2 s1 p in
      (env3, s2, CPAs resolvedName cP)

    -- Or pattern: both alternatives must bind the same names
    -- For simplicity, we require identical structure (proper impl would check)
    when CPOr p1 p2 ->
      let (env2, s1, cP1) = resolvePattern env state p1 in
      let (env3, s2, cP2) = resolvePattern env2 s1 p2 in
      (env3, s2, CPOr cP1 cP2)
  end

  and resolvePatterns env state pats =
    match pats
      when Nil -> (env, state, Nil)
      when Cons p rest ->
        let (env2, s1, cP) = resolvePattern env state p in
        let (env3, s2, cRest) = resolvePatterns env2 s1 rest in
        (env3, s2, Cons cP cRest)
    end

  and resolvePatternFields env state fields =
    match fields
      when Nil -> (env, state, Nil)
      when Cons (CPatternField name pat) rest ->
        let (env2, s1, cPat) = resolvePattern env state pat in
        let (env3, s2, cRest) = resolvePatternFields env2 s1 rest in
        (env3, s2, Cons (CPatternField name cPat) cRest)
    end

  -- ===========================================================================
  -- Declaration Resolution
  -- ===========================================================================

  let rec resolveDecl env state decl = match decl
    when CDeclType name params cons ->
      -- Type declarations are global, no name resolution needed
      ResOk (CDeclType name params cons, env) state

    when CDeclLet (Name _ name) expr ->
      match resolveExpr env state expr
        when ResErr e -> ResErr e
        when ResOk cExpr s1 ->
          let (resolvedName, s2) = freshName name s1 in
          let env2 = extendEnv name resolvedName env in
          ResOk (CDeclLet resolvedName cExpr, env2) s2
      end

    when CDeclLetRec bindings ->
      -- Bind all names first
      let (env2, s1, names) = bindRecNames env state bindings in
      -- Resolve all bodies
      match resolveBindingBodies env2 s1 bindings names
        when ResErr e -> ResErr e
        when ResOk cBindings s2 ->
          ResOk (CDeclLetRec cBindings, env2) s2
      end

    when CDeclForeign (Name _ name) mod jsName ty ->
      let (resolvedName, s1) = freshName name state in
      let env2 = extendEnv name resolvedName env in
      ResOk (CDeclForeign resolvedName mod jsName ty, env2) s1
  end

  and resolveDecls env state decls =
    match decls
      when Nil -> ResOk (Nil, env) state
      when Cons d rest ->
        match resolveDecl env state d
          when ResErr e -> ResErr e
          when ResOk (cD, env2) s1 ->
            match resolveDecls env2 s1 rest
              when ResErr e -> ResErr e
              when ResOk (cRest, env3) s2 ->
                ResOk (Cons cD cRest, env3) s2
            end
        end
    end

  -- ===========================================================================
  -- Built-in Environment
  -- Includes operators and prelude functions
  -- ===========================================================================

  let makeBuiltinEnv dummy =
    let rec build names id = match names
      when Nil -> Env Nil
      when Cons name rest ->
        let restEnv = build rest (id + 1) in
        match restEnv when Env entries -> Env (Cons (name, Name id name) entries) end
    end in
    let ops = [
      "+", "-", "*", "/",
      "==", "!=", "<", "<=", ">", ">=",
      "&&", "||"
    ] in
    let preludeFns = [
      "map", "filter", "foldr", "foldl", "concat", "reverse", "append",
      "head", "tail", "length", "isEmpty",
      "flatMap", "pure"
    ] in
    let allNames = List.append ops preludeFns in
    build allNames 0

  -- ===========================================================================
  -- Entry Point
  -- ===========================================================================

  let resolve expr =
    let env = makeBuiltinEnv 0 in
    match resolveExpr env initState expr
      when ResErr e -> Left e
      when ResOk cExpr _ -> Right cExpr
    end

  let resolveProgram decls maybeExpr =
    let env = makeBuiltinEnv 0 in
    match resolveDecls env initState decls
      when ResErr e -> Left e
      when ResOk (cDecls, env2) s1 ->
        match maybeExpr
          when Nothing -> Right (cDecls, Nothing)
          when Just expr ->
            match resolveExpr env2 s1 expr
              when ResErr e -> Left e
              when ResOk cExpr _ -> Right (cDecls, Just cExpr)
            end
        end
    end

end
