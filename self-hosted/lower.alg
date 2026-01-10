-- =============================================================================
-- Lower Module
-- Lowering from Core AST to ANF IR
-- Reference: SPEC.md Section 10.2, Flanagan et al. (1993)
-- =============================================================================

module Lower
  use Types (..)
  use IR (..)

  -- ===========================================================================
  -- Lowering State
  -- ===========================================================================

  type LowerState = LowerState int

  let initLowerState = LowerState 0

  let freshName state prefix =
    match state
      when LowerState n ->
        (Name n (prefix + Int.toString n), LowerState (n + 1))
    end

  -- ===========================================================================
  -- Constructor Tag Registry
  -- ===========================================================================

  -- Registry maps constructor names to their tags
  type TagRegistry = TagRegistry (List (string, int))

  let emptyRegistry = TagRegistry Nil

  let lookupTag name registry =
    match registry
      when TagRegistry entries ->
        let rec find es = match es
          when Nil -> Nothing
          when Cons (n, tag) rest ->
            if n == name then Just tag
            else find rest
        end
        in find entries
    end

  let addTag name tag registry =
    match registry
      when TagRegistry entries ->
        TagRegistry (Cons (name, tag) entries)
    end

  -- Build registry from type declarations
  let buildRegistry decls =
    List.foldl (acc d -> match d
      when CDeclType _ _ constructors ->
        addConstructors acc 0 constructors
      when _ -> acc
    end) emptyRegistry decls

  let rec addConstructors registry tag constructors =
    match constructors
      when Nil -> registry
      when Cons (CConDecl name _) rest ->
        let registry2 = addTag name tag registry in
        addConstructors registry2 (tag + 1) rest
    end

  -- ===========================================================================
  -- Lowering Context
  -- ===========================================================================

  type LowerContext = LowerContext LowerState TagRegistry

  let mkContext registry = LowerContext initLowerState registry

  let getState ctx = match ctx when LowerContext state _ -> state end
  let getRegistry ctx = match ctx when LowerContext _ reg -> reg end

  let withState ctx state =
    match ctx when LowerContext _ reg -> LowerContext state reg end

  let fresh ctx prefix =
    let state = getState ctx in
    let (name, newState) = freshName state prefix in
    (name, withState ctx newState)

  -- ===========================================================================
  -- Binary Operators
  -- ===========================================================================

  -- Check if a name is a binary operator
  let isBinOp op =
    op == "+" || op == "-" || op == "*" || op == "/" ||
    op == "==" || op == "!=" ||
    op == "<" || op == "<=" || op == ">" || op == ">=" ||
    op == "&&" || op == "||"

  -- Collect application head and all arguments
  -- CApp (CApp f a1) a2 => (f, [a1, a2])
  let collectApp expr =
    let rec collect e args = match e
      when CApp e1 e2 -> collect e1 (Cons e2 args)
      when _ -> (e, args)
    end in
    collect expr Nil

  -- ===========================================================================
  -- ANF Conversion (Continuation-Passing Style)
  -- ===========================================================================

  -- The key insight: normalize ensures subexpressions are atoms
  -- We use CPS to thread the state and build let bindings

  -- lower : Context -> CExpr -> (Context, IRExpr)
  let rec lower ctx expr = match expr
    -- Variables are atoms
    when CVar name ->
      (ctx, IRAtom (AVar name))

    -- Literals are atoms
    when CLit lit ->
      (ctx, IRAtom (ALit lit))

    -- Constructors are atoms (nullary) or need application
    when CCon name ->
      let tag = match lookupTag name (getRegistry ctx)
        when Just t -> t
        when Nothing -> 0
      end in
      (ctx, IRAtom (ACon name tag))

    -- Application: check for constructor, binary op, or regular function
    when CApp e1 e2 ->
      -- First, collect all arguments and find the head
      let (head, args) = collectApp expr in
      match head
        -- Constructor application
        when CCon name ->
          let tag = match lookupTag name (getRegistry ctx)
            when Just t -> t
            when Nothing -> 0
          end in
          let (ctx2, atoms, wrap) = normalizeManyW ctx args in
          let (tmp, ctx3) = fresh ctx2 "con" in
          let result = IRLet tmp (IRBCon name tag atoms) (IRAtom (AVar tmp)) in
          (ctx3, wrap result)

        -- Binary operator pattern
        when CVar (Name _ op) if isBinOp op && List.length args == 2 ->
          match args
            when Cons arg1 (Cons arg2 Nil) ->
              let (ctx2, a1, wrap1) = normalize ctx arg1 in
              let (ctx3, a2, wrap2) = normalize ctx2 arg2 in
              let (tmp, ctx4) = fresh ctx3 "bin" in
              let result = IRLet tmp (IRBBinOp op a1 a2) (IRAtom (AVar tmp)) in
              (ctx4, wrap1 (wrap2 result))
            when _ ->
              -- Fallback
              let (ctx2, a1, wrap1) = normalize ctx e1 in
              let (ctx3, a2, wrap2) = normalize ctx2 e2 in
              let (tmp, ctx4) = fresh ctx3 "app" in
              let result = IRLet tmp (IRBApp a1 a2) (IRAtom (AVar tmp)) in
              (ctx4, wrap1 (wrap2 result))
          end

        -- Regular application
        when _ ->
          let (ctx2, a1, wrap1) = normalize ctx e1 in
          let (ctx3, a2, wrap2) = normalize ctx2 e2 in
          let (tmp, ctx4) = fresh ctx3 "app" in
          let result = IRLet tmp (IRBApp a1 a2) (IRAtom (AVar tmp)) in
          (ctx4, wrap1 (wrap2 result))
      end

    -- Lambda: lower body
    when CAbs param body ->
      let (ctx2, irBody) = lower ctx body in
      let (tmp, ctx3) = fresh ctx2 "fn" in
      (ctx3, IRLet tmp (IRBLambda param irBody) (IRAtom (AVar tmp)))

    -- Let: lower value and body
    -- Splice nested lets from value and bind final result to name
    when CLet name value body ->
      let (ctx2, irValue) = lower ctx value in
      let (ctx3, irBody) = lower ctx2 body in
      (ctx3, spliceLetBindings name irValue irBody)

    -- LetRec: lower all bindings and body
    when CLetRec bindings body ->
      let (ctx2, irBindings) = lowerRecBindings ctx bindings in
      let (ctx3, irBody) = lower ctx2 body in
      (ctx3, IRLetRec irBindings irBody)

    -- Match: normalize scrutinee, lower cases
    when CMatch scrutinee cases ->
      let (ctx2, atom, wrap) = normalize ctx scrutinee in
      let (ctx3, irCases) = lowerCases ctx2 cases in
      (ctx3, wrap (IRMatch atom irCases))

    -- Tuple: normalize all elements
    when CTuple exprs ->
      let (ctx2, atoms, wrap) = normalizeManyW ctx exprs in
      let (tmp, ctx3) = fresh ctx2 "tup" in
      (ctx3, wrap (IRLet tmp (IRBTuple atoms) (IRAtom (AVar tmp))))

    -- Record: normalize all field values
    when CRecord fields ->
      let (ctx2, irFields, wrap) = normalizeFieldsW ctx fields in
      let (tmp, ctx3) = fresh ctx2 "rec" in
      (ctx3, wrap (IRLet tmp (IRBRecord irFields) (IRAtom (AVar tmp))))

    -- Record update: normalize base and fields
    when CRecordUpdate base fields ->
      let (ctx2, baseAtom, wrap1) = normalize ctx base in
      let (ctx3, irFields, wrap2) = normalizeFieldsW ctx2 fields in
      let (tmp, ctx4) = fresh ctx3 "upd" in
      (ctx4, wrap1 (wrap2 (IRLet tmp (IRBRecordUpdate baseAtom irFields) (IRAtom (AVar tmp)))))

    -- Field access: normalize record expression
    when CFieldAccess e field ->
      let (ctx2, atom, wrap) = normalize ctx e in
      let (tmp, ctx3) = fresh ctx2 "fld" in
      (ctx3, wrap (IRLet tmp (IRBField atom field) (IRAtom (AVar tmp))))

    -- Foreign: normalize arguments
    when CForeign mod name ->
      let (tmp, ctx2) = fresh ctx "for" in
      (ctx2, IRLet tmp (IRBForeign mod name []) (IRAtom (AVar tmp)))
  end

  -- ===========================================================================
  -- Normalize: Ensure expression becomes an atom
  -- Returns (Context, Atom, IRExpr -> IRExpr) where the third element wraps
  -- subsequent code with any necessary let bindings
  -- ===========================================================================

  -- normalize : Context -> CExpr -> (Context, Atom, IRExpr -> IRExpr)
  and normalize ctx expr = match expr
    -- Already atomic - no wrapping needed
    when CVar name -> (ctx, AVar name, (k -> k))
    when CLit lit -> (ctx, ALit lit, (k -> k))
    when CCon name ->
      let tag = match lookupTag name (getRegistry ctx)
        when Just t -> t
        when Nothing -> 0
      end in
      (ctx, ACon name tag, (k -> k))

    -- Not atomic: lower and extract atom with full wrapper
    when _ ->
      let (ctx2, irExpr) = lower ctx expr in
      let (atom, wrapper) = extractAtomAndWrapper irExpr in
      (ctx2, atom, wrapper)
  end

  -- Extract the final atom and build a wrapper that preserves all let bindings
  and extractAtomAndWrapper expr = match expr
    when IRAtom atom -> (atom, (k -> k))
    when IRLet name binding body ->
      let (atom, innerWrapper) = extractAtomAndWrapper body in
      (atom, (k -> IRLet name binding (innerWrapper k)))
    when IRLetRec bindings body ->
      let (atom, innerWrapper) = extractAtomAndWrapper body in
      (atom, (k -> IRLetRec bindings (innerWrapper k)))
    when IRMatch atom cases ->
      -- Match results need to be bound to a variable
      -- This is a fallback - shouldn't normally happen
      (atom, (k -> k))
  end

  -- Splice nested lets from irValue and bind the final atom to name
  and spliceLetBindings name irValue irBody = match irValue
    when IRAtom atom ->
      -- Bind name to the atom
      IRLet name (atomToBinding atom) irBody
    when IRLet innerName binding innerBody ->
      -- Splice: hoist this binding, continue with rest
      IRLet innerName binding (spliceLetBindings name innerBody irBody)
    when IRLetRec bindings innerBody ->
      IRLetRec bindings (spliceLetBindings name innerBody irBody)
    when IRMatch atom cases ->
      -- Can't splice a match; wrap it in a let
      IRLet name (IRBTuple []) irBody
  end

  -- Convert atom to binding
  and atomToBinding atom = match atom
    when AVar name -> IRBAtom (AVar name)
    when ALit lit -> IRBAtom (ALit lit)
    when ACon name tag -> IRBCon name tag []
  end

  -- Normalize many expressions (returns combined wrapper)
  and normalizeManyW ctx exprs =
    match exprs
      when Nil -> (ctx, Nil, (k -> k))
      when Cons e rest ->
        let (ctx2, atom, wrap1) = normalize ctx e in
        let (ctx3, atoms, wrap2) = normalizeManyW ctx2 rest in
        (ctx3, Cons atom atoms, (k -> wrap1 (wrap2 k)))
    end

  -- Normalize record fields (returns combined wrapper)
  and normalizeFieldsW ctx fields =
    match fields
      when Nil -> (ctx, Nil, (k -> k))
      when Cons (CField name e) rest ->
        let (ctx2, atom, wrap1) = normalize ctx e in
        let (ctx3, restFields, wrap2) = normalizeFieldsW ctx2 rest in
        (ctx3, Cons (name, atom) restFields, (k -> wrap1 (wrap2 k)))
    end

  -- ===========================================================================
  -- Convert IRExpr to IRBinding
  -- ===========================================================================

  -- Extract binding from an IR expression
  and exprToBinding expr = match expr
    when IRAtom atom ->
      match atom
        when AVar name -> IRBApp (AVar name) (ALit (LInt 0))
        when ALit lit -> IRBTuple [ALit lit]
        when ACon name tag -> IRBCon name tag []
      end
    when IRLet _ binding _ -> binding
    when IRLetRec bindings body ->
      match bindings
        when Nil -> exprToBinding body
        when Cons (IRRecBinding _ binding) _ -> binding
      end
    when IRMatch _ _ -> IRBTuple []
  end

  -- ===========================================================================
  -- Lower Recursive Bindings
  -- ===========================================================================

  and lowerRecBindings ctx bindings =
    match bindings
      when Nil -> (ctx, Nil)
      when Cons (CBinding name expr) rest ->
        let (ctx2, irExpr) = lower ctx expr in
        let binding = exprToBinding irExpr in
        let (ctx3, restBindings) = lowerRecBindings ctx2 rest in
        (ctx3, Cons (IRRecBinding name binding) restBindings)
    end

  -- ===========================================================================
  -- Lower Match Cases
  -- ===========================================================================

  and lowerCases ctx cases =
    match cases
      when Nil -> (ctx, Nil)
      when Cons (CCase pat guard body) rest ->
        let irPat = lowerPattern ctx pat in
        let (ctx2, irBody) = lower ctx body in
        -- Handle guard by converting to if-then-else
        let finalBody = match guard
          when Nothing -> irBody
          when Just guardExpr ->
            let (ctx3, guardAtom, guardWrap) = normalize ctx2 guardExpr in
            -- If guard is false, match fails (simplified)
            guardWrap irBody
        end in
        let (ctx3, restCases) = lowerCases ctx2 rest in
        (ctx3, Cons (IRCase irPat finalBody) restCases)
    end

  -- ===========================================================================
  -- Lower Patterns
  -- ===========================================================================

  and lowerPattern ctx pat = match pat
    when CPWild -> IRPWild
    when CPVar name -> IRPVar name
    when CPLit lit -> IRPLit lit
    when CPCon name pats ->
      let tag = match lookupTag name (getRegistry ctx)
        when Just t -> t
        when Nothing -> 0
      end in
      IRPCon name tag (List.map (lowerPattern ctx) pats)
    when CPTuple pats ->
      IRPTuple (List.map (lowerPattern ctx) pats)
    when CPRecord fields ->
      IRPRecord (List.map (f -> match f
        when CPatternField name p -> (name, lowerPattern ctx p)
      end) fields)
    when CPAs name pat ->
      IRPAs name (lowerPattern ctx pat)
    when CPOr p1 _ ->
      -- Or patterns are simplified (take first alternative)
      lowerPattern ctx p1
  end

  -- ===========================================================================
  -- Lower Declarations
  -- ===========================================================================

  let lowerDecl ctx decl = match decl
    when CDeclType _ _ _ ->
      -- Type declarations don't generate IR
      (ctx, Nothing)

    when CDeclLet name expr ->
      let (ctx2, irExpr) = lower ctx expr in
      let binding = exprToBinding irExpr in
      (ctx2, Just (IRDeclLet name binding))

    when CDeclLetRec bindings ->
      let (ctx2, irBindings) = lowerRecBindings ctx bindings in
      (ctx2, Just (IRDeclLetRec irBindings))

    when CDeclForeign name mod jsName ty ->
      let binding = IRBForeign mod jsName [] in
      (ctx, Just (IRDeclLet name binding))
  end

  let rec lowerDecls ctx decls =
    match decls
      when Nil -> (ctx, Nil)
      when Cons d rest ->
        let (ctx2, maybeDecl) = lowerDecl ctx d in
        let (ctx3, restDecls) = lowerDecls ctx2 rest in
        match maybeDecl
          when Nothing -> (ctx3, restDecls)
          when Just irDecl -> (ctx3, Cons irDecl restDecls)
        end
    end

  -- ===========================================================================
  -- Entry Points
  -- ===========================================================================

  -- Lower a program (declarations + optional main expression)
  let lowerProgram decls maybeExpr =
    let registry = buildRegistry decls in
    let ctx = mkContext registry in
    let (ctx2, irDecls) = lowerDecls ctx decls in
    let irExpr = match maybeExpr
      when Nothing -> Nothing
      when Just expr ->
        let (_, ir) = lower ctx2 expr in
        Just ir
    end in
    IRProgram irDecls irExpr

  -- Lower a single expression (for testing)
  let lowerExpr registry expr =
    let ctx = mkContext registry in
    let (_, ir) = lower ctx expr in
    ir

end
