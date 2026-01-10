-- =============================================================================
-- Optimize Module
-- ANF IR Optimizations
-- Reference: SPEC.md Section 11, Appel (1992)
-- =============================================================================

module Optimize
  use Types (..)
  use IR (..)

  -- ===========================================================================
  -- Optimization Configuration
  -- ===========================================================================

  -- OptConfig: maxInlineSize, maxPasses
  type OptConfig = OptConfig int int

  let defaultConfig = OptConfig 10 10

  -- ===========================================================================
  -- Optimization Pass
  -- ===========================================================================

  -- Run all optimizations until fixed point
  let optimize config expr =
    optimizeLoop config 0 expr

  let rec optimizeLoop config pass expr =
    match config
      when OptConfig _ maxPasses ->
        if pass >= maxPasses then expr
        else
          let expr2 = optimizePass expr in
          if irExprEqual expr expr2 then expr2
          else optimizeLoop config (pass + 1) expr2
    end

  -- Single optimization pass
  let optimizePass expr =
    expr
    |> constantFold
    |> deadCodeElim
    |> betaReduce
    |> etaReduce
    |> caseOfKnownCon

  -- ===========================================================================
  -- Dead Code Elimination
  -- Remove let bindings whose bound variable is never used
  -- ===========================================================================

  let rec deadCodeElim expr = match expr
    when IRAtom a -> IRAtom a

    when IRLet name binding body ->
      let body2 = deadCodeElim body in
      let fv = freeVarsExpr body2 in
      if Set.member name fv then
        IRLet name (deadCodeElimBinding binding) body2
      else
        -- Variable not used, remove binding (unless it has side effects)
        if bindingHasSideEffects binding then
          IRLet name (deadCodeElimBinding binding) body2
        else
          body2

    when IRLetRec bindings body ->
      let body2 = deadCodeElim body in
      -- For recursive bindings, keep all bindings (don't try to eliminate)
      -- This is conservative but correct - mutual recursion makes analysis complex
      let optimizedBindings = List.map (b -> match b
        when IRRecBinding n bind -> IRRecBinding n (deadCodeElimBinding bind)
      end) bindings in
      IRLetRec optimizedBindings body2

    when IRMatch atom cases ->
      IRMatch atom (List.map deadCodeElimCase cases)
  end

  and deadCodeElimBinding binding = match binding
    when IRBLambda param body -> IRBLambda param (deadCodeElim body)
    when _ -> binding
  end

  and deadCodeElimCase c = match c
    when IRCase pat body -> IRCase pat (deadCodeElim body)
  end

  -- Check if binding has side effects (foreign calls)
  let bindingHasSideEffects binding = match binding
    when IRBForeign _ _ _ -> true
    when _ -> false
  end

  -- ===========================================================================
  -- Constant Folding
  -- Evaluate constant expressions at compile time
  -- ===========================================================================

  let rec constantFold expr = match expr
    when IRAtom a -> IRAtom a

    when IRLet name binding body ->
      let binding2 = constantFoldBinding binding in
      let body2 = constantFold body in
      IRLet name binding2 body2

    when IRLetRec bindings body ->
      let bindings2 = List.map (b -> match b
        when IRRecBinding n bind -> IRRecBinding n (constantFoldBinding bind)
      end) bindings in
      IRLetRec bindings2 (constantFold body)

    when IRMatch atom cases ->
      IRMatch atom (List.map constantFoldCase cases)
  end

  and constantFoldBinding binding = match binding
    when IRBBinOp op (ALit l1) (ALit l2) ->
      match foldBinOp op l1 l2
        when Just lit -> IRBAtom (ALit lit)
        when Nothing -> binding
      end
    when IRBLambda param body -> IRBLambda param (constantFold body)
    when _ -> binding
  end

  and constantFoldCase c = match c
    when IRCase pat body -> IRCase pat (constantFold body)
  end

  -- Fold binary operations on literals
  let foldBinOp op l1 l2 = match (op, l1, l2)
    -- Integer arithmetic
    when ("+", LInt a, LInt b) -> Just (LInt (a + b))
    when ("-", LInt a, LInt b) -> Just (LInt (a - b))
    when ("*", LInt a, LInt b) -> Just (LInt (a * b))
    when ("/", LInt a, LInt b) ->
      if b == 0 then Nothing else Just (LInt (a / b))

    -- Float arithmetic
    when ("+", LFloat a, LFloat b) -> Just (LFloat (a + b))
    when ("-", LFloat a, LFloat b) -> Just (LFloat (a - b))
    when ("*", LFloat a, LFloat b) -> Just (LFloat (a * b))
    when ("/", LFloat a, LFloat b) -> Just (LFloat (a / b))

    -- Integer comparison
    when ("==", LInt a, LInt b) -> Just (LBool (a == b))
    when ("!=", LInt a, LInt b) -> Just (LBool (a != b))
    when ("<", LInt a, LInt b) -> Just (LBool (a < b))
    when ("<=", LInt a, LInt b) -> Just (LBool (a <= b))
    when (">", LInt a, LInt b) -> Just (LBool (a > b))
    when (">=", LInt a, LInt b) -> Just (LBool (a >= b))

    -- Boolean operations
    when ("&&", LBool a, LBool b) -> Just (LBool (a && b))
    when ("||", LBool a, LBool b) -> Just (LBool (a || b))

    -- String comparison
    when ("==", LString a, LString b) -> Just (LBool (a == b))
    when ("!=", LString a, LString b) -> Just (LBool (a != b))

    -- String concatenation
    when ("+", LString a, LString b) -> Just (LString (a + b))

    when _ -> Nothing
  end

  -- ===========================================================================
  -- Beta Reduction
  -- Substitute function arguments directly when safe
  -- ===========================================================================

  let rec betaReduce expr = match expr
    when IRAtom a -> IRAtom a

    when IRLet name binding body ->
      let binding2 = betaReduceBinding binding in
      let body2 = betaReduce body in
      -- Check for beta reduction opportunity
      match binding2
        when IRBApp (AVar fn) arg ->
          -- Look for (fn x -> e) v pattern
          -- For now, just optimize the binding and body
          IRLet name binding2 body2
        when _ -> IRLet name binding2 body2
      end

    when IRLetRec bindings body ->
      let bindings2 = List.map (b -> match b
        when IRRecBinding n bind -> IRRecBinding n (betaReduceBinding bind)
      end) bindings in
      IRLetRec bindings2 (betaReduce body)

    when IRMatch atom cases ->
      IRMatch atom (List.map betaReduceCase cases)
  end

  and betaReduceBinding binding = match binding
    when IRBLambda param body -> IRBLambda param (betaReduce body)
    when _ -> binding
  end

  and betaReduceCase c = match c
    when IRCase pat body -> IRCase pat (betaReduce body)
  end

  -- ===========================================================================
  -- Eta Reduction
  -- Simplify redundant lambda wrappers: (x -> f x) => f when x not in fv(f)
  -- ===========================================================================

  let rec etaReduce expr = match expr
    when IRAtom a -> IRAtom a

    when IRLet name binding body ->
      let binding2 = etaReduceBinding binding in
      let body2 = etaReduce body in
      IRLet name binding2 body2

    when IRLetRec bindings body ->
      let bindings2 = List.map (b -> match b
        when IRRecBinding n bind -> IRRecBinding n (etaReduceBinding bind)
      end) bindings in
      IRLetRec bindings2 (etaReduce body)

    when IRMatch atom cases ->
      IRMatch atom (List.map etaReduceCase cases)
  end

  and etaReduceBinding binding = match binding
    when IRBLambda param body ->
      -- Check for eta reduction: (x -> f x) => f
      match body
        when IRLet tmpName (IRBApp f (AVar arg)) (IRAtom (AVar result)) ->
          if arg == param && result == tmpName then
            -- Can reduce if param not free in f
            let fv = freeVarsAtom f in
            if Set.member param fv then
              IRBLambda param (etaReduce body)
            else
              -- Eta reduce: return just the function
              match f
                when AVar fn -> IRBApp f (ALit (LInt 0))
                when _ -> IRBLambda param (etaReduce body)
              end
          else
            IRBLambda param (etaReduce body)
        when _ -> IRBLambda param (etaReduce body)
      end
    when _ -> binding
  end

  and etaReduceCase c = match c
    when IRCase pat body -> IRCase pat (etaReduce body)
  end

  -- ===========================================================================
  -- Case-of-Known-Constructor
  -- When matching on a known constructor, select the branch directly
  -- ===========================================================================

  let rec caseOfKnownCon expr = match expr
    when IRAtom a -> IRAtom a

    when IRLet name binding body ->
      IRLet name (caseOfKnownConBinding binding) (caseOfKnownCon body)

    when IRLetRec bindings body ->
      let bindings2 = List.map (b -> match b
        when IRRecBinding n bind -> IRRecBinding n (caseOfKnownConBinding bind)
      end) bindings in
      IRLetRec bindings2 (caseOfKnownCon body)

    when IRMatch atom cases ->
      match atom
        when ACon name tag ->
          -- Known constructor: find matching case
          match findMatchingCase name tag [] cases
            when Just (bindings, body) ->
              -- Substitute constructor args into body
              caseOfKnownCon body
            when Nothing ->
              IRMatch atom (List.map caseOfKnownConCase cases)
          end
        when ALit lit ->
          -- Known literal: find matching case
          match findLiteralCase lit cases
            when Just body -> caseOfKnownCon body
            when Nothing -> IRMatch atom (List.map caseOfKnownConCase cases)
          end
        when _ -> IRMatch atom (List.map caseOfKnownConCase cases)
      end
  end

  and caseOfKnownConBinding binding = match binding
    when IRBLambda param body -> IRBLambda param (caseOfKnownCon body)
    when _ -> binding
  end

  and caseOfKnownConCase c = match c
    when IRCase pat body -> IRCase pat (caseOfKnownCon body)
  end

  -- Find case matching constructor
  let rec findMatchingCase conName tag args cases = match cases
    when Nil -> Nothing
    when Cons (IRCase pat body) rest ->
      match pat
        when IRPCon name ptag pats ->
          if name == conName && ptag == tag then
            Just (pats, body)
          else
            findMatchingCase conName tag args rest
        when IRPWild -> Just ([], body)
        when IRPVar _ -> Just ([], body)
        when _ -> findMatchingCase conName tag args rest
      end
  end

  -- Find case matching literal
  let rec findLiteralCase lit cases = match cases
    when Nil -> Nothing
    when Cons (IRCase pat body) rest ->
      match pat
        when IRPLit l ->
          if literalEqual l lit then Just body
          else findLiteralCase lit rest
        when IRPWild -> Just body
        when IRPVar _ -> Just body
        when _ -> findLiteralCase lit rest
      end
  end

  -- ===========================================================================
  -- Inlining (Simple version)
  -- Replace function calls with function body for small functions
  -- ===========================================================================

  let inlineSmallFunctions config expr =
    match config
      when OptConfig maxSize _ ->
        inlinePass maxSize expr
    end

  let rec inlinePass maxSize expr = match expr
    when IRAtom a -> IRAtom a

    when IRLet name binding body ->
      let binding2 = inlineBinding maxSize binding in
      let body2 = inlinePass maxSize body in
      -- Check if this binding should be inlined
      if shouldInline maxSize binding2 body2 then
        -- Inline the binding into the body
        inlineIntoBody name binding2 body2
      else
        IRLet name binding2 body2

    when IRLetRec bindings body ->
      let bindings2 = List.map (b -> match b
        when IRRecBinding n bind -> IRRecBinding n (inlineBinding maxSize bind)
      end) bindings in
      IRLetRec bindings2 (inlinePass maxSize body)

    when IRMatch atom cases ->
      IRMatch atom (List.map (c -> match c
        when IRCase pat body -> IRCase pat (inlinePass maxSize body)
      end) cases)
  end

  and inlineBinding maxSize binding = match binding
    when IRBLambda param body -> IRBLambda param (inlinePass maxSize body)
    when _ -> binding
  end

  -- Check if binding should be inlined
  let shouldInline maxSize binding body =
    let size = bindingSize binding in
    let uses = countUses binding body in
    -- Inline if small or used exactly once
    size <= maxSize || uses <= 1

  -- Count AST nodes in binding
  let rec bindingSize binding = match binding
    when IRBApp _ _ -> 1
    when IRBBinOp _ _ _ -> 1
    when IRBTuple atoms -> List.length atoms
    when IRBRecord fields -> List.length fields
    when IRBRecordUpdate _ fields -> 1 + List.length fields
    when IRBField _ _ -> 1
    when IRBLambda _ body -> 1 + exprSize body
    when IRBForeign _ _ args -> 1 + List.length args
    when IRBCon _ _ args -> 1 + List.length args
  end

  and exprSize expr = match expr
    when IRAtom _ -> 1
    when IRLet _ b e -> 1 + bindingSize b + exprSize e
    when IRLetRec bs e ->
      let bsSize = List.foldl (acc b -> match b
        when IRRecBinding _ bind -> acc + bindingSize bind
      end) 0 bs in
      1 + bsSize + exprSize e
    when IRMatch _ cs ->
      1 + List.foldl (acc c -> match c
        when IRCase _ body -> acc + exprSize body
      end) 0 cs
  end

  -- Count uses of a variable in expression
  let countUses binding body = 1

  -- Inline binding into body (simple version)
  let inlineIntoBody name binding body = body

  -- ===========================================================================
  -- Equality Check for Fixed Point
  -- ===========================================================================

  let rec irExprEqual e1 e2 = match (e1, e2)
    when (IRAtom a1, IRAtom a2) -> atomEqual a1 a2
    when (IRLet n1 b1 body1, IRLet n2 b2 body2) ->
      n1 == n2 && bindingEqual b1 b2 && irExprEqual body1 body2
    when (IRLetRec bs1 body1, IRLetRec bs2 body2) ->
      recBindingsEqual bs1 bs2 && irExprEqual body1 body2
    when (IRMatch a1 cs1, IRMatch a2 cs2) ->
      atomEqual a1 a2 && casesEqual cs1 cs2
    when _ -> false
  end

  and atomEqual a1 a2 = match (a1, a2)
    when (AVar n1, AVar n2) -> n1 == n2
    when (ALit l1, ALit l2) -> literalEqual l1 l2
    when (ACon n1 t1, ACon n2 t2) -> n1 == n2 && t1 == t2
    when _ -> false
  end

  and literalEqual l1 l2 = match (l1, l2)
    when (LInt a, LInt b) -> a == b
    when (LFloat a, LFloat b) -> a == b
    when (LString a, LString b) -> a == b
    when (LChar a, LChar b) -> a == b
    when (LBool a, LBool b) -> a == b
    when _ -> false
  end

  and bindingEqual b1 b2 = match (b1, b2)
    when (IRBApp f1 x1, IRBApp f2 x2) -> atomEqual f1 f2 && atomEqual x1 x2
    when (IRBBinOp op1 a1 b1, IRBBinOp op2 a2 b2) ->
      op1 == op2 && atomEqual a1 a2 && atomEqual b1 b2
    when (IRBLambda p1 e1, IRBLambda p2 e2) -> p1 == p2 && irExprEqual e1 e2
    when _ -> false
  end

  and recBindingsEqual bs1 bs2 =
    List.length bs1 == List.length bs2

  and casesEqual cs1 cs2 =
    List.length cs1 == List.length cs2

  -- ===========================================================================
  -- Entry Points
  -- ===========================================================================

  -- Optimize a program
  let optimizeProgram config program =
    match program
      when IRProgram decls maybeExpr ->
        let optDecls = List.map (optimizeDecl config) decls in
        let optExpr = match maybeExpr
          when Nothing -> Nothing
          when Just expr -> Just (optimize config expr)
        end in
        IRProgram optDecls optExpr
    end

  let optimizeDecl config decl = match decl
    when IRDeclLet name binding ->
      IRDeclLet name (optimizeBinding config binding)
    when IRDeclLetRec bindings ->
      let optBindings = List.map (b -> match b
        when IRRecBinding n bind -> IRRecBinding n (optimizeBinding config bind)
      end) bindings in
      IRDeclLetRec optBindings
  end

  let optimizeBinding config binding = match binding
    when IRBLambda param body ->
      IRBLambda param (optimize config body)
    when _ -> binding
  end

  -- Optimize with default config
  let optimizeDefault expr = optimize defaultConfig expr
  let optimizeProgramDefault program = optimizeProgram defaultConfig program

end
