-- =============================================================================
-- Desugar Module
-- Surface AST to Core AST transformation
-- Reference: SPEC.md Section 6
-- =============================================================================

module Desugar
  use Types (..)

  -- ===========================================================================
  -- Fresh Variable Generation
  -- ===========================================================================

  -- State for fresh variable generation
  type FreshState = FreshState int

  let initFresh = FreshState 0

  let fresh state =
    match state
      when FreshState n ->
        let name = "$tmp" + Int.toString n in
        (name, FreshState (n + 1))
    end

  -- ===========================================================================
  -- Expression Desugaring
  -- D⟦_⟧ : Surface.Expr -> Core.Expr
  -- ===========================================================================

  let rec desugarExpr state expr = match expr
    -- Variables pass through (names resolved later)
    when SVar x -> (CVar (Name 0 x), state)

    -- Literals pass through
    when SLit l -> (CLit l, state)

    -- Application: D⟦ e1 e2 ⟧ = CApp (D⟦e1⟧) (D⟦e2⟧)
    when SApp e1 e2 ->
      let (c1, s1) = desugarExpr state e1 in
      let (c2, s2) = desugarExpr s1 e2 in
      (CApp c1 c2, s2)

    -- Multi-param lambda -> nested single-param lambdas
    -- D⟦ SAbs [] e ⟧ = D⟦e⟧
    -- D⟦ SAbs (x:xs) e ⟧ = CAbs x (D⟦ SAbs xs e ⟧)
    when SAbs params body ->
      desugarLambda state params body

    -- Let: D⟦ SLet x e1 e2 ⟧ = CLet x (D⟦e1⟧) (D⟦e2⟧)
    when SLet x e1 e2 ->
      let (c1, s1) = desugarExpr state e1 in
      let (c2, s2) = desugarExpr s1 e2 in
      (CLet (Name 0 x) c1 c2, s2)

    -- Let rec: D⟦ SLetRec bs e ⟧ = CLetRec (map desugar bs) (D⟦e⟧)
    when SLetRec bindings body ->
      let (cBindings, s1) = desugarBindings state bindings in
      let (cBody, s2) = desugarExpr s1 body in
      (CLetRec cBindings cBody, s2)

    -- If -> match on boolean
    -- D⟦ SIf c t f ⟧ = CMatch (D⟦c⟧) [Case true t, Case false f]
    when SIf cond thenBr elseBr ->
      let (cCond, s1) = desugarExpr state cond in
      let (cThen, s2) = desugarExpr s1 thenBr in
      let (cElse, s3) = desugarExpr s2 elseBr in
      let trueCase = CCase (CPLit (LBool true)) Nothing cThen in
      let falseCase = CCase (CPLit (LBool false)) Nothing cElse in
      (CMatch cCond [trueCase, falseCase], s3)

    -- Match: D⟦ SMatch e cases ⟧ = CMatch (D⟦e⟧) (map desugar cases)
    when SMatch scrutinee cases ->
      let (cScrutinee, s1) = desugarExpr state scrutinee in
      let (cCases, s2) = desugarCases s1 cases in
      (CMatch cScrutinee cCases, s2)

    -- Constructor: D⟦ SCon c ⟧ = CCon c
    when SCon c -> (CCon c, state)

    -- Tuple: D⟦ STuple es ⟧ = CTuple (map D es)
    when STuple exprs ->
      let (cExprs, s1) = desugarExprs state exprs in
      (CTuple cExprs, s1)

    -- Record: D⟦ SRecord fs ⟧ = CRecord (map desugar fs)
    when SRecord fields ->
      let (cFields, s1) = desugarFields state fields in
      (CRecord cFields, s1)

    -- Record update: D⟦ SRecordUpdate r fs ⟧ = CRecordUpdate (D⟦r⟧) (map desugar fs)
    when SRecordUpdate base fields ->
      let (cBase, s1) = desugarExpr state base in
      let (cFields, s2) = desugarFields s1 fields in
      (CRecordUpdate cBase cFields, s2)

    -- Field access: D⟦ SFieldAccess e f ⟧ = CFieldAccess (D⟦e⟧) f
    when SFieldAccess e field ->
      let (cE, s1) = desugarExpr state e in
      (CFieldAccess cE field, s1)

    -- List literal -> nested Cons
    -- D⟦ SList [] ⟧ = CCon "Nil"
    -- D⟦ SList (e:es) ⟧ = CApp (CApp (CCon "Cons") (D⟦e⟧)) (D⟦ SList es ⟧)
    when SList exprs ->
      desugarList state exprs

    -- Pipe -> application (flip)
    -- D⟦ SPipeOp e1 e2 ⟧ = CApp (D⟦e2⟧) (D⟦e1⟧)
    when SPipeOp e1 e2 ->
      let (c1, s1) = desugarExpr state e1 in
      let (c2, s2) = desugarExpr s1 e2 in
      (CApp c2 c1, s2)

    -- Cons operator -> Cons constructor application
    -- D⟦ SConsOp e1 e2 ⟧ = CApp (CApp (CCon "Cons") (D⟦e1⟧)) (D⟦e2⟧)
    when SConsOp e1 e2 ->
      let (c1, s1) = desugarExpr state e1 in
      let (c2, s2) = desugarExpr s1 e2 in
      (CApp (CApp (CCon "Cons") c1) c2, s2)

    -- Binary operators -> function application
    -- D⟦ SBinOp op e1 e2 ⟧ = CApp (CApp (CVar op) (D⟦e1⟧)) (D⟦e2⟧)
    when SBinOp op e1 e2 ->
      let (c1, s1) = desugarExpr state e1 in
      let (c2, s2) = desugarExpr s1 e2 in
      (CApp (CApp (CVar (Name 0 op)) c1) c2, s2)

    -- Unary minus -> application of negate or subtraction from 0
    when SUnaryMinus e ->
      let (cE, s1) = desugarExpr state e in
      -- Desugar to (0 - e) which becomes CApp (CApp (CVar "-") (CLit 0)) cE
      (CApp (CApp (CVar (Name 0 "-")) (CLit (LInt 0))) cE, s1)

    -- Do notation -> flatMap chain
    when SDo stmts ->
      desugarDo state stmts

    -- Type annotation -> erased (used only during type checking)
    -- D⟦ SAnnot e t ⟧ = D⟦e⟧
    when SAnnot e _ ->
      desugarExpr state e
  end

  -- ===========================================================================
  -- Lambda Desugaring
  -- ===========================================================================

  and desugarLambda state params body =
    match params
      when Nil -> desugarExpr state body
      when Cons x rest ->
        let (cBody, s1) = desugarLambda state rest body in
        (CAbs (Name 0 x) cBody, s1)
    end

  -- ===========================================================================
  -- List Desugaring
  -- ===========================================================================

  and desugarList state exprs =
    match exprs
      when Nil -> (CCon "Nil", state)
      when Cons e rest ->
        let (cE, s1) = desugarExpr state e in
        let (cRest, s2) = desugarList s1 rest in
        (CApp (CApp (CCon "Cons") cE) cRest, s2)
    end

  -- ===========================================================================
  -- Do-Notation Desugaring
  -- ===========================================================================

  -- D_do⟦ [DoExpr e] ⟧ = D⟦e⟧
  -- D_do⟦ DoBindPattern (SPVar x) e : rest ⟧ = flatMap (λx. D_do⟦rest⟧) (D⟦e⟧)
  -- D_do⟦ DoBindPattern p e : rest ⟧ = flatMap (λ$tmp. match $tmp when p -> D_do⟦rest⟧) (D⟦e⟧)
  -- D_do⟦ DoLet (SPVar x) e : rest ⟧ = let x = D⟦e⟧ in D_do⟦rest⟧
  -- D_do⟦ DoLet p e : rest ⟧ = match D⟦e⟧ when p -> D_do⟦rest⟧
  -- D_do⟦ DoExpr e : rest ⟧ = flatMap (λ_. D_do⟦rest⟧) (D⟦e⟧)

  and desugarDo state stmts =
    match stmts
      -- Final expression
      when Cons (DoExpr e) Nil -> desugarExpr state e

      -- Bind with simple variable
      when Cons (DoBindPattern (SPVar x) e) rest ->
        let (cE, s1) = desugarExpr state e in
        let (cRest, s2) = desugarDo s1 rest in
        let lambda = CAbs (Name 0 x) cRest in
        let flatMapApp = CApp (CApp (CVar (Name 0 "flatMap")) lambda) cE in
        (flatMapApp, s2)

      -- Bind with pattern
      when Cons (DoBindPattern pat e) rest ->
        let (tmpName, s1) = fresh state in
        let (cE, s2) = desugarExpr s1 e in
        let (cPat, s3) = desugarPattern s2 pat in
        let (cRest, s4) = desugarDo s3 rest in
        let matchExpr = CMatch (CVar (Name 0 tmpName)) [CCase cPat Nothing cRest] in
        let lambda = CAbs (Name 0 tmpName) matchExpr in
        let flatMapApp = CApp (CApp (CVar (Name 0 "flatMap")) lambda) cE in
        (flatMapApp, s4)

      -- Let with simple variable
      when Cons (DoLet (SPVar x) e) rest ->
        let (cE, s1) = desugarExpr state e in
        let (cRest, s2) = desugarDo s1 rest in
        (CLet (Name 0 x) cE cRest, s2)

      -- Let with pattern
      when Cons (DoLet pat e) rest ->
        let (cE, s1) = desugarExpr state e in
        let (cPat, s2) = desugarPattern s1 pat in
        let (cRest, s3) = desugarDo s2 rest in
        (CMatch cE [CCase cPat Nothing cRest], s3)

      -- Expression statement (discard result)
      when Cons (DoExpr e) rest ->
        let (cE, s1) = desugarExpr state e in
        let (cRest, s2) = desugarDo s1 rest in
        let lambda = CAbs (Name 0 "_") cRest in
        let flatMapApp = CApp (CApp (CVar (Name 0 "flatMap")) lambda) cE in
        (flatMapApp, s2)

      -- Empty do block (shouldn't happen in valid programs)
      when Nil -> (CTuple Nil, state)
    end

  -- ===========================================================================
  -- Pattern Desugaring
  -- D_pat⟦_⟧ : Surface.Pattern -> Core.Pattern
  -- ===========================================================================

  and desugarPattern state pat = match pat
    when SPWild -> (CPWild, state)
    when SPVar x -> (CPVar (Name 0 x), state)
    when SPLit l -> (CPLit l, state)
    when SPCon c pats ->
      let (cPats, s1) = desugarPatterns state pats in
      (CPCon c cPats, s1)
    when SPTuple pats ->
      let (cPats, s1) = desugarPatterns state pats in
      (CPTuple cPats, s1)
    when SPRecord fields ->
      let (cFields, s1) = desugarPatternFields state fields in
      (CPRecord cFields, s1)
    when SPAs x p ->
      let (cP, s1) = desugarPattern state p in
      (CPAs (Name 0 x) cP, s1)
    when SPOr p1 p2 ->
      let (cP1, s1) = desugarPattern state p1 in
      let (cP2, s2) = desugarPattern s1 p2 in
      (CPOr cP1 cP2, s2)
    -- Cons pattern -> Cons constructor pattern
    when SPCons p1 p2 ->
      let (cP1, s1) = desugarPattern state p1 in
      let (cP2, s2) = desugarPattern s1 p2 in
      (CPCon "Cons" [cP1, cP2], s2)
    -- List pattern -> nested Cons patterns
    when SPList pats ->
      desugarListPattern state pats
  end

  and desugarListPattern state pats =
    match pats
      when Nil -> (CPCon "Nil" Nil, state)
      when Cons p rest ->
        let (cP, s1) = desugarPattern state p in
        let (cRest, s2) = desugarListPattern s1 rest in
        (CPCon "Cons" [cP, cRest], s2)
    end

  -- ===========================================================================
  -- Helper Functions
  -- ===========================================================================

  and desugarExprs state exprs =
    match exprs
      when Nil -> (Nil, state)
      when Cons e rest ->
        let (cE, s1) = desugarExpr state e in
        let (cRest, s2) = desugarExprs s1 rest in
        (Cons cE cRest, s2)
    end

  and desugarBindings state bindings =
    match bindings
      when Nil -> (Nil, state)
      when Cons (SBinding name expr) rest ->
        let (cExpr, s1) = desugarExpr state expr in
        let (cRest, s2) = desugarBindings s1 rest in
        (Cons (CBinding (Name 0 name) cExpr) cRest, s2)
    end

  and desugarFields state fields =
    match fields
      when Nil -> (Nil, state)
      when Cons (SField name expr) rest ->
        let (cExpr, s1) = desugarExpr state expr in
        let (cRest, s2) = desugarFields s1 rest in
        (Cons (CField name cExpr) cRest, s2)
    end

  and desugarCases state cases =
    match cases
      when Nil -> (Nil, state)
      when Cons (SCase pat guard body) rest ->
        let (cPat, s1) = desugarPattern state pat in
        let (cGuard, s2) = desugarMaybeExpr s1 guard in
        let (cBody, s3) = desugarExpr s2 body in
        let (cRest, s4) = desugarCases s3 rest in
        (Cons (CCase cPat cGuard cBody) cRest, s4)
    end

  and desugarMaybeExpr state maybeExpr =
    match maybeExpr
      when Nothing -> (Nothing, state)
      when Just e ->
        let (cE, s1) = desugarExpr state e in
        (Just cE, s1)
    end

  and desugarPatterns state pats =
    match pats
      when Nil -> (Nil, state)
      when Cons p rest ->
        let (cP, s1) = desugarPattern state p in
        let (cRest, s2) = desugarPatterns s1 rest in
        (Cons cP cRest, s2)
    end

  and desugarPatternFields state fields =
    match fields
      when Nil -> (Nil, state)
      when Cons (SPatternField name pat) rest ->
        let (cPat, s1) = desugarPattern state pat in
        let (cRest, s2) = desugarPatternFields s1 rest in
        (Cons (CPatternField name cPat) cRest, s2)
    end

  -- ===========================================================================
  -- Type Desugaring (for annotations)
  -- ===========================================================================

  let rec desugarType ty = match ty
    when STVar x -> CTVar x
    when STCon c -> CTCon c
    when STApp t1 t2 -> CTApp (desugarType t1) (desugarType t2)
    when STFun t1 t2 -> CTFun (desugarType t1) (desugarType t2)
    when STTuple tys -> CTTuple (List.map desugarType tys)
    when STRecord fields ->
      CTRecord (List.map (f -> match f when STypeField name ty -> CTypeField name (desugarType ty) end) fields)
  end

  -- ===========================================================================
  -- Declaration Desugaring
  -- ===========================================================================

  let rec desugarDecl state decl = match decl
    when SDeclType name params cons ->
      let cCons = List.map (c -> match c
        when SConDecl cname tys -> CConDecl cname (List.map desugarType tys)
      end) cons in
      (CDeclType name params cCons, state)

    when SDeclLet name expr ->
      let (cExpr, s1) = desugarExpr state expr in
      (CDeclLet (Name 0 name) cExpr, s1)

    when SDeclLetRec bindings ->
      let (cBindings, s1) = desugarBindings state bindings in
      (CDeclLetRec cBindings, s1)

    when SDeclForeign name ty ->
      -- Foreign declarations need module and JS name
      -- For now, assume same name in "foreign" module
      (CDeclForeign (Name 0 name) "foreign" name (desugarType ty), state)

    when SDeclModule name uses decls ->
      -- Modules are flattened during desugaring
      let (cDecls, s1) = desugarDecls state decls in
      -- Module is kept for name resolution purposes
      -- The inner declarations are desugared but kept in module structure
      -- This will be handled by the name resolver
      (CDeclType name Nil Nil, s1)  -- Placeholder, modules handled specially
  end

  and desugarDecls state decls =
    match decls
      when Nil -> (Nil, state)
      when Cons d rest ->
        let (cD, s1) = desugarDecl state d in
        let (cRest, s2) = desugarDecls s1 rest in
        (Cons cD cRest, s2)
    end

  -- ===========================================================================
  -- Program Desugaring
  -- ===========================================================================

  let desugarProgram program =
    match program
      when SProgram decls maybeExpr ->
        let (cDecls, s1) = desugarDecls initFresh decls in
        let (cExpr, _) = match maybeExpr
          when Nothing -> (Nothing, s1)
          when Just e ->
            let (ce, s2) = desugarExpr s1 e in
            (Just ce, s2)
        end in
        (cDecls, cExpr)
    end

  -- ===========================================================================
  -- Entry Point
  -- ===========================================================================

  let desugar expr =
    let (cExpr, _) = desugarExpr initFresh expr in
    cExpr

end
