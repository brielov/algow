-- =============================================================================
-- DESUGARING
-- =============================================================================
--
-- Transforms Surface AST to Core AST via structural recursion.
-- All syntactic sugar is removed, producing a minimal canonical form.
--
-- Reference: SPEC.md Section 6
--
-- =============================================================================

module Desugar

  use Common (..)
  use Parser (
    Expr, Pattern, Case, DoStmt, Type, ConDecl, Decl, Program, UseImport,
    -- Expression constructors
    EVar, ELit, EApp, EAbs, ELet, ELetRec, EIf, EMatch, ECon, ETuple,
    ERecord, ERecordUpdate, EField, EList, EPipe, ECons, EBinOp, EDo, EAnnot,
    -- Pattern constructors
    PWild, PVar, PLit, PCon, PTuple, PRecord, PAs, POr, PCons, PList,
    -- DoStmt constructors
    DoBindPattern, DoLet, DoExpr,
    -- Decl constructors
    DeclType, DeclLet, DeclLetRec, DeclForeign, DeclUse, DeclModule
  )

  -- =========================================================================
  -- CORE AST TYPES
  -- =========================================================================
  --
  -- The Core language is minimal: single-param lambdas, no syntactic sugar.
  -- Names are still strings here; unique IDs are assigned during name resolution.
  --
  -- =========================================================================

  -- Core expressions
  type CoreExpr
    = CVar string
    | CLit Literal
    | CApp CoreExpr CoreExpr
    | CAbs string CoreExpr                    -- Single parameter only
    | CLet string CoreExpr CoreExpr
    | CLetRec (List (string, CoreExpr)) CoreExpr
    | CMatch CoreExpr (List CoreCase)
    | CCon string
    | CTuple (List CoreExpr)
    | CRecord (List (string, CoreExpr))
    | CRecordUpdate CoreExpr (List (string, CoreExpr))
    | CField CoreExpr string

  -- Core patterns
  type CorePattern
    = CPWild
    | CPVar string
    | CPLit Literal
    | CPCon string (List CorePattern)
    | CPTuple (List CorePattern)
    | CPRecord (List (string, CorePattern))
    | CPAs string CorePattern
    | CPOr CorePattern CorePattern

  -- Core case
  type CoreCase = { pattern : CorePattern, guard : Maybe CoreExpr, body : CoreExpr }

  -- Core types (for foreign declarations)
  type CoreType
    = CTVar string
    | CTCon string
    | CTApp CoreType CoreType
    | CTFun CoreType CoreType
    | CTTuple (List CoreType)
    | CTRecord (List (string, CoreType))

  -- Core use import
  type CoreUseImport
    = CUseAll                         -- use Module (..)
    | CUseNames (List string)         -- use Module (x, y, z)

  -- Core declarations
  type CoreDecl
    = CDeclType string (List string) (List CoreConDecl)
    | CDeclLet string CoreExpr
    | CDeclLetRec (List (string, CoreExpr))
    | CDeclForeign string CoreType
    | CDeclUse string CoreUseImport   -- use Module (imports)
    | CDeclModule string (List CoreDecl)

  type CoreConDecl = { name : string, fields : List CoreType }

  -- Core program
  type CoreProgram = { decls : List CoreDecl, expr : Maybe CoreExpr }

  -- =========================================================================
  -- FRESH NAME GENERATION
  -- =========================================================================
  --
  -- For do-notation desugaring, we need fresh variable names.
  -- Simple counter-based approach.
  --
  -- =========================================================================

  type FreshState = int

  let freshName = prefix n -> (String.concat prefix (Int.toString n), n + 1)

  -- =========================================================================
  -- EXPRESSION DESUGARING
  -- =========================================================================
  --
  -- D⟦_⟧ : Surface.Expr -> Core.Expr
  --
  -- =========================================================================

  let rec desugarExpr = expr -> match expr
    -- Direct translations
    when EVar x -> CVar x
    when ELit l -> CLit l
    when ECon c -> CCon c

    -- Application
    when EApp e1 e2 -> CApp (desugarExpr e1) (desugarExpr e2)

    -- Multi-param lambda → nested single-param lambdas
    -- D⟦ SAbs [] e ⟧ = D⟦e⟧
    -- D⟦ SAbs (x:xs) e ⟧ = CAbs x (D⟦ SAbs xs e ⟧)
    when EAbs params body -> desugarAbs params body

    -- Let and let rec
    when ELet x e1 e2 -> CLet x (desugarExpr e1) (desugarExpr e2)
    when ELetRec bindings body ->
      CLetRec (List.map desugarBinding bindings) (desugarExpr body)

    -- If → match on boolean
    -- D⟦ SIf c t f ⟧ = CMatch (D⟦c⟧) [Case true t, Case false f]
    when EIf cond thenBr elseBr ->
      CMatch (desugarExpr cond) [
        { pattern = CPLit (LBool true), guard = Nothing, body = desugarExpr thenBr },
        { pattern = CPLit (LBool false), guard = Nothing, body = desugarExpr elseBr }
      ]

    -- Match
    when EMatch scrutinee cases ->
      CMatch (desugarExpr scrutinee) (List.map desugarCase cases)

    -- Tuple
    when ETuple elems -> CTuple (List.map desugarExpr elems)

    -- Record
    when ERecord fields ->
      CRecord (List.map desugarField fields)

    -- Record update
    when ERecordUpdate base fields ->
      CRecordUpdate (desugarExpr base) (List.map desugarField fields)

    -- Field access
    when EField e field -> CField (desugarExpr e) field

    -- List literal → nested Cons
    -- D⟦ SList [] ⟧ = CCon "Nil"
    -- D⟦ SList (e:es) ⟧ = CApp (CApp (CCon "Cons") (D⟦e⟧)) (D⟦ SList es ⟧)
    when EList elems -> desugarList elems

    -- Pipe → application (flip)
    -- D⟦ SPipe e1 e2 ⟧ = CApp (D⟦e2⟧) (D⟦e1⟧)
    when EPipe e1 e2 -> CApp (desugarExpr e2) (desugarExpr e1)

    -- Cons → Cons constructor application
    -- D⟦ SCons e1 e2 ⟧ = CApp (CApp (CCon "Cons") (D⟦e1⟧)) (D⟦e2⟧)
    when ECons e1 e2 ->
      CApp (CApp (CCon "Cons") (desugarExpr e1)) (desugarExpr e2)

    -- Binary operators → function application
    -- D⟦ SBinOp op e1 e2 ⟧ = CApp (CApp (CVar op) (D⟦e1⟧)) (D⟦e2⟧)
    -- Special cases for && and || (short-circuit)
    when EBinOp op e1 e2 -> desugarBinOp op e1 e2

    -- Do-notation → flatMap
    when EDo stmts -> desugarDo stmts 0 |> fst

    -- Type annotation is erased
    -- D⟦ SAnnot e t ⟧ = D⟦e⟧
    when EAnnot e _ -> desugarExpr e
  end

  -- Desugar multi-param lambda to nested single-param lambdas
  and desugarAbs = params body -> match params
    when Nil -> desugarExpr body
    when Cons x rest -> CAbs x (desugarAbs rest body)
  end

  -- Desugar a binding pair
  and desugarBinding = pair -> match pair
    when (name, expr) -> (name, desugarExpr expr)
  end

  -- Desugar a field pair
  and desugarField = pair -> match pair
    when (name, expr) -> (name, desugarExpr expr)
  end

  -- Desugar list literal to nested Cons
  and desugarList = elems -> match elems
    when Nil -> CCon "Nil"
    when Cons e rest ->
      CApp (CApp (CCon "Cons") (desugarExpr e)) (desugarList rest)
  end

  -- Desugar binary operator
  -- && and || need special handling for short-circuit evaluation
  and desugarBinOp = op e1 e2 -> match op
    -- a && b → if a then b else false
    when "&&" ->
      CMatch (desugarExpr e1) [
        { pattern = CPLit (LBool true), guard = Nothing, body = desugarExpr e2 },
        { pattern = CPLit (LBool false), guard = Nothing, body = CLit (LBool false) }
      ]
    -- a || b → if a then true else b
    when "||" ->
      CMatch (desugarExpr e1) [
        { pattern = CPLit (LBool true), guard = Nothing, body = CLit (LBool true) },
        { pattern = CPLit (LBool false), guard = Nothing, body = desugarExpr e2 }
      ]
    -- Regular binary operators → function application
    when _ -> CApp (CApp (CVar op) (desugarExpr e1)) (desugarExpr e2)
  end

  -- =========================================================================
  -- DO-NOTATION DESUGARING
  -- =========================================================================
  --
  -- D_do⟦_⟧ : List Surface.DoStmt -> Core.Expr
  --
  -- Do-notation desugars to flatMap calls:
  --   do { x <- e; rest } → flatMap (λx. rest) e
  --   do { let x = e; rest } → let x = e in rest
  --   do { e; rest } → flatMap (λ_. rest) e
  --   do { e } → e
  --
  -- =========================================================================

  and desugarDo = stmts freshN -> match stmts
    -- Final expression
    when Cons (DoExpr e) Nil -> (desugarExpr e, freshN)

    -- Bind with pattern: p <- e; rest
    -- → flatMap (λ$tmp -> match $tmp when p -> rest) e
    when Cons (DoBindPattern pat e) rest ->
      let (tmp, freshN2) = freshName "$do" freshN in
      let (restExpr, freshN3) = desugarDo rest freshN2 in
      let body = match pat
        -- Simple variable: no need for intermediate match
        when PVar x -> CAbs x restExpr
        -- Complex pattern: need match
        when _ ->
          CAbs tmp (CMatch (CVar tmp) [
            { pattern = desugarPattern pat, guard = Nothing, body = restExpr }
          ])
      end in
      (CApp (CApp (CVar "flatMap") body) (desugarExpr e), freshN3)

    -- Let with pattern: let p = e; rest
    -- Simple var → CLet, complex pattern → match
    when Cons (DoLet pat e) rest ->
      let (restExpr, freshN2) = desugarDo rest freshN in
      let result = match pat
        when PVar x -> CLet x (desugarExpr e) restExpr
        when _ ->
          CMatch (desugarExpr e) [
            { pattern = desugarPattern pat, guard = Nothing, body = restExpr }
          ]
      end in
      (result, freshN2)

    -- Expression statement (discard result): e; rest
    -- → flatMap (λ_. rest) e
    when Cons (DoExpr e) rest ->
      let (restExpr, freshN2) = desugarDo rest freshN in
      let body = CAbs "_" restExpr in
      (CApp (CApp (CVar "flatMap") body) (desugarExpr e), freshN2)

    -- Empty (shouldn't happen in valid code)
    when Nil -> (CCon "Unit", freshN)
  end

  -- =========================================================================
  -- PATTERN DESUGARING
  -- =========================================================================
  --
  -- D_pat⟦_⟧ : Surface.Pattern -> Core.Pattern
  --
  -- =========================================================================

  and desugarPattern = pat -> match pat
    when PWild -> CPWild
    when PVar x -> CPVar x
    when PLit l -> CPLit l
    when PCon c pats -> CPCon c (List.map desugarPattern pats)
    when PTuple pats -> CPTuple (List.map desugarPattern pats)
    when PRecord fields -> CPRecord (List.map desugarPatternField fields)
    when PAs x p -> CPAs x (desugarPattern p)
    when POr p1 p2 -> CPOr (desugarPattern p1) (desugarPattern p2)

    -- Cons pattern → CPCon "Cons" [p1, p2]
    when PCons p1 p2 ->
      CPCon "Cons" [desugarPattern p1, desugarPattern p2]

    -- List pattern → nested Cons pattern
    when PList pats -> desugarListPattern pats
  end

  and desugarPatternField = pair -> match pair
    when (name, pat) -> (name, desugarPattern pat)
  end

  -- Desugar list pattern to nested Cons
  -- D_pat⟦ SPList [] ⟧ = CPCon "Nil" []
  -- D_pat⟦ SPList (p:ps) ⟧ = CPCon "Cons" [D_pat⟦p⟧, D_pat⟦ SPList ps ⟧]
  and desugarListPattern = pats -> match pats
    when Nil -> CPCon "Nil" Nil
    when Cons p rest ->
      CPCon "Cons" [desugarPattern p, desugarListPattern rest]
  end

  -- =========================================================================
  -- CASE DESUGARING
  -- =========================================================================

  and desugarCase = c ->
    { pattern = desugarPattern c.pattern
    , guard = Maybe.map desugarExpr c.guard
    , body = desugarExpr c.body
    }

  -- =========================================================================
  -- TYPE DESUGARING
  -- =========================================================================

  let rec desugarType = t -> match t
    when TVar x -> CTVar x
    when TCon c -> CTCon c
    when TApp t1 t2 -> CTApp (desugarType t1) (desugarType t2)
    when TFun t1 t2 -> CTFun (desugarType t1) (desugarType t2)
    when TTuple ts -> CTTuple (List.map desugarType ts)
    when TRecord fields -> CTRecord (List.map desugarTypeField fields)
  end

  and desugarTypeField = pair -> match pair
    when (name, t) -> (name, desugarType t)
  end

  -- =========================================================================
  -- DECLARATION DESUGARING
  -- =========================================================================

  let rec desugarDecl = decl -> match decl
    when DeclType name params cons ->
      CDeclType name params (List.map desugarConDecl cons)

    when DeclLet name expr ->
      CDeclLet name (desugarExpr expr)

    when DeclLetRec bindings ->
      CDeclLetRec (List.map desugarBinding bindings)

    when DeclForeign name typ ->
      CDeclForeign name (desugarType typ)

    when DeclUse modName import ->
      -- Preserve use declarations for name resolution
      let coreImport = match import
        when UseAll -> CUseAll
        when UseNames names -> CUseNames names
      end in
      CDeclUse modName coreImport

    when DeclModule name decls ->
      CDeclModule name (List.map desugarDecl decls)
  end

  and desugarConDecl = con ->
    { name = con.name, fields = List.map desugarType con.fields }

  -- =========================================================================
  -- PROGRAM DESUGARING
  -- =========================================================================

  let desugarProgram = program ->
    let decls = List.map desugarDecl program.decls in
    let expr = Maybe.map desugarExpr program.expr in
    { decls = decls, expr = expr }

end
