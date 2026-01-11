-- =============================================================================
-- NAME RESOLUTION
-- =============================================================================
--
-- Assigns unique identifiers to all bindings and resolves variable references.
-- Transforms Core AST (string names) to Resolved AST (unique Names).
--
-- Reference: SPEC.md Section 7
--
-- =============================================================================

module Resolve

  use Common (..)
  use Desugar (
    CoreExpr, CorePattern, CoreCase, CoreType, CoreDecl, CoreConDecl, CoreProgram,
    CoreUseImport, CUseAll, CUseNames,
    -- Expression constructors
    CVar, CLit, CApp, CAbs, CLet, CLetRec, CMatch, CCon, CTuple,
    CRecord, CRecordUpdate, CField,
    -- Pattern constructors
    CPWild, CPVar, CPLit, CPCon, CPTuple, CPRecord, CPAs, CPOr,
    -- Type constructors
    CTVar, CTCon, CTApp, CTFun, CTTuple, CTRecord,
    -- Declaration constructors
    CDeclType, CDeclLet, CDeclLetRec, CDeclForeign, CDeclUse, CDeclModule
  )

  -- =========================================================================
  -- NAME TYPE
  -- =========================================================================
  --
  -- Names have unique IDs for unambiguous reference and original strings
  -- for error messages and debugging.
  --
  -- =========================================================================

  type Name = { id : int, original : string }

  -- Create a name with given ID and original string
  let mkName = id original -> { id = id, original = original }

  -- =========================================================================
  -- RESOLVED AST
  -- =========================================================================
  --
  -- Same structure as Core AST, but with Name instead of string for bindings.
  --
  -- =========================================================================

  -- Resolved expressions
  type RExpr
    = RVar Name
    | RLit Literal
    | RApp RExpr RExpr
    | RAbs Name RExpr
    | RLet Name RExpr RExpr
    | RLetRec (List (Name, RExpr)) RExpr
    | RMatch RExpr (List RCase)
    | RCon string                          -- Constructors keep string names
    | RTuple (List RExpr)
    | RRecord (List (string, RExpr))       -- Field names stay as strings
    | RRecordUpdate RExpr (List (string, RExpr))
    | RField RExpr string

  -- Resolved patterns
  type RPattern
    = RPWild
    | RPVar Name
    | RPLit Literal
    | RPCon string (List RPattern)         -- Constructor names stay as strings
    | RPTuple (List RPattern)
    | RPRecord (List (string, RPattern))   -- Field names stay as strings
    | RPAs Name RPattern
    | RPOr RPattern RPattern

  -- Resolved case
  type RCase = { pattern : RPattern, guard : Maybe RExpr, body : RExpr }

  -- Resolved declarations
  type RDecl
    = RDeclType string (List string) (List CoreConDecl)  -- Type decls unchanged
    | RDeclLet Name RExpr
    | RDeclLetRec (List (Name, RExpr))
    | RDeclForeign Name CoreType           -- Foreign keeps type as-is
    | RDeclModule string (List RDecl)

  -- Resolved program
  type RProgram = { decls : List RDecl, expr : Maybe RExpr }

  -- =========================================================================
  -- ENVIRONMENT
  -- =========================================================================
  --
  -- Maps original names (strings) to resolved Names.
  -- Implemented as association list for simplicity.
  --
  -- =========================================================================

  type Env = List (string, Name)

  let emptyEnv = Nil

  -- Look up a name in the environment
  let rec lookup = key env -> match env
    when Nil -> Nothing
    when Cons (k, v) rest ->
      if k == key then Just v
      else lookup key rest
  end

  -- Extend environment with a new binding
  let extend = key value env -> Cons (key, value) env

  -- Extend environment with multiple bindings
  let rec extendMany = bindings env -> match bindings
    when Nil -> env
    when Cons (k, v) rest -> extendMany rest (extend k v env)
  end

  -- =========================================================================
  -- RESOLUTION STATE
  -- =========================================================================
  --
  -- Thread fresh ID counter and environment through resolution.
  --
  -- =========================================================================

  type ResolveState = {
    nextId : int,
    env : Env,
    errors : List string,
    importedNames : List (string, string)  -- (name, module) for duplicate detection
  }

  let initState = env -> { nextId = 0, env = env, errors = Nil, importedNames = Nil }

  -- Generate a fresh name
  let freshName = original state ->
    let name = mkName state.nextId original in
    let newState = { state | nextId = state.nextId + 1 } in
    (name, newState)

  -- Add an error
  let addError = msg state ->
    { state | errors = Cons msg state.errors }

  -- Extend state's environment (for let bindings - allows shadowing)
  let extendEnv = key value state ->
    { state | env = extend key value state.env }

  -- Check if a name was already imported
  let findImportSource = name imports -> match imports
    when Nil -> Nothing
    when Cons (n, mod) rest ->
      if n == name then Just mod
      else findImportSource name rest
  end

  -- Import a name with duplicate detection
  -- Returns error if name was already imported from another module
  let importName = name modName state ->
    match findImportSource name state.importedNames
      when Just prevMod ->
        -- Already imported - report error
        let msg = String.concat "'" (String.concat name (String.concat "' is already imported from '" (String.concat prevMod "'"))) in
        addError msg state
      when Nothing ->
        -- Not imported yet - add to env and track
        let (resolved, st1) = freshName name state in
        let st2 = extendEnv name resolved st1 in
        { st2 | importedNames = Cons (name, modName) st2.importedNames }
    end

  -- =========================================================================
  -- EXPRESSION RESOLUTION
  -- =========================================================================

  let rec resolveExpr = expr state -> match expr
    -- Variable: look up in environment
    when CVar x ->
      match lookup x state.env
        when Just name -> (RVar name, state)
        when Nothing ->
          -- Unbound variable - create placeholder and report error
          let (name, st1) = freshName x state in
          let st2 = addError (String.concat "unbound variable: " x) st1 in
          (RVar name, st2)
      end

    -- Literal: unchanged
    when CLit l -> (RLit l, state)

    -- Application
    when CApp e1 e2 ->
      let (r1, st1) = resolveExpr e1 state in
      let (r2, st2) = resolveExpr e2 st1 in
      (RApp r1 r2, st2)

    -- Lambda: bind parameter
    when CAbs x body ->
      let (name, st1) = freshName x state in
      let st2 = extendEnv x name st1 in
      let (rbody, st3) = resolveExpr body st2 in
      -- Restore original environment (lexical scoping)
      let st4 = { st3 | env = state.env } in
      (RAbs name rbody, st4)

    -- Let: bind name in body
    when CLet x e1 e2 ->
      let (r1, st1) = resolveExpr e1 state in
      let (name, st2) = freshName x st1 in
      let st3 = extendEnv x name st2 in
      let (r2, st4) = resolveExpr e2 st3 in
      let st5 = { st4 | env = state.env } in
      (RLet name r1 r2, st5)

    -- Let rec: bind all names first (for mutual recursion)
    when CLetRec bindings body ->
      let (st1, names) = bindRecNames bindings state in
      let (rbindings, st2) = resolveBindings bindings names st1 in
      let (rbody, st3) = resolveExpr body st2 in
      let st4 = { st3 | env = state.env } in
      (RLetRec rbindings rbody, st4)

    -- Match
    when CMatch scrutinee cases ->
      let (rscrutinee, st1) = resolveExpr scrutinee state in
      let (rcases, st2) = resolveCases cases st1 in
      (RMatch rscrutinee rcases, st2)

    -- Constructor: unchanged (constructors are global)
    when CCon c -> (RCon c, state)

    -- Tuple
    when CTuple elems ->
      let (relems, st1) = resolveExprs elems state in
      (RTuple relems, st1)

    -- Record
    when CRecord fields ->
      let (rfields, st1) = resolveFields fields state in
      (RRecord rfields, st1)

    -- Record update
    when CRecordUpdate base fields ->
      let (rbase, st1) = resolveExpr base state in
      let (rfields, st2) = resolveFields fields st1 in
      (RRecordUpdate rbase rfields, st2)

    -- Field access
    when CField e field ->
      let (re, st1) = resolveExpr e state in
      (RField re field, st1)
  end

  -- Resolve a list of expressions
  and resolveExprs = exprs state -> match exprs
    when Nil -> (Nil, state)
    when Cons e rest ->
      let (re, st1) = resolveExpr e state in
      let (rrest, st2) = resolveExprs rest st1 in
      (Cons re rrest, st2)
  end

  -- Resolve record fields
  and resolveFields = fields state -> match fields
    when Nil -> (Nil, state)
    when Cons (name, e) rest ->
      let (re, st1) = resolveExpr e state in
      let (rrest, st2) = resolveFields rest st1 in
      (Cons (name, re) rrest, st2)
  end

  -- Bind all names in a recursive binding group
  and bindRecNames = bindings state -> match bindings
    when Nil -> (state, Nil)
    when Cons (x, _) rest ->
      let (name, st1) = freshName x state in
      let st2 = extendEnv x name st1 in
      let (st3, names) = bindRecNames rest st2 in
      (st3, Cons name names)
  end

  -- Resolve recursive bindings (names already bound)
  and resolveBindings = bindings names state -> match (bindings, names)
    when (Nil, _) -> (Nil, state)
    when (_, Nil) -> (Nil, state)
    when (Cons (_, e) restB, Cons name restN) ->
      let (re, st1) = resolveExpr e state in
      let (rrest, st2) = resolveBindings restB restN st1 in
      (Cons (name, re) rrest, st2)
  end

  -- =========================================================================
  -- CASE RESOLUTION
  -- =========================================================================

  and resolveCases = cases state -> match cases
    when Nil -> (Nil, state)
    when Cons c rest ->
      let (rc, st1) = resolveCase c state in
      let (rrest, st2) = resolveCases rest st1 in
      (Cons rc rrest, st2)
  end

  and resolveCase = c state ->
    -- Pattern bindings are scoped to guard and body
    let (rpat, st1, patEnv) = resolvePattern c.pattern state in
    let st2 = { st1 | env = extendMany patEnv state.env } in
    let (rguard, st3) = match c.guard
      when Nothing -> (Nothing, st2)
      when Just g ->
        let (rg, st) = resolveExpr g st2 in
        (Just rg, st)
    end in
    let (rbody, st4) = resolveExpr c.body st3 in
    -- Restore original environment
    let st5 = { st4 | env = state.env } in
    ({ pattern = rpat, guard = rguard, body = rbody }, st5)

  -- =========================================================================
  -- PATTERN RESOLUTION
  -- =========================================================================
  --
  -- Returns resolved pattern, updated state, and list of bindings introduced.
  --
  -- =========================================================================

  and resolvePattern = pat state -> match pat
    when CPWild -> (RPWild, state, Nil)

    when CPVar x ->
      let (name, st1) = freshName x state in
      (RPVar name, st1, [(x, name)])

    when CPLit l -> (RPLit l, state, Nil)

    when CPCon c pats ->
      let (rpats, st1, bindings) = resolvePatterns pats state in
      (RPCon c rpats, st1, bindings)

    when CPTuple pats ->
      let (rpats, st1, bindings) = resolvePatterns pats state in
      (RPTuple rpats, st1, bindings)

    when CPRecord fields ->
      let (rfields, st1, bindings) = resolvePatternFields fields state in
      (RPRecord rfields, st1, bindings)

    when CPAs x p ->
      let (name, st1) = freshName x state in
      let (rp, st2, innerBindings) = resolvePattern p st1 in
      (RPAs name rp, st2, Cons (x, name) innerBindings)

    when CPOr p1 p2 ->
      -- Both branches must bind same names (checked elsewhere)
      let (rp1, st1, bindings1) = resolvePattern p1 state in
      let (rp2, st2, _) = resolvePattern p2 st1 in
      (RPOr rp1 rp2, st2, bindings1)
  end

  and resolvePatterns = pats state -> match pats
    when Nil -> (Nil, state, Nil)
    when Cons p rest ->
      let (rp, st1, bindings1) = resolvePattern p state in
      let (rrest, st2, bindings2) = resolvePatterns rest st1 in
      (Cons rp rrest, st2, List.concat bindings1 bindings2)
  end

  and resolvePatternFields = fields state -> match fields
    when Nil -> (Nil, state, Nil)
    when Cons (name, p) rest ->
      let (rp, st1, bindings1) = resolvePattern p state in
      let (rrest, st2, bindings2) = resolvePatternFields rest st1 in
      (Cons (name, rp) rrest, st2, List.concat bindings1 bindings2)
  end

  -- =========================================================================
  -- DECLARATION RESOLUTION
  -- =========================================================================

  let rec resolveDecl = decl state -> match decl
    -- Type declarations: unchanged (no term-level names)
    when CDeclType name params cons ->
      (RDeclType name params cons, state)

    -- Let declaration
    when CDeclLet x expr ->
      let (rexpr, st1) = resolveExpr expr state in
      let (name, st2) = freshName x st1 in
      let st3 = extendEnv x name st2 in
      (RDeclLet name rexpr, st3)

    -- Let rec declaration
    when CDeclLetRec bindings ->
      let (st1, names) = bindRecNames bindings state in
      let (rbindings, st2) = resolveBindings bindings names st1 in
      (RDeclLetRec rbindings, st2)

    -- Foreign declaration
    when CDeclForeign x typ ->
      let (name, st1) = freshName x state in
      let st2 = extendEnv x name st1 in
      (RDeclForeign name typ, st2)

    -- Use declaration: import names with duplicate checking
    when CDeclUse modName import ->
      let st1 = match import
        when CUseAll ->
          -- Import all from module - needs module registry to know what names to import
          -- For now, UseAll doesn't add names (module system not fully implemented)
          state
        when CUseNames names ->
          -- Import specific names with duplicate checking
          importNames names modName state
      end in
      -- Use declarations don't produce resolved declarations
      -- Return a dummy type declaration that will be filtered
      (RDeclType "_use" Nil Nil, st1)

    -- Module declaration
    when CDeclModule name decls ->
      let (rdecls, st1) = resolveDecls decls state in
      (RDeclModule name rdecls, st1)
  end

  -- Import multiple names from a module
  and importNames = names modName state -> match names
    when Nil -> state
    when Cons name rest ->
      let st1 = importName name modName state in
      importNames rest modName st1
  end

  and resolveDecls = decls state -> match decls
    when Nil -> (Nil, state)
    when Cons d rest ->
      let (rd, st1) = resolveDecl d state in
      let (rrest, st2) = resolveDecls rest st1 in
      (Cons rd rrest, st2)
  end

  -- =========================================================================
  -- PROGRAM RESOLUTION
  -- =========================================================================

  -- Filter out dummy use declarations
  let filterUseDecls = decls ->
    List.filter (d -> match d
      when RDeclType "_use" _ _ -> false
      when _ -> true
    end) decls

  let resolveProgram = program initialEnv ->
    let state = initState initialEnv in
    let (rdecls, st1) = resolveDecls program.decls state in
    let (rexpr, st2) = match program.expr
      when Nothing -> (Nothing, st1)
      when Just e ->
        let (re, st) = resolveExpr e st1 in
        (Just re, st)
    end in
    ({ decls = filterUseDecls rdecls, expr = rexpr }, st2.errors)

end
