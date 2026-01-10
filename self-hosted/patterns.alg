-- =============================================================================
-- Patterns Module
-- Pattern compilation to decision trees
-- Reference: SPEC.md Section 9, Maranget (2008)
-- =============================================================================

module Patterns
  use Types (..)

  -- Helper: flatMap is not in prelude, so define it
  let flatMap f xs = List.concat (List.map f xs)

  -- ===========================================================================
  -- Decision Trees
  -- ===========================================================================

  -- Decision tree representation
  type Decision
    = DFail                                    -- Match failure
    | DLeaf CExpr                              -- Success, evaluate expression
    | DSwitch Name (List Branch) (Maybe Decision)  -- Switch on constructor

  -- Branch: constructor name -> subtree
  type Branch = Branch string (List Name) Decision

  -- ===========================================================================
  -- Pattern Matrix
  -- ===========================================================================

  -- A row in the pattern matrix
  type PatternRow = PatternRow (List CPattern) CExpr

  -- The pattern matrix
  type PatternMatrix = PatternMatrix (List PatternRow)

  -- ===========================================================================
  -- Compilation Context
  -- ===========================================================================

  type CompileState = CompileState int

  let initCompileState = CompileState 0

  let freshName state prefix =
    match state
      when CompileState n ->
        (Name n (prefix + Int.toString n), CompileState (n + 1))
    end

  -- ===========================================================================
  -- Pattern Analysis
  -- ===========================================================================

  -- Check if pattern is a wildcard or variable (catches anything)
  let isWildcard pat = match pat
    when CPWild -> true
    when CPVar _ -> true
    when _ -> false
  end

  -- Check if first column is all wildcards
  let rec allWildcards matrix = match matrix
    when PatternMatrix Nil -> true
    when PatternMatrix (Cons (PatternRow pats _) rest) ->
      match pats
        when Nil -> allWildcards (PatternMatrix rest)
        when Cons p _ ->
          if isWildcard p
          then allWildcards (PatternMatrix rest)
          else false
      end
  end

  -- Get the head constructor of a pattern (if any)
  let headConstructor pat = match pat
    when CPCon name _ -> Just name
    when CPLit l -> Just (litTag l)
    when CPTuple _ -> Just "$tuple"
    when CPRecord _ -> Just "$record"
    when _ -> Nothing
  end

  -- Convert literal to a tag string for matching
  let litTag l = match l
    when LInt n -> "$int:" + Int.toString n
    when LFloat f -> "$float:" + Float.toString f
    when LString s -> "$string:" + s
    when LChar c -> "$char:" + Char.toString c
    when LBool b -> if b then "$true" else "$false"
  end

  -- ===========================================================================
  -- Specialization
  -- ===========================================================================

  -- Specialize the matrix for constructor c
  -- Filter rows that match c and expand patterns
  let specialize c arity matrix =
    match matrix
      when PatternMatrix rows ->
        PatternMatrix (flatMap (specializeRow c arity) rows)
    end

  let specializeRow c arity row = match row
    when PatternRow pats body ->
      match pats
        when Nil -> []
        when Cons p rest ->
          if matchesConstructor c p
          then [PatternRow (List.append (expandPattern p arity) rest) body]
          else []
      end
  end

  -- Check if pattern matches the given constructor
  let matchesConstructor c pat = match pat
    when CPWild -> true
    when CPVar _ -> true
    when CPCon name _ -> name == c
    when CPLit l -> litTag l == c
    when CPTuple _ -> c == "$tuple"
    when CPRecord _ -> c == "$record"
    when CPAs _ p -> matchesConstructor c p
    when CPOr p1 p2 -> matchesConstructor c p1 || matchesConstructor c p2
  end

  -- Expand a pattern to its subpatterns (for specialization)
  let expandPattern pat arity = match pat
    when CPWild -> List.map (n -> CPWild) (range 0 arity)
    when CPVar _ -> List.map (n -> CPWild) (range 0 arity)
    when CPCon _ pats -> pats
    when CPLit _ -> []
    when CPTuple pats -> pats
    when CPRecord fields -> List.map (f -> match f when CPatternField _ p -> p end) fields
    when CPAs _ p -> expandPattern p arity
    when CPOr p1 _ -> expandPattern p1 arity
  end

  -- Generate a range [start, end)
  let rec range start stop =
    if start >= stop then Nil
    else Cons start (range (start + 1) stop)

  -- ===========================================================================
  -- Default Matrix
  -- ===========================================================================

  -- Default matrix: rows where first pattern is a wildcard
  let defaultMatrix matrix =
    match matrix
      when PatternMatrix rows ->
        PatternMatrix (flatMap defaultRow rows)
    end

  let defaultRow row = match row
    when PatternRow pats body ->
      match pats
        when Nil -> []
        when Cons p rest ->
          if isWildcard p
          then [PatternRow rest body]
          else
            match p
              when CPAs _ inner ->
                if isWildcard inner
                then [PatternRow rest body]
                else []
              when _ -> []
            end
      end
  end

  -- ===========================================================================
  -- Constructor Collection
  -- ===========================================================================

  -- Collect all head constructors from first column
  let collectConstructors matrix =
    match matrix
      when PatternMatrix rows ->
        let heads = flatMap (r -> match r
          when PatternRow pats _ ->
            match pats
              when Nil -> []
              when Cons p _ ->
                match headConstructor p
                  when Just c -> [c]
                  when Nothing -> []
                end
            end
        end) rows in
        unique heads
    end

  -- Remove duplicates from a list
  let rec unique xs = match xs
    when Nil -> Nil
    when Cons x rest ->
      if List.any (y -> y == x) rest
      then unique rest
      else Cons x (unique rest)
  end

  -- Get arity of constructor
  let constructorArity c pat = match pat
    when CPCon name pats ->
      if name == c then List.length pats else 0
    when CPTuple pats -> List.length pats
    when CPRecord fields -> List.length fields
    when _ -> 0
  end

  -- Find arity from matrix
  let findArity c matrix =
    match matrix
      when PatternMatrix rows ->
        let rec search rs = match rs
          when Nil -> 0
          when Cons (PatternRow pats _) rest ->
            match pats
              when Nil -> search rest
              when Cons p _ ->
                let a = constructorArity c p in
                if a > 0 then a else search rest
            end
        end
        in search rows
    end

  -- ===========================================================================
  -- Main Compilation Algorithm
  -- ===========================================================================

  -- Compile a match expression to a decision tree
  let compile scrutinee cases =
    let (tree, _) = compileMatrix initCompileState [scrutinee] (casesToMatrix cases) in
    tree

  -- Convert match cases to pattern matrix
  let casesToMatrix cases =
    PatternMatrix (List.map caseToRow cases)

  let caseToRow c = match c
    when CCase pat _ body -> PatternRow [pat] body
  end

  -- Main compilation function (Maranget algorithm)
  let rec compileMatrix state scrutinees matrix = match (scrutinees, matrix)
    -- No scrutinees left
    when (Nil, PatternMatrix rows) ->
      match rows
        when Nil -> (DFail, state)
        when Cons (PatternRow _ body) _ -> (DLeaf body, state)
      end

    -- Have scrutinees, check if first column is all wildcards
    when (Cons v rest, _) ->
      if allWildcards matrix then
        -- All wildcards: skip first column, bind variables
        let matrix2 = removeFirstColumn matrix in
        let (boundMatrix, state2) = bindFirstColumn v matrix state in
        compileMatrix state2 rest boundMatrix
      else
        -- Not all wildcards: switch on first scrutinee
        let constructors = collectConstructors matrix in
        let (branches, state2) = compileBranches state rest v matrix constructors in
        let defMatrix = defaultMatrix matrix in
        let (defaultTree, state3) = compileDefault state2 rest defMatrix in
        (DSwitch v branches defaultTree, state3)
  end

  -- Remove first column from matrix
  let removeFirstColumn matrix =
    match matrix
      when PatternMatrix rows ->
        PatternMatrix (List.map (r -> match r
          when PatternRow pats body ->
            match pats
              when Nil -> PatternRow Nil body
              when Cons _ rest -> PatternRow rest body
            end
        end) rows)
    end

  -- Bind variables from first column
  and bindFirstColumn scrutinee matrix state =
    match matrix
      when PatternMatrix rows ->
        let (newRows, newState) = List.foldl (acc row ->
          let (rs, s) = acc in
          let (newRow, s2) = bindRow scrutinee row s in
          (List.append rs [newRow], s2)
        ) (Nil, state) rows in
        (PatternMatrix newRows, newState)
    end

  and bindRow scrutinee row state = match row
    when PatternRow pats body ->
      match pats
        when Nil -> (PatternRow Nil body, state)
        when Cons p rest ->
          let body2 = bindPatternVars scrutinee p body in
          (PatternRow rest body2, state)
      end
  end

  -- Bind pattern variables in body
  and bindPatternVars scrutinee pat body = match pat
    when CPWild -> body
    when CPVar name -> CLet name (CVar scrutinee) body
    when CPAs name inner ->
      CLet name (CVar scrutinee) (bindPatternVars scrutinee inner body)
    when _ -> body
  end

  -- Compile branches for each constructor
  and compileBranches state restScrutinees scrutinee matrix constructors =
    match constructors
      when Nil -> (Nil, state)
      when Cons c rest ->
        let arity = findArity c matrix in
        let specializedMatrix = specialize c arity matrix in
        -- Create fresh names for constructor arguments
        let (argNames, state2) = freshNames state "arg" arity in
        let newScrutinees = List.append argNames restScrutinees in
        let (subtree, state3) = compileMatrix state2 newScrutinees specializedMatrix in
        let (restBranches, state4) = compileBranches state3 restScrutinees scrutinee matrix rest in
        (Cons (Branch c argNames subtree) restBranches, state4)
    end

  -- Generate fresh names
  and freshNames state prefix count =
    if count <= 0 then (Nil, state)
    else
      let (name, state2) = freshName state prefix in
      let (rest, state3) = freshNames state2 prefix (count - 1) in
      (Cons name rest, state3)

  -- Compile default case
  and compileDefault state scrutinees matrix =
    match matrix
      when PatternMatrix Nil -> (Nothing, state)
      when PatternMatrix _ ->
        let (tree, state2) = compileMatrix state scrutinees matrix in
        (Just tree, state2)
    end

  -- ===========================================================================
  -- Convert Decision Tree Back to Core Expression
  -- ===========================================================================

  -- Convert decision tree to Core match expressions
  let decisionToExpr scrutinee tree =
    match tree
      when DFail ->
        -- Should not happen if exhaustive, but emit error for safety
        CApp (CVar (Name 0 "error")) (CLit (LString "Pattern match failure"))

      when DLeaf body -> body

      when DSwitch var branches defaultCase ->
        -- Build match expression from branches
        let cases = List.map branchToCase branches in
        let allCases = match defaultCase
          when Nothing -> cases
          when Just defTree ->
            let defBody = decisionToExpr scrutinee defTree in
            List.append cases [CCase CPWild Nothing defBody]
        end in
        CMatch (CVar var) allCases
    end

  let branchToCase branch = match branch
    when Branch conName argNames subtree ->
      let pat = if String.head conName == Just '$' then
        -- Handle special cases (literals, tuples, records)
        parseSpecialPattern conName argNames
      else
        CPCon conName (List.map (n -> CPVar n) argNames)
      in
      CCase pat Nothing (decisionToExpr (Name 0 "scrutinee") subtree)
  end

  -- Parse special pattern tags back to patterns
  let parseSpecialPattern tag argNames =
    if String.startsWith "$int:" tag then
      let numStr = String.drop 5 tag in
      match Int.fromString numStr
        when Just n -> CPLit (LInt n)
        when Nothing -> CPWild
      end
    else if String.startsWith "$float:" tag then
      let numStr = String.drop 7 tag in
      match Float.fromString numStr
        when Just f -> CPLit (LFloat f)
        when Nothing -> CPWild
      end
    else if String.startsWith "$string:" tag then
      CPLit (LString (String.drop 8 tag))
    else if String.startsWith "$char:" tag then
      match String.head (String.drop 6 tag)
        when Just c -> CPLit (LChar c)
        when Nothing -> CPWild
      end
    else if tag == "$true" then
      CPLit (LBool true)
    else if tag == "$false" then
      CPLit (LBool false)
    else if tag == "$tuple" then
      CPTuple (List.map (n -> CPVar n) argNames)
    else if tag == "$record" then
      -- Record patterns need field names which we don't have here
      -- This is a simplification
      CPWild
    else
      CPWild

  -- ===========================================================================
  -- Entry Points
  -- ===========================================================================

  -- Compile a match expression
  -- scrutinee is a Name, not a CExpr
  let compileMatchWithName scrutinee cases =
    let tree = compile scrutinee cases in
    decisionToExpr scrutinee tree

  -- Compile all matches in an expression
  let rec compilePatterns expr = match expr
    when CVar _ -> expr
    when CLit _ -> expr
    when CCon _ -> expr
    when CForeign _ _ -> expr

    when CApp e1 e2 ->
      CApp (compilePatterns e1) (compilePatterns e2)

    when CAbs param body ->
      CAbs param (compilePatterns body)

    when CLet name value body ->
      CLet name (compilePatterns value) (compilePatterns body)

    when CLetRec bindings body ->
      let newBindings = List.map (b -> match b
        when CBinding n e -> CBinding n (compilePatterns e)
      end) bindings in
      CLetRec newBindings (compilePatterns body)

    when CMatch scrutinee cases ->
      -- First compile patterns in case bodies
      let compiledCases = List.map (c -> match c
        when CCase pat guard body ->
          CCase pat (Maybe.map compilePatterns guard) (compilePatterns body)
      end) cases in
      -- If scrutinee is already a variable, use it directly
      -- Otherwise bind it to a fresh name
      match scrutinee
        when CVar name ->
          compileMatchWithName name compiledCases
        when _ ->
          -- Bind the scrutinee expression to a fresh variable
          let scrutName = Name 9999 "$scrut" in
          CLet scrutName (compilePatterns scrutinee) (compileMatchWithName scrutName compiledCases)
      end

    when CTuple exprs ->
      CTuple (List.map compilePatterns exprs)

    when CRecord fields ->
      CRecord (compilePatternsFields fields)

    when CRecordUpdate base fields ->
      CRecordUpdate (compilePatterns base) (compilePatternsFields fields)

    when CFieldAccess e field ->
      CFieldAccess (compilePatterns e) field
  end

  let rec compilePatternsFields fields = match fields
    when Nil -> Nil
    when Cons (CField name e) rest ->
      Cons (CField name (compilePatterns e)) (compilePatternsFields rest)
  end

  -- Compile patterns in declarations
  let compilePatternsDecl decl = match decl
    when CDeclType name params cons -> decl
    when CDeclLet name expr -> CDeclLet name (compilePatterns expr)
    when CDeclLetRec bindings ->
      let newBindings = compilePatternsBindings bindings in
      CDeclLetRec newBindings
    when CDeclForeign name mod jsName ty -> decl
  end

  let rec compilePatternsBindings bindings = match bindings
    when Nil -> Nil
    when Cons (CBinding n e) rest ->
      Cons (CBinding n (compilePatterns e)) (compilePatternsBindings rest)
  end

  let compilePatternsProgram decls maybeExpr =
    let newDecls = List.map compilePatternsDecl decls in
    let newExpr = Maybe.map compilePatterns maybeExpr in
    (newDecls, newExpr)

end
