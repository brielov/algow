-- =============================================================================
-- IR Module
-- A-Normal Form (ANF) Intermediate Representation
-- Reference: SPEC.md Section 10, Flanagan et al. (1993)
-- =============================================================================

module IR
  use Types (..)

  -- ===========================================================================
  -- Atoms (Trivial expressions - no computation)
  -- ===========================================================================

  type Atom
    = AVar Name                   -- Variable reference
    | ALit Literal                -- Literal value
    | ACon string int             -- Constructor with tag

  -- ===========================================================================
  -- IR Expressions
  -- ===========================================================================

  -- Main expression type (in ANF)
  type IRExpr
    = IRAtom Atom                           -- Trivial expression
    | IRLet Name IRBinding IRExpr           -- let x = binding in body
    | IRLetRec (List IRRecBinding) IRExpr   -- letrec bindings in body
    | IRMatch Atom (List IRCase)            -- Match on atomic value

  -- ===========================================================================
  -- IR Bindings (Right-hand side of let)
  -- ===========================================================================

  type IRBinding
    = IRBAtom Atom                              -- Simple atom (for constant folding)
    | IRBApp Atom Atom                          -- Function application
    | IRBBinOp string Atom Atom                 -- Binary operation
    | IRBTuple (List Atom)                      -- Tuple construction
    | IRBRecord (List (string, Atom))           -- Record construction
    | IRBRecordUpdate Atom (List (string, Atom))-- Record update
    | IRBField Atom string                      -- Field access
    | IRBLambda Name IRExpr                     -- Lambda abstraction
    | IRBForeign string string (List Atom)      -- Foreign function call
    | IRBCon string int (List Atom)             -- Constructor application

  -- Recursive binding
  type IRRecBinding = IRRecBinding Name IRBinding

  -- ===========================================================================
  -- IR Patterns and Cases
  -- ===========================================================================

  type IRPattern
    = IRPWild                                -- Wildcard
    | IRPVar Name                            -- Variable binding
    | IRPLit Literal                         -- Literal pattern
    | IRPCon string int (List IRPattern)    -- Constructor (name, tag, args)
    | IRPTuple (List IRPattern)             -- Tuple pattern
    | IRPRecord (List (string, IRPattern))  -- Record pattern
    | IRPAs Name IRPattern                  -- As-pattern

  -- Match case
  type IRCase = IRCase IRPattern IRExpr

  -- ===========================================================================
  -- IR Declarations
  -- ===========================================================================

  type IRDecl
    = IRDeclLet Name IRBinding              -- let name = binding
    | IRDeclLetRec (List IRRecBinding)      -- letrec bindings

  -- ===========================================================================
  -- IR Program
  -- ===========================================================================

  type IRProgram = IRProgram (List IRDecl) (Maybe IRExpr)

  -- ===========================================================================
  -- Atom Predicates
  -- ===========================================================================

  let isVar atom = match atom
    when AVar _ -> true
    when _ -> false
  end

  let isLit atom = match atom
    when ALit _ -> true
    when _ -> false
  end

  let isCon atom = match atom
    when ACon _ _ -> true
    when _ -> false
  end

  -- ===========================================================================
  -- Atom Constructors
  -- ===========================================================================

  let aVar name = AVar name
  let aLit lit = ALit lit
  let aInt n = ALit (LInt n)
  let aFloat f = ALit (LFloat f)
  let aString s = ALit (LString s)
  let aChar c = ALit (LChar c)
  let aBool b = ALit (LBool b)
  let aCon name tag = ACon name tag

  -- ===========================================================================
  -- Expression Constructors
  -- ===========================================================================

  let irAtom atom = IRAtom atom
  let irVar name = IRAtom (AVar name)
  let irLit lit = IRAtom (ALit lit)

  let irLet name binding body = IRLet name binding body
  let irLetRec bindings body = IRLetRec bindings body
  let irMatch atom cases = IRMatch atom cases

  -- ===========================================================================
  -- Binding Constructors
  -- ===========================================================================

  let irbApp f x = IRBApp f x
  let irbBinOp op a b = IRBBinOp op a b
  let irbTuple atoms = IRBTuple atoms
  let irbRecord fields = IRBRecord fields
  let irbRecordUpdate base fields = IRBRecordUpdate base fields
  let irbField record field = IRBField record field
  let irbLambda param body = IRBLambda param body
  let irbForeign mod name args = IRBForeign mod name args
  let irbCon name tag args = IRBCon name tag args

  -- ===========================================================================
  -- Pattern Constructors
  -- ===========================================================================

  let irpWild = IRPWild
  let irpVar name = IRPVar name
  let irpLit lit = IRPLit lit
  let irpCon name tag pats = IRPCon name tag pats
  let irpTuple pats = IRPTuple pats
  let irpRecord fields = IRPRecord fields
  let irpAs name pat = IRPAs name pat

  -- ===========================================================================
  -- Free Variables
  -- ===========================================================================

  -- Free variables in an atom
  let freeVarsAtom atom = match atom
    when AVar name -> Set.singleton name
    when ALit _ -> Set.empty
    when ACon _ _ -> Set.empty
  end

  -- Free variables in a list of atoms
  let freeVarsAtoms atoms =
    List.foldl (acc a -> Set.union acc (freeVarsAtom a)) Set.empty atoms

  -- Free variables in a binding
  let rec freeVarsBinding binding = match binding
    when IRBAtom atom -> freeVarsAtom atom
    when IRBApp f x -> Set.union (freeVarsAtom f) (freeVarsAtom x)
    when IRBBinOp _ a b -> Set.union (freeVarsAtom a) (freeVarsAtom b)
    when IRBTuple atoms -> freeVarsAtoms atoms
    when IRBRecord fields ->
      List.foldl (acc f -> match f when (_, a) -> Set.union acc (freeVarsAtom a) end) Set.empty fields
    when IRBRecordUpdate base fields ->
      let baseFv = freeVarsAtom base in
      let fieldsFv = List.foldl (acc f -> match f when (_, a) -> Set.union acc (freeVarsAtom a) end) Set.empty fields in
      Set.union baseFv fieldsFv
    when IRBField a _ -> freeVarsAtom a
    when IRBLambda param body ->
      Set.difference (freeVarsExpr body) (Set.singleton param)
    when IRBForeign _ _ args -> freeVarsAtoms args
    when IRBCon _ _ args -> freeVarsAtoms args
  end

  -- Free variables in an expression
  and freeVarsExpr expr = match expr
    when IRAtom a -> freeVarsAtom a
    when IRLet name binding body ->
      let bindingFv = freeVarsBinding binding in
      let bodyFv = Set.difference (freeVarsExpr body) (Set.singleton name) in
      Set.union bindingFv bodyFv
    when IRLetRec bindings body ->
      let names = List.map (b -> match b when IRRecBinding n _ -> n end) bindings in
      let nameSet = Set.fromList names in
      let bindingsFv = List.foldl (acc b -> match b
        when IRRecBinding _ bind -> Set.union acc (freeVarsBinding bind)
      end) Set.empty bindings in
      let bodyFv = freeVarsExpr body in
      Set.difference (Set.union bindingsFv bodyFv) nameSet
    when IRMatch atom cases ->
      let atomFv = freeVarsAtom atom in
      let casesFv = List.foldl (acc c -> Set.union acc (freeVarsCase c)) Set.empty cases in
      Set.union atomFv casesFv
  end

  -- Free variables in a case
  and freeVarsCase c = match c
    when IRCase pat body ->
      let boundNames = patternNames pat in
      Set.difference (freeVarsExpr body) boundNames
  end

  -- Names bound by a pattern
  and patternNames pat = match pat
    when IRPWild -> Set.empty
    when IRPVar name -> Set.singleton name
    when IRPLit _ -> Set.empty
    when IRPCon _ _ pats -> List.foldl (acc p -> Set.union acc (patternNames p)) Set.empty pats
    when IRPTuple pats -> List.foldl (acc p -> Set.union acc (patternNames p)) Set.empty pats
    when IRPRecord fields -> List.foldl (acc f -> match f when (_, p) -> Set.union acc (patternNames p) end) Set.empty fields
    when IRPAs name pat -> Set.union (Set.singleton name) (patternNames pat)
  end

  -- ===========================================================================
  -- Substitution
  -- ===========================================================================

  -- Substitute a name with an atom in an expression
  let rec substExpr name atom expr = match expr
    when IRAtom a -> IRAtom (substAtom name atom a)
    when IRLet x binding body ->
      if x == name then
        IRLet x (substBinding name atom binding) body
      else
        IRLet x (substBinding name atom binding) (substExpr name atom body)
    when IRLetRec bindings body ->
      let names = List.map (b -> match b when IRRecBinding n _ -> n end) bindings in
      if List.any (n -> n == name) names then
        -- name is rebound, don't substitute in body
        IRLetRec bindings body
      else
        let newBindings = List.map (b -> match b
          when IRRecBinding n bind -> IRRecBinding n (substBinding name atom bind)
        end) bindings in
        IRLetRec newBindings (substExpr name atom body)
    when IRMatch a cases ->
      IRMatch (substAtom name atom a) (List.map (substCase name atom) cases)
  end

  and substAtom name atom a = match a
    when AVar n -> if n == name then atom else a
    when _ -> a
  end

  and substBinding name atom binding = match binding
    when IRBAtom a -> IRBAtom (substAtom name atom a)
    when IRBApp f x -> IRBApp (substAtom name atom f) (substAtom name atom x)
    when IRBBinOp op a b -> IRBBinOp op (substAtom name atom a) (substAtom name atom b)
    when IRBTuple atoms -> IRBTuple (List.map (substAtom name atom) atoms)
    when IRBRecord fields ->
      IRBRecord (substRecordFields name atom fields)
    when IRBRecordUpdate base fields ->
      IRBRecordUpdate (substAtom name atom base) (substRecordFields name atom fields)
    when IRBField a field -> IRBField (substAtom name atom a) field
    when IRBLambda param body ->
      if param == name then binding
      else IRBLambda param (substExpr name atom body)
    when IRBForeign mod n args -> IRBForeign mod n (List.map (substAtom name atom) args)
    when IRBCon cn tag args -> IRBCon cn tag (List.map (substAtom name atom) args)
  end

  and substRecordFields name atom fields = match fields
    when Nil -> Nil
    when Cons (n, a) rest -> Cons (n, substAtom name atom a) (substRecordFields name atom rest)
  end

  and substCase name atom c = match c
    when IRCase pat body ->
      let bound = patternNames pat in
      if Set.member name bound then c
      else IRCase pat (substExpr name atom body)
  end

end
