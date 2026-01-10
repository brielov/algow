-- =============================================================================
-- Codegen Module
-- JavaScript Code Generation from ANF IR
-- Reference: SPEC.md Section 12
-- =============================================================================

module Codegen
  use Types (..)
  use IR (..)

  -- ===========================================================================
  -- Code Generation Context
  -- ===========================================================================

  -- CodegenConfig: debug, minify
  type CodegenConfig = CodegenConfig bool bool

  let defaultConfig = CodegenConfig false false
  let debugConfig = CodegenConfig true false

  -- ===========================================================================
  -- Name Mangling
  -- ===========================================================================

  -- Convert Name to valid JavaScript identifier
  let mangleName name = match name
    when Name id original ->
      let base = sanitizeIdent original in
      base + "$" + Int.toString id
  end

  -- Sanitize identifier for JavaScript
  let sanitizeIdent s =
    let rec sanitize chars = match chars
      when Nil -> Nil
      when Cons c rest ->
        let c2 = if isValidJsChar c then c else '_' in
        Cons c2 (sanitize rest)
    end in
    String.fromList (sanitize (String.toList s))

  let isValidJsChar c =
    Char.isAlphaNum c || c == '_' || c == '$'

  -- ===========================================================================
  -- Literal Generation
  -- ===========================================================================

  let genLiteral lit = match lit
    when LInt n -> Int.toString n
    when LFloat f -> Float.toString f
    when LString s -> "\"" + escapeString s + "\""
    when LChar c -> "\"" + escapeChar c + "\""
    when LBool true -> "true"
    when LBool false -> "false"
  end

  -- Escape string for JavaScript
  let escapeString s =
    let rec escape chars = match chars
      when Nil -> Nil
      when Cons c rest ->
        let escaped = escapeChar c in
        List.append (String.toList escaped) (escape rest)
    end in
    String.fromList (escape (String.toList s))

  let escapeChar c =
    if c == '\n' then "\\n"
    else if c == '\r' then "\\r"
    else if c == '\t' then "\\t"
    else if c == '"' then "\\\""
    else if c == '\\' then "\\\\"
    else Char.toString c

  -- ===========================================================================
  -- Atom Generation
  -- ===========================================================================

  let genAtom atom = match atom
    when AVar name -> mangleName name
    when ALit lit -> genLiteral lit
    when ACon name tag ->
      -- Nullary constructor
      if name == "Nil" then "null"
      else if name == "Nothing" then "[" + Int.toString tag + "]"
      else "[" + Int.toString tag + "]"
  end

  -- ===========================================================================
  -- Expression Generation
  -- ===========================================================================

  let rec genExpr expr = match expr
    when IRAtom atom -> genAtom atom

    when IRLet name binding body ->
      let bindCode = genBinding binding in
      let varName = mangleName name in
      let bodyCode = genExpr body in
      "(()=>{const " + varName + "=" + bindCode + ";return " + bodyCode + "})()"

    when IRLetRec bindings body ->
      let bindingsCode = genRecBindings bindings in
      let bodyCode = genExpr body in
      "(()=>{" + bindingsCode + "return " + bodyCode + "})()"

    when IRMatch atom cases ->
      genMatch atom cases
  end

  -- ===========================================================================
  -- Binding Generation
  -- ===========================================================================

  and genBinding binding = match binding
    when IRBAtom atom ->
      genAtom atom

    when IRBApp f x ->
      genAtom f + "(" + genAtom x + ")"

    when IRBBinOp op a b ->
      let jsOp = translateOp op in
      "(" + genAtom a + jsOp + genAtom b + ")"

    when IRBTuple atoms ->
      "[" + String.join "," (List.map genAtom atoms) + "]"

    when IRBRecord fields ->
      let fieldStrs = List.map (f -> match f
        when (name, atom) -> name + ":" + genAtom atom
      end) fields in
      "{" + String.join "," fieldStrs + "}"

    when IRBRecordUpdate base fields ->
      let baseCode = genAtom base in
      let fieldStrs = List.map (f -> match f
        when (name, atom) -> name + ":" + genAtom atom
      end) fields in
      "{..." + baseCode + "," + String.join "," fieldStrs + "}"

    when IRBField record field ->
      genAtom record + "." + field

    when IRBLambda param body ->
      let paramName = mangleName param in
      let bodyCode = genExpr body in
      "(" + paramName + ")=>" + bodyCode

    when IRBForeign mod name args ->
      let argsCode = List.map genAtom args in
      -- Foreign function access
      if mod == "" then
        name
      else
        mod + "." + name + List.foldl (acc a -> acc + "(" + a + ")") "" argsCode

    when IRBCon name tag args ->
      -- Constructor application
      if name == "Cons" then
        -- Special case: List uses { h, t } representation
        match args
          when Cons h (Cons t Nil) -> "{h:" + genAtom h + ",t:" + genAtom t + "}"
          when _ -> genDefaultCon tag args
        end
      else if name == "Nil" then
        "null"
      else
        genDefaultCon tag args
  end

  -- Default constructor representation: [tag, ...args]
  let genDefaultCon tag args =
    let argsCode = List.map genAtom args in
    "[" + Int.toString tag + (if List.isEmpty argsCode then "" else ",") + String.join "," argsCode + "]"

  -- Translate operator to JavaScript
  let translateOp op = match op
    when "+" -> "+"
    when "-" -> "-"
    when "*" -> "*"
    when "/" -> "/"
    when "==" -> "==="
    when "!=" -> "!=="
    when "<" -> "<"
    when "<=" -> "<="
    when ">" -> ">"
    when ">=" -> ">="
    when "&&" -> "&&"
    when "||" -> "||"
    when _ -> op
  end

  -- ===========================================================================
  -- Recursive Bindings Generation
  -- ===========================================================================

  and genRecBindings bindings =
    let bindingStrs = List.map (b -> match b
      when IRRecBinding name binding ->
        let varName = mangleName name in
        let bindCode = genBinding binding in
        "const " + varName + "=" + bindCode + ";"
    end) bindings in
    String.join "" bindingStrs

  -- ===========================================================================
  -- Match Generation
  -- ===========================================================================

  and genMatch atom cases =
    let scrutinee = genAtom atom in
    -- Check match type: literal, list, tuple, or ADT
    if isLiteralMatchCases cases then
      genLiteralMatch scrutinee cases
    else if isListMatchCases cases then
      genListMatch scrutinee cases
    else if isTupleMatchCases cases then
      genTupleMatch scrutinee cases
    else
      genTagMatch scrutinee cases

  -- Check if this is a match on literal values
  let isLiteralMatchCases cases =
    List.any (c -> match c
      when IRCase pat _ ->
        match pat
          when IRPLit _ -> true
          when _ -> false
        end
    end) cases

  -- Check if this is a match on List type
  let isListMatchCases cases =
    List.any (c -> match c
      when IRCase pat _ ->
        match pat
          when IRPCon name _ _ -> name == "Nil" || name == "Cons"
          when _ -> false
        end
    end) cases

  -- Check if this is a match on tuple type
  let isTupleMatchCases cases =
    List.any (c -> match c
      when IRCase pat _ ->
        match pat
          when IRPTuple _ -> true
          when _ -> false
        end
    end) cases

  -- Generate tuple match (just bind pattern variables)
  and genTupleMatch scrutinee cases =
    -- Tuples don't have tags, just bind the pattern variables directly
    match cases
      when Cons (IRCase pat body) _ ->
        let bindings = genPatternBinding scrutinee pat in
        "(()=>{" + bindings + "return " + genExpr body + "})()"
      when Nil -> "undefined"
    end

  -- Generate literal match (direct switch on value)
  and genLiteralMatch scrutinee cases =
    let switchCases = List.map (genLiteralCase scrutinee) cases in
    "(()=>{switch(" + scrutinee + "){" + String.join "" switchCases + "}})()"

  and genLiteralCase scrutinee c = match c
    when IRCase pat body ->
      match pat
        when IRPLit lit ->
          "case " + genLiteral lit + ":return " + genExpr body + ";"
        when IRPWild ->
          "default:return " + genExpr body + ";"
        when IRPVar name ->
          let varName = mangleName name in
          "default:{const " + varName + "=" + scrutinee + ";return " + genExpr body + ";}"
        when _ ->
          "default:return " + genExpr body + ";"
      end
  end

  -- Generate List match (null vs object check)
  and genListMatch scrutinee cases =
    let branches = List.map (genListCase scrutinee) cases in
    String.join "" branches + "undefined"

  and genListCase scrutinee c = match c
    when IRCase pat body ->
      match pat
        when IRPCon name _ pats ->
          if name == "Nil" then
            scrutinee + "===null?" + genExpr body + ":"
          else if name == "Cons" then
            -- Extract head and tail
            let bindings = genConsBindings scrutinee pats in
            scrutinee + "!==null?(()=>{" + bindings + "return " + genExpr body + "})():"
          else
            genTagCase scrutinee pat body
        when IRPWild -> genExpr body
        when IRPVar name ->
          let varName = mangleName name in
          "(()=>{const " + varName + "=" + scrutinee + ";return " + genExpr body + "})()"
        when _ -> genTagCase scrutinee pat body
      end
  end

  -- Generate bindings for Cons pattern
  let genConsBindings scrutinee pats = match pats
    when Cons hPat (Cons tPat Nil) ->
      genPatternBinding (scrutinee + ".h") hPat +
      genPatternBinding (scrutinee + ".t") tPat
    when _ -> ""
  end

  -- Generate a single tag case (fallback from list matching)
  and genTagCase scrutinee pat body =
    match pat
      when IRPCon _ tag pats ->
        let bindings = genConBindings scrutinee 1 pats in
        "(()=>{if(" + scrutinee + "[0]===" + Int.toString tag + "){" + bindings + "return " + genExpr body + ";}})() || "
      when _ ->
        genExpr body
    end

  -- Generate tag-based match (for ADTs)
  and genTagMatch scrutinee cases =
    -- Use switch on tag
    let switchCases = List.map (genSwitchCase scrutinee) cases in
    "(()=>{switch(" + scrutinee + "[0]){" + String.join "" switchCases + "}})()"

  and genSwitchCase scrutinee c = match c
    when IRCase pat body ->
      match pat
        when IRPCon _ tag pats ->
          let bindings = genConBindings scrutinee 1 pats in
          "case " + Int.toString tag + ":{" + bindings + "return " + genExpr body + ";}"
        when IRPLit lit ->
          -- Literal matching (shouldn't have tag, but handle it)
          "case " + genLiteral lit + ":return " + genExpr body + ";"
        when IRPWild ->
          "default:return " + genExpr body + ";"
        when IRPVar name ->
          let varName = mangleName name in
          "default:{const " + varName + "=" + scrutinee + ";return " + genExpr body + ";}"
        when _ ->
          "default:return " + genExpr body + ";"
      end
  end

  -- Generate constructor argument bindings
  let rec genConBindings scrutinee idx pats = match pats
    when Nil -> ""
    when Cons pat rest ->
      let access = scrutinee + "[" + Int.toString idx + "]" in
      genPatternBinding access pat + genConBindings scrutinee (idx + 1) rest
  end

  -- Generate binding for a single pattern
  let genPatternBinding access pat = match pat
    when IRPWild -> ""
    when IRPVar name ->
      let varName = mangleName name in
      "const " + varName + "=" + access + ";"
    when IRPCon _ _ pats ->
      genConBindings access 1 pats
    when IRPTuple pats ->
      genTupleBindings access 0 pats
    when IRPRecord fields ->
      genRecordBindings access fields
    when IRPAs name inner ->
      let varName = mangleName name in
      "const " + varName + "=" + access + ";" + genPatternBinding access inner
    when IRPLit _ -> ""
  end

  let rec genTupleBindings access idx pats = match pats
    when Nil -> ""
    when Cons pat rest ->
      let elemAccess = access + "[" + Int.toString idx + "]" in
      genPatternBinding elemAccess pat + genTupleBindings access (idx + 1) rest
  end

  let genRecordBindings access fields =
    List.foldl (acc f -> match f
      when (name, pat) ->
        let fieldAccess = access + "." + name in
        acc + genPatternBinding fieldAccess pat
    end) "" fields

  -- ===========================================================================
  -- Declaration Generation
  -- ===========================================================================

  let genDecl decl = match decl
    when IRDeclLet name binding ->
      let varName = mangleName name in
      let bindCode = genBinding binding in
      "const " + varName + "=" + bindCode + ";"

    when IRDeclLetRec bindings ->
      genRecBindings bindings
  end

  -- ===========================================================================
  -- Program Generation
  -- ===========================================================================

  let genProgram program = match program
    when IRProgram decls maybeExpr ->
      let declsCode = String.join "\n" (List.map genDecl decls) in
      let mainCode = match maybeExpr
        when Nothing -> ""
        when Just expr -> genExpr expr
      end in
      declsCode + (if mainCode == "" then "" else "\n" + mainCode)
  end

  -- ===========================================================================
  -- Runtime Code
  -- ===========================================================================

  -- Minimal runtime support functions
  let runtimeCode = "
const $eq = (a) => (b) => {
  if (a === b) return true;
  if (a === null || b === null) return a === b;
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!$eq(a[i])(b[i])) return false;
    return true;
  }
  if (typeof a === 'object' && typeof b === 'object') {
    const ka = Object.keys(a), kb = Object.keys(b);
    if (ka.length !== kb.length) return false;
    for (const k of ka) if (!$eq(a[k])(b[k])) return false;
    return true;
  }
  return false;
};
"

  -- ===========================================================================
  -- Entry Points
  -- ===========================================================================

  -- Generate complete JavaScript program
  let generate program =
    runtimeCode + "\n" + genProgram program

  -- Generate without runtime (for testing)
  let generateNoRuntime program =
    genProgram program

  -- Generate a single expression
  let generateExpr expr =
    genExpr expr

end
