-- =============================================================================
-- JAVASCRIPT CODE GENERATION
-- =============================================================================
--
-- Generates JavaScript code from optimized ANF IR.
--
-- Runtime representations:
-- - Primitives: JavaScript primitives (number, string, boolean)
-- - Closures: Native JavaScript closures
-- - Constructors: { $tag: "Name", $args: [...] }
-- - Tuples: JavaScript arrays
-- - Records: Plain JavaScript objects
--
-- Reference: SPEC.md Section 12
--
-- =============================================================================

module Codegen

  use Common (..)
  use IR (..)
  use Infer (Type, TVar, TCon, TFun, TApp, TRecord, TTuple)

  -- =========================================================================
  -- CODE GENERATION CONTEXT
  -- =========================================================================

  type CodeGenContext = {
    indent : int,
    lines : List string,
    constructors : List string,
    declaredNames : List string,
    scrutineeCounter : int
  }

  let initContext = constructors -> {
    indent = 0,
    lines = Nil,
    constructors = constructors,
    declaredNames = Nil,
    scrutineeCounter = 0
  }

  -- Emit a line of code
  let emit = code ctx ->
    let indentation = String.replicate (ctx.indent * 2) " " in
    { ctx | lines = Cons (String.concat indentation code) ctx.lines }

  -- Generate fresh scrutinee variable
  let freshScrutinee = ctx ->
    let name = String.concat "_s" (Int.toString ctx.scrutineeCounter) in
    (name, { ctx | scrutineeCounter = ctx.scrutineeCounter + 1 })

  -- Check if name is declared
  let isDeclared = name ctx -> List.any (n -> n == name) ctx.declaredNames

  -- Add declared name
  let addDeclared = name ctx -> { ctx | declaredNames = Cons name ctx.declaredNames }

  -- Check if name is a constructor
  let isConstructor = name ctx -> List.any (c -> c == name) ctx.constructors

  -- =========================================================================
  -- JAVASCRIPT RESERVED WORDS
  -- =========================================================================

  let jsReserved = [
    "break", "case", "catch", "class", "const", "continue", "debugger",
    "default", "delete", "do", "else", "export", "extends", "finally",
    "for", "function", "if", "import", "in", "instanceof", "let", "new",
    "return", "static", "super", "switch", "this", "throw", "try",
    "typeof", "var", "void", "while", "with", "yield",
    "null", "true", "false", "enum", "await"
  ]

  let isReserved = name -> List.any (w -> w == name) jsReserved

  -- Convert name to valid JS identifier
  let toJsId = name ->
    if isReserved name then String.concat "$" name
    else name

  -- =========================================================================
  -- ATOM GENERATION
  -- =========================================================================

  let genAtom = atom ctx -> match atom
    when ALit lit _ -> genLiteral lit
    when AVar name _ ->
      if isConstructor name ctx then String.concat "$con(\"" (String.concat name "\")")
      else toJsId name
    when AForeignVar mod name _ ->
      String.concat "$foreign[\"" (String.concat mod (String.concat "\"][\"" (String.concat name "\"]")))
  end

  and genLiteral = lit -> match lit
    when LInt n -> Int.toString n
    when LFloat f -> Float.toString f
    when LString s -> String.concat "\"" (String.concat (escapeString s) "\"")
    when LChar c -> String.concat "\"" (String.concat (Char.toString c) "\"")
    when LBool b -> if b then "true" else "false"
  end

  -- Escape special characters in strings
  and escapeString = s ->
    -- Simplified: just handle common escapes
    String.replace "\"" "\\\"" (String.replace "\n" "\\n" (String.replace "\\" "\\\\" s))

  -- =========================================================================
  -- EXPRESSION GENERATION
  -- =========================================================================

  let rec genExpr = expr ctx -> match expr
    when EAtom atom -> (genAtom atom ctx, ctx)

    when ELet name binding body ->
      let (bindingCode, ctx1) = genBinding binding ctx in
      let jsName = toJsId name in
      let ctx2 = if isDeclared jsName ctx1 then
          emit (String.concat jsName (String.concat " = " (String.concat bindingCode ";"))) ctx1
        else
          emit (String.concat "let " (String.concat jsName (String.concat " = " (String.concat bindingCode ";")))) (addDeclared jsName ctx1)
      in
      genExpr body ctx2

    when ELetRec bindings body ->
      -- Two-phase init for mutual recursion
      let ctx1 = List.foldl (c pair -> match pair when (n, _) ->
        let jsName = toJsId n in
        if isDeclared jsName c then c
        else emit (String.concat "let " (String.concat jsName ";")) (addDeclared jsName c)
      end) ctx bindings in
      let ctx2 = List.foldl (c pair -> match pair when (n, b) ->
        let (code, c1) = genBinding b c in
        emit (String.concat (toJsId n) (String.concat " = " (String.concat code ";"))) c1
      end) ctx1 bindings in
      genExpr body ctx2
  end

  -- =========================================================================
  -- BINDING GENERATION
  -- =========================================================================

  and genBinding = binding ctx -> match binding
    when BAtom atom -> (genAtom atom ctx, ctx)

    when BApp func arg _ ->
      let funcCode = genAtom func ctx in
      let argCode = genAtom arg ctx in
      (String.concat "$apply(" (String.concat funcCode (String.concat ", " (String.concat argCode ")"))), ctx)

    when BBinOp op left right opType resType ->
      genBinOp op left right opType ctx

    when BIf cond thenB elseB _ ->
      genIf cond thenB elseB ctx

    when BTuple elems _ ->
      let elemCodes = List.map (e -> genAtom e ctx) elems in
      (String.concat "[" (String.concat (String.join ", " elemCodes) "]"), ctx)

    when BRecord fields _ ->
      let fieldCodes = List.map (pair -> match pair when (n, v) -> String.concat n (String.concat ": " (genAtom v ctx)) end) fields in
      (String.concat "{ " (String.concat (String.join ", " fieldCodes) " }"), ctx)

    when BRecordUpdate base fields _ ->
      let baseCode = genAtom base ctx in
      let fieldCodes = List.map (pair -> match pair when (n, v) -> String.concat n (String.concat ": " (genAtom v ctx)) end) fields in
      (String.concat "{ ..." (String.concat baseCode (String.concat ", " (String.concat (String.join ", " fieldCodes) " }"))), ctx)

    when BFieldAccess record field _ ->
      let recordCode = genAtom record ctx in
      (String.concat recordCode (String.concat "." field), ctx)

    when BTupleIndex tuple idx _ ->
      let tupleCode = genAtom tuple ctx in
      (String.concat tupleCode (String.concat "[" (String.concat (Int.toString idx) "]")), ctx)

    when BMatch scrutinee cases _ ->
      genMatch scrutinee cases ctx

    when BLambda param paramType body funcType ->
      genLambda param body ctx
  end

  -- =========================================================================
  -- BINARY OPERATIONS
  -- =========================================================================

  and genBinOp = op left right opType ctx ->
    let leftCode = genAtom left ctx in
    let rightCode = genAtom right ctx in
    match op
      when "+" -> (String.concat "(" (String.concat leftCode (String.concat " + " (String.concat rightCode ")"))), ctx)
      when "-" -> (String.concat "(" (String.concat leftCode (String.concat " - " (String.concat rightCode ")"))), ctx)
      when "*" -> (String.concat "(" (String.concat leftCode (String.concat " * " (String.concat rightCode ")"))), ctx)
      when "/" -> (String.concat "(" (String.concat leftCode (String.concat " / " (String.concat rightCode ")"))), ctx)
      when "<" -> (String.concat "(" (String.concat leftCode (String.concat " < " (String.concat rightCode ")"))), ctx)
      when ">" -> (String.concat "(" (String.concat leftCode (String.concat " > " (String.concat rightCode ")"))), ctx)
      when "<=" -> (String.concat "(" (String.concat leftCode (String.concat " <= " (String.concat rightCode ")"))), ctx)
      when ">=" -> (String.concat "(" (String.concat leftCode (String.concat " >= " (String.concat rightCode ")"))), ctx)
      when "==" ->
        if isComplexType opType then
          (String.concat "$eq(" (String.concat leftCode (String.concat ", " (String.concat rightCode ")"))), ctx)
        else
          (String.concat "(" (String.concat leftCode (String.concat " === " (String.concat rightCode ")"))), ctx)
      when "!=" ->
        if isComplexType opType then
          (String.concat "!$eq(" (String.concat leftCode (String.concat ", " (String.concat rightCode ")"))), ctx)
        else
          (String.concat "(" (String.concat leftCode (String.concat " !== " (String.concat rightCode ")"))), ctx)
      when _ -> (String.concat "(" (String.concat leftCode (String.concat " " (String.concat op (String.concat " " (String.concat rightCode ")"))))), ctx)
    end

  -- Check if type needs deep equality
  and isComplexType = typ -> match typ
    when TCon name -> not (List.any (n -> n == name) ["int", "float", "string", "char", "boolean"])
    when TVar _ -> true
    when TFun _ _ -> true
    when TApp _ _ -> true
    when TRecord _ _ -> true
    when TTuple _ -> true
  end

  -- =========================================================================
  -- CONDITIONAL GENERATION
  -- =========================================================================

  and genIf = cond thenB elseB ctx ->
    let condCode = genAtom cond ctx in
    -- Generate branches in IIFEs
    let ctx1 = { ctx | indent = ctx.indent + 1, lines = Nil } in
    let (thenResult, ctx2) = genExpr thenB ctx1 in
    let thenLines = ctx2.lines in
    let ctx3 = { ctx | indent = ctx.indent + 1, lines = Nil } in
    let (elseResult, ctx4) = genExpr elseB ctx3 in
    let elseLines = ctx4.lines in
    let thenCode = if List.isEmpty thenLines then thenResult
      else String.concat "(() => {\n" (String.concat (String.join "\n" (List.reverse thenLines)) (String.concat "\nreturn " (String.concat thenResult ";\n})()")))
    in
    let elseCode = if List.isEmpty elseLines then elseResult
      else String.concat "(() => {\n" (String.concat (String.join "\n" (List.reverse elseLines)) (String.concat "\nreturn " (String.concat elseResult ";\n})()")))
    in
    (String.concat "(" (String.concat condCode (String.concat " ? " (String.concat thenCode (String.concat " : " (String.concat elseCode ")"))))), ctx)

  -- =========================================================================
  -- LAMBDA GENERATION
  -- =========================================================================

  and genLambda = param body ctx ->
    let jsParam = toJsId param in
    let ctx1 = { ctx | indent = ctx.indent + 1, lines = Nil, declaredNames = Cons jsParam ctx.declaredNames } in
    let (bodyResult, ctx2) = genExpr body ctx1 in
    let bodyLines = ctx2.lines in
    if List.isEmpty bodyLines then
      let code = String.concat "(" (String.concat jsParam (String.concat ") => " bodyResult)) in
      (code, ctx)
    else
      let bodyCode = String.join "\n" (List.reverse bodyLines) in
      let returnStmt = String.concat "return " (String.concat bodyResult ";") in
      let code = String.concat "(" (String.concat jsParam (String.concat ") => {\n" (String.concat bodyCode (String.concat "\n" (String.concat returnStmt "\n}"))))) in
      (code, ctx)

  -- =========================================================================
  -- PATTERN MATCHING
  -- =========================================================================

  and genMatch = scrutinee cases ctx ->
    let scrutineeCode = genAtom scrutinee ctx in
    let (scrutineeVar, ctx1) = freshScrutinee ctx in
    let lines = [String.concat "((" (String.concat scrutineeVar ") => {")] in
    let (caseLines, ctx2) = genCases scrutineeVar cases 0 ctx1 in
    let allLines = List.concat lines (List.concat caseLines ["})" (String.concat "(" (String.concat scrutineeCode ")"))]) in
    (String.join "\n" allLines, ctx2)

  and genCases = scrutineeVar cases idx ctx -> match cases
    when Nil -> (Nil, ctx)
    when Cons c rest ->
      let (caseLines, ctx1) = genCase scrutineeVar c idx ctx in
      let (restLines, ctx2) = genCases scrutineeVar rest (idx + 1) ctx1 in
      (List.concat caseLines restLines, ctx2)
  end

  and genCase = scrutineeVar c idx ctx ->
    let (condition, bindings) = genPatternMatch scrutineeVar c.pattern in
    let prefix = if idx == 0 then "  if" else "  } else if" in
    let lines = [String.concat prefix (String.concat " (" (String.concat condition ") {"))] in
    -- Emit bindings
    let bindingLines = List.map (pair -> match pair when (n, expr) -> String.concat "    const " (String.concat (toJsId n) (String.concat " = " (String.concat expr ";"))) end) bindings in
    -- Generate body
    let ctx1 = { ctx | indent = ctx.indent + 2, lines = Nil } in
    let (bodyResult, ctx2) = genExpr c.body ctx1 in
    let bodyLines = List.map (l -> String.concat "    " l) (List.reverse ctx2.lines) in
    let returnLine = String.concat "    return " (String.concat bodyResult ";") in
    let allLines = List.concat lines (List.concat bindingLines (List.concat bodyLines [returnLine])) in
    (allLines, ctx)

  -- Generate pattern match condition and bindings
  and genPatternMatch = scrutinee pattern -> match pattern
    when PVar name _ -> ("true", [(name, scrutinee)])
    when PWild _ -> ("true", Nil)
    when PLit lit _ ->
      let value = genLiteral lit in
      (String.concat scrutinee (String.concat " === " value), Nil)
    when PCon name args _ ->
      let tagCheck = String.concat scrutinee (String.concat ".$tag === \"" (String.concat name "\"")) in
      let (argConditions, argBindings) = genConArgs scrutinee args 0 in
      let condition = if List.isEmpty argConditions then tagCheck
        else String.concat tagCheck (String.concat " && " (String.join " && " argConditions))
      in
      (condition, argBindings)
    when PTuple elems _ ->
      let (elemConditions, elemBindings) = genTupleElems scrutinee elems 0 in
      let condition = if List.isEmpty elemConditions then "true" else String.join " && " elemConditions in
      (condition, elemBindings)
    when PRecord fields _ ->
      let (fieldConditions, fieldBindings) = genRecordFields scrutinee fields in
      let condition = if List.isEmpty fieldConditions then "true" else String.join " && " fieldConditions in
      (condition, fieldBindings)
    when PAs name inner _ ->
      let (innerCond, innerBindings) = genPatternMatch scrutinee inner in
      (innerCond, Cons (name, scrutinee) innerBindings)
    when POr alts _ ->
      -- Use first alternative's bindings
      let conditionsAndBindings = List.map (p -> genPatternMatch scrutinee p) alts in
      let conditions = List.map (pair -> match pair when (c, _) -> String.concat "(" (String.concat c ")") end) conditionsAndBindings in
      let bindings = match conditionsAndBindings when Cons (_, b) _ -> b when _ -> Nil end in
      (String.join " || " conditions, bindings)
  end

  and genConArgs = scrutinee args idx -> match args
    when Nil -> (Nil, Nil)
    when Cons arg rest ->
      let argScrutinee = String.concat scrutinee (String.concat ".$args[" (String.concat (Int.toString idx) "]")) in
      let (cond, binds) = genPatternMatch argScrutinee arg in
      let (restConds, restBinds) = genConArgs scrutinee rest (idx + 1) in
      let conditions = if cond == "true" then restConds else Cons cond restConds in
      (conditions, List.concat binds restBinds)
  end

  and genTupleElems = scrutinee elems idx -> match elems
    when Nil -> (Nil, Nil)
    when Cons elem rest ->
      let elemScrutinee = String.concat scrutinee (String.concat "[" (String.concat (Int.toString idx) "]")) in
      let (cond, binds) = genPatternMatch elemScrutinee elem in
      let (restConds, restBinds) = genTupleElems scrutinee rest (idx + 1) in
      let conditions = if cond == "true" then restConds else Cons cond restConds in
      (conditions, List.concat binds restBinds)
  end

  and genRecordFields = scrutinee fields -> match fields
    when Nil -> (Nil, Nil)
    when Cons (name, pattern) rest ->
      let fieldScrutinee = String.concat scrutinee (String.concat "." name) in
      let (cond, binds) = genPatternMatch fieldScrutinee pattern in
      let (restConds, restBinds) = genRecordFields scrutinee rest in
      let conditions = if cond == "true" then restConds else Cons cond restConds in
      (conditions, List.concat binds restBinds)
  end

  -- =========================================================================
  -- RUNTIME HEADER
  -- =========================================================================

  let runtimeHeader = "// Generated by Algow Compiler
\"use strict\";

// Load runtime (defines $apply, $con, $eq, $foreign)
const { $apply, $con, $eq, $foreign } = require(\"./runtime.js\");
"

  -- Inline runtime for standalone execution (no external file needed)
  let inlineRuntime = "// Generated by Algow Compiler (standalone)
\"use strict\";

const $apply = (fn, arg) => {
  if (typeof fn === \"function\") return fn(arg);
  return { $tag: fn.$tag, $args: [...fn.$args, arg] };
};

const $con = (tag, ...args) => ({ $tag: tag, $args: args });

const $eq = (a, b) => {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  if (typeof a !== \"object\" || a === null) return false;
  if (\"$tag\" in a && \"$tag\" in b) {
    if (a.$tag !== b.$tag) return false;
    if (a.$args.length !== b.$args.length) return false;
    return a.$args.every((x, i) => $eq(x, b.$args[i]));
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((x, i) => $eq(x, b[i]));
  }
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every(k => k in b && $eq(a[k], b[k]));
};

const $foreign = {};
"

  -- =========================================================================
  -- MAIN ENTRY POINTS
  -- =========================================================================

  -- Generate with external runtime reference
  let generate = ir constructors ->
    let ctx = initContext constructors in
    let (result, finalCtx) = genExpr ir ctx in
    let codeLines = List.reverse finalCtx.lines in
    let mainCode = String.join "\n" codeLines in
    String.concat runtimeHeader (String.concat "\n// Generated code\n" (String.concat mainCode (String.concat "\nconst $result = " (String.concat result ";\nconsole.log($result);\n"))))

  -- Generate standalone (runtime inlined)
  let generateStandalone = ir constructors ->
    let ctx = initContext constructors in
    let (result, finalCtx) = genExpr ir ctx in
    let codeLines = List.reverse finalCtx.lines in
    let mainCode = String.join "\n" codeLines in
    String.concat inlineRuntime (String.concat "\n// Generated code\n" (String.concat mainCode (String.concat "\nconst $result = " (String.concat result ";\nconsole.log($result);\n"))))

end
