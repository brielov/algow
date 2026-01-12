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

  let isValidJsChar c =
    Char.isAlphaNum c || c == '_' || c == '$'

  -- Sanitize identifier for JavaScript
  let sanitizeIdent s =
    let rec sanitize chars = match chars
      when Nil -> Nil
      when Cons c rest ->
        let c2 = if isValidJsChar c then c else '_' in
        Cons c2 (sanitize rest)
    end in
    String.fromList (sanitize (String.toList s))

  -- Convert Name to valid JavaScript identifier
  let mangleName name = match name
    when Name id original ->
      let base = sanitizeIdent original in
      base + "$" + Int.toString id
  end

  -- ===========================================================================
  -- Literal Generation
  -- ===========================================================================

  let escapeChar c =
    if c == '\n' then "\\n"
    else if c == '\r' then "\\r"
    else if c == '\t' then "\\t"
    else if c == '"' then "\\\""
    else if c == '\\' then "\\\\"
    else Char.toString c

  -- Escape string for JavaScript (tail-recursive with accumulator)
  let escapeString s =
    let rec escape acc chars = match chars
      when Nil -> acc
      when Cons c rest ->
        let escaped = escapeChar c in
        escape (acc + escaped) rest
    end in
    escape "" (String.toList s)

  let genLiteral lit = match lit
    when LInt n -> Int.toString n
    when LFloat f -> Float.toString f
    when LString s -> "\"" + escapeString s + "\""
    when LChar c -> "\"" + escapeChar c + "\""
    when LBool true -> "true"
    when LBool false -> "false"
  end

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
  -- Helper Functions for Generation
  -- ===========================================================================

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

  -- Default constructor representation: [tag, ...args]
  let genDefaultCon tag args =
    let argsCode = List.map genAtom args in
    "[" + Int.toString tag + (if List.isEmpty argsCode then "" else ",") + String.join "," argsCode + "]"

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

  -- ===========================================================================
  -- Pattern Binding Helpers (mutually recursive, needed by match generation)
  -- ===========================================================================

  let rec genConBindings scrutinee idx pats = match pats
    when Nil -> ""
    when Cons pat rest ->
      let access = scrutinee + "[" + Int.toString idx + "]" in
      genPatternBinding access pat + genConBindings scrutinee (idx + 1) rest
  end

  and genPatternBinding access pat = match pat
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

  and genTupleBindings access idx pats = match pats
    when Nil -> ""
    when Cons pat rest ->
      let elemAccess = access + "[" + Int.toString idx + "]" in
      genPatternBinding elemAccess pat + genTupleBindings access (idx + 1) rest
  end

  and genRecordBindings access fields =
    List.foldl (acc f -> match f
      when (name, pat) ->
        let fieldAccess = access + "." + name in
        acc + genPatternBinding fieldAccess pat
    end) "" fields

  -- Cons binding helper
  let genConsBindings scrutinee pats = match pats
    when Cons hPat (Cons tPat Nil) ->
      genPatternBinding (scrutinee + ".h") hPat +
      genPatternBinding (scrutinee + ".t") tPat
    when _ -> ""
  end

  -- ===========================================================================
  -- Helper Functions for Pattern Analysis
  -- ===========================================================================

  -- Check if a case is a catch-all (wildcard or variable)
  let isCatchAll c = match c
    when IRCase pat _ ->
      match pat
        when IRPWild -> true
        when IRPVar _ -> true
        when _ -> false
      end
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
      -- Foreign function access using $foreign runtime object
      let base = if mod == "" then name else "$foreign[\"" + mod + "\"][\"" + name + "\"]" in
      base + List.foldl (acc a -> acc + "(" + a + ")") "" argsCode

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
    -- Only add undefined fallback if there's no catch-all case
    let hasCatchAll = List.any isCatchAll cases in
    String.join "" branches + (if hasCatchAll then "" else "undefined")

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

  -- Minimal runtime - just $eq. $foreign is loaded from runtime.js or injected.
  let runtimeCode = "const $eq=(a)=>(b)=>{if(a===b)return true;if(a===null||b===null)return a===b;if(Array.isArray(a)&&Array.isArray(b)){if(a.length!==b.length)return false;for(let i=0;i<a.length;i++)if(!$eq(a[i])(b[i]))return false;return true;}if(typeof a==='object'&&typeof b==='object'){const ka=Object.keys(a),kb=Object.keys(b);if(ka.length!==kb.length)return false;for(const k of ka)if(!$eq(a[k])(b[k]))return false;return true;}return false;};const $foreign={String:{length:s=>s.length,concat:a=>b=>a+b,substring:st=>en=>s=>s.substring(st,en),charAt:i=>s=>i<0||i>=s.length?[0]:[1,s[i]],head:s=>s.length===0?[0]:[1,s[0]],tail:s=>s.length===0?'':s.slice(1),drop:n=>s=>s.slice(n),take:n=>s=>s.slice(0,n),isEmpty:s=>s.length===0,toList:s=>{let r=null;for(let i=s.length-1;i>=0;i--)r={h:s[i],t:r};return r;},fromList:l=>{let r='';while(l!==null){r+=l.h;l=l.t;}return r;},join:sep=>l=>{let a=[];while(l!==null){a.push(l.h);l=l.t;}return a.join(sep);},split:sep=>s=>{const a=s.split(sep);let r=null;for(let i=a.length-1;i>=0;i--)r={h:a[i],t:r};return r;}},Char:{isAlpha:c=>/[a-zA-Z]/.test(c),isDigit:c=>/[0-9]/.test(c),isAlphaNum:c=>/[a-zA-Z0-9]/.test(c),isSpace:c=>/\\s/.test(c),toString:c=>c,toCode:c=>c.charCodeAt(0),fromCode:n=>String.fromCharCode(n)},Int:{toString:n=>String(n),parse:s=>{const n=parseInt(s,10);return isNaN(n)?[0]:[1,n];}},Float:{toString:f=>String(f),parse:s=>{const f=parseFloat(s);return isNaN(f)?[0]:[1,f];}},IO:{readFile:p=>{try{return[1,require('fs').readFileSync(p,'utf8')];}catch(e){return[0,e.message];}},writeFile:p=>c=>{try{require('fs').writeFileSync(p,c);return[1,null];}catch(e){return[0,e.message];}},readDir:p=>{try{return[1,require('fs').readdirSync(p).reduce((r,f)=>({h:f,t:r}),null)];}catch(e){return[0,e.message];}},isDirectory:p=>{try{return require('fs').statSync(p).isDirectory();}catch(e){return false;}},args:_=>{const a=process.argv.slice(2);let r=null;for(let i=a.length-1;i>=0;i--)r={h:a[i],t:r};return r;},print:s=>{process.stdout.write(s);return null;},println:s=>{console.log(s);return null;}},List:{length:l=>{let n=0;while(l!==null){n++;l=l.t;}return n;},isEmpty:l=>l===null,append:a=>b=>{if(a===null)return b;let r={h:a.h,t:null},c=r;a=a.t;while(a!==null){c.t={h:a.h,t:null};c=c.t;a=a.t;}c.t=b;return r;},concat:ll=>{let r=null;while(ll!==null){r=$foreign.List.append(r)(ll.h);ll=ll.t;}return r;},map:f=>l=>{if(l===null)return null;let r={h:f(l.h),t:null},c=r;l=l.t;while(l!==null){c.t={h:f(l.h),t:null};c=c.t;l=l.t;}return r;},filter:f=>l=>{let r=null,last=null;while(l!==null){if(f(l.h)){const n={h:l.h,t:null};if(r===null)r=n;else last.t=n;last=n;}l=l.t;}return r;},foldl:f=>z=>l=>{while(l!==null){z=f(z)(l.h);l=l.t;}return z;},foldr:f=>z=>l=>{if(l===null)return z;const a=[];while(l!==null){a.push(l.h);l=l.t;}for(let i=a.length-1;i>=0;i--)z=f(a[i])(z);return z;},any:f=>l=>{while(l!==null){if(f(l.h))return true;l=l.t;}return false;},all:f=>l=>{while(l!==null){if(!f(l.h))return false;l=l.t;}return true;}},Maybe:{map:f=>m=>m[0]===0?m:[1,f(m[1])]},Debug:{log:x=>{console.log(x);return x;},trace:l=>x=>{console.log(l+':',x);return x;},panic:m=>{throw new Error(m);}},Map:{empty:new Map(),singleton:k=>v=>new Map([[k,v]]),insert:k=>v=>m=>new Map(m).set(k,v),lookup:k=>m=>m.has(k)?[1,m.get(k)]:[0],delete:k=>m=>{const m2=new Map(m);m2.delete(k);return m2;},member:k=>m=>m.has(k),size:m=>m.size,keys:m=>{let r=null;for(const k of[...m.keys()].reverse())r={h:k,t:r};return r;},values:m=>{let r=null;for(const v of[...m.values()].reverse())r={h:v,t:r};return r;},toList:m=>{let r=null;for(const[k,v]of[...m.entries()].reverse())r={h:[k,v],t:r};return r;},fromList:l=>{const m=new Map();while(l!==null){const[k,v]=l.h;m.set(k,v);l=l.t;}return m;}},Set:{empty:new Set(),singleton:v=>new Set([v]),insert:v=>s=>new Set(s).add(v),delete:v=>s=>{const s2=new Set(s);s2.delete(v);return s2;},member:v=>s=>s.has(v),size:s=>s.size,toList:s=>{let r=null;for(const v of[...s].reverse())r={h:v,t:r};return r;},fromList:l=>{const s=new Set();while(l!==null){s.add(l.h);l=l.t;}return s;},union:s1=>s2=>s1.union(s2),intersect:s1=>s2=>s1.intersection(s2),difference:s1=>s2=>s1.difference(s2)}};\n"

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
