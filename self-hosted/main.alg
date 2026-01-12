-- =============================================================================
-- Main Module
-- Entry point for the Algow compiler
-- Reference: SPEC.md Appendix A
-- =============================================================================

module Main
  use Types (..)
  use Lexer (..)
  use Parser (..)
  use Desugar (..)
  use Resolve (..)
  use Infer (..)
  use Patterns (..)
  use IR (..)
  use Lower (..)
  use Optimize (..)
  use Codegen (..)
  use Unit (Unit)

  -- Helper: flatMap is not in prelude, so define it
  let flatMap f xs = List.concat (List.map f xs)

  -- ===========================================================================
  -- Type Inference for Programs
  -- ===========================================================================

  -- Convert Core.Type to inference Type
  let rec coreTypeToType cty = match cty
    when CTVar name -> TVar name
    when CTCon name -> TCon name
    when CTFun t1 t2 -> TFun (coreTypeToType t1) (coreTypeToType t2)
    when CTApp t1 t2 -> TApp (coreTypeToType t1) (coreTypeToType t2)
    when CTTuple ts -> TTuple (List.map coreTypeToType ts)
    when CTRecord fields ->
      TRecord (List.map (f -> match f
        when CTypeField name t -> TField name (coreTypeToType t)
      end) fields)
  end

  -- Convert type to scheme (no generalization)
  let typeToScheme ty = Forall Nil (coreTypeToType ty)

  -- Collect type variables from a type
  let rec collectTypeVars ty = match ty
    when TVar v -> [v]
    when TCon _ -> []
    when TFun t1 t2 -> List.append (collectTypeVars t1) (collectTypeVars t2)
    when TApp t1 t2 -> List.append (collectTypeVars t1) (collectTypeVars t2)
    when TTuple ts -> flatMap collectTypeVars ts
    when TRecord fields ->
      flatMap (f -> match f when TField _ t -> collectTypeVars t end) fields
  end

  -- Build the result type from type name and params
  let buildResultType name params =
    List.foldl (acc p -> TApp acc (TVar p)) (TCon name) params

  -- Build constructor type from argument types
  let buildConType argTypes resultType =
    List.foldr (argTy acc -> TFun (coreTypeToType argTy) acc) resultType argTypes

  -- Add constructor types to environment
  let addConstructorTypes env typeName params constructors =
    let resultType = buildResultType typeName params in
    List.foldl (acc c -> match c
      when CConDef name argTypes ->
        let conType = buildConType argTypes resultType in
        let vars = collectTypeVars conType in
        let scheme = Forall vars conType in
        Infer.extendTypeEnv name scheme acc
    end) env constructors

  -- Create fresh type variables for names
  let rec createFreshTvars env names counter = match names
    when Nil -> (Nil, env)
    when Cons n rest ->
      let tv = TVar ("t" + Int.toString counter) in
      let env2 = Infer.extendTypeEnv n (Forall Nil tv) env in
      let (tvs, env3) = createFreshTvars env2 rest (counter + 1) in
      (Cons tv tvs, env3)
  end

  -- Add generalized types to environment
  let rec addGeneralizedTypes env names types = match (names, types)
    when (Nil, _) -> env
    when (_, Nil) -> env
    when (Cons n restN, Cons t restT) ->
      let scheme = Infer.generalize env t in
      let env2 = Infer.extendTypeEnv n scheme env in
      addGeneralizedTypes env2 restN restT
  end

  -- Process a single declaration
  let rec processDecl env decl = match decl
    when CDeclType name params constructors ->
      -- Add constructor types to environment
      let env2 = addConstructorTypes env name params constructors in
      (env2, Nil)

    when CDeclTypeAlias name params ty ->
      -- Type aliases don't add values to the environment
      (env, Nil)

    when CDeclModule modName innerDecls ->
      -- Process module's inner declarations
      buildTypeEnv env innerDecls

    when CDeclLet name expr ->
      match Infer.inferExpr env expr
        when Left err -> (env, [err])
        when Right ty ->
          let scheme = Infer.generalize env ty in
          let env2 = Infer.extendTypeEnv name scheme env in
          (env2, Nil)
      end

    when CDeclLetRec bindings ->
      -- Infer types for recursive bindings
      let names = List.map (b -> match b when CBinding n _ -> n end) bindings in
      let exprs = List.map (b -> match b when CBinding _ e -> e end) bindings in
      -- Create fresh type variables for all bindings
      let (tvs, env2) = createFreshTvars env names 0 in
      -- Infer types with the extended environment
      let results = List.map (Infer.inferExpr env2) exprs in
      let errors = flatMap (r -> match r
        when Left e -> [e]
        when Right _ -> []
      end) results in
      if not (List.isEmpty errors) then
        (env, errors)
      else
        -- Generalize and add to environment
        let types = flatMap (r -> match r
          when Left _ -> []
          when Right t -> [t]
        end) results in
        let env3 = addGeneralizedTypes env names types in
        (env3, Nil)

    when CDeclForeign name mod jsName ty ->
      -- Foreign declarations provide their own type
      let scheme = typeToScheme ty in
      let env2 = Infer.extendTypeEnv name scheme env in
      (env2, Nil)

    when CDeclUse _ -> (env, Nil)
  end

  -- Build type environment from declarations
  and buildTypeEnv env decls = match decls
    when Nil -> (env, Nil)
    when Cons d rest ->
      let (env2, errs) = processDecl env d in
      let (env3, moreErrs) = buildTypeEnv env2 rest in
      (env3, List.append errs moreErrs)
  end

  let inferProgram decls maybeExpr =
    -- Build initial type environment from declarations
    let (env, errors) = buildTypeEnv Infer.emptyTypeEnv decls in
    if not (List.isEmpty errors) then
      Left (String.join "\n" errors)
    else
      match maybeExpr
        when Nothing -> Right env
        when Just expr ->
          match Infer.inferExpr env expr
            when Left err -> Left err
            when Right ty -> Right env
          end
      end

  -- ===========================================================================
  -- Compiler Pipeline
  -- ===========================================================================

  -- The complete compilation pipeline:
  -- Source -> Tokens -> Surface AST -> Core AST -> Resolved -> Typed ->
  -- Pattern-compiled -> IR -> Optimized IR -> JavaScript

  type CompileResult
    = CompileOk string            -- Generated JavaScript
    | CompileErr string           -- Error message

  -- Main compilation function
  let compile source =
    -- 1. Lexing
    let tokens = Lexer.tokenize source in

    -- 2. Parsing
    match Parser.parse tokens
      when ParseErr msg _ -> CompileErr ("Parse error: " + msg)
      when ParseOk (SProgram decls maybeExpr) _ ->

        -- 3. Desugaring
        let (coreDecls, coreExpr) = Desugar.desugarProgram decls maybeExpr in

        -- 4. Name resolution
        match Resolve.resolveProgram coreDecls coreExpr
          when Left err -> CompileErr ("Resolution error: " + err)
          when Right (resolvedDecls, resolvedExpr) ->

            -- 5. Type inference
            match inferProgram resolvedDecls resolvedExpr
              when Left err -> CompileErr ("Type error: " + err)
              when Right typedProgram ->

                -- 6. Pattern compilation
                let (patternDecls, patternExpr) = Patterns.compilePatternsProgram resolvedDecls resolvedExpr in

                -- 7. Lowering to IR
                let irProgram = Lower.lowerProgram patternDecls patternExpr in

                -- 8. Optimization
                let optimizedIR = Optimize.optimizeProgramDefault irProgram in

                -- 9. Code generation
                let jsCode = Codegen.generate optimizedIR in

                CompileOk jsCode
            end
        end
    end

  -- ===========================================================================
  -- Type Checking Only
  -- ===========================================================================

  let typeCheck source =
    let tokens = Lexer.tokenize source in
    match Parser.parse tokens
      when ParseErr msg _ -> Left ("Parse error: " + msg)
      when ParseOk (SProgram decls maybeExpr) _ ->
        let (coreDecls, coreExpr) = Desugar.desugarProgram decls maybeExpr in
        match Resolve.resolveProgram coreDecls coreExpr
          when Left err -> Left ("Resolution error: " + err)
          when Right (resolvedDecls, resolvedExpr) ->
            match inferProgram resolvedDecls resolvedExpr
              when Left err -> Left ("Type error: " + err)
              when Right typedProgram -> Right typedProgram
            end
        end
    end

  -- ===========================================================================
  -- CLI Interface
  -- ===========================================================================

  -- CliArgs: input, typeCheckOnly, compileOnly, emitIR, help
  type CliArgs = CliArgs string bool bool bool bool

  let defaultArgs = CliArgs "" false false false false

  let parseArgs args =
    let rec parse acc rest = match rest
      when Nil -> acc
      when Cons arg rest2 ->
        match acc
          when CliArgs input tc co ir help ->
            if arg == "-t" || arg == "--type" then
              parse (CliArgs input true co ir help) rest2
            else if arg == "-c" || arg == "--compile" then
              parse (CliArgs input tc true ir help) rest2
            else if arg == "--emit-ir" then
              parse (CliArgs input tc co true help) rest2
            else if arg == "-h" || arg == "--help" then
              parse (CliArgs input tc co ir true) rest2
            else
              parse (CliArgs arg tc co ir help) rest2
        end
    end in
    parse defaultArgs args

  let showHelp =
    "Algow Compiler (self-hosted)\n\n" +
    "Usage: algow [options] <file.alg>\n\n" +
    "Options:\n" +
    "  -t, --type     Type check only (show inferred types)\n" +
    "  -c, --compile  Compile to JavaScript\n" +
    "  --emit-ir      Emit IR (for debugging)\n" +
    "  -h, --help     Show this help message\n"

  -- ===========================================================================
  -- Main Entry Point
  -- ===========================================================================

  -- This would be called by the runtime with command line arguments
  -- For now, we expose the compile function as the main entry point

  let main args =
    let cliArgs = parseArgs args in
    match cliArgs
      when CliArgs input typeCheckOnly compileOnly emitIR help ->
        if help then
          showHelp
        else if input == "" then
          "Error: No input file specified\n" + showHelp
        else
          -- Read file and compile
          match IO.readFile input
            when Left _ -> "Error: Could not read file " + input
            when Right source ->
              match compile source
                when CompileOk js -> js
                when CompileErr err -> "Error: " + err
              end
          end
    end

  -- Helper: filter list
  let rec filter pred xs = match xs
    when Nil -> Nil
    when Cons x rest -> if pred x then Cons x (filter pred rest) else filter pred rest
  end

  -- Helper: check if string ends with suffix
  let endsWith suffix str =
    let suffixLen = String.length suffix in
    let strLen = String.length str in
    if strLen < suffixLen then false
    else String.substring (strLen - suffixLen) strLen str == suffix

  -- Helper: combine path components (handles trailing slash)
  let pathJoin dir file =
    if endsWith "/" dir then dir + file
    else dir + "/" + file

  -- Load and parse a single file, returning (filename, program) or error
  let loadAndParse filepath =
    match IO.readFile filepath
      when Left _ -> Left ("Could not read: " + filepath)
      when Right source ->
        let tokens = Lexer.tokenize source in
        match Parser.parse tokens
          when ParseErr msg _ -> Left ("Parse error in " + filepath + ": " + msg)
          when ParseOk prog _ -> Right (filepath, prog)
        end
    end

  -- Merge multiple parsed programs into one
  let rec mergePrograms progs mainProg = match progs
    when Nil -> mainProg
    when Cons (_, SProgram decls _) rest ->
      match mainProg
        when SProgram mainDecls mainExpr ->
          mergePrograms rest (SProgram (List.append decls mainDecls) mainExpr)
      end
  end

  -- Find main.alg program from list
  let rec findMain progs = match progs
    when Nil -> Nothing
    when Cons (filepath, prog) rest ->
      if endsWith "main.alg" filepath then Just (filepath, prog)
      else findMain rest
  end

  -- Compile a single file
  let compileSingle source =
    let tokens = Lexer.tokenize source in
    match Parser.parse tokens
      when ParseErr msg _ -> "Parse error: " + msg
      when ParseOk prog _ ->
        let (coreDecls, coreExpr) = Desugar.desugarProgram prog in
        match Resolve.resolveProgram coreDecls coreExpr
          when Left err -> "Resolution error: " + err
          when Right (resolvedDecls, resolvedExpr) ->
            let (patternDecls, patternExpr) = Patterns.compilePatternsProgram resolvedDecls resolvedExpr in
            let irProgram = Lower.lowerProgram patternDecls patternExpr in
            let optimizedIR = Optimize.optimizeProgramDefault irProgram in
            Codegen.generate optimizedIR
        end
    end

  -- Compile merged program
  let compileMerged prog =
    let (coreDecls, coreExpr) = Desugar.desugarProgram prog in
    match Resolve.resolveProgram coreDecls coreExpr
      when Left err -> "Resolution error: " + err
      when Right (resolvedDecls, resolvedExpr) ->
        let (patternDecls, patternExpr) = Patterns.compilePatternsProgram resolvedDecls resolvedExpr in
        let irProgram = Lower.lowerProgram patternDecls patternExpr in
        let optimizedIR = Optimize.optimizeProgramDefault irProgram in
        Codegen.generate optimizedIR
    end

  -- Compile a directory of .alg files
  let compileDirectory dir =
    match IO.readDir dir
      when Left _ -> "Error: Could not read directory " + dir
      when Right files ->
        let algFiles = filter (endsWith ".alg") files in
        if List.isEmpty algFiles then "Error: No .alg files found in " + dir
        else
          -- Load and parse all files
          let loadResults = List.map (f -> loadAndParse (pathJoin dir f)) algFiles in
          let errors = flatMap (r -> match r when Left e -> [e] when Right _ -> [] end) loadResults in
          if not (List.isEmpty errors) then
            match errors when Cons e _ -> e when Nil -> "Unknown error" end
          else
            let parsed = flatMap (r -> match r when Left _ -> [] when Right p -> [p] end) loadResults in
            match findMain parsed
              when Nothing -> "Error: No main.alg found in " + dir
              when Just (_, mainProg) ->
                let otherProgs = filter (p -> match p when (f, _) -> not (endsWith "main.alg" f) end) parsed in
                let merged = mergePrograms otherProgs mainProg in
                compileMerged merged
            end
    end

  -- Entry point that reads args and runs the compiler
  let run dummy =
    let args = IO.args Unit in
    match args
      when Nil -> showHelp
      when Cons "-h" _ -> showHelp
      when Cons "--help" _ -> showHelp
      when Cons path _ ->
        if IO.isDirectory path then
          compileDirectory path
        else
          match IO.readFile path
            when Left _ -> "Error: Could not read file " + path
            when Right source -> compileSingle source
          end
    end

end

-- Entry point (pass dummy value since run ignores it)
Main.run 0
