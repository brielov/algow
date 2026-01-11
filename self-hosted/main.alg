-- =============================================================================
-- ALGOW SELF-HOSTED COMPILER - MAIN ENTRY POINT
-- =============================================================================
--
-- This is the main module that ties together all compiler phases:
-- 1. Lexer: Source text -> Tokens
-- 2. Parser: Tokens -> AST
-- 3. Desugar: AST -> Core AST
-- 4. Resolve: Name resolution
-- 5. Infer: Type inference (Algorithm W)
-- 6. Lower: AST -> ANF IR
-- 7. Patterns: Pattern compilation
-- 8. Optimize: IR optimization
-- 9. Codegen: IR -> JavaScript
--
-- Reference: SPEC.md
--
-- =============================================================================

module Main

  use Parser (parse, Program, Diagnostic)
  use Desugar (desugarProgram)
  use Resolve (resolveProgram, emptyEnv)
  use Infer (inferProgram, Type, emptyTypeEnv)
  use Lower (lowerProgram)
  use Optimize (optimize)
  use Codegen (generateStandalone)
  use Maybe (Just, Nothing)
  use Either (Left, Right)
  use List (Nil, Cons, map, concat, find)
  use IO (readFile, printLine, args, FileNotFound, PermissionDenied, IsDirectory, AlreadyExists, UnknownError)

  -- =========================================================================
  -- MULTI-FILE TYPES
  -- =========================================================================

  type FileSource = {
    filename : string,
    source : string
  }

  type ParsedFile = {
    filename : string,
    program : Program,
    diagnostics : List Diagnostic,
    source : string
  }

  -- =========================================================================
  -- FILE OPERATIONS
  -- =========================================================================

  -- Load a single file
  let loadFile = filename ->
    match readFile filename
      when Left err -> Left (String.concat "Failed to read " (String.concat filename ": " (ioErrorToString err)))
      when Right source -> Right { filename = filename, source = source }
    end

  and ioErrorToString = err -> match err
    when FileNotFound path -> String.concat "File not found: " path
    when PermissionDenied path -> String.concat "Permission denied: " path
    when IsDirectory path -> String.concat "Is a directory: " path
    when AlreadyExists path -> String.concat "Already exists: " path
    when UnknownError msg -> msg
  end

  -- Load multiple files
  let rec loadFiles = filenames -> match filenames
    when Nil -> Right Nil
    when Cons filename rest ->
      match loadFile filename
        when Left err -> Left err
        when Right file ->
          match loadFiles rest
            when Left err -> Left err
            when Right files -> Right (Cons file files)
          end
      end
  end

  -- Parse a file source
  let parseFile = file ->
    let (program, diagnostics) = parse file.source in
    { filename = file.filename, program = program, diagnostics = diagnostics, source = file.source }

  -- Parse multiple file sources
  let parseFiles = files -> map parseFile files

  -- =========================================================================
  -- ERROR COLLECTION
  -- =========================================================================

  -- Collect parse errors from all files
  let collectParseErrors = parsedFiles ->
    let collectFileErrors = file ->
      map (err -> { file = file.filename, error = err }) file.diagnostics
    in
    concat (map collectFileErrors parsedFiles)

  -- =========================================================================
  -- COMPILER PIPELINE
  -- =========================================================================

  type CompileResult
    = CompileOk string           -- Success with generated JS
    | CompileError string        -- Failure with error message

  -- Full compilation: source -> JavaScript (single file)
  let compile = source ->
    -- Phase 1 & 2: Lexing and Parsing
    let (program, diagnostics) = parse source in
    match diagnostics
      when Cons diag _ -> CompileError (String.concat "Parse error: " diag.message)
      when Nil ->
        -- Phase 3: Desugar
        let coreProgram = desugarProgram program in

        -- Phase 4: Name resolution
        let (resolvedProgram, resolveErrors) = resolveProgram coreProgram emptyEnv in
        match resolveErrors
          when Cons err _ -> CompileError (String.concat "Resolution error: " err)
          when Nil ->
            -- Phase 5: Type inference (also collects constructors)
            let inferResult = inferProgram resolvedProgram emptyTypeEnv in
            match inferResult.errors
              when Cons err _ -> CompileError (String.concat "Type error: " err)
              when Nil ->
                match (resolvedProgram.expr, inferResult.exprType)
                  when (Just expr, Just typ) ->
                    -- Phase 6: Lower to IR
                    let ir = lowerProgram expr typ in

                    -- Phase 7: Optimize
                    let optimized = optimize ir in

                    -- Phase 8: Code generation (uses inferred constructors)
                    let js = generateStandalone optimized inferResult.constructors in
                    CompileOk js
                  when _ -> CompileError "No expression to compile"
                end
            end
        end
    end

  -- Compile multiple files
  let compileFiles = filenames ->
    match loadFiles filenames
      when Left err -> CompileError err
      when Right files ->
        let parsedFiles = parseFiles files in
        let parseErrors = collectParseErrors parsedFiles in
        match parseErrors
          when Cons err _ -> CompileError (String.concat "Parse error in " (String.concat err.file (String.concat ": " err.error.message)))
          when Nil ->
            -- Merge all modules and expressions from parsed files
            -- For now, just compile the first file that has an expression
            let fileWithExpr = find (f -> match f.program.expr when Just _ -> true when Nothing -> false end) parsedFiles in
            match fileWithExpr
              when Nothing -> CompileError "No expression to compile in any file"
              when Just file -> compile file.source
            end
        end
    end

  -- =========================================================================
  -- TYPE CHECK ONLY
  -- =========================================================================

  type TypeCheckResult
    = TypeOk Type
    | TypeError string

  let typeCheck = source ->
    let (program, diagnostics) = parse source in
    match diagnostics
      when Cons diag _ -> TypeError (String.concat "Parse error: " diag.message)
      when Nil ->
        let coreProgram = desugarProgram program in
        let (resolvedProgram, resolveErrors) = resolveProgram coreProgram emptyEnv in
        match resolveErrors
          when Cons err _ -> TypeError (String.concat "Resolution error: " err)
          when Nil ->
            let inferResult = inferProgram resolvedProgram emptyTypeEnv in
            match inferResult.errors
              when Cons err _ -> TypeError err
              when Nil ->
                match inferResult.exprType
                  when Just typ -> TypeOk typ
                  when Nothing -> TypeError "No expression to type check"
                end
            end
        end
    end

  -- =========================================================================
  -- EMIT IR (FOR DEBUGGING)
  -- =========================================================================

  let emitIR = source ->
    let (program, diagnostics) = parse source in
    match diagnostics
      when Cons diag _ -> Left (String.concat "Parse error: " diag.message)
      when Nil ->
        let coreProgram = desugarProgram program in
        let (resolvedProgram, resolveErrors) = resolveProgram coreProgram emptyEnv in
        match resolveErrors
          when Cons err _ -> Left (String.concat "Resolution error: " err)
          when Nil ->
            let inferResult = inferProgram resolvedProgram emptyTypeEnv in
            match inferResult.errors
              when Cons err _ -> Left err
              when Nil ->
                match (resolvedProgram.expr, inferResult.exprType)
                  when (Just expr, Just typ) ->
                    let ir = lowerProgram expr typ in
                    let optimized = optimize ir in
                    Right optimized
                  when _ -> Left "No expression"
                end
            end
        end
    end

  -- =========================================================================
  -- VERSION INFO
  -- =========================================================================

  let version = "0.1.0 (self-hosted)"

  let banner = "Algow Compiler v0.1.0 (self-hosted)
A statically typed functional language with Hindley-Milner type inference.

Usage:
  algow <file.alg>              Compile a file to JavaScript
  algow -t <file.alg>           Type check only
  algow -c <file.alg>           Compile to JavaScript (explicit)
  algow --emit-ir <file.alg>    Emit IR (for debugging)
  algow -e \"expr\"               Compile inline expression
  algow --help                  Show this help message
  algow --version               Show version
"

  -- =========================================================================
  -- COMMAND-LINE INTERFACE
  -- =========================================================================

  -- Process command line arguments
  let rec processArgs = arguments -> match arguments
    -- Help
    when Cons "--help" _ -> printLine banner
    when Cons "-h" _ -> printLine banner

    -- Version
    when Cons "--version" _ -> printLine version
    when Cons "-v" _ -> printLine version

    -- Type check only
    when Cons "-t" (Cons filename _) ->
      match loadFile filename
        when Left err -> printLine (String.concat "Error: " err)
        when Right file ->
          match typeCheck file.source
            when TypeError err -> printLine (String.concat "Type error: " err)
            when TypeOk typ -> printLine "Type check passed"
          end
      end

    -- Compile to JS
    when Cons "-c" (Cons filename _) ->
      match loadFile filename
        when Left err -> printLine (String.concat "Error: " err)
        when Right file ->
          match compile file.source
            when CompileError err -> printLine (String.concat "Compile error: " err)
            when CompileOk js -> printLine js
          end
      end

    -- Emit IR
    when Cons "--emit-ir" (Cons filename _) ->
      match loadFile filename
        when Left err -> printLine (String.concat "Error: " err)
        when Right file ->
          match emitIR file.source
            when Left err -> printLine (String.concat "Error: " err)
            when Right ir -> printLine "IR generated (pretty-printing not implemented)"
          end
      end

    -- Inline expression
    when Cons "-e" (Cons expr _) ->
      match compile expr
        when CompileError err -> printLine (String.concat "Compile error: " err)
        when CompileOk js -> printLine js
      end

    -- Single file (default: compile)
    when Cons filename Nil ->
      match loadFile filename
        when Left err -> printLine (String.concat "Error: " err)
        when Right file ->
          match compile file.source
            when CompileError err -> printLine (String.concat "Compile error: " err)
            when CompileOk js -> printLine js
          end
      end

    -- Multiple files
    when Cons filename rest ->
      let allFiles = Cons filename rest in
      match compileFiles allFiles
        when CompileError err -> printLine (String.concat "Compile error: " err)
        when CompileOk js -> printLine js
      end

    -- No arguments
    when Nil -> printLine banner
  end

  -- Entry point
  let main = processArgs (args Unit)

end
