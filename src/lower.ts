/**
 * AST to IR Lowering (ANF Conversion)
 *
 * Transforms the typed AST into A-Normal Form (ANF) IR.
 *
 * Key transformations:
 * 1. All intermediate values are bound to names (no nested complex expressions)
 * 2. Arguments to operations become atomic (literals or variables)
 * 3. Type information is preserved from the type checker
 *
 * Example transformation:
 *   f (g x) (h y)
 * becomes:
 *   let t0 = g x in
 *   let t1 = h y in
 *   let t2 = f t0 in
 *   t2 t1
 */

import * as ast from "./ast";
import * as ir from "./ir";
import {
  applySubst,
  type CheckOutput,
  type ForeignMap,
  type ModuleTypeEnv,
  type Subst,
  type Type,
  type TypeEnv,
} from "./checker";

// Re-export ForeignMap for consumers
export type { ForeignMap };

/** Helper for exhaustive switch checking - TypeScript will error if called with non-never */
const assertNever = (x: never): never => {
  throw new Error(`Unexpected value: ${JSON.stringify(x)}`);
};

// =============================================================================
// LOWERING CONTEXT
// =============================================================================

type LowerContext = {
  /** Fresh variable counter for generating unique names */
  varCounter: number;
  /** Type environment - maps names to type schemes */
  typeEnv: TypeEnv;
  /** Substitution from type checker for resolving type variables */
  subst: Subst;
  /** Direct span to type mapping from type checker */
  spanTypes: ReadonlyMap<string, Type>;
  /** Map from local names to foreign function info */
  foreignFunctions: ForeignMap;
  /** Module environment for resolving qualified access */
  moduleEnv: ModuleTypeEnv;
};

const createContext = (
  typeEnv: TypeEnv,
  checkOutput: CheckOutput,
  foreignFunctions: ForeignMap = new Map(),
  moduleEnv: ModuleTypeEnv = new Map(),
): LowerContext => ({
  varCounter: 0,
  typeEnv: new Map(typeEnv),
  subst: checkOutput.subst,
  spanTypes: checkOutput.spanTypes,
  foreignFunctions,
  moduleEnv,
});

/** Generate a fresh variable name */
const freshVar = (ctx: LowerContext, prefix = "_t"): string => {
  return `${prefix}${ctx.varCounter++}`;
};

/** Extend the type environment with a new binding */
const extendEnv = (ctx: LowerContext, name: string, type: Type): void => {
  ctx.typeEnv.set(name, { vars: [], constraints: [], type });
};

// =============================================================================
// TYPE RESOLUTION
// =============================================================================

/** Look up a type by span from the type checker's output */
const lookupSpanType = (ctx: LowerContext, span: ast.Span | undefined): Type | null => {
  if (!span) return null;
  const key = `${span.start}:${span.end}`;
  const type = ctx.spanTypes.get(key);
  return type ? applySubst(ctx.subst, type) : null;
};

/** Look up a variable's type in the environment */
const lookupType = (ctx: LowerContext, name: string): Type => {
  const scheme = ctx.typeEnv.get(name);
  if (!scheme) {
    throw new Error(`Unknown variable during lowering: ${name}`);
  }
  // Instantiate the scheme (for simplicity, just return the type)
  // In a full implementation, we'd substitute fresh type variables
  return applySubst(ctx.subst, scheme.type);
};

/** Get the return type of a function type (or placeholder if unknown) */
const getReturnType = (type: Type): Type => {
  if (type.kind === "TFun") {
    return type.ret;
  }
  // For type variables or unknown types, return a placeholder
  // This happens in recursive bindings before the type is fully known
  return { kind: "TVar", name: "_return" };
};

/** Get field type from a record type */
const getFieldType = (type: Type, field: string): Type => {
  if (type.kind === "TRecord") {
    const fieldType = type.fields.get(field);
    if (fieldType) {
      return fieldType;
    }
  }
  throw new Error(`Expected record type with field ${field}, got ${type.kind}`);
};

// =============================================================================
// BUILT-IN TYPES
// =============================================================================

const tInt: Type = { kind: "TCon", name: "int" };
const tFloat: Type = { kind: "TCon", name: "float" };
const tStr: Type = { kind: "TCon", name: "string" };
const tChar: Type = { kind: "TCon", name: "char" };
const tBool: Type = { kind: "TCon", name: "boolean" };

// =============================================================================
// ANF CONVERSION
// =============================================================================

/**
 * Result of normalizing an expression.
 * Contains bindings to prepend and the atomic result.
 */
type NormalizeResult = {
  /** Let bindings that need to be prepended */
  bindings: Array<{ name: string; binding: ir.IRBinding }>;
  /** The atomic result */
  atom: ir.IRAtom;
};

/**
 * Normalize an expression to an atom.
 * If already atomic, return it directly.
 * Otherwise, bind to a fresh variable and return the variable.
 */
const normalize = (ctx: LowerContext, expr: ast.Expr): NormalizeResult => {
  // Check if expression is already atomic
  switch (expr.kind) {
    case "Int":
      return { bindings: [], atom: ir.irLit(expr.value, tInt) };
    case "Float":
      return { bindings: [], atom: ir.irLit(expr.value, tFloat) };
    case "Str":
      return { bindings: [], atom: ir.irLit(expr.value, tStr) };
    case "Char":
      return { bindings: [], atom: ir.irLit(expr.value, tChar) };
    case "Bool":
      return { bindings: [], atom: ir.irLit(expr.value, tBool) };
    case "Var": {
      const type = lookupType(ctx, expr.name);
      // Check if this is a foreign function
      const foreignInfo = ctx.foreignFunctions.get(expr.name);
      if (foreignInfo) {
        return { bindings: [], atom: ir.irForeignVar(foreignInfo.module, foreignInfo.name, type) };
      }
      return { bindings: [], atom: ir.irVar(expr.name, type) };
    }
  }

  // Complex expression - lower it and bind to fresh variable
  const lowered = lowerExpr(ctx, expr);

  // If the lowered expression is just an atom, return it directly
  if (lowered.kind === "IRAtomExpr") {
    return { bindings: [], atom: lowered.atom };
  }

  // Extract the binding from the lowered expression
  const name = freshVar(ctx);
  const type = lowered.type;
  extendEnv(ctx, name, type);

  // Unwrap the lowered expression to get the binding
  const { bindings, finalBinding } = extractBindings(lowered);

  return {
    bindings: [...bindings, { name, binding: finalBinding }],
    atom: ir.irVar(name, type),
  };
};

/**
 * Extract bindings from an IR expression.
 * Returns all let bindings and the final binding.
 */
const extractBindings = (
  expr: ir.IRExpr,
): { bindings: Array<{ name: string; binding: ir.IRBinding }>; finalBinding: ir.IRBinding } => {
  const bindings: Array<{ name: string; binding: ir.IRBinding }> = [];
  let current = expr;

  // Collect all let bindings
  while (current.kind === "IRLet" || current.kind === "IRLetRec") {
    if (current.kind === "IRLet") {
      bindings.push({ name: current.name, binding: current.binding });
    } else {
      // IRLetRec has multiple bindings
      for (const b of current.bindings) {
        bindings.push({ name: b.name, binding: b.binding });
      }
    }
    current = current.body;
  }

  // The final expression should be an atom
  if (current.kind !== "IRAtomExpr") {
    throw new Error("Expected atom at end of ANF expression");
  }

  // If we have bindings, the last one is the "final" binding
  // Otherwise, wrap the atom as an atom binding
  if (bindings.length > 0) {
    const lastBinding = bindings.pop()!;
    return { bindings, finalBinding: lastBinding.binding };
  }

  return { bindings: [], finalBinding: ir.irAtomBinding(current.atom) };
};

/**
 * Normalize multiple expressions, collecting all bindings.
 */
const normalizeMany = (
  ctx: LowerContext,
  exprs: readonly ast.Expr[],
): { bindings: Array<{ name: string; binding: ir.IRBinding }>; atoms: ir.IRAtom[] } => {
  const allBindings: Array<{ name: string; binding: ir.IRBinding }> = [];
  const atoms: ir.IRAtom[] = [];

  for (const expr of exprs) {
    const result = normalize(ctx, expr);
    allBindings.push(...result.bindings);
    atoms.push(result.atom);
  }

  return { bindings: allBindings, atoms };
};

/**
 * Wrap an expression with let bindings.
 */
const wrapWithBindings = (
  bindings: Array<{ name: string; binding: ir.IRBinding }>,
  body: ir.IRExpr,
): ir.IRExpr => {
  let result = body;
  for (let i = bindings.length - 1; i >= 0; i--) {
    const { name, binding } = bindings[i]!;
    result = ir.irLet(name, binding, result);
  }
  return result;
};

// =============================================================================
// EXPRESSION LOWERING
// =============================================================================

/**
 * Lower an AST expression to IR.
 */
const lowerExpr = (ctx: LowerContext, expr: ast.Expr): ir.IRExpr => {
  switch (expr.kind) {
    case "Int":
      return ir.irAtomExpr(ir.irLit(expr.value, tInt));

    case "Float":
      return ir.irAtomExpr(ir.irLit(expr.value, tFloat));

    case "Str":
      return ir.irAtomExpr(ir.irLit(expr.value, tStr));

    case "Char":
      return ir.irAtomExpr(ir.irLit(expr.value, tChar));

    case "Bool":
      return ir.irAtomExpr(ir.irLit(expr.value, tBool));

    case "Var": {
      const type = lookupType(ctx, expr.name);
      // Check if this is a foreign function
      const foreignInfo = ctx.foreignFunctions.get(expr.name);
      if (foreignInfo) {
        return ir.irAtomExpr(ir.irForeignVar(foreignInfo.module, foreignInfo.name, type));
      }
      return ir.irAtomExpr(ir.irVar(expr.name, type));
    }

    case "Let":
      return lowerLet(ctx, expr);

    case "LetRec":
      return lowerLetRec(ctx, expr);

    case "Abs":
      return lowerAbs(ctx, expr);

    case "App":
      return lowerApp(ctx, expr);

    case "BinOp":
      return lowerBinOp(ctx, expr);

    case "If":
      return lowerIf(ctx, expr);

    case "Tuple":
      return lowerTuple(ctx, expr);

    case "Record":
      return lowerRecord(ctx, expr);

    case "RecordUpdate":
      return lowerRecordUpdate(ctx, expr);

    case "FieldAccess":
      return lowerFieldAccess(ctx, expr);

    case "TupleIndex":
      return lowerTupleIndex(ctx, expr);

    case "Match":
      return lowerMatch(ctx, expr);

    case "QualifiedVar": {
      // Look up the module to check if this is a foreign function
      const mod = ctx.moduleEnv.get(expr.moduleName);
      if (mod) {
        // Check if this is a foreign function in that module
        if (mod.foreignNames.has(expr.member)) {
          // Get the type from the module's type environment
          const scheme = mod.typeEnv.get(expr.member);
          if (scheme) {
            const type = applySubst(ctx.subst, scheme.type);
            return ir.irAtomExpr(ir.irForeignVar(expr.moduleName, expr.member, type));
          }
        }
        // Non-foreign function in the module - use the qualified name
        const scheme = mod.typeEnv.get(expr.member);
        if (scheme) {
          const qualifiedName = `${expr.moduleName}.${expr.member}`;
          return ir.irAtomExpr(ir.irVar(qualifiedName, applySubst(ctx.subst, scheme.type)));
        }
      }
      // Fall back to checking the local type environment (for imported names)
      const scheme = ctx.typeEnv.get(expr.member);
      if (scheme) {
        // Check if this is an imported foreign function
        const foreignInfo = ctx.foreignFunctions.get(expr.member);
        if (foreignInfo && foreignInfo.module === expr.moduleName) {
          return ir.irAtomExpr(
            ir.irForeignVar(
              foreignInfo.module,
              foreignInfo.name,
              applySubst(ctx.subst, scheme.type),
            ),
          );
        }
        return ir.irAtomExpr(ir.irVar(expr.member, applySubst(ctx.subst, scheme.type)));
      }
      // Member not found
      throw new Error(
        `Qualified access (${expr.moduleName}.${expr.member}) requires importing the module. ` +
          `Add 'use ${expr.moduleName} (..)' to import all bindings.`,
      );
    }

    default:
      return assertNever(expr);
  }
};

const lowerLet = (ctx: LowerContext, expr: ast.Let): ir.IRExpr => {
  const valueIR = lowerExpr(ctx, expr.value);

  // Extract bindings from the value
  const { bindings, finalBinding } = extractBindings(valueIR);

  // Add the binding to the environment
  extendEnv(ctx, expr.name, finalBinding.type);

  // Lower the body
  const bodyIR = lowerExpr(ctx, expr.body);

  // Create the let for the main binding
  const letExpr = ir.irLet(expr.name, finalBinding, bodyIR);

  // Wrap with any preceding bindings
  return wrapWithBindings(bindings, letExpr);
};

const lowerLetRec = (ctx: LowerContext, expr: ast.LetRec): ir.IRExpr => {
  // For letrec, we need to add ALL bindings to env BEFORE lowering any values
  // so mutual recursive references can find each other

  // Step 1: Add placeholder types for ALL bindings
  for (const binding of expr.bindings) {
    const placeholderType: Type = { kind: "TVar", name: `_rec_${binding.name}` };
    extendEnv(ctx, binding.name, placeholderType);
  }

  // Step 2: Lower all values (all names are in scope for mutual recursion)
  // Separate lambda bindings (go in LetRec) from non-lambda bindings (become Lets)
  const lambdaBindings: ir.IRRecBinding[] = [];
  const nonLambdaBindings: Array<{ name: string; binding: ir.IRBinding }> = [];

  for (const binding of expr.bindings) {
    const valueIR = lowerExpr(ctx, binding.value);
    const { bindings: preBindings, finalBinding } = extractBindings(valueIR);

    // Check if this is a lambda binding
    if (finalBinding.kind === "IRLambdaBinding") {
      // Lambda bindings go in the LetRec
      lambdaBindings.push(ir.irRecBinding(binding.name, finalBinding));
      // Pre-bindings from lambda lowering should be minimal (just the lambda itself)
      // but if there are any, they go before the binding
      for (const b of preBindings) {
        lambdaBindings.push(ir.irRecBinding(b.name, b.binding));
      }
    } else {
      // Non-lambda bindings become sequential Lets inside the body
      // Include any pre-bindings first
      for (const b of preBindings) {
        nonLambdaBindings.push(b);
      }
      nonLambdaBindings.push({ name: binding.name, binding: finalBinding });
    }

    // Update the environment with the actual type
    extendEnv(ctx, binding.name, finalBinding.type);
  }

  // Step 3: Lower the body
  let bodyIR = lowerExpr(ctx, expr.body);

  // Step 4: Wrap body with non-lambda bindings (in reverse order so they're in correct order)
  for (let i = nonLambdaBindings.length - 1; i >= 0; i--) {
    const { name, binding } = nonLambdaBindings[i]!;
    bodyIR = ir.irLet(name, binding, bodyIR);
  }

  // Step 5: Create the letrec (only if there are lambda bindings)
  if (lambdaBindings.length === 0) {
    return bodyIR;
  }

  return ir.irLetRec(lambdaBindings, bodyIR);
};

const lowerAbs = (ctx: LowerContext, expr: ast.Abs): ir.IRExpr => {
  // Get parameter type from the type checker's span mapping
  // Fall back to placeholder if not available (shouldn't happen for well-typed code)
  const paramType: Type = lookupSpanType(ctx, expr.paramSpan) ?? {
    kind: "TVar",
    name: `_param${ctx.varCounter++}`,
  };

  // Extend environment with parameter
  const savedEnv = new Map(ctx.typeEnv);
  extendEnv(ctx, expr.param, paramType);

  // Lower the body
  const bodyIR = lowerExpr(ctx, expr.body);

  // Restore environment
  ctx.typeEnv = savedEnv;

  // Create the lambda binding
  const funcType: Type = { kind: "TFun", param: paramType, ret: bodyIR.type };
  const binding = ir.irLambdaBinding(expr.param, paramType, bodyIR, funcType);

  // Wrap in a let to name the lambda
  const name = freshVar(ctx, "_fn");
  extendEnv(ctx, name, funcType);

  return ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, funcType)));
};

const lowerApp = (ctx: LowerContext, expr: ast.App): ir.IRExpr => {
  // Normalize function and argument to atoms
  const funcResult = normalize(ctx, expr.func);
  const argResult = normalize(ctx, expr.param);

  // Get the return type from the function type
  const funcType = funcResult.atom.type;
  const returnType = getReturnType(funcType);

  // Create the application binding
  const binding = ir.irAppBinding(funcResult.atom, argResult.atom, returnType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, returnType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, returnType)));

  // Wrap with all the bindings from normalization
  return wrapWithBindings([...funcResult.bindings, ...argResult.bindings], result);
};

const lowerBinOp = (ctx: LowerContext, expr: ast.BinOp): ir.IRExpr => {
  // Normalize operands to atoms
  const leftResult = normalize(ctx, expr.left);
  const rightResult = normalize(ctx, expr.right);

  // Determine result type based on operator
  const operandType = leftResult.atom.type;
  let resultType: Type;

  switch (expr.op) {
    case "+":
      // + works on both numbers and strings, result type matches operand
      resultType = operandType;
      break;
    case "-":
    case "*":
    case "/":
      // Arithmetic operators - result type matches operand type (Int or Float)
      resultType = operandType;
      break;
    case "<":
    case "<=":
    case ">":
    case ">=":
    case "==":
    case "!=":
      resultType = tBool;
      break;
  }

  // Create the binary operation binding
  const binding = ir.irBinOpBinding(
    expr.op,
    leftResult.atom,
    rightResult.atom,
    operandType,
    resultType,
  );

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, resultType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, resultType)));

  return wrapWithBindings([...leftResult.bindings, ...rightResult.bindings], result);
};

const lowerIf = (ctx: LowerContext, expr: ast.If): ir.IRExpr => {
  // Normalize condition to atom
  const condResult = normalize(ctx, expr.cond);

  // Lower both branches
  const thenIR = lowerExpr(ctx, expr.then);
  const elseIR = lowerExpr(ctx, expr.else);

  // Both branches should have the same type
  const resultType = thenIR.type;

  // Create the if binding
  const binding = ir.irIfBinding(condResult.atom, thenIR, elseIR, resultType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, resultType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, resultType)));

  return wrapWithBindings(condResult.bindings, result);
};

const lowerTuple = (ctx: LowerContext, expr: ast.Tuple): ir.IRExpr => {
  // Handle single-element tuple (unwrap)
  if (expr.elements.length === 1) {
    return lowerExpr(ctx, expr.elements[0]!);
  }

  // Normalize all elements to atoms
  const { bindings, atoms } = normalizeMany(ctx, expr.elements);

  // Build the tuple type
  const elementTypes = atoms.map((a) => a.type);
  const tupleType: Type = { kind: "TTuple", elements: elementTypes };

  // Create the tuple binding
  const binding = ir.irTupleBinding(atoms, tupleType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, tupleType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, tupleType)));

  return wrapWithBindings(bindings, result);
};

const lowerRecord = (ctx: LowerContext, expr: ast.Record): ir.IRExpr => {
  // Normalize all field values to atoms
  const fieldExprs = expr.fields.map((f) => f.value);
  const { bindings, atoms } = normalizeMany(ctx, fieldExprs);

  // Build IR record fields
  const irFields = expr.fields.map((f, i) => ir.irRecordField(f.name, atoms[i]!));

  // Build the record type
  const fieldTypes = new Map<string, Type>();
  for (let i = 0; i < expr.fields.length; i++) {
    fieldTypes.set(expr.fields[i]!.name, atoms[i]!.type);
  }
  const recordType: Type = { kind: "TRecord", fields: fieldTypes, row: null };

  // Create the record binding
  const binding = ir.irRecordBinding(irFields, recordType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, recordType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, recordType)));

  return wrapWithBindings(bindings, result);
};

const lowerRecordUpdate = (ctx: LowerContext, expr: ast.RecordUpdate): ir.IRExpr => {
  // Normalize the base record to an atom
  const baseResult = normalize(ctx, expr.base);

  // Normalize all field values to atoms
  const fieldExprs = expr.fields.map((f) => f.value);
  const fieldResults = normalizeMany(ctx, fieldExprs);

  // Build IR record fields
  const irFields = expr.fields.map((f, i) => ir.irRecordField(f.name, fieldResults.atoms[i]!));

  // Build the result type from the base type and updated fields
  let resultType: Type;
  const baseType = baseResult.atom.type;
  if (baseType.kind === "TRecord") {
    // Clone base fields and update/add the new ones
    const fieldTypes = new Map(baseType.fields);
    for (let i = 0; i < expr.fields.length; i++) {
      fieldTypes.set(expr.fields[i]!.name, fieldResults.atoms[i]!.type);
    }
    resultType = { kind: "TRecord", fields: fieldTypes, row: baseType.row };
  } else {
    // Fallback: build type from just the updated fields
    const fieldTypes = new Map<string, Type>();
    for (let i = 0; i < expr.fields.length; i++) {
      fieldTypes.set(expr.fields[i]!.name, fieldResults.atoms[i]!.type);
    }
    resultType = { kind: "TRecord", fields: fieldTypes, row: null };
  }

  // Create the record update binding
  const binding = ir.irRecordUpdateBinding(baseResult.atom, irFields, resultType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, resultType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, resultType)));

  // Combine all bindings
  const allBindings = [...baseResult.bindings, ...fieldResults.bindings];
  return wrapWithBindings(allBindings, result);
};

const lowerFieldAccess = (ctx: LowerContext, expr: ast.FieldAccess): ir.IRExpr => {
  // Normalize record to atom
  const recordResult = normalize(ctx, expr.record);

  // Get the field type
  const fieldType = getFieldType(recordResult.atom.type, expr.field);

  // Create the field access binding
  const binding = ir.irFieldAccessBinding(recordResult.atom, expr.field, fieldType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, fieldType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, fieldType)));

  return wrapWithBindings(recordResult.bindings, result);
};

const lowerTupleIndex = (ctx: LowerContext, expr: ast.TupleIndex): ir.IRExpr => {
  // Normalize tuple to atom
  const tupleResult = normalize(ctx, expr.tuple);

  // Get the element type
  const tupleType = tupleResult.atom.type;
  if (tupleType.kind !== "TTuple") {
    throw new Error(`Expected tuple type, got ${tupleType.kind}`);
  }
  const elementType = tupleType.elements[expr.index]!;

  // Create the tuple index binding
  const binding = ir.irTupleIndexBinding(tupleResult.atom, expr.index, elementType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, elementType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, elementType)));

  return wrapWithBindings(tupleResult.bindings, result);
};

const lowerMatch = (ctx: LowerContext, expr: ast.Match): ir.IRExpr => {
  // Normalize scrutinee to atom
  const scrutineeResult = normalize(ctx, expr.expr);

  // Lower each case
  const irCases: ir.IRCase[] = [];
  let resultType: Type | null = null;

  for (const case_ of expr.cases) {
    // Lower the pattern
    const irPattern = lowerPattern(case_.pattern, scrutineeResult.atom.type);

    // Extend environment with pattern bindings
    const savedEnv = new Map(ctx.typeEnv);
    extendPatternBindings(ctx, case_.pattern, scrutineeResult.atom.type);

    // Lower the guard if present
    let guardIR: ir.IRExpr | undefined;
    if (case_.guard) {
      guardIR = lowerExpr(ctx, case_.guard);
    }

    // Lower the body
    const bodyIR = lowerExpr(ctx, case_.body);
    resultType = bodyIR.type;

    // Restore environment
    ctx.typeEnv = savedEnv;

    irCases.push(ir.irCase(irPattern, bodyIR, guardIR));
  }

  if (!resultType) {
    throw new Error("Match expression has no cases");
  }

  // Create the match binding
  const binding = ir.irMatchBinding(scrutineeResult.atom, irCases, resultType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, resultType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, resultType)));

  return wrapWithBindings(scrutineeResult.bindings, result);
};

// =============================================================================
// PATTERN LOWERING
// =============================================================================

const lowerPattern = (pattern: ast.Pattern, type: Type): ir.IRPattern => {
  switch (pattern.kind) {
    case "PVar":
      return ir.irPVar(pattern.name, type);

    case "PWildcard":
      return ir.irPWildcard(type);

    case "PLit": {
      let litType: Type;
      if (typeof pattern.value === "number") {
        litType = Number.isInteger(pattern.value) ? tInt : tFloat;
      } else if (typeof pattern.value === "string") {
        litType = tStr;
      } else {
        litType = tBool;
      }
      return ir.irPLit(pattern.value, litType);
    }

    case "PChar":
      return ir.irPLit(pattern.value, tChar);

    case "PCon": {
      // For constructors, we need to figure out the argument types
      // This is simplified - in a full implementation we'd look up the constructor type
      const args = pattern.args.map((arg, i) => {
        // Use a placeholder type for now
        const argType: Type = { kind: "TVar", name: `_arg${i}` };
        return lowerPattern(arg, argType);
      });
      return ir.irPCon(pattern.name, args, type);
    }

    case "QualifiedPCon": {
      // Qualified constructor pattern (Module.Constructor)
      // Lower using just the constructor name (module already resolved by type checker)
      const args = pattern.args.map((arg, i) => {
        const argType: Type = { kind: "TVar", name: `_arg${i}` };
        return lowerPattern(arg, argType);
      });
      return ir.irPCon(pattern.constructor, args, type);
    }

    case "PTuple": {
      const elementTypes = type.kind === "TTuple" ? type.elements : [];
      const elements = pattern.elements.map((elem, i) => {
        const elemType = elementTypes[i] ?? { kind: "TVar", name: `_elem${i}` };
        return lowerPattern(elem, elemType);
      });
      return ir.irPTuple(elements, type);
    }

    case "PRecord": {
      const fields = pattern.fields.map((f) => {
        const fieldType =
          type.kind === "TRecord"
            ? (type.fields.get(f.name) ?? { kind: "TVar" as const, name: `_field_${f.name}` })
            : { kind: "TVar" as const, name: `_field_${f.name}` };
        return ir.irPRecordField(f.name, lowerPattern(f.pattern, fieldType));
      });
      return ir.irPRecord(fields, type);
    }

    case "PAs": {
      const innerPattern = lowerPattern(pattern.pattern, type);
      return ir.irPAs(innerPattern, pattern.name, type);
    }

    case "POr": {
      const alternatives = pattern.alternatives.map((alt) => lowerPattern(alt, type));
      return ir.irPOr(alternatives, type);
    }
  }
};

/**
 * Extend the environment with bindings from a pattern.
 */
const extendPatternBindings = (ctx: LowerContext, pattern: ast.Pattern, type: Type): void => {
  switch (pattern.kind) {
    case "PVar":
      extendEnv(ctx, pattern.name, type);
      break;

    case "PWildcard":
    case "PLit":
    case "PChar":
      // No bindings
      break;

    case "PCon":
      // Extend with bindings from each argument
      for (let i = 0; i < pattern.args.length; i++) {
        const argType: Type = { kind: "TVar", name: `_arg${i}` };
        extendPatternBindings(ctx, pattern.args[i]!, argType);
      }
      break;

    case "QualifiedPCon":
      // Qualified constructor pattern - extend with bindings from each argument
      for (let i = 0; i < pattern.args.length; i++) {
        const argType: Type = { kind: "TVar", name: `_arg${i}` };
        extendPatternBindings(ctx, pattern.args[i]!, argType);
      }
      break;

    case "PTuple": {
      const elementTypes = type.kind === "TTuple" ? type.elements : [];
      for (let i = 0; i < pattern.elements.length; i++) {
        const elemType = elementTypes[i] ?? { kind: "TVar", name: `_elem${i}` };
        extendPatternBindings(ctx, pattern.elements[i]!, elemType);
      }
      break;
    }

    case "PRecord":
      for (const f of pattern.fields) {
        const fieldType =
          type.kind === "TRecord"
            ? (type.fields.get(f.name) ?? { kind: "TVar" as const, name: `_field_${f.name}` })
            : { kind: "TVar" as const, name: `_field_${f.name}` };
        extendPatternBindings(ctx, f.pattern, fieldType);
      }
      break;

    case "PAs":
      // Extend with the as-binding and inner pattern bindings
      extendEnv(ctx, pattern.name, type);
      extendPatternBindings(ctx, pattern.pattern, type);
      break;

    case "POr":
      // Or-patterns: all alternatives bind same variables, so just use the first
      if (pattern.alternatives.length > 0) {
        extendPatternBindings(ctx, pattern.alternatives[0]!, type);
      }
      break;
  }
};

// =============================================================================
// MAIN ENTRY POINT
// =============================================================================

/**
 * Lower a typed AST to ANF IR.
 *
 * @param expr The AST expression to lower
 * @param typeEnv The type environment (includes constructors and prelude)
 * @param checkOutput The output from type checking
 * @param foreignFunctions Map of local names to foreign function info
 * @param moduleEnv Module environment for resolving qualified access
 * @returns The IR expression in ANF
 */
export const lowerToIR = (
  expr: ast.Expr,
  typeEnv: TypeEnv,
  checkOutput: CheckOutput,
  foreignFunctions: ForeignMap = new Map(),
  moduleEnv: ModuleTypeEnv = new Map(),
): ir.IRExpr => {
  const ctx = createContext(typeEnv, checkOutput, foreignFunctions, moduleEnv);
  return lowerExpr(ctx, expr);
};
