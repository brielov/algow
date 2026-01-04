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
import type { CheckOutput, Type, TypeEnv } from "./checker";

// =============================================================================
// LOWERING CONTEXT
// =============================================================================

type LowerContext = {
  /** Fresh variable counter for generating unique names */
  varCounter: number;
  /** Type environment - maps names to type schemes */
  typeEnv: TypeEnv;
  /** Substitution from type checker for resolving type variables */
  subst: ReadonlyMap<string, Type>;
};

const createContext = (typeEnv: TypeEnv, checkOutput: CheckOutput): LowerContext => ({
  varCounter: 0,
  typeEnv: new Map(typeEnv),
  subst: checkOutput.subst,
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

/** Apply substitution to resolve type variables */
const applySubst = (subst: ReadonlyMap<string, Type>, type: Type): Type => {
  switch (type.kind) {
    case "TCon":
      return type;
    case "TVar":
      return subst.get(type.name) ?? type;
    case "TFun":
      return {
        kind: "TFun",
        param: applySubst(subst, type.param),
        ret: applySubst(subst, type.ret),
      };
    case "TApp":
      return {
        kind: "TApp",
        con: applySubst(subst, type.con),
        arg: applySubst(subst, type.arg),
      };
    case "TRecord": {
      const fields = new Map<string, Type>();
      for (const [name, fieldType] of type.fields) {
        fields.set(name, applySubst(subst, fieldType));
      }
      return {
        kind: "TRecord",
        fields,
        row: type.row ? applySubst(subst, type.row) : null,
      };
    }
    case "TTuple":
      return {
        kind: "TTuple",
        elements: type.elements.map((t) => applySubst(subst, t)),
      };
  }
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

const tNum: Type = { kind: "TCon", name: "number" };
const tStr: Type = { kind: "TCon", name: "string" };
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
    case "Num":
      return { bindings: [], atom: ir.irLit(expr.value, tNum) };
    case "Str":
      return { bindings: [], atom: ir.irLit(expr.value, tStr) };
    case "Bool":
      return { bindings: [], atom: ir.irLit(expr.value, tBool) };
    case "Var": {
      const type = lookupType(ctx, expr.name);
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
    bindings.push({ name: current.name, binding: current.binding });
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
    case "Num":
      return ir.irAtomExpr(ir.irLit(expr.value, tNum));

    case "Str":
      return ir.irAtomExpr(ir.irLit(expr.value, tStr));

    case "Bool":
      return ir.irAtomExpr(ir.irLit(expr.value, tBool));

    case "Var": {
      const type = lookupType(ctx, expr.name);
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

    case "FieldAccess":
      return lowerFieldAccess(ctx, expr);

    case "TupleIndex":
      return lowerTupleIndex(ctx, expr);

    case "Match":
      return lowerMatch(ctx, expr);
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
  // For letrec, we need to add the binding to env BEFORE lowering the value
  // so recursive references can find it

  // Add a placeholder type to the environment for the recursive reference
  // We'll use a type variable that will be unified with the actual type
  const placeholderType: Type = { kind: "TVar", name: `_rec_${expr.name}` };
  extendEnv(ctx, expr.name, placeholderType);

  // Now lower the value (the recursive function can reference itself)
  const valueIR = lowerExpr(ctx, expr.value);
  const { bindings, finalBinding } = extractBindings(valueIR);

  // Update the environment with the actual type
  extendEnv(ctx, expr.name, finalBinding.type);

  // Lower the body
  const bodyIR = lowerExpr(ctx, expr.body);

  // Create the letrec
  const letRecExpr = ir.irLetRec(expr.name, finalBinding, bodyIR);

  return wrapWithBindings(bindings, letRecExpr);
};

const lowerAbs = (ctx: LowerContext, expr: ast.Abs): ir.IRExpr => {
  // We need to determine the function type
  // For now, we'll create a fresh type variable for the parameter
  // and infer from context

  // Create a placeholder type for the parameter
  // In a full implementation, we'd get this from the type checker
  const paramType: Type = { kind: "TVar", name: `_param${ctx.varCounter++}` };

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
    case "-":
    case "*":
    case "/":
      resultType = tNum;
      break;
    case "<":
    case "<=":
    case ">":
    case ">=":
    case "==":
    case "!=":
      resultType = tBool;
      break;
    case "++":
      resultType = tStr;
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
        litType = tNum;
      } else if (typeof pattern.value === "string") {
        litType = tStr;
      } else {
        litType = tBool;
      }
      return ir.irPLit(pattern.value, litType);
    }

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
      // No bindings
      break;

    case "PCon":
      // Extend with bindings from each argument
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
 * @returns The IR expression in ANF
 */
export const lowerToIR = (
  expr: ast.Expr,
  typeEnv: TypeEnv,
  checkOutput: CheckOutput,
): ir.IRExpr => {
  const ctx = createContext(typeEnv, checkOutput);
  return lowerExpr(ctx, expr);
};
