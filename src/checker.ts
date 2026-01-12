/**
 * Type Inference Engine implementing Algorithm W (Section 8)
 *
 * Works on Core AST with unique Name identifiers.
 */

import type { Diagnostic } from "./diagnostics";
import { error as diagError, typeMismatch } from "./diagnostics";
import * as C from "./core";
import type { Name } from "./core";
import {
  type Type,
  type Subst,
  type TypeEnv,
  type Constraint,
  type ConstructorRegistry,
  type AliasRegistry,
  tvar,
  tcon,
  tfun,
  tapp,
  ttuple,
  trecord,
  scheme,
  mono,
  tInt,
  tFloat,
  tStr,
  tBool,
  tChar,
  applySubst,
  applySubstEnv,
  composeSubst,
  ftv,
  generalize,
  instantiate,
  freshTypeVar,
  resetTypeVarCounter,
  typeToString,
  instances,
} from "./types";

// Re-export typeToString for use in index.ts
export { typeToString } from "./types";

// =============================================================================
// Check Context
// =============================================================================

type CheckContext = {
  readonly diagnostics: Diagnostic[];
  readonly registry: ConstructorRegistry;
  readonly aliasRegistry: AliasRegistry;
  // Map from Name.id to inferred type (for LSP/lowering)
  readonly typeMap: Map<number, Type>;
  // Map from span.start to instantiated type (for polymorphic expressions)
  readonly exprTypeMap: Map<number, Type>;
};

const createContext = (
  registry: ConstructorRegistry = new Map(),
  aliasRegistry: AliasRegistry = new Map(),
): CheckContext => ({
  diagnostics: [],
  registry,
  aliasRegistry,
  typeMap: new Map(),
  exprTypeMap: new Map(),
});

const addError = (ctx: CheckContext, message: string, span?: C.CExpr["span"]): void => {
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(diagError(start, end, message));
};

const recordType = (ctx: CheckContext, name: Name, type: Type): void => {
  ctx.typeMap.set(name.id, type);
};

/** Record the instantiated type of an expression by its span position */
const recordExprType = (ctx: CheckContext, span: C.CExpr["span"], type: Type): void => {
  if (span) {
    ctx.exprTypeMap.set(span.start, type);
  }
};

// =============================================================================
// Unification (Section 8.4)
// =============================================================================

const unify = (ctx: CheckContext, t1: Type, t2: Type, span?: C.CExpr["span"]): Subst => {
  if (t1.kind === "TVar" && t2.kind === "TVar" && t1.name === t2.name) {
    return new Map();
  }

  if (t1.kind === "TVar") {
    return bindVar(ctx, t1.name, t2);
  }

  if (t2.kind === "TVar") {
    return bindVar(ctx, t2.name, t1);
  }

  if (t1.kind === "TCon" && t2.kind === "TCon" && t1.name === t2.name) {
    return new Map();
  }

  if (t1.kind === "TFun" && t2.kind === "TFun") {
    const s1 = unify(ctx, t1.param, t2.param, span);
    const s2 = unify(ctx, applySubst(s1, t1.ret), applySubst(s1, t2.ret), span);
    return composeSubst(s1, s2);
  }

  if (t1.kind === "TApp" && t2.kind === "TApp") {
    const s1 = unify(ctx, t1.con, t2.con, span);
    const s2 = unify(ctx, applySubst(s1, t1.arg), applySubst(s1, t2.arg), span);
    return composeSubst(s1, s2);
  }

  if (t1.kind === "TRecord" && t2.kind === "TRecord") {
    return unifyRecords(ctx, t1, t2, span);
  }

  if (t1.kind === "TTuple" && t2.kind === "TTuple") {
    if (t1.elements.length !== t2.elements.length) {
      addError(ctx, `Tuple arity mismatch: ${t1.elements.length} vs ${t2.elements.length}`, span);
      return new Map();
    }
    let subst: Subst = new Map();
    for (let i = 0; i < t1.elements.length; i++) {
      const s = unify(
        ctx,
        applySubst(subst, t1.elements[i]!),
        applySubst(subst, t2.elements[i]!),
        span,
      );
      subst = composeSubst(subst, s);
    }
    return subst;
  }

  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(typeMismatch(start, end, typeToString(t1), typeToString(t2)));
  return new Map();
};

const bindVar = (ctx: CheckContext, name: string, type: Type): Subst => {
  if (type.kind === "TVar" && type.name === name) {
    return new Map();
  }
  if (ftv(type).has(name)) {
    addError(ctx, `Infinite type: ${name} occurs in ${typeToString(type)}`);
    return new Map();
  }
  return new Map([[name, type]]);
};

const unifyRecords = (
  ctx: CheckContext,
  t1: { kind: "TRecord"; fields: ReadonlyMap<string, Type>; row: Type | null },
  t2: { kind: "TRecord"; fields: ReadonlyMap<string, Type>; row: Type | null },
  span?: C.CExpr["span"],
): Subst => {
  let subst: Subst = new Map();

  // Unify common fields
  for (const [name, type1] of t1.fields) {
    const type2 = t2.fields.get(name);
    if (type2) {
      const s = unify(ctx, applySubst(subst, type1), applySubst(subst, type2), span);
      subst = composeSubst(subst, s);
    }
  }

  // Handle extra fields and row polymorphism
  const extra1 = [...t1.fields.entries()].filter(([n]) => !t2.fields.has(n));
  const extra2 = [...t2.fields.entries()].filter(([n]) => !t1.fields.has(n));

  if (extra1.length > 0 && t2.row) {
    const newRow = t1.row ?? null;
    const extraRecord = trecord(
      extra1.map(([n, t]) => [n, applySubst(subst, t)]),
      newRow,
    );
    const s = unify(ctx, applySubst(subst, t2.row), extraRecord, span);
    subst = composeSubst(subst, s);
  } else if (extra1.length > 0 && !t2.row) {
    addError(ctx, `Record missing fields: ${extra1.map(([n]) => n).join(", ")}`, span);
  }

  if (extra2.length > 0 && t1.row) {
    const newRow = t2.row ?? null;
    const extraRecord = trecord(
      extra2.map(([n, t]) => [n, applySubst(subst, t)]),
      newRow,
    );
    const s = unify(ctx, applySubst(subst, t1.row), extraRecord, span);
    subst = composeSubst(subst, s);
  } else if (extra2.length > 0 && !t1.row) {
    addError(ctx, `Record missing fields: ${extra2.map(([n]) => n).join(", ")}`, span);
  }

  // Unify row variables if both are open
  if (t1.row && t2.row && extra1.length === 0 && extra2.length === 0) {
    const s = unify(ctx, applySubst(subst, t1.row), applySubst(subst, t2.row), span);
    subst = composeSubst(subst, s);
  }

  return subst;
};

// =============================================================================
// Expression Inference (Section 8.5)
// =============================================================================

type InferResult = [Subst, Type, readonly Constraint[]];

const inferExpr = (ctx: CheckContext, env: TypeEnv, expr: C.CExpr): InferResult => {
  switch (expr.kind) {
    case "CVar":
      return inferVar(ctx, env, expr);
    case "CLit":
      return inferLit(expr);
    case "CApp":
      return inferApp(ctx, env, expr);
    case "CAbs":
      return inferAbs(ctx, env, expr);
    case "CLet":
      return inferLet(ctx, env, expr);
    case "CLetRec":
      return inferLetRec(ctx, env, expr);
    case "CMatch":
      return inferMatch(ctx, env, expr);
    case "CCon":
      return inferCon(ctx, env, expr);
    case "CTuple":
      return inferTuple(ctx, env, expr);
    case "CRecord":
      return inferRecord(ctx, env, expr);
    case "CRecordUpdate":
      return inferRecordUpdate(ctx, env, expr);
    case "CField":
      return inferField(ctx, env, expr);
    case "CForeign":
      return inferForeign(ctx, env, expr);
    case "CBinOp":
      return inferBinOp(ctx, env, expr);
  }
};

const inferVar = (ctx: CheckContext, env: TypeEnv, expr: C.CVar): InferResult => {
  // Look up by name ID
  const key = `${expr.name.id}:${expr.name.original}`;
  const s = env.get(key) ?? env.get(expr.name.original);

  if (!s) {
    addError(ctx, `Unbound variable: ${expr.name.original}`, expr.span);
    return [new Map(), freshTypeVar(), []];
  }

  const type = instantiate(s);
  // Record the instantiated type for polymorphic resolution during lowering
  recordExprType(ctx, expr.span, type);
  return [new Map(), type, [...s.constraints]];
};

const inferLit = (expr: C.CLit): InferResult => {
  switch (expr.value.kind) {
    case "int":
      return [new Map(), tInt, []];
    case "float":
      return [new Map(), tFloat, []];
    case "string":
      return [new Map(), tStr, []];
    case "char":
      return [new Map(), tChar, []];
    case "bool":
      return [new Map(), tBool, []];
  }
};

const inferApp = (ctx: CheckContext, env: TypeEnv, expr: C.CApp): InferResult => {
  const [s1, t1, c1] = inferExpr(ctx, env, expr.func);
  const [s2, t2, c2] = inferExpr(ctx, applySubstEnv(s1, env), expr.arg);

  const resultType = freshTypeVar();
  const funcType = tfun(t2, resultType);
  const s3 = unify(ctx, applySubst(s2, t1), funcType, expr.span);

  const finalResultType = applySubst(s3, resultType);
  // Record the result type of the application for polymorphic resolution during lowering
  recordExprType(ctx, expr.span, finalResultType);
  return [composeSubst(composeSubst(s1, s2), s3), finalResultType, [...c1, ...c2]];
};

const inferAbs = (ctx: CheckContext, env: TypeEnv, expr: C.CAbs): InferResult => {
  const paramType = freshTypeVar();
  const key = `${expr.param.id}:${expr.param.original}`;
  const newEnv = new Map(env);
  newEnv.set(key, mono(paramType));

  const [s, bodyType, constraints] = inferExpr(ctx, newEnv, expr.body);
  const resultType = tfun(applySubst(s, paramType), bodyType);

  recordType(ctx, expr.param, applySubst(s, paramType));

  return [s, resultType, constraints];
};

const inferLet = (ctx: CheckContext, env: TypeEnv, expr: C.CLet): InferResult => {
  const [s1, t1, c1] = inferExpr(ctx, env, expr.value);
  const env1 = applySubstEnv(s1, env);
  const generalizedScheme = generalize(env1, t1);

  const key = `${expr.name.id}:${expr.name.original}`;
  const newEnv = new Map(env1);
  newEnv.set(key, generalizedScheme);

  const [s2, t2, c2] = inferExpr(ctx, newEnv, expr.body);

  recordType(ctx, expr.name, applySubst(composeSubst(s1, s2), t1));

  return [composeSubst(s1, s2), t2, [...c1, ...c2]];
};

const inferLetRec = (ctx: CheckContext, env: TypeEnv, expr: C.CLetRec): InferResult => {
  // Create fresh type variables for all bindings
  const bindingTypes = new Map<string, Type>();
  let newEnv = new Map(env);

  for (const b of expr.bindings) {
    const tv = freshTypeVar();
    const key = `${b.name.id}:${b.name.original}`;
    bindingTypes.set(key, tv);
    newEnv.set(key, mono(tv));
  }

  // Infer types for all bindings
  let subst: Subst = new Map();
  const allConstraints: Constraint[] = [];

  for (const b of expr.bindings) {
    const key = `${b.name.id}:${b.name.original}`;
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, newEnv), b.value);
    subst = composeSubst(subst, s);
    allConstraints.push(...c);

    const expectedType = bindingTypes.get(key)!;
    const s2 = unify(ctx, applySubst(subst, expectedType), t, b.value.span);
    subst = composeSubst(subst, s2);
  }

  // Generalize and update environment
  const finalEnv = applySubstEnv(subst, newEnv);
  for (const b of expr.bindings) {
    const key = `${b.name.id}:${b.name.original}`;
    const t = applySubst(subst, bindingTypes.get(key)!);
    const generalizedScheme = generalize(finalEnv, t);
    finalEnv.set(key, generalizedScheme);
    recordType(ctx, b.name, t);
  }

  const [s2, t2, c2] = inferExpr(ctx, finalEnv, expr.body);

  return [composeSubst(subst, s2), t2, [...allConstraints, ...c2]];
};

const inferMatch = (ctx: CheckContext, env: TypeEnv, expr: C.CMatch): InferResult => {
  const [s1, scrutineeType, c1] = inferExpr(ctx, env, expr.scrutinee);
  let subst = s1;
  const allConstraints = [...c1];

  const resultType = freshTypeVar();

  for (const c of expr.cases) {
    const [patSubst, patBindings] = inferPattern(
      ctx,
      env,
      applySubst(subst, scrutineeType),
      c.pattern,
    );
    subst = composeSubst(subst, patSubst);

    // Extend environment with pattern bindings
    let caseEnv = applySubstEnv(subst, env);
    for (const [key, type] of patBindings) {
      caseEnv = new Map(caseEnv);
      caseEnv.set(key, mono(type));
    }

    // Infer guard if present
    if (c.guard) {
      const [gs, gt, gc] = inferExpr(ctx, caseEnv, c.guard);
      subst = composeSubst(subst, gs);
      const s = unify(ctx, gt, tBool, c.guard.span);
      subst = composeSubst(subst, s);
      allConstraints.push(...gc);
    }

    // Infer body
    const [bs, bt, bc] = inferExpr(ctx, caseEnv, c.body);
    subst = composeSubst(subst, bs);
    allConstraints.push(...bc);

    // Unify with result type
    const s = unify(ctx, applySubst(subst, resultType), bt, c.body.span);
    subst = composeSubst(subst, s);
  }

  return [subst, applySubst(subst, resultType), allConstraints];
};

const inferCon = (ctx: CheckContext, env: TypeEnv, expr: C.CCon): InferResult => {
  const s = env.get(expr.name);
  if (!s) {
    addError(ctx, `Unknown constructor: ${expr.name}`, expr.span);
    return [new Map(), freshTypeVar(), []];
  }
  return [new Map(), instantiate(s), [...s.constraints]];
};

const inferTuple = (ctx: CheckContext, env: TypeEnv, expr: C.CTuple): InferResult => {
  let subst: Subst = new Map();
  const allConstraints: Constraint[] = [];
  const elementTypes: Type[] = [];

  for (const e of expr.elements) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), e);
    subst = composeSubst(subst, s);
    allConstraints.push(...c);
    elementTypes.push(t);
  }

  return [subst, ttuple(elementTypes.map((t) => applySubst(subst, t))), allConstraints];
};

const inferRecord = (ctx: CheckContext, env: TypeEnv, expr: C.CRecord): InferResult => {
  let subst: Subst = new Map();
  const allConstraints: Constraint[] = [];
  const fieldTypes: [string, Type][] = [];

  for (const f of expr.fields) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), f.value);
    subst = composeSubst(subst, s);
    allConstraints.push(...c);
    fieldTypes.push([f.name, t]);
  }

  return [subst, trecord(fieldTypes.map(([n, t]) => [n, applySubst(subst, t)])), allConstraints];
};

const inferRecordUpdate = (ctx: CheckContext, env: TypeEnv, expr: C.CRecordUpdate): InferResult => {
  const [s1, recordType, c1] = inferExpr(ctx, env, expr.record);
  let subst = s1;
  const allConstraints = [...c1];

  const fieldTypes: [string, Type][] = [];
  for (const f of expr.fields) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), f.value);
    subst = composeSubst(subst, s);
    allConstraints.push(...c);
    fieldTypes.push([f.name, t]);
  }

  // Create constraint that record has these fields
  const rowVar = freshTypeVar();
  const updateRecord = trecord(
    fieldTypes.map(([n, t]) => [n, applySubst(subst, t)]),
    rowVar,
  );
  const s2 = unify(ctx, applySubst(subst, recordType), updateRecord, expr.span);
  subst = composeSubst(subst, s2);

  return [subst, applySubst(subst, recordType), allConstraints];
};

const inferField = (ctx: CheckContext, env: TypeEnv, expr: C.CField): InferResult => {
  const [s1, recordType, c1] = inferExpr(ctx, env, expr.record);
  const resolved = applySubst(s1, recordType);

  if (resolved.kind === "TVar") {
    const fieldType = freshTypeVar();
    const rowVar = freshTypeVar();
    const openRecord = trecord([[expr.field, fieldType]], rowVar);
    const s2 = unify(ctx, resolved, openRecord, expr.span);
    return [composeSubst(s1, s2), applySubst(s2, fieldType), c1];
  }

  if (resolved.kind !== "TRecord") {
    addError(ctx, `Cannot access field '${expr.field}' on non-record type`, expr.span);
    return [s1, freshTypeVar(), c1];
  }

  const fieldType = resolved.fields.get(expr.field);
  if (fieldType) {
    return [s1, fieldType, c1];
  }

  if (resolved.row) {
    const newFieldType = freshTypeVar();
    const newRowVar = freshTypeVar();
    const s2 = unify(
      ctx,
      resolved.row,
      trecord([[expr.field, newFieldType]], newRowVar),
      expr.span,
    );
    return [composeSubst(s1, s2), applySubst(s2, newFieldType), c1];
  }

  addError(ctx, `Record has no field '${expr.field}'`, expr.span);
  return [s1, freshTypeVar(), c1];
};

const inferForeign = (_ctx: CheckContext, env: TypeEnv, expr: C.CForeign): InferResult => {
  // Foreign functions should have their types in env
  const key = `${expr.module}.${expr.name}`;
  const s = env.get(key);
  if (s) {
    return [new Map(), instantiate(s), []];
  }
  // Return a fresh type variable if not found
  return [new Map(), freshTypeVar(), []];
};

const inferBinOp = (ctx: CheckContext, env: TypeEnv, expr: C.CBinOp): InferResult => {
  const [s1, t1, c1] = inferExpr(ctx, env, expr.left);
  const [s2, t2, c2] = inferExpr(ctx, applySubstEnv(s1, env), expr.right);

  // Determine result type based on operator
  let operandType: Type;
  let resultType: Type;

  switch (expr.op) {
    case "+":
      // Addition: works for int and string
      // Use fresh type variable, constrained by unification
      operandType = freshTypeVar();
      resultType = operandType;
      break;
    case "-":
    case "*":
    case "/":
      // Arithmetic operators: int -> int -> int
      operandType = tInt;
      resultType = tInt;
      break;
    case "==":
    case "!=":
      // Equality: a -> a -> bool (polymorphic)
      operandType = applySubst(s2, t1);
      resultType = tBool;
      break;
    case "<":
    case "<=":
    case ">":
    case ">=":
      // Comparison: a -> a -> bool (polymorphic, works on int/float/string/char)
      operandType = applySubst(s2, t1);
      resultType = tBool;
      break;
    default:
      // Unknown operator - use fresh type variables
      operandType = freshTypeVar();
      resultType = freshTypeVar();
  }

  // Unify operands with expected type
  const s3 = unify(ctx, applySubst(s2, t1), operandType, expr.span);
  const s4 = unify(
    ctx,
    applySubst(composeSubst(s2, s3), t2),
    applySubst(s3, operandType),
    expr.span,
  );

  return [
    composeSubst(composeSubst(composeSubst(s1, s2), s3), s4),
    applySubst(s4, resultType),
    [...c1, ...c2],
  ];
};

// =============================================================================
// Pattern Inference
// =============================================================================

type PatternBindings = Map<string, Type>;

const inferPattern = (
  ctx: CheckContext,
  env: TypeEnv,
  expectedType: Type,
  pattern: C.CPattern,
): [Subst, PatternBindings] => {
  const bindings: PatternBindings = new Map();

  const infer = (expected: Type, pat: C.CPattern): Subst => {
    switch (pat.kind) {
      case "CPWild":
        return new Map();

      case "CPVar": {
        const key = `${pat.name.id}:${pat.name.original}`;
        bindings.set(key, expected);
        return new Map();
      }

      case "CPLit": {
        let litType: Type;
        switch (pat.value.kind) {
          case "int":
            litType = tInt;
            break;
          case "float":
            litType = tFloat;
            break;
          case "string":
            litType = tStr;
            break;
          case "char":
            litType = tChar;
            break;
          case "bool":
            litType = tBool;
            break;
        }
        return unify(ctx, expected, litType, pat.span);
      }

      case "CPCon": {
        // Look up constructor type in environment
        const conScheme = env.get(pat.name);
        if (!conScheme) {
          addError(ctx, `Unknown constructor: ${pat.name}`, pat.span);
          return new Map();
        }

        // Instantiate and decompose constructor type
        const conType = instantiate(conScheme);

        // Extract argument types and result type from the constructor function type
        const argTypes: Type[] = [];
        let resultType = conType;
        while (resultType.kind === "TFun") {
          argTypes.push(resultType.param);
          resultType = resultType.ret;
        }

        // Unify expected type with the result type of the constructor
        let subst = unify(ctx, expected, resultType, pat.span);

        // Check arity
        if (pat.args.length !== argTypes.length) {
          addError(
            ctx,
            `Constructor ${pat.name} expects ${argTypes.length} arguments but got ${pat.args.length}`,
            pat.span,
          );
          return subst;
        }

        // Infer patterns for each argument
        for (let i = 0; i < pat.args.length; i++) {
          const s = infer(applySubst(subst, argTypes[i]!), pat.args[i]!);
          subst = composeSubst(subst, s);
        }
        return subst;
      }

      case "CPTuple": {
        if (expected.kind === "TTuple") {
          if (expected.elements.length !== pat.elements.length) {
            addError(ctx, `Tuple pattern arity mismatch`, pat.span);
            return new Map();
          }
          let subst: Subst = new Map();
          for (let i = 0; i < pat.elements.length; i++) {
            const s = infer(applySubst(subst, expected.elements[i]!), pat.elements[i]!);
            subst = composeSubst(subst, s);
          }
          return subst;
        }
        // Expected is a type variable - create tuple constraint
        const elemTypes = pat.elements.map(() => freshTypeVar());
        const tupleType = ttuple(elemTypes);
        let subst = unify(ctx, expected, tupleType, pat.span);
        for (let i = 0; i < pat.elements.length; i++) {
          const s = infer(applySubst(subst, elemTypes[i]!), pat.elements[i]!);
          subst = composeSubst(subst, s);
        }
        return subst;
      }

      case "CPRecord": {
        // Create field types and build expected record type
        const fieldTypes: [string, Type][] = [];
        for (const f of pat.fields) {
          fieldTypes.push([f.name, freshTypeVar()]);
        }
        const rowVar = freshTypeVar();
        const recordType = trecord(fieldTypes, rowVar);

        // Unify with expected type
        let subst = unify(ctx, expected, recordType, pat.span);

        // Infer each field pattern
        for (const f of pat.fields) {
          const fieldType = fieldTypes.find(([n]) => n === f.name)![1];
          const s = infer(applySubst(subst, fieldType), f.pattern);
          subst = composeSubst(subst, s);
        }
        return subst;
      }

      case "CPAs": {
        const key = `${pat.name.id}:${pat.name.original}`;
        bindings.set(key, expected);
        return infer(expected, pat.pattern);
      }

      case "CPOr": {
        const s1 = infer(expected, pat.left);
        const s2 = infer(applySubst(s1, expected), pat.right);
        return composeSubst(s1, s2);
      }
    }
  };

  const subst = infer(expectedType, pattern);
  return [subst, bindings];
};

// =============================================================================
// Declaration Processing
// =============================================================================

export type CheckOutput = {
  readonly subst: Subst;
  readonly type: Type | null;
  readonly constraints: readonly Constraint[];
  readonly diagnostics: readonly Diagnostic[];
  readonly typeEnv: TypeEnv;
  readonly constructorRegistry: ConstructorRegistry;
  readonly typeMap: ReadonlyMap<number, Type>;
  // Map from span.start to instantiated type (for polymorphic expressions)
  readonly exprTypeMap: ReadonlyMap<number, Type>;
};

export const checkProgram = (
  program: C.CProgram,
  initialEnv: TypeEnv = new Map(),
  initialRegistry: ConstructorRegistry = new Map(),
): CheckOutput => {
  resetTypeVarCounter();

  const ctx = createContext(initialRegistry);
  let env = new Map(initialEnv);
  let subst: Subst = new Map();
  const allConstraints: Constraint[] = [];

  // First pass: process all type declarations (register constructors)
  for (const decl of program.decls) {
    if (decl.kind === "CDeclType") {
      // Register constructors
      for (const con of decl.constructors) {
        // Build constructor type: field1 -> field2 -> ... -> ResultType
        let conType: Type = tcon(decl.name);
        for (const param of decl.params) {
          conType = tapp(conType, tvar(param));
        }
        for (let i = con.fields.length - 1; i >= 0; i--) {
          conType = tfun(convertCoreType(con.fields[i]!), conType);
        }
        const conScheme = scheme(decl.params, conType);
        env.set(con.name, conScheme);
      }
      // Register type in constructor registry
      ctx.registry.set(
        decl.name,
        decl.constructors.map((c) => c.name),
      );
    }
  }

  // Second pass: process bindings and foreign declarations
  for (const decl of program.decls) {
    switch (decl.kind) {
      case "CDeclType":
        // Already processed in first pass
        break;

      case "CDeclLet": {
        const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), decl.value);
        subst = composeSubst(subst, s);
        allConstraints.push(...c);

        const key = `${decl.name.id}:${decl.name.original}`;
        const generalizedScheme = generalize(applySubstEnv(subst, env), t);
        env.set(key, generalizedScheme);
        recordType(ctx, decl.name, applySubst(subst, t));
        break;
      }

      case "CDeclLetRec": {
        // Create fresh type variables for all bindings
        const bindingTypes = new Map<string, Type>();
        for (const b of decl.bindings) {
          const tv = freshTypeVar();
          const key = `${b.name.id}:${b.name.original}`;
          bindingTypes.set(key, tv);
          env.set(key, mono(tv));
        }

        // Infer types for all bindings
        for (const b of decl.bindings) {
          const key = `${b.name.id}:${b.name.original}`;
          const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), b.value);
          subst = composeSubst(subst, s);
          allConstraints.push(...c);

          const expectedType = bindingTypes.get(key)!;
          const s2 = unify(ctx, applySubst(subst, expectedType), t, b.value.span);
          subst = composeSubst(subst, s2);
        }

        // Generalize
        for (const b of decl.bindings) {
          const key = `${b.name.id}:${b.name.original}`;
          const t = applySubst(subst, bindingTypes.get(key)!);
          const generalizedScheme = generalize(applySubstEnv(subst, env), t);
          env.set(key, generalizedScheme);
          recordType(ctx, b.name, t);
        }
        break;
      }

      case "CDeclForeign": {
        const type = convertCoreType(decl.type);
        const key = `${decl.name.id}:${decl.name.original}`;
        env.set(key, mono(type));
        // Also register with module.name key for lookup
        env.set(`${decl.module}.${decl.jsName}`, mono(type));
        recordType(ctx, decl.name, type);
        break;
      }
    }
  }

  // Check main expression if present
  let resultType: Type | null = null;
  if (program.expr) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), program.expr);
    subst = composeSubst(subst, s);
    allConstraints.push(...c);
    resultType = t;
  }

  // Solve constraints
  const finalConstraints = allConstraints.map((c) => ({
    className: c.className,
    type: applySubst(subst, c.type),
  }));
  solveConstraints(ctx, finalConstraints);

  return {
    subst,
    type: resultType ? applySubst(subst, resultType) : null,
    constraints: finalConstraints,
    diagnostics: ctx.diagnostics,
    typeEnv: env,
    constructorRegistry: ctx.registry,
    typeMap: ctx.typeMap,
    exprTypeMap: ctx.exprTypeMap,
  };
};

// =============================================================================
// Type Conversion
// =============================================================================

const convertCoreType = (ctype: C.CType): Type => {
  switch (ctype.kind) {
    case "CTVar":
      return tvar(ctype.name);
    case "CTCon":
      return tcon(ctype.name);
    case "CTApp":
      return tapp(convertCoreType(ctype.func), convertCoreType(ctype.arg));
    case "CTFun":
      return tfun(convertCoreType(ctype.param), convertCoreType(ctype.result));
    case "CTTuple":
      return ttuple(ctype.elements.map(convertCoreType));
    case "CTRecord":
      return trecord(ctype.fields.map((f) => [f.name, convertCoreType(f.type)]));
  }
};

// =============================================================================
// Constraint Solving
// =============================================================================

const solveConstraints = (ctx: CheckContext, constraints: readonly Constraint[]): void => {
  for (const c of constraints) {
    const { className, type } = c;

    if (type.kind === "TVar") continue; // Defer

    if (type.kind === "TCon") {
      const classInstances = instances.get(className);
      if (!classInstances?.has(type.name)) {
        addError(ctx, `Type '${type.name}' does not satisfy ${className}`);
      }
      continue;
    }

    if (type.kind === "TTuple" && (className === "Eq" || className === "Ord")) {
      // Tuples satisfy Eq/Ord if all elements do
      continue;
    }

    addError(ctx, `Type '${typeToString(type)}' does not satisfy ${className}`);
  }
};
