/**
 * Type Inference Engine implementing Algorithm W (Section 8)
 *
 * Works on Core AST with unique Name identifiers.
 */

import type { Diagnostic } from "./diagnostics";
import { error as diagError, typeMismatch } from "./diagnostics";
import type { NodeId, Span } from "./surface";
import * as C from "./core";
import type { Name } from "./core";
import type { SymbolTableBuilder } from "./lsp/symbols";
import { setDefinitionScheme } from "./lsp/symbols";
import {
  type Type,
  type Subst,
  type TypeEnv,
  type Constraint,
  type ConstructorRegistry,
  type AliasRegistry,
  type Scheme,
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
  // Symbol table builder for recording types on definitions
  readonly symbolTableBuilder: SymbolTableBuilder;
  // Unified type map: NodeId → Type (for LSP/lowering)
  readonly nodeTypeMap: Map<NodeId, Type>;
  // Span position to NodeId mapping (for LSP position lookup)
  readonly spanToNodeId: Map<number, NodeId>;
  // Legacy map from Name.id to inferred type (for bindings - will be removed)
  readonly typeMap: Map<number, Type>;
  // Deferred tuple projections: TVar name -> (index -> element type)
  // Used to collect all tuple index accesses before creating the tuple type
  readonly tupleProjections: Map<string, Map<number, Type>>;
};

const createContext = (
  symbolTableBuilder: SymbolTableBuilder,
  registry: ConstructorRegistry = new Map(),
  aliasRegistry: AliasRegistry = new Map(),
): CheckContext => ({
  diagnostics: [],
  registry,
  aliasRegistry,
  symbolTableBuilder,
  nodeTypeMap: new Map(),
  spanToNodeId: new Map(),
  typeMap: new Map(),
  tupleProjections: new Map(),
});

const addError = (ctx: CheckContext, message: string, span?: C.CExpr["span"]): void => {
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(diagError(start, end, message));
};

/** Record type for a named binding (legacy - used by lower.ts) */
const recordType = (ctx: CheckContext, name: Name, type: Type): void => {
  ctx.typeMap.set(name.id, type);
};

/** Record scheme for a named binding in both typeMap and symbol table */
const recordScheme = (ctx: CheckContext, name: Name, s: Scheme): void => {
  ctx.typeMap.set(name.id, s.type);
  setDefinitionScheme(ctx.symbolTableBuilder, name.id, s);
};

/** Record the type of an expression by its nodeId and span position */
const recordNodeType = (ctx: CheckContext, nodeId: NodeId, span: Span, type: Type): void => {
  ctx.nodeTypeMap.set(nodeId, type);
  ctx.spanToNodeId.set(span.start, nodeId);
};

/**
 * Record a tuple projection constraint: typeVar.index should have type elemType
 * Returns the element type (existing if already recorded, or fresh)
 */
const recordTupleProjection = (ctx: CheckContext, typeVarName: string, index: number): Type => {
  let projections = ctx.tupleProjections.get(typeVarName);
  if (!projections) {
    projections = new Map();
    ctx.tupleProjections.set(typeVarName, projections);
  }
  let elemType = projections.get(index);
  if (!elemType) {
    elemType = freshTypeVar();
    projections.set(index, elemType);
  }
  return elemType;
};

/**
 * Resolve all pending tuple projections for a type variable into a proper tuple type.
 * Returns a substitution binding the type variable to a tuple.
 */
const resolveTupleProjections = (ctx: CheckContext, typeVarName: string, subst: Subst): Subst => {
  const projections = ctx.tupleProjections.get(typeVarName);
  if (!projections || projections.size === 0) {
    return subst;
  }

  // Find the maximum index to determine tuple size
  const maxIndex = Math.max(...projections.keys());
  const elements: Type[] = [];
  for (let i = 0; i <= maxIndex; i++) {
    const elemType = projections.get(i);
    // Apply current substitution to the element type
    elements.push(elemType ? applySubst(subst, elemType) : freshTypeVar());
  }

  const tupleType = ttuple(elements);

  // Check if the type variable is already bound in the substitution
  const existingType = subst.get(typeVarName);
  if (existingType) {
    // The TVar is already bound - unify the existing type with our tuple
    const s = unify(ctx, applySubst(subst, existingType), tupleType, undefined);
    return composeSubst(subst, s);
  }

  // Bind the type variable to the tuple
  const s = new Map([[typeVarName, tupleType]]);
  return composeSubst(subst, s);
};

// =============================================================================
// Unification (Section 8.4)
// =============================================================================

const unify = (ctx: CheckContext, t1: Type, t2: Type, span?: Span): Subst => {
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
  const key = `${expr.name.id}:${expr.name.text}`;
  const s = env.get(key) ?? env.get(expr.name.text);

  if (!s) {
    addError(ctx, `Unbound variable: ${expr.name.text}`, expr.span);
    return [new Map(), freshTypeVar(), []];
  }

  const type = instantiate(s);
  // Record the instantiated type for polymorphic resolution during lowering
  recordNodeType(ctx, expr.nodeId, expr.span, type);
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
  recordNodeType(ctx, expr.nodeId, expr.span, finalResultType);
  return [composeSubst(composeSubst(s1, s2), s3), finalResultType, [...c1, ...c2]];
};

const inferAbs = (ctx: CheckContext, env: TypeEnv, expr: C.CAbs): InferResult => {
  const paramType = freshTypeVar();
  const key = `${expr.param.id}:${expr.param.text}`;
  const newEnv = new Map(env);
  newEnv.set(key, mono(paramType));

  const [s, bodyType, constraints] = inferExpr(ctx, newEnv, expr.body);

  // Resolve any deferred tuple projections on the parameter's type variable
  // This handles cases like: p -> let x = p.0 in let y = p.1 in (y, x)
  // where multiple tuple accesses need to be collected before creating the tuple type
  const s2 = resolveTupleProjections(ctx, paramType.name, s);
  // Clean up resolved projections
  ctx.tupleProjections.delete(paramType.name);

  const finalParamType = applySubst(s2, paramType);
  const resultType = tfun(finalParamType, applySubst(s2, bodyType));

  recordType(ctx, expr.param, finalParamType);

  return [s2, resultType, constraints];
};

const inferLet = (ctx: CheckContext, env: TypeEnv, expr: C.CLet): InferResult => {
  const [s1, t1, c1] = inferExpr(ctx, env, expr.value);
  const env1 = applySubstEnv(s1, env);
  const generalizedScheme = generalize(env1, t1);

  const key = `${expr.name.id}:${expr.name.text}`;
  const newEnv = new Map(env1);
  newEnv.set(key, generalizedScheme);

  const [s2, t2, c2] = inferExpr(ctx, newEnv, expr.body);

  const finalSubst = composeSubst(s1, s2);
  const finalScheme = scheme(
    generalizedScheme.vars,
    applySubst(finalSubst, generalizedScheme.type),
    generalizedScheme.constraints,
  );
  recordScheme(ctx, expr.name, finalScheme);

  return [finalSubst, t2, [...c1, ...c2]];
};

const inferLetRec = (ctx: CheckContext, env: TypeEnv, expr: C.CLetRec): InferResult => {
  // Create fresh type variables for all bindings
  const bindingTypes = new Map<string, Type>();
  let newEnv = new Map(env);

  for (const b of expr.bindings) {
    const tv = freshTypeVar();
    const key = `${b.name.id}:${b.name.text}`;
    bindingTypes.set(key, tv);
    newEnv.set(key, mono(tv));
  }

  // Infer types for all bindings
  let subst: Subst = new Map();
  const allConstraints: Constraint[] = [];

  for (const b of expr.bindings) {
    const key = `${b.name.id}:${b.name.text}`;
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, newEnv), b.value);
    subst = composeSubst(subst, s);
    allConstraints.push(...c);

    const expectedType = bindingTypes.get(key)!;
    const s2 = unify(ctx, applySubst(subst, expectedType), t, b.value.span);
    subst = composeSubst(subst, s2);
  }

  // Generalize and update environment
  const finalEnv = applySubstEnv(subst, newEnv);
  const bindingSchemes: { name: Name; scheme: Scheme }[] = [];
  for (const b of expr.bindings) {
    const key = `${b.name.id}:${b.name.text}`;
    const t = applySubst(subst, bindingTypes.get(key)!);
    const generalizedScheme = generalize(finalEnv, t);
    finalEnv.set(key, generalizedScheme);
    bindingSchemes.push({ name: b.name, scheme: generalizedScheme });
  }

  const [s2, t2, c2] = inferExpr(ctx, finalEnv, expr.body);

  // Record schemes with body substitution applied
  for (const { name, scheme: s } of bindingSchemes) {
    const finalScheme = scheme(s.vars, applySubst(s2, s.type), s.constraints);
    recordScheme(ctx, name, finalScheme);
  }

  return [composeSubst(subst, s2), t2, [...allConstraints, ...c2]];
};

const inferMatch = (ctx: CheckContext, env: TypeEnv, expr: C.CMatch): InferResult => {
  const [s1, scrutineeType, c1] = inferExpr(ctx, env, expr.scrutinee);
  let subst = s1;
  const allConstraints = [...c1];

  const resultType = freshTypeVar();

  for (const c of expr.cases) {
    // Validate or-pattern bindings before type checking
    validateOrPatternBindings(ctx, c.pattern);

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

    // Record types for pattern-bound variables (for lowering)
    recordPatternTypes(ctx, env, subst, c.pattern, applySubst(subst, scrutineeType));
  }

  // Check exhaustiveness (only if no guards - guards make exhaustiveness hard to check)
  const hasGuards = expr.cases.some((c) => c.guard !== null);
  if (!hasGuards) {
    const patterns = expr.cases.map((c) => c.pattern);
    const finalScrutineeType = applySubst(subst, scrutineeType);
    const missing = checkExhaustiveness(ctx, finalScrutineeType, patterns);
    if (missing.length > 0) {
      addExhaustivenessWarning(ctx, missing, expr.span);
    }
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

  // Check for tuple index access (e.g., pair.0, triple.1)
  const tupleIndex = parseInt(expr.field, 10);
  if (!isNaN(tupleIndex)) {
    if (resolved.kind === "TTuple") {
      if (tupleIndex < 0 || tupleIndex >= resolved.elements.length) {
        addError(
          ctx,
          `Tuple index ${tupleIndex} out of bounds. Tuple has ${resolved.elements.length} element(s) (indices 0-${resolved.elements.length - 1})`,
          expr.span,
        );
        return [s1, freshTypeVar(), c1];
      }
      return [s1, resolved.elements[tupleIndex]!, c1];
    }
    if (resolved.kind === "TVar") {
      // Defer tuple constraint creation - record the projection for later resolution
      // This allows multiple tuple accesses (e.g., p.0 and p.1) to be collected
      // before creating the properly-sized tuple type
      const elemType = recordTupleProjection(ctx, resolved.name, tupleIndex);
      return [s1, elemType, c1];
    }
    addError(
      ctx,
      `Cannot use tuple index '.${tupleIndex}' on non-tuple type '${typeToString(resolved)}'`,
      expr.span,
    );
    return [s1, freshTypeVar(), c1];
  }

  // Regular record field access
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

  // Check for division by zero
  if (
    expr.op === "/" &&
    expr.right.kind === "CLit" &&
    expr.right.value.kind === "int" &&
    expr.right.value.value === 0
  ) {
    addError(ctx, "Division by zero", expr.span);
  }

  // Resolve operand types
  const leftType = applySubst(s2, t1);
  const rightType = t2;
  const constraints: Constraint[] = [...c1, ...c2];
  let subst = composeSubst(s1, s2);

  // Helper to check if type is numeric
  const isInt = (t: Type) => t.kind === "TCon" && t.name === "int";
  const isFloat = (t: Type) => t.kind === "TCon" && t.name === "float";
  const isString = (t: Type) => t.kind === "TCon" && t.name === "string";
  const isNumeric = (t: Type) => isInt(t) || isFloat(t);

  let resultType: Type;

  switch (expr.op) {
    case "+": {
      // String concatenation
      if (isString(leftType) && isString(rightType)) {
        resultType = tStr;
        break;
      }
      if (isString(leftType) || isString(rightType)) {
        // One is string, other must be too
        const s3 = unify(ctx, leftType, tStr, expr.span);
        subst = composeSubst(subst, s3);
        const s4 = unify(ctx, applySubst(s3, rightType), tStr, expr.span);
        subst = composeSubst(subst, s4);
        resultType = tStr;
        break;
      }
      // Numeric addition with widening
      if (isNumeric(leftType) && isNumeric(rightType)) {
        // Both are concrete numeric types - widen if either is float
        resultType = isFloat(leftType) || isFloat(rightType) ? tFloat : tInt;
      } else if (isNumeric(leftType) || isNumeric(rightType)) {
        // One is concrete numeric, other is type var - constrain the var
        const concreteType = isNumeric(leftType) ? leftType : rightType;
        const varType = isNumeric(leftType) ? rightType : leftType;
        constraints.push({ className: "Num", type: varType });
        // Result depends on concrete type (may widen when var is resolved)
        resultType = isFloat(concreteType) ? tFloat : freshTypeVar();
        if (resultType.kind === "TVar") {
          constraints.push({ className: "Num", type: resultType });
        }
      } else {
        // Both are type variables - use Num constraint
        constraints.push({ className: "Num", type: leftType });
        constraints.push({ className: "Num", type: rightType });
        const s3 = unify(ctx, leftType, rightType, expr.span);
        subst = composeSubst(subst, s3);
        resultType = applySubst(subst, leftType);
      }
      break;
    }
    case "-":
    case "*": {
      // Numeric with widening
      if (isNumeric(leftType) && isNumeric(rightType)) {
        resultType = isFloat(leftType) || isFloat(rightType) ? tFloat : tInt;
      } else if (isNumeric(leftType) || isNumeric(rightType)) {
        const concreteType = isNumeric(leftType) ? leftType : rightType;
        const varType = isNumeric(leftType) ? rightType : leftType;
        constraints.push({ className: "Num", type: varType });
        resultType = isFloat(concreteType) ? tFloat : freshTypeVar();
        if (resultType.kind === "TVar") {
          constraints.push({ className: "Num", type: resultType });
        }
      } else if (leftType.kind === "TVar" || rightType.kind === "TVar") {
        constraints.push({ className: "Num", type: leftType });
        constraints.push({ className: "Num", type: rightType });
        const s3 = unify(ctx, leftType, rightType, expr.span);
        subst = composeSubst(subst, s3);
        resultType = applySubst(subst, leftType);
      } else {
        addError(
          ctx,
          `Cannot perform arithmetic on '${typeToString(leftType)}' and '${typeToString(rightType)}'`,
          expr.span,
        );
        resultType = tInt;
      }
      break;
    }
    case "/": {
      // Division always returns float
      if (!isNumeric(leftType) && leftType.kind !== "TVar") {
        addError(ctx, `Cannot divide type '${typeToString(leftType)}'`, expr.span);
      }
      if (!isNumeric(rightType) && rightType.kind !== "TVar") {
        addError(ctx, `Cannot divide type '${typeToString(rightType)}'`, expr.span);
      }
      // If type vars, add Num constraint
      if (leftType.kind === "TVar") {
        constraints.push({ className: "Num", type: leftType });
      }
      if (rightType.kind === "TVar") {
        constraints.push({ className: "Num", type: rightType });
      }
      resultType = tFloat;
      break;
    }
    case "==":
    case "!=": {
      // Equality: requires Eq constraint
      const operandType = leftType;
      constraints.push({ className: "Eq", type: operandType });
      const s3 = unify(ctx, rightType, operandType, expr.span);
      subst = composeSubst(subst, s3);
      resultType = tBool;
      break;
    }
    case "<":
    case "<=":
    case ">":
    case ">=": {
      // Comparison: requires Ord constraint
      const operandType = leftType;
      constraints.push({ className: "Ord", type: operandType });
      const s3 = unify(ctx, rightType, operandType, expr.span);
      subst = composeSubst(subst, s3);
      resultType = tBool;
      break;
    }
    default:
      // Unknown operator
      addError(ctx, `Unknown operator: ${expr.op}`, expr.span);
      resultType = freshTypeVar();
  }

  return [subst, resultType, constraints];
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
        const key = `${pat.name.id}:${pat.name.text}`;
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
        const key = `${pat.name.id}:${pat.name.text}`;
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

/** Record types for all variables in a pattern */
const recordPatternTypes = (
  ctx: CheckContext,
  env: TypeEnv,
  subst: Subst,
  pattern: C.CPattern,
  scrutineeType: Type,
): void => {
  const recordInPattern = (pat: C.CPattern, expectedType: Type): void => {
    switch (pat.kind) {
      case "CPWild":
        break;

      case "CPVar":
        recordType(ctx, pat.name, applySubst(subst, expectedType));
        break;

      case "CPLit":
        break;

      case "CPCon": {
        // Get constructor type to find argument types
        const conScheme = env.get(pat.name);
        if (!conScheme) break;
        const conType = instantiate(conScheme);

        // Extract argument types
        const argTypes: Type[] = [];
        let resultType = conType;
        while (resultType.kind === "TFun") {
          argTypes.push(resultType.param);
          resultType = resultType.ret;
        }

        // Unify to get specialized argument types
        const conSubst = unify(ctx, expectedType, resultType);
        for (let i = 0; i < pat.args.length && i < argTypes.length; i++) {
          recordInPattern(pat.args[i]!, applySubst(conSubst, argTypes[i]!));
        }
        break;
      }

      case "CPTuple": {
        if (expectedType.kind === "TTuple") {
          for (let i = 0; i < pat.elements.length; i++) {
            const elemType = expectedType.elements[i];
            if (elemType) {
              recordInPattern(pat.elements[i]!, elemType);
            }
          }
        }
        break;
      }

      case "CPRecord": {
        if (expectedType.kind === "TRecord") {
          for (const f of pat.fields) {
            const fieldType = expectedType.fields.get(f.name);
            if (fieldType) {
              recordInPattern(f.pattern, fieldType);
            }
          }
        }
        break;
      }

      case "CPAs":
        recordType(ctx, pat.name, applySubst(subst, expectedType));
        recordInPattern(pat.pattern, expectedType);
        break;

      case "CPOr":
        // For or-patterns, all alternatives should bind the same variables
        // with the same types, so just process the left side
        recordInPattern(pat.left, expectedType);
        break;
    }
  };

  recordInPattern(pattern, scrutineeType);
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
  // Legacy: Map from Name.id to type (for bindings)
  readonly typeMap: ReadonlyMap<number, Type>;
  // Unified: Map from NodeId to instantiated type (for all expressions)
  readonly nodeTypeMap: ReadonlyMap<NodeId, Type>;
  // Maps span.start to NodeId for LSP position lookups
  readonly spanToNodeId: ReadonlyMap<number, NodeId>;
};

export const checkProgram = (
  program: C.CProgram,
  symbolTableBuilder: SymbolTableBuilder,
  initialEnv: TypeEnv = new Map(),
  initialRegistry: ConstructorRegistry = new Map(),
): CheckOutput => {
  resetTypeVarCounter();

  const ctx = createContext(symbolTableBuilder, initialRegistry);
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

  // Second pass: process foreign declarations first (they have explicit types)
  for (const decl of program.decls) {
    if (decl.kind === "CDeclForeign") {
      const type = convertCoreType(decl.type);
      const key = `${decl.name.id}:${decl.name.text}`;
      // Generalize foreign function types to allow polymorphic usage
      // e.g., `encode : a -> string` should instantiate fresh `a` each time
      const freeVars = [...ftv(type)];
      const foreignScheme = scheme(freeVars, type);
      env.set(key, foreignScheme);
      // Also register with module.name key for lookup
      env.set(`${decl.module}.${decl.jsName}`, foreignScheme);
      recordScheme(ctx, decl.name, foreignScheme);
    }
  }

  // Third pass: collect all let bindings for SCC analysis
  const allBindings: BindingInfo[] = [];
  const bindingByName = new Map<string, BindingInfo>();

  for (const decl of program.decls) {
    if (decl.kind === "CDeclLet") {
      const info: BindingInfo = { name: decl.name, value: decl.value, span: decl.span };
      allBindings.push(info);
      bindingByName.set(decl.name.text, info);
    } else if (decl.kind === "CDeclLetRec") {
      // Explicit let rec bindings are already known to be mutually recursive
      // We'll handle them separately to preserve their grouping
      for (const b of decl.bindings) {
        const info: BindingInfo = { name: b.name, value: b.value, span: decl.span };
        allBindings.push(info);
        bindingByName.set(b.name.text, info);
      }
    }
  }

  // Build dependency graph and find SCCs
  const depGraph = buildDependencyGraph(allBindings);
  const sccs = findSCCs(depGraph);

  // Process each SCC in topological order
  for (const scc of sccs) {
    if (scc.length === 1 && !isSelfRecursive(scc, depGraph)) {
      // Single non-recursive binding: infer and generalize immediately
      const binding = bindingByName.get(scc[0]!)!;
      const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), binding.value);
      subst = composeSubst(subst, s);
      allConstraints.push(...c);

      const key = `${binding.name.id}:${binding.name.text}`;
      const generalizedScheme = generalize(applySubstEnv(subst, env), t);
      env.set(key, generalizedScheme);
      recordScheme(ctx, binding.name, generalizedScheme);
    } else {
      // Mutually recursive bindings: treat as letrec
      const sccBindings = scc.map((name) => bindingByName.get(name)!);

      // Create fresh type variables for all bindings in the SCC
      const bindingTypes = new Map<string, Type>();
      for (const b of sccBindings) {
        const tv = freshTypeVar();
        const key = `${b.name.id}:${b.name.text}`;
        bindingTypes.set(key, tv);
        env.set(key, mono(tv));
      }

      // Infer types for all bindings
      for (const b of sccBindings) {
        const key = `${b.name.id}:${b.name.text}`;
        const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), b.value);
        subst = composeSubst(subst, s);
        allConstraints.push(...c);

        const expectedType = bindingTypes.get(key)!;
        const s2 = unify(ctx, applySubst(subst, expectedType), t, b.span);
        subst = composeSubst(subst, s2);
      }

      // Generalize: remove current bindings from env to allow proper generalization
      const outerEnv = applySubstEnv(subst, env);
      for (const b of sccBindings) {
        const key = `${b.name.id}:${b.name.text}`;
        outerEnv.delete(key);
      }
      for (const b of sccBindings) {
        const key = `${b.name.id}:${b.name.text}`;
        const t = applySubst(subst, bindingTypes.get(key)!);
        const generalizedScheme = generalize(outerEnv, t);
        env.set(key, generalizedScheme);
        recordScheme(ctx, b.name, generalizedScheme);
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
    nodeTypeMap: ctx.nodeTypeMap,
    spanToNodeId: ctx.spanToNodeId,
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

// =============================================================================
// Exhaustiveness Checking
// =============================================================================

/**
 * Check if a set of patterns exhaustively covers all possible values of a type.
 * Returns a list of missing pattern descriptions if not exhaustive.
 */
const checkExhaustiveness = (
  ctx: CheckContext,
  type: Type,
  patterns: readonly C.CPattern[],
): string[] => {
  // If any pattern is a wildcard or variable at the top level, it's exhaustive
  if (patterns.some((p) => p.kind === "CPWild" || p.kind === "CPVar")) {
    return [];
  }

  // Check based on the type
  const resolvedType = resolveType(ctx, type);

  switch (resolvedType.kind) {
    case "TVar":
      // Unknown type - assume exhaustive if we have at least one pattern
      return patterns.length > 0 ? [] : ["_"];

    case "TCon": {
      // Check for bool type specially
      if (resolvedType.name === "bool") {
        return checkBoolExhaustiveness(patterns);
      }
      // For other primitive types (int, float, string, char), literals are never exhaustive
      if (["int", "float", "string", "char"].includes(resolvedType.name)) {
        return ["_"];
      }
      // Check ADT exhaustiveness
      return checkAdtExhaustiveness(ctx, resolvedType.name, patterns);
    }

    case "TApp": {
      // Get the base type name for ADT checking
      const baseName = getBaseTypeName(resolvedType);
      if (baseName) {
        return checkAdtExhaustiveness(ctx, baseName, patterns);
      }
      return [];
    }

    case "TTuple": {
      return checkTupleExhaustiveness(ctx, resolvedType.elements, patterns);
    }

    case "TFun":
      // Function types - can't really pattern match on them
      return [];

    case "TRecord":
      // Record patterns - check if all patterns cover all possibilities
      return checkRecordExhaustiveness(ctx, resolvedType, patterns);

    default:
      return [];
  }
};

/** Resolve type aliases */
const resolveType = (ctx: CheckContext, type: Type): Type => {
  if (type.kind === "TCon") {
    const alias = ctx.aliasRegistry.get(type.name);
    if (alias && alias.params.length === 0) {
      return resolveType(ctx, alias.type);
    }
  }
  return type;
};

/** Get the base type constructor name from a type application */
const getBaseTypeName = (type: Type): string | null => {
  if (type.kind === "TCon") return type.name;
  if (type.kind === "TApp") return getBaseTypeName(type.con);
  return null;
};

/** Check exhaustiveness for bool type */
const checkBoolExhaustiveness = (patterns: readonly C.CPattern[]): string[] => {
  let hasTrue = false;
  let hasFalse = false;

  for (const p of patterns) {
    if (p.kind === "CPWild" || p.kind === "CPVar") return [];
    if (p.kind === "CPLit" && p.value.kind === "bool") {
      if (p.value.value) hasTrue = true;
      else hasFalse = true;
    }
  }

  const missing: string[] = [];
  if (!hasTrue) missing.push("true");
  if (!hasFalse) missing.push("false");
  return missing;
};

/** Check exhaustiveness for ADT types */
const checkAdtExhaustiveness = (
  ctx: CheckContext,
  typeName: string,
  patterns: readonly C.CPattern[],
): string[] => {
  const constructors = ctx.registry.get(typeName);
  if (!constructors) {
    // Unknown type - assume exhaustive
    return [];
  }

  // Track which constructors are covered
  const covered = new Map<string, C.CPattern[]>();
  for (const con of constructors) {
    covered.set(con, []);
  }

  // Helper to process a single pattern
  const processPattern = (p: C.CPattern): boolean => {
    if (p.kind === "CPWild" || p.kind === "CPVar") {
      // Wildcard covers all constructors
      return true;
    }
    if (p.kind === "CPCon") {
      const existing = covered.get(p.name);
      if (existing) {
        existing.push(p);
      }
    }
    if (p.kind === "CPAs") {
      // As-pattern: check the inner pattern
      return processPattern(p.pattern);
    }
    if (p.kind === "CPOr") {
      // Flatten or-patterns (recursively collect all alternatives)
      const alts = flattenOrPattern(p);
      for (const alt of alts) {
        if (processPattern(alt)) return true;
      }
    }
    return false;
  };

  // Collect patterns for each constructor
  for (const p of patterns) {
    if (processPattern(p)) {
      return []; // Exhaustive due to wildcard/variable
    }
  }

  // Check each constructor
  const missing: string[] = [];
  for (const [conName, conPatterns] of covered) {
    if (conPatterns.length === 0) {
      missing.push(conName);
    }
    // TODO: For constructors with arguments, we could recursively check
    // if all argument patterns are exhaustive, but this is complex
  }

  return missing;
};

/** Check exhaustiveness for tuple types */
const checkTupleExhaustiveness = (
  ctx: CheckContext,
  elementTypes: readonly Type[],
  patterns: readonly C.CPattern[],
): string[] => {
  // If any pattern is a wildcard or variable, the tuple match is exhaustive
  if (patterns.some((p) => p.kind === "CPWild" || p.kind === "CPVar")) {
    return [];
  }

  // Collect tuple patterns
  const tuplePatterns = patterns.filter((p) => p.kind === "CPTuple") as C.CPTuple[];

  if (tuplePatterns.length === 0) {
    // No tuple patterns and no wildcards - not exhaustive
    return [`(${elementTypes.map(() => "_").join(", ")})`];
  }

  // For each position, check if it's covered
  // This is a simplified check - full exhaustiveness for tuples is complex
  for (let i = 0; i < elementTypes.length; i++) {
    const positionPatterns = tuplePatterns
      .map((tp) => tp.elements[i])
      .filter(Boolean) as C.CPattern[];
    const missing = checkExhaustiveness(ctx, elementTypes[i]!, positionPatterns);
    if (missing.length > 0) {
      // Position not exhaustive
      const parts = elementTypes.map((_, j) => (j === i ? missing[0] : "_"));
      return [`(${parts.join(", ")})`];
    }
  }

  return [];
};

/** Check exhaustiveness for record types */
const checkRecordExhaustiveness = (
  _ctx: CheckContext,
  _type: Type & { kind: "TRecord" },
  patterns: readonly C.CPattern[],
): string[] => {
  // Record patterns are typically not exhaustive unless there's a wildcard
  if (patterns.some((p) => p.kind === "CPWild" || p.kind === "CPVar")) {
    return [];
  }
  // Records with row variables are open, so we can't enumerate all possibilities
  return [];
};

/** Add a warning for non-exhaustive patterns */
const addExhaustivenessWarning = (
  ctx: CheckContext,
  missing: string[],
  span?: C.CExpr["span"],
): void => {
  const missingStr = missing.slice(0, 3).join(", ") + (missing.length > 3 ? ", ..." : "");
  const message = `Non-exhaustive patterns. Missing: ${missingStr}`;
  const start = span?.start ?? 0;
  const end = span?.end ?? 0;
  ctx.diagnostics.push({
    start,
    end,
    message,
    severity: "warning",
    kind: "non-exhaustive",
  });
};

/** Flatten an or-pattern into a list of alternative patterns */
const flattenOrPattern = (p: C.CPOr): C.CPattern[] => {
  const result: C.CPattern[] = [];
  const flatten = (pat: C.CPattern): void => {
    if (pat.kind === "CPOr") {
      flatten(pat.left);
      flatten(pat.right);
    } else {
      result.push(pat);
    }
  };
  flatten(p);
  return result;
};

// =============================================================================
// Dependency Analysis and SCC
// =============================================================================

type BindingInfo = {
  name: Name;
  value: C.CExpr;
  span?: C.CExpr["span"];
};

/**
 * Collect all free variables referenced in an expression.
 * Returns the set of binding names (original names) that are referenced.
 */
const collectFreeVars = (expr: C.CExpr, bound: Set<string> = new Set()): Set<string> => {
  const freeVars = new Set<string>();

  const collect = (e: C.CExpr, localBound: Set<string>): void => {
    switch (e.kind) {
      case "CVar":
        if (!localBound.has(e.name.text)) {
          freeVars.add(e.name.text);
        }
        break;

      case "CLit":
        break;

      case "CApp":
        collect(e.func, localBound);
        collect(e.arg, localBound);
        break;

      case "CAbs": {
        const newBound = new Set(localBound);
        newBound.add(e.param.text);
        collect(e.body, newBound);
        break;
      }

      case "CLet": {
        collect(e.value, localBound);
        const newBound = new Set(localBound);
        newBound.add(e.name.text);
        collect(e.body, newBound);
        break;
      }

      case "CLetRec": {
        const newBound = new Set(localBound);
        for (const b of e.bindings) {
          newBound.add(b.name.text);
        }
        for (const b of e.bindings) {
          collect(b.value, newBound);
        }
        collect(e.body, newBound);
        break;
      }

      case "CMatch":
        collect(e.scrutinee, localBound);
        for (const c of e.cases) {
          const patBound = new Set(localBound);
          collectPatternBindings(c.pattern, patBound);
          if (c.guard) collect(c.guard, patBound);
          collect(c.body, patBound);
        }
        break;

      case "CCon":
        break;

      case "CTuple":
        for (const elem of e.elements) {
          collect(elem, localBound);
        }
        break;

      case "CRecord":
        for (const f of e.fields) {
          collect(f.value, localBound);
        }
        break;

      case "CRecordUpdate":
        collect(e.record, localBound);
        for (const f of e.fields) {
          collect(f.value, localBound);
        }
        break;

      case "CField":
        collect(e.record, localBound);
        break;

      case "CForeign":
        break;

      case "CBinOp":
        collect(e.left, localBound);
        collect(e.right, localBound);
        break;
    }
  };

  collect(expr, bound);
  return freeVars;
};

/** Collect variable names bound by a pattern into the given set */
const collectPatternBindings = (pattern: C.CPattern, bound: Set<string>): void => {
  switch (pattern.kind) {
    case "CPWild":
    case "CPLit":
      break;
    case "CPVar":
      bound.add(pattern.name.text);
      break;
    case "CPCon":
      for (const arg of pattern.args) {
        collectPatternBindings(arg, bound);
      }
      break;
    case "CPTuple":
      for (const elem of pattern.elements) {
        collectPatternBindings(elem, bound);
      }
      break;
    case "CPRecord":
      for (const f of pattern.fields) {
        collectPatternBindings(f.pattern, bound);
      }
      break;
    case "CPAs":
      bound.add(pattern.name.text);
      collectPatternBindings(pattern.pattern, bound);
      break;
    case "CPOr":
      collectPatternBindings(pattern.left, bound);
      break;
  }
};

/**
 * Build a dependency graph for a set of bindings.
 * Returns a map from binding name to the set of binding names it depends on.
 */
const buildDependencyGraph = (bindings: readonly BindingInfo[]): Map<string, Set<string>> => {
  const bindingNames = new Set(bindings.map((b) => b.name.text));
  const graph = new Map<string, Set<string>>();

  for (const binding of bindings) {
    const freeVars = collectFreeVars(binding.value);
    // Only include dependencies on other bindings in this group
    const deps = new Set([...freeVars].filter((v) => bindingNames.has(v)));
    graph.set(binding.name.text, deps);
  }

  return graph;
};

/**
 * Tarjan's algorithm for finding strongly connected components.
 * Returns SCCs in reverse topological order (dependencies come later).
 */
const findSCCs = (graph: Map<string, Set<string>>): string[][] => {
  const index = new Map<string, number>();
  const lowlink = new Map<string, number>();
  const onStack = new Set<string>();
  const stack: string[] = [];
  const sccs: string[][] = [];
  let indexCounter = 0;

  const strongconnect = (v: string): void => {
    index.set(v, indexCounter);
    lowlink.set(v, indexCounter);
    indexCounter++;
    stack.push(v);
    onStack.add(v);

    const successors = graph.get(v) ?? new Set();
    for (const w of successors) {
      if (!index.has(w)) {
        strongconnect(w);
        lowlink.set(v, Math.min(lowlink.get(v)!, lowlink.get(w)!));
      } else if (onStack.has(w)) {
        lowlink.set(v, Math.min(lowlink.get(v)!, index.get(w)!));
      }
    }

    if (lowlink.get(v) === index.get(v)) {
      const scc: string[] = [];
      let w: string;
      do {
        w = stack.pop()!;
        onStack.delete(w);
        scc.push(w);
      } while (w !== v);
      sccs.push(scc);
    }
  };

  for (const v of graph.keys()) {
    if (!index.has(v)) {
      strongconnect(v);
    }
  }

  // Tarjan's algorithm produces SCCs in topological order:
  // - When A depends on B, we recurse to B first
  // - B's SCC is formed before A's SCC
  // - So dependencies come before dependents in the output
  return sccs;
};

/**
 * Check if an SCC is self-recursive (single binding that references itself)
 */
const isSelfRecursive = (scc: string[], graph: Map<string, Set<string>>): boolean => {
  if (scc.length !== 1) return false;
  const name = scc[0]!;
  const deps = graph.get(name) ?? new Set();
  return deps.has(name);
};

// =============================================================================
// Or-Pattern Variable Binding Validation
// =============================================================================

/**
 * Collect all variable names bound by a pattern.
 */
const collectPatternVars = (pattern: C.CPattern): Set<string> => {
  const vars = new Set<string>();

  const collect = (p: C.CPattern): void => {
    switch (p.kind) {
      case "CPWild":
        break;
      case "CPVar":
        vars.add(p.name.text);
        break;
      case "CPLit":
        break;
      case "CPCon":
        for (const arg of p.args) {
          collect(arg);
        }
        break;
      case "CPTuple":
        for (const elem of p.elements) {
          collect(elem);
        }
        break;
      case "CPRecord":
        for (const field of p.fields) {
          collect(field.pattern);
        }
        break;
      case "CPAs":
        vars.add(p.name.text);
        collect(p.pattern);
        break;
      case "CPOr":
        // For or-patterns, collect from both sides (they should match)
        collect(p.left);
        collect(p.right);
        break;
    }
  };

  collect(pattern);
  return vars;
};

/**
 * Validate that an or-pattern binds the same variables in all alternatives.
 * Returns an error message if validation fails, null otherwise.
 */
const validateOrPatternBindings = (ctx: CheckContext, pattern: C.CPattern): void => {
  const validate = (p: C.CPattern): void => {
    if (p.kind === "CPOr") {
      const leftVars = collectPatternVars(p.left);
      const rightVars = collectPatternVars(p.right);

      // Check that both sides bind the same variables
      const onlyLeft = [...leftVars].filter((v) => !rightVars.has(v));
      const onlyRight = [...rightVars].filter((v) => !leftVars.has(v));

      if (onlyLeft.length > 0 || onlyRight.length > 0) {
        const missing: string[] = [];
        if (onlyLeft.length > 0) {
          missing.push(`'${onlyLeft.join("', '")}' only in left alternative`);
        }
        if (onlyRight.length > 0) {
          missing.push(`'${onlyRight.join("', '")}' only in right alternative`);
        }
        addError(
          ctx,
          `Or-pattern alternatives must bind the same variables. ${missing.join("; ")}`,
          p.span,
        );
      }

      // Recursively validate nested patterns
      validate(p.left);
      validate(p.right);
    } else if (p.kind === "CPCon") {
      for (const arg of p.args) {
        validate(arg);
      }
    } else if (p.kind === "CPTuple") {
      for (const elem of p.elements) {
        validate(elem);
      }
    } else if (p.kind === "CPRecord") {
      for (const field of p.fields) {
        validate(field.pattern);
      }
    } else if (p.kind === "CPAs") {
      validate(p.pattern);
    }
  };

  validate(pattern);
};
