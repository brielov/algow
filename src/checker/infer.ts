/**
 * Expression Type Inference
 *
 * Type inference for all expression forms using Algorithm W.
 */

import type * as C from "../core";
import {
  type Type,
  type TypeEnv,
  type Subst,
  type Constraint,
  type Scheme,
  tInt,
  tFloat,
  tStr,
  tChar,
  tBool,
  tfun,
  ttuple,
  trecord,
  mono,
  scheme,
  freshTypeVar,
  instantiate,
  generalize,
  composeSubst,
  applySubst,
  applySubstEnv,
  typeToString,
} from "../types";
import {
  type CheckContext,
  addError,
  recordType,
  recordScheme,
  recordNodeType,
  recordTupleProjection,
  resolveTupleProjections,
} from "./context";
import { unify } from "./unify";
import { inferPattern, recordPatternTypes, validateOrPatternBindings } from "./patterns";
import { checkExhaustiveness, addExhaustivenessWarning } from "./exhaustiveness";

// =============================================================================
// Expression Inference (Section 8.5)
// =============================================================================

export type InferResult = [Subst, Type, readonly Constraint[]];

export const inferExpr = (ctx: CheckContext, env: TypeEnv, expr: C.CExpr): InferResult => {
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
      return inferForeign(env, expr);
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
  const s2 = resolveTupleProjections(ctx, paramType.name, s, unify);
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
  const bindingSchemes: { name: C.Name; scheme: Scheme }[] = [];
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

const inferForeign = (env: TypeEnv, expr: C.CForeign): InferResult => {
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
