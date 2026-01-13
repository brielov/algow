/**
 * Core AST to IR Lowering (Section 11)
 *
 * Transforms typed Core AST into A-Normal Form (ANF) IR.
 *
 * Key transformations:
 * 1. All intermediate values are bound to names (no nested complex expressions)
 * 2. Arguments to operations become atoms (literals, variables, or constructors)
 * 3. Type information is preserved from the type checker
 */

import * as C from "./core";
import type { Name } from "./core";
import type { Literal, NodeId, Span } from "./surface";
import * as IR from "./ir";
import {
  type Type,
  type TypeEnv,
  type Subst,
  tvar,
  tInt,
  tFloat,
  tStr,
  tBool,
  tChar,
  tfun,
  ttuple,
  trecord,
  applySubst,
  instantiate,
} from "./types";
import type { CheckOutput } from "./checker";
import { type Diagnostic, internalError } from "./diagnostics";

// =============================================================================
// Lowering Context
// =============================================================================

type LowerContext = {
  /** Fresh variable counter */
  varCounter: number;
  /** Substitution from type checker */
  subst: Subst;
  /** Unified type map: NodeId → Type (for all expressions and bindings) */
  nodeTypeMap: ReadonlyMap<NodeId, Type>;
  /** Type environment - used to lookup constructor types */
  typeEnv: TypeEnv;
  /** Set of async foreign function keys (Module.name) */
  asyncForeignFunctions: ReadonlySet<string>;
  /** Collected diagnostics (internal errors) */
  diagnostics: Diagnostic[];
};

const createContext = (
  checkOutput: CheckOutput,
  asyncForeignFunctions: ReadonlySet<string> = new Set(),
): LowerContext => ({
  varCounter: 0,
  subst: checkOutput.subst,
  nodeTypeMap: checkOutput.nodeTypeMap,
  typeEnv: checkOutput.typeEnv,
  asyncForeignFunctions,
  diagnostics: [],
});

/**
 * Generate a fresh name for temporaries.
 * Uses negative IDs to avoid collision with resolved names.
 * The nodeId is synthetic since lowering happens after type checking and
 * types are already attached to IR nodes - these Names are never looked up.
 */
const syntheticSpan: Span = { fileId: 0, start: 0, end: 0 };
const freshName = (ctx: LowerContext, prefix = "_t"): Name => {
  const id = --ctx.varCounter; // Use negative IDs to avoid collision
  return { id, nodeId: id, text: `${prefix}${-id}`, span: syntheticSpan };
};

/**
 * Report an internal compiler error and return a fallback atom.
 * This allows compilation to continue with a placeholder value.
 */
const reportICE = (ctx: LowerContext, span: Span, context: string, details: string): IR.Atom => {
  ctx.diagnostics.push(internalError(span.start, span.end, context, details));
  // Return a placeholder variable to allow compilation to continue
  const name = freshName(ctx, "_ice");
  return IR.avar(name, tvar("_error"));
};

// =============================================================================
// Type Utilities
// =============================================================================

/**
 * Look up the type of a name.
 * Uses nodeTypeMap (unified type storage indexed by NodeId).
 */
const lookupType = (ctx: LowerContext, name: Name): Type => {
  // Primary lookup: by nodeId in unified nodeTypeMap
  if (name.nodeId >= 0) {
    const type = ctx.nodeTypeMap.get(name.nodeId);
    if (type) {
      return applySubst(ctx.subst, type);
    }
  }
  // Fallback: type environment (for constructor types etc.)
  const scheme = ctx.typeEnv.get(`${name.id}:${name.text}`);
  if (scheme) {
    return applySubst(ctx.subst, scheme.type);
  }
  // Unknown type - use a type variable
  return { kind: "TVar", name: `?${name.text}` };
};

/** Look up the instantiated type of an expression by its nodeId */
const lookupExprType = (ctx: LowerContext, nodeId: NodeId): Type | undefined => {
  const type = ctx.nodeTypeMap.get(nodeId);
  if (type) {
    return applySubst(ctx.subst, type);
  }
  return undefined;
};

/**
 * Get constructor argument types, specialized to the scrutinee type.
 * E.g., for Just matching against Maybe (Maybe int), returns [Maybe int]
 * instead of the generic [a].
 */
const getConstructorArgTypes = (
  ctx: LowerContext,
  conName: string,
  scrutineeType: Type,
): Type[] => {
  // Look up constructor scheme in typeEnv
  const scheme = ctx.typeEnv.get(conName);
  if (!scheme) return [];

  // Instantiate the scheme and extract argument types from the function type
  const conType = instantiate(scheme);
  const argTypes: Type[] = [];

  let current = conType;
  while (current.kind === "TFun") {
    argTypes.push(current.param);
    current = current.ret;
  }

  // current is now the return type of the constructor (e.g., Maybe a)
  // Build a substitution by matching current against scrutineeType
  const subst = matchTypes(current, scrutineeType);

  // Apply substitution to specialize argument types
  return argTypes.map((t) => applySubst(subst, t));
};

/**
 * Match a pattern type against a concrete type to build a substitution.
 * This is a simple one-way pattern match (not full unification).
 */
const matchTypes = (pattern: Type, concrete: Type): Map<string, Type> => {
  const subst = new Map<string, Type>();

  const match = (p: Type, c: Type): void => {
    if (p.kind === "TVar") {
      // Type variable matches anything
      subst.set(p.name, c);
    } else if (p.kind === "TCon" && c.kind === "TCon" && p.name === c.name) {
      // Same type constructor
    } else if (p.kind === "TApp" && c.kind === "TApp") {
      // Match type application
      match(p.con, c.con);
      match(p.arg, c.arg);
    } else if (p.kind === "TFun" && c.kind === "TFun") {
      // Match function types
      match(p.param, c.param);
      match(p.ret, c.ret);
    } else if (
      p.kind === "TTuple" &&
      c.kind === "TTuple" &&
      p.elements.length === c.elements.length
    ) {
      // Match tuple types
      for (let i = 0; i < p.elements.length; i++) {
        match(p.elements[i]!, c.elements[i]!);
      }
    }
    // Other cases: no match, skip
  };

  match(pattern, concrete);
  return subst;
};

/** Get literal type */
const literalType = (value: Literal): Type => {
  switch (value.kind) {
    case "int":
      return tInt;
    case "float":
      return tFloat;
    case "string":
      return tStr;
    case "char":
      return tChar;
    case "bool":
      return tBool;
  }
};

// =============================================================================
// ANF Conversion
// =============================================================================

type NormalizeResult = {
  bindings: { name: Name; binding: IR.IRBinding }[];
  atom: IR.Atom;
};

/**
 * Normalize an expression to an atom.
 * If already atomic, return it directly.
 * Otherwise, bind to a fresh variable and return the variable.
 */
const normalize = (ctx: LowerContext, expr: C.CExpr): NormalizeResult => {
  switch (expr.kind) {
    case "CLit": {
      const type = literalType(expr.value);
      return { bindings: [], atom: IR.alit(expr.value, type) };
    }

    case "CVar": {
      const type = lookupType(ctx, expr.name);
      return { bindings: [], atom: IR.avar(expr.name, type) };
    }

    case "CCon": {
      // Look up constructor type from typeEnv
      const scheme = ctx.typeEnv.get(expr.name);
      const type = scheme ? instantiate(scheme) : tvar(`?${expr.name}`);
      return { bindings: [], atom: IR.acon(expr.name, type) };
    }

    default: {
      // Complex expression - lower it and bind to fresh variable
      const lowered = lowerExpr(ctx, expr);

      // If lowered is just an atom, return it directly
      if (lowered.kind === "IRAtom") {
        return { bindings: [], atom: lowered.atom };
      }

      // For expressions that are a chain of lets ending in an atom, extract bindings
      if (canExtractBindings(lowered)) {
        const { bindings, finalAtom } = extractBindings(ctx, lowered, expr.span);
        return { bindings, atom: finalAtom };
      }

      // For match expressions (or lets ending in match), bind to a fresh variable
      const { bindings: letBindings, finalExpr } = extractBindingsUntilNonLet(lowered);
      if (finalExpr.kind === "IRMatch") {
        const name = freshName(ctx, "$v");
        const type = finalExpr.type;
        const binding = IR.irbmatch(finalExpr.scrutinee, finalExpr.cases, type);
        return { bindings: [...letBindings, { name, binding }], atom: IR.avar(name, type) };
      }

      // For other complex expressions, report ICE and return placeholder
      const atom = reportICE(
        ctx,
        expr.span,
        "normalize",
        `unexpected IR expression kind: ${finalExpr.kind}`,
      );
      return { bindings: letBindings, atom };
    }
  }
};

/**
 * Extract let bindings until we hit a non-let expression.
 * Returns the bindings and the final non-let expression.
 */
const extractBindingsUntilNonLet = (
  expr: IR.IRExpr,
): { bindings: { name: Name; binding: IR.IRBinding }[]; finalExpr: IR.IRExpr } => {
  const bindings: { name: Name; binding: IR.IRBinding }[] = [];
  let current = expr;

  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  if (current.kind === "IRLetRec") {
    bindings.push(...current.bindings);
    current = current.body;
  }

  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  return { bindings, finalExpr: current };
};

/**
 * Check if an IR expression can have bindings extracted (chain of lets ending in atom).
 */
const canExtractBindings = (expr: IR.IRExpr): boolean => {
  let current = expr;
  while (current.kind === "IRLet") {
    current = current.body;
  }
  if (current.kind === "IRLetRec") {
    current = current.body;
  }
  while (current.kind === "IRLet") {
    current = current.body;
  }
  return current.kind === "IRAtom";
};

/**
 * Extract all let bindings from an IR expression.
 * Returns the bindings and the final atom.
 */
const extractBindings = (
  ctx: LowerContext,
  expr: IR.IRExpr,
  span: Span,
): { bindings: { name: Name; binding: IR.IRBinding }[]; finalAtom: IR.Atom } => {
  const bindings: { name: Name; binding: IR.IRBinding }[] = [];
  let current = expr;

  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  if (current.kind === "IRLetRec") {
    bindings.push(...current.bindings);
    current = current.body;
  }

  // Handle nested lets after letrec
  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  if (current.kind !== "IRAtom") {
    // Report ICE and return placeholder atom
    const atom = reportICE(
      ctx,
      span,
      "extractBindings",
      `expected atom at end of ANF expression, got ${current.kind}`,
    );
    return { bindings, finalAtom: atom };
  }

  return { bindings, finalAtom: current.atom };
};

/**
 * Wrap an expression with let bindings.
 */
const wrapWithBindings = (
  bindings: { name: Name; binding: IR.IRBinding }[],
  body: IR.IRExpr,
): IR.IRExpr => {
  let result = body;
  for (let i = bindings.length - 1; i >= 0; i--) {
    const { name, binding } = bindings[i]!;
    result = IR.irlet(name, binding, result);
  }
  return result;
};

// =============================================================================
// Expression Lowering
// =============================================================================

const lowerExpr = (ctx: LowerContext, expr: C.CExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "CLit": {
      const type = literalType(expr.value);
      return IR.iratom(IR.alit(expr.value, type));
    }

    case "CVar": {
      const type = lookupType(ctx, expr.name);
      return IR.iratom(IR.avar(expr.name, type));
    }

    case "CCon": {
      // Look up constructor type from typeEnv
      const scheme = ctx.typeEnv.get(expr.name);
      const type = scheme ? instantiate(scheme) : tvar(`?${expr.name}`);
      return IR.iratom(IR.acon(expr.name, type));
    }

    case "CApp":
      return lowerApp(ctx, expr);

    case "CAbs":
      return lowerAbs(ctx, expr);

    case "CLet":
      return lowerLet(ctx, expr);

    case "CLetRec":
      return lowerLetRec(ctx, expr);

    case "CMatch":
      return lowerMatch(ctx, expr);

    case "CTuple":
      return lowerTuple(ctx, expr);

    case "CRecord":
      return lowerRecord(ctx, expr);

    case "CRecordUpdate":
      return lowerRecordUpdate(ctx, expr);

    case "CField":
      return lowerField(ctx, expr);

    case "CForeign":
      return lowerForeign(ctx, expr);

    case "CBinOp":
      return lowerBinOp(ctx, expr);
  }
};

const lowerApp = (ctx: LowerContext, expr: C.CApp): IR.IRExpr => {
  // Normalize function and argument to atoms
  const funcResult = normalize(ctx, expr.func);
  const argResult = normalize(ctx, expr.arg);

  // Get return type - use nodeId-based lookup for polymorphic instantiation
  let returnType = lookupExprType(ctx, expr.nodeId);
  if (!returnType) {
    // Fall back to deriving from function type
    const funcType = funcResult.atom.type;
    returnType = funcType.kind === "TFun" ? funcType.ret : tvar("_ret");
  }

  // Create application binding
  const binding = IR.irbapp(funcResult.atom, argResult.atom, returnType);

  // Generate fresh name for result
  const name = freshName(ctx);

  // Build the let expression
  const body = IR.iratom(IR.avar(name, returnType));
  const result = IR.irlet(name, binding, body);

  // Wrap with normalization bindings
  return wrapWithBindings([...funcResult.bindings, ...argResult.bindings], result);
};

const lowerAbs = (ctx: LowerContext, expr: C.CAbs): IR.IRExpr => {
  // Get parameter type from type map
  const paramType = lookupType(ctx, expr.param);

  // Lower the body
  const bodyIR = lowerExpr(ctx, expr.body);
  const bodyType = IR.exprType(bodyIR);

  // Create function type
  const funcType = tfun(paramType, bodyType);

  // Create lambda binding
  const binding = IR.irblambda(expr.param, bodyIR, funcType);

  // Generate fresh name for the lambda
  const name = freshName(ctx, "_fn");

  // Build the let expression
  const body = IR.iratom(IR.avar(name, funcType));
  return IR.irlet(name, binding, body);
};

const lowerLet = (ctx: LowerContext, expr: C.CLet): IR.IRExpr => {
  // Lower the value
  const valueIR = lowerExpr(ctx, expr.value);

  // Extract bindings from value
  if (valueIR.kind === "IRAtom") {
    // Simple case: value is already an atom
    const binding = IR.irbatom(valueIR.atom);
    const bodyIR = lowerExpr(ctx, expr.body);
    return IR.irlet(expr.name, binding, bodyIR);
  }

  // Try to extract bindings ending in an atom
  if (canExtractBindings(valueIR)) {
    const { bindings, finalAtom } = extractBindings(ctx, valueIR, expr.span);
    const binding = IR.irbatom(finalAtom);
    const bodyIR = lowerExpr(ctx, expr.body);
    const result = IR.irlet(expr.name, binding, bodyIR);
    return wrapWithBindings(bindings, result);
  }

  // Handle let chains ending in match
  const { bindings, finalExpr } = extractBindingsUntilNonLet(valueIR);
  if (finalExpr.kind === "IRMatch") {
    const matchBinding = IR.irbmatch(finalExpr.scrutinee, finalExpr.cases, finalExpr.type);
    const bodyIR = lowerExpr(ctx, expr.body);
    const result = IR.irlet(expr.name, matchBinding, bodyIR);
    return wrapWithBindings(bindings, result);
  }

  // Report ICE and return placeholder expression
  const atom = reportICE(
    ctx,
    expr.span,
    "lowerLet",
    `unexpected IR expression kind: ${finalExpr.kind}`,
  );
  return IR.iratom(atom);
};

const lowerLetRec = (ctx: LowerContext, expr: C.CLetRec): IR.IRExpr => {
  // Lower all bindings
  const irBindings: { name: Name; binding: IR.IRBinding }[] = [];

  for (const b of expr.bindings) {
    const valueIR = lowerExpr(ctx, b.value);

    // For letrec, values should be lambdas
    if (valueIR.kind === "IRLet" && valueIR.binding.kind === "IRBLambda") {
      // The value is a let binding a lambda - use the lambda directly
      irBindings.push({ name: b.name, binding: valueIR.binding });
    } else if (valueIR.kind === "IRAtom") {
      // Simple atom
      irBindings.push({ name: b.name, binding: IR.irbatom(valueIR.atom) });
    } else {
      // Complex expression - extract the binding
      const { bindings, finalAtom } = extractBindings(ctx, valueIR, b.value.span);
      // Add any intermediate bindings
      for (const binding of bindings) {
        irBindings.push(binding);
      }
      irBindings.push({ name: b.name, binding: IR.irbatom(finalAtom) });
    }
  }

  // Lower the body
  const bodyIR = lowerExpr(ctx, expr.body);

  return IR.irletrec(irBindings, bodyIR);
};

/**
 * Expand or-patterns in a case into multiple cases.
 * For example: `Left a | Right a -> body` becomes two cases:
 *   `Left a -> body` and `Right a -> body`
 *
 * Important: The body was resolved with the left branch's variable bindings,
 * so we must rewrite the right branch's pattern variables to use the same
 * Name IDs as the left branch.
 */
const expandOrPatterns = (cases: readonly C.CCase[]): C.CCase[] => {
  const expanded: C.CCase[] = [];

  for (const c of cases) {
    expandCase(c.pattern, c.guard, c.body, null, expanded);
  }

  return expanded;
};

/**
 * Collect variable bindings from a pattern (original name -> Name).
 */
const collectPatternBindings = (pattern: C.CPattern): Map<string, Name> => {
  const bindings = new Map<string, Name>();

  const collect = (p: C.CPattern): void => {
    switch (p.kind) {
      case "CPVar":
        bindings.set(p.name.text, p.name);
        break;
      case "CPCon":
        p.args.forEach(collect);
        break;
      case "CPTuple":
        p.elements.forEach(collect);
        break;
      case "CPRecord":
        p.fields.forEach((f) => collect(f.pattern));
        break;
      case "CPAs":
        bindings.set(p.name.text, p.name);
        collect(p.pattern);
        break;
      case "CPOr":
        collect(p.left);
        break;
      case "CPWild":
      case "CPLit":
        break;
    }
  };

  collect(pattern);
  return bindings;
};

/**
 * Rewrite pattern variables to use names from the given map.
 */
const rewritePatternVars = (pattern: C.CPattern, nameMap: Map<string, Name>): C.CPattern => {
  switch (pattern.kind) {
    case "CPWild":
    case "CPLit":
      return pattern;

    case "CPVar": {
      const newName = nameMap.get(pattern.name.text);
      if (newName) {
        return C.cpvar(pattern.nodeId, newName, pattern.span);
      }
      return pattern;
    }

    case "CPCon":
      return C.cpcon(
        pattern.nodeId,
        pattern.name,
        pattern.args.map((a) => rewritePatternVars(a, nameMap)),
        pattern.span,
      );

    case "CPTuple":
      return C.cptuple(
        pattern.nodeId,
        pattern.elements.map((e) => rewritePatternVars(e, nameMap)),
        pattern.span,
      );

    case "CPRecord":
      return C.cprecord(
        pattern.nodeId,
        pattern.fields.map((f) => ({
          name: f.name,
          pattern: rewritePatternVars(f.pattern, nameMap),
        })),
        pattern.span,
      );

    case "CPAs": {
      const newName = nameMap.get(pattern.name.text);
      return C.cpas(
        pattern.nodeId,
        newName ?? pattern.name,
        rewritePatternVars(pattern.pattern, nameMap),
        pattern.span,
      );
    }

    case "CPOr":
      return C.cpor(
        pattern.nodeId,
        rewritePatternVars(pattern.left, nameMap),
        rewritePatternVars(pattern.right, nameMap),
        pattern.span,
      );
  }
};

const expandCase = (
  pattern: C.CPattern,
  guard: C.CExpr | null,
  body: C.CExpr,
  leftBindings: Map<string, Name> | null,
  result: C.CCase[],
): void => {
  if (pattern.kind === "CPOr") {
    // Collect bindings from the left branch (used for body resolution)
    const bindings = leftBindings ?? collectPatternBindings(pattern.left);

    // Expand left branch (no rewriting needed - it has the original names)
    expandCase(pattern.left, guard, body, bindings, result);

    // Expand right branch with rewritten variable names
    const rewrittenRight = rewritePatternVars(pattern.right, bindings);
    expandCase(rewrittenRight, guard, body, bindings, result);
  } else {
    // No or-pattern at top level, but there might be nested or-patterns
    // For now, we only expand top-level or-patterns
    // Nested or-patterns would require more complex expansion
    result.push({ pattern, guard, body });
  }
};

const lowerMatch = (ctx: LowerContext, expr: C.CMatch): IR.IRExpr => {
  // Normalize scrutinee to atom
  const scrutineeResult = normalize(ctx, expr.scrutinee);

  // Expand or-patterns into multiple cases
  const expandedCases = expandOrPatterns(expr.cases);

  // Lower each case
  const irCases: IR.IRCase[] = [];
  let resultType: Type | null = null;

  for (const c of expandedCases) {
    // Lower the pattern
    const irPattern = lowerPattern(ctx, c.pattern, scrutineeResult.atom.type);

    // Lower guard if present
    const guardIR = c.guard ? lowerExpr(ctx, c.guard) : null;

    // Lower body
    const bodyIR = lowerExpr(ctx, c.body);
    resultType = IR.exprType(bodyIR);

    irCases.push({ pattern: irPattern, guard: guardIR, body: bodyIR });
  }

  if (!resultType) {
    resultType = { kind: "TVar", name: "_match" };
  }

  // Create match expression
  const matchExpr = IR.irmatch(scrutineeResult.atom, irCases, resultType);

  // If there are bindings from normalizing scrutinee, wrap them
  if (scrutineeResult.bindings.length > 0) {
    return wrapWithBindings(scrutineeResult.bindings, matchExpr);
  }

  return matchExpr;
};

const lowerTuple = (ctx: LowerContext, expr: C.CTuple): IR.IRExpr => {
  // Handle empty tuple - unit value
  if (expr.elements.length === 0) {
    const type = ttuple([]);
    const name = freshName(ctx);
    const binding = IR.irbtuple([], type);
    const body = IR.iratom(IR.avar(name, type));
    return IR.irlet(name, binding, body);
  }

  // Normalize all elements
  const allBindings: { name: Name; binding: IR.IRBinding }[] = [];
  const atoms: IR.Atom[] = [];

  for (const elem of expr.elements) {
    const result = normalize(ctx, elem);
    allBindings.push(...result.bindings);
    atoms.push(result.atom);
  }

  // Build tuple type
  const elemTypes = atoms.map((a) => a.type);
  const tupleType = ttuple(elemTypes);

  // Create tuple binding
  const binding = IR.irbtuple(atoms, tupleType);
  const name = freshName(ctx);

  const body = IR.iratom(IR.avar(name, tupleType));
  const result = IR.irlet(name, binding, body);

  return wrapWithBindings(allBindings, result);
};

const lowerRecord = (ctx: LowerContext, expr: C.CRecord): IR.IRExpr => {
  // Normalize all field values
  const allBindings: { name: Name; binding: IR.IRBinding }[] = [];
  const irFields: { name: string; value: IR.Atom }[] = [];

  for (const f of expr.fields) {
    const result = normalize(ctx, f.value);
    allBindings.push(...result.bindings);
    irFields.push({ name: f.name, value: result.atom });
  }

  // Build record type
  const fieldTypes: [string, Type][] = irFields.map((f) => [f.name, f.value.type]);
  const recordType = trecord(fieldTypes);

  // Create record binding
  const binding = IR.irbrecord(irFields, recordType);
  const name = freshName(ctx);

  const body = IR.iratom(IR.avar(name, recordType));
  const result = IR.irlet(name, binding, body);

  return wrapWithBindings(allBindings, result);
};

const lowerRecordUpdate = (ctx: LowerContext, expr: C.CRecordUpdate): IR.IRExpr => {
  // Normalize base record
  const baseResult = normalize(ctx, expr.record);

  // Normalize field values
  const allBindings = [...baseResult.bindings];
  const irFields: { name: string; value: IR.Atom }[] = [];

  for (const f of expr.fields) {
    const result = normalize(ctx, f.value);
    allBindings.push(...result.bindings);
    irFields.push({ name: f.name, value: result.atom });
  }

  // Build result type (same as base type with updated fields)
  let resultType: Type;
  const baseType = baseResult.atom.type;
  if (baseType.kind === "TRecord") {
    const fieldMap = new Map(baseType.fields);
    for (const f of irFields) {
      fieldMap.set(f.name, f.value.type);
    }
    const fieldTypes: [string, Type][] = [...fieldMap.entries()];
    resultType = trecord(fieldTypes, baseType.row);
  } else {
    resultType = baseType;
  }

  // Create record update binding
  const binding = IR.irbrecordupdate(baseResult.atom, irFields, resultType);
  const name = freshName(ctx);

  const body = IR.iratom(IR.avar(name, resultType));
  const result = IR.irlet(name, binding, body);

  return wrapWithBindings(allBindings, result);
};

const lowerField = (ctx: LowerContext, expr: C.CField): IR.IRExpr => {
  // Normalize record
  const recordResult = normalize(ctx, expr.record);

  // Get field type
  let fieldType: Type = { kind: "TVar", name: `_field_${expr.field}` };
  const recordType = recordResult.atom.type;
  if (recordType.kind === "TRecord") {
    const ft = recordType.fields.get(expr.field);
    if (ft) {
      fieldType = ft;
    }
  }

  // Create field access binding
  const binding = IR.irbfield(recordResult.atom, expr.field, fieldType);
  const name = freshName(ctx);

  const body = IR.iratom(IR.avar(name, fieldType));
  const result = IR.irlet(name, binding, body);

  return wrapWithBindings(recordResult.bindings, result);
};

const lowerForeign = (ctx: LowerContext, expr: C.CForeign): IR.IRExpr => {
  // Foreign calls have no arguments in the Core AST
  // They're just references to foreign values
  // The type comes from the foreign declaration
  const type: Type = { kind: "TVar", name: `_foreign_${expr.name}` };

  // Check if this foreign function is async
  const foreignKey = `${expr.module}.${expr.name}`;
  const isAsync = ctx.asyncForeignFunctions.has(foreignKey);

  // Create foreign binding
  const binding = IR.irbforeign(expr.module, expr.name, [], type, isAsync);
  const name = freshName(ctx);

  const body = IR.iratom(IR.avar(name, type));
  return IR.irlet(name, binding, body);
};

const lowerBinOp = (ctx: LowerContext, expr: C.CBinOp): IR.IRExpr => {
  // Normalize both operands to atoms
  const leftResult = normalize(ctx, expr.left);
  const rightResult = normalize(ctx, expr.right);

  // Determine result type based on operator
  let resultType: Type;
  switch (expr.op) {
    case "+":
    case "-":
    case "*":
    case "/":
      resultType = tInt;
      break;
    case "==":
    case "!=":
    case "<":
    case "<=":
    case ">":
    case ">=":
      resultType = tBool;
      break;
    default:
      resultType = tvar(`_binop_${expr.op}`);
  }

  // Create binary operation binding
  const binding = IR.irbbinop(expr.op, leftResult.atom, rightResult.atom, resultType);
  const name = freshName(ctx);

  const body = IR.iratom(IR.avar(name, resultType));
  const result = IR.irlet(name, binding, body);

  // Wrap with normalization bindings
  return wrapWithBindings([...leftResult.bindings, ...rightResult.bindings], result);
};

// =============================================================================
// Pattern Lowering
// =============================================================================

const lowerPattern = (
  ctx: LowerContext,
  pattern: C.CPattern,
  scrutineeType: Type,
): IR.IRPattern => {
  switch (pattern.kind) {
    case "CPWild":
      return IR.irpwild();

    case "CPVar":
      return IR.irpvar(pattern.name, scrutineeType);

    case "CPLit":
      return IR.irplit(pattern.value);

    case "CPCon": {
      // Get constructor argument types, specialized to the scrutinee type
      const argTypes = getConstructorArgTypes(ctx, pattern.name, scrutineeType);
      const args = pattern.args.map((arg, i) => {
        // Apply substitution to resolve nested type variables
        const argType = applySubst(
          ctx.subst,
          argTypes[i] ?? { kind: "TVar" as const, name: `_arg${i}` },
        );
        return lowerPattern(ctx, arg, argType);
      });
      return IR.irpcon(pattern.name, args, scrutineeType);
    }

    case "CPTuple": {
      const elemTypes = scrutineeType.kind === "TTuple" ? scrutineeType.elements : [];
      const elements = pattern.elements.map((elem, i) => {
        const elemType = elemTypes[i] ?? { kind: "TVar" as const, name: `_elem${i}` };
        return lowerPattern(ctx, elem, elemType);
      });
      return IR.irptuple(elements);
    }

    case "CPRecord": {
      const fields = pattern.fields.map((f) => {
        let fieldType: Type = { kind: "TVar", name: `_field_${f.name}` };
        if (scrutineeType.kind === "TRecord") {
          const ft = scrutineeType.fields.get(f.name);
          if (ft) {
            fieldType = ft;
          }
        }
        return {
          name: f.name,
          pattern: lowerPattern(ctx, f.pattern, fieldType),
        };
      });
      return IR.irprecord(fields);
    }

    case "CPAs": {
      // As-pattern: bind the whole value and match the inner pattern
      const innerPattern = lowerPattern(ctx, pattern.pattern, scrutineeType);
      return IR.irpas(pattern.name, innerPattern, scrutineeType);
    }

    case "CPOr": {
      // Or-patterns should be expanded into multiple cases before reaching here.
      // This is handled in lowerMatch by expandOrPatterns.
      // If we get here, it means the expansion didn't happen, which is a bug.
      reportICE(
        ctx,
        pattern.span,
        "lowerPattern",
        "or-pattern should have been expanded before lowering",
      );
      // Return a wildcard pattern as fallback to allow compilation to continue
      return IR.irpwild();
    }
  }
};

// =============================================================================
// Declaration Lowering
// =============================================================================

/**
 * Helper to convert an IR expression body into declarations bound to a name.
 */
const lowerBodyToDecls = (
  ctx: LowerContext,
  body: IR.IRExpr,
  name: Name,
  span: Span,
): IR.IRDecl[] => {
  const decls: IR.IRDecl[] = [];

  if (body.kind === "IRAtom") {
    decls.push(IR.irdecllet(name, IR.irbatom(body.atom)));
    return decls;
  }

  if (canExtractBindings(body)) {
    const { bindings, finalAtom } = extractBindings(ctx, body, span);
    for (const b of bindings) {
      decls.push(IR.irdecllet(b.name, b.binding));
    }
    decls.push(IR.irdecllet(name, IR.irbatom(finalAtom)));
    return decls;
  }

  if (body.kind === "IRLetRec") {
    decls.push(IR.irdeclletrec(body.bindings));
    decls.push(...lowerBodyToDecls(ctx, body.body, name, span));
    return decls;
  }

  if (body.kind === "IRMatch") {
    const binding = IR.irbmatch(body.scrutinee, body.cases, body.type);
    decls.push(IR.irdecllet(name, binding));
    return decls;
  }

  if (body.kind === "IRLet") {
    const { bindings, finalExpr } = extractBindingsUntilNonLet(body);
    for (const b of bindings) {
      decls.push(IR.irdecllet(b.name, b.binding));
    }
    if (finalExpr.kind === "IRMatch") {
      const binding = IR.irbmatch(finalExpr.scrutinee, finalExpr.cases, finalExpr.type);
      decls.push(IR.irdecllet(name, binding));
      return decls;
    }
    if (finalExpr.kind === "IRAtom") {
      decls.push(IR.irdecllet(name, IR.irbatom(finalExpr.atom)));
      return decls;
    }
    // Recursively handle nested letrec
    if (finalExpr.kind === "IRLetRec") {
      decls.push(IR.irdeclletrec(finalExpr.bindings));
      decls.push(...lowerBodyToDecls(ctx, finalExpr.body, name, span));
      return decls;
    }
  }

  // Report ICE and return placeholder declaration
  const atom = reportICE(ctx, span, "lowerBodyToDecls", `unexpected IR kind: ${body.kind}`);
  decls.push(IR.irdecllet(name, IR.irbatom(atom)));
  return decls;
};

const lowerDecl = (ctx: LowerContext, decl: C.CDecl): IR.IRDecl[] => {
  switch (decl.kind) {
    case "CDeclType": {
      // Convert type declaration to IR type declaration
      const constructors = decl.constructors.map((con, tag) => ({
        name: con.name,
        tag,
        arity: con.fields.length,
      }));
      return [IR.irdecltype(decl.name, constructors)];
    }

    case "CDeclLet": {
      // Lower the value
      const valueIR = lowerExpr(ctx, decl.value);

      // Extract the binding
      if (valueIR.kind === "IRAtom") {
        return [IR.irdecllet(decl.name, IR.irbatom(valueIR.atom))];
      }

      // For expressions that are a chain of lets ending in an atom
      if (canExtractBindings(valueIR)) {
        const { bindings, finalAtom } = extractBindings(ctx, valueIR, decl.span);
        const decls: IR.IRDecl[] = [];

        // Emit all intermediate bindings as declarations
        for (const b of bindings) {
          decls.push(IR.irdecllet(b.name, b.binding));
        }

        // The final binding is for the declared name
        decls.push(IR.irdecllet(decl.name, IR.irbatom(finalAtom)));
        return decls;
      }

      // Handle let chains ending in match
      if (valueIR.kind === "IRLet" || valueIR.kind === "IRMatch") {
        const { bindings, finalExpr } = extractBindingsUntilNonLet(valueIR);
        const decls: IR.IRDecl[] = [];

        // Emit all intermediate bindings as declarations
        for (const b of bindings) {
          decls.push(IR.irdecllet(b.name, b.binding));
        }

        if (finalExpr.kind === "IRMatch") {
          // Create a match binding for the declared name
          const binding = IR.irbmatch(finalExpr.scrutinee, finalExpr.cases, finalExpr.type);
          decls.push(IR.irdecllet(decl.name, binding));
          return decls;
        }

        // If it ended with an atom somehow
        if (finalExpr.kind === "IRAtom") {
          decls.push(IR.irdecllet(decl.name, IR.irbatom(finalExpr.atom)));
          return decls;
        }
      }

      // Handle letrec expressions (e.g., let foo = let rec bar = ... in bar 5)
      if (valueIR.kind === "IRLetRec") {
        const decls: IR.IRDecl[] = [];
        // Emit the letrec bindings as a CDeclLetRec
        decls.push(IR.irdeclletrec(valueIR.bindings));
        // Handle the body using the helper
        decls.push(...lowerBodyToDecls(ctx, valueIR.body, decl.name, decl.span));
        return decls;
      }

      // Report ICE and return placeholder declaration
      const atom = reportICE(ctx, decl.span, "lowerDecl", `unexpected IR kind: ${valueIR.kind}`);
      return [IR.irdecllet(decl.name, IR.irbatom(atom))];
    }

    case "CDeclLetRec": {
      // Lower all bindings
      const irBindings: { name: Name; binding: IR.IRBinding }[] = [];

      for (const b of decl.bindings) {
        const valueIR = lowerExpr(ctx, b.value);

        if (valueIR.kind === "IRLet" && valueIR.binding.kind === "IRBLambda") {
          irBindings.push({ name: b.name, binding: valueIR.binding });
        } else if (valueIR.kind === "IRAtom") {
          irBindings.push({ name: b.name, binding: IR.irbatom(valueIR.atom) });
        } else {
          const { bindings, finalAtom } = extractBindings(ctx, valueIR, b.value.span);
          for (const binding of bindings) {
            irBindings.push(binding);
          }
          irBindings.push({ name: b.name, binding: IR.irbatom(finalAtom) });
        }
      }

      return [IR.irdeclletrec(irBindings)];
    }

    case "CDeclForeign": {
      // Foreign declarations bind the foreign function to a variable
      const type = lookupType(ctx, decl.name);
      // The module is extracted from the name if qualified (e.g., "String.length")
      // Otherwise it's from a module context or "Operators"
      let module = decl.module || "Operators";
      let jsName = decl.jsName;

      // Handle qualified names like "String.length"
      if (decl.name.text.includes(".")) {
        const parts = decl.name.text.split(".");
        // parts[0] is guaranteed by includes(".") check, but satisfy type checker
        if (parts[0]) {
          module = parts[0];
          jsName = parts.slice(1).join(".");
        }
      }

      const binding = IR.irbforeign(module, jsName, [], type, decl.isAsync);
      return [IR.irdecllet(decl.name, binding)];
    }
  }
};

// =============================================================================
// Program Lowering
// =============================================================================

export type LowerResult = {
  program: IR.IRProgram;
  diagnostics: readonly Diagnostic[];
};

/**
 * Lower a typed Core program to ANF IR.
 */
export const lowerProgram = (program: C.CProgram, checkOutput: CheckOutput): LowerResult => {
  // Build set of async foreign functions from declarations
  const asyncForeignFunctions = new Set<string>();
  for (const decl of program.decls) {
    if (decl.kind === "CDeclForeign" && decl.isAsync) {
      asyncForeignFunctions.add(`${decl.module}.${decl.jsName}`);
    }
  }

  const ctx = createContext(checkOutput, asyncForeignFunctions);

  // Lower declarations
  const irDecls: IR.IRDecl[] = [];
  for (const decl of program.decls) {
    irDecls.push(...lowerDecl(ctx, decl));
  }

  // Lower main expression if present
  const main = program.expr ? lowerExpr(ctx, program.expr) : null;

  return {
    program: { decls: irDecls, main },
    diagnostics: ctx.diagnostics,
  };
};
