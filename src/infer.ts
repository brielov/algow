import * as ast from "./ast";

type Type =
  | {
      readonly kind: "TVar";
      readonly name: string;
    }
  | {
      readonly kind: "TCon";
      readonly name: string;
    }
  | {
      readonly kind: "TFun";
      readonly param: Type;
      readonly ret: Type;
    }
  | {
      readonly kind: "TApp";
      readonly con: Type;
      readonly arg: Type;
    }
  | {
      readonly kind: "TRecord";
      readonly fields: ReadonlyMap<string, Type>;
      readonly row: Type | null; // null = closed record, TVar = open record
    }
  | {
      readonly kind: "TTuple";
      readonly elements: readonly Type[];
    };

const tvar = (name: string): Type => ({ kind: "TVar", name });
const tcon = (name: string): Type => ({ kind: "TCon", name });
const tfun = (param: Type, ret: Type): Type => ({ kind: "TFun", param, ret });
const tapp = (con: Type, arg: Type): Type => ({ kind: "TApp", con, arg });
const trecord = (fields: readonly [string, Type][], row: Type | null = null): Type => ({
  kind: "TRecord",
  fields: new Map(fields),
  row,
});
const ttuple = (elements: readonly Type[]): Type => ({ kind: "TTuple", elements });

const tNum = tcon("number");
const tStr = tcon("string");
const tBool = tcon("boolean");
const tList = tcon("List");
const tMaybe = tcon("Maybe");

const tListOf = (elem: Type): Type => tapp(tList, elem);
const tMaybeOf = (elem: Type): Type => tapp(tMaybe, elem);

type Constraint = {
  readonly class_: string;
  readonly type: Type;
};

type Scheme = {
  readonly vars: readonly string[];
  readonly constraints: readonly Constraint[];
  readonly type: Type;
};

type PatternBindings = Map<string, Type>;
export type ConstructorRegistry = Map<string, readonly string[]>;

const scheme = (
  vars: readonly string[],
  type: Type,
  constraints: readonly Constraint[] = [],
): Scheme => ({
  vars,
  constraints,
  type,
});

type TypeEnv = Map<string, Scheme>;
type Subst = Map<string, Type>;

const instances: Map<string, Set<string>> = new Map([
  ["Eq", new Set(["number", "string", "boolean"])],
  ["Ord", new Set(["number", "string"])],
  ["Add", new Set(["number", "string"])],
]);

const applySubst = (subst: Subst, type: Type): Type => {
  switch (type.kind) {
    case "TCon":
      return type;
    case "TVar":
      return subst.get(type.name) ?? type;
    case "TFun":
      return tfun(applySubst(subst, type.param), applySubst(subst, type.ret));
    case "TApp":
      return tapp(applySubst(subst, type.con), applySubst(subst, type.arg));
    case "TRecord": {
      const newFields = new Map<string, Type>();
      for (const [name, fieldType] of type.fields) {
        newFields.set(name, applySubst(subst, fieldType));
      }
      const newRow = type.row ? applySubst(subst, type.row) : null;
      return trecord([...newFields.entries()], newRow);
    }
    case "TTuple":
      return ttuple(type.elements.map((t) => applySubst(subst, t)));
  }
};

const applySubstScheme = (subst: Subst, scheme_: Scheme): Scheme => {
  const filtered: Subst = new Map();
  for (const [name, type] of subst) {
    if (!scheme_.vars.includes(name)) {
      filtered.set(name, type);
    }
  }
  return scheme(scheme_.vars, applySubst(filtered, scheme_.type));
};

const applySubstEnv = (subst: Subst, env: TypeEnv): TypeEnv => {
  const result: TypeEnv = new Map();
  for (const [name, scheme] of env) {
    result.set(name, applySubstScheme(subst, scheme));
  }
  return result;
};

const applySubstConstraint = (subst: Subst, c: Constraint): Constraint => ({
  class_: c.class_,
  type: applySubst(subst, c.type),
});

const applySubstConstraints = (subst: Subst, cs: readonly Constraint[]): Constraint[] =>
  cs.map((c) => applySubstConstraint(subst, c));

const composeSubst = (s1: Subst, s2: Subst): Subst => {
  const result: Subst = new Map();

  for (const [name, type] of s1) {
    result.set(name, applySubst(s2, type));
  }

  for (const [name, type] of s2) {
    if (!result.has(name)) {
      result.set(name, type);
    }
  }

  return result;
};

const unify = (t1: Type, t2: Type): Subst => {
  if (t1.kind === "TVar" && t2.kind === "TVar" && t1.name === t2.name) {
    return new Map();
  }
  if (t1.kind === "TVar") {
    return bindVar(t1.name, t2);
  }
  if (t2.kind === "TVar") {
    return bindVar(t2.name, t1);
  }
  if (t1.kind === "TCon" && t2.kind === "TCon" && t1.name === t2.name) {
    return new Map();
  }
  if (t1.kind === "TFun" && t2.kind === "TFun") {
    const s1 = unify(t1.param, t2.param);
    const s2 = unify(applySubst(s1, t1.ret), applySubst(s1, t2.ret));
    return composeSubst(s1, s2);
  }
  if (t1.kind === "TApp" && t2.kind === "TApp") {
    const s1 = unify(t1.con, t2.con);
    const s2 = unify(applySubst(s1, t1.arg), applySubst(s1, t2.arg));
    return composeSubst(s1, s2);
  }
  if (t1.kind === "TRecord" && t2.kind === "TRecord") {
    return unifyRecords(t1, t2);
  }
  if (t1.kind === "TTuple" && t2.kind === "TTuple") {
    if (t1.elements.length !== t2.elements.length) {
      throw new TypeError(
        `Tuple arity mismatch: (${t1.elements.length} elements) vs (${t2.elements.length} elements)`,
      );
    }
    let currentSubst: Subst = new Map();
    for (let i = 0; i < t1.elements.length; i++) {
      const elem1 = applySubst(currentSubst, t1.elements[i]!);
      const elem2 = applySubst(currentSubst, t2.elements[i]!);
      const s = unify(elem1, elem2);
      currentSubst = composeSubst(currentSubst, s);
    }
    return currentSubst;
  }
  throw new TypeError(`Cannot unify ${typeToString(t1)} with ${typeToString(t2)}`);
};

const bindVar = (name: string, type: Type): Subst => {
  if (type.kind === "TVar" && type.name === name) {
    return new Map();
  }
  if (freeTypeVars(type).has(name)) {
    throw new TypeError(`Infinite type: ${name} appears in ${typeToString(type)}`);
  }
  return new Map([[name, type]]);
};

let typeVarCounter = 0;

const freshTypeVar = (): Type => {
  return tvar(`t${typeVarCounter++}`);
};

type TRecord = Extract<Type, { kind: "TRecord" }>;

const unifyRecords = (t1: TRecord, t2: TRecord): Subst => {
  const fields1 = new Set(t1.fields.keys());
  const fields2 = new Set(t2.fields.keys());

  // Find common fields and fields unique to each record
  const commonFields = fields1.intersection(fields2);
  const onlyIn1 = fields1.difference(fields2);
  const onlyIn2 = fields2.difference(fields1);

  // Unify common fields
  let currentSubst: Subst = new Map();
  for (const fieldName of commonFields) {
    const fieldType1 = applySubst(currentSubst, t1.fields.get(fieldName)!);
    const fieldType2 = applySubst(currentSubst, t2.fields.get(fieldName)!);
    const s = unify(fieldType1, fieldType2);
    currentSubst = composeSubst(currentSubst, s);
  }

  // Handle row variables for extra fields
  const row1 = t1.row ? applySubst(currentSubst, t1.row) : null;
  const row2 = t2.row ? applySubst(currentSubst, t2.row) : null;

  // Build record types for the extra fields
  const extraFields1: [string, Type][] = [...onlyIn1].map((f) => [
    f,
    applySubst(currentSubst, t1.fields.get(f)!),
  ]);
  const extraFields2: [string, Type][] = [...onlyIn2].map((f) => [
    f,
    applySubst(currentSubst, t2.fields.get(f)!),
  ]);

  if (onlyIn1.size === 0 && onlyIn2.size === 0) {
    // No extra fields, just unify row variables
    if (row1 && row2) {
      const s = unify(row1, row2);
      return composeSubst(currentSubst, s);
    }
    if (row1 && !row2) {
      // row1 must be empty record
      const s = unify(row1, trecord([]));
      return composeSubst(currentSubst, s);
    }
    if (!row1 && row2) {
      // row2 must be empty record
      const s = unify(row2, trecord([]));
      return composeSubst(currentSubst, s);
    }
    // Both closed, fields match
    return currentSubst;
  }

  if (onlyIn1.size > 0 && onlyIn2.size === 0) {
    // t1 has extra fields, t2 must absorb them via its row var
    if (!row2) {
      throw new TypeError(
        `Record field mismatch: t2 missing fields { ${[...onlyIn1].join(", ")} }`,
      );
    }
    // row2 = { extraFields1 | row1 }
    const extraRecord = trecord(extraFields1, row1);
    const s = unify(row2, extraRecord);
    return composeSubst(currentSubst, s);
  }

  if (onlyIn2.size > 0 && onlyIn1.size === 0) {
    // t2 has extra fields, t1 must absorb them via its row var
    if (!row1) {
      throw new TypeError(
        `Record field mismatch: t1 missing fields { ${[...onlyIn2].join(", ")} }`,
      );
    }
    // row1 = { extraFields2 | row2 }
    const extraRecord = trecord(extraFields2, row2);
    const s = unify(row1, extraRecord);
    return composeSubst(currentSubst, s);
  }

  // Both have extra fields - both must be open
  if (!row1) {
    throw new TypeError(`Record field mismatch: t1 missing fields { ${[...onlyIn2].join(", ")} }`);
  }
  if (!row2) {
    throw new TypeError(`Record field mismatch: t2 missing fields { ${[...onlyIn1].join(", ")} }`);
  }

  // Create fresh row variable for combined tail
  const freshRow = freshTypeVar();
  // row1 = { extraFields2 | freshRow }
  const s1 = unify(row1, trecord(extraFields2, freshRow));
  currentSubst = composeSubst(currentSubst, s1);
  // row2 = { extraFields1 | freshRow }
  const s2 = unify(applySubst(currentSubst, row2), trecord(extraFields1, freshRow));
  return composeSubst(currentSubst, s2);
};

const instantiate = (scheme: Scheme): Type => {
  const freshVars = new Map<string, Type>();
  for (const name of scheme.vars) {
    freshVars.set(name, freshTypeVar());
  }
  return applySubst(freshVars, scheme.type);
};

const generalize = (env: TypeEnv, type: Type): Scheme => {
  const typeVars = freeTypeVars(type);
  const envVars = freeEnvVars(env);
  const vars = typeVars.difference(envVars);
  return scheme([...vars], type);
};

const freeTypeVars = (type: Type): Set<string> => {
  switch (type.kind) {
    case "TCon":
      return new Set();
    case "TVar":
      return new Set([type.name]);
    case "TFun":
      return freeTypeVars(type.param).union(freeTypeVars(type.ret));
    case "TApp":
      return freeTypeVars(type.con).union(freeTypeVars(type.arg));
    case "TRecord": {
      let result = new Set<string>();
      for (const fieldType of type.fields.values()) {
        result = result.union(freeTypeVars(fieldType));
      }
      if (type.row) {
        result = result.union(freeTypeVars(type.row));
      }
      return result;
    }
    case "TTuple": {
      let result = new Set<string>();
      for (const elem of type.elements) {
        result = result.union(freeTypeVars(elem));
      }
      return result;
    }
  }
};

const freeSchemeVars = (scheme: Scheme): Set<string> => {
  return freeTypeVars(scheme.type).difference(new Set(scheme.vars));
};

const freeEnvVars = (type: TypeEnv): Set<string> => {
  let result = new Set<string>();
  for (const scheme of type.values()) {
    result = result.union(freeSchemeVars(scheme));
  }
  return result;
};

export const typeToString = (type: Type): string => {
  switch (type.kind) {
    case "TVar":
      return type.name;
    case "TCon":
      return type.name;
    case "TFun": {
      const param =
        type.param.kind === "TFun" ? `(${typeToString(type.param)})` : typeToString(type.param);
      return `${param} -> ${typeToString(type.ret)}`;
    }
    case "TApp": {
      const arg =
        type.arg.kind === "TApp" || type.arg.kind === "TFun"
          ? `(${typeToString(type.arg)})`
          : typeToString(type.arg);
      return `${typeToString(type.con)} ${arg}`;
    }
    case "TRecord": {
      const entries: string[] = [];
      for (const [name, fieldType] of type.fields) {
        entries.push(`${name}: ${typeToString(fieldType)}`);
      }
      if (type.row) {
        const rowStr = typeToString(type.row);
        if (entries.length > 0) {
          return `{ ${entries.join(", ")} | ${rowStr} }`;
        }
        return `{ | ${rowStr} }`;
      }
      return `{ ${entries.join(", ")} }`;
    }
    case "TTuple":
      return `(${type.elements.map(typeToString).join(", ")})`;
  }
};

const solveConstraints = (constraints: readonly Constraint[]): void => {
  for (const c of constraints) {
    if (c.type.kind === "TVar") {
      continue;
    }

    if (c.type.kind === "TCon") {
      const classInstances = instances.get(c.class_);
      if (!classInstances?.has(c.type.name)) {
        throw new TypeError(`Type '${c.type.name}' does not satisfy ${c.class_}`);
      }
    }

    if (c.type.kind === "TFun") {
      throw new TypeError(`Function types does not satisfy ${c.class_}`);
    }
  }
};

export const infer = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.Expr): InferResult => {
  typeVarCounter = 0;
  const [subst, type, constraints] = inferExpr(env, registry, expr);
  const finalConstraints = applySubstConstraints(subst, constraints);
  solveConstraints(finalConstraints);
  return [subst, type, finalConstraints];
};

type InferResult = [Subst, Type, readonly Constraint[]];

const inferExpr = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.Expr): InferResult => {
  switch (expr.kind) {
    case "Abs":
      return inferAbs(env, registry, expr);
    case "App":
      return inferApp(env, registry, expr);
    case "BinOp":
      return inferBinOp(env, registry, expr);
    case "Bool":
      return [new Map(), tBool, []];
    case "If":
      return inferIf(env, registry, expr);
    case "Let":
      return inferLet(env, registry, expr);
    case "LetRec":
      return inferLetRec(env, registry, expr);
    case "Num":
      return [new Map(), tNum, []];
    case "Str":
      return [new Map(), tStr, []];
    case "Tuple":
      return inferTuple(env, registry, expr);
    case "Var":
      return inferVar(env, expr);
    case "Match":
      return inferMatch(env, registry, expr);
    case "FieldAccess":
      return inferFieldAccess(env, registry, expr);
    case "Record":
      return inferRecord(env, registry, expr);
  }
};

const inferFieldAccess = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.FieldAccess,
): InferResult => {
  const [s1, recordType, constraints] = inferExpr(env, registry, expr.record);
  const resolvedType = applySubst(s1, recordType);

  // If it's a type variable, constrain it to be an open record with the field
  if (resolvedType.kind === "TVar") {
    const fieldType = freshTypeVar();
    const rowVar = freshTypeVar();
    const openRecord = trecord([[expr.field, fieldType]], rowVar);
    const s2 = unify(resolvedType, openRecord);
    return [composeSubst(s1, s2), applySubst(s2, fieldType), constraints];
  }

  if (resolvedType.kind !== "TRecord") {
    throw new TypeError(
      `Cannot access field '${expr.field}' on non-record type: ${typeToString(resolvedType)}`,
    );
  }

  // Check if field exists directly
  const fieldType = resolvedType.fields.get(expr.field);
  if (fieldType) {
    return [s1, fieldType, constraints];
  }

  // Field not in known fields - check if record is open
  if (resolvedType.row) {
    // Constrain the row to contain this field
    const newFieldType = freshTypeVar();
    const newRowVar = freshTypeVar();
    const s2 = unify(resolvedType.row, trecord([[expr.field, newFieldType]], newRowVar));
    return [composeSubst(s1, s2), applySubst(s2, newFieldType), constraints];
  }

  // Closed record without the field
  throw new TypeError(
    `Record has no field '${expr.field}'. Available: ${[...resolvedType.fields.keys()].join(", ")}`,
  );
};

const inferRecord = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Record,
): InferResult => {
  let subst: Subst = new Map();
  let constraints: Constraint[] = [];
  const fieldTypes = new Map<string, Type>();

  for (const field of expr.fields) {
    const [s, t, c] = inferExpr(applySubstEnv(subst, env), registry, field.value);
    subst = composeSubst(subst, s);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    fieldTypes.set(field.name, applySubst(subst, t));
  }

  return [subst, trecord([...fieldTypes.entries()]), constraints];
};

const inferAbs = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.Abs): InferResult => {
  const paramType = freshTypeVar();
  const newEnv = new Map(env);
  newEnv.set(expr.param, scheme([], paramType));
  const [subst, bodyType, constraints] = inferExpr(newEnv, registry, expr.body);
  return [subst, tfun(applySubst(subst, paramType), bodyType), constraints];
};

const inferApp = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.App): InferResult => {
  const [s1, funcType, c1] = inferExpr(env, registry, expr.func);
  const [s2, paramType, c2] = inferExpr(applySubstEnv(s1, env), registry, expr.param);
  const returnType = freshTypeVar();
  const s3 = unify(applySubst(s2, funcType), tfun(paramType, returnType));
  const subst = composeSubst(composeSubst(s1, s2), s3);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);
  return [subst, applySubst(s3, returnType), constraints];
};

const inferBinOp = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.BinOp): InferResult => {
  const [s1, leftType, c1] = inferExpr(env, registry, expr.left);
  const [s2, rightType, c2] = inferExpr(applySubstEnv(s1, env), registry, expr.right);
  const s3 = unify(applySubst(s2, leftType), rightType);
  const operandType = applySubst(s3, rightType);
  const subst = composeSubst(composeSubst(s1, s2), s3);
  const constraints = [...applySubstConstraints(subst, c1), ...applySubstConstraints(subst, c2)];
  switch (expr.op) {
    case "+": {
      constraints.push({ class_: "Add", type: operandType });
      return [subst, operandType, constraints];
    }
    case "-":
    case "/":
    case "*": {
      const s4 = unify(operandType, tNum);
      return [composeSubst(subst, s4), tNum, constraints];
    }
    case "<":
    case ">":
    case "<=":
    case ">=": {
      constraints.push({ class_: "Ord", type: operandType });
      return [subst, tBool, constraints];
    }
    case "==":
    case "!=": {
      constraints.push({ class_: "Eq", type: operandType });
      return [subst, tBool, constraints];
    }
  }
};

const inferIf = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.If): InferResult => {
  const [s1, condType, c1] = inferExpr(env, registry, expr.cond);
  const s2 = unify(condType, tBool);
  let subst = composeSubst(s1, s2);
  const [s3, thenType, c2] = inferExpr(applySubstEnv(subst, env), registry, expr.then);
  subst = composeSubst(subst, s3);
  const [s4, elseType, c3] = inferExpr(applySubstEnv(subst, env), registry, expr.else);
  subst = composeSubst(subst, s4);
  const s5 = unify(applySubst(s4, thenType), elseType);
  const finalSubst = composeSubst(subst, s5);
  const constraints = applySubstConstraints(finalSubst, [...c1, ...c2, ...c3]);
  return [finalSubst, applySubst(s5, elseType), constraints];
};

const inferLet = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.Let): InferResult => {
  const [s1, valueType, c1] = inferExpr(env, registry, expr.value);
  const env1 = applySubstEnv(s1, env);
  const scheme = generalize(env1, applySubst(s1, valueType));
  const env2 = new Map(env1);
  env2.set(expr.name, scheme);
  const [s2, bodyType, c2] = inferExpr(env2, registry, expr.body);
  const subst = composeSubst(s1, s2);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);
  return [subst, bodyType, constraints];
};

const inferLetRec = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.LetRec,
): InferResult => {
  const newEnv = new Map(env);
  newEnv.set(expr.name, scheme([], freshTypeVar()));
  const newExpr = ast.let_(expr.name, expr.value, expr.body);
  return inferLet(newEnv, registry, newExpr);
};

const inferVar = (env: TypeEnv, expr: ast.Var): InferResult => {
  const scheme = env.get(expr.name);
  if (!scheme) {
    throw new TypeError(`Unknown variable: ${expr.name}`);
  }
  return [new Map(), instantiate(scheme), []];
};

const inferTuple = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.Tuple): InferResult => {
  if (expr.elements.length === 0) {
    throw new Error("Tuples cannot be empty");
  }

  if (expr.elements.length === 1) {
    return inferExpr(env, registry, expr.elements[0]!);
  }

  let subst: Subst = new Map();
  let constraints: Constraint[] = [];
  const types: Type[] = [];

  for (const elem of expr.elements) {
    const [s, t, c] = inferExpr(applySubstEnv(subst, env), registry, elem);
    subst = composeSubst(subst, s);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    types.push(applySubst(subst, t));
  }

  return [subst, ttuple(types), constraints];
};

const inferPattern = (
  env: TypeEnv,
  pattern: ast.Pattern,
  expectedType: Type,
  subst: Subst,
): [Subst, PatternBindings] => {
  switch (pattern.kind) {
    case "PVar": {
      return [subst, new Map([[pattern.name, applySubst(subst, expectedType)]])];
    }
    case "PWildcard": {
      return [subst, new Map()];
    }
    case "PLit": {
      const litType =
        typeof pattern.value === "number" ? tNum : typeof pattern.value === "string" ? tStr : tBool;
      const s = unify(applySubst(subst, expectedType), litType);
      return [composeSubst(subst, s), new Map()];
    }
    case "PCon": {
      const conScheme = env.get(pattern.name);
      if (!conScheme) {
        throw new TypeError(`Unknown constructor: ${pattern.name}`);
      }
      const conType = instantiate(conScheme);
      const argTypes: Type[] = [];
      let resultType = conType;
      while (resultType.kind === "TFun") {
        argTypes.push(resultType.param);
        resultType = resultType.ret;
      }

      if (argTypes.length !== pattern.args.length) {
        throw new TypeError(
          `Constructor ${pattern.name} expects ${argTypes.length} args, got ${pattern.args.length}`,
        );
      }

      const s1 = unify(applySubst(subst, resultType), applySubst(subst, expectedType));
      let currentSubst = composeSubst(subst, s1);
      const allBindings: PatternBindings = new Map();

      for (let i = 0; i < pattern.args.length; i++) {
        const argType = applySubst(currentSubst, argTypes[i]!);
        const [s, bindings] = inferPattern(env, pattern.args[i]!, argType, currentSubst);
        currentSubst = s;
        for (const [name, type] of bindings) {
          allBindings.set(name, type);
        }
      }
      return [currentSubst, allBindings];
    }
    case "PRecord": {
      const expectedResolved = applySubst(subst, expectedType);

      if (expectedResolved.kind === "TVar") {
        // Build open record type from pattern fields
        const fieldTypes = new Map<string, Type>();
        let currentSubst = subst;
        const allBindings: PatternBindings = new Map();

        for (const field of pattern.fields) {
          const fieldType = freshTypeVar();
          fieldTypes.set(field.name, fieldType);
          const [s, bindings] = inferPattern(env, field.pattern, fieldType, currentSubst);
          currentSubst = s;
          for (const [name, type] of bindings) {
            allBindings.set(name, type);
          }
        }

        // Create open record with row variable for additional fields
        const rowVar = freshTypeVar();
        const recordType = trecord([...fieldTypes.entries()], rowVar);
        const s = unify(applySubst(currentSubst, expectedType), recordType);
        return [composeSubst(currentSubst, s), allBindings];
      }

      if (expectedResolved.kind !== "TRecord") {
        throw new TypeError(
          `Cannot match record pattern against ${typeToString(expectedResolved)}`,
        );
      }

      let currentSubst = subst;
      const allBindings: PatternBindings = new Map();

      for (const field of pattern.fields) {
        let fieldType = expectedResolved.fields.get(field.name);

        if (!fieldType && expectedResolved.row) {
          // Field not in known fields but record is open - constrain row
          const newFieldType = freshTypeVar();
          const newRowVar = freshTypeVar();
          const s = unify(
            applySubst(currentSubst, expectedResolved.row),
            trecord([[field.name, newFieldType]], newRowVar),
          );
          currentSubst = composeSubst(currentSubst, s);
          fieldType = applySubst(currentSubst, newFieldType);
        }

        if (!fieldType) {
          throw new TypeError(
            `Record has no field '${field.name}'. Available: ${[...expectedResolved.fields.keys()].join(", ")}`,
          );
        }

        const [s, bindings] = inferPattern(env, field.pattern, fieldType, currentSubst);
        currentSubst = s;
        for (const [name, type] of bindings) {
          allBindings.set(name, type);
        }
      }

      return [currentSubst, allBindings];
    }
    case "PTuple": {
      const expectedResolved = applySubst(subst, expectedType);

      if (expectedResolved.kind === "TVar") {
        // Build tuple type from pattern elements
        const elemTypes: Type[] = [];
        let currentSubst = subst;
        const allBindings: PatternBindings = new Map();

        for (const elem of pattern.elements) {
          const elemType = freshTypeVar();
          elemTypes.push(elemType);
          const [s, bindings] = inferPattern(env, elem, elemType, currentSubst);
          currentSubst = s;
          for (const [name, type] of bindings) {
            allBindings.set(name, type);
          }
        }

        const tupleType = ttuple(elemTypes);
        const s = unify(applySubst(currentSubst, expectedType), tupleType);
        return [composeSubst(currentSubst, s), allBindings];
      }

      if (expectedResolved.kind !== "TTuple") {
        throw new TypeError(`Cannot match tuple pattern against ${typeToString(expectedResolved)}`);
      }

      if (pattern.elements.length !== expectedResolved.elements.length) {
        throw new TypeError(
          `Tuple pattern has ${pattern.elements.length} elements, but expected ${expectedResolved.elements.length}`,
        );
      }

      let currentSubst = subst;
      const allBindings: PatternBindings = new Map();

      for (let i = 0; i < pattern.elements.length; i++) {
        const elemType = expectedResolved.elements[i]!;
        const [s, bindings] = inferPattern(env, pattern.elements[i]!, elemType, currentSubst);
        currentSubst = s;
        for (const [name, type] of bindings) {
          allBindings.set(name, type);
        }
      }

      return [currentSubst, allBindings];
    }
  }
};

const inferMatch = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.Match): InferResult => {
  if (expr.cases.length === 0) {
    throw new TypeError("Match expression must have at least one case");
  }

  const [s1, scrutineeType, c1] = inferExpr(env, registry, expr.expr);
  let subst = s1;
  let constraints: Constraint[] = [...c1];
  let resultType: Type | null = null;

  for (const case_ of expr.cases) {
    const [s2, bindings] = inferPattern(
      applySubstEnv(subst, env),
      case_.pattern,
      applySubst(subst, scrutineeType),
      subst,
    );
    subst = s2;

    const caseEnv = new Map(applySubstEnv(subst, env));
    for (const [name, type] of bindings) {
      caseEnv.set(name, scheme([], applySubst(subst, type)));
    }

    const [s3, bodyType, c2] = inferExpr(caseEnv, registry, case_.body);
    subst = composeSubst(subst, s3);
    constraints = [...applySubstConstraints(subst, constraints), ...c2];

    if (resultType === null) {
      resultType = bodyType;
    } else {
      const s4 = unify(applySubst(subst, resultType), applySubst(subst, bodyType));
      subst = composeSubst(subst, s4);
      resultType = applySubst(s4, bodyType);
    }
  }

  const patterns = expr.cases.map((c) => c.pattern);
  const missing = checkExhaustiveness(registry, applySubst(subst, scrutineeType), patterns);

  if (missing.length > 0) {
    throw new TypeError(`Non-exhaustive patterns. Missing: ${missing.join(", ")}`);
  }

  return [subst, applySubst(subst, resultType!), constraints];
};

const typeExprToType = (texpr: ast.TypeExpr): Type => {
  switch (texpr.kind) {
    case "TyApp": {
      return tapp(typeExprToType(texpr.con), typeExprToType(texpr.arg));
    }
    case "TyCon": {
      return tcon(texpr.name);
    }
    case "TyFun": {
      return tfun(typeExprToType(texpr.param), typeExprToType(texpr.ret));
    }
    case "TyVar":
      return tvar(texpr.name);
  }
};

export const processDataDecl = (decl: ast.DataDecl): [TypeEnv, ConstructorRegistry] => {
  const env: TypeEnv = new Map();
  const registry: ConstructorRegistry = new Map();

  let resultType = tcon(decl.name);

  for (const param of decl.typeParams) {
    resultType = tapp(resultType, tvar(param));
  }

  const constructorNames: string[] = [];

  for (const con of decl.constructors) {
    constructorNames.push(con.name);

    let conType = resultType;
    for (let i = con.fields.length - 1; i >= 0; i--) {
      conType = tfun(typeExprToType(con.fields[i]!), conType);
    }
    env.set(con.name, scheme([...decl.typeParams], conType));
  }

  registry.set(decl.name, constructorNames);

  return [env, registry];
};

const getTypeConName = (type: Type): string | null => {
  switch (type.kind) {
    case "TApp":
      return getTypeConName(type.con);
    case "TCon":
      return type.name;
    default:
      return null;
  }
};

const getPatternConstructors = (patterns: readonly ast.Pattern[]): Set<string> => {
  const constructors = new Set<string>();
  for (const pattern of patterns) {
    switch (pattern.kind) {
      case "PCon": {
        constructors.add(pattern.name);
        break;
      }
      case "PWildcard":
      case "PVar": {
        return new Set(["*"]);
      }
      case "PLit": {
        break;
      }
    }
  }
  return constructors;
};

const checkExhaustiveness = (
  registry: ConstructorRegistry,
  scrutineeType: Type,
  patterns: readonly ast.Pattern[],
): readonly string[] => {
  const typeName = getTypeConName(scrutineeType);
  if (!typeName) return [];

  const allConstructors = registry.get(typeName);
  if (!allConstructors) {
    return [];
  }

  const matchedConstructors = getPatternConstructors(patterns);
  if (matchedConstructors.has("*")) {
    return [];
  }

  const missing: string[] = [];
  for (const con of allConstructors) {
    if (!matchedConstructors.has(con)) {
      missing.push(con);
    }
  }

  return missing;
};

export const mergeEnvs = (...envs: readonly TypeEnv[]): TypeEnv => {
  const result: TypeEnv = new Map();
  for (const env of envs) {
    for (const [name, s] of env) {
      result.set(name, s);
    }
  }
  return result;
};

export const mergeRegistries = (
  ...registries: readonly ConstructorRegistry[]
): ConstructorRegistry => {
  const result: ConstructorRegistry = new Map();
  for (const reg of registries) {
    for (const [name, cons] of reg) {
      result.set(name, cons);
    }
  }
  return result;
};

export const baseEnv: TypeEnv = new Map([
  // map : ∀a b. (a -> b) -> List a -> List b
  [
    "map",
    scheme(
      ["a", "b"],
      tfun(tfun(tvar("a"), tvar("b")), tfun(tListOf(tvar("a")), tListOf(tvar("b")))),
    ),
  ],
  // head : ∀a. List a -> Maybe a
  ["head", scheme(["a"], tfun(tListOf(tvar("a")), tMaybeOf(tvar("a"))))],
  // tail : ∀a. List a -> Maybe (List a)
  ["tail", scheme(["a"], tfun(tListOf(tvar("a")), tMaybeOf(tListOf(tvar("a")))))],
  // isEmpty : ∀a. List a -> boolean
  ["isEmpty", scheme(["a"], tfun(tListOf(tvar("a")), tBool))],
  // length : ∀a. List a -> number
  ["length", scheme(["a"], tfun(tListOf(tvar("a")), tNum))],
  // concat : ∀a. List a -> List a -> List a
  ["concat", scheme(["a"], tfun(tListOf(tvar("a")), tfun(tListOf(tvar("a")), tListOf(tvar("a")))))],
  // reverse : ∀a. List a -> List a
  ["reverse", scheme(["a"], tfun(tListOf(tvar("a")), tListOf(tvar("a"))))],
  // filter : ∀a. (a -> boolean) -> List a -> List a
  [
    "filter",
    scheme(["a"], tfun(tfun(tvar("a"), tBool), tfun(tListOf(tvar("a")), tListOf(tvar("a"))))),
  ],
  // foldr : ∀a b. (a -> b -> b) -> b -> List a -> b
  [
    "foldr",
    scheme(
      ["a", "b"],
      tfun(
        tfun(tvar("a"), tfun(tvar("b"), tvar("b"))),
        tfun(tvar("b"), tfun(tListOf(tvar("a")), tvar("b"))),
      ),
    ),
  ],
]);
