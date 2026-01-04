// src/eval.ts
var vnum = (value) => ({ kind: "VNum", value });
var vstr = (value) => ({ kind: "VStr", value });
var vbool = (value) => ({ kind: "VBool", value });
var vclosure = (param, body, env) => ({
  kind: "VClosure",
  param,
  body,
  env
});
var vcon = (name, args = []) => ({
  kind: "VCon",
  name,
  args
});
var vtuple = (elements) => ({ kind: "VTuple", elements });
var vrecord = (fields) => ({ kind: "VRecord", fields });
var emptyEnv = new Map;
var extendEnv = (env, name, value) => {
  const newEnv = new Map(env);
  newEnv.set(name, value);
  return newEnv;
};

class RuntimeError extends Error {
  constructor(message) {
    super(message);
    this.name = "RuntimeError";
  }
}
var evaluate = (env, expr) => {
  switch (expr.kind) {
    case "Num":
      return vnum(expr.value);
    case "Str":
      return vstr(expr.value);
    case "Bool":
      return vbool(expr.value);
    case "Var":
      return env.get(expr.name);
    case "Abs":
      return vclosure(expr.param, expr.body, env);
    case "App": {
      const func = evaluate(env, expr.func);
      const arg = evaluate(env, expr.param);
      return apply(func, arg);
    }
    case "Let": {
      const value = evaluate(env, expr.value);
      const newEnv = extendEnv(env, expr.name, value);
      return evaluate(newEnv, expr.body);
    }
    case "LetRec": {
      const newEnv = new Map(env);
      const placeholder = vcon("__placeholder__");
      newEnv.set(expr.name, placeholder);
      const value = evaluate(newEnv, expr.value);
      newEnv.set(expr.name, value);
      if (value.kind === "VClosure") {
        const closureEnv = new Map(value.env);
        closureEnv.set(expr.name, value);
        const fixedClosure = vclosure(value.param, value.body, closureEnv);
        newEnv.set(expr.name, fixedClosure);
        return evaluate(newEnv, expr.body);
      }
      return evaluate(newEnv, expr.body);
    }
    case "If": {
      const cond = evaluate(env, expr.cond);
      return cond.value ? evaluate(env, expr.then) : evaluate(env, expr.else);
    }
    case "BinOp":
      return evalBinOp(env, expr);
    case "Tuple": {
      const elements = expr.elements.map((e) => evaluate(env, e));
      return elements.length === 1 ? elements[0] : vtuple(elements);
    }
    case "Record": {
      const fields = new Map;
      for (const field of expr.fields) {
        fields.set(field.name, evaluate(env, field.value));
      }
      return vrecord(fields);
    }
    case "FieldAccess": {
      const record = evaluate(env, expr.record);
      return record.fields.get(expr.field);
    }
    case "Match":
      return evalMatch(env, expr);
  }
};
var apply = (func, arg) => {
  if (func.kind === "VClosure") {
    const newEnv = extendEnv(func.env, func.param, arg);
    return evaluate(newEnv, func.body);
  }
  return vcon(func.name, [...func.args, arg]);
};
var evalBinOp = (env, expr) => {
  const left = evaluate(env, expr.left);
  const right = evaluate(env, expr.right);
  switch (expr.op) {
    case "-":
      return vnum(left.value - right.value);
    case "*":
      return vnum(left.value * right.value);
    case "/": {
      const divisor = right.value;
      if (divisor === 0)
        throw new RuntimeError("Division by zero");
      return vnum(left.value / divisor);
    }
    case "+":
      if (left.kind === "VNum") {
        return vnum(left.value + right.value);
      }
      return vstr(left.value + right.value);
    case "<":
      if (left.kind === "VNum")
        return vbool(left.value < right.value);
      return vbool(left.value < right.value);
    case ">":
      if (left.kind === "VNum")
        return vbool(left.value > right.value);
      return vbool(left.value > right.value);
    case "<=":
      if (left.kind === "VNum")
        return vbool(left.value <= right.value);
      return vbool(left.value <= right.value);
    case ">=":
      if (left.kind === "VNum")
        return vbool(left.value >= right.value);
      return vbool(left.value >= right.value);
    case "==":
      return vbool(valuesEqual(left, right));
    case "!=":
      return vbool(!valuesEqual(left, right));
  }
};
var valuesEqual = (a, b) => {
  if (a.kind !== b.kind)
    return false;
  switch (a.kind) {
    case "VNum":
      return a.value === b.value;
    case "VStr":
      return a.value === b.value;
    case "VBool":
      return a.value === b.value;
    case "VCon": {
      const bCon = b;
      if (a.name !== bCon.name || a.args.length !== bCon.args.length)
        return false;
      return a.args.every((arg, i) => valuesEqual(arg, bCon.args[i]));
    }
    case "VTuple": {
      const bTuple = b;
      if (a.elements.length !== bTuple.elements.length)
        return false;
      return a.elements.every((elem, i) => valuesEqual(elem, bTuple.elements[i]));
    }
    case "VRecord": {
      const bRecord = b;
      if (a.fields.size !== bRecord.fields.size)
        return false;
      for (const [key, val] of a.fields) {
        const bVal = bRecord.fields.get(key);
        if (bVal === undefined || !valuesEqual(val, bVal))
          return false;
      }
      return true;
    }
    case "VClosure":
      return false;
  }
};
var matchPattern = (pattern, value) => {
  switch (pattern.kind) {
    case "PWildcard":
      return { matched: true, bindings: new Map };
    case "PVar":
      return { matched: true, bindings: new Map([[pattern.name, value]]) };
    case "PLit": {
      if (typeof pattern.value === "number" && value.kind === "VNum") {
        return pattern.value === value.value ? { matched: true, bindings: new Map } : { matched: false };
      }
      if (typeof pattern.value === "string" && value.kind === "VStr") {
        return pattern.value === value.value ? { matched: true, bindings: new Map } : { matched: false };
      }
      if (typeof pattern.value === "boolean" && value.kind === "VBool") {
        return pattern.value === value.value ? { matched: true, bindings: new Map } : { matched: false };
      }
      return { matched: false };
    }
    case "PCon": {
      if (value.kind !== "VCon" || value.name !== pattern.name) {
        return { matched: false };
      }
      if (value.args.length !== pattern.args.length) {
        return { matched: false };
      }
      const bindings = new Map;
      for (let i = 0;i < pattern.args.length; i++) {
        const result = matchPattern(pattern.args[i], value.args[i]);
        if (!result.matched)
          return { matched: false };
        for (const [name, val] of result.bindings) {
          bindings.set(name, val);
        }
      }
      return { matched: true, bindings };
    }
    case "PTuple": {
      if (value.kind !== "VTuple" || value.elements.length !== pattern.elements.length) {
        return { matched: false };
      }
      const bindings = new Map;
      for (let i = 0;i < pattern.elements.length; i++) {
        const result = matchPattern(pattern.elements[i], value.elements[i]);
        if (!result.matched)
          return { matched: false };
        for (const [name, val] of result.bindings) {
          bindings.set(name, val);
        }
      }
      return { matched: true, bindings };
    }
    case "PRecord": {
      if (value.kind !== "VRecord") {
        return { matched: false };
      }
      const bindings = new Map;
      for (const field of pattern.fields) {
        const fieldValue = value.fields.get(field.name);
        if (fieldValue === undefined)
          return { matched: false };
        const result = matchPattern(field.pattern, fieldValue);
        if (!result.matched)
          return { matched: false };
        for (const [name, val] of result.bindings) {
          bindings.set(name, val);
        }
      }
      return { matched: true, bindings };
    }
  }
};
var evalMatch = (env, expr) => {
  const scrutinee = evaluate(env, expr.expr);
  for (const case_ of expr.cases) {
    const result = matchPattern(case_.pattern, scrutinee);
    if (result.matched) {
      let caseEnv = env;
      for (const [name, value] of result.bindings) {
        caseEnv = extendEnv(caseEnv, name, value);
      }
      return evaluate(caseEnv, case_.body);
    }
  }
  throw new Error("Unreachable: exhaustiveness check failed");
};
var valueToString = (value) => {
  switch (value.kind) {
    case "VNum":
      return String(value.value);
    case "VStr":
      return `"${value.value}"`;
    case "VBool":
      return String(value.value);
    case "VClosure":
      return "<function>";
    case "VCon":
      if (value.args.length === 0)
        return value.name;
      return `(${value.name} ${value.args.map(valueToString).join(" ")})`;
    case "VTuple":
      return `(${value.elements.map(valueToString).join(", ")})`;
    case "VRecord": {
      const fields = [...value.fields.entries()].map(([k, v]) => `${k}: ${valueToString(v)}`).join(", ");
      return `{ ${fields} }`;
    }
  }
};
var createConstructorEnv = (constructorNames) => {
  const env = new Map;
  for (const name of constructorNames) {
    env.set(name, vcon(name));
  }
  return env;
};

// src/diagnostics.ts
var error = (start, end, message) => ({
  start,
  end,
  message,
  severity: "error"
});

// src/symbols.ts
var createSymbolTableBuilder = () => ({
  definitions: [],
  references: [],
  scope: new Map
});
var addDefinition = (builder, name, span, kind, type = null) => {
  const def = { name, span, kind, type };
  builder.definitions.push(def);
  const stack = builder.scope.get(name);
  if (stack) {
    stack.push(def);
  } else {
    builder.scope.set(name, [def]);
  }
  return def;
};
var addReference = (builder, name, span) => {
  const stack = builder.scope.get(name);
  const definition = stack && stack.length > 0 ? stack[stack.length - 1] : null;
  const ref = { name, span, definition };
  builder.references.push(ref);
  return ref;
};
var popScope = (builder, name) => {
  const stack = builder.scope.get(name);
  if (stack && stack.length > 0) {
    stack.pop();
  }
};
var finalize = (builder) => ({
  definitions: builder.definitions,
  references: builder.references
});
var findDefinitionAt = (table, position) => {
  for (const def of table.definitions) {
    if (position >= def.span.start && position < def.span.end) {
      return def;
    }
  }
  return null;
};
var findReferenceAt = (table, position) => {
  for (const ref of table.references) {
    if (position >= ref.span.start && position < ref.span.end) {
      return ref;
    }
  }
  return null;
};
var findReferences = (table, definition) => {
  return table.references.filter((ref) => ref.definition === definition);
};
var goToDefinition = (table, position) => {
  const ref = findReferenceAt(table, position);
  if (ref) {
    return ref.definition;
  }
  const def = findDefinitionAt(table, position);
  if (def) {
    return def;
  }
  return null;
};
var findAllOccurrences = (table, position) => {
  const definition = goToDefinition(table, position);
  if (!definition) {
    return { definition: null, references: [] };
  }
  return { definition, references: findReferences(table, definition) };
};

// src/infer.ts
var createContext = () => ({
  diagnostics: [],
  symbols: createSymbolTableBuilder()
});
var addError = (ctx, message, span) => {
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(error(start, end, message));
};
var tvar = (name) => ({ kind: "TVar", name });
var tcon = (name) => ({ kind: "TCon", name });
var tfun = (param, ret) => ({ kind: "TFun", param, ret });
var tapp = (con, arg) => ({ kind: "TApp", con, arg });
var trecord = (fields, row = null) => ({
  kind: "TRecord",
  fields: new Map(fields),
  row
});
var ttuple = (elements) => ({ kind: "TTuple", elements });
var tNum = tcon("number");
var tStr = tcon("string");
var tBool = tcon("boolean");
var instances = new Map([
  ["Eq", new Set(["number", "string", "boolean"])],
  ["Ord", new Set(["number", "string"])],
  ["Add", new Set(["number", "string"])]
]);
var scheme = (vars, type, constraints = []) => ({
  vars,
  constraints,
  type
});
var applySubst = (subst, type) => {
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
      const newFields = new Map;
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
var applySubstScheme = (subst, scheme_) => {
  const filtered = new Map;
  for (const [name, type] of subst) {
    if (!scheme_.vars.includes(name)) {
      filtered.set(name, type);
    }
  }
  return scheme(scheme_.vars, applySubst(filtered, scheme_.type));
};
var applySubstEnv = (subst, env) => {
  const result = new Map;
  for (const [name, s] of env) {
    result.set(name, applySubstScheme(subst, s));
  }
  return result;
};
var applySubstConstraint = (subst, c) => ({
  class_: c.class_,
  type: applySubst(subst, c.type)
});
var applySubstConstraints = (subst, cs) => cs.map((c) => applySubstConstraint(subst, c));
var composeSubst = (s1, s2) => {
  const result = new Map;
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
var unify = (ctx, t1, t2, span) => {
  if (t1.kind === "TVar" && t2.kind === "TVar" && t1.name === t2.name) {
    return new Map;
  }
  if (t1.kind === "TVar") {
    return bindVar(ctx, t1.name, t2);
  }
  if (t2.kind === "TVar") {
    return bindVar(ctx, t2.name, t1);
  }
  if (t1.kind === "TCon" && t2.kind === "TCon" && t1.name === t2.name) {
    return new Map;
  }
  if (t1.kind === "TFun" && t2.kind === "TFun") {
    const s1 = unify(ctx, t1.param, t2.param);
    const s2 = unify(ctx, applySubst(s1, t1.ret), applySubst(s1, t2.ret));
    return composeSubst(s1, s2);
  }
  if (t1.kind === "TApp" && t2.kind === "TApp") {
    const s1 = unify(ctx, t1.con, t2.con);
    const s2 = unify(ctx, applySubst(s1, t1.arg), applySubst(s1, t2.arg));
    return composeSubst(s1, s2);
  }
  if (t1.kind === "TRecord" && t2.kind === "TRecord") {
    return unifyRecords(ctx, t1, t2);
  }
  if (t1.kind === "TTuple" && t2.kind === "TTuple") {
    if (t1.elements.length !== t2.elements.length) {
      addError(ctx, `Tuple arity mismatch: (${t1.elements.length} elements) vs (${t2.elements.length} elements)`);
      return new Map;
    }
    let currentSubst = new Map;
    for (let i = 0;i < t1.elements.length; i++) {
      const elem1 = applySubst(currentSubst, t1.elements[i]);
      const elem2 = applySubst(currentSubst, t2.elements[i]);
      const s = unify(ctx, elem1, elem2);
      currentSubst = composeSubst(currentSubst, s);
    }
    return currentSubst;
  }
  addError(ctx, `Cannot unify ${typeToString(t1)} with ${typeToString(t2)}`, span);
  return new Map;
};
var bindVar = (ctx, name, type) => {
  if (type.kind === "TVar" && type.name === name) {
    return new Map;
  }
  if (freeTypeVars(type).has(name)) {
    addError(ctx, `Infinite type: ${name} appears in ${typeToString(type)}`);
    return new Map;
  }
  return new Map([[name, type]]);
};
var typeVarCounter = 0;
var freshTypeVar = () => {
  return tvar(`t${typeVarCounter++}`);
};
var unifyRecords = (ctx, t1, t2) => {
  const fields1 = new Set(t1.fields.keys());
  const fields2 = new Set(t2.fields.keys());
  const commonFields = fields1.intersection(fields2);
  const onlyIn1 = fields1.difference(fields2);
  const onlyIn2 = fields2.difference(fields1);
  let currentSubst = new Map;
  for (const fieldName of commonFields) {
    const fieldType1 = applySubst(currentSubst, t1.fields.get(fieldName));
    const fieldType2 = applySubst(currentSubst, t2.fields.get(fieldName));
    const s = unify(ctx, fieldType1, fieldType2);
    currentSubst = composeSubst(currentSubst, s);
  }
  const row1 = t1.row ? applySubst(currentSubst, t1.row) : null;
  const row2 = t2.row ? applySubst(currentSubst, t2.row) : null;
  const extraFields1 = [...onlyIn1].map((f) => [
    f,
    applySubst(currentSubst, t1.fields.get(f))
  ]);
  const extraFields2 = [...onlyIn2].map((f) => [
    f,
    applySubst(currentSubst, t2.fields.get(f))
  ]);
  if (onlyIn1.size === 0 && onlyIn2.size === 0) {
    if (row1 && row2) {
      const s = unify(ctx, row1, row2);
      return composeSubst(currentSubst, s);
    }
    if (row1 && !row2) {
      const s = unify(ctx, row1, trecord([]));
      return composeSubst(currentSubst, s);
    }
    if (!row1 && row2) {
      const s = unify(ctx, row2, trecord([]));
      return composeSubst(currentSubst, s);
    }
    return currentSubst;
  }
  if (onlyIn1.size > 0 && onlyIn2.size === 0) {
    if (!row2) {
      addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn1].join(", ")} }`);
      return currentSubst;
    }
    const extraRecord = trecord(extraFields1, row1);
    const s = unify(ctx, row2, extraRecord);
    return composeSubst(currentSubst, s);
  }
  if (onlyIn2.size > 0 && onlyIn1.size === 0) {
    if (!row1) {
      addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn2].join(", ")} }`);
      return currentSubst;
    }
    const extraRecord = trecord(extraFields2, row2);
    const s = unify(ctx, row1, extraRecord);
    return composeSubst(currentSubst, s);
  }
  if (!row1) {
    addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn2].join(", ")} }`);
    return currentSubst;
  }
  if (!row2) {
    addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn1].join(", ")} }`);
    return currentSubst;
  }
  const freshRow = freshTypeVar();
  const s1 = unify(ctx, row1, trecord(extraFields2, freshRow));
  currentSubst = composeSubst(currentSubst, s1);
  const s2 = unify(ctx, applySubst(currentSubst, row2), trecord(extraFields1, freshRow));
  return composeSubst(currentSubst, s2);
};
var instantiate = (s) => {
  const freshVars = new Map;
  for (const name of s.vars) {
    freshVars.set(name, freshTypeVar());
  }
  return applySubst(freshVars, s.type);
};
var generalize = (env, type) => {
  const typeVars = freeTypeVars(type);
  const envVars = freeEnvVars(env);
  const vars = typeVars.difference(envVars);
  return scheme([...vars], type);
};
var freeTypeVars = (type) => {
  switch (type.kind) {
    case "TCon":
      return new Set;
    case "TVar":
      return new Set([type.name]);
    case "TFun":
      return freeTypeVars(type.param).union(freeTypeVars(type.ret));
    case "TApp":
      return freeTypeVars(type.con).union(freeTypeVars(type.arg));
    case "TRecord": {
      let result = new Set;
      for (const fieldType of type.fields.values()) {
        result = result.union(freeTypeVars(fieldType));
      }
      if (type.row) {
        result = result.union(freeTypeVars(type.row));
      }
      return result;
    }
    case "TTuple": {
      let result = new Set;
      for (const elem of type.elements) {
        result = result.union(freeTypeVars(elem));
      }
      return result;
    }
  }
};
var freeSchemeVars = (s) => {
  return freeTypeVars(s.type).difference(new Set(s.vars));
};
var freeEnvVars = (env) => {
  let result = new Set;
  for (const s of env.values()) {
    result = result.union(freeSchemeVars(s));
  }
  return result;
};
var typeToString = (type) => {
  switch (type.kind) {
    case "TVar":
      return type.name;
    case "TCon":
      return type.name;
    case "TFun": {
      const param = type.param.kind === "TFun" ? `(${typeToString(type.param)})` : typeToString(type.param);
      return `${param} -> ${typeToString(type.ret)}`;
    }
    case "TApp": {
      const arg = type.arg.kind === "TApp" || type.arg.kind === "TFun" ? `(${typeToString(type.arg)})` : typeToString(type.arg);
      return `${typeToString(type.con)} ${arg}`;
    }
    case "TRecord": {
      const entries = [];
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
var solveConstraints = (ctx, constraints) => {
  for (const c of constraints) {
    if (c.type.kind === "TVar") {
      continue;
    }
    if (c.type.kind === "TCon") {
      const classInstances = instances.get(c.class_);
      if (!classInstances?.has(c.type.name)) {
        addError(ctx, `Type '${c.type.name}' does not satisfy ${c.class_}`);
      }
    }
    if (c.type.kind === "TFun") {
      addError(ctx, `Function types do not satisfy ${c.class_}`);
    }
  }
};
var infer = (env, registry, expr) => {
  typeVarCounter = 0;
  const ctx = createContext();
  const [subst, type, constraints] = inferExpr(ctx, env, registry, expr);
  const finalConstraints = applySubstConstraints(subst, constraints);
  solveConstraints(ctx, finalConstraints);
  return {
    subst,
    type,
    constraints: finalConstraints,
    diagnostics: ctx.diagnostics,
    symbols: finalize(ctx.symbols)
  };
};
var inferExpr = (ctx, env, registry, expr) => {
  switch (expr.kind) {
    case "Abs":
      return inferAbs(ctx, env, registry, expr);
    case "App":
      return inferApp(ctx, env, registry, expr);
    case "BinOp":
      return inferBinOp(ctx, env, registry, expr);
    case "Bool":
      return [new Map, tBool, []];
    case "If":
      return inferIf(ctx, env, registry, expr);
    case "Let":
      return inferLet(ctx, env, registry, expr);
    case "LetRec":
      return inferLetRec(ctx, env, registry, expr);
    case "Num":
      return [new Map, tNum, []];
    case "Str":
      return [new Map, tStr, []];
    case "Tuple":
      return inferTuple(ctx, env, registry, expr);
    case "Var":
      return inferVar(ctx, env, expr);
    case "Match":
      return inferMatch(ctx, env, registry, expr);
    case "FieldAccess":
      return inferFieldAccess(ctx, env, registry, expr);
    case "Record":
      return inferRecord(ctx, env, registry, expr);
  }
};
var inferFieldAccess = (ctx, env, registry, expr) => {
  const [s1, recordType, constraints] = inferExpr(ctx, env, registry, expr.record);
  const resolvedType = applySubst(s1, recordType);
  if (resolvedType.kind === "TVar") {
    const fieldType2 = freshTypeVar();
    const rowVar = freshTypeVar();
    const openRecord = trecord([[expr.field, fieldType2]], rowVar);
    const s2 = unify(ctx, resolvedType, openRecord);
    return [composeSubst(s1, s2), applySubst(s2, fieldType2), constraints];
  }
  if (resolvedType.kind !== "TRecord") {
    addError(ctx, `Cannot access field '${expr.field}' on non-record type: ${typeToString(resolvedType)}`);
    return [s1, freshTypeVar(), constraints];
  }
  const fieldType = resolvedType.fields.get(expr.field);
  if (fieldType) {
    return [s1, fieldType, constraints];
  }
  if (resolvedType.row) {
    const newFieldType = freshTypeVar();
    const newRowVar = freshTypeVar();
    const s2 = unify(ctx, resolvedType.row, trecord([[expr.field, newFieldType]], newRowVar));
    return [composeSubst(s1, s2), applySubst(s2, newFieldType), constraints];
  }
  addError(ctx, `Record has no field '${expr.field}'. Available: ${[...resolvedType.fields.keys()].join(", ")}`);
  return [s1, freshTypeVar(), constraints];
};
var inferRecord = (ctx, env, registry, expr) => {
  let subst = new Map;
  let constraints = [];
  const fieldTypes = new Map;
  for (const field of expr.fields) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), registry, field.value);
    subst = composeSubst(subst, s);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    fieldTypes.set(field.name, applySubst(subst, t));
  }
  return [subst, trecord([...fieldTypes.entries()]), constraints];
};
var inferAbs = (ctx, env, registry, expr) => {
  const paramType = freshTypeVar();
  const paramSpan = expr.paramSpan ?? expr.span;
  if (paramSpan) {
    addDefinition(ctx.symbols, expr.param, paramSpan, "parameter", paramType);
  }
  const newEnv = new Map(env);
  newEnv.set(expr.param, scheme([], paramType));
  const [subst, bodyType, constraints] = inferExpr(ctx, newEnv, registry, expr.body);
  popScope(ctx.symbols, expr.param);
  return [subst, tfun(applySubst(subst, paramType), bodyType), constraints];
};
var inferApp = (ctx, env, registry, expr) => {
  const [s1, funcType, c1] = inferExpr(ctx, env, registry, expr.func);
  const [s2, paramType, c2] = inferExpr(ctx, applySubstEnv(s1, env), registry, expr.param);
  const returnType = freshTypeVar();
  const s3 = unify(ctx, applySubst(s2, funcType), tfun(paramType, returnType));
  const subst = composeSubst(composeSubst(s1, s2), s3);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);
  return [subst, applySubst(s3, returnType), constraints];
};
var inferBinOp = (ctx, env, registry, expr) => {
  const [s1, leftType, c1] = inferExpr(ctx, env, registry, expr.left);
  const [s2, rightType, c2] = inferExpr(ctx, applySubstEnv(s1, env), registry, expr.right);
  const s3 = unify(ctx, applySubst(s2, leftType), rightType, expr.span);
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
      const s4 = unify(ctx, operandType, tNum, expr.span);
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
var inferIf = (ctx, env, registry, expr) => {
  const [s1, condType, c1] = inferExpr(ctx, env, registry, expr.cond);
  const s2 = unify(ctx, condType, tBool);
  let subst = composeSubst(s1, s2);
  const [s3, thenType, c2] = inferExpr(ctx, applySubstEnv(subst, env), registry, expr.then);
  subst = composeSubst(subst, s3);
  const [s4, elseType, c3] = inferExpr(ctx, applySubstEnv(subst, env), registry, expr.else);
  subst = composeSubst(subst, s4);
  const s5 = unify(ctx, applySubst(s4, thenType), elseType);
  const finalSubst = composeSubst(subst, s5);
  const constraints = applySubstConstraints(finalSubst, [...c1, ...c2, ...c3]);
  return [finalSubst, applySubst(s5, elseType), constraints];
};
var inferLet = (ctx, env, registry, expr) => {
  const [s1, valueType, c1] = inferExpr(ctx, env, registry, expr.value);
  const env1 = applySubstEnv(s1, env);
  const generalizedScheme = generalize(env1, applySubst(s1, valueType));
  const nameSpan = expr.nameSpan ?? expr.span;
  if (nameSpan) {
    addDefinition(ctx.symbols, expr.name, nameSpan, "variable", applySubst(s1, valueType));
  }
  const env2 = new Map(env1);
  env2.set(expr.name, generalizedScheme);
  const [s2, bodyType, c2] = inferExpr(ctx, env2, registry, expr.body);
  popScope(ctx.symbols, expr.name);
  const subst = composeSubst(s1, s2);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);
  return [subst, bodyType, constraints];
};
var inferLetRec = (ctx, env, registry, expr) => {
  const placeholderType = freshTypeVar();
  const envWithPlaceholder = new Map(env);
  envWithPlaceholder.set(expr.name, scheme([], placeholderType));
  const nameSpan = expr.nameSpan ?? expr.span;
  if (nameSpan) {
    addDefinition(ctx.symbols, expr.name, nameSpan, "variable", null);
  }
  const [s1, valueType, c1] = inferExpr(ctx, envWithPlaceholder, registry, expr.value);
  const env1 = applySubstEnv(s1, env);
  const generalizedScheme = generalize(env1, applySubst(s1, valueType));
  if (nameSpan) {
    const defStack = ctx.symbols.scope.get(expr.name);
    if (defStack && defStack.length > 0) {
      const def = defStack[defStack.length - 1];
      def.type = applySubst(s1, valueType);
    }
  }
  const env2 = new Map(env1);
  env2.set(expr.name, generalizedScheme);
  const [s2, bodyType, c2] = inferExpr(ctx, env2, registry, expr.body);
  popScope(ctx.symbols, expr.name);
  const subst = composeSubst(s1, s2);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);
  return [subst, bodyType, constraints];
};
var inferVar = (ctx, env, expr) => {
  if (expr.span) {
    addReference(ctx.symbols, expr.name, expr.span);
  }
  const s = env.get(expr.name);
  if (!s) {
    addError(ctx, `Unknown variable: ${expr.name}`, expr.span);
    return [new Map, freshTypeVar(), []];
  }
  return [new Map, instantiate(s), []];
};
var inferTuple = (ctx, env, registry, expr) => {
  if (expr.elements.length === 0) {
    addError(ctx, "Tuples cannot be empty");
    return [new Map, ttuple([]), []];
  }
  if (expr.elements.length === 1) {
    return inferExpr(ctx, env, registry, expr.elements[0]);
  }
  let subst = new Map;
  let constraints = [];
  const types = [];
  for (const elem of expr.elements) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), registry, elem);
    subst = composeSubst(subst, s);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    types.push(applySubst(subst, t));
  }
  return [subst, ttuple(types), constraints];
};
var inferPattern = (ctx, env, pattern, expectedType, subst) => {
  switch (pattern.kind) {
    case "PVar": {
      const boundType = applySubst(subst, expectedType);
      if (pattern.span) {
        addDefinition(ctx.symbols, pattern.name, pattern.span, "parameter", boundType);
      }
      return [subst, new Map([[pattern.name, boundType]])];
    }
    case "PWildcard": {
      return [subst, new Map];
    }
    case "PLit": {
      const litType = typeof pattern.value === "number" ? tNum : typeof pattern.value === "string" ? tStr : tBool;
      const s = unify(ctx, applySubst(subst, expectedType), litType);
      return [composeSubst(subst, s), new Map];
    }
    case "PCon": {
      if (pattern.span) {
        addReference(ctx.symbols, pattern.name, pattern.span);
      }
      const conScheme = env.get(pattern.name);
      if (!conScheme) {
        addError(ctx, `Unknown constructor: ${pattern.name}`, pattern.span);
        return [subst, new Map];
      }
      const conType = instantiate(conScheme);
      const argTypes = [];
      let resultType = conType;
      while (resultType.kind === "TFun") {
        argTypes.push(resultType.param);
        resultType = resultType.ret;
      }
      if (argTypes.length !== pattern.args.length) {
        addError(ctx, `Constructor ${pattern.name} expects ${argTypes.length} args, got ${pattern.args.length}`);
        return [subst, new Map];
      }
      const s1 = unify(ctx, applySubst(subst, resultType), applySubst(subst, expectedType));
      let currentSubst = composeSubst(subst, s1);
      const allBindings = new Map;
      for (let i = 0;i < pattern.args.length; i++) {
        const argType = applySubst(currentSubst, argTypes[i]);
        const [s, bindings] = inferPattern(ctx, env, pattern.args[i], argType, currentSubst);
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
        const fieldTypes = new Map;
        let currentSubst2 = subst;
        const allBindings2 = new Map;
        for (const field of pattern.fields) {
          const fieldType = freshTypeVar();
          fieldTypes.set(field.name, fieldType);
          const [s2, bindings] = inferPattern(ctx, env, field.pattern, fieldType, currentSubst2);
          currentSubst2 = s2;
          for (const [name, type] of bindings) {
            allBindings2.set(name, type);
          }
        }
        const rowVar = freshTypeVar();
        const recordType = trecord([...fieldTypes.entries()], rowVar);
        const s = unify(ctx, applySubst(currentSubst2, expectedType), recordType);
        return [composeSubst(currentSubst2, s), allBindings2];
      }
      if (expectedResolved.kind !== "TRecord") {
        addError(ctx, `Cannot match record pattern against ${typeToString(expectedResolved)}`);
        return [subst, new Map];
      }
      let currentSubst = subst;
      const allBindings = new Map;
      for (const field of pattern.fields) {
        let fieldType = expectedResolved.fields.get(field.name);
        if (!fieldType && expectedResolved.row) {
          const newFieldType = freshTypeVar();
          const newRowVar = freshTypeVar();
          const s2 = unify(ctx, applySubst(currentSubst, expectedResolved.row), trecord([[field.name, newFieldType]], newRowVar));
          currentSubst = composeSubst(currentSubst, s2);
          fieldType = applySubst(currentSubst, newFieldType);
        }
        if (!fieldType) {
          addError(ctx, `Record has no field '${field.name}'. Available: ${[...expectedResolved.fields.keys()].join(", ")}`);
          continue;
        }
        const [s, bindings] = inferPattern(ctx, env, field.pattern, fieldType, currentSubst);
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
        const elemTypes = [];
        let currentSubst2 = subst;
        const allBindings2 = new Map;
        for (const elem of pattern.elements) {
          const elemType = freshTypeVar();
          elemTypes.push(elemType);
          const [s2, bindings] = inferPattern(ctx, env, elem, elemType, currentSubst2);
          currentSubst2 = s2;
          for (const [name, type] of bindings) {
            allBindings2.set(name, type);
          }
        }
        const tupleType = ttuple(elemTypes);
        const s = unify(ctx, applySubst(currentSubst2, expectedType), tupleType);
        return [composeSubst(currentSubst2, s), allBindings2];
      }
      if (expectedResolved.kind !== "TTuple") {
        addError(ctx, `Cannot match tuple pattern against ${typeToString(expectedResolved)}`);
        return [subst, new Map];
      }
      if (pattern.elements.length !== expectedResolved.elements.length) {
        addError(ctx, `Tuple pattern has ${pattern.elements.length} elements, but expected ${expectedResolved.elements.length}`);
        return [subst, new Map];
      }
      let currentSubst = subst;
      const allBindings = new Map;
      for (let i = 0;i < pattern.elements.length; i++) {
        const elemType = expectedResolved.elements[i];
        const [s, bindings] = inferPattern(ctx, env, pattern.elements[i], elemType, currentSubst);
        currentSubst = s;
        for (const [name, type] of bindings) {
          allBindings.set(name, type);
        }
      }
      return [currentSubst, allBindings];
    }
  }
};
var inferMatch = (ctx, env, registry, expr) => {
  if (expr.cases.length === 0) {
    addError(ctx, "Match expression must have at least one case");
    return [new Map, freshTypeVar(), []];
  }
  const [s1, scrutineeType, c1] = inferExpr(ctx, env, registry, expr.expr);
  let subst = s1;
  let constraints = [...c1];
  let resultType = null;
  for (const case_ of expr.cases) {
    const [s2, bindings] = inferPattern(ctx, applySubstEnv(subst, env), case_.pattern, applySubst(subst, scrutineeType), subst);
    subst = s2;
    const caseEnv = new Map(applySubstEnv(subst, env));
    for (const [name, type] of bindings) {
      caseEnv.set(name, scheme([], applySubst(subst, type)));
    }
    const [s3, bodyType, c2] = inferExpr(ctx, caseEnv, registry, case_.body);
    subst = composeSubst(subst, s3);
    constraints = [...applySubstConstraints(subst, constraints), ...c2];
    if (resultType === null) {
      resultType = bodyType;
    } else {
      const s4 = unify(ctx, applySubst(subst, resultType), applySubst(subst, bodyType));
      subst = composeSubst(subst, s4);
      resultType = applySubst(s4, bodyType);
    }
  }
  const patterns = expr.cases.map((c) => c.pattern);
  const missing = checkExhaustiveness(registry, applySubst(subst, scrutineeType), patterns);
  if (missing.length > 0) {
    addError(ctx, `Non-exhaustive patterns. Missing: ${missing.join(", ")}`);
  }
  return [subst, applySubst(subst, resultType), constraints];
};
var typeExprToType = (texpr) => {
  switch (texpr.kind) {
    case "TyApp":
      return tapp(typeExprToType(texpr.con), typeExprToType(texpr.arg));
    case "TyCon":
      return tcon(texpr.name);
    case "TyFun":
      return tfun(typeExprToType(texpr.param), typeExprToType(texpr.ret));
    case "TyVar":
      return tvar(texpr.name);
  }
};
var processDataDecl = (decl) => {
  const env = new Map;
  const registry = new Map;
  let resultType = tcon(decl.name);
  for (const param of decl.typeParams) {
    resultType = tapp(resultType, tvar(param));
  }
  const constructorNames = [];
  for (const con of decl.constructors) {
    constructorNames.push(con.name);
    let conType = resultType;
    for (let i = con.fields.length - 1;i >= 0; i--) {
      conType = tfun(typeExprToType(con.fields[i]), conType);
    }
    env.set(con.name, scheme([...decl.typeParams], conType));
  }
  registry.set(decl.name, constructorNames);
  return [env, registry];
};
var getTypeConName = (type) => {
  switch (type.kind) {
    case "TApp":
      return getTypeConName(type.con);
    case "TCon":
      return type.name;
    default:
      return null;
  }
};
var getPatternConstructors = (patterns) => {
  const constructors = new Set;
  for (const pattern of patterns) {
    switch (pattern.kind) {
      case "PCon":
        constructors.add(pattern.name);
        break;
      case "PWildcard":
      case "PVar":
        return new Set(["*"]);
      case "PLit":
        break;
    }
  }
  return constructors;
};
var checkExhaustiveness = (registry, scrutineeType, patterns) => {
  const typeName = getTypeConName(scrutineeType);
  if (!typeName)
    return [];
  const allConstructors = registry.get(typeName);
  if (!allConstructors) {
    return [];
  }
  const matchedConstructors = getPatternConstructors(patterns);
  if (matchedConstructors.has("*")) {
    return [];
  }
  const missing = [];
  for (const con of allConstructors) {
    if (!matchedConstructors.has(con)) {
      missing.push(con);
    }
  }
  return missing;
};
var mergeEnvs = (...envs) => {
  const result = new Map;
  for (const env of envs) {
    for (const [name, s] of env) {
      result.set(name, s);
    }
  }
  return result;
};
var mergeRegistries = (...registries) => {
  const result = new Map;
  for (const reg of registries) {
    for (const [name, cons] of reg) {
      result.set(name, cons);
    }
  }
  return result;
};
var baseEnv = new Map;

// src/ast.ts
var num = (value, span) => ({ kind: "Num", value, span });
var bool = (value, span) => ({ kind: "Bool", value, span });
var str = (value, span) => ({ kind: "Str", value, span });
var if_ = (cond, then, else_, span) => ({
  kind: "If",
  cond,
  then,
  else: else_,
  span
});
var let_ = (name, value, body, span, nameSpan) => ({
  kind: "Let",
  name,
  nameSpan,
  value,
  body,
  span
});
var letRec = (name, value, body, span, nameSpan) => ({
  kind: "LetRec",
  name,
  nameSpan,
  value,
  body,
  span
});
var var_ = (name, span) => ({ kind: "Var", name, span });
var abs = (param, body, span, paramSpan) => ({
  kind: "Abs",
  param,
  paramSpan,
  body,
  span
});
var app = (func, param, span) => ({
  kind: "App",
  func,
  param,
  span
});
var tuple = (elements, span) => ({
  kind: "Tuple",
  elements,
  span
});
var record = (fields, span) => ({
  kind: "Record",
  fields,
  span
});
var field = (name, value, span) => ({
  name,
  value,
  span
});
var fieldAccess = (record2, field2, span) => ({
  kind: "FieldAccess",
  record: record2,
  field: field2,
  span
});
var binOp = (op, left, right, span) => ({
  kind: "BinOp",
  op,
  left,
  right,
  span
});
var pwildcard = (span) => ({ kind: "PWildcard", span });
var pvar = (name, span) => ({ kind: "PVar", name, span });
var pcon = (name, args, span) => ({
  kind: "PCon",
  name,
  args,
  span
});
var plit = (value, span) => ({
  kind: "PLit",
  value,
  span
});
var precord = (fields, span) => ({
  kind: "PRecord",
  fields,
  span
});
var pfield = (name, pattern, span) => ({
  name,
  pattern,
  span
});
var ptuple = (elements, span) => ({
  kind: "PTuple",
  elements,
  span
});
var case_ = (pattern, body, span) => ({
  pattern,
  body,
  span
});
var match = (expr, cases, span) => ({
  kind: "Match",
  expr,
  cases,
  span
});
var tyvar = (name, span) => ({ kind: "TyVar", name, span });
var tycon = (name, span) => ({ kind: "TyCon", name, span });
var tyapp = (con, arg, span) => ({
  kind: "TyApp",
  con,
  arg,
  span
});
var conDecl = (name, fields, span) => ({
  name,
  fields,
  span
});
var dataDecl = (name, typeParams, constructors, span) => ({ kind: "DataDecl", name, typeParams, constructors, span });

// src/lexer.ts
var TokenKind;
((TokenKind2) => {
  TokenKind2[TokenKind2["Eof"] = 0] = "Eof";
  TokenKind2[TokenKind2["Number"] = 1] = "Number";
  TokenKind2[TokenKind2["String"] = 2] = "String";
  TokenKind2[TokenKind2["Lower"] = 3] = "Lower";
  TokenKind2[TokenKind2["Upper"] = 4] = "Upper";
  TokenKind2[TokenKind2["Let"] = 5] = "Let";
  TokenKind2[TokenKind2["Rec"] = 6] = "Rec";
  TokenKind2[TokenKind2["In"] = 7] = "In";
  TokenKind2[TokenKind2["If"] = 8] = "If";
  TokenKind2[TokenKind2["Then"] = 9] = "Then";
  TokenKind2[TokenKind2["Else"] = 10] = "Else";
  TokenKind2[TokenKind2["Match"] = 11] = "Match";
  TokenKind2[TokenKind2["With"] = 12] = "With";
  TokenKind2[TokenKind2["End"] = 13] = "End";
  TokenKind2[TokenKind2["Data"] = 14] = "Data";
  TokenKind2[TokenKind2["True"] = 15] = "True";
  TokenKind2[TokenKind2["False"] = 16] = "False";
  TokenKind2[TokenKind2["Plus"] = 17] = "Plus";
  TokenKind2[TokenKind2["Minus"] = 18] = "Minus";
  TokenKind2[TokenKind2["Star"] = 19] = "Star";
  TokenKind2[TokenKind2["Slash"] = 20] = "Slash";
  TokenKind2[TokenKind2["Lt"] = 21] = "Lt";
  TokenKind2[TokenKind2["Le"] = 22] = "Le";
  TokenKind2[TokenKind2["Gt"] = 23] = "Gt";
  TokenKind2[TokenKind2["Ge"] = 24] = "Ge";
  TokenKind2[TokenKind2["EqEq"] = 25] = "EqEq";
  TokenKind2[TokenKind2["Ne"] = 26] = "Ne";
  TokenKind2[TokenKind2["Pipe"] = 27] = "Pipe";
  TokenKind2[TokenKind2["Arrow"] = 28] = "Arrow";
  TokenKind2[TokenKind2["Eq"] = 29] = "Eq";
  TokenKind2[TokenKind2["Bar"] = 30] = "Bar";
  TokenKind2[TokenKind2["Comma"] = 31] = "Comma";
  TokenKind2[TokenKind2["Dot"] = 32] = "Dot";
  TokenKind2[TokenKind2["Underscore"] = 33] = "Underscore";
  TokenKind2[TokenKind2["ColonColon"] = 34] = "ColonColon";
  TokenKind2[TokenKind2["LParen"] = 35] = "LParen";
  TokenKind2[TokenKind2["RParen"] = 36] = "RParen";
  TokenKind2[TokenKind2["LBrace"] = 37] = "LBrace";
  TokenKind2[TokenKind2["RBrace"] = 38] = "RBrace";
  TokenKind2[TokenKind2["Error"] = 39] = "Error";
})(TokenKind ||= {});
var EOF = -1;
var TAB = 9;
var LF = 10;
var CR = 13;
var SPACE = 32;
var DIGIT_0 = 48;
var DIGIT_9 = 57;
var UPPER_A = 65;
var UPPER_Z = 90;
var LOWER_A = 97;
var LOWER_Z = 122;
var UNDERSCORE = 95;
var DOUBLE_QUOTE = 34;
var BACKSLASH = 92;
var PLUS = 43;
var MINUS = 45;
var STAR = 42;
var SLASH = 47;
var LT = 60;
var GT = 62;
var EQ = 61;
var BANG = 33;
var PIPE = 124;
var COMMA = 44;
var DOT = 46;
var LPAREN = 40;
var RPAREN = 41;
var LBRACE = 123;
var RBRACE = 125;
var COLON = 58;
var keywords = new Map([
  ["let", 5 /* Let */],
  ["rec", 6 /* Rec */],
  ["in", 7 /* In */],
  ["if", 8 /* If */],
  ["then", 9 /* Then */],
  ["else", 10 /* Else */],
  ["match", 11 /* Match */],
  ["with", 12 /* With */],
  ["end", 13 /* End */],
  ["data", 14 /* Data */],
  ["true", 15 /* True */],
  ["false", 16 /* False */]
]);
var isDigit = (ch) => ch >= DIGIT_0 && ch <= DIGIT_9;
var isLower = (ch) => ch >= LOWER_A && ch <= LOWER_Z;
var isUpper = (ch) => ch >= UPPER_A && ch <= UPPER_Z;
var isIdentContinue = (ch) => isLower(ch) || isUpper(ch) || isDigit(ch) || ch === UNDERSCORE;
var createLexer = (source) => ({
  source,
  pos: 0,
  atLineStart: true
});
var peek = (state) => state.pos < state.source.length ? state.source.charCodeAt(state.pos) : EOF;
var peekAt = (state, offset) => {
  const idx = state.pos + offset;
  return idx < state.source.length ? state.source.charCodeAt(idx) : EOF;
};
var advance = (state) => {
  state.pos++;
};
var slice = (state, start, end) => state.source.slice(start, end);
var skipWhitespaceAndComments = (state) => {
  state.atLineStart = false;
  while (true) {
    const ch = peek(state);
    if (ch === SPACE || ch === TAB) {
      advance(state);
      continue;
    }
    if (ch === LF || ch === CR) {
      state.atLineStart = true;
      advance(state);
      continue;
    }
    if (ch === MINUS && peekAt(state, 1) === MINUS) {
      advance(state);
      advance(state);
      while (peek(state) !== LF && peek(state) !== EOF) {
        advance(state);
      }
      continue;
    }
    if (ch === LBRACE && peekAt(state, 1) === MINUS) {
      scanBlockComment(state);
      continue;
    }
    break;
  }
};
var scanBlockComment = (state) => {
  advance(state);
  advance(state);
  let depth = 1;
  while (depth > 0 && peek(state) !== EOF) {
    const ch = peek(state);
    if (ch === LBRACE && peekAt(state, 1) === MINUS) {
      advance(state);
      advance(state);
      depth++;
    } else if (ch === MINUS && peekAt(state, 1) === RBRACE) {
      advance(state);
      advance(state);
      depth--;
    } else {
      advance(state);
    }
  }
};
var scanNumber = (state, start) => {
  while (isDigit(peek(state))) {
    advance(state);
  }
  if (peek(state) === DOT && isDigit(peekAt(state, 1))) {
    advance(state);
    while (isDigit(peek(state))) {
      advance(state);
    }
  }
  return [1 /* Number */, start, state.pos];
};
var scanString = (state, start) => {
  advance(state);
  while (peek(state) !== DOUBLE_QUOTE && peek(state) !== EOF) {
    if (peek(state) === BACKSLASH) {
      advance(state);
    }
    advance(state);
  }
  if (peek(state) === EOF) {
    return [39 /* Error */, start, state.pos];
  }
  advance(state);
  return [2 /* String */, start, state.pos];
};
var scanLowerOrKeyword = (state, start) => {
  if (peek(state) === UNDERSCORE && !isIdentContinue(peekAt(state, 1))) {
    advance(state);
    return [33 /* Underscore */, start, state.pos];
  }
  while (isIdentContinue(peek(state))) {
    advance(state);
  }
  const text = state.source.slice(start, state.pos);
  const keyword = keywords.get(text);
  if (keyword !== undefined) {
    return [keyword, start, state.pos];
  }
  return [3 /* Lower */, start, state.pos];
};
var scanUpper = (state, start) => {
  while (isIdentContinue(peek(state))) {
    advance(state);
  }
  return [4 /* Upper */, start, state.pos];
};
var scanOperator = (state, start, ch) => {
  advance(state);
  switch (ch) {
    case PLUS:
      return [17 /* Plus */, start, state.pos];
    case MINUS:
      return [18 /* Minus */, start, state.pos];
    case STAR:
      return [19 /* Star */, start, state.pos];
    case SLASH:
      return [20 /* Slash */, start, state.pos];
    case LT:
      if (peek(state) === EQ) {
        advance(state);
        return [22 /* Le */, start, state.pos];
      }
      return [21 /* Lt */, start, state.pos];
    case GT:
      if (peek(state) === EQ) {
        advance(state);
        return [24 /* Ge */, start, state.pos];
      }
      return [23 /* Gt */, start, state.pos];
    case EQ:
      if (peek(state) === EQ) {
        advance(state);
        return [25 /* EqEq */, start, state.pos];
      }
      if (peek(state) === GT) {
        advance(state);
        return [28 /* Arrow */, start, state.pos];
      }
      return [29 /* Eq */, start, state.pos];
    case BANG:
      if (peek(state) === EQ) {
        advance(state);
        return [26 /* Ne */, start, state.pos];
      }
      return [39 /* Error */, start, state.pos];
    case PIPE:
      if (peek(state) === GT) {
        advance(state);
        return [27 /* Pipe */, start, state.pos];
      }
      return [30 /* Bar */, start, state.pos];
    case COMMA:
      return [31 /* Comma */, start, state.pos];
    case DOT:
      return [32 /* Dot */, start, state.pos];
    case LPAREN:
      return [35 /* LParen */, start, state.pos];
    case RPAREN:
      return [36 /* RParen */, start, state.pos];
    case LBRACE:
      return [37 /* LBrace */, start, state.pos];
    case RBRACE:
      return [38 /* RBrace */, start, state.pos];
    case COLON:
      if (peek(state) === COLON) {
        advance(state);
        return [34 /* ColonColon */, start, state.pos];
      }
      return [39 /* Error */, start, state.pos];
    default:
      return [39 /* Error */, start, state.pos];
  }
};
var nextToken = (state) => {
  skipWhitespaceAndComments(state);
  const start = state.pos;
  const ch = peek(state);
  if (ch === EOF) {
    return [0 /* Eof */, start, start];
  }
  if (isDigit(ch)) {
    return scanNumber(state, start);
  }
  if (ch === DOUBLE_QUOTE) {
    return scanString(state, start);
  }
  if (isLower(ch) || ch === UNDERSCORE) {
    return scanLowerOrKeyword(state, start);
  }
  if (isUpper(ch)) {
    return scanUpper(state, start);
  }
  return scanOperator(state, start, ch);
};

// src/parser.ts
var createParser = (source) => {
  const lexer = createLexer(source);
  return {
    lexer,
    diagnostics: [],
    current: nextToken(lexer)
  };
};
var at = (state, kind) => state.current[0] === kind;
var atAny = (state, ...kinds) => kinds.includes(state.current[0]);
var advance2 = (state) => {
  const prev = state.current;
  state.current = nextToken(state.lexer);
  return prev;
};
var expect = (state, kind, message) => {
  if (at(state, kind)) {
    return advance2(state);
  }
  error2(state, message);
  return null;
};
var text = (state, token) => slice(state.lexer, token[1], token[2]);
var error2 = (state, message) => {
  state.diagnostics.push({
    start: state.current[1],
    end: state.current[2],
    message,
    severity: "error"
  });
};
var span = (start, end) => ({ start, end });
var tokenSpan = (token) => span(token[1], token[2]);
var atNewStatement = (state) => state.lexer.atLineStart && !at(state, 30 /* Bar */);
var synchronize = (state) => {
  while (!at(state, 0 /* Eof */)) {
    if (atAny(state, 5 /* Let */, 14 /* Data */, 8 /* If */, 11 /* Match */, 13 /* End */)) {
      return;
    }
    advance2(state);
  }
};
var parse = (source) => {
  const state = createParser(source);
  const declarations = [];
  const bindings = [];
  let expr = null;
  while (!at(state, 0 /* Eof */)) {
    if (at(state, 14 /* Data */)) {
      const decl = parseDataDecl(state);
      if (decl)
        declarations.push(decl);
    } else if (at(state, 5 /* Let */)) {
      const result = parseLetBindingOrExpr(state);
      if (result.kind === "binding") {
        bindings.push(result.binding);
      } else {
        expr = result.expr;
        break;
      }
    } else {
      expr = parseExpr(state);
      break;
    }
  }
  return {
    program: { declarations, bindings, expr },
    diagnostics: state.diagnostics
  };
};
var parseLetBindingOrExpr = (state) => {
  advance2(state);
  const recursive = at(state, 6 /* Rec */);
  if (recursive)
    advance2(state);
  const nameToken = expect(state, 3 /* Lower */, "expected binding name");
  if (!nameToken) {
    synchronize(state);
    return { kind: "expr", expr: num(0) };
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = [];
  while (at(state, 3 /* Lower */)) {
    const paramToken = advance2(state);
    params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
  }
  if (!expect(state, 29 /* Eq */, "expected '=' after parameters")) {
    synchronize(state);
    return { kind: "expr", expr: num(0) };
  }
  const body = parseExpr(state);
  if (at(state, 7 /* In */)) {
    advance2(state);
    let value = body;
    for (let i = params.length - 1;i >= 0; i--) {
      const p = params[i];
      value = abs(p.name, value, undefined, p.span);
    }
    const continuation = parseExpr(state);
    const expr = recursive ? letRec(name, value, continuation, undefined, nameSpan) : let_(name, value, continuation, undefined, nameSpan);
    return { kind: "expr", expr };
  }
  return { kind: "binding", binding: { name, nameSpan, params, body, recursive } };
};
var parseDataDecl = (state) => {
  advance2(state);
  const nameToken = expect(state, 4 /* Upper */, "expected type name");
  if (!nameToken) {
    synchronize(state);
    return null;
  }
  const name = text(state, nameToken);
  const typeParams = [];
  while (at(state, 3 /* Lower */)) {
    typeParams.push(text(state, advance2(state)));
  }
  if (!expect(state, 29 /* Eq */, "expected '=' after type parameters")) {
    synchronize(state);
    return null;
  }
  const constructors = [];
  const first = parseConstructor(state);
  if (first)
    constructors.push(first);
  while (at(state, 30 /* Bar */)) {
    advance2(state);
    const con = parseConstructor(state);
    if (con)
      constructors.push(con);
  }
  return dataDecl(name, typeParams, constructors);
};
var parseConstructor = (state) => {
  const nameToken = expect(state, 4 /* Upper */, "expected constructor name");
  if (!nameToken)
    return null;
  const name = text(state, nameToken);
  const fields = [];
  while (atAny(state, 3 /* Lower */, 4 /* Upper */, 35 /* LParen */) && !atNewStatement(state)) {
    const field2 = parseTypeAtom(state);
    if (field2)
      fields.push(field2);
    else
      break;
  }
  return conDecl(name, fields);
};
var parseTypeAtom = (state) => {
  if (at(state, 3 /* Lower */)) {
    return tyvar(text(state, advance2(state)));
  }
  if (at(state, 4 /* Upper */)) {
    let type = tycon(text(state, advance2(state)));
    while (atAny(state, 3 /* Lower */, 4 /* Upper */, 35 /* LParen */) && !atNewStatement(state)) {
      const arg = parseTypeAtomSimple(state);
      if (arg) {
        type = tyapp(type, arg);
      } else {
        break;
      }
    }
    return type;
  }
  if (at(state, 35 /* LParen */)) {
    advance2(state);
    const inner = parseTypeAtom(state);
    expect(state, 36 /* RParen */, "expected ')' after type");
    return inner;
  }
  return null;
};
var parseTypeAtomSimple = (state) => {
  if (at(state, 3 /* Lower */)) {
    return tyvar(text(state, advance2(state)));
  }
  if (at(state, 4 /* Upper */)) {
    return tycon(text(state, advance2(state)));
  }
  if (at(state, 35 /* LParen */)) {
    advance2(state);
    const inner = parseTypeAtom(state);
    expect(state, 36 /* RParen */, "expected ')' after type");
    return inner;
  }
  return null;
};
var parseExpr = (state) => parsePrecedence(state, 0 /* None */);
var parsePrecedence = (state, minBp) => {
  let left = parsePrefix(state);
  while (true) {
    const bp = infixBindingPower(state);
    if (bp <= minBp)
      break;
    left = parseInfix(state, left, bp);
  }
  return left;
};
var parsePrefix = (state) => {
  const token = state.current;
  const kind = token[0];
  const start = token[1];
  switch (kind) {
    case 1 /* Number */: {
      advance2(state);
      return num(parseFloat(text(state, token)), tokenSpan(token));
    }
    case 2 /* String */: {
      advance2(state);
      return str(parseStringContent(text(state, token)), tokenSpan(token));
    }
    case 15 /* True */:
      advance2(state);
      return bool(true, tokenSpan(token));
    case 16 /* False */:
      advance2(state);
      return bool(false, tokenSpan(token));
    case 3 /* Lower */: {
      advance2(state);
      const name = text(state, token);
      const paramSpan = tokenSpan(token);
      if (at(state, 28 /* Arrow */)) {
        advance2(state);
        const body = parseExpr(state);
        const end = body.span?.end ?? state.current[1];
        return abs(name, body, span(start, end), paramSpan);
      }
      return var_(name, tokenSpan(token));
    }
    case 4 /* Upper */: {
      advance2(state);
      return var_(text(state, token), tokenSpan(token));
    }
    case 35 /* LParen */:
      return parseParenOrTuple(state);
    case 37 /* LBrace */:
      return parseRecord(state);
    case 8 /* If */:
      return parseIf(state);
    case 11 /* Match */:
      return parseMatch(state);
    case 5 /* Let */:
      return parseLetExpr(state);
    default: {
      error2(state, `unexpected token: ${TokenKind[kind]}`);
      advance2(state);
      return num(0);
    }
  }
};
var parseInfix = (state, left, bp) => {
  const kind = state.current[0];
  const start = left.span?.start ?? 0;
  const binOp2 = (op) => {
    advance2(state);
    const right = parsePrecedence(state, bp);
    const end = right.span?.end ?? state.current[1];
    return binOp(op, left, right, span(start, end));
  };
  switch (kind) {
    case 17 /* Plus */:
      return binOp2("+");
    case 18 /* Minus */:
      return binOp2("-");
    case 19 /* Star */:
      return binOp2("*");
    case 20 /* Slash */:
      return binOp2("/");
    case 21 /* Lt */:
      return binOp2("<");
    case 22 /* Le */:
      return binOp2("<=");
    case 23 /* Gt */:
      return binOp2(">");
    case 24 /* Ge */:
      return binOp2(">=");
    case 25 /* EqEq */:
      return binOp2("==");
    case 26 /* Ne */:
      return binOp2("!=");
    case 27 /* Pipe */: {
      advance2(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return app(right, left, span(start, end));
    }
    case 34 /* ColonColon */: {
      advance2(state);
      const right = parsePrecedence(state, bp - 1);
      const end = right.span?.end ?? state.current[1];
      return app(app(var_("Cons"), left), right, span(start, end));
    }
    case 32 /* Dot */: {
      advance2(state);
      const fieldToken = expect(state, 3 /* Lower */, "expected field name after '.'");
      const field2 = fieldToken ? text(state, fieldToken) : "?";
      const end = fieldToken ? fieldToken[2] : state.current[1];
      return fieldAccess(left, field2, span(start, end));
    }
    default: {
      const right = parsePrefix(state);
      const end = right.span?.end ?? state.current[1];
      return app(left, right, span(start, end));
    }
  }
};
var infixBindingPower = (state) => {
  const kind = state.current[0];
  switch (kind) {
    case 27 /* Pipe */:
      return 10 /* Pipe */;
    case 34 /* ColonColon */:
      return 15 /* Cons */;
    case 25 /* EqEq */:
    case 26 /* Ne */:
      return 20 /* Equality */;
    case 21 /* Lt */:
    case 22 /* Le */:
    case 23 /* Gt */:
    case 24 /* Ge */:
      return 30 /* Comparison */;
    case 17 /* Plus */:
    case 18 /* Minus */:
      return 40 /* Additive */;
    case 19 /* Star */:
    case 20 /* Slash */:
      return 50 /* Multiplicative */;
    case 32 /* Dot */:
      return 70 /* FieldAccess */;
    case 3 /* Lower */:
    case 4 /* Upper */:
    case 1 /* Number */:
    case 2 /* String */:
    case 15 /* True */:
    case 16 /* False */:
    case 35 /* LParen */:
    case 37 /* LBrace */:
      if (state.lexer.atLineStart) {
        return 0 /* None */;
      }
      return 60 /* Application */;
    default:
      return 0 /* None */;
  }
};
var parseParenOrTuple = (state) => {
  const start = state.current[1];
  advance2(state);
  if (at(state, 36 /* RParen */)) {
    advance2(state);
    error2(state, "empty parentheses");
    return num(0);
  }
  const first = parseExpr(state);
  if (at(state, 31 /* Comma */)) {
    const elements = [first];
    while (at(state, 31 /* Comma */)) {
      advance2(state);
      elements.push(parseExpr(state));
    }
    const endToken = expect(state, 36 /* RParen */, "expected ')' after tuple");
    const end = endToken ? endToken[2] : state.current[1];
    return tuple(elements, span(start, end));
  }
  expect(state, 36 /* RParen */, "expected ')' after expression");
  return first;
};
var parseRecord = (state) => {
  const start = state.current[1];
  advance2(state);
  const fields = [];
  if (!at(state, 38 /* RBrace */)) {
    do {
      if (at(state, 31 /* Comma */))
        advance2(state);
      const nameToken = expect(state, 3 /* Lower */, "expected field name");
      if (!nameToken)
        break;
      const name = text(state, nameToken);
      const fieldStart = nameToken[1];
      expect(state, 29 /* Eq */, "expected '=' after field name");
      const value = parseExpr(state);
      const fieldEnd = value.span?.end ?? state.current[1];
      fields.push(field(name, value, span(fieldStart, fieldEnd)));
    } while (at(state, 31 /* Comma */));
  }
  const endToken = expect(state, 38 /* RBrace */, "expected '}' after record");
  const end = endToken ? endToken[2] : state.current[1];
  return record(fields, span(start, end));
};
var parseIf = (state) => {
  const start = state.current[1];
  advance2(state);
  const cond = parseExpr(state);
  expect(state, 9 /* Then */, "expected 'then' after condition");
  const thenBranch = parseExpr(state);
  expect(state, 10 /* Else */, "expected 'else' after 'then' branch");
  const elseBranch = parseExpr(state);
  const end = elseBranch.span?.end ?? state.current[1];
  return if_(cond, thenBranch, elseBranch, span(start, end));
};
var parseMatch = (state) => {
  const start = state.current[1];
  advance2(state);
  const scrutinee = parseExpr(state);
  expect(state, 12 /* With */, "expected 'with' after match expression");
  const cases = [];
  while (at(state, 30 /* Bar */)) {
    const caseStart = state.current[1];
    advance2(state);
    const pattern = parsePattern(state);
    expect(state, 28 /* Arrow */, "expected '=>' after pattern");
    const body = parseExpr(state);
    const caseEnd = body.span?.end ?? state.current[1];
    cases.push(case_(pattern, body, span(caseStart, caseEnd)));
  }
  const endToken = expect(state, 13 /* End */, "expected 'end' after match cases");
  const end = endToken ? endToken[2] : state.current[1];
  return match(scrutinee, cases, span(start, end));
};
var parseLetExpr = (state) => {
  const start = state.current[1];
  advance2(state);
  const recursive = at(state, 6 /* Rec */);
  if (recursive)
    advance2(state);
  const nameToken = expect(state, 3 /* Lower */, "expected binding name");
  if (!nameToken) {
    return num(0);
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = [];
  while (at(state, 3 /* Lower */)) {
    const paramToken = advance2(state);
    params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
  }
  expect(state, 29 /* Eq */, "expected '=' after name");
  let value = parseExpr(state);
  for (let i = params.length - 1;i >= 0; i--) {
    const p = params[i];
    value = abs(p.name, value, undefined, p.span);
  }
  expect(state, 7 /* In */, "expected 'in' after let value");
  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];
  return recursive ? letRec(name, value, body, span(start, end), nameSpan) : let_(name, value, body, span(start, end), nameSpan);
};
var parsePattern = (state) => {
  const token = state.current;
  const kind = token[0];
  const start = token[1];
  switch (kind) {
    case 33 /* Underscore */:
      advance2(state);
      return pwildcard(tokenSpan(token));
    case 3 /* Lower */:
      advance2(state);
      return pvar(text(state, token), tokenSpan(token));
    case 4 /* Upper */: {
      advance2(state);
      const name = text(state, token);
      const args = [];
      while (isPatternStart(state)) {
        args.push(parsePatternAtom(state));
      }
      const end = args.length > 0 ? args[args.length - 1].span?.end ?? state.current[1] : token[2];
      return pcon(name, args, span(start, end));
    }
    case 1 /* Number */: {
      advance2(state);
      return plit(parseFloat(text(state, token)), tokenSpan(token));
    }
    case 2 /* String */: {
      advance2(state);
      return plit(parseStringContent(text(state, token)), tokenSpan(token));
    }
    case 15 /* True */:
      advance2(state);
      return plit(true, tokenSpan(token));
    case 16 /* False */:
      advance2(state);
      return plit(false, tokenSpan(token));
    case 35 /* LParen */:
      return parseTuplePattern(state);
    case 37 /* LBrace */:
      return parseRecordPattern(state);
    default:
      error2(state, `unexpected token in pattern: ${TokenKind[kind]}`);
      advance2(state);
      return pwildcard();
  }
};
var parsePatternAtom = (state) => {
  const token = state.current;
  const kind = token[0];
  switch (kind) {
    case 33 /* Underscore */:
      advance2(state);
      return pwildcard(tokenSpan(token));
    case 3 /* Lower */:
      advance2(state);
      return pvar(text(state, token), tokenSpan(token));
    case 4 /* Upper */:
      advance2(state);
      return pcon(text(state, token), [], tokenSpan(token));
    case 1 /* Number */: {
      advance2(state);
      return plit(parseFloat(text(state, token)), tokenSpan(token));
    }
    case 2 /* String */: {
      advance2(state);
      return plit(parseStringContent(text(state, token)), tokenSpan(token));
    }
    case 15 /* True */:
      advance2(state);
      return plit(true, tokenSpan(token));
    case 16 /* False */:
      advance2(state);
      return plit(false, tokenSpan(token));
    case 35 /* LParen */:
      return parseTuplePattern(state);
    case 37 /* LBrace */:
      return parseRecordPattern(state);
    default:
      error2(state, `unexpected token in pattern: ${TokenKind[kind]}`);
      return pwildcard();
  }
};
var isPatternStart = (state) => atAny(state, 33 /* Underscore */, 3 /* Lower */, 4 /* Upper */, 1 /* Number */, 2 /* String */, 15 /* True */, 16 /* False */, 35 /* LParen */, 37 /* LBrace */);
var parseTuplePattern = (state) => {
  const start = state.current[1];
  advance2(state);
  if (at(state, 36 /* RParen */)) {
    advance2(state);
    error2(state, "empty pattern");
    return pwildcard();
  }
  const first = parsePattern(state);
  if (at(state, 31 /* Comma */)) {
    const elements = [first];
    while (at(state, 31 /* Comma */)) {
      advance2(state);
      elements.push(parsePattern(state));
    }
    const endToken = expect(state, 36 /* RParen */, "expected ')' after tuple pattern");
    const end = endToken ? endToken[2] : state.current[1];
    return ptuple(elements, span(start, end));
  }
  expect(state, 36 /* RParen */, "expected ')' after pattern");
  return first;
};
var parseRecordPattern = (state) => {
  const start = state.current[1];
  advance2(state);
  const fields = [];
  if (!at(state, 38 /* RBrace */)) {
    do {
      if (at(state, 31 /* Comma */))
        advance2(state);
      const nameToken = expect(state, 3 /* Lower */, "expected field name");
      if (!nameToken)
        break;
      const name = text(state, nameToken);
      const fieldStart = nameToken[1];
      expect(state, 29 /* Eq */, "expected '=' after field name");
      const pattern = parsePattern(state);
      const fieldEnd = pattern.span?.end ?? state.current[1];
      fields.push(pfield(name, pattern, span(fieldStart, fieldEnd)));
    } while (at(state, 31 /* Comma */));
  }
  const endToken = expect(state, 38 /* RBrace */, "expected '}' after record pattern");
  const end = endToken ? endToken[2] : state.current[1];
  return precord(fields, span(start, end));
};
var parseStringContent = (quoted) => {
  const inner = quoted.slice(1, -1);
  let result = "";
  let i = 0;
  while (i < inner.length) {
    if (inner[i] === "\\") {
      i++;
      if (i >= inner.length)
        break;
      switch (inner[i]) {
        case "n":
          result += `
`;
          break;
        case "t":
          result += "\t";
          break;
        case "r":
          result += "\r";
          break;
        case "\\":
          result += "\\";
          break;
        case '"':
          result += '"';
          break;
        default:
          result += inner[i];
      }
    } else {
      result += inner[i];
    }
    i++;
  }
  return result;
};
var programToExpr = (program) => {
  if (!program.expr && program.bindings.length === 0) {
    return null;
  }
  let expr = program.expr ?? num(0);
  for (let i = program.bindings.length - 1;i >= 0; i--) {
    const binding = program.bindings[i];
    let value = binding.body;
    for (let j = binding.params.length - 1;j >= 0; j--) {
      const p = binding.params[j];
      value = abs(p.name, value, undefined, p.span);
    }
    if (binding.recursive) {
      expr = letRec(binding.name, value, expr, undefined, binding.nameSpan);
    } else {
      expr = let_(binding.name, value, expr, undefined, binding.nameSpan);
    }
  }
  return expr;
};

// src/prelude.ts
var maybe = dataDecl("Maybe", ["a"], [conDecl("Nothing", []), conDecl("Just", [tyvar("a")])]);
var either = dataDecl("Either", ["a", "b"], [conDecl("Left", [tyvar("a")]), conDecl("Right", [tyvar("b")])]);
var list = dataDecl("List", ["a"], [
  conDecl("Nil", []),
  conDecl("Cons", [tyvar("a"), tyapp(tycon("List"), tyvar("a"))])
]);
var declarations = [maybe, either, list];
var map = letRec("map", abs("f", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("Nil")),
  case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(var_("Cons"), app(var_("f"), var_("x"))), app(app(var_("map"), var_("f")), var_("rest"))))
]))), var_("map"));
var filter = letRec("filter", abs("p", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("Nil")),
  case_(pcon("Cons", [pvar("x"), pvar("rest")]), if_(app(var_("p"), var_("x")), app(app(var_("Cons"), var_("x")), app(app(var_("filter"), var_("p")), var_("rest"))), app(app(var_("filter"), var_("p")), var_("rest"))))
]))), var_("filter"));
var head = abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("Nothing")),
  case_(pcon("Cons", [pvar("x"), pwildcard()]), app(var_("Just"), var_("x")))
]));
var tail = abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("Nothing")),
  case_(pcon("Cons", [pwildcard(), pvar("rest")]), app(var_("Just"), var_("rest")))
]));
var isEmpty = abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), bool(true)),
  case_(pcon("Cons", [pwildcard(), pwildcard()]), bool(false))
]));
var length = letRec("length", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), num(0)),
  case_(pcon("Cons", [pwildcard(), pvar("rest")]), binOp("+", num(1), app(var_("length"), var_("rest"))))
])), var_("length"));
var foldr = letRec("foldr", abs("f", abs("z", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("z")),
  case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(var_("f"), var_("x")), app(app(app(var_("foldr"), var_("f")), var_("z")), var_("rest"))))
])))), var_("foldr"));
var foldl = letRec("foldl", abs("f", abs("z", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("z")),
  case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(app(var_("foldl"), var_("f")), app(app(var_("f"), var_("z")), var_("x"))), var_("rest")))
])))), var_("foldl"));
var reverse = abs("xs", app(app(app(foldl, abs("acc", abs("x", app(app(var_("Cons"), var_("x")), var_("acc"))))), var_("Nil")), var_("xs")));
var concat = abs("xs", abs("ys", app(app(app(foldr, var_("Cons")), var_("ys")), var_("xs"))));
var id = abs("x", var_("x"));
var const_ = abs("x", abs("_", var_("x")));
var compose = abs("f", abs("g", abs("x", app(var_("f"), app(var_("g"), var_("x"))))));
var flip = abs("f", abs("a", abs("b", app(app(var_("f"), var_("b")), var_("a")))));

// src/lsp/positions.ts
var offsetToPosition = (source, offset) => {
  let line = 0;
  let character = 0;
  for (let i = 0;i < offset && i < source.length; i++) {
    if (source[i] === `
`) {
      line++;
      character = 0;
    } else {
      character++;
    }
  }
  return { line, character };
};
var positionToOffset = (source, position) => {
  let currentLine = 0;
  let offset = 0;
  while (currentLine < position.line && offset < source.length) {
    if (source[offset] === `
`) {
      currentLine++;
    }
    offset++;
  }
  return offset + position.character;
};
var spanToRange = (source, span2) => ({
  start: offsetToPosition(source, span2.start),
  end: offsetToPosition(source, span2.end)
});

// src/lsp/transport.ts
var isRequest = (msg) => ("id" in msg) && ("method" in msg);
var isNotification = (msg) => !("id" in msg) && ("method" in msg);
var successResponse = (id2, result) => ({
  jsonrpc: "2.0",
  id: id2,
  result
});
var errorResponse = (id2, code, message) => ({
  jsonrpc: "2.0",
  id: id2,
  error: { code, message }
});
var notification = (method, params) => ({
  jsonrpc: "2.0",
  method,
  params
});

// src/lsp/server.ts
var createServer = (transport) => {
  const documents = new Map;
  transport.onMessage((message) => {
    if (isRequest(message)) {
      handleRequest(message);
    } else if (isNotification(message)) {
      handleNotification(message);
    }
  });
  transport.onClose(() => {});
  const handleRequest = (request) => {
    const { id: id2, method, params } = request;
    try {
      switch (method) {
        case "initialize":
          transport.send(successResponse(id2, handleInitialize(params)));
          break;
        case "shutdown":
          transport.send(successResponse(id2, null));
          break;
        case "textDocument/hover":
          transport.send(successResponse(id2, handleHover(params)));
          break;
        case "textDocument/definition":
          transport.send(successResponse(id2, handleDefinition(params)));
          break;
        case "textDocument/prepareRename":
          transport.send(successResponse(id2, handlePrepareRename(params)));
          break;
        case "textDocument/rename":
          transport.send(successResponse(id2, handleRename(params)));
          break;
        case "algow/evaluate":
          transport.send(successResponse(id2, handleEvaluate(params)));
          break;
        default:
          transport.send(errorResponse(id2, -32601, `Method not found: ${method}`));
      }
    } catch (err) {
      transport.send(errorResponse(id2, -32603, err.message));
    }
  };
  const handleNotification = (msg) => {
    const { method, params } = msg;
    switch (method) {
      case "initialized":
        break;
      case "textDocument/didOpen":
        handleDidOpen(params);
        break;
      case "textDocument/didChange":
        handleDidChange(params);
        break;
      case "textDocument/didClose":
        handleDidClose(params);
        break;
      case "exit":
        process.exit(0);
        break;
    }
  };
  const handleInitialize = (_params) => ({
    capabilities: {
      textDocumentSync: 1,
      hoverProvider: true,
      definitionProvider: true,
      renameProvider: {
        prepareProvider: true
      }
    }
  });
  const handleDidOpen = (params) => {
    const { uri, version, text: text2 } = params.textDocument;
    const state = analyzeDocument(uri, version, text2);
    documents.set(uri, state);
    publishDiagnostics(uri, state.diagnostics);
  };
  const handleDidChange = (params) => {
    const { uri, version } = params.textDocument;
    const text2 = params.contentChanges[params.contentChanges.length - 1]?.text ?? "";
    const state = analyzeDocument(uri, version, text2);
    documents.set(uri, state);
    publishDiagnostics(uri, state.diagnostics);
  };
  const handleDidClose = (params) => {
    const { uri } = params.textDocument;
    documents.delete(uri);
    publishDiagnostics(uri, []);
  };
  const handleHover = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.inferResult)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const { symbols } = doc.inferResult;
    const def = findDefinitionAt(symbols, offset);
    if (def && def.type) {
      return {
        contents: {
          kind: "markdown",
          value: `\`\`\`algow
${def.name}: ${typeToString(def.type)}
\`\`\``
        },
        range: spanToRange(doc.text, def.span)
      };
    }
    const ref = findReferenceAt(symbols, offset);
    if (ref && ref.definition && ref.definition.type) {
      return {
        contents: {
          kind: "markdown",
          value: `\`\`\`algow
${ref.name}: ${typeToString(ref.definition.type)}
\`\`\``
        },
        range: spanToRange(doc.text, ref.span)
      };
    }
    return null;
  };
  const handleDefinition = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.inferResult)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const def = goToDefinition(doc.inferResult.symbols, offset);
    if (!def)
      return null;
    return {
      uri: params.textDocument.uri,
      range: spanToRange(doc.text, def.span)
    };
  };
  const handlePrepareRename = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.inferResult)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const { definition } = findAllOccurrences(doc.inferResult.symbols, offset);
    if (!definition)
      return null;
    const ref = findReferenceAt(doc.inferResult.symbols, offset);
    if (ref) {
      return spanToRange(doc.text, ref.span);
    }
    const def = findDefinitionAt(doc.inferResult.symbols, offset);
    if (def) {
      return spanToRange(doc.text, def.span);
    }
    return null;
  };
  const handleRename = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.inferResult)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const { definition, references } = findAllOccurrences(doc.inferResult.symbols, offset);
    if (!definition)
      return null;
    const edits = [];
    edits.push({
      range: spanToRange(doc.text, definition.span),
      newText: params.newName
    });
    for (const ref of references) {
      edits.push({
        range: spanToRange(doc.text, ref.span),
        newText: params.newName
      });
    }
    return {
      changes: {
        [params.textDocument.uri]: edits
      }
    };
  };
  const handleEvaluate = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }
    if (doc.diagnostics.some((d) => d.severity === 1)) {
      return { success: false, error: "Cannot evaluate: document has errors" };
    }
    const expr = programToExpr(doc.program);
    if (!expr) {
      return { success: false, error: "No expression to evaluate" };
    }
    try {
      const constructorNames = [];
      for (const decl of declarations) {
        for (const con of decl.constructors) {
          constructorNames.push(con.name);
        }
      }
      for (const decl of doc.program.declarations) {
        for (const con of decl.constructors) {
          constructorNames.push(con.name);
        }
      }
      const constructorEnv = createConstructorEnv(constructorNames);
      const result = evaluate(constructorEnv, expr);
      return { success: true, value: valueToString(result) };
    } catch (err) {
      if (err instanceof RuntimeError) {
        return { success: false, error: err.message };
      }
      return { success: false, error: err.message };
    }
  };
  const analyzeDocument = (uri, version, text2) => {
    const lspDiagnostics = [];
    const parseResult = parse(text2);
    for (const diag of parseResult.diagnostics) {
      lspDiagnostics.push(convertDiagnostic(text2, diag));
    }
    const expr = programToExpr(parseResult.program);
    let inferResult = null;
    let typeEnv = new Map;
    let registry = new Map;
    for (const decl of declarations) {
      const [newEnv, newReg] = processDataDecl(decl);
      typeEnv = mergeEnvs(typeEnv, newEnv);
      registry = mergeRegistries(registry, newReg);
    }
    for (const decl of parseResult.program.declarations) {
      const [newEnv, newReg] = processDataDecl(decl);
      typeEnv = mergeEnvs(typeEnv, newEnv);
      registry = mergeRegistries(registry, newReg);
    }
    if (expr) {
      inferResult = infer(typeEnv, registry, expr);
      for (const diag of inferResult.diagnostics) {
        lspDiagnostics.push(convertDiagnostic(text2, diag));
      }
    }
    return {
      uri,
      version,
      text: text2,
      diagnostics: lspDiagnostics,
      inferResult,
      program: parseResult.program,
      registry
    };
  };
  const convertDiagnostic = (source, diag) => {
    const severity = diag.severity === "error" ? 1 : diag.severity === "warning" ? 2 : 3;
    return {
      range: spanToRange(source, { start: diag.start, end: diag.end }),
      message: diag.message,
      severity,
      source: "algow"
    };
  };
  const publishDiagnostics = (uri, diagnostics) => {
    transport.send(notification("textDocument/publishDiagnostics", { uri, diagnostics }));
  };
};

// src/lsp/worker.ts
var createWorkerTransport = () => {
  let messageHandler = null;
  let _closeHandler = null;
  self.onmessage = (event) => {
    messageHandler?.(event.data);
  };
  return {
    send(message) {
      self.postMessage(message);
    },
    onMessage(handler) {
      messageHandler = handler;
    },
    onClose(handler) {
      _closeHandler = handler;
    }
  };
};

// playground/worker.ts
var transport = createWorkerTransport();
createServer(transport);
