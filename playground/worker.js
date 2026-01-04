// src/diagnostics.ts
var error = (start, end, message) => ({
  start,
  end,
  message,
  severity: "error"
});
var typeMismatch = (start, end, expected, actual, context) => ({
  start,
  end,
  message: context ? `Type mismatch in ${context}` : "Type mismatch",
  severity: "error",
  kind: "type-mismatch",
  expected,
  actual
});
var unboundVariable = (start, end, name, suggestions) => ({
  start,
  end,
  message: `Unknown variable: ${name}`,
  severity: "error",
  kind: "unbound-variable",
  suggestions
});
var levenshteinDistance = (a, b) => {
  if (a.length === 0)
    return b.length;
  if (b.length === 0)
    return a.length;
  const matrix = [];
  for (let i = 0;i <= b.length; i++) {
    matrix[i] = [i];
  }
  for (let j = 0;j <= a.length; j++) {
    matrix[0][j] = j;
  }
  for (let i = 1;i <= b.length; i++) {
    for (let j = 1;j <= a.length; j++) {
      const cost = a[j - 1] === b[i - 1] ? 0 : 1;
      matrix[i][j] = Math.min(matrix[i - 1][j] + 1, matrix[i][j - 1] + 1, matrix[i - 1][j - 1] + cost);
    }
  }
  return matrix[b.length][a.length];
};
var findSimilarNames = (name, candidates, maxDistance = 2, maxSuggestions = 3) => {
  const scored = [];
  for (const candidate of candidates) {
    if (candidate === name)
      continue;
    const dist = levenshteinDistance(name.toLowerCase(), candidate.toLowerCase());
    if (dist <= maxDistance) {
      scored.push([candidate, dist]);
    }
  }
  return scored.sort((a, b) => a[1] - b[1]).slice(0, maxSuggestions).map(([s]) => s);
};

// src/binder.ts
var createContext = () => ({
  definitions: [],
  references: [],
  diagnostics: [],
  scope: new Map
});
var addDefinition = (ctx, name, span, kind) => {
  const def = { name, span, kind };
  ctx.definitions.push(def);
  const stack = ctx.scope.get(name);
  if (stack) {
    stack.push(def);
  } else {
    ctx.scope.set(name, [def]);
  }
  return def;
};
var addReference = (ctx, name, span) => {
  const stack = ctx.scope.get(name);
  const definition = stack && stack.length > 0 ? stack[stack.length - 1] : null;
  const ref = { name, span, definition };
  ctx.references.push(ref);
  if (!definition) {
    const suggestions = findSimilarNames(name, ctx.scope.keys());
    ctx.diagnostics.push(unboundVariable(span.start, span.end, name, suggestions.length > 0 ? suggestions : undefined));
  }
  return ref;
};
var popScope = (ctx, name) => {
  const stack = ctx.scope.get(name);
  if (stack && stack.length > 0) {
    stack.pop();
  }
};
var finalize = (ctx) => ({
  definitions: ctx.definitions,
  references: ctx.references
});
var bindWithConstructors = (constructorNames, expr) => {
  const ctx = createContext();
  for (const name of constructorNames) {
    addDefinition(ctx, name, { start: 0, end: 0 }, "constructor");
  }
  bindExpr(ctx, expr);
  return {
    symbols: finalize(ctx),
    diagnostics: ctx.diagnostics
  };
};
var bindExpr = (ctx, expr) => {
  switch (expr.kind) {
    case "Num":
    case "Bool":
    case "Str":
      break;
    case "Var":
      bindVar(ctx, expr);
      break;
    case "Let":
      bindLet(ctx, expr);
      break;
    case "LetRec":
      bindLetRec(ctx, expr);
      break;
    case "Abs":
      bindAbs(ctx, expr);
      break;
    case "App":
      bindExpr(ctx, expr.func);
      bindExpr(ctx, expr.param);
      break;
    case "If":
      bindExpr(ctx, expr.cond);
      bindExpr(ctx, expr.then);
      bindExpr(ctx, expr.else);
      break;
    case "BinOp":
      bindExpr(ctx, expr.left);
      bindExpr(ctx, expr.right);
      break;
    case "Tuple":
      for (const element of expr.elements) {
        bindExpr(ctx, element);
      }
      break;
    case "Record":
      for (const field of expr.fields) {
        bindExpr(ctx, field.value);
      }
      break;
    case "FieldAccess":
      bindExpr(ctx, expr.record);
      break;
    case "Match":
      bindMatch(ctx, expr);
      break;
  }
};
var bindVar = (ctx, expr) => {
  const span = expr.span ?? { start: 0, end: 0 };
  addReference(ctx, expr.name, span);
};
var bindLet = (ctx, expr) => {
  bindExpr(ctx, expr.value);
  const nameSpan = expr.nameSpan ?? expr.span ?? { start: 0, end: 0 };
  addDefinition(ctx, expr.name, nameSpan, "variable");
  bindExpr(ctx, expr.body);
  popScope(ctx, expr.name);
};
var bindLetRec = (ctx, expr) => {
  for (const binding of expr.bindings) {
    const nameSpan = binding.nameSpan ?? expr.span ?? { start: 0, end: 0 };
    addDefinition(ctx, binding.name, nameSpan, "variable");
  }
  for (const binding of expr.bindings) {
    bindExpr(ctx, binding.value);
  }
  bindExpr(ctx, expr.body);
  for (let i = expr.bindings.length - 1;i >= 0; i--) {
    popScope(ctx, expr.bindings[i].name);
  }
};
var bindAbs = (ctx, expr) => {
  const paramSpan = expr.paramSpan ?? expr.span ?? { start: 0, end: 0 };
  addDefinition(ctx, expr.param, paramSpan, "parameter");
  bindExpr(ctx, expr.body);
  popScope(ctx, expr.param);
};
var bindMatch = (ctx, expr) => {
  bindExpr(ctx, expr.expr);
  for (const case_ of expr.cases) {
    const bindings = bindPattern(ctx, case_.pattern);
    bindExpr(ctx, case_.body);
    for (const name of bindings) {
      popScope(ctx, name);
    }
  }
};
var bindPattern = (ctx, pattern) => {
  switch (pattern.kind) {
    case "PVar": {
      const span = pattern.span ?? { start: 0, end: 0 };
      addDefinition(ctx, pattern.name, span, "parameter");
      return [pattern.name];
    }
    case "PWildcard":
      return [];
    case "PCon": {
      const conSpan = pattern.nameSpan ?? (pattern.span ? {
        start: pattern.span.start,
        end: pattern.span.start + pattern.name.length
      } : null);
      if (conSpan) {
        addReference(ctx, pattern.name, conSpan);
      }
      const bindings = [];
      for (const arg of pattern.args) {
        bindings.push(...bindPattern(ctx, arg));
      }
      return bindings;
    }
    case "PLit":
      return [];
    case "PTuple": {
      const bindings = [];
      for (const element of pattern.elements) {
        bindings.push(...bindPattern(ctx, element));
      }
      return bindings;
    }
    case "PRecord": {
      const bindings = [];
      for (const field of pattern.fields) {
        bindings.push(...bindPattern(ctx, field.pattern));
      }
      return bindings;
    }
    case "PAs": {
      const span = pattern.nameSpan ?? pattern.span ?? { start: 0, end: 0 };
      addDefinition(ctx, pattern.name, span, "parameter");
      const innerBindings = bindPattern(ctx, pattern.pattern);
      return [pattern.name, ...innerBindings];
    }
    case "POr": {
      if (pattern.alternatives.length > 0) {
        return bindPattern(ctx, pattern.alternatives[0]);
      }
      return [];
    }
  }
};
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

// src/checker.ts
var createContext2 = (symbols) => ({
  diagnostics: [],
  types: new Map,
  symbols
});
var addError = (ctx, message, span) => {
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(error(start, end, message));
};
var recordType = (ctx, span, type) => {
  for (const def of ctx.symbols.definitions) {
    if (def.span.start === span.start && def.span.end === span.end) {
      ctx.types.set(def, type);
      return;
    }
  }
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
  className: c.className,
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
    return bindVar2(ctx, t1.name, t2);
  }
  if (t2.kind === "TVar") {
    return bindVar2(ctx, t2.name, t1);
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
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(typeMismatch(start, end, typeToString(t1), typeToString(t2)));
  return new Map;
};
var bindVar2 = (ctx, name, type) => {
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
      const classInstances = instances.get(c.className);
      if (!classInstances?.has(c.type.name)) {
        addError(ctx, `Type '${c.type.name}' does not satisfy ${c.className}`);
      }
    }
    if (c.type.kind === "TFun") {
      addError(ctx, `Function types do not satisfy ${c.className}`);
    }
  }
};
var check = (env, registry, expr, symbols) => {
  typeVarCounter = 0;
  const ctx = createContext2(symbols);
  const [subst, type, constraints] = inferExpr(ctx, env, registry, expr);
  const finalConstraints = applySubstConstraints(subst, constraints);
  solveConstraints(ctx, finalConstraints);
  return {
    subst,
    type,
    constraints: finalConstraints,
    diagnostics: ctx.diagnostics,
    types: ctx.types
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
    case "TupleIndex":
      return inferTupleIndex(ctx, env, registry, expr);
    case "Record":
      return inferRecord(ctx, env, registry, expr);
  }
};
var inferFieldAccess = (ctx, env, registry, expr) => {
  const [s1, recordType2, constraints] = inferExpr(ctx, env, registry, expr.record);
  const resolvedType = applySubst(s1, recordType2);
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
var inferTupleIndex = (ctx, env, registry, expr) => {
  const [s1, tupleType, constraints] = inferExpr(ctx, env, registry, expr.tuple);
  const resolvedType = applySubst(s1, tupleType);
  if (resolvedType.kind === "TVar") {
    const elementType = freshTypeVar();
    return [s1, elementType, constraints];
  }
  if (resolvedType.kind !== "TTuple") {
    addError(ctx, `Cannot index into non-tuple type: ${typeToString(resolvedType)}`);
    return [s1, freshTypeVar(), constraints];
  }
  if (expr.index < 0 || expr.index >= resolvedType.elements.length) {
    addError(ctx, `Tuple index ${expr.index} out of bounds for tuple of ${resolvedType.elements.length} element(s)`);
    return [s1, freshTypeVar(), constraints];
  }
  return [s1, resolvedType.elements[expr.index], constraints];
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
  let paramType;
  let subst = new Map;
  if (expr.paramType) {
    const [annotatedType, tvSubst] = instantiateTypeExpr(expr.paramType);
    paramType = annotatedType;
    subst = tvSubst;
  } else {
    paramType = freshTypeVar();
  }
  const newEnv = new Map(env);
  newEnv.set(expr.param, scheme([], paramType));
  const [bodySubst, bodyType, constraints] = inferExpr(ctx, newEnv, registry, expr.body);
  subst = composeSubst(subst, bodySubst);
  const paramSpan = expr.paramSpan ?? expr.span;
  if (paramSpan) {
    recordType(ctx, paramSpan, applySubst(subst, paramType));
  }
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
      constraints.push({ className: "Add", type: operandType });
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
      constraints.push({ className: "Ord", type: operandType });
      return [subst, tBool, constraints];
    }
    case "==":
    case "!=": {
      constraints.push({ className: "Eq", type: operandType });
      return [subst, tBool, constraints];
    }
    case "++": {
      const s4 = unify(ctx, operandType, tStr, expr.span);
      return [composeSubst(subst, s4), tStr, constraints];
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
  let [s1, valueType, c1] = inferExpr(ctx, env, registry, expr.value);
  if (expr.returnType) {
    const [annotatedType] = instantiateTypeExpr(expr.returnType);
    let currentType = applySubst(s1, valueType);
    while (currentType.kind === "TFun") {
      currentType = currentType.ret;
    }
    const s3 = unify(ctx, currentType, annotatedType, expr.nameSpan ?? expr.span);
    s1 = composeSubst(s1, s3);
    valueType = applySubst(s3, valueType);
  }
  const env1 = applySubstEnv(s1, env);
  const generalizedScheme = generalize(env1, applySubst(s1, valueType));
  const nameSpan = expr.nameSpan ?? expr.span;
  if (nameSpan) {
    recordType(ctx, nameSpan, applySubst(s1, valueType));
  }
  const env2 = new Map(env1);
  env2.set(expr.name, generalizedScheme);
  const [s2, bodyType, c2] = inferExpr(ctx, env2, registry, expr.body);
  const subst = composeSubst(s1, s2);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);
  return [subst, bodyType, constraints];
};
var inferLetRec = (ctx, env, registry, expr) => {
  const placeholders = new Map;
  const envWithPlaceholders = new Map(env);
  for (const binding of expr.bindings) {
    const placeholder = freshTypeVar();
    placeholders.set(binding.name, placeholder);
    envWithPlaceholders.set(binding.name, scheme([], placeholder));
  }
  let subst = new Map;
  const valueTypes = new Map;
  let constraints = [];
  for (const binding of expr.bindings) {
    let [s, valueType, c] = inferExpr(ctx, applySubstEnv(subst, envWithPlaceholders), registry, binding.value);
    subst = composeSubst(subst, s);
    if (binding.returnType) {
      const [annotatedType] = instantiateTypeExpr(binding.returnType);
      let currentType = applySubst(subst, valueType);
      while (currentType.kind === "TFun") {
        currentType = currentType.ret;
      }
      const s3 = unify(ctx, currentType, annotatedType, binding.nameSpan);
      subst = composeSubst(subst, s3);
      valueType = applySubst(s3, valueType);
    }
    valueTypes.set(binding.name, valueType);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    const placeholder = applySubst(subst, placeholders.get(binding.name));
    const s22 = unify(ctx, placeholder, valueType, binding.value.span);
    subst = composeSubst(subst, s22);
  }
  const env1 = applySubstEnv(subst, env);
  const env2 = new Map(env1);
  for (const binding of expr.bindings) {
    const valueType = applySubst(subst, valueTypes.get(binding.name));
    const generalizedScheme = generalize(env1, valueType);
    env2.set(binding.name, generalizedScheme);
    const nameSpan = binding.nameSpan ?? expr.span;
    if (nameSpan) {
      recordType(ctx, nameSpan, valueType);
    }
  }
  const [s2, bodyType, c2] = inferExpr(ctx, env2, registry, expr.body);
  const finalSubst = composeSubst(subst, s2);
  const finalConstraints = applySubstConstraints(finalSubst, [...constraints, ...c2]);
  return [finalSubst, bodyType, finalConstraints];
};
var inferVar = (ctx, env, expr) => {
  const s = env.get(expr.name);
  if (!s) {
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
        recordType(ctx, pattern.span, boundType);
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
        const recordType2 = trecord([...fieldTypes.entries()], rowVar);
        const s = unify(ctx, applySubst(currentSubst2, expectedType), recordType2);
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
    case "PAs": {
      const [s, innerBindings] = inferPattern(ctx, env, pattern.pattern, expectedType, subst);
      const boundType = applySubst(s, expectedType);
      if (pattern.nameSpan) {
        recordType(ctx, pattern.nameSpan, boundType);
      }
      const allBindings = new Map(innerBindings);
      allBindings.set(pattern.name, boundType);
      return [s, allBindings];
    }
    case "POr": {
      if (pattern.alternatives.length === 0) {
        addError(ctx, "Or-pattern must have at least one alternative");
        return [subst, new Map];
      }
      const [s1, firstBindings] = inferPattern(ctx, env, pattern.alternatives[0], expectedType, subst);
      let currentSubst = s1;
      for (let i = 1;i < pattern.alternatives.length; i++) {
        const [s2, altBindings] = inferPattern(ctx, env, pattern.alternatives[i], applySubst(currentSubst, expectedType), currentSubst);
        currentSubst = s2;
        if (firstBindings.size !== altBindings.size) {
          addError(ctx, `Or-pattern alternatives must bind the same variables (alternative ${i + 1} binds different variables)`);
          continue;
        }
        for (const [name, type1] of firstBindings) {
          const type2 = altBindings.get(name);
          if (!type2) {
            addError(ctx, `Or-pattern alternative ${i + 1} is missing binding for '${name}'`);
          } else {
            const s3 = unify(ctx, applySubst(currentSubst, type1), applySubst(currentSubst, type2));
            currentSubst = composeSubst(currentSubst, s3);
          }
        }
      }
      return [currentSubst, firstBindings];
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
    if (case_.guard) {
      const [sg, guardType, cg] = inferExpr(ctx, caseEnv, registry, case_.guard);
      subst = composeSubst(subst, sg);
      constraints = [...applySubstConstraints(subst, constraints), ...cg];
      const s4 = unify(ctx, applySubst(subst, guardType), tBool);
      subst = composeSubst(subst, s4);
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
  const patterns = expr.cases.filter((c) => !c.guard).map((c) => c.pattern);
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
var PRIMITIVE_TYPES = new Set(["number", "string", "boolean"]);
var instantiateTypeExpr = (texpr) => {
  const varMapping = new Map;
  const convert = (t) => {
    switch (t.kind) {
      case "TyApp":
        return tapp(convert(t.con), convert(t.arg));
      case "TyCon":
        return tcon(t.name);
      case "TyFun":
        return tfun(convert(t.param), convert(t.ret));
      case "TyVar": {
        if (PRIMITIVE_TYPES.has(t.name)) {
          return tcon(t.name);
        }
        let fresh = varMapping.get(t.name);
        if (!fresh) {
          fresh = freshTypeVar();
          varMapping.set(t.name, fresh);
        }
        return fresh;
      }
    }
  };
  const result = convert(texpr);
  const subst = new Map;
  return [result, subst];
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
      case "PAs": {
        const inner = getPatternConstructors([pattern.pattern]);
        for (const c of inner) {
          constructors.add(c);
        }
        if (inner.has("*"))
          return new Set(["*"]);
        break;
      }
      case "POr": {
        const inner = getPatternConstructors(pattern.alternatives);
        for (const c of inner) {
          constructors.add(c);
        }
        if (inner.has("*"))
          return new Set(["*"]);
        break;
      }
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
var processDeclarations = (declarations, initial = {
  typeEnv: new Map,
  registry: new Map,
  constructorNames: []
}) => {
  let typeEnv = initial.typeEnv;
  let registry = initial.registry;
  const constructorNames = [...initial.constructorNames];
  for (const decl of declarations) {
    const [newEnv, newReg] = processDataDecl(decl);
    typeEnv = mergeEnvs(typeEnv, newEnv);
    registry = mergeRegistries(registry, newReg);
    for (const con of decl.constructors) {
      constructorNames.push(con.name);
    }
  }
  return { typeEnv, registry, constructorNames };
};
var baseEnv = new Map;

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
var vref = () => ({ kind: "VRef", value: null });
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
    case "Var": {
      const value = env.get(expr.name);
      if (value.kind === "VRef") {
        if (value.value === null) {
          throw new RuntimeError(`Uninitialized recursive binding: ${expr.name}`);
        }
        return value.value;
      }
      return value;
    }
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
      const refs = new Map;
      let newEnv = env;
      for (const binding of expr.bindings) {
        const ref = vref();
        refs.set(binding.name, ref);
        newEnv = extendEnv(newEnv, binding.name, ref);
      }
      for (const binding of expr.bindings) {
        const value = evaluate(newEnv, binding.value);
        refs.get(binding.name).value = value;
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
    case "TupleIndex": {
      const tuple = evaluate(env, expr.tuple);
      return tuple.elements[expr.index];
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
    case "++":
      return vstr(left.value + right.value);
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
    case "VRef":
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
    case "PAs": {
      const result = matchPattern(pattern.pattern, value);
      if (!result.matched)
        return { matched: false };
      result.bindings.set(pattern.name, value);
      return result;
    }
    case "POr": {
      for (const alt of pattern.alternatives) {
        const result = matchPattern(alt, value);
        if (result.matched)
          return result;
      }
      return { matched: false };
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
      if (case_.guard) {
        const guardResult = evaluate(caseEnv, case_.guard);
        if (!guardResult.value) {
          continue;
        }
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
    case "VRef":
      return value.value ? valueToString(value.value) : "<uninitialized>";
  }
};
var createConstructorEnv = (constructorNames) => {
  const env = new Map;
  for (const name of constructorNames) {
    env.set(name, vcon(name));
  }
  return env;
};

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
var let_ = (name, value, body, span, nameSpan, returnType) => ({
  kind: "Let",
  name,
  nameSpan,
  returnType,
  value,
  body,
  span
});
var recBinding = (name, value, nameSpan, returnType) => ({
  name,
  nameSpan,
  returnType,
  value
});
var letRec = (bindings, body, span) => ({
  kind: "LetRec",
  bindings,
  body,
  span
});
var var_ = (name, span) => ({ kind: "Var", name, span });
var abs = (param, body, span, paramSpan, paramType) => ({
  kind: "Abs",
  param,
  paramSpan,
  paramType,
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
var tupleIndex = (tuple2, index, span) => ({
  kind: "TupleIndex",
  tuple: tuple2,
  index,
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
var pcon = (name, args, span, nameSpan) => ({
  kind: "PCon",
  name,
  args,
  span,
  nameSpan
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
var pas = (pattern, name, span, nameSpan) => ({
  kind: "PAs",
  pattern,
  name,
  span,
  nameSpan
});
var por = (alternatives, span) => ({
  kind: "POr",
  alternatives,
  span
});
var case_ = (pattern, body, guard, span) => ({
  pattern,
  guard,
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
var tyfun = (param, ret, span) => ({
  kind: "TyFun",
  param,
  ret,
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
  TokenKind2[TokenKind2["As"] = 17] = "As";
  TokenKind2[TokenKind2["AndKw"] = 18] = "AndKw";
  TokenKind2[TokenKind2["Plus"] = 19] = "Plus";
  TokenKind2[TokenKind2["Minus"] = 20] = "Minus";
  TokenKind2[TokenKind2["Star"] = 21] = "Star";
  TokenKind2[TokenKind2["Slash"] = 22] = "Slash";
  TokenKind2[TokenKind2["Lt"] = 23] = "Lt";
  TokenKind2[TokenKind2["Le"] = 24] = "Le";
  TokenKind2[TokenKind2["Gt"] = 25] = "Gt";
  TokenKind2[TokenKind2["Ge"] = 26] = "Ge";
  TokenKind2[TokenKind2["EqEq"] = 27] = "EqEq";
  TokenKind2[TokenKind2["Ne"] = 28] = "Ne";
  TokenKind2[TokenKind2["Pipe"] = 29] = "Pipe";
  TokenKind2[TokenKind2["Arrow"] = 30] = "Arrow";
  TokenKind2[TokenKind2["Eq"] = 31] = "Eq";
  TokenKind2[TokenKind2["Bar"] = 32] = "Bar";
  TokenKind2[TokenKind2["Comma"] = 33] = "Comma";
  TokenKind2[TokenKind2["Dot"] = 34] = "Dot";
  TokenKind2[TokenKind2["Underscore"] = 35] = "Underscore";
  TokenKind2[TokenKind2["ColonColon"] = 36] = "ColonColon";
  TokenKind2[TokenKind2["And"] = 37] = "And";
  TokenKind2[TokenKind2["Or"] = 38] = "Or";
  TokenKind2[TokenKind2["PlusPlus"] = 39] = "PlusPlus";
  TokenKind2[TokenKind2["Colon"] = 40] = "Colon";
  TokenKind2[TokenKind2["LParen"] = 41] = "LParen";
  TokenKind2[TokenKind2["RParen"] = 42] = "RParen";
  TokenKind2[TokenKind2["LBrace"] = 43] = "LBrace";
  TokenKind2[TokenKind2["RBrace"] = 44] = "RBrace";
  TokenKind2[TokenKind2["LBracket"] = 45] = "LBracket";
  TokenKind2[TokenKind2["RBracket"] = 46] = "RBracket";
  TokenKind2[TokenKind2["Error"] = 47] = "Error";
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
var LBRACKET = 91;
var RBRACKET = 93;
var COLON = 58;
var AMPERSAND = 38;
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
  ["false", 16 /* False */],
  ["as", 17 /* As */],
  ["and", 18 /* AndKw */]
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
    return [47 /* Error */, start, state.pos];
  }
  advance(state);
  return [2 /* String */, start, state.pos];
};
var scanLowerOrKeyword = (state, start) => {
  if (peek(state) === UNDERSCORE && !isIdentContinue(peekAt(state, 1))) {
    advance(state);
    return [35 /* Underscore */, start, state.pos];
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
      if (peek(state) === PLUS) {
        advance(state);
        return [39 /* PlusPlus */, start, state.pos];
      }
      return [19 /* Plus */, start, state.pos];
    case MINUS:
      return [20 /* Minus */, start, state.pos];
    case STAR:
      return [21 /* Star */, start, state.pos];
    case SLASH:
      return [22 /* Slash */, start, state.pos];
    case LT:
      if (peek(state) === EQ) {
        advance(state);
        return [24 /* Le */, start, state.pos];
      }
      return [23 /* Lt */, start, state.pos];
    case GT:
      if (peek(state) === EQ) {
        advance(state);
        return [26 /* Ge */, start, state.pos];
      }
      return [25 /* Gt */, start, state.pos];
    case EQ:
      if (peek(state) === EQ) {
        advance(state);
        return [27 /* EqEq */, start, state.pos];
      }
      if (peek(state) === GT) {
        advance(state);
        return [30 /* Arrow */, start, state.pos];
      }
      return [31 /* Eq */, start, state.pos];
    case BANG:
      if (peek(state) === EQ) {
        advance(state);
        return [28 /* Ne */, start, state.pos];
      }
      return [47 /* Error */, start, state.pos];
    case PIPE:
      if (peek(state) === GT) {
        advance(state);
        return [29 /* Pipe */, start, state.pos];
      }
      if (peek(state) === PIPE) {
        advance(state);
        return [38 /* Or */, start, state.pos];
      }
      return [32 /* Bar */, start, state.pos];
    case AMPERSAND:
      if (peek(state) === AMPERSAND) {
        advance(state);
        return [37 /* And */, start, state.pos];
      }
      return [47 /* Error */, start, state.pos];
    case COMMA:
      return [33 /* Comma */, start, state.pos];
    case DOT:
      return [34 /* Dot */, start, state.pos];
    case LPAREN:
      return [41 /* LParen */, start, state.pos];
    case RPAREN:
      return [42 /* RParen */, start, state.pos];
    case LBRACE:
      return [43 /* LBrace */, start, state.pos];
    case RBRACE:
      return [44 /* RBrace */, start, state.pos];
    case LBRACKET:
      return [45 /* LBracket */, start, state.pos];
    case RBRACKET:
      return [46 /* RBracket */, start, state.pos];
    case COLON:
      if (peek(state) === COLON) {
        advance(state);
        return [36 /* ColonColon */, start, state.pos];
      }
      return [40 /* Colon */, start, state.pos];
    default:
      return [47 /* Error */, start, state.pos];
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
var atNewStatement = (state) => state.lexer.atLineStart && !at(state, 32 /* Bar */);
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
  const start = state.current[1];
  advance2(state);
  const recursive = at(state, 6 /* Rec */);
  if (recursive)
    advance2(state);
  if (!recursive && atAny(state, 41 /* LParen */, 43 /* LBrace */, 35 /* Underscore */, 4 /* Upper */)) {
    const pattern = parsePattern(state);
    expect(state, 31 /* Eq */, "expected '=' after pattern");
    const value = parseExpr(state);
    expect(state, 7 /* In */, "expected 'in' after let value");
    const body2 = parseExpr(state);
    const end = body2.span?.end ?? state.current[1];
    return { kind: "expr", expr: match(value, [case_(pattern, body2)], span(start, end)) };
  }
  const nameToken = expect(state, 3 /* Lower */, "expected binding name");
  if (!nameToken) {
    synchronize(state);
    return { kind: "expr", expr: num(0) };
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = [];
  while (at(state, 3 /* Lower */) || at(state, 41 /* LParen */)) {
    if (at(state, 41 /* LParen */)) {
      advance2(state);
      const paramToken = expect(state, 3 /* Lower */, "expected parameter name");
      if (!paramToken)
        break;
      const paramName = text(state, paramToken);
      let paramType;
      if (at(state, 40 /* Colon */)) {
        advance2(state);
        paramType = parseType(state) ?? undefined;
      }
      expect(state, 42 /* RParen */, "expected ')' after parameter");
      params.push({ name: paramName, span: tokenSpan(paramToken), type: paramType });
    } else {
      const paramToken = advance2(state);
      params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
    }
  }
  let returnType;
  if (at(state, 40 /* Colon */)) {
    advance2(state);
    returnType = parseType(state) ?? undefined;
  }
  if (!expect(state, 31 /* Eq */, "expected '=' after parameters")) {
    synchronize(state);
    return { kind: "expr", expr: num(0) };
  }
  const body = parseExpr(state);
  if (at(state, 7 /* In */) || recursive && at(state, 18 /* AndKw */)) {
    let value = body;
    for (let i = params.length - 1;i >= 0; i--) {
      const p = params[i];
      value = abs(p.name, value, undefined, p.span, p.type);
    }
    if (recursive) {
      const bindings = [recBinding(name, value, nameSpan, returnType)];
      while (at(state, 18 /* AndKw */)) {
        advance2(state);
        bindings.push(parseRecBinding(state));
      }
      expect(state, 7 /* In */, "expected 'in' after let rec bindings");
      const continuation2 = parseExpr(state);
      return { kind: "expr", expr: letRec(bindings, continuation2) };
    }
    advance2(state);
    const continuation = parseExpr(state);
    return {
      kind: "expr",
      expr: let_(name, value, continuation, undefined, nameSpan, returnType)
    };
  }
  return { kind: "binding", binding: { name, nameSpan, params, returnType, body, recursive } };
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
  if (!expect(state, 31 /* Eq */, "expected '=' after type parameters")) {
    synchronize(state);
    return null;
  }
  const constructors = [];
  const first = parseConstructor(state);
  if (first)
    constructors.push(first);
  while (at(state, 32 /* Bar */)) {
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
  while (atAny(state, 3 /* Lower */, 4 /* Upper */, 41 /* LParen */) && !atNewStatement(state)) {
    const field2 = parseTypeAtom(state);
    if (field2)
      fields.push(field2);
    else
      break;
  }
  return conDecl(name, fields);
};
var parseType = (state) => {
  const left = parseTypeAtom(state);
  if (!left)
    return null;
  if (at(state, 20 /* Minus */) && state.lexer.source.charCodeAt(state.current[2]) === 62) {
    advance2(state);
    advance2(state);
    const right = parseType(state);
    if (!right)
      return left;
    return tyfun(left, right);
  }
  return left;
};
var parseTypeAtom = (state) => {
  if (at(state, 3 /* Lower */)) {
    return tyvar(text(state, advance2(state)));
  }
  if (at(state, 4 /* Upper */)) {
    let type = tycon(text(state, advance2(state)));
    while (atAny(state, 3 /* Lower */, 4 /* Upper */, 41 /* LParen */) && !atNewStatement(state)) {
      const arg = parseTypeAtomSimple(state);
      if (arg) {
        type = tyapp(type, arg);
      } else {
        break;
      }
    }
    return type;
  }
  if (at(state, 41 /* LParen */)) {
    advance2(state);
    const inner = parseTypeAtom(state);
    expect(state, 42 /* RParen */, "expected ')' after type");
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
  if (at(state, 41 /* LParen */)) {
    advance2(state);
    const inner = parseTypeAtom(state);
    expect(state, 42 /* RParen */, "expected ')' after type");
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
      return str(parseStringContent(state, text(state, token), token[1]), tokenSpan(token));
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
      if (at(state, 30 /* Arrow */)) {
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
    case 41 /* LParen */:
      return parseParenOrTuple(state);
    case 20 /* Minus */: {
      advance2(state);
      const operand = parsePrecedence(state, 50 /* Multiplicative */ + 1);
      const end = operand.span?.end ?? state.current[1];
      return binOp("-", num(0, span(start, start)), operand, span(start, end));
    }
    case 43 /* LBrace */:
      return parseRecord(state);
    case 45 /* LBracket */:
      return parseListLiteral(state);
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
    case 19 /* Plus */:
      return binOp2("+");
    case 20 /* Minus */:
      return binOp2("-");
    case 21 /* Star */:
      return binOp2("*");
    case 22 /* Slash */:
      return binOp2("/");
    case 23 /* Lt */:
      return binOp2("<");
    case 24 /* Le */:
      return binOp2("<=");
    case 25 /* Gt */:
      return binOp2(">");
    case 26 /* Ge */:
      return binOp2(">=");
    case 27 /* EqEq */:
      return binOp2("==");
    case 28 /* Ne */:
      return binOp2("!=");
    case 39 /* PlusPlus */:
      return binOp2("++");
    case 37 /* And */: {
      advance2(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return if_(left, right, bool(false), span(start, end));
    }
    case 38 /* Or */: {
      advance2(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return if_(left, bool(true), right, span(start, end));
    }
    case 29 /* Pipe */: {
      advance2(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return app(right, left, span(start, end));
    }
    case 36 /* ColonColon */: {
      advance2(state);
      const right = parsePrecedence(state, bp - 1);
      const end = right.span?.end ?? state.current[1];
      return app(app(var_("Cons"), left), right, span(start, end));
    }
    case 34 /* Dot */: {
      advance2(state);
      if (at(state, 1 /* Number */)) {
        const indexToken = state.current;
        advance2(state);
        const indexStr = text(state, indexToken);
        const index = parseInt(indexStr, 10);
        if (!Number.isInteger(index) || index < 0) {
          state.diagnostics.push({
            start: indexToken[1],
            end: indexToken[2],
            message: "tuple index must be a non-negative integer",
            severity: "error"
          });
        }
        const end2 = indexToken[2];
        return tupleIndex(left, index, span(start, end2));
      }
      const fieldToken = expect(state, 3 /* Lower */, "expected field name or index after '.'");
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
    case 29 /* Pipe */:
      return 10 /* Pipe */;
    case 38 /* Or */:
      return 12 /* Or */;
    case 37 /* And */:
      return 14 /* And */;
    case 36 /* ColonColon */:
      return 15 /* Cons */;
    case 27 /* EqEq */:
    case 28 /* Ne */:
      return 20 /* Equality */;
    case 23 /* Lt */:
    case 24 /* Le */:
    case 25 /* Gt */:
    case 26 /* Ge */:
      return 30 /* Comparison */;
    case 19 /* Plus */:
    case 20 /* Minus */:
    case 39 /* PlusPlus */:
      return 40 /* Additive */;
    case 21 /* Star */:
    case 22 /* Slash */:
      return 50 /* Multiplicative */;
    case 34 /* Dot */:
      return 70 /* FieldAccess */;
    case 3 /* Lower */:
    case 4 /* Upper */:
    case 1 /* Number */:
    case 2 /* String */:
    case 15 /* True */:
    case 16 /* False */:
    case 41 /* LParen */:
    case 43 /* LBrace */:
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
  if (at(state, 42 /* RParen */)) {
    advance2(state);
    error2(state, "empty parentheses");
    return num(0);
  }
  if (at(state, 3 /* Lower */)) {
    const savedPos = state.lexer.pos;
    const savedCurrent = state.current;
    const nameToken = advance2(state);
    if (at(state, 40 /* Colon */)) {
      advance2(state);
      const paramType = parseType(state);
      expect(state, 42 /* RParen */, "expected ')' after type");
      if (at(state, 30 /* Arrow */)) {
        advance2(state);
        const body = parseExpr(state);
        const end = body.span?.end ?? state.current[1];
        return abs(text(state, nameToken), body, span(start, end), tokenSpan(nameToken), paramType ?? undefined);
      }
      error2(state, "expected '=>' after annotated parameter");
      return num(0);
    }
    state.lexer.pos = savedPos;
    state.current = savedCurrent;
  }
  const first = parseExpr(state);
  if (at(state, 33 /* Comma */)) {
    const elements = [first];
    while (at(state, 33 /* Comma */)) {
      advance2(state);
      elements.push(parseExpr(state));
    }
    const endToken = expect(state, 42 /* RParen */, "expected ')' after tuple");
    const end = endToken ? endToken[2] : state.current[1];
    return tuple(elements, span(start, end));
  }
  expect(state, 42 /* RParen */, "expected ')' after expression");
  return first;
};
var parseRecord = (state) => {
  const start = state.current[1];
  advance2(state);
  const fields = [];
  if (!at(state, 44 /* RBrace */)) {
    do {
      if (at(state, 33 /* Comma */))
        advance2(state);
      const nameToken = expect(state, 3 /* Lower */, "expected field name");
      if (!nameToken)
        break;
      const name = text(state, nameToken);
      const fieldStart = nameToken[1];
      const fieldEnd = nameToken[2];
      if (at(state, 31 /* Eq */)) {
        advance2(state);
        const value = parseExpr(state);
        const valueEnd = value.span?.end ?? state.current[1];
        fields.push(field(name, value, span(fieldStart, valueEnd)));
      } else {
        const value = var_(name, span(fieldStart, fieldEnd));
        fields.push(field(name, value, span(fieldStart, fieldEnd)));
      }
    } while (at(state, 33 /* Comma */));
  }
  const endToken = expect(state, 44 /* RBrace */, "expected '}' after record");
  const end = endToken ? endToken[2] : state.current[1];
  return record(fields, span(start, end));
};
var parseListLiteral = (state) => {
  const start = state.current[1];
  advance2(state);
  const elements = [];
  if (!at(state, 46 /* RBracket */)) {
    do {
      if (at(state, 33 /* Comma */))
        advance2(state);
      elements.push(parseExpr(state));
    } while (at(state, 33 /* Comma */));
  }
  const endToken = expect(state, 46 /* RBracket */, "expected ']' after list");
  const end = endToken ? endToken[2] : state.current[1];
  let result = var_("Nil", span(end - 1, end));
  for (let i = elements.length - 1;i >= 0; i--) {
    const elem = elements[i];
    const elemStart = elem.span?.start ?? start;
    result = app(app(var_("Cons"), elem), result, span(elemStart, end));
  }
  return result;
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
  while (at(state, 32 /* Bar */)) {
    const caseStart = state.current[1];
    advance2(state);
    let pattern = parsePattern(state);
    if (at(state, 32 /* Bar */)) {
      const alternatives = [pattern];
      while (at(state, 32 /* Bar */)) {
        advance2(state);
        alternatives.push(parsePattern(state));
      }
      const orEnd = alternatives[alternatives.length - 1]?.span?.end ?? state.current[1];
      pattern = por(alternatives, span(caseStart, orEnd));
    }
    let guard;
    if (at(state, 8 /* If */)) {
      advance2(state);
      guard = parseExpr(state);
    }
    expect(state, 30 /* Arrow */, "expected '=>' after pattern");
    const body = parseExpr(state);
    const caseEnd = body.span?.end ?? state.current[1];
    cases.push(case_(pattern, body, guard, span(caseStart, caseEnd)));
  }
  const endToken = expect(state, 13 /* End */, "expected 'end' after match cases");
  const end = endToken ? endToken[2] : state.current[1];
  return match(scrutinee, cases, span(start, end));
};
var parseRecBinding = (state) => {
  const nameToken = expect(state, 3 /* Lower */, "expected binding name");
  if (!nameToken) {
    return recBinding("_error_", num(0));
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = [];
  while (at(state, 3 /* Lower */) || at(state, 41 /* LParen */)) {
    if (at(state, 41 /* LParen */)) {
      advance2(state);
      const paramToken = expect(state, 3 /* Lower */, "expected parameter name");
      if (!paramToken)
        break;
      const paramName = text(state, paramToken);
      let paramType;
      if (at(state, 40 /* Colon */)) {
        advance2(state);
        paramType = parseType(state) ?? undefined;
      }
      expect(state, 42 /* RParen */, "expected ')' after parameter");
      params.push({ name: paramName, span: tokenSpan(paramToken), type: paramType });
    } else {
      const paramToken = advance2(state);
      params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
    }
  }
  let returnType;
  if (at(state, 40 /* Colon */)) {
    advance2(state);
    returnType = parseType(state) ?? undefined;
  }
  expect(state, 31 /* Eq */, "expected '=' after name");
  let value = parseExpr(state);
  for (let i = params.length - 1;i >= 0; i--) {
    const p = params[i];
    value = abs(p.name, value, undefined, p.span, p.type);
  }
  return recBinding(name, value, nameSpan, returnType);
};
var parseLetExpr = (state) => {
  const start = state.current[1];
  advance2(state);
  const recursive = at(state, 6 /* Rec */);
  if (recursive)
    advance2(state);
  if (!recursive && atAny(state, 41 /* LParen */, 43 /* LBrace */, 35 /* Underscore */, 4 /* Upper */)) {
    return parseLetDestructuring(state, start);
  }
  if (recursive) {
    const bindings = [parseRecBinding(state)];
    while (at(state, 18 /* AndKw */)) {
      advance2(state);
      bindings.push(parseRecBinding(state));
    }
    expect(state, 7 /* In */, "expected 'in' after let rec bindings");
    const body2 = parseExpr(state);
    const end2 = body2.span?.end ?? state.current[1];
    return letRec(bindings, body2, span(start, end2));
  }
  const nameToken = expect(state, 3 /* Lower */, "expected binding name");
  if (!nameToken) {
    return num(0);
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = [];
  while (at(state, 3 /* Lower */) || at(state, 41 /* LParen */)) {
    if (at(state, 41 /* LParen */)) {
      advance2(state);
      const paramToken = expect(state, 3 /* Lower */, "expected parameter name");
      if (!paramToken)
        break;
      const paramName = text(state, paramToken);
      let paramType;
      if (at(state, 40 /* Colon */)) {
        advance2(state);
        paramType = parseType(state) ?? undefined;
      }
      expect(state, 42 /* RParen */, "expected ')' after parameter");
      params.push({ name: paramName, span: tokenSpan(paramToken), type: paramType });
    } else {
      const paramToken = advance2(state);
      params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
    }
  }
  let returnType;
  if (at(state, 40 /* Colon */)) {
    advance2(state);
    returnType = parseType(state) ?? undefined;
  }
  expect(state, 31 /* Eq */, "expected '=' after name");
  let value = parseExpr(state);
  for (let i = params.length - 1;i >= 0; i--) {
    const p = params[i];
    value = abs(p.name, value, undefined, p.span, p.type);
  }
  expect(state, 7 /* In */, "expected 'in' after let value");
  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];
  return let_(name, value, body, span(start, end), nameSpan, returnType);
};
var parseLetDestructuring = (state, start) => {
  const pattern = parsePattern(state);
  expect(state, 31 /* Eq */, "expected '=' after pattern");
  const value = parseExpr(state);
  expect(state, 7 /* In */, "expected 'in' after let value");
  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];
  return match(value, [case_(pattern, body)], span(start, end));
};
var PATTERN_STARTS = new Set([
  35 /* Underscore */,
  3 /* Lower */,
  4 /* Upper */,
  1 /* Number */,
  2 /* String */,
  15 /* True */,
  16 /* False */,
  41 /* LParen */,
  43 /* LBrace */
]);
var parsePattern = (state, allowArgs = true) => {
  const pattern = parsePatternCore(state, allowArgs);
  if (at(state, 17 /* As */)) {
    advance2(state);
    const nameToken = expect(state, 3 /* Lower */, "expected name after 'as'");
    if (!nameToken)
      return pattern;
    const name = text(state, nameToken);
    const nameSpan = tokenSpan(nameToken);
    const start = pattern.span?.start ?? nameToken[1];
    const end = nameToken[2];
    return pas(pattern, name, span(start, end), nameSpan);
  }
  return pattern;
};
var parsePatternCore = (state, allowArgs = true) => {
  const token = state.current;
  const kind = token[0];
  const start = token[1];
  switch (kind) {
    case 35 /* Underscore */:
      advance2(state);
      return pwildcard(tokenSpan(token));
    case 3 /* Lower */:
      advance2(state);
      return pvar(text(state, token), tokenSpan(token));
    case 4 /* Upper */: {
      advance2(state);
      const name = text(state, token);
      const nameSpan = tokenSpan(token);
      if (!allowArgs)
        return pcon(name, [], nameSpan, nameSpan);
      const args = [];
      while (PATTERN_STARTS.has(state.current[0])) {
        args.push(parsePatternCore(state, false));
      }
      const end = args[args.length - 1]?.span?.end ?? token[2];
      return pcon(name, args, span(start, end), nameSpan);
    }
    case 1 /* Number */:
      advance2(state);
      return plit(parseFloat(text(state, token)), tokenSpan(token));
    case 2 /* String */:
      advance2(state);
      return plit(parseStringContent(state, text(state, token), token[1]), tokenSpan(token));
    case 15 /* True */:
      advance2(state);
      return plit(true, tokenSpan(token));
    case 16 /* False */:
      advance2(state);
      return plit(false, tokenSpan(token));
    case 41 /* LParen */:
      return parseTuplePattern(state);
    case 43 /* LBrace */:
      return parseRecordPattern(state);
    default:
      error2(state, `unexpected token in pattern: ${TokenKind[kind]}`);
      if (allowArgs)
        advance2(state);
      return pwildcard();
  }
};
var parseTuplePattern = (state) => {
  const start = state.current[1];
  advance2(state);
  if (at(state, 42 /* RParen */)) {
    advance2(state);
    error2(state, "empty pattern");
    return pwildcard();
  }
  const first = parsePattern(state);
  if (at(state, 33 /* Comma */)) {
    const elements = [first];
    while (at(state, 33 /* Comma */)) {
      advance2(state);
      elements.push(parsePattern(state));
    }
    const endToken = expect(state, 42 /* RParen */, "expected ')' after tuple pattern");
    const end = endToken ? endToken[2] : state.current[1];
    return ptuple(elements, span(start, end));
  }
  expect(state, 42 /* RParen */, "expected ')' after pattern");
  return first;
};
var parseRecordPattern = (state) => {
  const start = state.current[1];
  advance2(state);
  const fields = [];
  if (!at(state, 44 /* RBrace */)) {
    do {
      if (at(state, 33 /* Comma */))
        advance2(state);
      const nameToken = expect(state, 3 /* Lower */, "expected field name");
      if (!nameToken)
        break;
      const name = text(state, nameToken);
      const fieldStart = nameToken[1];
      const fieldEnd = nameToken[2];
      if (at(state, 31 /* Eq */)) {
        advance2(state);
        const pattern = parsePattern(state);
        const patternEnd = pattern.span?.end ?? state.current[1];
        fields.push(pfield(name, pattern, span(fieldStart, patternEnd)));
      } else {
        const pattern = pvar(name, span(fieldStart, fieldEnd));
        fields.push(pfield(name, pattern, span(fieldStart, fieldEnd)));
      }
    } while (at(state, 33 /* Comma */));
  }
  const endToken = expect(state, 44 /* RBrace */, "expected '}' after record pattern");
  const end = endToken ? endToken[2] : state.current[1];
  return precord(fields, span(start, end));
};
var parseStringContent = (state, quoted, tokenStart) => {
  const inner = quoted.slice(1, -1);
  let result = "";
  let i = 0;
  while (i < inner.length) {
    if (inner[i] === "\\") {
      const escapeStart = tokenStart + 1 + i;
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
          state.diagnostics.push({
            message: `Unknown escape sequence: \\${inner[i]}`,
            start: escapeStart,
            end: escapeStart + 2,
            severity: "error"
          });
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
      value = abs(p.name, value, undefined, p.span, p.type);
    }
    if (binding.recursive) {
      expr = letRec([recBinding(binding.name, value, binding.nameSpan, binding.returnType)], expr);
    } else {
      expr = let_(binding.name, value, expr, undefined, binding.nameSpan, binding.returnType);
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
var map = letRec([
  recBinding("map", abs("f", abs("xs", match(var_("xs"), [
    case_(pcon("Nil", []), var_("Nil")),
    case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(var_("Cons"), app(var_("f"), var_("x"))), app(app(var_("map"), var_("f")), var_("rest"))))
  ]))))
], var_("map"));
var filter = letRec([
  recBinding("filter", abs("p", abs("xs", match(var_("xs"), [
    case_(pcon("Nil", []), var_("Nil")),
    case_(pcon("Cons", [pvar("x"), pvar("rest")]), if_(app(var_("p"), var_("x")), app(app(var_("Cons"), var_("x")), app(app(var_("filter"), var_("p")), var_("rest"))), app(app(var_("filter"), var_("p")), var_("rest"))))
  ]))))
], var_("filter"));
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
var length = letRec([
  recBinding("length", abs("xs", match(var_("xs"), [
    case_(pcon("Nil", []), num(0)),
    case_(pcon("Cons", [pwildcard(), pvar("rest")]), binOp("+", num(1), app(var_("length"), var_("rest"))))
  ])))
], var_("length"));
var foldr = letRec([
  recBinding("foldr", abs("f", abs("z", abs("xs", match(var_("xs"), [
    case_(pcon("Nil", []), var_("z")),
    case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(var_("f"), var_("x")), app(app(app(var_("foldr"), var_("f")), var_("z")), var_("rest"))))
  ])))))
], var_("foldr"));
var foldl = letRec([
  recBinding("foldl", abs("f", abs("z", abs("xs", match(var_("xs"), [
    case_(pcon("Nil", []), var_("z")),
    case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(app(var_("foldl"), var_("f")), app(app(var_("f"), var_("z")), var_("x"))), var_("rest")))
  ])))))
], var_("foldl"));
var reverse = abs("xs", app(app(app(foldl, abs("acc", abs("x", app(app(var_("Cons"), var_("x")), var_("acc"))))), var_("Nil")), var_("xs")));
var concat = abs("xs", abs("ys", app(app(app(foldr, var_("Cons")), var_("ys")), var_("xs"))));
var id = abs("x", var_("x"));
var const_ = abs("x", abs("_", var_("x")));
var compose = abs("f", abs("g", abs("x", app(var_("f"), app(var_("g"), var_("x"))))));
var flip = abs("f", abs("a", abs("b", app(app(var_("f"), var_("b")), var_("a")))));
var functions = {
  map,
  filter,
  head,
  tail,
  isEmpty,
  length,
  foldr,
  foldl,
  reverse,
  concat,
  id,
  const: const_,
  compose,
  flip
};

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
var SYNC_FULL = 1;
var SEVERITY_ERROR = 1;
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
        case "textDocument/completion":
          transport.send(successResponse(id2, handleCompletion(params)));
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
      textDocumentSync: SYNC_FULL,
      hoverProvider: true,
      definitionProvider: true,
      renameProvider: {
        prepareProvider: true
      },
      completionProvider: {
        triggerCharacters: ["."]
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
    if (!doc?.symbols || !doc.types)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const { symbols, types } = doc;
    const ref = findReferenceAt(symbols, offset);
    if (ref?.definition) {
      const type = types.get(ref.definition);
      if (type) {
        return {
          contents: {
            kind: "markdown",
            value: `\`\`\`algow
${ref.name}: ${typeToString(type)}
\`\`\``
          },
          range: spanToRange(doc.text, ref.span)
        };
      }
    }
    const def = findDefinitionAt(symbols, offset);
    if (def) {
      const type = types.get(def);
      if (type) {
        return {
          contents: {
            kind: "markdown",
            value: `\`\`\`algow
${def.name}: ${typeToString(type)}
\`\`\``
          },
          range: spanToRange(doc.text, def.span)
        };
      }
    }
    return null;
  };
  const handleDefinition = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.symbols)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const def = goToDefinition(doc.symbols, offset);
    if (!def)
      return null;
    return {
      uri: params.textDocument.uri,
      range: spanToRange(doc.text, def.span)
    };
  };
  const handlePrepareRename = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc?.symbols)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const ref = findReferenceAt(doc.symbols, offset);
    if (ref?.definition)
      return spanToRange(doc.text, ref.span);
    const def = findDefinitionAt(doc.symbols, offset);
    if (def)
      return spanToRange(doc.text, def.span);
    return null;
  };
  const handleRename = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.symbols)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const { definition, references } = findAllOccurrences(doc.symbols, offset);
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
  const COMPLETION_KIND_FUNCTION = 3;
  const COMPLETION_KIND_CONSTRUCTOR = 4;
  const COMPLETION_KIND_FIELD = 5;
  const COMPLETION_KIND_VARIABLE = 6;
  const COMPLETION_KIND_KEYWORD = 14;
  const handleCompletion = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const items = [];
    const beforeCursor = doc.text.slice(0, offset);
    const dotMatch = beforeCursor.match(/(\w+)\s*\.\s*$/);
    if (dotMatch) {
      const varName = dotMatch[1];
      const fieldItems = getRecordFieldCompletions(doc, varName);
      items.push(...fieldItems);
    } else {
      items.push(...getVariableCompletions(doc, offset));
      items.push(...getConstructorCompletions(doc));
      items.push(...getPreludeFunctionCompletions());
      items.push(...getKeywordCompletions());
    }
    return { isIncomplete: false, items };
  };
  const getRecordFieldCompletions = (doc, varName) => {
    if (!doc.symbols || !doc.types)
      return [];
    for (const def of doc.symbols.definitions) {
      if (def.name === varName) {
        const type = doc.types.get(def);
        if (type) {
          return extractRecordFields(type);
        }
      }
    }
    return [];
  };
  const extractRecordFields = (type) => {
    const items = [];
    const resolved = resolveType(type);
    if (resolved.kind === "TRecord") {
      for (const [fieldName, fieldType] of resolved.fields) {
        items.push({
          label: fieldName,
          kind: COMPLETION_KIND_FIELD,
          detail: typeToString(fieldType)
        });
      }
    } else if (resolved.kind === "TTuple") {
      for (let i = 0;i < resolved.elements.length; i++) {
        items.push({
          label: String(i),
          kind: COMPLETION_KIND_FIELD,
          detail: typeToString(resolved.elements[i])
        });
      }
    }
    return items;
  };
  const resolveType = (type) => {
    return type;
  };
  const getVariableCompletions = (doc, _offset) => {
    if (!doc.symbols || !doc.types)
      return [];
    const items = [];
    const seen = new Set;
    for (const def of doc.symbols.definitions) {
      if (seen.has(def.name))
        continue;
      seen.add(def.name);
      if (def.kind === "constructor")
        continue;
      const type = doc.types.get(def);
      const detail = type ? typeToString(type) : undefined;
      items.push({
        label: def.name,
        kind: def.kind === "parameter" ? COMPLETION_KIND_VARIABLE : COMPLETION_KIND_FUNCTION,
        detail
      });
    }
    return items;
  };
  const getConstructorCompletions = (doc) => {
    const items = [];
    for (const [typeName, constructors] of doc.registry) {
      for (const conName of constructors) {
        items.push({
          label: conName,
          kind: COMPLETION_KIND_CONSTRUCTOR,
          detail: typeName
        });
      }
    }
    return items;
  };
  const getPreludeFunctionCompletions = () => {
    const items = [];
    for (const name of Object.keys(functions)) {
      items.push({
        label: name,
        kind: COMPLETION_KIND_FUNCTION,
        detail: "prelude"
      });
    }
    return items;
  };
  const getKeywordCompletions = () => {
    const keywords2 = [
      "let",
      "rec",
      "in",
      "if",
      "then",
      "else",
      "match",
      "with",
      "end",
      "data",
      "true",
      "false"
    ];
    return keywords2.map((kw) => ({
      label: kw,
      kind: COMPLETION_KIND_KEYWORD
    }));
  };
  const handleEvaluate = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }
    if (doc.diagnostics.some((d) => d.severity === SEVERITY_ERROR)) {
      return { success: false, error: "Cannot evaluate: document has errors" };
    }
    const expr = programToExpr(doc.program);
    if (!expr) {
      return { success: false, error: "No expression to evaluate" };
    }
    try {
      const prelude = processDeclarations(declarations);
      const { constructorNames } = processDeclarations(doc.program.declarations, prelude);
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
    const prelude = processDeclarations(declarations);
    const { typeEnv, registry, constructorNames } = processDeclarations(parseResult.program.declarations, prelude);
    const expr = programToExpr(parseResult.program);
    let symbols = null;
    let types = null;
    if (expr) {
      const bindResult = bindWithConstructors(constructorNames, expr);
      symbols = bindResult.symbols;
      for (const diag of bindResult.diagnostics) {
        lspDiagnostics.push(convertDiagnostic(text2, diag));
      }
      const checkResult = check(typeEnv, registry, expr, symbols);
      types = checkResult.types;
      for (const diag of checkResult.diagnostics) {
        lspDiagnostics.push(convertDiagnostic(text2, diag));
      }
    }
    return {
      uri,
      version,
      text: text2,
      diagnostics: lspDiagnostics,
      symbols,
      types,
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
    onClose() {}
  };
};

// playground/worker.ts
var transport = createWorkerTransport();
createServer(transport);
