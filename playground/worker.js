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
var qualifiedVar = (moduleName, member, span, moduleSpan, memberSpan) => ({
  kind: "QualifiedVar",
  moduleName,
  moduleSpan,
  member,
  memberSpan,
  span
});
var qualifiedPCon = (moduleName, constructor, args, span, moduleSpan, constructorSpan) => ({
  kind: "QualifiedPCon",
  moduleName,
  moduleSpan,
  constructor,
  constructorSpan,
  args,
  span
});
var moduleDecl = (name, declarations, bindings, span, nameSpan) => ({
  kind: "ModuleDecl",
  name,
  nameSpan,
  declarations,
  bindings,
  span
});
var useDecl = (moduleName, imports, alias, span, moduleSpan, aliasSpan) => ({
  kind: "UseDecl",
  moduleName,
  moduleSpan,
  alias,
  aliasSpan,
  imports,
  span
});
var importAll = () => ({ kind: "All" });
var importSpecific = (items) => ({
  kind: "Specific",
  items
});
var importItem = (name, constructors, span) => ({
  name,
  span,
  constructors
});

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
      for (const field2 of expr.fields) {
        bindExpr(ctx, field2.value);
      }
      break;
    case "FieldAccess":
      bindExpr(ctx, expr.record);
      break;
    case "TupleIndex":
      bindExpr(ctx, expr.tuple);
      break;
    case "QualifiedVar":
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
  for (const case_2 of expr.cases) {
    const bindings = bindPattern(ctx, case_2.pattern);
    bindExpr(ctx, case_2.body);
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
    case "QualifiedPCon": {
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
      for (const field2 of pattern.fields) {
        bindings.push(...bindPattern(ctx, field2.pattern));
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
var createContext2 = (symbols, moduleEnv = new Map, moduleAliases = new Map) => {
  const definitionMap = new Map;
  for (const def of symbols.definitions) {
    definitionMap.set(`${def.span.start}:${def.span.end}`, def);
  }
  return {
    diagnostics: [],
    types: new Map,
    symbols,
    definitionMap,
    moduleEnv,
    moduleAliases,
    typeVarCounter: 0,
    spanTypes: new Map
  };
};
var addError = (ctx, message, span) => {
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(error(start, end, message));
};
var recordType = (ctx, span, type) => {
  const key = `${span.start}:${span.end}`;
  const def = ctx.definitionMap.get(key);
  if (def) {
    ctx.types.set(def, type);
  }
  ctx.spanTypes.set(key, type);
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
var freshTypeVar = (ctx) => {
  return tvar(`t${ctx.typeVarCounter++}`);
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
  const freshRow = freshTypeVar(ctx);
  const s1 = unify(ctx, row1, trecord(extraFields2, freshRow));
  currentSubst = composeSubst(currentSubst, s1);
  const s2 = unify(ctx, applySubst(currentSubst, row2), trecord(extraFields1, freshRow));
  return composeSubst(currentSubst, s2);
};
var instantiate = (ctx, s) => {
  const freshVars = new Map;
  for (const name of s.vars) {
    freshVars.set(name, freshTypeVar(ctx));
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
  const solve = (c) => {
    const { className, type } = c;
    switch (type.kind) {
      case "TVar":
        return;
      case "TCon": {
        const classInstances = instances.get(className);
        if (!classInstances?.has(type.name)) {
          addError(ctx, `Type '${type.name}' does not satisfy ${className}`);
        }
        return;
      }
      case "TFun":
        addError(ctx, `Function types do not satisfy ${className}`);
        return;
      case "TTuple": {
        if (className === "Eq" || className === "Ord") {
          for (const elemType of type.elements) {
            solve({ className, type: elemType });
          }
          return;
        }
        addError(ctx, `Tuples do not satisfy ${className}`);
        return;
      }
      case "TApp":
        addError(ctx, `Type '${typeToString(type)}' does not satisfy ${className}`);
        return;
      case "TRecord":
        addError(ctx, `Record types do not satisfy ${className}`);
        return;
    }
  };
  for (const c of constraints) {
    solve(c);
  }
};
var check = (env, registry, expr, symbols, moduleEnv = new Map, moduleAliases = new Map) => {
  const ctx = createContext2(symbols, moduleEnv, moduleAliases);
  const [subst, type, constraints] = inferExpr(ctx, env, registry, expr);
  const finalConstraints = applySubstConstraints(subst, constraints);
  solveConstraints(ctx, finalConstraints);
  return {
    subst,
    type,
    constraints: finalConstraints,
    diagnostics: ctx.diagnostics,
    types: ctx.types,
    spanTypes: ctx.spanTypes
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
    case "QualifiedVar":
      return inferQualifiedVar(ctx, expr);
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
    const fieldType2 = freshTypeVar(ctx);
    const rowVar = freshTypeVar(ctx);
    const openRecord = trecord([[expr.field, fieldType2]], rowVar);
    const s2 = unify(ctx, resolvedType, openRecord);
    return [composeSubst(s1, s2), applySubst(s2, fieldType2), constraints];
  }
  if (resolvedType.kind !== "TRecord") {
    addError(ctx, `Cannot access field '${expr.field}' on non-record type: ${typeToString(resolvedType)}`);
    return [s1, freshTypeVar(ctx), constraints];
  }
  const fieldType = resolvedType.fields.get(expr.field);
  if (fieldType) {
    return [s1, fieldType, constraints];
  }
  if (resolvedType.row) {
    const newFieldType = freshTypeVar(ctx);
    const newRowVar = freshTypeVar(ctx);
    const s2 = unify(ctx, resolvedType.row, trecord([[expr.field, newFieldType]], newRowVar));
    return [composeSubst(s1, s2), applySubst(s2, newFieldType), constraints];
  }
  addError(ctx, `Record has no field '${expr.field}'. Available: ${[...resolvedType.fields.keys()].join(", ")}`);
  return [s1, freshTypeVar(ctx), constraints];
};
var inferTupleIndex = (ctx, env, registry, expr) => {
  const [s1, tupleType, constraints] = inferExpr(ctx, env, registry, expr.tuple);
  const resolvedType = applySubst(s1, tupleType);
  if (resolvedType.kind === "TVar") {
    const elementType = freshTypeVar(ctx);
    return [s1, elementType, constraints];
  }
  if (resolvedType.kind !== "TTuple") {
    addError(ctx, `Cannot index into non-tuple type: ${typeToString(resolvedType)}`);
    return [s1, freshTypeVar(ctx), constraints];
  }
  if (expr.index < 0 || expr.index >= resolvedType.elements.length) {
    addError(ctx, `Tuple index ${expr.index} out of bounds for tuple of ${resolvedType.elements.length} element(s)`);
    return [s1, freshTypeVar(ctx), constraints];
  }
  return [s1, resolvedType.elements[expr.index], constraints];
};
var inferRecord = (ctx, env, registry, expr) => {
  let subst = new Map;
  let constraints = [];
  const fieldTypes = new Map;
  for (const field2 of expr.fields) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), registry, field2.value);
    subst = composeSubst(subst, s);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    fieldTypes.set(field2.name, applySubst(subst, t));
  }
  return [subst, trecord([...fieldTypes.entries()]), constraints];
};
var inferAbs = (ctx, env, registry, expr) => {
  let paramType;
  let subst = new Map;
  if (expr.paramType) {
    const annotatedType = instantiateTypeExpr(ctx, expr.paramType);
    paramType = annotatedType;
  } else {
    paramType = freshTypeVar(ctx);
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
  const returnType = freshTypeVar(ctx);
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
    const annotatedType = instantiateTypeExpr(ctx, expr.returnType);
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
    const placeholder = freshTypeVar(ctx);
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
      const annotatedType = instantiateTypeExpr(ctx, binding.returnType);
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
    return [new Map, freshTypeVar(ctx), []];
  }
  return [new Map, instantiate(ctx, s), []];
};
var inferQualifiedVar = (ctx, expr) => {
  const realModule = ctx.moduleAliases.get(expr.moduleName) ?? expr.moduleName;
  const mod = ctx.moduleEnv.get(realModule);
  if (!mod) {
    addError(ctx, `Unknown module: ${expr.moduleName}`, expr.span);
    return [new Map, freshTypeVar(ctx), []];
  }
  const s = mod.typeEnv.get(expr.member);
  if (!s) {
    addError(ctx, `Module '${expr.moduleName}' does not export '${expr.member}'`, expr.span);
    return [new Map, freshTypeVar(ctx), []];
  }
  return [new Map, instantiate(ctx, s), []];
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
      const conType = instantiate(ctx, conScheme);
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
    case "QualifiedPCon": {
      const realModule = ctx.moduleAliases.get(pattern.moduleName) ?? pattern.moduleName;
      const mod = ctx.moduleEnv.get(realModule);
      if (!mod) {
        addError(ctx, `Unknown module: ${pattern.moduleName}`, pattern.span);
        return [subst, new Map];
      }
      const conScheme = mod.typeEnv.get(pattern.constructor);
      if (!conScheme) {
        addError(ctx, `Module '${pattern.moduleName}' does not export '${pattern.constructor}'`, pattern.span);
        return [subst, new Map];
      }
      const conType = instantiate(ctx, conScheme);
      const argTypes = [];
      let resultType = conType;
      while (resultType.kind === "TFun") {
        argTypes.push(resultType.param);
        resultType = resultType.ret;
      }
      if (argTypes.length !== pattern.args.length) {
        addError(ctx, `Constructor ${pattern.moduleName}.${pattern.constructor} expects ${argTypes.length} args, got ${pattern.args.length}`);
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
        for (const field2 of pattern.fields) {
          const fieldType = freshTypeVar(ctx);
          fieldTypes.set(field2.name, fieldType);
          const [s2, bindings] = inferPattern(ctx, env, field2.pattern, fieldType, currentSubst2);
          currentSubst2 = s2;
          for (const [name, type] of bindings) {
            allBindings2.set(name, type);
          }
        }
        const rowVar = freshTypeVar(ctx);
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
      for (const field2 of pattern.fields) {
        let fieldType = expectedResolved.fields.get(field2.name);
        if (!fieldType && expectedResolved.row) {
          const newFieldType = freshTypeVar(ctx);
          const newRowVar = freshTypeVar(ctx);
          const s2 = unify(ctx, applySubst(currentSubst, expectedResolved.row), trecord([[field2.name, newFieldType]], newRowVar));
          currentSubst = composeSubst(currentSubst, s2);
          fieldType = applySubst(currentSubst, newFieldType);
        }
        if (!fieldType) {
          addError(ctx, `Record has no field '${field2.name}'. Available: ${[...expectedResolved.fields.keys()].join(", ")}`);
          continue;
        }
        const [s, bindings] = inferPattern(ctx, env, field2.pattern, fieldType, currentSubst);
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
          const elemType = freshTypeVar(ctx);
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
        const keys1 = new Set(firstBindings.keys());
        const keys2 = new Set(altBindings.keys());
        const diff1 = [...keys1].filter((k) => !keys2.has(k));
        const diff2 = [...keys2].filter((k) => !keys1.has(k));
        if (diff1.length > 0 || diff2.length > 0) {
          const missing = diff1.length > 0 ? `missing ${diff1.join(", ")}` : "";
          const extra = diff2.length > 0 ? `extra ${diff2.join(", ")}` : "";
          const message = [`Or-pattern alternatives must bind the same variables.`, missing, extra].filter(Boolean).join(" ");
          addError(ctx, message);
          continue;
        }
        for (const [name, type1] of firstBindings) {
          const type2 = altBindings.get(name);
          const s3 = unify(ctx, applySubst(currentSubst, type1), applySubst(currentSubst, type2));
          currentSubst = composeSubst(currentSubst, s3);
        }
      }
      return [currentSubst, firstBindings];
    }
  }
};
var inferMatch = (ctx, env, registry, expr) => {
  if (expr.cases.length === 0) {
    addError(ctx, "Match expression must have at least one case");
    return [new Map, freshTypeVar(ctx), []];
  }
  const [s1, scrutineeType, c1] = inferExpr(ctx, env, registry, expr.expr);
  let subst = s1;
  let constraints = [...c1];
  let resultType = null;
  for (const case_2 of expr.cases) {
    const [s2, bindings] = inferPattern(ctx, applySubstEnv(subst, env), case_2.pattern, applySubst(subst, scrutineeType), subst);
    subst = s2;
    const caseEnv = new Map(applySubstEnv(subst, env));
    for (const [name, type] of bindings) {
      caseEnv.set(name, scheme([], applySubst(subst, type)));
    }
    if (case_2.guard) {
      const [sg, guardType, cg] = inferExpr(ctx, caseEnv, registry, case_2.guard);
      subst = composeSubst(subst, sg);
      constraints = [...applySubstConstraints(subst, constraints), ...cg];
      const s4 = unify(ctx, applySubst(subst, guardType), tBool);
      subst = composeSubst(subst, s4);
    }
    const [s3, bodyType, c2] = inferExpr(ctx, caseEnv, registry, case_2.body);
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
  const lastCase = expr.cases[expr.cases.length - 1];
  let isExhaustive = false;
  if (lastCase) {
    const isWildcardPattern = (p) => {
      if (p.kind === "PVar" || p.kind === "PWildcard")
        return true;
      if (p.kind === "PAs")
        return isWildcardPattern(p.pattern);
      return false;
    };
    if (isWildcardPattern(lastCase.pattern) && !lastCase.guard) {
      isExhaustive = true;
    }
    if (isWildcardPattern(lastCase.pattern) && lastCase.guard?.kind === "Bool" && lastCase.guard.value === true) {
      isExhaustive = true;
    }
  }
  if (!isExhaustive) {
    const patterns = expr.cases.filter((c) => !c.guard).map((c) => c.pattern);
    const missing = checkExhaustiveness(env, registry, applySubst(subst, scrutineeType), patterns);
    if (missing.length > 0) {
      addError(ctx, `Non-exhaustive patterns. Missing: ${missing.join(", ")}`);
    }
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
var instantiateTypeExpr = (ctx, texpr) => {
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
          fresh = freshTypeVar(ctx);
          varMapping.set(t.name, fresh);
        }
        return fresh;
      }
    }
  };
  return convert(texpr);
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
var getConstructorArity = (env, conName) => {
  const scheme2 = env.get(conName);
  if (!scheme2)
    return 0;
  let arity = 0;
  let t = scheme2.type;
  while (t.kind === "TFun") {
    arity++;
    t = t.ret;
  }
  return arity;
};
var getConstructorsWithArity = (env, registry, typeName) => {
  const conNames = registry.get(typeName);
  if (!conNames)
    return [];
  return conNames.map((name) => ({
    name,
    arity: getConstructorArity(env, name)
  }));
};
var extractTypeArgs = (type) => {
  const args = [];
  let current = type;
  while (current.kind === "TApp") {
    args.unshift(current.arg);
    current = current.con;
  }
  return args;
};
var getConstructorArgTypes = (env, conName, scrutineeType) => {
  const scheme2 = env.get(conName);
  if (!scheme2)
    return [];
  const typeArgs = extractTypeArgs(scrutineeType);
  const subst = new Map;
  for (let i = 0;i < scheme2.vars.length && i < typeArgs.length; i++) {
    subst.set(scheme2.vars[i], typeArgs[i]);
  }
  const argTypes = [];
  let t = scheme2.type;
  while (t.kind === "TFun") {
    argTypes.push(applySubst(subst, t.param));
    t = t.ret;
  }
  return argTypes;
};
var toSimplePattern = (p) => {
  switch (p.kind) {
    case "PWildcard":
    case "PVar":
      return { kind: "Wild" };
    case "PCon":
      return { kind: "Con", name: p.name, args: p.args.map(toSimplePattern) };
    case "QualifiedPCon":
      return {
        kind: "Con",
        name: p.constructor,
        args: p.args.map(toSimplePattern)
      };
    case "PTuple":
      return { kind: "Tuple", elements: p.elements.map(toSimplePattern) };
    case "PLit":
      return { kind: "Lit", value: p.value };
    case "PAs":
      return toSimplePattern(p.pattern);
    case "POr":
      return { kind: "Or", alternatives: p.alternatives.map(toSimplePattern) };
    case "PRecord":
      return { kind: "Wild" };
  }
};
var simplePatternToString = (p) => {
  switch (p.kind) {
    case "Wild":
      return "_";
    case "Con":
      if (p.args.length === 0)
        return p.name;
      return `${p.name} ${p.args.map((a) => {
        const s = simplePatternToString(a);
        return a.kind === "Con" && a.args.length > 0 ? `(${s})` : s;
      }).join(" ")}`;
    case "Tuple":
      return `(${p.elements.map(simplePatternToString).join(", ")})`;
    case "Lit":
      return typeof p.value === "string" ? `"${p.value}"` : String(p.value);
    case "Or":
      return p.alternatives.map(simplePatternToString).join(" | ");
  }
};
var specialize = (matrix, conName, arity) => {
  const result = [];
  for (const row of matrix) {
    if (row.length === 0)
      continue;
    const first = row[0];
    const rest = row.slice(1);
    switch (first.kind) {
      case "Con":
        if (first.name === conName) {
          result.push([...first.args, ...rest]);
        }
        break;
      case "Wild":
        result.push([...Array(arity).fill({ kind: "Wild" }), ...rest]);
        break;
      case "Or":
        for (const alt of first.alternatives) {
          const expanded = specialize([[alt, ...rest]], conName, arity);
          result.push(...expanded);
        }
        break;
      case "Tuple":
      case "Lit":
        break;
    }
  }
  return result;
};
var specializeTuple = (matrix, arity) => {
  const result = [];
  for (const row of matrix) {
    if (row.length === 0)
      continue;
    const first = row[0];
    const rest = row.slice(1);
    switch (first.kind) {
      case "Tuple":
        if (first.elements.length === arity) {
          result.push([...first.elements, ...rest]);
        }
        break;
      case "Wild":
        result.push([...Array(arity).fill({ kind: "Wild" }), ...rest]);
        break;
      case "Or":
        for (const alt of first.alternatives) {
          const expanded = specializeTuple([[alt, ...rest]], arity);
          result.push(...expanded);
        }
        break;
      default:
        break;
    }
  }
  return result;
};
var defaultMatrix = (matrix) => {
  const result = [];
  for (const row of matrix) {
    if (row.length === 0)
      continue;
    const first = row[0];
    const rest = row.slice(1);
    switch (first.kind) {
      case "Wild":
        result.push(rest);
        break;
      case "Or":
        for (const alt of first.alternatives) {
          const expanded = defaultMatrix([[alt, ...rest]]);
          result.push(...expanded);
        }
        break;
      default:
        break;
    }
  }
  return result;
};
var findWitness = (env, registry, matrix, types, depth = 0) => {
  const MAX_DEPTH = 10;
  if (depth > MAX_DEPTH) {
    if (matrix.length === 0 && types.length > 0) {
      return types.map(() => ({ kind: "Wild" }));
    }
    return null;
  }
  if (types.length === 0) {
    return matrix.length === 0 ? [] : null;
  }
  const firstType = types[0];
  const restTypes = types.slice(1);
  const typeName = getTypeConName(firstType);
  if (firstType.kind === "TTuple") {
    const arity = firstType.elements.length;
    const specialized = specializeTuple(matrix, arity);
    const witness2 = findWitness(env, registry, specialized, [...firstType.elements, ...restTypes], depth + 1);
    if (witness2) {
      const tupleElements = witness2.slice(0, arity);
      const rest = witness2.slice(arity);
      return [{ kind: "Tuple", elements: tupleElements }, ...rest];
    }
    return null;
  }
  if (typeName) {
    const constructors = getConstructorsWithArity(env, registry, typeName);
    if (constructors.length === 0) {
      const def2 = defaultMatrix(matrix);
      const witness2 = findWitness(env, registry, def2, restTypes, depth);
      if (witness2) {
        return [{ kind: "Wild" }, ...witness2];
      }
      return null;
    }
    for (const con of constructors) {
      const specialized = specialize(matrix, con.name, con.arity);
      const argTypes = getConstructorArgTypes(env, con.name, firstType);
      const finalArgTypes = argTypes.length > 0 ? argTypes : Array(con.arity).fill({ kind: "TVar", name: "_" });
      const witness2 = findWitness(env, registry, specialized, [...finalArgTypes, ...restTypes], depth + 1);
      if (witness2) {
        const args = witness2.slice(0, con.arity);
        const rest = witness2.slice(con.arity);
        return [{ kind: "Con", name: con.name, args }, ...rest];
      }
    }
    return null;
  }
  const def = defaultMatrix(matrix);
  const witness = findWitness(env, registry, def, restTypes, depth);
  if (witness) {
    return [{ kind: "Wild" }, ...witness];
  }
  return null;
};
var checkExhaustiveness = (env, registry, scrutineeType, patterns) => {
  const matrix = patterns.map((p) => [toSimplePattern(p)]);
  const witness = findWitness(env, registry, matrix, [scrutineeType]);
  if (witness && witness.length > 0) {
    return [simplePatternToString(witness[0])];
  }
  return [];
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
var processModule = (mod, baseEnv, baseRegistry) => {
  const { typeEnv: dataEnv, registry, constructorNames } = processDeclarations(mod.declarations);
  const env = new Map(baseEnv);
  for (const [k, v] of dataEnv)
    env.set(k, v);
  const fullRegistry = new Map(baseRegistry);
  for (const [k, v] of registry)
    fullRegistry.set(k, v);
  if (mod.bindings.length > 0) {
    const emptySymbols = { definitions: [], references: [] };
    const ctx = createContext2(emptySymbols);
    const placeholders = new Map;
    const envWithPlaceholders = new Map(env);
    for (const binding of mod.bindings) {
      const placeholder = freshTypeVar(ctx);
      placeholders.set(binding.name, placeholder);
      envWithPlaceholders.set(binding.name, scheme([], placeholder));
    }
    let subst = new Map;
    const valueTypes = new Map;
    for (const binding of mod.bindings) {
      const [s, valueType] = inferExpr(ctx, applySubstEnv(subst, envWithPlaceholders), fullRegistry, binding.value);
      subst = composeSubst(subst, s);
      valueTypes.set(binding.name, valueType);
      const placeholder = applySubst(subst, placeholders.get(binding.name));
      const s2 = unify(ctx, placeholder, valueType);
      subst = composeSubst(subst, s2);
    }
    const env1 = applySubstEnv(subst, env);
    for (const binding of mod.bindings) {
      const valueType = applySubst(subst, valueTypes.get(binding.name));
      const generalizedScheme = generalize(env1, valueType);
      env.set(binding.name, generalizedScheme);
    }
  }
  return { typeEnv: env, registry: fullRegistry, constructorNames };
};
var processModules = (modules) => {
  const result = new Map;
  let accEnv = new Map;
  let accRegistry = new Map;
  for (const mod of modules) {
    const info = processModule(mod, accEnv, accRegistry);
    result.set(mod.name, info);
    for (const [k, v] of info.typeEnv)
      accEnv.set(k, v);
    for (const [k, v] of info.registry)
      accRegistry.set(k, v);
  }
  return result;
};
var processUseStatements = (uses, moduleEnv) => {
  const localEnv = new Map;
  const localRegistry = new Map;
  const constructorNames = [];
  const aliases = new Map;
  for (const use of uses) {
    if (use.alias) {
      aliases.set(use.alias, use.moduleName);
    }
    const mod = moduleEnv.get(use.moduleName);
    if (!mod) {
      continue;
    }
    if (use.imports?.kind === "All") {
      for (const [name, scheme2] of mod.typeEnv) {
        localEnv.set(name, scheme2);
      }
      for (const [typeName, cons] of mod.registry) {
        localRegistry.set(typeName, cons);
      }
      constructorNames.push(...mod.constructorNames);
    } else if (use.imports?.kind === "Specific") {
      for (const item of use.imports.items) {
        const scheme2 = mod.typeEnv.get(item.name);
        if (scheme2) {
          localEnv.set(item.name, scheme2);
        }
        if (item.constructors) {
          const cons = mod.registry.get(item.name);
          if (cons) {
            if (item.constructors === "all") {
              localRegistry.set(item.name, cons);
              for (const conName of cons) {
                const conScheme = mod.typeEnv.get(conName);
                if (conScheme) {
                  localEnv.set(conName, conScheme);
                  constructorNames.push(conName);
                }
              }
            } else {
              for (const conName of item.constructors) {
                const conScheme = mod.typeEnv.get(conName);
                if (conScheme) {
                  localEnv.set(conName, conScheme);
                  constructorNames.push(conName);
                }
              }
            }
          }
        }
      }
    }
  }
  return { localEnv, localRegistry, constructorNames, aliases };
};
var baseEnv = new Map;

// src/eval.ts
var assertNever = (x) => {
  throw new Error(`Unexpected value: ${JSON.stringify(x)}`);
};
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
      if (!value) {
        throw new RuntimeError(`Undefined variable: ${expr.name}`);
      }
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
      for (const field2 of expr.fields) {
        fields.set(field2.name, evaluate(env, field2.value));
      }
      return vrecord(fields);
    }
    case "FieldAccess": {
      const record2 = evaluate(env, expr.record);
      const value = record2.fields.get(expr.field);
      if (value === undefined) {
        throw new RuntimeError(`Record has no field '${expr.field}'`);
      }
      return value;
    }
    case "TupleIndex": {
      const tuple2 = evaluate(env, expr.tuple);
      const value = tuple2.elements[expr.index];
      if (value === undefined) {
        throw new RuntimeError(`Tuple index ${expr.index} out of bounds`);
      }
      return value;
    }
    case "QualifiedVar": {
      const value = env.get(expr.member);
      if (!value) {
        throw new RuntimeError(`Qualified access (${expr.moduleName}.${expr.member}) requires importing the module. ` + `Add 'use ${expr.moduleName} (..)' to import all bindings.`);
      }
      if (value.kind === "VRef") {
        if (value.value === null) {
          throw new RuntimeError(`Uninitialized recursive binding: ${expr.member}`);
        }
        return value.value;
      }
      return value;
    }
    case "Match":
      return evalMatch(env, expr);
    default:
      return assertNever(expr);
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
      for (let i = 0;i < a.args.length; i++) {
        if (!valuesEqual(a.args[i], bCon.args[i]))
          return false;
      }
      return true;
    }
    case "VTuple": {
      const bTuple = b;
      if (a.elements.length !== bTuple.elements.length)
        return false;
      for (let i = 0;i < a.elements.length; i++) {
        if (!valuesEqual(a.elements[i], bTuple.elements[i]))
          return false;
      }
      return true;
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
    case "QualifiedPCon": {
      if (value.kind !== "VCon" || value.name !== pattern.constructor) {
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
      for (const field2 of pattern.fields) {
        const fieldValue = value.fields.get(field2.name);
        if (fieldValue === undefined)
          return { matched: false };
        const result = matchPattern(field2.pattern, fieldValue);
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
    default:
      return assertNever(pattern);
  }
};
var evalMatch = (env, expr) => {
  const scrutinee = evaluate(env, expr.expr);
  for (const case_2 of expr.cases) {
    const result = matchPattern(case_2.pattern, scrutinee);
    if (result.matched) {
      let caseEnv = env;
      for (const [name, value] of result.bindings) {
        caseEnv = extendEnv(caseEnv, name, value);
      }
      if (case_2.guard) {
        const guardResult = evaluate(caseEnv, case_2.guard);
        if (!guardResult.value) {
          continue;
        }
      }
      return evaluate(caseEnv, case_2.body);
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
  TokenKind2[TokenKind2["End"] = 12] = "End";
  TokenKind2[TokenKind2["Type"] = 13] = "Type";
  TokenKind2[TokenKind2["When"] = 14] = "When";
  TokenKind2[TokenKind2["True"] = 15] = "True";
  TokenKind2[TokenKind2["False"] = 16] = "False";
  TokenKind2[TokenKind2["As"] = 17] = "As";
  TokenKind2[TokenKind2["AndKw"] = 18] = "AndKw";
  TokenKind2[TokenKind2["Module"] = 19] = "Module";
  TokenKind2[TokenKind2["Use"] = 20] = "Use";
  TokenKind2[TokenKind2["Plus"] = 21] = "Plus";
  TokenKind2[TokenKind2["Minus"] = 22] = "Minus";
  TokenKind2[TokenKind2["Star"] = 23] = "Star";
  TokenKind2[TokenKind2["Slash"] = 24] = "Slash";
  TokenKind2[TokenKind2["Lt"] = 25] = "Lt";
  TokenKind2[TokenKind2["Le"] = 26] = "Le";
  TokenKind2[TokenKind2["Gt"] = 27] = "Gt";
  TokenKind2[TokenKind2["Ge"] = 28] = "Ge";
  TokenKind2[TokenKind2["EqEq"] = 29] = "EqEq";
  TokenKind2[TokenKind2["Ne"] = 30] = "Ne";
  TokenKind2[TokenKind2["Pipe"] = 31] = "Pipe";
  TokenKind2[TokenKind2["Arrow"] = 32] = "Arrow";
  TokenKind2[TokenKind2["Eq"] = 33] = "Eq";
  TokenKind2[TokenKind2["Bar"] = 34] = "Bar";
  TokenKind2[TokenKind2["Comma"] = 35] = "Comma";
  TokenKind2[TokenKind2["Dot"] = 36] = "Dot";
  TokenKind2[TokenKind2["Underscore"] = 37] = "Underscore";
  TokenKind2[TokenKind2["And"] = 38] = "And";
  TokenKind2[TokenKind2["Or"] = 39] = "Or";
  TokenKind2[TokenKind2["Colon"] = 40] = "Colon";
  TokenKind2[TokenKind2["ColonColon"] = 41] = "ColonColon";
  TokenKind2[TokenKind2["LParen"] = 42] = "LParen";
  TokenKind2[TokenKind2["RParen"] = 43] = "RParen";
  TokenKind2[TokenKind2["LBrace"] = 44] = "LBrace";
  TokenKind2[TokenKind2["RBrace"] = 45] = "RBrace";
  TokenKind2[TokenKind2["LBracket"] = 46] = "LBracket";
  TokenKind2[TokenKind2["RBracket"] = 47] = "RBracket";
  TokenKind2[TokenKind2["Error"] = 48] = "Error";
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
  ["end", 12 /* End */],
  ["type", 13 /* Type */],
  ["when", 14 /* When */],
  ["true", 15 /* True */],
  ["false", 16 /* False */],
  ["as", 17 /* As */],
  ["and", 18 /* AndKw */],
  ["module", 19 /* Module */],
  ["use", 20 /* Use */]
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
    return [48 /* Error */, start, state.pos];
  }
  advance(state);
  return [2 /* String */, start, state.pos];
};
var scanLowerOrKeyword = (state, start) => {
  if (peek(state) === UNDERSCORE && !isIdentContinue(peekAt(state, 1))) {
    advance(state);
    return [37 /* Underscore */, start, state.pos];
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
      return [21 /* Plus */, start, state.pos];
    case MINUS:
      if (peek(state) === GT) {
        advance(state);
        return [32 /* Arrow */, start, state.pos];
      }
      return [22 /* Minus */, start, state.pos];
    case STAR:
      return [23 /* Star */, start, state.pos];
    case SLASH:
      return [24 /* Slash */, start, state.pos];
    case LT:
      if (peek(state) === EQ) {
        advance(state);
        return [26 /* Le */, start, state.pos];
      }
      return [25 /* Lt */, start, state.pos];
    case GT:
      if (peek(state) === EQ) {
        advance(state);
        return [28 /* Ge */, start, state.pos];
      }
      return [27 /* Gt */, start, state.pos];
    case EQ:
      if (peek(state) === EQ) {
        advance(state);
        return [29 /* EqEq */, start, state.pos];
      }
      return [33 /* Eq */, start, state.pos];
    case BANG:
      if (peek(state) === EQ) {
        advance(state);
        return [30 /* Ne */, start, state.pos];
      }
      return [48 /* Error */, start, state.pos];
    case PIPE:
      if (peek(state) === GT) {
        advance(state);
        return [31 /* Pipe */, start, state.pos];
      }
      if (peek(state) === PIPE) {
        advance(state);
        return [39 /* Or */, start, state.pos];
      }
      return [34 /* Bar */, start, state.pos];
    case AMPERSAND:
      if (peek(state) === AMPERSAND) {
        advance(state);
        return [38 /* And */, start, state.pos];
      }
      return [48 /* Error */, start, state.pos];
    case COMMA:
      return [35 /* Comma */, start, state.pos];
    case DOT:
      return [36 /* Dot */, start, state.pos];
    case LPAREN:
      return [42 /* LParen */, start, state.pos];
    case RPAREN:
      return [43 /* RParen */, start, state.pos];
    case LBRACE:
      return [44 /* LBrace */, start, state.pos];
    case RBRACE:
      return [45 /* RBrace */, start, state.pos];
    case LBRACKET:
      return [46 /* LBracket */, start, state.pos];
    case RBRACKET:
      return [47 /* RBracket */, start, state.pos];
    case COLON:
      if (peek(state) === COLON) {
        advance(state);
        return [41 /* ColonColon */, start, state.pos];
      }
      return [40 /* Colon */, start, state.pos];
    default:
      return [48 /* Error */, start, state.pos];
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
var atNewStatement = (state) => state.lexer.atLineStart && !at(state, 34 /* Bar */);
var isLambdaStart = (state) => {
  const savedPos = state.lexer.pos;
  const savedCurrent = state.current;
  while (at(state, 3 /* Lower */)) {
    advance2(state);
  }
  const isLambda = at(state, 32 /* Arrow */);
  state.lexer.pos = savedPos;
  state.current = savedCurrent;
  return isLambda;
};
var parseLambda = (state) => {
  const start = state.current[1];
  const params = [];
  while (at(state, 3 /* Lower */) && !at(state, 32 /* Arrow */)) {
    const token = advance2(state);
    params.push({ name: text(state, token), span: tokenSpan(token) });
  }
  expect(state, 32 /* Arrow */, "expected '->'");
  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];
  let result = body;
  for (let i = params.length - 1;i >= 0; i--) {
    const p = params[i];
    const absStart = i === 0 ? start : p.span.start;
    result = abs(p.name, result, span(absStart, end), p.span);
  }
  return result;
};
var synchronize = (state) => {
  while (!at(state, 0 /* Eof */)) {
    if (atAny(state, 5 /* Let */, 13 /* Type */, 8 /* If */, 11 /* Match */, 12 /* End */)) {
      return;
    }
    advance2(state);
  }
};
var parseParams = (state) => {
  const params = [];
  while (at(state, 3 /* Lower */) || at(state, 42 /* LParen */)) {
    if (at(state, 42 /* LParen */)) {
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
      expect(state, 43 /* RParen */, "expected ')' after parameter");
      params.push({ name: paramName, span: tokenSpan(paramToken), type: paramType });
    } else {
      const paramToken = advance2(state);
      params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
    }
  }
  return params;
};
var wrapInLambdas = (params, body) => {
  let result = body;
  for (let i = params.length - 1;i >= 0; i--) {
    const p = params[i];
    result = abs(p.name, result, undefined, p.span, p.type);
  }
  return result;
};
var parse = (source) => {
  const state = createParser(source);
  const modules = [];
  const uses = [];
  const declarations = [];
  const bindings = [];
  let expr = null;
  while (at(state, 19 /* Module */)) {
    const mod = parseModuleDecl(state);
    if (mod)
      modules.push(mod);
  }
  while (at(state, 20 /* Use */)) {
    const use = parseUseDecl(state);
    if (use)
      uses.push(use);
  }
  while (!at(state, 0 /* Eof */)) {
    if (at(state, 13 /* Type */)) {
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
    program: { modules, uses, declarations, bindings, expr },
    diagnostics: state.diagnostics
  };
};
var parseLetBindingOrExpr = (state) => {
  const start = state.current[1];
  advance2(state);
  const recursive = at(state, 6 /* Rec */);
  if (recursive)
    advance2(state);
  if (!recursive && atAny(state, 42 /* LParen */, 44 /* LBrace */, 37 /* Underscore */, 4 /* Upper */)) {
    const pattern = parsePattern(state);
    expect(state, 33 /* Eq */, "expected '=' after pattern");
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
  const params = parseParams(state);
  let returnType;
  if (at(state, 40 /* Colon */)) {
    advance2(state);
    returnType = parseType(state) ?? undefined;
  }
  if (!expect(state, 33 /* Eq */, "expected '=' after parameters")) {
    synchronize(state);
    return { kind: "expr", expr: num(0) };
  }
  const body = parseExpr(state);
  if (at(state, 7 /* In */) || recursive && at(state, 18 /* AndKw */)) {
    const value = wrapInLambdas(params, body);
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
  if (!expect(state, 33 /* Eq */, "expected '=' after type parameters")) {
    synchronize(state);
    return null;
  }
  const constructors = [];
  const first = parseConstructor(state);
  if (first)
    constructors.push(first);
  while (at(state, 34 /* Bar */)) {
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
  while (atAny(state, 3 /* Lower */, 4 /* Upper */, 42 /* LParen */) && !atNewStatement(state)) {
    const field2 = parseTypeAtomSimple(state);
    if (field2)
      fields.push(field2);
    else
      break;
  }
  return conDecl(name, fields);
};
var parseModuleDecl = (state) => {
  const start = state.current[1];
  advance2(state);
  const nameToken = expect(state, 4 /* Upper */, "expected module name");
  if (!nameToken) {
    synchronize(state);
    return null;
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const declarations = [];
  const bindings = [];
  while (!at(state, 12 /* End */) && !at(state, 0 /* Eof */)) {
    if (at(state, 13 /* Type */)) {
      const decl = parseDataDecl(state);
      if (decl)
        declarations.push(decl);
    } else if (at(state, 5 /* Let */)) {
      const binding = parseModuleBinding(state);
      if (binding)
        bindings.push(binding);
    } else {
      error2(state, "expected 'data' or 'let' declaration in module");
      advance2(state);
    }
  }
  const endToken = expect(state, 12 /* End */, "expected 'end' after module body");
  const end = endToken ? endToken[2] : state.current[1];
  return moduleDecl(name, declarations, bindings, span(start, end), nameSpan);
};
var parseModuleBinding = (state) => {
  advance2(state);
  const recursive = at(state, 6 /* Rec */);
  if (recursive)
    advance2(state);
  const nameToken = expect(state, 3 /* Lower */, "expected binding name");
  if (!nameToken) {
    synchronize(state);
    return null;
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = parseParams(state);
  let returnType;
  if (at(state, 40 /* Colon */)) {
    advance2(state);
    returnType = parseType(state) ?? undefined;
  }
  expect(state, 33 /* Eq */, "expected '=' after parameters");
  const value = wrapInLambdas(params, parseExpr(state));
  return recBinding(name, value, nameSpan, returnType);
};
var parseUseDecl = (state) => {
  const start = state.current[1];
  advance2(state);
  const moduleToken = expect(state, 4 /* Upper */, "expected module name");
  if (!moduleToken) {
    synchronize(state);
    return null;
  }
  const moduleName = text(state, moduleToken);
  const moduleSpan = tokenSpan(moduleToken);
  let imports = null;
  let alias;
  let aliasSpan;
  if (at(state, 42 /* LParen */)) {
    advance2(state);
    if (at(state, 36 /* Dot */)) {
      advance2(state);
      if (at(state, 36 /* Dot */)) {
        advance2(state);
        imports = importAll();
      } else {
        error2(state, "expected '..' for import all");
      }
      expect(state, 43 /* RParen */, "expected ')' after '..'");
    } else {
      const items = parseImportItems(state);
      imports = importSpecific(items);
      expect(state, 43 /* RParen */, "expected ')' after import list");
    }
  }
  if (at(state, 17 /* As */)) {
    advance2(state);
    const aliasToken = expect(state, 4 /* Upper */, "expected alias name");
    if (aliasToken) {
      alias = text(state, aliasToken);
      aliasSpan = tokenSpan(aliasToken);
    }
  }
  const end = state.current[1];
  return useDecl(moduleName, imports, alias, span(start, end), moduleSpan, aliasSpan);
};
var parseImportItems = (state) => {
  const items = [];
  do {
    if (at(state, 35 /* Comma */))
      advance2(state);
    if (at(state, 4 /* Upper */)) {
      const nameToken = advance2(state);
      const name = text(state, nameToken);
      const nameSpan = tokenSpan(nameToken);
      let constructors;
      if (at(state, 42 /* LParen */)) {
        advance2(state);
        if (at(state, 36 /* Dot */)) {
          advance2(state);
          if (at(state, 36 /* Dot */)) {
            advance2(state);
            constructors = "all";
          } else {
            error2(state, "expected '..' for all constructors");
          }
        } else {
          const cons = [];
          do {
            if (at(state, 35 /* Comma */))
              advance2(state);
            const conToken = expect(state, 4 /* Upper */, "expected constructor name");
            if (conToken)
              cons.push(text(state, conToken));
          } while (at(state, 35 /* Comma */));
          constructors = cons;
        }
        expect(state, 43 /* RParen */, "expected ')' after constructors");
      }
      items.push(importItem(name, constructors, nameSpan));
    } else if (at(state, 3 /* Lower */)) {
      const nameToken = advance2(state);
      items.push(importItem(text(state, nameToken), undefined, tokenSpan(nameToken)));
    } else {
      break;
    }
  } while (at(state, 35 /* Comma */));
  return items;
};
var parseType = (state) => {
  const left = parseTypeAtom(state);
  if (!left)
    return null;
  if (at(state, 32 /* Arrow */)) {
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
    while (atAny(state, 3 /* Lower */, 4 /* Upper */, 42 /* LParen */) && !atNewStatement(state)) {
      const arg = parseTypeAtomSimple(state);
      if (arg) {
        type = tyapp(type, arg);
      } else {
        break;
      }
    }
    return type;
  }
  if (at(state, 42 /* LParen */)) {
    advance2(state);
    const inner = parseTypeAtom(state);
    expect(state, 43 /* RParen */, "expected ')' after type");
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
  if (at(state, 42 /* LParen */)) {
    advance2(state);
    const inner = parseTypeAtom(state);
    expect(state, 43 /* RParen */, "expected ')' after type");
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
      if (isLambdaStart(state)) {
        return parseLambda(state);
      }
      advance2(state);
      const name = text(state, token);
      return var_(name, tokenSpan(token));
    }
    case 4 /* Upper */: {
      advance2(state);
      return var_(text(state, token), tokenSpan(token));
    }
    case 42 /* LParen */:
      return parseParenOrTuple(state);
    case 22 /* Minus */: {
      advance2(state);
      const operand = parsePrecedence(state, 50 /* Multiplicative */ + 1);
      const end = operand.span?.end ?? state.current[1];
      return binOp("-", num(0, span(start, start)), operand, span(start, end));
    }
    case 44 /* LBrace */:
      return parseRecord(state);
    case 46 /* LBracket */:
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
    case 21 /* Plus */:
      return binOp2("+");
    case 22 /* Minus */:
      return binOp2("-");
    case 23 /* Star */:
      return binOp2("*");
    case 24 /* Slash */:
      return binOp2("/");
    case 25 /* Lt */:
      return binOp2("<");
    case 26 /* Le */:
      return binOp2("<=");
    case 27 /* Gt */:
      return binOp2(">");
    case 28 /* Ge */:
      return binOp2(">=");
    case 29 /* EqEq */:
      return binOp2("==");
    case 30 /* Ne */:
      return binOp2("!=");
    case 38 /* And */: {
      advance2(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return if_(left, right, bool(false), span(start, end));
    }
    case 39 /* Or */: {
      advance2(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return if_(left, bool(true), right, span(start, end));
    }
    case 31 /* Pipe */: {
      advance2(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return app(right, left, span(start, end));
    }
    case 41 /* ColonColon */: {
      advance2(state);
      const right = parsePrecedence(state, bp - 1);
      const end = right.span?.end ?? state.current[1];
      const cons = var_("Cons");
      return app(app(cons, left), right, span(start, end));
    }
    case 36 /* Dot */: {
      advance2(state);
      if (left.kind === "Var" && left.name.length > 0 && left.name[0].toUpperCase() === left.name[0]) {
        if (at(state, 3 /* Lower */) || at(state, 4 /* Upper */)) {
          const memberToken = advance2(state);
          const member = text(state, memberToken);
          const memberSpan = tokenSpan(memberToken);
          const end2 = memberToken[2];
          return qualifiedVar(left.name, member, span(start, end2), left.span, memberSpan);
        }
      }
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
    case 31 /* Pipe */:
      return 10 /* Pipe */;
    case 41 /* ColonColon */:
      return 11 /* Cons */;
    case 39 /* Or */:
      return 12 /* Or */;
    case 38 /* And */:
      return 14 /* And */;
    case 29 /* EqEq */:
    case 30 /* Ne */:
      return 20 /* Equality */;
    case 25 /* Lt */:
    case 26 /* Le */:
    case 27 /* Gt */:
    case 28 /* Ge */:
      return 30 /* Comparison */;
    case 21 /* Plus */:
    case 22 /* Minus */:
      return 40 /* Additive */;
    case 23 /* Star */:
    case 24 /* Slash */:
      return 50 /* Multiplicative */;
    case 36 /* Dot */:
      return 70 /* FieldAccess */;
    case 3 /* Lower */:
    case 4 /* Upper */:
    case 1 /* Number */:
    case 2 /* String */:
    case 15 /* True */:
    case 16 /* False */:
    case 42 /* LParen */:
    case 44 /* LBrace */:
    case 46 /* LBracket */:
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
  if (at(state, 43 /* RParen */)) {
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
      expect(state, 43 /* RParen */, "expected ')' after type");
      if (at(state, 32 /* Arrow */)) {
        advance2(state);
        const body = parseExpr(state);
        const end = body.span?.end ?? state.current[1];
        return abs(text(state, nameToken), body, span(start, end), tokenSpan(nameToken), paramType ?? undefined);
      }
      error2(state, "expected '->' after annotated parameter");
      return num(0);
    }
    state.lexer.pos = savedPos;
    state.current = savedCurrent;
  }
  const first = parseExpr(state);
  if (at(state, 35 /* Comma */)) {
    const elements = [first];
    while (at(state, 35 /* Comma */)) {
      advance2(state);
      elements.push(parseExpr(state));
    }
    const endToken = expect(state, 43 /* RParen */, "expected ')' after tuple");
    const end = endToken ? endToken[2] : state.current[1];
    return tuple(elements, span(start, end));
  }
  expect(state, 43 /* RParen */, "expected ')' after expression");
  return first;
};
var parseRecord = (state) => {
  const start = state.current[1];
  advance2(state);
  const fields = [];
  if (!at(state, 45 /* RBrace */)) {
    do {
      if (at(state, 35 /* Comma */))
        advance2(state);
      const nameToken = expect(state, 3 /* Lower */, "expected field name");
      if (!nameToken)
        break;
      const name = text(state, nameToken);
      const fieldStart = nameToken[1];
      const fieldEnd = nameToken[2];
      if (at(state, 33 /* Eq */)) {
        advance2(state);
        const value = parseExpr(state);
        const valueEnd = value.span?.end ?? state.current[1];
        fields.push(field(name, value, span(fieldStart, valueEnd)));
      } else {
        const value = var_(name, span(fieldStart, fieldEnd));
        fields.push(field(name, value, span(fieldStart, fieldEnd)));
      }
    } while (at(state, 35 /* Comma */));
  }
  const endToken = expect(state, 45 /* RBrace */, "expected '}' after record");
  const end = endToken ? endToken[2] : state.current[1];
  return record(fields, span(start, end));
};
var parseListLiteral = (state) => {
  const start = state.current[1];
  advance2(state);
  const elements = [];
  if (!at(state, 47 /* RBracket */)) {
    do {
      if (at(state, 35 /* Comma */))
        advance2(state);
      elements.push(parseExpr(state));
    } while (at(state, 35 /* Comma */));
  }
  const endToken = expect(state, 47 /* RBracket */, "expected ']' after list");
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
  const cases = [];
  while (at(state, 14 /* When */)) {
    const caseStart = state.current[1];
    advance2(state);
    let pattern = parsePattern(state);
    if (at(state, 34 /* Bar */)) {
      const alternatives = [pattern];
      while (at(state, 34 /* Bar */)) {
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
    expect(state, 32 /* Arrow */, "expected '->' after pattern");
    const body = parseExpr(state);
    const caseEnd = body.span?.end ?? state.current[1];
    cases.push(case_(pattern, body, guard, span(caseStart, caseEnd)));
  }
  const endToken = expect(state, 12 /* End */, "expected 'end' after match cases");
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
  const params = parseParams(state);
  let returnType;
  if (at(state, 40 /* Colon */)) {
    advance2(state);
    returnType = parseType(state) ?? undefined;
  }
  expect(state, 33 /* Eq */, "expected '=' after name");
  const value = wrapInLambdas(params, parseExpr(state));
  return recBinding(name, value, nameSpan, returnType);
};
var parseLetExpr = (state) => {
  const start = state.current[1];
  advance2(state);
  const recursive = at(state, 6 /* Rec */);
  if (recursive)
    advance2(state);
  if (!recursive && atAny(state, 42 /* LParen */, 44 /* LBrace */, 37 /* Underscore */, 4 /* Upper */)) {
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
  const params = parseParams(state);
  let returnType;
  if (at(state, 40 /* Colon */)) {
    advance2(state);
    returnType = parseType(state) ?? undefined;
  }
  expect(state, 33 /* Eq */, "expected '=' after name");
  const value = wrapInLambdas(params, parseExpr(state));
  expect(state, 7 /* In */, "expected 'in' after let value");
  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];
  return let_(name, value, body, span(start, end), nameSpan, returnType);
};
var parseLetDestructuring = (state, start) => {
  const pattern = parsePattern(state);
  expect(state, 33 /* Eq */, "expected '=' after pattern");
  const value = parseExpr(state);
  expect(state, 7 /* In */, "expected 'in' after let value");
  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];
  return match(value, [case_(pattern, body)], span(start, end));
};
var PATTERN_STARTS = new Set([
  37 /* Underscore */,
  3 /* Lower */,
  4 /* Upper */,
  1 /* Number */,
  2 /* String */,
  15 /* True */,
  16 /* False */,
  42 /* LParen */,
  44 /* LBrace */
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
    case 37 /* Underscore */:
      advance2(state);
      return pwildcard(tokenSpan(token));
    case 3 /* Lower */:
      advance2(state);
      return pvar(text(state, token), tokenSpan(token));
    case 4 /* Upper */: {
      advance2(state);
      const name = text(state, token);
      const nameSpan = tokenSpan(token);
      if (at(state, 36 /* Dot */)) {
        advance2(state);
        const conToken = expect(state, 4 /* Upper */, "expected constructor name after '.'");
        if (!conToken)
          return pwildcard();
        const conName = text(state, conToken);
        const conSpan = tokenSpan(conToken);
        if (!allowArgs) {
          const end3 = conToken[2];
          return qualifiedPCon(name, conName, [], span(start, end3), nameSpan, conSpan);
        }
        const args2 = [];
        while (PATTERN_STARTS.has(state.current[0])) {
          args2.push(parsePatternCore(state, false));
        }
        const end2 = args2[args2.length - 1]?.span?.end ?? conToken[2];
        return qualifiedPCon(name, conName, args2, span(start, end2), nameSpan, conSpan);
      }
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
    case 42 /* LParen */:
      return parseTuplePattern(state);
    case 44 /* LBrace */:
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
  if (at(state, 43 /* RParen */)) {
    advance2(state);
    error2(state, "empty pattern");
    return pwildcard();
  }
  const first = parsePattern(state);
  if (at(state, 35 /* Comma */)) {
    const elements = [first];
    while (at(state, 35 /* Comma */)) {
      advance2(state);
      elements.push(parsePattern(state));
    }
    const endToken = expect(state, 43 /* RParen */, "expected ')' after tuple pattern");
    const end = endToken ? endToken[2] : state.current[1];
    return ptuple(elements, span(start, end));
  }
  expect(state, 43 /* RParen */, "expected ')' after pattern");
  return first;
};
var parseRecordPattern = (state) => {
  const start = state.current[1];
  advance2(state);
  const fields = [];
  if (!at(state, 45 /* RBrace */)) {
    do {
      if (at(state, 35 /* Comma */))
        advance2(state);
      const nameToken = expect(state, 3 /* Lower */, "expected field name");
      if (!nameToken)
        break;
      const name = text(state, nameToken);
      const fieldStart = nameToken[1];
      const fieldEnd = nameToken[2];
      if (at(state, 33 /* Eq */)) {
        advance2(state);
        const pattern = parsePattern(state);
        const patternEnd = pattern.span?.end ?? state.current[1];
        fields.push(pfield(name, pattern, span(fieldStart, patternEnd)));
      } else {
        const pattern = pvar(name, span(fieldStart, fieldEnd));
        fields.push(pfield(name, pattern, span(fieldStart, fieldEnd)));
      }
    } while (at(state, 35 /* Comma */));
  }
  const endToken = expect(state, 45 /* RBrace */, "expected '}' after record pattern");
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
var programToExpr = (program, modules = [], uses = []) => {
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
  const importedBindings = [];
  const seenModules = new Set;
  for (const use of uses) {
    if (seenModules.has(use.moduleName))
      continue;
    seenModules.add(use.moduleName);
    const mod = modules.find((m) => m.name === use.moduleName);
    if (!mod)
      continue;
    importedBindings.push(...mod.bindings);
  }
  if (importedBindings.length > 0) {
    expr = letRec(importedBindings, expr);
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
var mapExpr = abs("f", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("Nil")),
  case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(var_("Cons"), app(var_("f"), var_("x"))), app(app(var_("map"), var_("f")), var_("rest"))))
])));
var filterExpr = abs("p", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("Nil")),
  case_(pcon("Cons", [pvar("x"), pvar("rest")]), if_(app(var_("p"), var_("x")), app(app(var_("Cons"), var_("x")), app(app(var_("filter"), var_("p")), var_("rest"))), app(app(var_("filter"), var_("p")), var_("rest"))))
])));
var headExpr = abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("Nothing")),
  case_(pcon("Cons", [pvar("x"), pwildcard()]), app(var_("Just"), var_("x")))
]));
var tailExpr = abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("Nothing")),
  case_(pcon("Cons", [pwildcard(), pvar("rest")]), app(var_("Just"), var_("rest")))
]));
var isEmptyExpr = abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), bool(true)),
  case_(pcon("Cons", [pwildcard(), pwildcard()]), bool(false))
]));
var lengthExpr = abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), num(0)),
  case_(pcon("Cons", [pwildcard(), pvar("rest")]), binOp("+", num(1), app(var_("length"), var_("rest"))))
]));
var foldrExpr = abs("f", abs("z", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("z")),
  case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(var_("f"), var_("x")), app(app(app(var_("foldr"), var_("f")), var_("z")), var_("rest"))))
]))));
var foldlExpr = abs("f", abs("z", abs("xs", match(var_("xs"), [
  case_(pcon("Nil", []), var_("z")),
  case_(pcon("Cons", [pvar("x"), pvar("rest")]), app(app(app(var_("foldl"), var_("f")), app(app(var_("f"), var_("z")), var_("x"))), var_("rest")))
]))));
var reverseExpr = abs("xs", app(app(app(var_("foldl"), abs("acc", abs("x", app(app(var_("Cons"), var_("x")), var_("acc"))))), var_("Nil")), var_("xs")));
var concatExpr = abs("xs", abs("ys", app(app(app(var_("foldr"), var_("Cons")), var_("ys")), var_("xs"))));
var idExpr = abs("x", var_("x"));
var constExpr = abs("x", abs("_", var_("x")));
var composeExpr = abs("f", abs("g", abs("x", app(var_("f"), app(var_("g"), var_("x"))))));
var flipExpr = abs("f", abs("a", abs("b", app(app(var_("f"), var_("b")), var_("a")))));
var maybeModule = moduleDecl("Maybe", [maybe], []);
var eitherModule = moduleDecl("Either", [either], []);
var listModule = moduleDecl("List", [list], [
  recBinding("map", mapExpr),
  recBinding("filter", filterExpr),
  recBinding("head", headExpr),
  recBinding("tail", tailExpr),
  recBinding("isEmpty", isEmptyExpr),
  recBinding("length", lengthExpr),
  recBinding("foldr", foldrExpr),
  recBinding("foldl", foldlExpr),
  recBinding("reverse", reverseExpr),
  recBinding("concat", concatExpr)
]);
var coreModule = moduleDecl("Core", [], [
  recBinding("id", idExpr),
  recBinding("const", constExpr),
  recBinding("compose", composeExpr),
  recBinding("flip", flipExpr)
]);
var modules = [maybeModule, eitherModule, listModule, coreModule];

// src/ir.ts
var irLit = (value, type) => ({
  kind: "IRLit",
  value,
  type
});
var irVar = (name, type) => ({
  kind: "IRVar",
  name,
  type
});
var irAtomExpr = (atom) => ({
  kind: "IRAtomExpr",
  atom,
  type: atom.type
});
var irLet = (name, binding, body) => ({
  kind: "IRLet",
  name,
  binding,
  body,
  type: body.type
});
var irRecBinding = (name, binding) => ({
  name,
  binding
});
var irLetRec = (bindings, body) => ({
  kind: "IRLetRec",
  bindings,
  body,
  type: body.type
});
var irAtomBinding = (atom) => ({
  kind: "IRAtomBinding",
  atom,
  type: atom.type
});
var irAppBinding = (func, arg, type) => ({
  kind: "IRAppBinding",
  func,
  arg,
  type
});
var irBinOpBinding = (op, left, right, operandType, type) => ({
  kind: "IRBinOpBinding",
  op,
  left,
  right,
  operandType,
  type
});
var irIfBinding = (cond, thenBranch, elseBranch, type) => ({
  kind: "IRIfBinding",
  cond,
  thenBranch,
  elseBranch,
  type
});
var irTupleBinding = (elements, type) => ({
  kind: "IRTupleBinding",
  elements,
  type
});
var irRecordBinding = (fields, type) => ({
  kind: "IRRecordBinding",
  fields,
  type
});
var irRecordField = (name, value) => ({
  name,
  value
});
var irFieldAccessBinding = (record2, field2, type) => ({
  kind: "IRFieldAccessBinding",
  record: record2,
  field: field2,
  type
});
var irTupleIndexBinding = (tuple2, index, type) => ({
  kind: "IRTupleIndexBinding",
  tuple: tuple2,
  index,
  type
});
var irMatchBinding = (scrutinee, cases, type) => ({
  kind: "IRMatchBinding",
  scrutinee,
  cases,
  type
});
var irLambdaBinding = (param, paramType, body, type, tailRecursive) => ({
  kind: "IRLambdaBinding",
  param,
  paramType,
  body,
  type,
  tailRecursive
});
var irPVar = (name, type) => ({
  kind: "IRPVar",
  name,
  type
});
var irPWildcard = (type) => ({
  kind: "IRPWildcard",
  type
});
var irPCon = (name, args, type) => ({
  kind: "IRPCon",
  name,
  args,
  type
});
var irPLit = (value, type) => ({
  kind: "IRPLit",
  value,
  type
});
var irPTuple = (elements, type) => ({
  kind: "IRPTuple",
  elements,
  type
});
var irPRecord = (fields, type) => ({
  kind: "IRPRecord",
  fields,
  type
});
var irPRecordField = (name, pattern) => ({
  name,
  pattern
});
var irPAs = (pattern, name, type) => ({
  kind: "IRPAs",
  pattern,
  name,
  type
});
var irPOr = (alternatives, type) => ({
  kind: "IRPOr",
  alternatives,
  type
});
var irCase = (pattern, body, guard) => ({
  pattern,
  guard,
  body
});
var printIR = (expr, indent = 0) => {
  const pad = "  ".repeat(indent);
  switch (expr.kind) {
    case "IRAtomExpr":
      return pad + printAtom(expr.atom);
    case "IRLet":
      return pad + `let ${expr.name} = ${printBinding(expr.binding, indent)}
` + printIR(expr.body, indent);
    case "IRLetRec": {
      const bindings = expr.bindings.map((b) => `${pad}  ${b.name} = ${printBinding(b.binding, indent + 1)}`).join(`
`);
      return `${pad}let rec
${bindings}
${pad}in
${printIR(expr.body, indent)}`;
    }
  }
};
var printAtom = (atom) => {
  switch (atom.kind) {
    case "IRLit":
      return typeof atom.value === "string" ? `"${atom.value}"` : String(atom.value);
    case "IRVar":
      return atom.name;
  }
};
var printBinding = (binding, indent) => {
  const pad = "  ".repeat(indent);
  switch (binding.kind) {
    case "IRAtomBinding":
      return printAtom(binding.atom);
    case "IRAppBinding":
      return `${printAtom(binding.func)} ${printAtom(binding.arg)}`;
    case "IRBinOpBinding":
      return `${printAtom(binding.left)} ${binding.op} ${printAtom(binding.right)}`;
    case "IRIfBinding":
      return `if ${printAtom(binding.cond)} then
` + printIR(binding.thenBranch, indent + 1) + `
${pad}else
` + printIR(binding.elseBranch, indent + 1);
    case "IRTupleBinding":
      return `(${binding.elements.map(printAtom).join(", ")})`;
    case "IRRecordBinding":
      return `{ ${binding.fields.map((f) => `${f.name} = ${printAtom(f.value)}`).join(", ")} }`;
    case "IRFieldAccessBinding":
      return `${printAtom(binding.record)}.${binding.field}`;
    case "IRTupleIndexBinding":
      return `${printAtom(binding.tuple)}.${binding.index}`;
    case "IRMatchBinding": {
      const cases = binding.cases.map((c) => {
        const guard = c.guard ? ` if ${printIR(c.guard, 0).trim()}` : "";
        return `${pad}  | ${printPattern(c.pattern)}${guard} -> ${printIR(c.body, indent + 2).trim()}`;
      }).join(`
`);
      return `match ${printAtom(binding.scrutinee)}
${cases}`;
    }
    case "IRLambdaBinding":
      return `\\${binding.param} -> ${printIR(binding.body, indent).trim()}`;
    case "IRClosureBinding":
      return `closure(${binding.funcId}, [${binding.captures.map(printAtom).join(", ")}])`;
  }
};
var printPattern = (pattern) => {
  switch (pattern.kind) {
    case "IRPVar":
      return pattern.name;
    case "IRPWildcard":
      return "_";
    case "IRPLit":
      return typeof pattern.value === "string" ? `"${pattern.value}"` : String(pattern.value);
    case "IRPCon":
      if (pattern.args.length === 0)
        return pattern.name;
      return `${pattern.name} ${pattern.args.map(printPattern).join(" ")}`;
    case "IRPTuple":
      return `(${pattern.elements.map(printPattern).join(", ")})`;
    case "IRPRecord":
      return `{ ${pattern.fields.map((f) => `${f.name} = ${printPattern(f.pattern)}`).join(", ")} }`;
    case "IRPAs":
      return `${printPattern(pattern.pattern)} as ${pattern.name}`;
    case "IRPOr":
      return pattern.alternatives.map(printPattern).join(" | ");
  }
};

// src/lower.ts
var assertNever2 = (x) => {
  throw new Error(`Unexpected value: ${JSON.stringify(x)}`);
};
var createContext3 = (typeEnv, checkOutput) => ({
  varCounter: 0,
  typeEnv: new Map(typeEnv),
  subst: checkOutput.subst,
  spanTypes: checkOutput.spanTypes
});
var freshVar = (ctx, prefix = "_t") => {
  return `${prefix}${ctx.varCounter++}`;
};
var extendEnv2 = (ctx, name, type) => {
  ctx.typeEnv.set(name, { vars: [], constraints: [], type });
};
var lookupSpanType = (ctx, span2) => {
  if (!span2)
    return null;
  const key = `${span2.start}:${span2.end}`;
  const type = ctx.spanTypes.get(key);
  return type ? applySubst(ctx.subst, type) : null;
};
var lookupType = (ctx, name) => {
  const scheme2 = ctx.typeEnv.get(name);
  if (!scheme2) {
    throw new Error(`Unknown variable during lowering: ${name}`);
  }
  return applySubst(ctx.subst, scheme2.type);
};
var getReturnType = (type) => {
  if (type.kind === "TFun") {
    return type.ret;
  }
  return { kind: "TVar", name: "_return" };
};
var getFieldType = (type, field2) => {
  if (type.kind === "TRecord") {
    const fieldType = type.fields.get(field2);
    if (fieldType) {
      return fieldType;
    }
  }
  throw new Error(`Expected record type with field ${field2}, got ${type.kind}`);
};
var tNum2 = { kind: "TCon", name: "number" };
var tStr2 = { kind: "TCon", name: "string" };
var tBool2 = { kind: "TCon", name: "boolean" };
var normalize = (ctx, expr) => {
  switch (expr.kind) {
    case "Num":
      return { bindings: [], atom: irLit(expr.value, tNum2) };
    case "Str":
      return { bindings: [], atom: irLit(expr.value, tStr2) };
    case "Bool":
      return { bindings: [], atom: irLit(expr.value, tBool2) };
    case "Var": {
      const type2 = lookupType(ctx, expr.name);
      return { bindings: [], atom: irVar(expr.name, type2) };
    }
  }
  const lowered = lowerExpr(ctx, expr);
  if (lowered.kind === "IRAtomExpr") {
    return { bindings: [], atom: lowered.atom };
  }
  const name = freshVar(ctx);
  const type = lowered.type;
  extendEnv2(ctx, name, type);
  const { bindings, finalBinding } = extractBindings(lowered);
  return {
    bindings: [...bindings, { name, binding: finalBinding }],
    atom: irVar(name, type)
  };
};
var extractBindings = (expr) => {
  const bindings = [];
  let current = expr;
  while (current.kind === "IRLet" || current.kind === "IRLetRec") {
    if (current.kind === "IRLet") {
      bindings.push({ name: current.name, binding: current.binding });
    } else {
      for (const b of current.bindings) {
        bindings.push({ name: b.name, binding: b.binding });
      }
    }
    current = current.body;
  }
  if (current.kind !== "IRAtomExpr") {
    throw new Error("Expected atom at end of ANF expression");
  }
  if (bindings.length > 0) {
    const lastBinding = bindings.pop();
    return { bindings, finalBinding: lastBinding.binding };
  }
  return { bindings: [], finalBinding: irAtomBinding(current.atom) };
};
var normalizeMany = (ctx, exprs) => {
  const allBindings = [];
  const atoms = [];
  for (const expr of exprs) {
    const result = normalize(ctx, expr);
    allBindings.push(...result.bindings);
    atoms.push(result.atom);
  }
  return { bindings: allBindings, atoms };
};
var wrapWithBindings = (bindings, body) => {
  let result = body;
  for (let i = bindings.length - 1;i >= 0; i--) {
    const { name, binding } = bindings[i];
    result = irLet(name, binding, result);
  }
  return result;
};
var lowerExpr = (ctx, expr) => {
  switch (expr.kind) {
    case "Num":
      return irAtomExpr(irLit(expr.value, tNum2));
    case "Str":
      return irAtomExpr(irLit(expr.value, tStr2));
    case "Bool":
      return irAtomExpr(irLit(expr.value, tBool2));
    case "Var": {
      const type = lookupType(ctx, expr.name);
      return irAtomExpr(irVar(expr.name, type));
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
    case "QualifiedVar": {
      const scheme2 = ctx.typeEnv.get(expr.member);
      if (scheme2) {
        return irAtomExpr(irVar(expr.member, applySubst(ctx.subst, scheme2.type)));
      }
      throw new Error(`Qualified access (${expr.moduleName}.${expr.member}) requires importing the module. ` + `Add 'use ${expr.moduleName} (..)' to import all bindings.`);
    }
    default:
      return assertNever2(expr);
  }
};
var lowerLet = (ctx, expr) => {
  const valueIR = lowerExpr(ctx, expr.value);
  const { bindings, finalBinding } = extractBindings(valueIR);
  extendEnv2(ctx, expr.name, finalBinding.type);
  const bodyIR = lowerExpr(ctx, expr.body);
  const letExpr = irLet(expr.name, finalBinding, bodyIR);
  return wrapWithBindings(bindings, letExpr);
};
var lowerLetRec = (ctx, expr) => {
  for (const binding of expr.bindings) {
    const placeholderType = { kind: "TVar", name: `_rec_${binding.name}` };
    extendEnv2(ctx, binding.name, placeholderType);
  }
  const irBindings = [];
  const allPreBindings = [];
  for (const binding of expr.bindings) {
    const valueIR = lowerExpr(ctx, binding.value);
    const { bindings: preBindings, finalBinding } = extractBindings(valueIR);
    for (const b of preBindings) {
      allPreBindings.push(b);
    }
    irBindings.push(irRecBinding(binding.name, finalBinding));
    extendEnv2(ctx, binding.name, finalBinding.type);
  }
  const bodyIR = lowerExpr(ctx, expr.body);
  const letRecExpr = irLetRec(irBindings, bodyIR);
  return wrapWithBindings(allPreBindings, letRecExpr);
};
var lowerAbs = (ctx, expr) => {
  const paramType = lookupSpanType(ctx, expr.paramSpan) ?? {
    kind: "TVar",
    name: `_param${ctx.varCounter++}`
  };
  const savedEnv = new Map(ctx.typeEnv);
  extendEnv2(ctx, expr.param, paramType);
  const bodyIR = lowerExpr(ctx, expr.body);
  ctx.typeEnv = savedEnv;
  const funcType = { kind: "TFun", param: paramType, ret: bodyIR.type };
  const binding = irLambdaBinding(expr.param, paramType, bodyIR, funcType);
  const name = freshVar(ctx, "_fn");
  extendEnv2(ctx, name, funcType);
  return irLet(name, binding, irAtomExpr(irVar(name, funcType)));
};
var lowerApp = (ctx, expr) => {
  const funcResult = normalize(ctx, expr.func);
  const argResult = normalize(ctx, expr.param);
  const funcType = funcResult.atom.type;
  const returnType = getReturnType(funcType);
  const binding = irAppBinding(funcResult.atom, argResult.atom, returnType);
  const name = freshVar(ctx);
  extendEnv2(ctx, name, returnType);
  const result = irLet(name, binding, irAtomExpr(irVar(name, returnType)));
  return wrapWithBindings([...funcResult.bindings, ...argResult.bindings], result);
};
var lowerBinOp = (ctx, expr) => {
  const leftResult = normalize(ctx, expr.left);
  const rightResult = normalize(ctx, expr.right);
  const operandType = leftResult.atom.type;
  let resultType;
  switch (expr.op) {
    case "+":
      resultType = operandType;
      break;
    case "-":
    case "*":
    case "/":
      resultType = tNum2;
      break;
    case "<":
    case "<=":
    case ">":
    case ">=":
    case "==":
    case "!=":
      resultType = tBool2;
      break;
  }
  const binding = irBinOpBinding(expr.op, leftResult.atom, rightResult.atom, operandType, resultType);
  const name = freshVar(ctx);
  extendEnv2(ctx, name, resultType);
  const result = irLet(name, binding, irAtomExpr(irVar(name, resultType)));
  return wrapWithBindings([...leftResult.bindings, ...rightResult.bindings], result);
};
var lowerIf = (ctx, expr) => {
  const condResult = normalize(ctx, expr.cond);
  const thenIR = lowerExpr(ctx, expr.then);
  const elseIR = lowerExpr(ctx, expr.else);
  const resultType = thenIR.type;
  const binding = irIfBinding(condResult.atom, thenIR, elseIR, resultType);
  const name = freshVar(ctx);
  extendEnv2(ctx, name, resultType);
  const result = irLet(name, binding, irAtomExpr(irVar(name, resultType)));
  return wrapWithBindings(condResult.bindings, result);
};
var lowerTuple = (ctx, expr) => {
  if (expr.elements.length === 1) {
    return lowerExpr(ctx, expr.elements[0]);
  }
  const { bindings, atoms } = normalizeMany(ctx, expr.elements);
  const elementTypes = atoms.map((a) => a.type);
  const tupleType = { kind: "TTuple", elements: elementTypes };
  const binding = irTupleBinding(atoms, tupleType);
  const name = freshVar(ctx);
  extendEnv2(ctx, name, tupleType);
  const result = irLet(name, binding, irAtomExpr(irVar(name, tupleType)));
  return wrapWithBindings(bindings, result);
};
var lowerRecord = (ctx, expr) => {
  const fieldExprs = expr.fields.map((f) => f.value);
  const { bindings, atoms } = normalizeMany(ctx, fieldExprs);
  const irFields = expr.fields.map((f, i) => irRecordField(f.name, atoms[i]));
  const fieldTypes = new Map;
  for (let i = 0;i < expr.fields.length; i++) {
    fieldTypes.set(expr.fields[i].name, atoms[i].type);
  }
  const recordType2 = { kind: "TRecord", fields: fieldTypes, row: null };
  const binding = irRecordBinding(irFields, recordType2);
  const name = freshVar(ctx);
  extendEnv2(ctx, name, recordType2);
  const result = irLet(name, binding, irAtomExpr(irVar(name, recordType2)));
  return wrapWithBindings(bindings, result);
};
var lowerFieldAccess = (ctx, expr) => {
  const recordResult = normalize(ctx, expr.record);
  const fieldType = getFieldType(recordResult.atom.type, expr.field);
  const binding = irFieldAccessBinding(recordResult.atom, expr.field, fieldType);
  const name = freshVar(ctx);
  extendEnv2(ctx, name, fieldType);
  const result = irLet(name, binding, irAtomExpr(irVar(name, fieldType)));
  return wrapWithBindings(recordResult.bindings, result);
};
var lowerTupleIndex = (ctx, expr) => {
  const tupleResult = normalize(ctx, expr.tuple);
  const tupleType = tupleResult.atom.type;
  if (tupleType.kind !== "TTuple") {
    throw new Error(`Expected tuple type, got ${tupleType.kind}`);
  }
  const elementType = tupleType.elements[expr.index];
  const binding = irTupleIndexBinding(tupleResult.atom, expr.index, elementType);
  const name = freshVar(ctx);
  extendEnv2(ctx, name, elementType);
  const result = irLet(name, binding, irAtomExpr(irVar(name, elementType)));
  return wrapWithBindings(tupleResult.bindings, result);
};
var lowerMatch = (ctx, expr) => {
  const scrutineeResult = normalize(ctx, expr.expr);
  const irCases = [];
  let resultType = null;
  for (const case_2 of expr.cases) {
    const irPattern = lowerPattern(case_2.pattern, scrutineeResult.atom.type);
    const savedEnv = new Map(ctx.typeEnv);
    extendPatternBindings(ctx, case_2.pattern, scrutineeResult.atom.type);
    let guardIR;
    if (case_2.guard) {
      guardIR = lowerExpr(ctx, case_2.guard);
    }
    const bodyIR = lowerExpr(ctx, case_2.body);
    resultType = bodyIR.type;
    ctx.typeEnv = savedEnv;
    irCases.push(irCase(irPattern, bodyIR, guardIR));
  }
  if (!resultType) {
    throw new Error("Match expression has no cases");
  }
  const binding = irMatchBinding(scrutineeResult.atom, irCases, resultType);
  const name = freshVar(ctx);
  extendEnv2(ctx, name, resultType);
  const result = irLet(name, binding, irAtomExpr(irVar(name, resultType)));
  return wrapWithBindings(scrutineeResult.bindings, result);
};
var lowerPattern = (pattern, type) => {
  switch (pattern.kind) {
    case "PVar":
      return irPVar(pattern.name, type);
    case "PWildcard":
      return irPWildcard(type);
    case "PLit": {
      let litType;
      if (typeof pattern.value === "number") {
        litType = tNum2;
      } else if (typeof pattern.value === "string") {
        litType = tStr2;
      } else {
        litType = tBool2;
      }
      return irPLit(pattern.value, litType);
    }
    case "PCon": {
      const args = pattern.args.map((arg, i) => {
        const argType = { kind: "TVar", name: `_arg${i}` };
        return lowerPattern(arg, argType);
      });
      return irPCon(pattern.name, args, type);
    }
    case "QualifiedPCon": {
      const args = pattern.args.map((arg, i) => {
        const argType = { kind: "TVar", name: `_arg${i}` };
        return lowerPattern(arg, argType);
      });
      return irPCon(pattern.constructor, args, type);
    }
    case "PTuple": {
      const elementTypes = type.kind === "TTuple" ? type.elements : [];
      const elements = pattern.elements.map((elem, i) => {
        const elemType = elementTypes[i] ?? { kind: "TVar", name: `_elem${i}` };
        return lowerPattern(elem, elemType);
      });
      return irPTuple(elements, type);
    }
    case "PRecord": {
      const fields = pattern.fields.map((f) => {
        const fieldType = type.kind === "TRecord" ? type.fields.get(f.name) ?? { kind: "TVar", name: `_field_${f.name}` } : { kind: "TVar", name: `_field_${f.name}` };
        return irPRecordField(f.name, lowerPattern(f.pattern, fieldType));
      });
      return irPRecord(fields, type);
    }
    case "PAs": {
      const innerPattern = lowerPattern(pattern.pattern, type);
      return irPAs(innerPattern, pattern.name, type);
    }
    case "POr": {
      const alternatives = pattern.alternatives.map((alt) => lowerPattern(alt, type));
      return irPOr(alternatives, type);
    }
  }
};
var extendPatternBindings = (ctx, pattern, type) => {
  switch (pattern.kind) {
    case "PVar":
      extendEnv2(ctx, pattern.name, type);
      break;
    case "PWildcard":
    case "PLit":
      break;
    case "PCon":
      for (let i = 0;i < pattern.args.length; i++) {
        const argType = { kind: "TVar", name: `_arg${i}` };
        extendPatternBindings(ctx, pattern.args[i], argType);
      }
      break;
    case "QualifiedPCon":
      for (let i = 0;i < pattern.args.length; i++) {
        const argType = { kind: "TVar", name: `_arg${i}` };
        extendPatternBindings(ctx, pattern.args[i], argType);
      }
      break;
    case "PTuple": {
      const elementTypes = type.kind === "TTuple" ? type.elements : [];
      for (let i = 0;i < pattern.elements.length; i++) {
        const elemType = elementTypes[i] ?? { kind: "TVar", name: `_elem${i}` };
        extendPatternBindings(ctx, pattern.elements[i], elemType);
      }
      break;
    }
    case "PRecord":
      for (const f of pattern.fields) {
        const fieldType = type.kind === "TRecord" ? type.fields.get(f.name) ?? { kind: "TVar", name: `_field_${f.name}` } : { kind: "TVar", name: `_field_${f.name}` };
        extendPatternBindings(ctx, f.pattern, fieldType);
      }
      break;
    case "PAs":
      extendEnv2(ctx, pattern.name, type);
      extendPatternBindings(ctx, pattern.pattern, type);
      break;
    case "POr":
      if (pattern.alternatives.length > 0) {
        extendPatternBindings(ctx, pattern.alternatives[0], type);
      }
      break;
  }
};
var lowerToIR = (expr, typeEnv, checkOutput) => {
  const ctx = createContext3(typeEnv, checkOutput);
  return lowerExpr(ctx, expr);
};

// src/backend/runtime.ts
var RUNTIME = `// Algow Runtime
"use strict";

/**
 * Apply a function to an argument.
 * Handles both closures and partial constructor application.
 */
const $apply = (fn, arg) => {
  if (typeof fn === "function") {
    return fn(arg);
  }
  // Constructor - partial application
  return { $tag: fn.$tag, $args: [...fn.$args, arg] };
};

/**
 * Create a constructor value.
 */
const $con = (tag, ...args) => ({ $tag: tag, $args: args });

/**
 * Deep structural equality for values.
 */
const $eq = (a, b) => {
  // Same reference or primitive equality
  if (a === b) return true;

  // Different types
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object" || a === null) return false;

  // Constructor equality
  if ("$tag" in a && "$tag" in b) {
    if (a.$tag !== b.$tag) return false;
    if (a.$args.length !== b.$args.length) return false;
    return a.$args.every((x, i) => $eq(x, b.$args[i]));
  }

  // Array (tuple) equality
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((x, i) => $eq(x, b[i]));
  }

  // Object (record) equality
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every(k => k in b && $eq(a[k], b[k]));
};

`;

// src/backend/js.ts
var assertNever3 = (x) => {
  throw new Error(`Unexpected value: ${JSON.stringify(x)}`);
};
var createContext4 = (constructorNames) => ({
  indent: 0,
  lines: [],
  constructors: new Set(constructorNames)
});
var emit = (ctx, code) => {
  const indentation = "  ".repeat(ctx.indent);
  ctx.lines.push(indentation + code);
};
var toJsId = (name) => {
  return name.replace(/[^a-zA-Z0-9_$]/g, "_");
};
var genExpr = (ctx, expr) => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return genAtom(ctx, expr.atom);
    case "IRLet": {
      const binding = genBinding(ctx, expr.binding);
      const jsName = toJsId(expr.name);
      emit(ctx, `const ${jsName} = ${binding};`);
      return genExpr(ctx, expr.body);
    }
    case "IRLetRec": {
      const hasLambda = expr.bindings.some((b) => b.binding.kind === "IRLambdaBinding");
      if (hasLambda) {
        for (const { name } of expr.bindings) {
          emit(ctx, `let ${toJsId(name)};`);
        }
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `${toJsId(name)} = ${bindingCode};`);
        }
      } else {
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
        }
      }
      return genExpr(ctx, expr.body);
    }
    default:
      return assertNever3(expr);
  }
};
var genAtom = (ctx, atom) => {
  switch (atom.kind) {
    case "IRLit":
      if (typeof atom.value === "string") {
        return JSON.stringify(atom.value);
      }
      return String(atom.value);
    case "IRVar": {
      if (ctx.constructors.has(atom.name)) {
        return `$con("${atom.name}")`;
      }
      return toJsId(atom.name);
    }
    default:
      return assertNever3(atom);
  }
};
var genBinding = (ctx, binding) => {
  switch (binding.kind) {
    case "IRAtomBinding":
      return genAtom(ctx, binding.atom);
    case "IRAppBinding": {
      const func = genAtom(ctx, binding.func);
      const arg = genAtom(ctx, binding.arg);
      if (binding.func.kind === "IRVar" && ctx.constructors.has(binding.func.name)) {
        return `$apply(${func}, ${arg})`;
      }
      return `${func}(${arg})`;
    }
    case "IRBinOpBinding":
      return genBinOp(ctx, binding);
    case "IRIfBinding":
      return genIf(ctx, binding);
    case "IRTupleBinding": {
      const elements = binding.elements.map((e) => genAtom(ctx, e));
      return `[${elements.join(", ")}]`;
    }
    case "IRRecordBinding": {
      const fields = binding.fields.map((f) => `${f.name}: ${genAtom(ctx, f.value)}`);
      return `{ ${fields.join(", ")} }`;
    }
    case "IRFieldAccessBinding": {
      const record2 = genAtom(ctx, binding.record);
      return `${record2}.${binding.field}`;
    }
    case "IRTupleIndexBinding": {
      const tuple2 = genAtom(ctx, binding.tuple);
      return `${tuple2}[${binding.index}]`;
    }
    case "IRMatchBinding":
      return genMatch(ctx, binding);
    case "IRLambdaBinding":
      return genLambda(ctx, binding);
    case "IRClosureBinding": {
      const captures = binding.captures.map((c) => genAtom(ctx, c));
      return `{ $fn: ${binding.funcId}, $env: [${captures.join(", ")}] }`;
    }
    default:
      return assertNever3(binding);
  }
};
var genBinOp = (ctx, binding) => {
  const left = genAtom(ctx, binding.left);
  const right = genAtom(ctx, binding.right);
  switch (binding.op) {
    case "+":
    case "-":
    case "*":
    case "/":
    case "<":
    case ">":
    case "<=":
    case ">=":
      return `(${left} ${binding.op} ${right})`;
    case "==":
      if (isComplexType(binding.operandType)) {
        return `$eq(${left}, ${right})`;
      }
      return `(${left} === ${right})`;
    case "!=":
      if (isComplexType(binding.operandType)) {
        return `!$eq(${left}, ${right})`;
      }
      return `(${left} !== ${right})`;
  }
};
var isComplexType = (type) => {
  switch (type.kind) {
    case "TCon":
      return !["number", "string", "boolean"].includes(type.name);
    case "TVar":
      return true;
    case "TFun":
    case "TApp":
    case "TRecord":
    case "TTuple":
      return true;
  }
};
var genIf = (ctx, binding) => {
  const cond = genAtom(ctx, binding.cond);
  ctx.indent++;
  const thenLines = [];
  const savedLines = ctx.lines;
  ctx.lines = thenLines;
  const thenResult = genExpr(ctx, binding.thenBranch);
  const thenCode = thenLines.length > 0 ? `(() => {
${thenLines.join(`
`)}
${"  ".repeat(ctx.indent - 1)}return ${thenResult};
${"  ".repeat(ctx.indent - 1)}})()` : thenResult;
  const elseLines = [];
  ctx.lines = elseLines;
  const elseResult = genExpr(ctx, binding.elseBranch);
  const elseCode = elseLines.length > 0 ? `(() => {
${elseLines.join(`
`)}
${"  ".repeat(ctx.indent - 1)}return ${elseResult};
${"  ".repeat(ctx.indent - 1)}})()` : elseResult;
  ctx.lines = savedLines;
  ctx.indent--;
  return `(${cond} ? ${thenCode} : ${elseCode})`;
};
var genLambda = (ctx, binding) => {
  if (binding.tailRecursive) {
    return genTailRecursiveLambda(ctx, binding);
  }
  const param = toJsId(binding.param);
  ctx.indent++;
  const bodyLines = [];
  const savedLines = ctx.lines;
  ctx.lines = bodyLines;
  const bodyResult = genExpr(ctx, binding.body);
  ctx.lines = savedLines;
  ctx.indent--;
  if (bodyLines.length === 0) {
    return `(${param}) => ${bodyResult}`;
  }
  const indentation = "  ".repeat(ctx.indent + 1);
  const body = bodyLines.map((l) => indentation + l.trimStart()).join(`
`);
  return `(${param}) => {
${body}
${"  ".repeat(ctx.indent)}return ${bodyResult};
${"  ".repeat(ctx.indent)}}`;
};
var genTailRecursiveLambda = (ctx, binding) => {
  const tco = binding.tailRecursive;
  const params = tco.params.map(toJsId);
  const selfName = tco.selfName;
  let innerBody = binding.body;
  while (innerBody.kind === "IRLet" && innerBody.binding.kind === "IRLambdaBinding") {
    innerBody = innerBody.binding.body;
  }
  ctx.indent++;
  const bodyLines = [];
  const savedLines = ctx.lines;
  ctx.lines = bodyLines;
  const bodyResult = genExprTCO(ctx, innerBody, selfName, params);
  ctx.lines = savedLines;
  ctx.indent--;
  if (params.length === 1) {
    const indentation2 = "  ".repeat(ctx.indent + 1);
    const loopIndent2 = "  ".repeat(ctx.indent + 2);
    const body2 = bodyLines.map((l) => loopIndent2 + l.trimStart()).join(`
`);
    return `(${params[0]}) => {
${indentation2}while (true) {
${body2}
${loopIndent2}return ${bodyResult};
${indentation2}}
${"  ".repeat(ctx.indent)}}`;
  }
  let result = "";
  for (let i = 0;i < params.length - 1; i++) {
    result += `(${params[i]}) => `;
  }
  const lastParam = params[params.length - 1];
  const indentation = "  ".repeat(ctx.indent + 1);
  const loopIndent = "  ".repeat(ctx.indent + 2);
  const body = bodyLines.map((l) => loopIndent + l.trimStart()).join(`
`);
  result += `(${lastParam}) => {
${indentation}while (true) {
${body}
${loopIndent}return ${bodyResult};
${indentation}}
${"  ".repeat(ctx.indent)}}`;
  return result;
};
var genExprTCO = (ctx, expr, selfName, params) => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return genAtom(ctx, expr.atom);
    case "IRLet": {
      const tailCallArgs = extractTailCallArgs(expr, selfName, params.length);
      if (tailCallArgs) {
        emitNonAppBindings(ctx, expr, selfName);
        const assignments = params.map((p, i) => `${p} = ${genAtom(ctx, tailCallArgs[i])};`).join(" ");
        emit(ctx, `${assignments} continue;`);
        return "undefined";
      }
      const binding = genBindingTCO(ctx, expr.binding, selfName, params);
      const jsName = toJsId(expr.name);
      emit(ctx, `const ${jsName} = ${binding};`);
      return genExprTCO(ctx, expr.body, selfName, params);
    }
    case "IRLetRec": {
      const hasLambda = expr.bindings.some((b) => b.binding.kind === "IRLambdaBinding");
      if (hasLambda) {
        for (const { name } of expr.bindings) {
          emit(ctx, `let ${toJsId(name)};`);
        }
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `${toJsId(name)} = ${bindingCode};`);
        }
      } else {
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
        }
      }
      return genExprTCO(ctx, expr.body, selfName, params);
    }
  }
};
var emitNonAppBindings = (ctx, expr, selfName) => {
  let current = expr;
  while (current.kind === "IRLet") {
    const { name, binding } = current;
    if (binding.kind !== "IRAppBinding") {
      const bindingCode = genBinding(ctx, binding);
      emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
    } else if (binding.func.kind === "IRVar" && binding.func.name !== selfName) {
      const isPartialApp = isPartOfTailCallChain(expr, name, selfName);
      if (!isPartialApp) {
        const bindingCode = genBinding(ctx, binding);
        emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
      }
    }
    current = current.body;
  }
};
var isPartOfTailCallChain = (expr, bindingName, selfName) => {
  const bindings = [];
  let current = expr;
  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }
  if (current.kind !== "IRAtomExpr" || current.atom.kind !== "IRVar") {
    return false;
  }
  let targetVar = current.atom.name;
  for (let i = bindings.length - 1;i >= 0; i--) {
    const { name, binding } = bindings[i];
    if (name === targetVar && binding.kind === "IRAppBinding") {
      if (name === bindingName) {
        return true;
      }
      if (binding.func.kind === "IRVar") {
        if (binding.func.name === selfName) {
          return false;
        }
        targetVar = binding.func.name;
      } else {
        return false;
      }
    }
  }
  return false;
};
var extractTailCallArgs = (expr, selfName, paramCount) => {
  const bindings = [];
  let current = expr;
  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }
  if (current.kind !== "IRAtomExpr" || current.atom.kind !== "IRVar") {
    return null;
  }
  const resultVar = current.atom.name;
  const args = [];
  let targetVar = resultVar;
  for (let i = bindings.length - 1;i >= 0; i--) {
    const { name, binding } = bindings[i];
    if (name === targetVar && binding.kind === "IRAppBinding") {
      args.unshift(binding.arg);
      if (binding.func.kind === "IRVar") {
        if (binding.func.name === selfName) {
          if (args.length === paramCount) {
            return args;
          }
          return null;
        }
        targetVar = binding.func.name;
      } else {
        return null;
      }
    }
  }
  return null;
};
var genBindingTCO = (ctx, binding, selfName, params) => {
  switch (binding.kind) {
    case "IRIfBinding":
      return genIfTCO(ctx, binding, selfName, params);
    case "IRMatchBinding":
      return genMatchTCO(ctx, binding, selfName, params);
    default:
      return genBinding(ctx, binding);
  }
};
var genIfTCO = (ctx, binding, selfName, params) => {
  const cond = genAtom(ctx, binding.cond);
  ctx.indent++;
  const thenLines = [];
  const savedLines = ctx.lines;
  ctx.lines = thenLines;
  const thenResult = genExprTCO(ctx, binding.thenBranch, selfName, params);
  const thenHasContinue = thenLines.some((l) => l.includes("continue;"));
  const thenCode = thenHasContinue ? `(() => {
${thenLines.join(`
`)}
${"  ".repeat(ctx.indent - 1)}return ${thenResult};
${"  ".repeat(ctx.indent - 1)}})()` : thenLines.length > 0 ? `(() => {
${thenLines.join(`
`)}
${"  ".repeat(ctx.indent - 1)}return ${thenResult};
${"  ".repeat(ctx.indent - 1)}})()` : thenResult;
  const elseLines = [];
  ctx.lines = elseLines;
  const elseResult = genExprTCO(ctx, binding.elseBranch, selfName, params);
  const elseHasContinue = elseLines.some((l) => l.includes("continue;"));
  const elseCode = elseHasContinue ? `(() => {
${elseLines.join(`
`)}
${"  ".repeat(ctx.indent - 1)}return ${elseResult};
${"  ".repeat(ctx.indent - 1)}})()` : elseLines.length > 0 ? `(() => {
${elseLines.join(`
`)}
${"  ".repeat(ctx.indent - 1)}return ${elseResult};
${"  ".repeat(ctx.indent - 1)}})()` : elseResult;
  ctx.lines = savedLines;
  ctx.indent--;
  if (thenHasContinue || elseHasContinue) {
    emit(ctx, `if (${cond}) {`);
    for (const line of thenLines) {
      emit(ctx, "  " + line.trimStart());
    }
    if (!thenHasContinue) {
      emit(ctx, `  return ${thenResult};`);
    }
    emit(ctx, `} else {`);
    for (const line of elseLines) {
      emit(ctx, "  " + line.trimStart());
    }
    if (!elseHasContinue) {
      emit(ctx, `  return ${elseResult};`);
    }
    emit(ctx, `}`);
    return "undefined";
  }
  return `(${cond} ? ${thenCode} : ${elseCode})`;
};
var genMatchTCO = (ctx, binding, selfName, params) => {
  const scrutinee = genAtom(ctx, binding.scrutinee);
  const scrutineeVar = "_s";
  const hasGuards = binding.cases.some((c) => c.guard !== undefined);
  const caseResults = [];
  for (const case_2 of binding.cases) {
    ctx.indent += 2;
    const bodyLines = [];
    const savedLines = ctx.lines;
    ctx.lines = bodyLines;
    let guardResult;
    if (case_2.guard) {
      guardResult = genExpr(ctx, case_2.guard);
    }
    const bodyResult = genExprTCO(ctx, case_2.body, selfName, params);
    const hasContinue = bodyLines.some((l) => l.includes("continue;"));
    ctx.lines = savedLines;
    ctx.indent -= 2;
    caseResults.push({ case_: case_2, bodyLines, bodyResult, hasContinue, guardResult });
  }
  const anyContinue = caseResults.some((r) => r.hasContinue);
  const lines = [];
  if (anyContinue) {
    emit(ctx, `let ${scrutineeVar} = ${scrutinee};`);
    for (let i = 0;i < caseResults.length; i++) {
      const { case_: case_2, bodyLines, bodyResult, hasContinue, guardResult } = caseResults[i];
      const { condition, bindings } = genPatternMatch(scrutineeVar, case_2.pattern);
      const prefix = hasGuards || i === 0 ? "if" : "} else if";
      const suffix = hasGuards && i > 0 ? "}" : "";
      if (suffix)
        emit(ctx, suffix);
      emit(ctx, `${prefix} (${condition}) {`);
      ctx.indent++;
      for (const [name, expr] of bindings) {
        emit(ctx, `const ${toJsId(name)} = ${expr};`);
      }
      for (const line of bodyLines) {
        emit(ctx, line.trimStart());
      }
      if (guardResult) {
        if (hasContinue) {
          emit(ctx, `if (${guardResult}) { /* continue handled above */ }`);
        } else {
          emit(ctx, `if (${guardResult}) { ${scrutineeVar} = ${bodyResult}; }`);
        }
      } else {
        if (!hasContinue) {
          emit(ctx, `${scrutineeVar} = ${bodyResult};`);
        }
      }
      ctx.indent--;
    }
    emit(ctx, "}");
    return scrutineeVar;
  }
  lines.push(`((${scrutineeVar}) => {`);
  for (let i = 0;i < caseResults.length; i++) {
    const { case_: case_2, bodyLines, bodyResult, guardResult } = caseResults[i];
    const { condition, bindings } = genPatternMatch(scrutineeVar, case_2.pattern);
    const prefix = hasGuards || i === 0 ? "if" : "} else if";
    const suffix = hasGuards && i > 0 ? "}" : "";
    if (suffix)
      lines.push(`  ${suffix}`);
    lines.push(`  ${prefix} (${condition}) {`);
    for (const [name, expr] of bindings) {
      lines.push(`    const ${toJsId(name)} = ${expr};`);
    }
    for (const line of bodyLines) {
      lines.push("    " + line.trimStart());
    }
    if (guardResult) {
      lines.push(`    if (${guardResult}) return ${bodyResult};`);
    } else {
      lines.push(`    return ${bodyResult};`);
    }
  }
  lines.push("  }");
  lines.push(`})(${scrutinee})`);
  return lines.join(`
` + "  ".repeat(ctx.indent));
};
var canUseSwitchDispatch = (cases) => {
  if (cases.length === 0)
    return false;
  const tags = new Set;
  for (const case_2 of cases) {
    if (case_2.pattern.kind !== "IRPCon") {
      return false;
    }
    if (tags.has(case_2.pattern.name)) {
      return false;
    }
    tags.add(case_2.pattern.name);
  }
  return true;
};
var genNestedPatternConditions = (scrutineeVar, pattern) => {
  const conditions = [];
  const bindings = [];
  for (let i = 0;i < pattern.args.length; i++) {
    const argScrutinee = `${scrutineeVar}.$args[${i}]`;
    const result = genPatternMatch(argScrutinee, pattern.args[i]);
    if (result.condition !== "true") {
      conditions.push(result.condition);
    }
    bindings.push(...result.bindings);
  }
  return { conditions, bindings };
};
var genMatchSwitch = (ctx, scrutineeVar, scrutinee, cases) => {
  const lines = [];
  lines.push(`((${scrutineeVar}) => {`);
  lines.push(`  switch (${scrutineeVar}.$tag) {`);
  for (const case_2 of cases) {
    const pattern = case_2.pattern;
    lines.push(`    case "${pattern.name}": {`);
    const { conditions, bindings } = genNestedPatternConditions(scrutineeVar, pattern);
    for (const [name, expr] of bindings) {
      lines.push(`      const ${toJsId(name)} = ${expr};`);
    }
    ctx.indent += 3;
    const bodyLines = [];
    const savedLines = ctx.lines;
    ctx.lines = bodyLines;
    const bodyResult = genExpr(ctx, case_2.body);
    ctx.lines = savedLines;
    ctx.indent -= 3;
    for (const line of bodyLines) {
      lines.push("      " + line.trimStart());
    }
    if (conditions.length > 0) {
      lines.push(`      if (${conditions.join(" && ")}) return ${bodyResult};`);
      lines.push(`      break;`);
    } else {
      lines.push(`      return ${bodyResult};`);
    }
    lines.push(`    }`);
  }
  lines.push(`  }`);
  lines.push(`})(${scrutinee})`);
  return lines.join(`
` + "  ".repeat(ctx.indent));
};
var genMatch = (ctx, binding) => {
  const scrutinee = genAtom(ctx, binding.scrutinee);
  const scrutineeVar = "_s";
  const hasGuards = binding.cases.some((c) => c.guard !== undefined);
  if (!hasGuards && canUseSwitchDispatch(binding.cases)) {
    return genMatchSwitch(ctx, scrutineeVar, scrutinee, binding.cases);
  }
  const lines = [];
  lines.push(`((${scrutineeVar}) => {`);
  for (let i = 0;i < binding.cases.length; i++) {
    const case_2 = binding.cases[i];
    const { condition, bindings } = genPatternMatch(scrutineeVar, case_2.pattern);
    const prefix = hasGuards || i === 0 ? "if" : "} else if";
    const suffix = hasGuards && i > 0 ? "}" : "";
    if (suffix)
      lines.push(`  ${suffix}`);
    lines.push(`  ${prefix} (${condition}) {`);
    for (const [name, expr] of bindings) {
      lines.push(`    const ${toJsId(name)} = ${expr};`);
    }
    ctx.indent += 2;
    const bodyLines = [];
    const savedLines = ctx.lines;
    ctx.lines = bodyLines;
    let guardResult;
    if (case_2.guard) {
      guardResult = genExpr(ctx, case_2.guard);
    }
    const bodyResult = genExpr(ctx, case_2.body);
    ctx.lines = savedLines;
    ctx.indent -= 2;
    for (const line of bodyLines) {
      lines.push("    " + line.trimStart());
    }
    if (guardResult) {
      lines.push(`    if (${guardResult}) return ${bodyResult};`);
    } else {
      lines.push(`    return ${bodyResult};`);
    }
  }
  lines.push("  }");
  lines.push(`})(${scrutinee})`);
  return lines.join(`
` + "  ".repeat(ctx.indent));
};
var genPatternMatch = (scrutinee, pattern) => {
  switch (pattern.kind) {
    case "IRPVar":
      return { condition: "true", bindings: [[pattern.name, scrutinee]] };
    case "IRPWildcard":
      return { condition: "true", bindings: [] };
    case "IRPLit": {
      const value = typeof pattern.value === "string" ? JSON.stringify(pattern.value) : String(pattern.value);
      return { condition: `${scrutinee} === ${value}`, bindings: [] };
    }
    case "IRPCon": {
      const conditions = [`${scrutinee}.$tag === "${pattern.name}"`];
      const bindings = [];
      for (let i = 0;i < pattern.args.length; i++) {
        const argScrutinee = `${scrutinee}.$args[${i}]`;
        const result = genPatternMatch(argScrutinee, pattern.args[i]);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }
      return { condition: conditions.join(" && "), bindings };
    }
    case "IRPTuple": {
      const conditions = [];
      const bindings = [];
      for (let i = 0;i < pattern.elements.length; i++) {
        const elemScrutinee = `${scrutinee}[${i}]`;
        const result = genPatternMatch(elemScrutinee, pattern.elements[i]);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }
      return { condition: conditions.join(" && ") || "true", bindings };
    }
    case "IRPRecord": {
      const conditions = [];
      const bindings = [];
      for (const field2 of pattern.fields) {
        const fieldScrutinee = `${scrutinee}.${field2.name}`;
        const result = genPatternMatch(fieldScrutinee, field2.pattern);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }
      return { condition: conditions.join(" && ") || "true", bindings };
    }
    case "IRPAs": {
      const result = genPatternMatch(scrutinee, pattern.pattern);
      result.bindings.push([pattern.name, scrutinee]);
      return result;
    }
    case "IRPOr": {
      const conditions = [];
      let bindings = [];
      for (let i = 0;i < pattern.alternatives.length; i++) {
        const result = genPatternMatch(scrutinee, pattern.alternatives[i]);
        conditions.push(`(${result.condition})`);
        if (i === 0) {
          bindings = result.bindings;
        }
      }
      return { condition: conditions.join(" || "), bindings };
    }
  }
};
var generateJS = (irExpr, constructorNames) => {
  const ctx = createContext4(constructorNames);
  const result = genExpr(ctx, irExpr);
  const code = [
    RUNTIME,
    "// Generated code",
    ...ctx.lines,
    `const $result = ${result};`,
    "console.log($result);"
  ].join(`
`);
  return { code, warnings: [] };
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
var successResponse = (id, result) => ({
  jsonrpc: "2.0",
  id,
  result
});
var errorResponse = (id, code, message) => ({
  jsonrpc: "2.0",
  id,
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
var preludeUses = modules.map((mod) => useDecl(mod.name, importAll()));
var preludeCache = (() => {
  const moduleEnv = processModules(modules);
  const { localEnv, localRegistry, constructorNames } = processUseStatements(preludeUses, moduleEnv);
  return {
    moduleEnv,
    typeEnv: localEnv,
    registry: localRegistry,
    constructorNames: [...constructorNames]
  };
})();
var processProgram = (program) => {
  const allModules = [...modules, ...program.modules];
  const allUses = [...preludeUses, ...program.uses];
  if (program.modules.length > 0) {
    const moduleEnv = processModules(allModules);
    const { localEnv, localRegistry, constructorNames } = processUseStatements(allUses, moduleEnv);
    return {
      typeEnv: localEnv,
      registry: localRegistry,
      constructorNames,
      allModules,
      allUses,
      moduleEnv
    };
  }
  if (program.uses.length > 0) {
    const { localEnv, localRegistry, constructorNames } = processUseStatements(allUses, preludeCache.moduleEnv);
    return {
      typeEnv: localEnv,
      registry: localRegistry,
      constructorNames,
      allModules,
      allUses,
      moduleEnv: preludeCache.moduleEnv
    };
  }
  return {
    typeEnv: preludeCache.typeEnv,
    registry: preludeCache.registry,
    constructorNames: preludeCache.constructorNames,
    allModules,
    allUses,
    moduleEnv: preludeCache.moduleEnv
  };
};
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
    const { id, method, params } = request;
    try {
      switch (method) {
        case "initialize":
          transport.send(successResponse(id, handleInitialize(params)));
          break;
        case "shutdown":
          transport.send(successResponse(id, null));
          break;
        case "textDocument/hover":
          transport.send(successResponse(id, handleHover(params)));
          break;
        case "textDocument/definition":
          transport.send(successResponse(id, handleDefinition(params)));
          break;
        case "textDocument/prepareRename":
          transport.send(successResponse(id, handlePrepareRename(params)));
          break;
        case "textDocument/rename":
          transport.send(successResponse(id, handleRename(params)));
          break;
        case "textDocument/completion":
          transport.send(successResponse(id, handleCompletion(params)));
          break;
        case "algow/evaluate":
          transport.send(successResponse(id, handleEvaluate(params)));
          break;
        case "algow/compile":
          transport.send(successResponse(id, handleCompile(params)));
          break;
        case "algow/emitIR":
          transport.send(successResponse(id, handleEmitIR(params)));
          break;
        default:
          transport.send(errorResponse(id, -32601, `Method not found: ${method}`));
      }
    } catch (err) {
      transport.send(errorResponse(id, -32603, err.message));
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
  const COMPLETION_KIND_KEYWORD = 14;
  const COMPLETION_KIND_MODULE = 9;
  const handleCompletion = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc)
      return null;
    const offset = positionToOffset(doc.text, params.position);
    const items = [];
    const beforeCursor = doc.text.slice(0, offset);
    const dotMatch = beforeCursor.match(/(\w+)\s*\.\s*$/);
    if (dotMatch) {
      const name = dotMatch[1];
      if (name[0] && name[0] === name[0].toUpperCase() && /[A-Z]/.test(name[0])) {
        const moduleItems = getModuleMemberCompletions(doc, name);
        if (moduleItems.length > 0) {
          items.push(...moduleItems);
        } else {
          const fieldItems = getRecordFieldCompletions(doc, name);
          items.push(...fieldItems);
        }
      } else {
        const fieldItems = getRecordFieldCompletions(doc, name);
        items.push(...fieldItems);
      }
    } else {
      items.push(...getTypeEnvCompletions(doc));
      items.push(...getConstructorCompletions(doc));
      items.push(...getKeywordCompletions());
      items.push(...getModuleCompletions(doc));
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
  const getTypeEnvCompletions = (doc) => {
    const items = [];
    for (const [name, scheme2] of doc.typeEnv) {
      items.push({
        label: name,
        kind: COMPLETION_KIND_FUNCTION,
        detail: typeToString(scheme2.type)
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
  const getKeywordCompletions = () => {
    const keywords2 = [
      "let",
      "rec",
      "in",
      "if",
      "then",
      "else",
      "match",
      "when",
      "end",
      "type",
      "module",
      "use",
      "as",
      "and",
      "true",
      "false"
    ];
    return keywords2.map((kw) => ({
      label: kw,
      kind: COMPLETION_KIND_KEYWORD
    }));
  };
  const getModuleCompletions = (doc) => {
    const items = [];
    for (const moduleName of doc.moduleEnv.keys()) {
      items.push({
        label: moduleName,
        kind: COMPLETION_KIND_MODULE,
        detail: "module"
      });
    }
    return items;
  };
  const getModuleMemberCompletions = (doc, moduleName) => {
    const items = [];
    const moduleInfo = doc.moduleEnv.get(moduleName);
    if (!moduleInfo)
      return items;
    for (const [name, scheme2] of moduleInfo.typeEnv) {
      items.push({
        label: name,
        kind: COMPLETION_KIND_FUNCTION,
        detail: typeToString(scheme2.type)
      });
    }
    for (const [typeName, constructors] of moduleInfo.registry) {
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
  const handleEvaluate = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }
    if (doc.diagnostics.some((d) => d.severity === SEVERITY_ERROR)) {
      return { success: false, error: "Cannot evaluate: document has errors" };
    }
    const expr = programToExpr(doc.program, doc.allModules, doc.allUses);
    if (!expr) {
      return { success: false, error: "No expression to evaluate" };
    }
    try {
      const constructorEnv = createConstructorEnv(doc.constructorNames);
      const result = evaluate(constructorEnv, expr);
      return { success: true, value: valueToString(result) };
    } catch (err) {
      if (err instanceof RuntimeError) {
        return { success: false, error: err.message };
      }
      return { success: false, error: err.message };
    }
  };
  const handleCompile = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }
    if (doc.diagnostics.some((d) => d.severity === SEVERITY_ERROR)) {
      return { success: false, error: "Cannot compile: document has errors" };
    }
    const expr = programToExpr(doc.program, doc.allModules, doc.allUses);
    if (!expr) {
      return { success: false, error: "No expression to compile" };
    }
    try {
      const checkResult = check(doc.typeEnv, doc.registry, expr, doc.symbols, doc.moduleEnv);
      const ir = lowerToIR(expr, doc.typeEnv, checkResult);
      const { code } = generateJS(ir, doc.constructorNames);
      return { success: true, code };
    } catch (err) {
      return { success: false, error: err.message };
    }
  };
  const handleEmitIR = (params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc || !doc.program) {
      return { success: false, error: "No document found" };
    }
    if (doc.diagnostics.some((d) => d.severity === SEVERITY_ERROR)) {
      return { success: false, error: "Cannot emit IR: document has errors" };
    }
    const expr = programToExpr(doc.program, doc.allModules, doc.allUses);
    if (!expr) {
      return { success: false, error: "No expression to compile" };
    }
    try {
      const checkResult = check(doc.typeEnv, doc.registry, expr, doc.symbols, doc.moduleEnv);
      const ir = lowerToIR(expr, doc.typeEnv, checkResult);
      const irText = printIR(ir);
      return { success: true, ir: irText };
    } catch (err) {
      return { success: false, error: err.message };
    }
  };
  const analyzeDocument = (uri, version, text2) => {
    const lspDiagnostics = [];
    const parseResult = parse(text2);
    for (const diag of parseResult.diagnostics) {
      lspDiagnostics.push(convertDiagnostic(text2, diag));
    }
    const { typeEnv, registry, constructorNames, allModules, allUses, moduleEnv } = processProgram(parseResult.program);
    const expr = programToExpr(parseResult.program, allModules, allUses);
    let symbols = null;
    let types = null;
    if (expr) {
      const bindResult = bindWithConstructors(constructorNames, expr);
      symbols = bindResult.symbols;
      for (const diag of bindResult.diagnostics) {
        lspDiagnostics.push(convertDiagnostic(text2, diag));
      }
      const checkResult = check(typeEnv, registry, expr, symbols, moduleEnv);
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
      typeEnv,
      program: parseResult.program,
      registry,
      constructorNames,
      allModules,
      allUses,
      moduleEnv
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
