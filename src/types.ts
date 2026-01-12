// Type System Types (Section 8)
// Internal representation of types during inference

// =============================================================================
// Types (Section 8.1)
// =============================================================================

export type Type =
  | TVar // Type variable (α, β, ...)
  | TCon // Type constructor (Int, String, ...)
  | TApp // Type application (Maybe Int)
  | TFun // Function type (a -> b)
  | TTuple // Tuple type (Int, String)
  | TRecord; // Record type { x : Int }

export type TVar = {
  readonly kind: "TVar";
  readonly name: string;
};

export type TCon = {
  readonly kind: "TCon";
  readonly name: string;
};

export type TApp = {
  readonly kind: "TApp";
  readonly con: Type;
  readonly arg: Type;
};

export type TFun = {
  readonly kind: "TFun";
  readonly param: Type;
  readonly ret: Type;
};

export type TTuple = {
  readonly kind: "TTuple";
  readonly elements: readonly Type[];
};

export type TRecord = {
  readonly kind: "TRecord";
  readonly fields: ReadonlyMap<string, Type>;
  readonly row: Type | null; // Row variable for open records
};

// =============================================================================
// Type Constructors
// =============================================================================

export const tvar = (name: string): TVar => ({ kind: "TVar", name });
export const tcon = (name: string): TCon => ({ kind: "TCon", name });
export const tfun = (param: Type, ret: Type): TFun => ({ kind: "TFun", param, ret });
export const tapp = (con: Type, arg: Type): TApp => ({ kind: "TApp", con, arg });
export const ttuple = (elements: readonly Type[]): TTuple => ({ kind: "TTuple", elements });
export const trecord = (fields: readonly [string, Type][], row: Type | null = null): TRecord => ({
  kind: "TRecord",
  fields: new Map(fields),
  row,
});

// =============================================================================
// Built-in Types
// =============================================================================

// Primitive types (lowercase)
export const tInt: Type = tcon("int");
export const tFloat: Type = tcon("float");
export const tStr: Type = tcon("string");
export const tBool: Type = tcon("bool");
export const tChar: Type = tcon("char");
export const tUnit: Type = tcon("unit");

// =============================================================================
// Type Schemes (Section 8.1)
// =============================================================================

export type Constraint = {
  readonly className: string;
  readonly type: Type;
};

export type Scheme = {
  readonly vars: readonly string[];
  readonly constraints: readonly Constraint[];
  readonly type: Type;
};

export const scheme = (
  vars: readonly string[],
  type: Type,
  constraints: readonly Constraint[] = [],
): Scheme => ({ vars, constraints, type });

// Monomorphic scheme (no quantified variables)
export const mono = (type: Type): Scheme => scheme([], type);

// =============================================================================
// Type Environment (Section 8.2)
// =============================================================================

export type TypeEnv = Map<string, Scheme>;

// =============================================================================
// Substitution (Section 8.3)
// =============================================================================

export type Subst = Map<string, Type>;

export const emptySubst: Subst = new Map();

// Apply a substitution to a type (Section 8.3)
export const applySubst = (subst: Subst, type: Type): Type => {
  switch (type.kind) {
    case "TCon":
      return type;

    case "TVar":
      return subst.get(type.name) ?? type;

    case "TFun":
      return tfun(applySubst(subst, type.param), applySubst(subst, type.ret));

    case "TApp":
      return tapp(applySubst(subst, type.con), applySubst(subst, type.arg));

    case "TTuple":
      return ttuple(type.elements.map((t) => applySubst(subst, t)));

    case "TRecord": {
      const newFields = new Map<string, Type>();
      for (const [name, fieldType] of type.fields) {
        newFields.set(name, applySubst(subst, fieldType));
      }
      const substitutedRow = type.row ? applySubst(subst, type.row) : null;
      // If the row resolves to another record, merge its fields
      if (substitutedRow && substitutedRow.kind === "TRecord") {
        for (const [name, fieldType] of substitutedRow.fields) {
          if (!newFields.has(name)) {
            newFields.set(name, fieldType);
          }
        }
        return { kind: "TRecord", fields: newFields, row: substitutedRow.row };
      }
      return { kind: "TRecord", fields: newFields, row: substitutedRow };
    }
  }
};

// Apply substitution to a scheme
export const applySubstScheme = (subst: Subst, s: Scheme): Scheme => {
  // Remove quantified variables from substitution (they're local to this scheme)
  const filteredSubst = new Map(subst);
  for (const v of s.vars) {
    filteredSubst.delete(v);
  }
  return scheme(
    s.vars,
    applySubst(filteredSubst, s.type),
    s.constraints.map((c) => ({ ...c, type: applySubst(filteredSubst, c.type) })),
  );
};

// Apply substitution to a type environment
export const applySubstEnv = (subst: Subst, env: TypeEnv): TypeEnv => {
  const result = new Map<string, Scheme>();
  for (const [name, s] of env) {
    result.set(name, applySubstScheme(subst, s));
  }
  return result;
};

// Compose two substitutions (Section 8.3)
export const composeSubst = (s1: Subst, s2: Subst): Subst => {
  const result = new Map<string, Type>();
  // Apply s1 to all types in s2
  for (const [name, type] of s2) {
    result.set(name, applySubst(s1, type));
  }
  // Add all bindings from s1 (s1 takes precedence)
  for (const [name, type] of s1) {
    result.set(name, type);
  }
  return result;
};

// =============================================================================
// Free Type Variables (Section 8.6)
// =============================================================================

export const ftv = (type: Type): Set<string> => {
  switch (type.kind) {
    case "TVar":
      return new Set([type.name]);
    case "TCon":
      return new Set();
    case "TFun":
      return new Set([...ftv(type.param), ...ftv(type.ret)]);
    case "TApp":
      return new Set([...ftv(type.con), ...ftv(type.arg)]);
    case "TTuple": {
      const result = new Set<string>();
      for (const t of type.elements) {
        for (const v of ftv(t)) result.add(v);
      }
      return result;
    }
    case "TRecord": {
      const result = new Set<string>();
      for (const t of type.fields.values()) {
        for (const v of ftv(t)) result.add(v);
      }
      if (type.row) {
        for (const v of ftv(type.row)) result.add(v);
      }
      return result;
    }
  }
};

export const ftvScheme = (s: Scheme): Set<string> => {
  const result = ftv(s.type);
  for (const v of s.vars) {
    result.delete(v);
  }
  return result;
};

export const ftvEnv = (env: TypeEnv): Set<string> => {
  const result = new Set<string>();
  for (const s of env.values()) {
    for (const v of ftvScheme(s)) result.add(v);
  }
  return result;
};

// =============================================================================
// Generalization & Instantiation (Section 8.6)
// =============================================================================

let _typeVarCounter = 0;

export const resetTypeVarCounter = (): void => {
  _typeVarCounter = 0;
};

export const freshTypeVar = (): Type => {
  return tvar(`t${_typeVarCounter++}`);
};

// Generalize: create polymorphic scheme (Section 8.6)
export const generalize = (env: TypeEnv, type: Type): Scheme => {
  const envFtv = ftvEnv(env);
  const typeFtv = ftv(type);
  const vars: string[] = [];
  for (const v of typeFtv) {
    if (!envFtv.has(v)) {
      vars.push(v);
    }
  }
  return scheme(vars, type);
};

// Instantiate: replace quantified variables with fresh ones (Section 8.6)
export const instantiate = (s: Scheme): Type => {
  const subst = new Map<string, Type>();
  for (const v of s.vars) {
    subst.set(v, freshTypeVar());
  }
  return applySubst(subst, s.type);
};

// =============================================================================
// Type Classes
// =============================================================================

export const instances: ReadonlyMap<string, ReadonlySet<string>> = new Map([
  ["Eq", new Set(["int", "float", "string", "bool", "char"])],
  ["Ord", new Set(["int", "float", "string", "char"])],
  ["Add", new Set(["int", "float", "string"])],
]);

// =============================================================================
// Registries
// =============================================================================

// Maps type name to its constructor names
export type ConstructorRegistry = Map<string, readonly string[]>;

// Maps alias name to params and aliased type
export type AliasRegistry = Map<
  string,
  { readonly params: readonly string[]; readonly type: Type }
>;

// =============================================================================
// Type Printing
// =============================================================================

export const typeToString = (type: Type): string => {
  switch (type.kind) {
    case "TVar":
      return type.name;
    case "TCon":
      return type.name;
    case "TFun": {
      const paramStr =
        type.param.kind === "TFun" ? `(${typeToString(type.param)})` : typeToString(type.param);
      return `${paramStr} -> ${typeToString(type.ret)}`;
    }
    case "TApp": {
      const conStr = typeToString(type.con);
      const argStr =
        type.arg.kind === "TApp" || type.arg.kind === "TFun"
          ? `(${typeToString(type.arg)})`
          : typeToString(type.arg);
      return `${conStr} ${argStr}`;
    }
    case "TTuple":
      return `(${type.elements.map(typeToString).join(", ")})`;
    case "TRecord": {
      const fields = [...type.fields.entries()]
        .map(([k, v]) => `${k}: ${typeToString(v)}`)
        .join(", ");
      if (type.row) {
        return `{ ${fields} | ${typeToString(type.row)} }`;
      }
      return `{ ${fields} }`;
    }
  }
};
