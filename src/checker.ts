/**
 * Type Inference Engine implementing Algorithm W (Hindley-Milner type inference).
 *
 * This module is the heart of the type checker. It takes an abstract syntax tree (AST)
 * and determines the types of all expressions without requiring explicit type annotations.
 *
 * Key concepts:
 *
 * 1. TYPE INFERENCE vs TYPE CHECKING
 *    - Type checking: verify that code matches given type annotations
 *    - Type inference: discover types automatically from how values are used
 *    Our system does inference - you write `fn x => x + 1` and we figure out it's `number -> number`
 *
 * 2. ALGORITHM W
 *    Named after Robin Milner, this algorithm:
 *    - Traverses the AST bottom-up
 *    - Assigns fresh type variables to unknowns
 *    - Collects constraints from how values are used
 *    - Solves constraints via unification
 *    - Produces the most general (principal) type
 *
 * 3. SUBSTITUTION
 *    A substitution maps type variables to types. When we learn that `t0 = number`,
 *    we record this and apply it to all types we're tracking. Substitutions compose:
 *    if we later learn `t1 = t0`, we can derive `t1 = number`.
 *
 * 4. UNIFICATION
 *    Given two types, find a substitution that makes them equal (or fail if impossible).
 *    Example: unify(`t0 -> boolean`, `number -> t1`) yields `{t0 = number, t1 = boolean}`
 *
 * 5. GENERALIZATION & INSTANTIATION
 *    - Generalization: turn a type with free variables into a polymorphic type scheme
 *      `t0 -> t0` becomes `∀a. a -> a` (for all types a, a -> a)
 *    - Instantiation: create fresh variables when using a polymorphic type
 *      Using `∀a. a -> a` might give us `t5 -> t5` with a fresh variable
 *
 * 6. LET POLYMORPHISM
 *    In `let id = fn x => x in (id 1, id "hello")`, the function `id` is used at
 *    two different types. This works because `let` generalizes before adding to
 *    the environment, allowing multiple instantiations. Lambda parameters don't
 *    get this treatment - they're monomorphic within their scope.
 *
 * References:
 * - "Algorithm W Step by Step" by Martin Grabmüller
 * - "Write You a Haskell" by Stephen Diehl
 * - "Types and Programming Languages" by Benjamin Pierce
 */

import * as ast from "./ast";
import type { Definition, SymbolTable } from "./binder";
import { type Diagnostic, error as diagError, typeMismatch } from "./diagnostics";

// =============================================================================
// INFERENCE CONTEXT
// =============================================================================

/**
 * Map from definitions to their inferred types.
 * Produced by the checker, used by LSP for hover information.
 */
export type TypeMap = ReadonlyMap<Definition, Type>;

/**
 * Mutable context for type inference.
 * Collects diagnostics and type assignments.
 */
type CheckContext = {
  readonly diagnostics: Diagnostic[];
  readonly types: Map<Definition, Type>;
  readonly symbols: SymbolTable;
  readonly definitionMap: ReadonlyMap<string, Definition>;
  readonly moduleEnv: ModuleTypeEnv;
  readonly moduleAliases: Map<string, string>; // alias -> real name
  /** Counter for generating unique type variable names */
  typeVarCounter: number;
  /** Direct span to type mapping for lowering */
  readonly spanTypes: Map<string, Type>;
};

/**
 * Create a fresh check context.
 */
const createContext = (
  symbols: SymbolTable,
  moduleEnv: ModuleTypeEnv = new Map(),
  moduleAliases: Map<string, string> = new Map(),
): CheckContext => {
  const definitionMap = new Map<string, Definition>();
  for (const def of symbols.definitions) {
    definitionMap.set(`${def.span.start}:${def.span.end}`, def);
  }

  return {
    diagnostics: [],
    types: new Map(),
    symbols,
    definitionMap,
    moduleEnv,
    moduleAliases,
    typeVarCounter: 0,
    spanTypes: new Map(),
  };
};

/**
 * Add an error diagnostic to the context.
 * Uses span position if available, otherwise 0,0.
 */
const addError = (ctx: CheckContext, message: string, span?: ast.Span): void => {
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(diagError(start, end, message));
};

/**
 * Record the type for a definition.
 * Looks up the definition by span in the symbol table.
 * Also records in spanTypes for direct lookup during lowering.
 */
const recordType = (ctx: CheckContext, span: ast.Span, type: Type): void => {
  const key = `${span.start}:${span.end}`;
  // Record for LSP (Definition-keyed)
  const def = ctx.definitionMap.get(key);
  if (def) {
    ctx.types.set(def, type);
  }
  // Record for lowering (span-keyed)
  ctx.spanTypes.set(key, type);
};

// =============================================================================
// INTERNAL TYPE REPRESENTATION
// =============================================================================

/**
 * Internal representation of types during inference.
 *
 * This is separate from ast.TypeExpr because:
 * - TypeExpr comes from user syntax (data declarations)
 * - Type is the internal representation with additional features like row types
 *
 * Types form a tree structure that we manipulate during inference.
 */
export type Type =
  | TVar // Type variable: an unknown type to be determined
  | TCon // Type constructor: a concrete type like `number` or `List`
  | TFun // Function type: param -> return
  | TApp // Type application: applying a type constructor to an argument
  | TRecord // Record type: { field: type, ... } with optional row variable
  | TTuple; // Tuple type: (type, type, ...)

/**
 * Type variable - represents an unknown type.
 *
 * During inference, we create fresh type variables for unknowns and later
 * discover what they should be through unification. For example, in `fn x => x`,
 * we assign `x` a fresh type variable `t0`, and the function gets type `t0 -> t0`.
 *
 * Type variables are named `t0`, `t1`, etc. for readability in output.
 */
export type TVar = {
  readonly kind: "TVar";
  readonly name: string;
};

/**
 * Type constructor - a concrete type name.
 *
 * Examples: `number`, `string`, `boolean`, `List`, `Maybe`
 *
 * Type constructors can be:
 * - Base types (number, string, boolean) - used directly
 * - Parameterized types (List, Maybe) - need type application
 */
export type TCon = {
  readonly kind: "TCon";
  readonly name: string;
};

/**
 * Function type - represents `param -> return`.
 *
 * All functions in our language take exactly one argument (curried style).
 * A function of two arguments is represented as `a -> (b -> c)`.
 *
 * Function types are right-associative: `a -> b -> c` means `a -> (b -> c)`.
 */
export type TFun = {
  readonly kind: "TFun";
  readonly param: Type;
  readonly ret: Type;
};

/**
 * Type application - applying a parameterized type to an argument.
 *
 * Example: `List number` is `TApp(TCon("List"), TCon("number"))`
 *
 * For multiple parameters, we use nested application (left-associative):
 * `Either string number` is `TApp(TApp(TCon("Either"), TCon("string")), TCon("number"))`
 */
export type TApp = {
  readonly kind: "TApp";
  readonly con: Type;
  readonly arg: Type;
};

/**
 * Record type - structural record with optional row polymorphism.
 *
 * Examples:
 * - `{ x: number, y: string }` - closed record (row = null)
 * - `{ x: number | ρ }` - open record (row = type variable)
 *
 * Row polymorphism allows functions to work on any record with certain fields:
 * `fn r => r.x` has type `{ x: t | ρ } -> t` - works on any record with an `x` field.
 *
 * The `row` field:
 * - null: closed record, no additional fields allowed
 * - TVar: open record, may have additional unknown fields
 * - TRecord: extended record (after unification)
 */
export type TRecord = {
  readonly kind: "TRecord";
  readonly fields: ReadonlyMap<string, Type>;
  readonly row: Type | null;
};

/**
 * Tuple type - fixed-size heterogeneous collection.
 *
 * Example: `(number, string, boolean)` is a 3-tuple
 *
 * Unlike lists, tuples have:
 * - Fixed arity (length) known at compile time
 * - Potentially different types for each position
 */
export type TTuple = {
  readonly kind: "TTuple";
  readonly elements: readonly Type[];
};

// =============================================================================
// TYPE CONSTRUCTORS (Smart constructors for types)
// =============================================================================

/** Create a type variable */
const tvar = (name: string): Type => ({ kind: "TVar", name });

/** Create a type constructor (named type) */
const tcon = (name: string): Type => ({ kind: "TCon", name });

/** Create a function type */
const tfun = (param: Type, ret: Type): Type => ({ kind: "TFun", param, ret });

/** Create a type application */
const tapp = (con: Type, arg: Type): Type => ({ kind: "TApp", con, arg });

/**
 * Create a record type.
 * @param fields Array of [fieldName, fieldType] pairs
 * @param row Optional row variable for open records (null for closed)
 */
const trecord = (fields: readonly [string, Type][], row: Type | null = null): Type => ({
  kind: "TRecord",
  fields: new Map(fields),
  row,
});

/** Create a tuple type */
const ttuple = (elements: readonly Type[]): Type => ({ kind: "TTuple", elements });

// =============================================================================
// BUILT-IN TYPES
// =============================================================================

/** The integer type */
const tInt = tcon("Int");

/** The floating-point type */
const tFloat = tcon("Float");

/** The string type */
const tStr = tcon("string");

/** The boolean type */
const tBool = tcon("boolean");

/** The character type */
const tChar = tcon("char");

// =============================================================================
// TYPE CLASSES (for operator overloading)
// =============================================================================

/**
 * A type class constraint records that a type must support certain operations.
 *
 * Example: The expression `x == y` generates an Eq constraint on the operand type.
 * Later, we verify that the concrete type actually supports equality.
 */
type Constraint = {
  readonly className: string; // The type class name (Eq, Ord, Add)
  readonly type: Type; // The type that must be an instance
};

/**
 * Type class instances - which concrete types support which operations.
 *
 * This is a simplified type class system. Real systems like Haskell's support:
 * - User-defined type classes
 * - Instance declarations
 * - Superclass constraints
 * - Default implementations
 *
 * Our system just tracks which built-in types support basic operations.
 */
const instances: Map<string, Set<string>> = new Map([
  // Eq: types that support equality testing (==, !=)
  ["Eq", new Set(["Int", "Float", "string", "boolean", "char"])],

  // Ord: types that support ordering comparisons (<, >, <=, >=)
  ["Ord", new Set(["Int", "Float", "string", "char"])],

  // Add: types that support the + operator (addition or concatenation)
  // Note: For mixed Int/Float, type widening to Float happens in inferBinOp
  ["Add", new Set(["Int", "Float", "string"])],
]);

// =============================================================================
// TYPE SCHEMES (Polymorphic types)
// =============================================================================

/**
 * A type scheme represents a polymorphic type with universally quantified variables.
 *
 * Written mathematically as: ∀a b c. constraints => type
 *
 * Example: The identity function `fn x => x` has scheme `∀a. a -> a`
 * This means "for any type a, this is a function from a to a".
 *
 * Components:
 * - vars: the quantified type variables (the "∀a b c" part)
 * - constraints: type class constraints that must hold
 * - type: the actual type with potentially free variables
 *
 * The vars list tells us which variables in `type` are universally quantified
 * (and should get fresh names on instantiation) vs free (referring to outer scope).
 */
export type Scheme = {
  readonly vars: readonly string[]; // Universally quantified variables
  readonly constraints: readonly Constraint[]; // Type class constraints
  readonly type: Type; // The type (may contain the quantified vars)
};

/**
 * Create a type scheme.
 * @param vars The quantified type variable names
 * @param type The type
 * @param constraints Optional type class constraints
 */
export const scheme = (
  vars: readonly string[],
  type: Type,
  constraints: readonly Constraint[] = [],
): Scheme => ({
  vars,
  constraints,
  type,
});

// =============================================================================
// ENVIRONMENTS AND REGISTRIES
// =============================================================================

/**
 * Type environment - maps variable names to their type schemes.
 *
 * The environment tracks what's in scope during type inference:
 * - Built-in functions (map, filter, etc.)
 * - Data constructors (Cons, Just, Nothing, etc.)
 * - Let-bound variables
 * - Function parameters
 *
 * Values bound by `let` get polymorphic schemes (can be used at multiple types).
 * Function parameters get monomorphic schemes (single type within the function).
 */
export type TypeEnv = Map<string, Scheme>;

/**
 * Substitution - maps type variable names to their resolved types.
 *
 * During inference, we discover relationships between types through unification.
 * A substitution records these discoveries: "t0 should be number", "t1 should be t2", etc.
 *
 * Substitutions are applied to types to resolve variables to their known values.
 * They compose: if s1 says t0=number and s2 says t1=t0, then s1∘s2 gives t1=number.
 */
export type Subst = Map<string, Type>;

/**
 * Maps variable names to types during pattern matching.
 * When we match `Cons x xs`, we learn that x and xs have certain types.
 */
type PatternBindings = Map<string, Type>;

/**
 * Registry of constructors for each algebraic data type.
 *
 * Used for exhaustiveness checking: given a type name like "Maybe", we look up
 * its constructors ["Nothing", "Just"] to verify all cases are covered.
 */
export type ConstructorRegistry = Map<string, readonly string[]>;

/**
 * Information about a module's exports.
 */
export type ModuleTypeInfo = {
  readonly typeEnv: TypeEnv;
  readonly registry: ConstructorRegistry;
  readonly constructorNames: readonly string[];
  /** Names of foreign functions declared in this module */
  readonly foreignNames: ReadonlySet<string>;
};

/**
 * Map from module names to their type information.
 * Used for resolving qualified access like Module.name.
 */
export type ModuleTypeEnv = Map<string, ModuleTypeInfo>;

// =============================================================================
// SUBSTITUTION APPLICATION
// =============================================================================

/**
 * Apply a substitution to a type, replacing type variables with their values.
 *
 * This is a fundamental operation in type inference. After unification discovers
 * that t0 = number, we apply this substitution everywhere to propagate the knowledge.
 *
 * The operation is recursive: if type contains t0 and subst maps t0 to number,
 * all occurrences of t0 in the type tree are replaced with number.
 *
 * @param subst The substitution to apply
 * @param type The type to transform
 * @returns The type with all substitutions applied
 */
export const applySubst = (subst: Subst, type: Type): Type => {
  switch (type.kind) {
    // Type constructors have no variables to substitute
    case "TCon":
      return type;

    // Type variable: look up in substitution, return as-is if not found
    case "TVar":
      return subst.get(type.name) ?? type;

    // Function type: substitute in both parameter and return types
    case "TFun":
      return tfun(applySubst(subst, type.param), applySubst(subst, type.ret));

    // Type application: substitute in both constructor and argument
    case "TApp":
      return tapp(applySubst(subst, type.con), applySubst(subst, type.arg));

    // Record type: substitute in all field types and the row variable
    case "TRecord": {
      const newFields = new Map<string, Type>();
      for (const [name, fieldType] of type.fields) {
        newFields.set(name, applySubst(subst, fieldType));
      }
      const newRow = type.row ? applySubst(subst, type.row) : null;
      return trecord([...newFields.entries()], newRow);
    }

    // Tuple type: substitute in all element types
    case "TTuple":
      return ttuple(type.elements.map((t) => applySubst(subst, t)));
  }
};

/**
 * Apply a substitution to a type scheme.
 *
 * Important: We must NOT substitute the scheme's quantified variables!
 * Those are bound by the ∀ and should not be affected by outer substitutions.
 *
 * Example: If scheme is `∀a. a -> t0` and subst is {a = number, t0 = string},
 * we should get `∀a. a -> string`, NOT `∀a. number -> string`.
 *
 * @param subst The substitution to apply
 * @param scheme_ The scheme to transform (renamed to avoid shadowing)
 * @returns The scheme with substitutions applied to free variables only
 */
const applySubstScheme = (subst: Subst, scheme_: Scheme): Scheme => {
  // Filter out the quantified variables from the substitution
  const filtered: Subst = new Map();
  for (const [name, type] of subst) {
    if (!scheme_.vars.includes(name)) {
      filtered.set(name, type);
    }
  }
  return scheme(scheme_.vars, applySubst(filtered, scheme_.type));
};

/**
 * Apply a substitution to all schemes in an environment.
 *
 * Used when we learn new type information that should propagate to the context.
 *
 * @param subst The substitution to apply
 * @param env The environment to transform
 * @returns New environment with substitutions applied
 */
const applySubstEnv = (subst: Subst, env: TypeEnv): TypeEnv => {
  const result: TypeEnv = new Map();
  for (const [name, s] of env) {
    result.set(name, applySubstScheme(subst, s));
  }
  return result;
};

/**
 * Apply a substitution to a type class constraint.
 */
const applySubstConstraint = (subst: Subst, c: Constraint): Constraint => ({
  className: c.className,
  type: applySubst(subst, c.type),
});

/**
 * Apply a substitution to a list of constraints.
 */
const applySubstConstraints = (subst: Subst, cs: readonly Constraint[]): Constraint[] =>
  cs.map((c) => applySubstConstraint(subst, c));

// =============================================================================
// SUBSTITUTION COMPOSITION
// =============================================================================

/**
 * Compose two substitutions: first apply s1, then apply s2.
 *
 * The composition s1 ∘ s2 means: when applying the result to a type,
 * first apply s2, then apply s1 to that result.
 *
 * However, we also need s2's effect on s1's mappings: if s1 maps t0 to t1,
 * and s2 maps t1 to number, then s1 ∘ s2 should map t0 to number.
 *
 * @param s1 First substitution (applied second to types)
 * @param s2 Second substitution (applied first to types)
 * @returns Combined substitution
 */
const composeSubst = (s1: Subst, s2: Subst): Subst => {
  const result: Subst = new Map();

  // Apply s2 to all types in s1's mappings
  for (const [name, type] of s1) {
    result.set(name, applySubst(s2, type));
  }

  // Add s2's mappings that aren't already in result
  for (const [name, type] of s2) {
    if (!result.has(name)) {
      result.set(name, type);
    }
  }

  return result;
};

// =============================================================================
// UNIFICATION
// =============================================================================

/**
 * Unify two types - find a substitution that makes them equal.
 *
 * Unification is the core constraint-solving mechanism in type inference.
 * Given two types, we find the most general substitution that makes them identical.
 *
 * Examples:
 * - unify(t0, number) → {t0 = number}
 * - unify(t0 -> t1, number -> boolean) → {t0 = number, t1 = boolean}
 * - unify(number, string) → ERROR (incompatible types)
 *
 * The algorithm:
 * 1. If either type is a variable, bind it to the other type
 * 2. If both are constructors, they must be the same name
 * 3. If both are compound (functions, applications), recursively unify parts
 * 4. Otherwise, fail (record diagnostic and return empty substitution)
 *
 * @param ctx Inference context for collecting diagnostics
 * @param t1 First type
 * @param t2 Second type
 * @returns Substitution that makes t1 and t2 equal (empty on error)
 */
const unify = (ctx: CheckContext, t1: Type, t2: Type, span?: ast.Span): Subst => {
  // Same type variable - already equal, no substitution needed
  if (t1.kind === "TVar" && t2.kind === "TVar" && t1.name === t2.name) {
    return new Map();
  }

  // t1 is a variable - bind it to t2
  if (t1.kind === "TVar") {
    return bindVar(ctx, t1.name, t2);
  }

  // t2 is a variable - bind it to t1
  if (t2.kind === "TVar") {
    return bindVar(ctx, t2.name, t1);
  }

  // Both are type constructors - must have the same name
  if (t1.kind === "TCon" && t2.kind === "TCon" && t1.name === t2.name) {
    return new Map();
  }

  // Both are function types - unify parameter and return types
  if (t1.kind === "TFun" && t2.kind === "TFun") {
    const s1 = unify(ctx, t1.param, t2.param);
    // Apply s1 before unifying return types (left-to-right dependency)
    const s2 = unify(ctx, applySubst(s1, t1.ret), applySubst(s1, t2.ret));
    return composeSubst(s1, s2);
  }

  // Both are type applications - unify constructor and argument
  if (t1.kind === "TApp" && t2.kind === "TApp") {
    const s1 = unify(ctx, t1.con, t2.con);
    const s2 = unify(ctx, applySubst(s1, t1.arg), applySubst(s1, t2.arg));
    return composeSubst(s1, s2);
  }

  // Both are record types - use specialized record unification
  if (t1.kind === "TRecord" && t2.kind === "TRecord") {
    return unifyRecords(ctx, t1, t2);
  }

  // Both are tuple types - unify element-wise
  if (t1.kind === "TTuple" && t2.kind === "TTuple") {
    if (t1.elements.length !== t2.elements.length) {
      addError(
        ctx,
        `Tuple arity mismatch: (${t1.elements.length} elements) vs (${t2.elements.length} elements)`,
      );
      return new Map();
    }
    let currentSubst: Subst = new Map();
    for (let i = 0; i < t1.elements.length; i++) {
      const elem1 = applySubst(currentSubst, t1.elements[i]!);
      const elem2 = applySubst(currentSubst, t2.elements[i]!);
      const s = unify(ctx, elem1, elem2);
      currentSubst = composeSubst(currentSubst, s);
    }
    return currentSubst;
  }

  // Types are incompatible
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(typeMismatch(start, end, typeToString(t1), typeToString(t2)));
  return new Map();
};

/**
 * Bind a type variable to a type.
 *
 * This creates a substitution {name = type}, but first checks for the
 * "occurs check" to prevent infinite types.
 *
 * The occurs check prevents situations like: t0 = List t0
 * This would create an infinitely nested type, which we reject.
 *
 * @param ctx Inference context for collecting diagnostics
 * @param name The type variable name
 * @param type The type to bind it to
 * @returns Substitution mapping name to type (empty on error)
 */
const bindVar = (ctx: CheckContext, name: string, type: Type): Subst => {
  // Binding to itself is a no-op
  if (type.kind === "TVar" && type.name === name) {
    return new Map();
  }

  // Occurs check: ensure the variable doesn't appear in the type
  if (freeTypeVars(type).has(name)) {
    addError(ctx, `Infinite type: ${name} appears in ${typeToString(type)}`);
    return new Map();
  }

  return new Map([[name, type]]);
};

// =============================================================================
// FRESH TYPE VARIABLES
// =============================================================================

/**
 * Generate a fresh type variable with a unique name.
 *
 * Fresh variables are used when we need to represent an unknown type:
 * - Function parameter types (before we know how they're used)
 * - Return types (before we analyze the function body)
 * - Instantiating polymorphic types
 *
 * @param ctx The check context containing the type variable counter
 */
const freshTypeVar = (ctx: CheckContext): Type => {
  return tvar(`t${ctx.typeVarCounter++}`);
};

// =============================================================================
// RECORD UNIFICATION (Row Polymorphism)
// =============================================================================

/**
 * Unify two record types with row polymorphism support.
 *
 * Row polymorphism allows records to have "extra" fields beyond what's specified.
 * A record `{ x: number | ρ }` has an `x` field and potentially more fields in ρ.
 *
 * This enables functions like `fn r => r.x` to work on any record with an x field,
 * regardless of what other fields it has.
 *
 * The algorithm handles several cases:
 * 1. Same fields, both closed → just unify field types
 * 2. Same fields, both open → unify field types and row variables
 * 3. Different fields → one must be open to absorb the difference
 * 4. Both have unique fields → both must be open, create fresh row for tail
 */
const unifyRecords = (ctx: CheckContext, t1: TRecord, t2: TRecord): Subst => {
  const fields1 = new Set(t1.fields.keys());
  const fields2 = new Set(t2.fields.keys());

  // Categorize fields
  const commonFields = fields1.intersection(fields2);
  const onlyIn1 = fields1.difference(fields2);
  const onlyIn2 = fields2.difference(fields1);

  // First, unify the types of common fields
  let currentSubst: Subst = new Map();
  for (const fieldName of commonFields) {
    const fieldType1 = applySubst(currentSubst, t1.fields.get(fieldName)!);
    const fieldType2 = applySubst(currentSubst, t2.fields.get(fieldName)!);
    const s = unify(ctx, fieldType1, fieldType2);
    currentSubst = composeSubst(currentSubst, s);
  }

  // Get row variables after applying current substitutions
  const row1 = t1.row ? applySubst(currentSubst, t1.row) : null;
  const row2 = t2.row ? applySubst(currentSubst, t2.row) : null;

  // Prepare extra fields with substitutions applied
  const extraFields1: [string, Type][] = [...onlyIn1].map((f) => [
    f,
    applySubst(currentSubst, t1.fields.get(f)!),
  ]);
  const extraFields2: [string, Type][] = [...onlyIn2].map((f) => [
    f,
    applySubst(currentSubst, t2.fields.get(f)!),
  ]);

  // Case 1: No extra fields on either side
  if (onlyIn1.size === 0 && onlyIn2.size === 0) {
    if (row1 && row2) {
      // Both open - unify row variables
      const s = unify(ctx, row1, row2);
      return composeSubst(currentSubst, s);
    }
    if (row1 && !row2) {
      // t1 open, t2 closed - t1's row must be empty
      const s = unify(ctx, row1, trecord([]));
      return composeSubst(currentSubst, s);
    }
    if (!row1 && row2) {
      // t1 closed, t2 open - t2's row must be empty
      const s = unify(ctx, row2, trecord([]));
      return composeSubst(currentSubst, s);
    }
    // Both closed with matching fields - already done
    return currentSubst;
  }

  // Case 2: t1 has extra fields, t2 doesn't
  if (onlyIn1.size > 0 && onlyIn2.size === 0) {
    if (!row2) {
      addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn1].join(", ")} }`);
      return currentSubst;
    }
    // t2's row must equal t1's extra fields (plus t1's row if any)
    const extraRecord = trecord(extraFields1, row1);
    const s = unify(ctx, row2, extraRecord);
    return composeSubst(currentSubst, s);
  }

  // Case 3: t2 has extra fields, t1 doesn't
  if (onlyIn2.size > 0 && onlyIn1.size === 0) {
    if (!row1) {
      addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn2].join(", ")} }`);
      return currentSubst;
    }
    // t1's row must equal t2's extra fields (plus t2's row if any)
    const extraRecord = trecord(extraFields2, row2);
    const s = unify(ctx, row1, extraRecord);
    return composeSubst(currentSubst, s);
  }

  // Case 4: Both have unique extra fields - both must be open
  if (!row1) {
    addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn2].join(", ")} }`);
    return currentSubst;
  }
  if (!row2) {
    addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn1].join(", ")} }`);
    return currentSubst;
  }

  // Both records contribute unique fields - create a shared tail
  const freshRow = freshTypeVar(ctx);
  // row1 = { t2's extra fields | freshRow }
  const s1 = unify(ctx, row1, trecord(extraFields2, freshRow));
  currentSubst = composeSubst(currentSubst, s1);
  // row2 = { t1's extra fields | freshRow }
  const s2 = unify(ctx, applySubst(currentSubst, row2), trecord(extraFields1, freshRow));
  return composeSubst(currentSubst, s2);
};

// =============================================================================
// INSTANTIATION AND GENERALIZATION
// =============================================================================

/**
 * Instantiate a type scheme with fresh type variables.
 *
 * When we use a polymorphic value, we create a fresh copy of its type
 * where all quantified variables are replaced with fresh type variables.
 *
 * Example: Using `id : ∀a. a -> a` creates a fresh instance like `t5 -> t5`.
 * Each use of `id` gets its own fresh variables, allowing different types.
 *
 * @param ctx The check context for generating fresh variables
 * @param s The scheme to instantiate
 * @returns A fresh monomorphic type
 */
const instantiate = (ctx: CheckContext, s: Scheme): Type => {
  // Create fresh variables for each quantified variable
  const freshVars = new Map<string, Type>();
  for (const name of s.vars) {
    freshVars.set(name, freshTypeVar(ctx));
  }
  // Apply the fresh variable substitution to the type
  return applySubst(freshVars, s.type);
};

/**
 * Generalize a type into a polymorphic scheme.
 *
 * Generalization quantifies over type variables that are:
 * 1. Free in the type (appear in it)
 * 2. NOT free in the environment (not constrained by context)
 *
 * This is what enables let-polymorphism: in `let id = fn x => x in ...`,
 * after inferring `id : t0 -> t0`, we generalize to `∀t0. t0 -> t0`
 * because t0 isn't constrained by anything in the environment.
 *
 * @param env The current type environment
 * @param type The type to generalize
 * @returns A polymorphic type scheme
 */
export const generalize = (env: TypeEnv, type: Type): Scheme => {
  const typeVars = freeTypeVars(type);
  const envVars = freeEnvVars(env);
  // Quantify over variables free in the type but not in the environment
  const vars = typeVars.difference(envVars);
  return scheme([...vars], type);
};

// =============================================================================
// FREE TYPE VARIABLES
// =============================================================================

/**
 * Find all free type variables in a type.
 *
 * A type variable is "free" if it's not bound by a surrounding quantifier.
 * For our Type representation, all type variables are free (quantifiers
 * only appear at the Scheme level).
 *
 * @param type The type to analyze
 * @returns Set of free type variable names
 */
export const freeTypeVars = (type: Type): Set<string> => {
  switch (type.kind) {
    case "TCon":
      return new Set(); // Type constructors have no variables

    case "TVar":
      return new Set([type.name]); // The variable itself is free

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

/**
 * Find free type variables in a scheme.
 *
 * The scheme's quantified variables are bound, so we exclude them.
 * Only variables in the type that AREN'T quantified are free.
 */
const freeSchemeVars = (s: Scheme): Set<string> => {
  return freeTypeVars(s.type).difference(new Set(s.vars));
};

/**
 * Find all free type variables in an environment.
 *
 * A variable is free in the environment if it's free in any scheme.
 * These represent type constraints from the surrounding context.
 */
const freeEnvVars = (env: TypeEnv): Set<string> => {
  let result = new Set<string>();
  for (const s of env.values()) {
    result = result.union(freeSchemeVars(s));
  }
  return result;
};

// =============================================================================
// TYPE PRETTY-PRINTING
// =============================================================================

/**
 * Convert a type to a human-readable string.
 *
 * This is used for error messages and debugging output.
 * The format follows common conventions:
 * - Function types: a -> b (right-associative)
 * - Type application: List a (space-separated)
 * - Tuples: (a, b, c)
 * - Records: { x: a, y: b } or { x: a | ρ } for open records
 */
export const typeToString = (type: Type): string => {
  switch (type.kind) {
    case "TVar":
      return type.name;

    case "TCon":
      return type.name;

    case "TFun": {
      // Parenthesize function parameters that are themselves functions
      const param =
        type.param.kind === "TFun" ? `(${typeToString(type.param)})` : typeToString(type.param);
      return `${param} -> ${typeToString(type.ret)}`;
    }

    case "TApp": {
      // Parenthesize complex arguments
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

// =============================================================================
// TYPE CLASS CONSTRAINT SOLVING
// =============================================================================

/**
 * Verify that all type class constraints are satisfied.
 *
 * After inference, we have constraints like "the operand of + must support Add".
 * This function checks that the concrete types actually have the required instances.
 *
 * Constraints on type variables are deferred - they'll be checked when instantiated.
 *
 * @param ctx Inference context for collecting diagnostics
 * @param constraints The constraints to check
 */
const solveConstraints = (ctx: CheckContext, constraints: readonly Constraint[]): void => {
  const solve = (c: Constraint): void => {
    const { className, type } = c;

    switch (type.kind) {
      case "TVar":
        // Defer until instantiation
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
        // Could be extended for things like List a, but for now, we don't have instances for them.
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

// =============================================================================
// MAIN INFERENCE ENTRY POINT
// =============================================================================

/**
 * The result of type inference: substitution, type, constraints, and diagnostics.
 */
type InferResult = [Subst, Type, readonly Constraint[]];

/**
 * The full result of type checking including diagnostics.
 */
export type CheckOutput = {
  readonly subst: Subst;
  readonly type: Type;
  readonly constraints: readonly Constraint[];
  readonly diagnostics: readonly Diagnostic[];
  readonly types: TypeMap;
  /** Direct span to type mapping for lowering */
  readonly spanTypes: ReadonlyMap<string, Type>;
};

/**
 * Check (infer types for) an expression.
 *
 * This is the main entry point for type checking. It:
 * 1. Resets the type variable counter for consistent output
 * 2. Runs the inference algorithm
 * 3. Applies final substitutions to constraints
 * 4. Verifies all type class constraints
 * 5. Returns diagnostics instead of throwing errors
 *
 * @param env The type environment (variables in scope)
 * @param registry The constructor registry (for exhaustiveness checking)
 * @param expr The expression to type-check
 * @param symbols The symbol table from the binder
 * @returns The inferred type information with diagnostics
 */
export const check = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Expr,
  symbols: SymbolTable,
  moduleEnv: ModuleTypeEnv = new Map(),
  moduleAliases: Map<string, string> = new Map(),
): CheckOutput => {
  const ctx = createContext(symbols, moduleEnv, moduleAliases);
  const [subst, type, constraints] = inferExpr(ctx, env, registry, expr);
  const finalConstraints = applySubstConstraints(subst, constraints);
  solveConstraints(ctx, finalConstraints);
  return {
    subst,
    type,
    constraints: finalConstraints,
    diagnostics: ctx.diagnostics,
    types: ctx.types,
    spanTypes: ctx.spanTypes,
  };
};

// =============================================================================
// EXPRESSION INFERENCE
// =============================================================================

/**
 * Infer the type of an expression (internal recursive function).
 *
 * This implements Algorithm W by case analysis on the expression kind.
 * Each case returns:
 * - A substitution (learned type information)
 * - The inferred type
 * - Any type class constraints generated
 *
 * @param ctx Inference context for collecting diagnostics
 * @param env Current type environment
 * @param registry Constructor registry for pattern matching
 * @param expr The expression to infer
 * @returns Inference result tuple
 */
const inferExpr = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Expr,
): InferResult => {
  switch (expr.kind) {
    case "Abs":
      return inferAbs(ctx, env, registry, expr);
    case "App":
      return inferApp(ctx, env, registry, expr);
    case "BinOp":
      return inferBinOp(ctx, env, registry, expr);
    case "Bool":
      return [new Map(), tBool, []];
    case "If":
      return inferIf(ctx, env, registry, expr);
    case "Let":
      return inferLet(ctx, env, registry, expr);
    case "LetRec":
      return inferLetRec(ctx, env, registry, expr);
    case "Int":
      return [new Map(), tInt, []];
    case "Float":
      return [new Map(), tFloat, []];
    case "Str":
      return [new Map(), tStr, []];
    case "Char":
      return [new Map(), tChar, []];
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

// =============================================================================
// RECORD AND FIELD ACCESS INFERENCE
// =============================================================================

/**
 * Infer the type of a field access expression (record.field).
 *
 * This handles row polymorphism: if we don't know the record type yet,
 * we constrain it to be an open record with the required field.
 *
 * Cases:
 * 1. Record type is a variable → constrain it to { field: t | ρ }
 * 2. Record type has the field → return the field's type
 * 3. Record is open but field not present → constrain the row
 * 4. Record is closed without field → error
 */
const inferFieldAccess = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.FieldAccess,
): InferResult => {
  // Infer the record expression's type
  const [s1, recordType, constraints] = inferExpr(ctx, env, registry, expr.record);
  const resolvedType = applySubst(s1, recordType);

  // Case 1: Type variable - constrain to open record with the field
  if (resolvedType.kind === "TVar") {
    const fieldType = freshTypeVar(ctx);
    const rowVar = freshTypeVar(ctx);
    const openRecord = trecord([[expr.field, fieldType]], rowVar);
    const s2 = unify(ctx, resolvedType, openRecord);
    return [composeSubst(s1, s2), applySubst(s2, fieldType), constraints];
  }

  // Must be a record type
  if (resolvedType.kind !== "TRecord") {
    addError(
      ctx,
      `Cannot access field '${expr.field}' on non-record type: ${typeToString(resolvedType)}`,
    );
    return [s1, freshTypeVar(ctx), constraints];
  }

  // Case 2: Field exists in known fields
  const fieldType = resolvedType.fields.get(expr.field);
  if (fieldType) {
    return [s1, fieldType, constraints];
  }

  // Case 3: Field not in known fields but record is open - constrain the row
  if (resolvedType.row) {
    const newFieldType = freshTypeVar(ctx);
    const newRowVar = freshTypeVar(ctx);
    const s2 = unify(ctx, resolvedType.row, trecord([[expr.field, newFieldType]], newRowVar));
    return [composeSubst(s1, s2), applySubst(s2, newFieldType), constraints];
  }

  // Case 4: Closed record without the field - error
  addError(
    ctx,
    `Record has no field '${expr.field}'. Available: ${[...resolvedType.fields.keys()].join(", ")}`,
  );
  return [s1, freshTypeVar(ctx), constraints];
};

/**
 * Infer the type of a tuple index expression (tuple.0, tuple.1).
 *
 * The index must be within bounds of the tuple's arity.
 */
const inferTupleIndex = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.TupleIndex,
): InferResult => {
  // Infer the tuple expression's type
  const [s1, tupleType, constraints] = inferExpr(ctx, env, registry, expr.tuple);
  const resolvedType = applySubst(s1, tupleType);

  // Case 1: Type variable - constrain to tuple with at least index+1 elements
  if (resolvedType.kind === "TVar") {
    // We don't know the tuple arity yet, so create a fresh type for the element
    // and let later unification resolve it
    const elementType = freshTypeVar(ctx);
    // We can't fully constrain this without knowing the arity, so just return
    // the fresh type and let pattern matching or other operations constrain it
    return [s1, elementType, constraints];
  }

  // Must be a tuple type
  if (resolvedType.kind !== "TTuple") {
    addError(ctx, `Cannot index into non-tuple type: ${typeToString(resolvedType)}`);
    return [s1, freshTypeVar(ctx), constraints];
  }

  // Check bounds
  if (expr.index < 0 || expr.index >= resolvedType.elements.length) {
    addError(
      ctx,
      `Tuple index ${expr.index} out of bounds for tuple of ${resolvedType.elements.length} element(s)`,
    );
    return [s1, freshTypeVar(ctx), constraints];
  }

  // Return the type at the specified index (bounds already checked above)
  return [s1, resolvedType.elements[expr.index]!, constraints];
};

/**
 * Infer the type of a record literal { field1: value1, field2: value2 }.
 *
 * We infer each field's value type and combine them into a closed record type.
 * The record is closed (no row variable) because we know exactly what fields it has.
 */
const inferRecord = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Record,
): InferResult => {
  let subst: Subst = new Map();
  let constraints: Constraint[] = [];
  const fieldTypes = new Map<string, Type>();

  // Infer each field's type
  for (const field of expr.fields) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), registry, field.value);
    subst = composeSubst(subst, s);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    fieldTypes.set(field.name, applySubst(subst, t));
  }

  // Return a closed record type (no row variable)
  return [subst, trecord([...fieldTypes.entries()]), constraints];
};

// =============================================================================
// FUNCTION INFERENCE
// =============================================================================

/**
 * Infer the type of a lambda abstraction (fn param => body).
 *
 * Algorithm:
 * 1. Create a fresh type variable for the parameter
 * 2. Add parameter to environment with monomorphic type (no generalization!)
 * 3. Infer the body's type in the extended environment
 * 4. The function type is: (parameter type with substitutions) -> body type
 *
 * Note: Lambda parameters are monomorphic within their body. This differs from
 * let-bound variables which are polymorphic. This is key to Algorithm W.
 */
const inferAbs = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Abs,
): InferResult => {
  // If there's a type annotation, use it; otherwise create a fresh type variable
  let paramType: Type;
  let subst: Subst = new Map();

  if (expr.paramType) {
    // Convert annotation to type, replacing type variables with fresh ones
    const annotatedType = instantiateTypeExpr(ctx, expr.paramType);
    paramType = annotatedType;
  } else {
    // No annotation - use fresh type variable
    paramType = freshTypeVar(ctx);
  }

  // Add parameter to environment (monomorphic - empty quantifier list)
  const newEnv = new Map(env);
  newEnv.set(expr.param, scheme([], paramType));

  // Infer body type with parameter in scope
  const [bodySubst, bodyType, constraints] = inferExpr(ctx, newEnv, registry, expr.body);
  subst = composeSubst(subst, bodySubst);

  // Record the parameter's final type
  const paramSpan = expr.paramSpan ?? expr.span;
  if (paramSpan) {
    recordType(ctx, paramSpan, applySubst(subst, paramType));
  }

  // Function type: apply substitutions to parameter type
  return [subst, tfun(applySubst(subst, paramType), bodyType), constraints];
};

/**
 * Infer the type of a function application (func arg).
 *
 * Algorithm:
 * 1. Infer the function's type
 * 2. Infer the argument's type
 * 3. Create a fresh variable for the return type
 * 4. Unify function type with (argType -> returnType)
 * 5. Apply resulting substitution to return type
 */
const inferApp = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.App,
): InferResult => {
  // Infer function type
  const [s1, funcType, c1] = inferExpr(ctx, env, registry, expr.func);

  // Infer argument type (with s1 applied to environment)
  const [s2, paramType, c2] = inferExpr(ctx, applySubstEnv(s1, env), registry, expr.param);

  // Fresh variable for result
  const returnType = freshTypeVar(ctx);

  // The function must have type: argType -> returnType
  const s3 = unify(ctx, applySubst(s2, funcType), tfun(paramType, returnType));

  const subst = composeSubst(composeSubst(s1, s2), s3);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);

  return [subst, applySubst(s3, returnType), constraints];
};

// =============================================================================
// OPERATOR INFERENCE
// =============================================================================

/**
 * Infer the type of a binary operator expression.
 *
 * Operators are polymorphic but constrained by type classes:
 * - Arithmetic (+): Add constraint (works on number and string)
 * - Arithmetic (-, *, /): Must be number
 * - Comparison (<, >, <=, >=): Ord constraint
 * - Equality (==, !=): Eq constraint
 *
 * Both operands must have the same type (unified first).
 */
const inferBinOp = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.BinOp,
): InferResult => {
  // Infer left operand
  const [s1, leftType, c1] = inferExpr(ctx, env, registry, expr.left);

  // Infer right operand
  const [s2, rightType, c2] = inferExpr(ctx, applySubstEnv(s1, env), registry, expr.right);

  const resolvedLeft = applySubst(s2, leftType);
  const resolvedRight = rightType;

  // Helper to check if a type is Int
  const isInt = (t: Type): boolean => t.kind === "TCon" && t.name === "Int";
  // Helper to check if a type is Float
  const isFloat = (t: Type): boolean => t.kind === "TCon" && t.name === "Float";
  // Helper to check if a type is numeric (Int or Float)
  const isNumeric = (t: Type): boolean => isInt(t) || isFloat(t);

  // Type widening for numeric operations: Int + Float → Float
  // Returns [subst, operandType] where operandType is the widened type
  const widenNumeric = (): [Subst, Type] => {
    // Both numeric: widen to Float if either is Float
    if (isNumeric(resolvedLeft) && isNumeric(resolvedRight)) {
      if (isFloat(resolvedLeft) || isFloat(resolvedRight)) {
        return [new Map(), tFloat];
      }
      return [new Map(), tInt];
    }
    // One is numeric, other is type variable: constrain variable to match
    if (isNumeric(resolvedLeft) && resolvedRight.kind === "TVar") {
      const s = unify(ctx, resolvedRight, resolvedLeft, expr.span);
      return [s, resolvedLeft];
    }
    if (isNumeric(resolvedRight) && resolvedLeft.kind === "TVar") {
      const s = unify(ctx, resolvedLeft, resolvedRight, expr.span);
      return [s, resolvedRight];
    }
    // Neither is numeric (or both are variables): unify and check later
    const s3 = unify(ctx, resolvedLeft, resolvedRight, expr.span);
    return [s3, applySubst(s3, resolvedRight)];
  };

  // For non-numeric operators, just unify types normally
  const unifyTypes = (): [Subst, Type] => {
    const s3 = unify(ctx, resolvedLeft, resolvedRight, expr.span);
    return [s3, applySubst(s3, resolvedRight)];
  };

  // Helper to ensure a type is numeric
  const ensureNumeric = (type: Type): Subst => {
    if (isNumeric(type)) {
      return new Map();
    }
    if (type.kind === "TVar") {
      return unify(ctx, type, tInt, expr.span);
    }
    return unify(ctx, type, tInt, expr.span);
  };

  switch (expr.op) {
    // Addition: polymorphic (Add class)
    // For numeric types, use widening; for strings, use normal unification
    case "+": {
      // Check if we're dealing with numeric types for widening
      if (isNumeric(resolvedLeft) || isNumeric(resolvedRight)) {
        const [s3, operandType] = widenNumeric();
        const subst = composeSubst(composeSubst(s1, s2), s3);
        const constraints = [
          ...applySubstConstraints(subst, c1),
          ...applySubstConstraints(subst, c2),
        ];
        constraints.push({ className: "Add", type: operandType });
        return [subst, operandType, constraints];
      }
      // Non-numeric: normal unification (e.g., string + string)
      const [s3, operandType] = unifyTypes();
      const subst = composeSubst(composeSubst(s1, s2), s3);
      const constraints = [
        ...applySubstConstraints(subst, c1),
        ...applySubstConstraints(subst, c2),
      ];
      constraints.push({ className: "Add", type: operandType });
      return [subst, operandType, constraints];
    }

    // Other arithmetic: must be numeric (Int or Float), use widening
    case "-":
    case "/":
    case "*": {
      const [s3, operandType] = widenNumeric();
      const subst = composeSubst(composeSubst(s1, s2), s3);
      const constraints = [
        ...applySubstConstraints(subst, c1),
        ...applySubstConstraints(subst, c2),
      ];
      const s4 = ensureNumeric(operandType);
      const finalType = applySubst(s4, operandType);
      return [composeSubst(subst, s4), finalType, constraints];
    }

    // Comparison: Ord constraint, returns boolean
    // Use widening for numeric comparisons
    case "<":
    case ">":
    case "<=":
    case ">=": {
      const [s3, operandType] =
        isNumeric(resolvedLeft) || isNumeric(resolvedRight) ? widenNumeric() : unifyTypes();
      const subst = composeSubst(composeSubst(s1, s2), s3);
      const constraints = [
        ...applySubstConstraints(subst, c1),
        ...applySubstConstraints(subst, c2),
      ];
      constraints.push({ className: "Ord", type: operandType });
      return [subst, tBool, constraints];
    }

    // Equality: Eq constraint, returns boolean
    // Use widening for numeric equality
    case "==":
    case "!=": {
      const [s3, operandType] =
        isNumeric(resolvedLeft) || isNumeric(resolvedRight) ? widenNumeric() : unifyTypes();
      const subst = composeSubst(composeSubst(s1, s2), s3);
      const constraints = [
        ...applySubstConstraints(subst, c1),
        ...applySubstConstraints(subst, c2),
      ];
      constraints.push({ className: "Eq", type: operandType });
      return [subst, tBool, constraints];
    }
  }
};

// =============================================================================
// CONDITIONAL INFERENCE
// =============================================================================

/**
 * Infer the type of an if-then-else expression.
 *
 * Rules:
 * - Condition must be boolean
 * - Both branches must have the same type (the result type)
 */
const inferIf = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.If,
): InferResult => {
  // Infer condition type
  const [s1, condType, c1] = inferExpr(ctx, env, registry, expr.cond);

  // Condition must be boolean
  const s2 = unify(ctx, condType, tBool);
  let subst = composeSubst(s1, s2);

  // Infer then branch
  const [s3, thenType, c2] = inferExpr(ctx, applySubstEnv(subst, env), registry, expr.then);
  subst = composeSubst(subst, s3);

  // Infer else branch
  const [s4, elseType, c3] = inferExpr(ctx, applySubstEnv(subst, env), registry, expr.else);
  subst = composeSubst(subst, s4);

  // Both branches must have the same type
  const s5 = unify(ctx, applySubst(s4, thenType), elseType);
  const finalSubst = composeSubst(subst, s5);

  const constraints = applySubstConstraints(finalSubst, [...c1, ...c2, ...c3]);
  return [finalSubst, applySubst(s5, elseType), constraints];
};

// =============================================================================
// LET BINDING INFERENCE
// =============================================================================

/**
 * Infer the type of a let binding (let name = value in body).
 *
 * This is where let-polymorphism happens:
 * 1. Infer the value's type
 * 2. GENERALIZE the type (quantify free variables not in environment)
 * 3. Add the generalized scheme to the environment
 * 4. Infer the body's type with the new binding
 *
 * The generalization step is what makes `let id = fn x => x in (id 1, id "hi")`
 * work - `id` gets type `∀a. a -> a` and each use instantiates it fresh.
 */
const inferLet = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Let,
): InferResult => {
  // Infer the value's type
  let [s1, valueType, c1] = inferExpr(ctx, env, registry, expr.value);

  // If there's a return type annotation, unify with the innermost return type
  // (after peeling off all function parameter types)
  if (expr.returnType) {
    const annotatedType = instantiateTypeExpr(ctx, expr.returnType);
    // Peel off function types to get to the return type
    let currentType = applySubst(s1, valueType);
    while (currentType.kind === "TFun") {
      currentType = currentType.ret;
    }
    const s3 = unify(ctx, currentType, annotatedType, expr.nameSpan ?? expr.span);
    s1 = composeSubst(s1, s3);
    valueType = applySubst(s3, valueType);
  }

  // Apply substitution to environment before generalizing
  const env1 = applySubstEnv(s1, env);

  // Generalize: quantify over variables not free in the environment
  const generalizedScheme = generalize(env1, applySubst(s1, valueType));

  // Record the binding's type
  const nameSpan = expr.nameSpan ?? expr.span;
  if (nameSpan) {
    recordType(ctx, nameSpan, applySubst(s1, valueType));
  }

  // Add binding to environment
  const env2 = new Map(env1);
  env2.set(expr.name, generalizedScheme);

  // Infer body with new binding
  const [s2, bodyType, c2] = inferExpr(ctx, env2, registry, expr.body);

  const subst = composeSubst(s1, s2);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);

  return [subst, bodyType, constraints];
};

/**
 * Infer the type of a recursive let binding (let rec f = ... and g = ... in body).
 *
 * Supports mutual recursion: all binding names are in scope within all values.
 *
 * Algorithm:
 * 1. Create fresh type variables as placeholders for ALL bindings
 * 2. Add ALL placeholders to the environment (monomorphic - no generalization yet)
 * 3. Infer each value's type with all placeholders in scope
 * 4. Generalize ALL results together and add to environment for body
 */
const inferLetRec = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.LetRec,
): InferResult => {
  // Step 1: Create placeholder type variables for ALL bindings
  const placeholders = new Map<string, Type>();
  const envWithPlaceholders = new Map(env);

  for (const binding of expr.bindings) {
    const placeholder = freshTypeVar(ctx);
    placeholders.set(binding.name, placeholder);
    envWithPlaceholders.set(binding.name, scheme([], placeholder));
  }

  // Step 2: Infer types for all values with placeholders in scope
  let subst: Subst = new Map();
  const valueTypes = new Map<string, Type>();
  let constraints: Constraint[] = [];

  for (const binding of expr.bindings) {
    let [s, valueType, c] = inferExpr(
      ctx,
      applySubstEnv(subst, envWithPlaceholders),
      registry,
      binding.value,
    );
    subst = composeSubst(subst, s);

    // If there's a return type annotation, unify with the innermost return type
    if (binding.returnType) {
      const annotatedType = instantiateTypeExpr(ctx, binding.returnType);
      // Peel off function types to get to the return type
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

    // Unify with placeholder to propagate constraints
    const placeholder = applySubst(subst, placeholders.get(binding.name)!);
    const s2 = unify(ctx, placeholder, valueType, binding.value.span);
    subst = composeSubst(subst, s2);
  }

  // Step 3: Generalize all bindings together
  const env1 = applySubstEnv(subst, env);
  const env2 = new Map(env1);

  for (const binding of expr.bindings) {
    const valueType = applySubst(subst, valueTypes.get(binding.name)!);
    const generalizedScheme = generalize(env1, valueType);
    env2.set(binding.name, generalizedScheme);

    // Record type for LSP
    const nameSpan = binding.nameSpan ?? expr.span;
    if (nameSpan) {
      recordType(ctx, nameSpan, valueType);
    }
  }

  // Step 4: Infer body with generalized bindings
  const [s2, bodyType, c2] = inferExpr(ctx, env2, registry, expr.body);

  const finalSubst = composeSubst(subst, s2);
  const finalConstraints = applySubstConstraints(finalSubst, [...constraints, ...c2]);

  return [finalSubst, bodyType, finalConstraints];
};

// =============================================================================
// VARIABLE INFERENCE
// =============================================================================

/**
 * Infer the type of a variable reference.
 *
 * Look up the variable in the environment and instantiate its scheme.
 * Each use of a polymorphic variable gets fresh type variables.
 *
 * Note: The binder already reports unbound variable errors and tracks references.
 * We still check here because the type environment might differ from the
 * binder's scope (e.g., constructors from data declarations).
 */
const inferVar = (ctx: CheckContext, env: TypeEnv, expr: ast.Var): InferResult => {
  const s = env.get(expr.name);
  if (!s) {
    // Don't duplicate error if binder already reported it
    // Just return a fresh type variable to continue checking
    return [new Map(), freshTypeVar(ctx), []];
  }
  // Instantiate with fresh variables
  return [new Map(), instantiate(ctx, s), []];
};

/**
 * Infer the type of a qualified variable reference (Module.name).
 *
 * Looks up the module in the module environment, then looks up the
 * member within that module's type environment.
 */
const inferQualifiedVar = (ctx: CheckContext, expr: ast.QualifiedVar): InferResult => {
  // Resolve module alias if present
  const realModule = ctx.moduleAliases.get(expr.moduleName) ?? expr.moduleName;

  // Look up module
  const mod = ctx.moduleEnv.get(realModule);
  if (!mod) {
    addError(ctx, `Unknown module: ${expr.moduleName}`, expr.span);
    return [new Map(), freshTypeVar(ctx), []];
  }

  // Look up member in module
  const s = mod.typeEnv.get(expr.member);
  if (!s) {
    addError(ctx, `Module '${expr.moduleName}' does not export '${expr.member}'`, expr.span);
    return [new Map(), freshTypeVar(ctx), []];
  }

  // Instantiate with fresh variables
  return [new Map(), instantiate(ctx, s), []];
};

// =============================================================================
// TUPLE INFERENCE
// =============================================================================

/**
 * Infer the type of a tuple expression (a, b, c).
 *
 * We infer each element's type and combine into a tuple type.
 * Single-element "tuples" are unwrapped to just their element type.
 */
const inferTuple = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Tuple,
): InferResult => {
  if (expr.elements.length === 0) {
    addError(ctx, "Tuples cannot be empty");
    return [new Map(), ttuple([]), []];
  }

  // Single element - not really a tuple
  if (expr.elements.length === 1) {
    return inferExpr(ctx, env, registry, expr.elements[0]!);
  }

  let subst: Subst = new Map();
  let constraints: Constraint[] = [];
  const types: Type[] = [];

  // Infer each element's type
  for (const elem of expr.elements) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), registry, elem);
    subst = composeSubst(subst, s);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    types.push(applySubst(subst, t));
  }

  return [subst, ttuple(types), constraints];
};

// =============================================================================
// PATTERN MATCHING INFERENCE
// =============================================================================

/**
 * Infer types and bindings from a pattern.
 *
 * Pattern inference:
 * 1. Takes an expected type (the scrutinee's type)
 * 2. Returns updated substitution and variable bindings
 *
 * Each pattern kind has its own inference logic:
 * - PVar: binds the variable to the expected type
 * - PWildcard: matches anything, binds nothing
 * - PLit: unifies expected type with literal's type
 * - PCon: looks up constructor, unifies, and recurses into arguments
 * - PRecord: builds/matches record type with fields
 * - PTuple: builds/matches tuple type with elements
 */
const inferPattern = (
  ctx: CheckContext,
  env: TypeEnv,
  pattern: ast.Pattern,
  expectedType: Type,
  subst: Subst,
): [Subst, PatternBindings] => {
  switch (pattern.kind) {
    case "PVar": {
      // Variable pattern: bind the name to the expected type
      const boundType = applySubst(subst, expectedType);

      // Record pattern binding's type
      if (pattern.span) {
        recordType(ctx, pattern.span, boundType);
      }

      return [subst, new Map([[pattern.name, boundType]])];
    }

    case "PWildcard": {
      // Wildcard: matches anything, no bindings
      return [subst, new Map()];
    }

    case "PLit": {
      // Literal: expected type must match the literal's type
      // For numbers, distinguish Int (integer values) from Float
      const litType =
        typeof pattern.value === "number"
          ? Number.isInteger(pattern.value)
            ? tInt
            : tFloat
          : typeof pattern.value === "string"
            ? tStr
            : tBool;
      const s = unify(ctx, applySubst(subst, expectedType), litType);
      return [composeSubst(subst, s), new Map()];
    }

    case "PChar": {
      // Character literal pattern: expected type must be char
      const s = unify(ctx, applySubst(subst, expectedType), tChar);
      return [composeSubst(subst, s), new Map()];
    }

    case "PCon": {
      // Constructor pattern: look up constructor type and match
      // Note: Binder already recorded this as a reference
      const conScheme = env.get(pattern.name);
      if (!conScheme) {
        addError(ctx, `Unknown constructor: ${pattern.name}`, pattern.span);
        return [subst, new Map()];
      }

      // Instantiate constructor type
      const conType = instantiate(ctx, conScheme);

      // Extract argument types from the constructor's function type
      // Con a (Con b c) has type: a -> b -> c -> Result
      const argTypes: Type[] = [];
      let resultType = conType;
      while (resultType.kind === "TFun") {
        argTypes.push(resultType.param);
        resultType = resultType.ret;
      }

      // Verify arity
      if (argTypes.length !== pattern.args.length) {
        addError(
          ctx,
          `Constructor ${pattern.name} expects ${argTypes.length} args, got ${pattern.args.length}`,
        );
        return [subst, new Map()];
      }

      // Unify constructor's result type with expected type
      const s1 = unify(ctx, applySubst(subst, resultType), applySubst(subst, expectedType));
      let currentSubst = composeSubst(subst, s1);

      // Recursively infer patterns for constructor arguments
      const allBindings: PatternBindings = new Map();
      for (let i = 0; i < pattern.args.length; i++) {
        const argType = applySubst(currentSubst, argTypes[i]!);
        const [s, bindings] = inferPattern(ctx, env, pattern.args[i]!, argType, currentSubst);
        currentSubst = s;
        for (const [name, type] of bindings) {
          allBindings.set(name, type);
        }
      }

      return [currentSubst, allBindings];
    }

    case "QualifiedPCon": {
      // Qualified constructor pattern: Module.Constructor
      const realModule = ctx.moduleAliases.get(pattern.moduleName) ?? pattern.moduleName;
      const mod = ctx.moduleEnv.get(realModule);

      if (!mod) {
        addError(ctx, `Unknown module: ${pattern.moduleName}`, pattern.span);
        return [subst, new Map()];
      }

      const conScheme = mod.typeEnv.get(pattern.constructor);
      if (!conScheme) {
        addError(
          ctx,
          `Module '${pattern.moduleName}' does not export '${pattern.constructor}'`,
          pattern.span,
        );
        return [subst, new Map()];
      }

      // Instantiate constructor type
      const conType = instantiate(ctx, conScheme);

      // Extract argument types from the constructor's function type
      const argTypes: Type[] = [];
      let resultType = conType;
      while (resultType.kind === "TFun") {
        argTypes.push(resultType.param);
        resultType = resultType.ret;
      }

      // Verify arity
      if (argTypes.length !== pattern.args.length) {
        addError(
          ctx,
          `Constructor ${pattern.moduleName}.${pattern.constructor} expects ${argTypes.length} args, got ${pattern.args.length}`,
        );
        return [subst, new Map()];
      }

      // Unify constructor's result type with expected type
      const s1 = unify(ctx, applySubst(subst, resultType), applySubst(subst, expectedType));
      let currentSubst = composeSubst(subst, s1);

      // Recursively infer patterns for constructor arguments
      const allBindings: PatternBindings = new Map();
      for (let i = 0; i < pattern.args.length; i++) {
        const argType = applySubst(currentSubst, argTypes[i]!);
        const [s, bindings] = inferPattern(ctx, env, pattern.args[i]!, argType, currentSubst);
        currentSubst = s;
        for (const [name, type] of bindings) {
          allBindings.set(name, type);
        }
      }

      return [currentSubst, allBindings];
    }

    case "PRecord": {
      const expectedResolved = applySubst(subst, expectedType);

      // If expected type is a variable, build an open record type
      if (expectedResolved.kind === "TVar") {
        const fieldTypes = new Map<string, Type>();
        let currentSubst = subst;
        const allBindings: PatternBindings = new Map();

        for (const field of pattern.fields) {
          const fieldType = freshTypeVar(ctx);
          fieldTypes.set(field.name, fieldType);
          const [s, bindings] = inferPattern(ctx, env, field.pattern, fieldType, currentSubst);
          currentSubst = s;
          for (const [name, type] of bindings) {
            allBindings.set(name, type);
          }
        }

        // Create open record (may have more fields)
        const rowVar = freshTypeVar(ctx);
        const recordType = trecord([...fieldTypes.entries()], rowVar);
        const s = unify(ctx, applySubst(currentSubst, expectedType), recordType);
        return [composeSubst(currentSubst, s), allBindings];
      }

      if (expectedResolved.kind !== "TRecord") {
        addError(ctx, `Cannot match record pattern against ${typeToString(expectedResolved)}`);
        return [subst, new Map()];
      }

      // Match against known record type
      let currentSubst = subst;
      const allBindings: PatternBindings = new Map();

      for (const field of pattern.fields) {
        let fieldType = expectedResolved.fields.get(field.name);

        // Field not in known fields but record is open
        if (!fieldType && expectedResolved.row) {
          const newFieldType = freshTypeVar(ctx);
          const newRowVar = freshTypeVar(ctx);
          const s = unify(
            ctx,
            applySubst(currentSubst, expectedResolved.row),
            trecord([[field.name, newFieldType]], newRowVar),
          );
          currentSubst = composeSubst(currentSubst, s);
          fieldType = applySubst(currentSubst, newFieldType);
        }

        if (!fieldType) {
          addError(
            ctx,
            `Record has no field '${field.name}'. Available: ${[...expectedResolved.fields.keys()].join(", ")}`,
          );
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

      // If expected type is a variable, build a tuple type
      if (expectedResolved.kind === "TVar") {
        const elemTypes: Type[] = [];
        let currentSubst = subst;
        const allBindings: PatternBindings = new Map();

        for (const elem of pattern.elements) {
          const elemType = freshTypeVar(ctx);
          elemTypes.push(elemType);
          const [s, bindings] = inferPattern(ctx, env, elem, elemType, currentSubst);
          currentSubst = s;
          for (const [name, type] of bindings) {
            allBindings.set(name, type);
          }
        }

        const tupleType = ttuple(elemTypes);
        const s = unify(ctx, applySubst(currentSubst, expectedType), tupleType);
        return [composeSubst(currentSubst, s), allBindings];
      }

      if (expectedResolved.kind !== "TTuple") {
        addError(ctx, `Cannot match tuple pattern against ${typeToString(expectedResolved)}`);
        return [subst, new Map()];
      }

      // Verify arity matches
      if (pattern.elements.length !== expectedResolved.elements.length) {
        addError(
          ctx,
          `Tuple pattern has ${pattern.elements.length} elements, but expected ${expectedResolved.elements.length}`,
        );
        return [subst, new Map()];
      }

      // Match element by element
      let currentSubst = subst;
      const allBindings: PatternBindings = new Map();

      for (let i = 0; i < pattern.elements.length; i++) {
        const elemType = expectedResolved.elements[i]!;
        const [s, bindings] = inferPattern(ctx, env, pattern.elements[i]!, elemType, currentSubst);
        currentSubst = s;
        for (const [name, type] of bindings) {
          allBindings.set(name, type);
        }
      }

      return [currentSubst, allBindings];
    }

    case "PAs": {
      // As-pattern: bind name to entire value, also match inner pattern
      const [s, innerBindings] = inferPattern(ctx, env, pattern.pattern, expectedType, subst);
      const boundType = applySubst(s, expectedType);

      // Record type for LSP
      if (pattern.nameSpan) {
        recordType(ctx, pattern.nameSpan, boundType);
      }

      // Add the as-binding (the name bound to the whole value)
      const allBindings: PatternBindings = new Map(innerBindings);
      allBindings.set(pattern.name, boundType);

      return [s, allBindings];
    }

    case "POr": {
      // Or-pattern: all alternatives must match and bind the same variables
      if (pattern.alternatives.length === 0) {
        addError(ctx, "Or-pattern must have at least one alternative");
        return [subst, new Map()];
      }

      // Infer first alternative to get the reference bindings
      const [s1, firstBindings] = inferPattern(
        ctx,
        env,
        pattern.alternatives[0]!,
        expectedType,
        subst,
      );
      let currentSubst = s1;

      // Check remaining alternatives have the same bindings
      for (let i = 1; i < pattern.alternatives.length; i++) {
        const [s2, altBindings] = inferPattern(
          ctx,
          env,
          pattern.alternatives[i]!,
          applySubst(currentSubst, expectedType),
          currentSubst,
        );
        currentSubst = s2;

        // Check that bindings match
        const keys1 = new Set(firstBindings.keys());
        const keys2 = new Set(altBindings.keys());
        const diff1 = [...keys1].filter((k) => !keys2.has(k));
        const diff2 = [...keys2].filter((k) => !keys1.has(k));

        if (diff1.length > 0 || diff2.length > 0) {
          const missing = diff1.length > 0 ? `missing ${diff1.join(", ")}` : "";
          const extra = diff2.length > 0 ? `extra ${diff2.join(", ")}` : "";
          const message = [`Or-pattern alternatives must bind the same variables.`, missing, extra]
            .filter(Boolean)
            .join(" ");
          addError(ctx, message);
          continue;
        }

        for (const [name, type1] of firstBindings) {
          const type2 = altBindings.get(name)!;
          // Unify the types
          const s3 = unify(ctx, applySubst(currentSubst, type1), applySubst(currentSubst, type2));
          currentSubst = composeSubst(currentSubst, s3);
        }
      }

      return [currentSubst, firstBindings];
    }
  }
};

/**
 * Infer the type of a match expression.
 *
 * Algorithm:
 * 1. Infer the scrutinee's type
 * 2. For each case:
 *    a. Infer pattern bindings against scrutinee type
 *    b. Infer body with bindings in scope
 *    c. Unify all body types
 * 3. Check exhaustiveness
 */
const inferMatch = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Match,
): InferResult => {
  if (expr.cases.length === 0) {
    addError(ctx, "Match expression must have at least one case");
    return [new Map(), freshTypeVar(ctx), []];
  }

  // Infer scrutinee type
  const [s1, scrutineeType, c1] = inferExpr(ctx, env, registry, expr.expr);
  let subst = s1;
  let constraints: Constraint[] = [...c1];
  let resultType: Type | null = null;

  // Process each case
  for (const case_ of expr.cases) {
    // Infer pattern bindings
    const [s2, bindings] = inferPattern(
      ctx,
      applySubstEnv(subst, env),
      case_.pattern,
      applySubst(subst, scrutineeType),
      subst,
    );
    subst = s2;

    // Create environment with pattern bindings
    const caseEnv = new Map(applySubstEnv(subst, env));
    for (const [name, type] of bindings) {
      caseEnv.set(name, scheme([], applySubst(subst, type)));
    }

    // Check guard expression if present (must be boolean)
    if (case_.guard) {
      const [sg, guardType, cg] = inferExpr(ctx, caseEnv, registry, case_.guard);
      subst = composeSubst(subst, sg);
      constraints = [...applySubstConstraints(subst, constraints), ...cg];
      const s4 = unify(ctx, applySubst(subst, guardType), tBool);
      subst = composeSubst(subst, s4);
    }

    // Infer body type
    const [s3, bodyType, c2] = inferExpr(ctx, caseEnv, registry, case_.body);
    subst = composeSubst(subst, s3);
    constraints = [...applySubstConstraints(subst, constraints), ...c2];

    // Unify with previous case types
    if (resultType === null) {
      resultType = bodyType;
    } else {
      const s4 = unify(ctx, applySubst(subst, resultType), applySubst(subst, bodyType));
      subst = composeSubst(subst, s4);
      resultType = applySubst(s4, bodyType);
    }
  }

  // Check exhaustiveness
  const lastCase = expr.cases[expr.cases.length - 1];
  let isExhaustive = false;

  if (lastCase) {
    const isWildcardPattern = (p: ast.Pattern): boolean => {
      if (p.kind === "PVar" || p.kind === "PWildcard") return true;
      if (p.kind === "PAs") return isWildcardPattern(p.pattern);
      return false;
    };

    // An unconditional catch-all makes it exhaustive
    if (isWildcardPattern(lastCase.pattern) && !lastCase.guard) {
      isExhaustive = true;
    }

    // A catch-all with a `true` guard is also exhaustive
    if (
      isWildcardPattern(lastCase.pattern) &&
      lastCase.guard?.kind === "Bool" &&
      lastCase.guard.value === true
    ) {
      isExhaustive = true;
    }
  }

  if (!isExhaustive) {
    // Fallback to checking constructors (only unguarded patterns count)
    const patterns = expr.cases.filter((c) => !c.guard).map((c) => c.pattern);
    const missing = checkExhaustiveness(env, registry, applySubst(subst, scrutineeType), patterns);

    if (missing.length > 0) {
      addError(ctx, `Non-exhaustive patterns. Missing: ${missing.join(", ")}`);
    }
  }

  return [subst, applySubst(subst, resultType!), constraints];
};

// =============================================================================
// DATA DECLARATION PROCESSING
// =============================================================================

/**
 * Convert a type expression (from AST) to internal Type representation.
 */
const typeExprToType = (texpr: ast.TypeExpr): Type => {
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

/**
 * Known primitive type names that should be treated as type constructors
 * even though they're parsed as TyVar (lowercase identifiers).
 */
const PRIMITIVE_TYPES = new Set(["Int", "Float", "string", "boolean"]);

/**
 * Convert a type expression to a Type, replacing type variables with fresh ones.
 * This is used for type annotations where user-written type variables like 'a'
 * should become fresh inference variables.
 *
 * Known primitive types (number, string, boolean) are treated as type constructors.
 *
 * Returns the instantiated type and a substitution mapping the fresh variables.
 */
const instantiateTypeExpr = (ctx: CheckContext, texpr: ast.TypeExpr): Type => {
  const varMapping = new Map<string, Type>();

  const convert = (t: ast.TypeExpr): Type => {
    switch (t.kind) {
      case "TyApp":
        return tapp(convert(t.con), convert(t.arg));
      case "TyCon":
        return tcon(t.name);
      case "TyFun":
        return tfun(convert(t.param), convert(t.ret));
      case "TyVar": {
        // Primitive types are type constructors, not variables
        if (PRIMITIVE_TYPES.has(t.name)) {
          return tcon(t.name);
        }
        // Replace type variable with a fresh one (cached for consistency)
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

/**
 * Process a data declaration to produce:
 * 1. Type environment entries for constructors
 * 2. Registry entries for exhaustiveness checking
 *
 * Example: `data Maybe a = Nothing | Just a` produces:
 * - Environment: Nothing : ∀a. Maybe a, Just : ∀a. a -> Maybe a
 * - Registry: Maybe -> [Nothing, Just]
 */
export const processDataDecl = (decl: ast.DataDecl): [TypeEnv, ConstructorRegistry] => {
  const env: TypeEnv = new Map();
  const registry: ConstructorRegistry = new Map();

  // Build the result type: TypeName typeParam1 typeParam2 ...
  let resultType: Type = tcon(decl.name);
  for (const param of decl.typeParams) {
    resultType = tapp(resultType, tvar(param));
  }

  const constructorNames: string[] = [];

  // Process each constructor
  for (const con of decl.constructors) {
    constructorNames.push(con.name);

    // Build constructor type: field1 -> field2 -> ... -> ResultType
    let conType = resultType;
    for (let i = con.fields.length - 1; i >= 0; i--) {
      conType = tfun(typeExprToType(con.fields[i]!), conType);
    }

    // Quantify over all type parameters
    env.set(con.name, scheme([...decl.typeParams], conType));
  }

  registry.set(decl.name, constructorNames);

  return [env, registry];
};

// =============================================================================
// EXHAUSTIVENESS CHECKING
// =============================================================================

/**
 * Extract the type constructor name from a type.
 * Used to look up constructors for exhaustiveness checking.
 */
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

/**
 * Get the arity of a constructor from its type scheme.
 * Counts the number of TFun arrows before reaching the result type.
 */
const getConstructorArity = (env: TypeEnv, conName: string): number => {
  const scheme = env.get(conName);
  if (!scheme) return 0;

  let arity = 0;
  let t = scheme.type;
  while (t.kind === "TFun") {
    arity++;
    t = t.ret;
  }
  return arity;
};

/**
 * Information about a constructor for exhaustiveness checking.
 */
type ConstructorInfo = {
  readonly name: string;
  readonly arity: number;
};

/**
 * Get all constructors for a type with their arities.
 */
const getConstructorsWithArity = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  typeName: string,
): readonly ConstructorInfo[] => {
  const conNames = registry.get(typeName);
  if (!conNames) return [];

  return conNames.map((name) => ({
    name,
    arity: getConstructorArity(env, name),
  }));
};

/**
 * Extract type arguments from a type application.
 * E.g., Maybe (Maybe a) -> [Maybe a]
 *       List number -> [number]
 */
const extractTypeArgs = (type: Type): readonly Type[] => {
  const args: Type[] = [];
  let current = type;
  while (current.kind === "TApp") {
    args.unshift(current.arg);
    current = current.con;
  }
  return args;
};

/**
 * Get the argument types for a constructor when applied to a specific type.
 * Uses the type arguments from the scrutinee type to instantiate the constructor's parameter types.
 */
const getConstructorArgTypes = (
  env: TypeEnv,
  conName: string,
  scrutineeType: Type,
): readonly Type[] => {
  const scheme = env.get(conName);
  if (!scheme) return [];

  // Extract type arguments from the scrutinee type (e.g., Maybe (Maybe a) -> [Maybe a])
  const typeArgs = extractTypeArgs(scrutineeType);

  // Build a substitution from the scheme's type parameters to the type arguments
  const subst: Subst = new Map();
  for (let i = 0; i < scheme.vars.length && i < typeArgs.length; i++) {
    subst.set(scheme.vars[i]!, typeArgs[i]!);
  }

  // Extract argument types from the constructor type and apply the substitution
  const argTypes: Type[] = [];
  let t = scheme.type;
  while (t.kind === "TFun") {
    argTypes.push(applySubst(subst, t.param));
    t = t.ret;
  }
  return argTypes;
};

/**
 * A simplified pattern representation for the exhaustiveness algorithm.
 * We only care about the structure, not the bindings.
 */
type SimplePattern =
  | { kind: "Wild" }
  | { kind: "Con"; name: string; args: readonly SimplePattern[] }
  | { kind: "Tuple"; elements: readonly SimplePattern[] }
  | { kind: "Lit"; value: unknown }
  | { kind: "Or"; alternatives: readonly SimplePattern[] };

/**
 * Convert an AST pattern to a simple pattern for exhaustiveness checking.
 */
const toSimplePattern = (p: ast.Pattern): SimplePattern => {
  switch (p.kind) {
    case "PWildcard":
    case "PVar":
      return { kind: "Wild" };
    case "PCon":
      return { kind: "Con", name: p.name, args: p.args.map(toSimplePattern) };
    case "QualifiedPCon":
      // For exhaustiveness, use just the constructor name (not qualified)
      // since the registry only has unqualified constructor names
      return {
        kind: "Con",
        name: p.constructor,
        args: p.args.map(toSimplePattern),
      };
    case "PTuple":
      return { kind: "Tuple", elements: p.elements.map(toSimplePattern) };
    case "PLit":
      return { kind: "Lit", value: p.value };
    case "PChar":
      return { kind: "Lit", value: p.value };
    case "PAs":
      return toSimplePattern(p.pattern);
    case "POr":
      return { kind: "Or", alternatives: p.alternatives.map(toSimplePattern) };
    case "PRecord":
      // Records are structural, treat as wildcard for exhaustiveness
      return { kind: "Wild" };
  }
};

/**
 * Convert a simple pattern back to a string for error messages.
 */
const simplePatternToString = (p: SimplePattern): string => {
  switch (p.kind) {
    case "Wild":
      return "_";
    case "Con":
      if (p.args.length === 0) return p.name;
      return `${p.name} ${p.args
        .map((a) => {
          const s = simplePatternToString(a);
          return a.kind === "Con" && a.args.length > 0 ? `(${s})` : s;
        })
        .join(" ")}`;
    case "Tuple":
      return `(${p.elements.map(simplePatternToString).join(", ")})`;
    case "Lit":
      return typeof p.value === "string" ? `"${p.value}"` : String(p.value);
    case "Or":
      return p.alternatives.map(simplePatternToString).join(" | ");
  }
};

/**
 * A pattern matrix row - a vector of simple patterns.
 */
type PatternRow = readonly SimplePattern[];

/**
 * A pattern matrix - multiple rows of patterns.
 */
type PatternMatrix = readonly PatternRow[];

/**
 * Specialize a pattern matrix for a specific constructor.
 *
 * For each row in the matrix:
 * - If it starts with the same constructor c(p1...pn), replace with p1...pn followed by the rest
 * - If it starts with a wildcard, expand to n wildcards followed by the rest
 * - If it starts with a different constructor, remove the row
 * - If it starts with an or-pattern, expand and recurse
 */
const specialize = (matrix: PatternMatrix, conName: string, arity: number): PatternMatrix => {
  const result: PatternRow[] = [];

  for (const row of matrix) {
    if (row.length === 0) continue;

    const first = row[0]!;
    const rest = row.slice(1);

    switch (first.kind) {
      case "Con":
        if (first.name === conName) {
          // Same constructor: expand arguments
          result.push([...first.args, ...rest]);
        }
        // Different constructor: row doesn't match, skip it
        break;

      case "Wild":
        // Wildcard matches any constructor: expand to n wildcards
        result.push([...Array(arity).fill({ kind: "Wild" } as SimplePattern), ...rest]);
        break;

      case "Or":
        // Or-pattern: expand each alternative and specialize
        for (const alt of first.alternatives) {
          const expanded = specialize([[alt, ...rest]], conName, arity);
          result.push(...expanded);
        }
        break;

      case "Tuple":
      case "Lit":
        // These don't match constructors, skip
        break;
    }
  }

  return result;
};

/**
 * Specialize a pattern matrix for tuples of a given arity.
 */
const specializeTuple = (matrix: PatternMatrix, arity: number): PatternMatrix => {
  const result: PatternRow[] = [];

  for (const row of matrix) {
    if (row.length === 0) continue;

    const first = row[0]!;
    const rest = row.slice(1);

    switch (first.kind) {
      case "Tuple":
        if (first.elements.length === arity) {
          result.push([...first.elements, ...rest]);
        }
        break;

      case "Wild":
        result.push([...Array(arity).fill({ kind: "Wild" } as SimplePattern), ...rest]);
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

/**
 * Compute the default matrix - rows that match when no constructor matches.
 * This handles the "wildcard" cases.
 */
const defaultMatrix = (matrix: PatternMatrix): PatternMatrix => {
  const result: PatternRow[] = [];

  for (const row of matrix) {
    if (row.length === 0) continue;

    const first = row[0]!;
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
        // Constructor/Tuple/Lit patterns don't contribute to default
        break;
    }
  }

  return result;
};

/**
 * Check if a pattern vector is useful with respect to a pattern matrix.
 *
 * A pattern vector q is useful if there exists a value that:
 * 1. Matches q
 * 2. Does not match any row in the matrix
 *
 * This is the core of the exhaustiveness algorithm.
 * Currently unused but kept for future redundancy checking.
 */
const _isUseful = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  matrix: PatternMatrix,
  pattern: PatternRow,
  types: readonly Type[],
): boolean => {
  // Base case: empty pattern vector
  if (pattern.length === 0) {
    // Useful iff the matrix has no rows (nothing covers the empty case)
    return matrix.length === 0;
  }

  const firstPattern = pattern[0]!;
  const restPattern = pattern.slice(1);
  const firstType = types[0];
  const restTypes = types.slice(1);

  switch (firstPattern.kind) {
    case "Con": {
      // Specialize for this constructor
      const arity = firstPattern.args.length;
      const specialized = specialize(matrix, firstPattern.name, arity);
      const newPattern = [...firstPattern.args, ...restPattern];
      // Get the actual argument types from the constructor and scrutinee type
      const argTypes = firstType ? getConstructorArgTypes(env, firstPattern.name, firstType) : [];
      const finalArgTypes =
        argTypes.length > 0 ? argTypes : Array(arity).fill({ kind: "TVar", name: "_" } as Type);
      return _isUseful(env, registry, specialized, newPattern, [...finalArgTypes, ...restTypes]);
    }

    case "Tuple": {
      const arity = firstPattern.elements.length;
      const specialized = specializeTuple(matrix, arity);
      const newPattern = [...firstPattern.elements, ...restPattern];
      const elemTypes =
        firstType?.kind === "TTuple"
          ? firstType.elements
          : Array(arity).fill({ kind: "TVar", name: "_" } as Type);
      return _isUseful(env, registry, specialized, newPattern, [...elemTypes, ...restTypes]);
    }

    case "Or": {
      // Or-pattern is useful if any alternative is useful
      return firstPattern.alternatives.some((alt) =>
        _isUseful(env, registry, matrix, [alt, ...restPattern], types),
      );
    }

    case "Lit": {
      // Literals: check if this specific literal is useful
      // For simplicity, we treat literals as always potentially useful
      // (proper handling would require tracking covered literals)
      const specialized = matrix.filter((row) => {
        const first = row[0];
        return (
          first?.kind === "Wild" || (first?.kind === "Lit" && first.value === firstPattern.value)
        );
      });
      return _isUseful(
        env,
        registry,
        specialized.map((row) => row.slice(1)),
        restPattern,
        restTypes,
      );
    }

    case "Wild": {
      // Wildcard: check if complete for the type
      if (!firstType) {
        // No type info, check default matrix
        return _isUseful(env, registry, defaultMatrix(matrix), restPattern, restTypes);
      }

      const typeName = getTypeConName(firstType);

      // Handle tuples
      if (firstType.kind === "TTuple") {
        const arity = firstType.elements.length;
        const specialized = specializeTuple(matrix, arity);
        const wildcards: SimplePattern[] = Array(arity).fill({ kind: "Wild" });
        return _isUseful(
          env,
          registry,
          specialized,
          [...wildcards, ...restPattern],
          [...firstType.elements, ...restTypes],
        );
      }

      // Handle ADTs
      if (typeName) {
        const constructors = getConstructorsWithArity(env, registry, typeName);

        if (constructors.length === 0) {
          // Unknown type or no constructors, check default
          return _isUseful(env, registry, defaultMatrix(matrix), restPattern, restTypes);
        }

        // Check if any constructor makes the wildcard useful
        for (const con of constructors) {
          const specialized = specialize(matrix, con.name, con.arity);
          const wildcards: SimplePattern[] = Array(con.arity).fill({ kind: "Wild" });
          // Get the actual argument types from the constructor and scrutinee type
          const argTypes = getConstructorArgTypes(env, con.name, firstType);
          const finalArgTypes =
            argTypes.length > 0
              ? argTypes
              : Array(con.arity).fill({ kind: "TVar", name: "_" } as Type);
          if (
            _isUseful(
              env,
              registry,
              specialized,
              [...wildcards, ...restPattern],
              [...finalArgTypes, ...restTypes],
            )
          ) {
            return true;
          }
        }
        return false;
      }

      // Primitive types or unknown: check default matrix
      return _isUseful(env, registry, defaultMatrix(matrix), restPattern, restTypes);
    }
  }
};

/**
 * Find a witness pattern that is not covered by the matrix.
 * Returns null if all patterns are covered (exhaustive).
 *
 * Uses depth limiting to handle recursive types (List, Tree, etc.)
 * gracefully - after a certain depth, we use wildcards.
 */
const findWitness = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  matrix: PatternMatrix,
  types: readonly Type[],
  depth: number = 0,
): SimplePattern[] | null => {
  // Depth limit to prevent infinite recursion on recursive types
  const MAX_DEPTH = 10;
  if (depth > MAX_DEPTH) {
    // At max depth, if matrix is empty, there's a missing case
    // Use wildcard as the witness
    if (matrix.length === 0 && types.length > 0) {
      return types.map(() => ({ kind: "Wild" }) as SimplePattern);
    }
    return null;
  }

  // Base case: empty pattern vector
  if (types.length === 0) {
    return matrix.length === 0 ? [] : null;
  }

  const firstType = types[0]!;
  const restTypes = types.slice(1);
  const typeName = getTypeConName(firstType);

  // Handle tuples
  if (firstType.kind === "TTuple") {
    const arity = firstType.elements.length;
    const specialized = specializeTuple(matrix, arity);
    const witness = findWitness(
      env,
      registry,
      specialized,
      [...firstType.elements, ...restTypes],
      depth + 1,
    );
    if (witness) {
      const tupleElements = witness.slice(0, arity);
      const rest = witness.slice(arity);
      return [{ kind: "Tuple", elements: tupleElements }, ...rest];
    }
    return null;
  }

  // Handle ADTs
  if (typeName) {
    const constructors = getConstructorsWithArity(env, registry, typeName);

    if (constructors.length === 0) {
      // No constructors known, check if wildcard is useful
      const def = defaultMatrix(matrix);
      const witness = findWitness(env, registry, def, restTypes, depth);
      if (witness) {
        return [{ kind: "Wild" }, ...witness];
      }
      return null;
    }

    // Try each constructor
    for (const con of constructors) {
      const specialized = specialize(matrix, con.name, con.arity);
      // Get the actual argument types from the constructor and scrutinee type
      const argTypes = getConstructorArgTypes(env, con.name, firstType);
      const finalArgTypes =
        argTypes.length > 0 ? argTypes : Array(con.arity).fill({ kind: "TVar", name: "_" } as Type);
      const witness = findWitness(
        env,
        registry,
        specialized,
        [...finalArgTypes, ...restTypes],
        depth + 1,
      );
      if (witness) {
        const args = witness.slice(0, con.arity);
        const rest = witness.slice(con.arity);
        return [{ kind: "Con", name: con.name, args }, ...rest];
      }
    }
    return null;
  }

  // Primitive or unknown type: check default
  const def = defaultMatrix(matrix);
  const witness = findWitness(env, registry, def, restTypes, depth);
  if (witness) {
    return [{ kind: "Wild" }, ...witness];
  }
  return null;
};

/**
 * Check if a set of patterns exhaustively covers all values of a type.
 *
 * Uses the pattern matrix algorithm for proper handling of:
 * - Nested constructor patterns
 * - Tuple patterns
 * - Or-patterns
 * - Wildcards and variable patterns
 *
 * Returns a list of missing pattern examples (empty if exhaustive).
 */
const checkExhaustiveness = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  scrutineeType: Type,
  patterns: readonly ast.Pattern[],
): readonly string[] => {
  // Convert patterns to simple patterns and create the matrix
  const matrix: PatternMatrix = patterns.map((p) => [toSimplePattern(p)]);

  // Find a witness (uncovered pattern)
  const witness = findWitness(env, registry, matrix, [scrutineeType]);

  if (witness && witness.length > 0) {
    return [simplePatternToString(witness[0]!)];
  }

  return [];
};

// =============================================================================
// ENVIRONMENT AND REGISTRY UTILITIES
// =============================================================================

/**
 * Merge multiple type environments into one.
 * Later environments override earlier ones for duplicate names.
 */
export const mergeEnvs = (...envs: readonly TypeEnv[]): TypeEnv => {
  const result: TypeEnv = new Map();
  for (const env of envs) {
    for (const [name, s] of env) {
      result.set(name, s);
    }
  }
  return result;
};

/**
 * Merge multiple constructor registries into one.
 */
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

/**
 * Result of processing data declarations.
 */
export type ProcessedDeclarations = {
  readonly typeEnv: TypeEnv;
  readonly registry: ConstructorRegistry;
  readonly constructorNames: readonly string[];
};

/**
 * Process multiple data declarations and collect their environments, registries, and constructor names.
 */
export const processDeclarations = (
  declarations: readonly ast.DataDecl[],
  initial: ProcessedDeclarations = {
    typeEnv: new Map(),
    registry: new Map(),
    constructorNames: [],
  },
): ProcessedDeclarations => {
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

// =============================================================================
// MODULE PROCESSING
// =============================================================================

/**
 * Process a single module declaration.
 * Returns the module's type information including typed bindings.
 */
export const processModule = (
  mod: ast.ModuleDecl,
  baseEnv?: TypeEnv,
  baseRegistry?: ConstructorRegistry,
): ModuleTypeInfo => {
  // Process data declarations
  const { typeEnv: dataEnv, registry, constructorNames } = processDeclarations(mod.declarations);

  // Merge with base environment (for prelude dependencies)
  const env: TypeEnv = new Map(baseEnv);
  for (const [k, v] of dataEnv) env.set(k, v);

  const fullRegistry: ConstructorRegistry = new Map(baseRegistry);
  for (const [k, v] of registry) fullRegistry.set(k, v);

  // Process foreign bindings - these have declared types but no implementation
  const foreignNames = new Set<string>();
  for (const foreign of mod.foreignBindings) {
    const type = typeExprToType(foreign.type);
    // Generalize over all free type variables (empty env = quantify over all)
    const foreignScheme = generalize(new Map(), type);
    env.set(foreign.name, foreignScheme);
    foreignNames.add(foreign.name);
  }

  // Type-check module bindings using the same algorithm as inferLetRec
  if (mod.bindings.length > 0) {
    // Use empty symbol table for module type-checking (no LSP features needed)
    const emptySymbols: SymbolTable = { definitions: [], references: [] };
    const ctx = createContext(emptySymbols);

    // Step 1: Create placeholder type variables for ALL bindings
    const placeholders = new Map<string, Type>();
    const envWithPlaceholders = new Map(env);

    for (const binding of mod.bindings) {
      const placeholder = freshTypeVar(ctx);
      placeholders.set(binding.name, placeholder);
      envWithPlaceholders.set(binding.name, scheme([], placeholder));
    }

    // Step 2: Infer types for all values with placeholders in scope
    let subst: Subst = new Map();
    const valueTypes = new Map<string, Type>();

    for (const binding of mod.bindings) {
      const [s, valueType] = inferExpr(
        ctx,
        applySubstEnv(subst, envWithPlaceholders),
        fullRegistry,
        binding.value,
      );
      subst = composeSubst(subst, s);
      valueTypes.set(binding.name, valueType);

      // Unify with placeholder to propagate constraints
      const placeholder = applySubst(subst, placeholders.get(binding.name)!);
      const s2 = unify(ctx, placeholder, valueType);
      subst = composeSubst(subst, s2);
    }

    // Step 3: Generalize all bindings and add to environment
    const env1 = applySubstEnv(subst, env);
    for (const binding of mod.bindings) {
      const valueType = applySubst(subst, valueTypes.get(binding.name)!);
      const generalizedScheme = generalize(env1, valueType);
      env.set(binding.name, generalizedScheme);
    }
  }

  return { typeEnv: env, registry: fullRegistry, constructorNames, foreignNames };
};

/**
 * Process multiple module declarations.
 * Modules are processed in order, allowing later modules to depend on earlier ones.
 */
export const processModules = (modules: readonly ast.ModuleDecl[]): ModuleTypeEnv => {
  const result: ModuleTypeEnv = new Map();

  // Accumulate environment as we process modules
  let accEnv: TypeEnv = new Map();
  let accRegistry: ConstructorRegistry = new Map();

  for (const mod of modules) {
    const info = processModule(mod, accEnv, accRegistry);
    result.set(mod.name, info);

    // Accumulate for next module
    for (const [k, v] of info.typeEnv) accEnv.set(k, v);
    for (const [k, v] of info.registry) accRegistry.set(k, v);
  }

  return result;
};

/** Info about a foreign function for code generation */
export type ForeignInfo = {
  readonly module: string;
  readonly name: string;
};

/** Map from local name to foreign function info */
export type ForeignMap = ReadonlyMap<string, ForeignInfo>;

/**
 * Process use statements to build local type environment and aliases.
 */
export const processUseStatements = (
  uses: readonly ast.UseDecl[],
  moduleEnv: ModuleTypeEnv,
): {
  localEnv: TypeEnv;
  localRegistry: ConstructorRegistry;
  constructorNames: string[];
  aliases: Map<string, string>;
  foreignFunctions: ForeignMap;
} => {
  const localEnv: TypeEnv = new Map();
  const localRegistry: ConstructorRegistry = new Map();
  const constructorNames: string[] = [];
  const aliases = new Map<string, string>();
  const foreignFunctions = new Map<string, ForeignInfo>();

  for (const use of uses) {
    // Track alias
    if (use.alias) {
      aliases.set(use.alias, use.moduleName);
    }

    // Get module info
    const mod = moduleEnv.get(use.moduleName);
    if (!mod) {
      // Error will be reported during type checking
      continue;
    }

    // Process imports
    if (use.imports?.kind === "All") {
      // Import everything unqualified
      for (const [name, scheme] of mod.typeEnv) {
        localEnv.set(name, scheme);
        // Track if this is a foreign function
        if (mod.foreignNames.has(name)) {
          foreignFunctions.set(name, { module: use.moduleName, name });
        }
      }
      for (const [typeName, cons] of mod.registry) {
        localRegistry.set(typeName, cons);
      }
      constructorNames.push(...mod.constructorNames);
    } else if (use.imports?.kind === "Specific") {
      // Import specific items
      for (const item of use.imports.items) {
        const scheme = mod.typeEnv.get(item.name);
        if (scheme) {
          localEnv.set(item.name, scheme);
          // Track if this is a foreign function
          if (mod.foreignNames.has(item.name)) {
            foreignFunctions.set(item.name, { module: use.moduleName, name: item.name });
          }
        }

        // Handle constructor imports for types
        if (item.constructors) {
          const cons = mod.registry.get(item.name);
          if (cons) {
            if (item.constructors === "all") {
              // Import all constructors
              localRegistry.set(item.name, cons);
              for (const conName of cons) {
                const conScheme = mod.typeEnv.get(conName);
                if (conScheme) {
                  localEnv.set(conName, conScheme);
                  constructorNames.push(conName);
                }
              }
            } else {
              // Import specific constructors
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
    // If imports is null, only qualified access is available (no unqualified imports)
  }

  return { localEnv, localRegistry, constructorNames, aliases, foreignFunctions };
};

// =============================================================================
// BASE ENVIRONMENT
// =============================================================================

/**
 * Empty base environment.
 * Use the prelude for standard library functions.
 */
export const baseEnv: TypeEnv = new Map();
