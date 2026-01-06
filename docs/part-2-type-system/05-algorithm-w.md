# Chapter 5: Algorithm W

Algorithm W is the complete type inference algorithm for Hindley-Milner type systems. Named after Robin Milner, it combines all the pieces we've seen: type representation, substitutions, and unification.

This is the central chapter of the type system documentation—the core of what makes Algow work.

---

## The Big Picture

Algorithm W traverses the AST and for each expression:

1. **Assigns** fresh type variables to unknowns
2. **Infers** types of sub-expressions recursively
3. **Unifies** types based on how they're used
4. **Collects** type class constraints (for operators)
5. **Returns** the inferred type and accumulated substitution

The result is a **principal type**—the most general type that works.

---

## The Inference Result

Each inference step returns three things:

```typescript
type InferResult = [Subst, Type, readonly Constraint[]];
```

- **Subst**: Knowledge learned about type variables
- **Type**: The inferred type of the expression
- **Constraint[]**: Type class constraints (e.g., `Eq a`, `Ord a`)

Constraints are collected during inference and verified at the end.

---

## The Entry Point

The main function orchestrates the entire process:

```typescript
export type CheckOutput = {
  readonly subst: Subst;
  readonly type: Type;
  readonly constraints: readonly Constraint[];
  readonly diagnostics: readonly Diagnostic[];
  readonly types: TypeMap;  // Maps spans to types for LSP
};

export const check = (
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Expr,
  symbols: SymbolTable,
): CheckOutput => {
  // Reset counter for consistent type variable names
  typeVarCounter = 0;

  // Create context for collecting diagnostics
  const ctx = createContext(symbols);

  // Run inference
  const [subst, type, constraints] = inferExpr(ctx, env, registry, expr);

  // Apply final substitution to constraints
  const finalConstraints = applySubstConstraints(subst, constraints);

  // Verify all type class constraints
  solveConstraints(ctx, finalConstraints);

  return {
    subst,
    type,
    constraints: finalConstraints,
    diagnostics: ctx.diagnostics,
    types: ctx.types,
  };
};
```

Key steps:

1. **Reset counter**: Ensures reproducible type variable names (`t0`, `t1`, ...)
2. **Create context**: Holds diagnostics and type information for LSP
3. **Run inference**: The recursive Algorithm W
4. **Apply substitution**: Resolve type variables in constraints
5. **Solve constraints**: Verify type class instances exist

---

## Type Environments

During inference, we track what's in scope:

```typescript
type TypeEnv = Map<string, Scheme>;
```

The environment maps names to type schemes:

- `x` → `∀. number` (monomorphic)
- `id` → `∀a. a -> a` (polymorphic)
- `Cons` → `∀a. a -> List a -> List a` (constructor)

---

## Instantiation

When we use a polymorphic value, we **instantiate** its scheme with fresh type variables:

```typescript
const instantiate = (scheme: Scheme): Type => {
  // Create fresh variables for each quantified variable
  const freshVars: Map<string, Type> = new Map();
  for (const v of scheme.vars) {
    freshVars.set(v, freshTypeVar());
  }

  // Replace quantified variables with fresh ones
  const replaceVars = (type: Type): Type => {
    switch (type.kind) {
      case "TVar":
        return freshVars.get(type.name) ?? type;
      case "TCon":
        return type;
      case "TFun":
        return tfun(replaceVars(type.param), replaceVars(type.ret));
      case "TApp":
        return tapp(replaceVars(type.con), replaceVars(type.arg));
      case "TTuple":
        return ttuple(type.elements.map(replaceVars));
      case "TRecord":
        return trecord(
          [...type.fields.entries()].map(([k, v]) => [k, replaceVars(v)]),
          type.row ? replaceVars(type.row) : undefined
        );
    }
  };

  return replaceVars(scheme.type);
};
```

### Example

Given scheme `∀a. a -> a`:

```
instantiate(∀a. a -> a)
  = t5 -> t5  (with fresh variable t5)
```

Each use gets fresh variables:

```
id 42       -- id instantiated as t5 -> t5, then t5 = number
id "hello"  -- id instantiated as t6 -> t6, then t6 = string
```

---

## Generalization

After inferring a let-bound value, we **generalize** it to a scheme:

```typescript
const generalize = (env: TypeEnv, type: Type): Scheme => {
  // Find free variables in the type
  const typeVars = freeTypeVars(type);

  // Find free variables in the environment
  const envVars = freeTypeVarsEnv(env);

  // Quantify variables in type but not in environment
  const quantified = [...typeVars].filter(v => !envVars.has(v));

  return scheme(quantified, type);
};
```

### Example

```
let id = fn x => x in ...
```

1. Infer `fn x => x`: type is `t0 -> t0`
2. Environment has no free variables
3. Generalize: `∀t0. t0 -> t0`

Now `id` can be used at different types in the body.

### Why Not Generalize Environment Variables?

```
fn f => let g = f in (g 1, g "hello")
```

Here `f` has type `t0` (a free variable from the outer lambda). If we generalized `g` to `∀t0. t0`, each use could have a different type—but `f` only has ONE type!

By excluding environment variables from generalization, we ensure `g` has the same type as `f` throughout.

---

## The Main Inference Function

Here's the complete dispatch:

```typescript
const inferExpr = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Expr,
): InferResult => {
  switch (expr.kind) {
    // Literals - no substitution, no constraints
    case "Num":
      return [new Map(), tNum, []];
    case "Bool":
      return [new Map(), tBool, []];
    case "Str":
      return [new Map(), tStr, []];

    // Simple expressions
    case "Var":
      return inferVar(ctx, env, expr);
    case "Abs":
      return inferAbs(ctx, env, registry, expr);
    case "App":
      return inferApp(ctx, env, registry, expr);

    // Compound expressions
    case "If":
      return inferIf(ctx, env, registry, expr);
    case "Let":
      return inferLet(ctx, env, registry, expr);
    case "LetRec":
      return inferLetRec(ctx, env, registry, expr);
    case "Match":
      return inferMatch(ctx, env, registry, expr);
    case "BinOp":
      return inferBinOp(ctx, env, registry, expr);

    // Data structures
    case "Tuple":
      return inferTuple(ctx, env, registry, expr);
    case "Record":
      return inferRecord(ctx, env, registry, expr);
    case "FieldAccess":
      return inferFieldAccess(ctx, env, registry, expr);
    case "TupleIndex":
      return inferTupleIndex(ctx, env, registry, expr);
  }
};
```

Let's walk through each case in detail.

---

## Inferring Literals

Literals have known types—no inference needed:

```typescript
case "Num":
  return [new Map(), tNum, []];

case "Bool":
  return [new Map(), tBool, []];

case "Str":
  return [new Map(), tStr, []];
```

No substitution (empty `Map`), no constraints (empty array).

---

## Inferring Variables

Look up the variable and instantiate its scheme:

```typescript
const inferVar = (
  ctx: CheckContext,
  env: TypeEnv,
  expr: ast.Var,
): InferResult => {
  const scheme = env.get(expr.name);

  if (!scheme) {
    addError(ctx, `Unknown variable: ${expr.name}`, expr.span);
    return [new Map(), freshTypeVar(), []];
  }

  // Instantiate polymorphic scheme with fresh variables
  const instantiated = instantiate(scheme);

  // Record type for LSP hover
  if (expr.span) {
    recordType(ctx, expr.span, instantiated);
  }

  // Carry forward any constraints from the scheme
  return [new Map(), instantiated, scheme.constraints];
};
```

The key insight: each use of a polymorphic variable gets fresh type variables, allowing different uses to have different types.

---

## Inferring Lambda Abstractions

Lambda inference creates a fresh type for the parameter, then infers the body:

```typescript
const inferAbs = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Abs,
): InferResult => {
  let paramType: Type;
  let subst: Subst = new Map();

  // Handle type annotation if present
  if (expr.paramType) {
    // Convert annotation to type, replacing type variables with fresh ones
    const [annotatedType, tvSubst] = instantiateTypeExpr(expr.paramType);
    paramType = annotatedType;
    subst = tvSubst;
  } else {
    // No annotation - use fresh type variable
    paramType = freshTypeVar();
  }

  // Add parameter to environment (MONOMORPHIC - empty quantifier list)
  const newEnv = new Map(env);
  newEnv.set(expr.param, scheme([], paramType));

  // Infer body type with parameter in scope
  const [bodySubst, bodyType, constraints] = inferExpr(ctx, newEnv, registry, expr.body);
  subst = composeSubst(subst, bodySubst);

  // Record the parameter's final type for LSP
  if (expr.paramSpan) {
    recordType(ctx, expr.paramSpan, applySubst(subst, paramType));
  }

  // Build function type: paramType -> bodyType
  return [subst, tfun(applySubst(subst, paramType), bodyType), constraints];
};
```

### Key Points

1. **Type annotations**: If the parameter has an annotation like `(x : number)`, we use that instead of a fresh variable
2. **Monomorphic binding**: The parameter is added with an empty quantifier list—it cannot be polymorphic inside the body
3. **Substitution applied**: We apply the substitution to the parameter type before building the function type

### Type Annotations on Lambdas

Algow supports type annotations on lambda parameters:

```
fn (x : number) => x + 1
fn (x : number) : number => x + 1  -- with return type too
```

When an annotation is present, `instantiateTypeExpr` converts the surface syntax to an internal type:

```typescript
const instantiateTypeExpr = (texpr: ast.TypeExpr): [Type, Subst] => {
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
        // Primitive types are constructors, not variables
        if (PRIMITIVE_TYPES.has(t.name)) {
          return tcon(t.name);
        }
        // Replace type variable with a fresh one (cached for consistency)
        let fresh = varMapping.get(t.name);
        if (!fresh) {
          fresh = freshTypeVar();
          varMapping.set(t.name, fresh);
        }
        return fresh;
      }
    }
  };

  return [convert(texpr), new Map()];
};
```

This ensures that type variables in annotations (like `a` in `fn (x : a) => x`) get consistent fresh variables.

---

## Inferring Function Application

Application is where unification does the heavy lifting:

```typescript
const inferApp = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.App,
): InferResult => {
  // 1. Infer function type
  const [s1, funcType, c1] = inferExpr(ctx, env, registry, expr.func);

  // 2. Infer argument type (with s1 applied to environment)
  const [s2, paramType, c2] = inferExpr(ctx, applySubstEnv(s1, env), registry, expr.param);

  // 3. Fresh variable for result
  const returnType = freshTypeVar();

  // 4. The function must have type: argType -> returnType
  const s3 = unify(ctx, applySubst(s2, funcType), tfun(paramType, returnType));

  // 5. Compose all substitutions
  const subst = composeSubst(composeSubst(s1, s2), s3);
  const constraints = applySubstConstraints(subst, [...c1, ...c2]);

  return [subst, applySubst(s3, returnType), constraints];
};
```

### Step-by-Step

1. **Infer function**: Get the function's type and substitution
2. **Infer argument**: With the updated environment (substitution applied)
3. **Create result variable**: We don't know the return type yet
4. **Unify**: The function type must match `argType -> resultType`
5. **Compose and apply**: Chain all substitutions, apply to return type

### Example: `id 42`

Given `id : ∀a. a -> a`:

1. Infer `id`: instantiate to `t5 -> t5`
2. Infer `42`: `number`
3. Fresh result: `t6`
4. Unify `t5 -> t5` with `number -> t6`:
   - `t5 = number`
   - `t5 = t6` (from return position)
   - Result: `{ t5 = number, t6 = number }`
5. Return type: `applySubst({ t6 = number }, t6) = number`

---

## Inferring Let Bindings

Let bindings are where polymorphism happens:

```typescript
const inferLet = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Let,
): InferResult => {
  // 1. Infer the value's type
  let [s1, valueType, c1] = inferExpr(ctx, env, registry, expr.value);

  // 2. Handle return type annotation if present
  if (expr.returnType) {
    const [annotatedType] = instantiateTypeExpr(expr.returnType);
    // Peel off function types to get to the return type
    let currentType = applySubst(s1, valueType);
    while (currentType.kind === "TFun") {
      currentType = currentType.ret;
    }
    const s3 = unify(ctx, currentType, annotatedType, expr.nameSpan ?? expr.span);
    s1 = composeSubst(s1, s3);
    valueType = applySubst(s3, valueType);
  }

  // 3. Apply substitution to environment before generalizing
  const env1 = applySubstEnv(s1, env);

  // 4. GENERALIZE: quantify over variables not free in the environment
  const generalizedScheme = generalize(env1, applySubst(s1, valueType));

  // 5. Record the binding's type for LSP
  if (expr.nameSpan) {
    recordType(ctx, expr.nameSpan, applySubst(s1, valueType));
  }

  // 6. Add binding to environment
  const newEnv = new Map(env1);
  newEnv.set(expr.name, generalizedScheme);

  // 7. Infer body type with new binding
  const [s2, bodyType, c2] = inferExpr(ctx, newEnv, registry, expr.body);

  const finalSubst = composeSubst(s1, s2);
  const constraints = applySubstConstraints(finalSubst, [...c1, ...c2]);
  return [finalSubst, bodyType, constraints];
};
```

### Key Points

1. **Infer value first**: Get the type of the right-hand side
2. **Handle annotations**: If there's a return type annotation, unify with it
3. **Apply substitution**: Before generalizing, apply what we've learned
4. **Generalize**: Create a polymorphic scheme
5. **Add to environment**: With the generalized scheme
6. **Infer body**: The body can use the binding polymorphically

### Why This Order Matters

We MUST apply the substitution to the environment BEFORE generalizing. Otherwise, we might generalize variables that should be constrained by the environment.

---

## Inferring Recursive Bindings

Recursive bindings are trickier—the binding is in scope during its own definition:

```typescript
const inferLetRec = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.LetRec,
): InferResult => {
  // 1. Create fresh type variables for all bindings
  const freshTypes: Map<string, Type> = new Map();
  for (const binding of expr.bindings) {
    freshTypes.set(binding.name, freshTypeVar());
  }

  // 2. Extend environment with fresh types (for mutual recursion)
  let newEnv = new Map(env);
  for (const [name, type] of freshTypes) {
    newEnv.set(name, scheme([], type));  // Monomorphic during inference
  }

  // 3. Infer all binding values
  let currentSubst: Subst = new Map();
  let allConstraints: Constraint[] = [];

  for (const binding of expr.bindings) {
    const [s, valueType, c] = inferExpr(
      ctx,
      applySubstEnv(currentSubst, newEnv),
      registry,
      binding.value
    );

    // Unify with expected type
    const expectedType = applySubst(
      composeSubst(currentSubst, s),
      freshTypes.get(binding.name)!
    );
    const unifySubst = unify(ctx, valueType, expectedType, binding.nameSpan);

    currentSubst = composeSubst(composeSubst(currentSubst, s), unifySubst);
    allConstraints = [...applySubstConstraints(currentSubst, allConstraints), ...c];
  }

  // 4. Generalize all bindings
  const finalEnv = applySubstEnv(currentSubst, newEnv);
  for (const binding of expr.bindings) {
    const finalType = applySubst(currentSubst, freshTypes.get(binding.name)!);
    finalEnv.set(binding.name, generalize(finalEnv, finalType));
  }

  // 5. Infer body
  const [bodySubst, bodyType, bodyConstraints] = inferExpr(ctx, finalEnv, registry, expr.body);

  const finalSubst = composeSubst(currentSubst, bodySubst);
  return [finalSubst, bodyType, [...allConstraints, ...bodyConstraints]];
};
```

### Why Fresh Types First?

Consider mutual recursion:

```
let rec isEven n = if n == 0 then true else isOdd (n - 1)
and isOdd n = if n == 0 then false else isEven (n - 1)
```

When inferring `isEven`, we need `isOdd` in scope—but we haven't inferred it yet! The solution:

1. Create fresh types for all bindings
2. Add them to the environment (monomorphically)
3. Infer each binding with all names available
4. Unify inferred types with expected types
5. Generalize after everything is inferred

---

## Inferring Conditionals

Both branches must have the same type:

```typescript
const inferIf = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.If,
): InferResult => {
  // 1. Infer condition type
  const [s1, condType, c1] = inferExpr(ctx, env, registry, expr.cond);

  // 2. Condition must be boolean
  const s2 = unify(ctx, condType, tBool);
  let subst = composeSubst(s1, s2);

  // 3. Infer then branch
  const [s3, thenType, c2] = inferExpr(ctx, applySubstEnv(subst, env), registry, expr.then);
  subst = composeSubst(subst, s3);

  // 4. Infer else branch
  const [s4, elseType, c3] = inferExpr(ctx, applySubstEnv(subst, env), registry, expr.else);
  subst = composeSubst(subst, s4);

  // 5. Both branches must have the same type
  const s5 = unify(ctx, applySubst(s4, thenType), elseType);
  const finalSubst = composeSubst(subst, s5);

  const constraints = applySubstConstraints(finalSubst, [...c1, ...c2, ...c3]);
  return [finalSubst, applySubst(s5, elseType), constraints];
};
```

Key constraints:

- Condition must be `boolean`
- Both branches must unify to the same type

---

## Inferring Binary Operators

Operators introduce type class constraints:

```typescript
const inferBinOp = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.BinOp,
): InferResult => {
  // 1. Infer left operand
  const [s1, leftType, c1] = inferExpr(ctx, env, registry, expr.left);

  // 2. Infer right operand
  const [s2, rightType, c2] = inferExpr(ctx, applySubstEnv(s1, env), registry, expr.right);

  // 3. Operands must have the same type
  const s3 = unify(ctx, applySubst(s2, leftType), rightType, expr.span);
  const operandType = applySubst(s3, rightType);

  const subst = composeSubst(composeSubst(s1, s2), s3);
  const constraints = [...applySubstConstraints(subst, c1), ...applySubstConstraints(subst, c2)];

  // 4. Handle each operator
  switch (expr.op) {
    // Addition: polymorphic (Add class), returns operand type
    case "+": {
      constraints.push({ className: "Add", type: operandType });
      return [subst, operandType, constraints];
    }

    // Other arithmetic: must be number, returns number
    case "-":
    case "/":
    case "*": {
      const s4 = unify(ctx, operandType, tNum, expr.span);
      return [composeSubst(subst, s4), tNum, constraints];
    }

    // Comparison: Ord constraint, returns boolean
    case "<":
    case ">":
    case "<=":
    case ">=": {
      constraints.push({ className: "Ord", type: operandType });
      return [subst, tBool, constraints];
    }

    // Equality: Eq constraint, returns boolean
    case "==":
    case "!=": {
      constraints.push({ className: "Eq", type: operandType });
      return [subst, tBool, constraints];
    }
  }
};
```

### Operator Categories

1. **`+` (Addition)**: Works on numbers and strings via `Add` class
2. **`-`, `*`, `/`**: Only work on numbers
3. **`<`, `>`, `<=`, `>=`**: Work on ordered types via `Ord` class
4. **`==`, `!=`**: Work on equatable types via `Eq` class (most types except functions)

The constraints are collected and verified after inference completes.

---

## Inferring Tuples

Tuples infer each element's type:

```typescript
const inferTuple = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Tuple,
): InferResult => {
  // Empty tuples are errors
  if (expr.elements.length === 0) {
    addError(ctx, "Tuples cannot be empty");
    return [new Map(), ttuple([]), []];
  }

  // Single element - just return that element's type (not a tuple)
  if (expr.elements.length === 1) {
    return inferExpr(ctx, env, registry, expr.elements[0]!);
  }

  let subst: Subst = new Map();
  let constraints: Constraint[] = [];
  const types: Type[] = [];

  // Infer each element's type, threading substitution through
  for (const elem of expr.elements) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), registry, elem);
    subst = composeSubst(subst, s);
    constraints = [...applySubstConstraints(subst, constraints), ...c];
    types.push(applySubst(subst, t));
  }

  return [subst, ttuple(types), constraints];
};
```

A tuple `(1, "hello", true)` gets type `(number, string, boolean)`.

---

## Inferring Records

Records create a closed record type:

```typescript
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

  // Return a CLOSED record type (no row variable)
  return [subst, trecord([...fieldTypes.entries()]), constraints];
};
```

A record literal `{ x = 1, y = "hi" }` gets the closed type `{ x : number, y : string }`.

---

## Inferring Field Access

Field access leverages row polymorphism:

```typescript
const inferFieldAccess = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.FieldAccess,
): InferResult => {
  // 1. Infer the record expression's type
  const [s1, recordType, constraints] = inferExpr(ctx, env, registry, expr.record);
  const resolvedType = applySubst(s1, recordType);

  // Case 1: Type variable - constrain to open record with the field
  if (resolvedType.kind === "TVar") {
    const fieldType = freshTypeVar();
    const rowVar = freshTypeVar();
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
    return [s1, freshTypeVar(), constraints];
  }

  // Case 2: Field exists in known fields
  const fieldType = resolvedType.fields.get(expr.field);
  if (fieldType) {
    return [s1, fieldType, constraints];
  }

  // Case 3: Field not in known fields but record is open - constrain the row
  if (resolvedType.row) {
    const newFieldType = freshTypeVar();
    const newRowVar = freshTypeVar();
    const s2 = unify(ctx, resolvedType.row, trecord([[expr.field, newFieldType]], newRowVar));
    return [composeSubst(s1, s2), applySubst(s2, newFieldType), constraints];
  }

  // Case 4: Closed record without the field - error
  addError(
    ctx,
    `Record has no field '${expr.field}'. Available: ${[...resolvedType.fields.keys()].join(", ")}`,
  );
  return [s1, freshTypeVar(), constraints];
};
```

### Four Cases

1. **Type variable**: We don't know what type the expression has yet. Create an open record type with the required field.
2. **Known field**: The record has the field—return its type.
3. **Open record, unknown field**: The record is open (has a row variable). Constrain the row variable to include the field.
4. **Closed record, missing field**: Error—the record doesn't have that field.

This enables functions that work on any record with a specific field:

```
let getX r = r.x
-- getX : { x : a | ρ } -> a
```

---

## Inferring Tuple Index

Tuple indexing extracts an element:

```typescript
const inferTupleIndex = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.TupleIndex,
): InferResult => {
  // 1. Infer the tuple expression's type
  const [s1, tupleType, constraints] = inferExpr(ctx, env, registry, expr.tuple);
  const resolvedType = applySubst(s1, tupleType);

  // Case 1: Type variable - we don't know the tuple arity yet
  if (resolvedType.kind === "TVar") {
    const elementType = freshTypeVar();
    return [s1, elementType, constraints];
  }

  // Must be a tuple type
  if (resolvedType.kind !== "TTuple") {
    addError(ctx, `Cannot index into non-tuple type: ${typeToString(resolvedType)}`);
    return [s1, freshTypeVar(), constraints];
  }

  // Check bounds
  if (expr.index < 0 || expr.index >= resolvedType.elements.length) {
    addError(
      ctx,
      `Tuple index ${expr.index} out of bounds for tuple of ${resolvedType.elements.length} element(s)`,
    );
    return [s1, freshTypeVar(), constraints];
  }

  // Return the type at the specified index
  return [s1, resolvedType.elements[expr.index]!, constraints];
};
```

For `pair.0` where `pair : (number, string)`, the result type is `number`.

---

## Inferring Pattern Matching

Match expressions require inferring patterns and checking exhaustiveness:

```typescript
const inferMatch = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: ast.Match,
): InferResult => {
  // 1. Infer scrutinee type
  const [s1, scrutineeType, c1] = inferExpr(ctx, env, registry, expr.scrutinee);
  let subst = s1;
  let constraints = [...c1];

  // 2. Fresh type for the result (all arms must match)
  const resultType = freshTypeVar();

  // 3. Check each arm
  for (const arm of expr.arms) {
    // Infer pattern bindings against scrutinee type
    const [patSubst, bindings] = inferPattern(
      ctx,
      env,
      arm.pattern,
      applySubst(subst, scrutineeType),
      subst
    );
    subst = composeSubst(subst, patSubst);

    // Extend environment with pattern bindings
    const armEnv = new Map(applySubstEnv(subst, env));
    for (const [name, type] of bindings) {
      armEnv.set(name, scheme([], type));
    }

    // Check guard if present (must be boolean)
    if (arm.guard) {
      const [guardSubst, guardType, guardConstraints] = inferExpr(
        ctx, armEnv, registry, arm.guard
      );
      subst = composeSubst(subst, guardSubst);
      const unifySubst = unify(ctx, guardType, tBool);
      subst = composeSubst(subst, unifySubst);
      constraints = [...constraints, ...guardConstraints];
    }

    // Infer body type
    const [bodySubst, bodyType, bodyConstraints] = inferExpr(
      ctx, armEnv, registry, arm.body
    );
    subst = composeSubst(subst, bodySubst);
    constraints = [...constraints, ...bodyConstraints];

    // Unify body type with result type
    const unifySubst = unify(ctx, bodyType, applySubst(subst, resultType));
    subst = composeSubst(subst, unifySubst);
  }

  // 4. Check exhaustiveness
  const patterns = expr.arms.map(arm => arm.pattern);
  checkExhaustiveness(ctx, registry, applySubst(subst, scrutineeType), patterns);

  return [subst, applySubst(subst, resultType), constraints];
};
```

### Pattern Inference

Each pattern kind has its own inference:

```typescript
const inferPattern = (
  ctx: CheckContext,
  env: TypeEnv,
  pattern: ast.Pattern,
  expectedType: Type,
  subst: Subst,
): [Subst, PatternBindings] => {
  switch (pattern.kind) {
    // Variable: binds to expected type
    case "PVar": {
      const bindings = new Map<string, Type>();
      bindings.set(pattern.name, expectedType);
      return [subst, bindings];
    }

    // Wildcard: matches anything, binds nothing
    case "PWildcard":
      return [subst, new Map()];

    // Literal: unify expected with literal's type
    case "PLit": {
      const litType = typeof pattern.value === "number" ? tNum
                    : typeof pattern.value === "string" ? tStr
                    : tBool;
      const s = unify(ctx, expectedType, litType);
      return [composeSubst(subst, s), new Map()];
    }

    // Constructor: look up, unify, recurse into arguments
    case "PCon": {
      const conScheme = env.get(pattern.name);
      if (!conScheme) {
        addError(ctx, `Unknown constructor: ${pattern.name}`);
        return [subst, new Map()];
      }

      const conType = instantiate(conScheme);
      // ... unify and recurse into subpatterns
    }

    // Tuple: build tuple type and match elements
    case "PTuple": {
      // ... similar to constructor
    }

    // Record: handle row polymorphism for partial matches
    case "PRecord": {
      // ... match known fields, allow extra via row variable
    }
  }
};
```

---

## Complete Example Trace

Let's trace through:

```
let id = fn x => x in id 42
```

### Step 1: Infer `fn x => x`

1. Fresh type `t0` for parameter `x`
2. Add `x : t0` to environment (monomorphic)
3. Infer body `x`: look up `x`, get `t0`
4. Build function type: `t0 -> t0`
5. Result: `(∅, t0 -> t0, [])`

### Step 2: Generalize

1. Type: `t0 -> t0`
2. Environment free vars: `{}`
3. Type free vars: `{t0}`
4. Quantify: `t0`
5. Scheme: `∀t0. t0 -> t0`

### Step 3: Add to environment

Environment now has: `{ id: ∀t0. t0 -> t0 }`

### Step 4: Infer `id 42`

1. Look up `id`: scheme `∀t0. t0 -> t0`
2. Instantiate: `t1 -> t1` (fresh `t1`)
3. Infer `42`: `number`
4. Fresh result variable: `t2`
5. Unify `t1 -> t1` with `number -> t2`:
   - Unify `t1` with `number`: `{ t1 = number }`
   - Apply: `t1` becomes `number`
   - Unify `number` (was `t1`) with `t2`: `{ t2 = number }`
   - Compose: `{ t1 = number, t2 = number }`
6. Apply to result: `applySubst({...}, t2) = number`

### Final Result

The expression `let id = fn x => x in id 42` has type `number`.

---

## Summary

Algorithm W is the complete inference algorithm:

1. **Literals**: Known types directly
2. **Variables**: Instantiate polymorphic schemes
3. **Lambdas**: Fresh parameter type, infer body (with optional type annotations)
4. **Applications**: Unify function type with `arg -> result`
5. **Let bindings**: Infer, generalize, extend environment
6. **Recursive bindings**: Pre-declare types, infer, unify, generalize
7. **Conditionals**: Condition boolean, branches same type
8. **Operators**: Type class constraints for polymorphic operators
9. **Tuples/Records**: Infer elements, build compound type
10. **Field/Index access**: Row polymorphism for records, bounds check for tuples
11. **Pattern matching**: Infer patterns, check exhaustiveness

The result is always the **principal type**—the most general type that works. No type annotations required, but they're supported when you want them.

The next chapter explores let polymorphism in more depth—why it's special and how it enables code reuse.
