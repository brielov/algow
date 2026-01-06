# Chapter 2: Type Representation

Before we can infer types, we need to represent them in code. This chapter explores how Algow represents types internally.

---

## The Type Union

Types are represented as a discriminated union, just like AST nodes:

```typescript
export type Type =
  | TVar     // Type variable (unknown type)
  | TCon     // Type constructor (named type)
  | TFun     // Function type
  | TApp     // Type application
  | TRecord  // Record type
  | TTuple;  // Tuple type
```

Each variant has a `kind` field for type-safe pattern matching.

---

## Type Variables (TVar)

A **type variable** represents an unknown type to be determined during inference:

```typescript
export type TVar = {
  readonly kind: "TVar";
  readonly name: string;  // e.g., "t0", "t1", "a", "b"
};
```

When we see `fn x => x`, we don't know what type `x` has. We create a fresh type variable:

```
x : t0
fn x => x : t0 -> t0
```

During inference, we discover constraints on type variables and eventually learn their concrete types.

### Fresh Variables

We generate unique names for type variables:

```typescript
let typeVarCounter = 0;

const freshTypeVar = (): Type => {
  return { kind: "TVar", name: `t${typeVarCounter++}` };
};
```

Each call produces a new variable: `t0`, `t1`, `t2`, etc.

---

## Type Constructors (TCon)

A **type constructor** is a named type:

```typescript
export type TCon = {
  readonly kind: "TCon";
  readonly name: string;
};
```

Built-in types:

```typescript
const tNum = { kind: "TCon", name: "number" };
const tStr = { kind: "TCon", name: "string" };
const tBool = { kind: "TCon", name: "boolean" };
```

User-defined types are also type constructors:

```typescript
const tList = { kind: "TCon", name: "List" };
const tMaybe = { kind: "TCon", name: "Maybe" };
```

---

## Function Types (TFun)

Functions map one type to another:

```typescript
export type TFun = {
  readonly kind: "TFun";
  readonly param: Type;  // Input type
  readonly ret: Type;    // Output type
};
```

Examples:

```
number -> number          -- TFun(TCon("number"), TCon("number"))
a -> a                    -- TFun(TVar("a"), TVar("a"))
number -> string -> boolean  -- TFun(number, TFun(string, boolean))
```

Function types are **right-associative**:

- `a -> b -> c` means `a -> (b -> c)`
- A function that takes `a` and returns a function from `b` to `c`

This matches curried functions: `add : number -> number -> number` takes a number and returns a function that takes another number.

---

## Type Application (TApp)

Type application applies a parameterized type to an argument:

```typescript
export type TApp = {
  readonly kind: "TApp";
  readonly con: Type;  // The type being applied
  readonly arg: Type;  // The type argument
};
```

Examples:

```
List number    -- TApp(TCon("List"), TCon("number"))
Maybe string   -- TApp(TCon("Maybe"), TCon("string"))
```

For multiple parameters, we nest applications (left-associative):

```
Either string number
  = TApp(TApp(TCon("Either"), TCon("string")), TCon("number"))
```

This mirrors function application: `Either string number` is like calling `Either` with `string`, then calling the result with `number`.

---

## Record Types (TRecord)

Records have named fields and optional row polymorphism:

```typescript
export type TRecord = {
  readonly kind: "TRecord";
  readonly fields: ReadonlyMap<string, Type>;
  readonly row: Type | null;  // Row variable for open records
};
```

### Closed Records

A closed record has exactly the specified fields:

```typescript
// { x: number, y: number }
{
  kind: "TRecord",
  fields: new Map([["x", tNum], ["y", tNum]]),
  row: null
}
```

### Open Records (Row Polymorphism)

An open record may have additional unknown fields:

```typescript
// { x: number | ρ } -- has x, maybe more
{
  kind: "TRecord",
  fields: new Map([["x", tNum]]),
  row: { kind: "TVar", name: "ρ" }
}
```

This enables functions like:

```
let getX r = r.x
-- getX : { x: a | ρ } -> a
```

The function works on ANY record with an `x` field, regardless of other fields:

```
getX { x = 1 }              -- OK
getX { x = 1, y = 2 }       -- OK
getX { x = 1, name = "hi" } -- OK
getX { y = 2 }              -- Error: no x field
```

---

## Tuple Types (TTuple)

Tuples are fixed-length, heterogeneous collections:

```typescript
export type TTuple = {
  readonly kind: "TTuple";
  readonly elements: readonly Type[];
};
```

Examples:

```typescript
// (number, string)
{ kind: "TTuple", elements: [tNum, tStr] }

// (number, string, boolean)
{ kind: "TTuple", elements: [tNum, tStr, tBool] }
```

Unlike records, tuples use positional access (`.0`, `.1`) rather than names.

---

## Smart Constructors

Helper functions make type construction concise:

```typescript
const tvar = (name: string): Type =>
  ({ kind: "TVar", name });

const tcon = (name: string): Type =>
  ({ kind: "TCon", name });

const tfun = (param: Type, ret: Type): Type =>
  ({ kind: "TFun", param, ret });

const tapp = (con: Type, arg: Type): Type =>
  ({ kind: "TApp", con, arg });

const trecord = (fields: [string, Type][], row: Type | null = null): Type =>
  ({ kind: "TRecord", fields: new Map(fields), row });

const ttuple = (elements: Type[]): Type =>
  ({ kind: "TTuple", elements });
```

---

## Pretty Printing Types

For error messages and debugging, we convert types to readable strings:

```typescript
const typeToString = (type: Type): string => {
  switch (type.kind) {
    case "TVar":
      return type.name;

    case "TCon":
      return type.name;

    case "TFun": {
      const param = type.param.kind === "TFun"
        ? `(${typeToString(type.param)})`  // Parenthesize nested functions on left
        : typeToString(type.param);
      return `${param} -> ${typeToString(type.ret)}`;
    }

    case "TApp":
      return `${typeToString(type.con)} ${typeToString(type.arg)}`;

    case "TRecord": {
      const fieldStrs = [...type.fields.entries()]
        .map(([name, t]) => `${name}: ${typeToString(t)}`);
      const rowStr = type.row ? ` | ${typeToString(type.row)}` : "";
      return `{ ${fieldStrs.join(", ")}${rowStr} }`;
    }

    case "TTuple":
      return `(${type.elements.map(typeToString).join(", ")})`;
  }
};
```

Examples:

```
typeToString(tfun(tNum, tNum))           // "number -> number"
typeToString(tfun(tNum, tfun(tStr, tBool)))  // "number -> string -> boolean"
typeToString(tapp(tcon("List"), tNum))   // "List number"
```

---

## Free Type Variables

We often need to find which type variables appear in a type:

```typescript
const freeTypeVars = (type: Type): Set<string> => {
  switch (type.kind) {
    case "TVar":
      return new Set([type.name]);

    case "TCon":
      return new Set();

    case "TFun":
      return freeTypeVars(type.param).union(freeTypeVars(type.ret));

    case "TApp":
      return freeTypeVars(type.con).union(freeTypeVars(type.arg));

    case "TRecord": {
      const vars = new Set<string>();
      for (const fieldType of type.fields.values()) {
        for (const v of freeTypeVars(fieldType)) {
          vars.add(v);
        }
      }
      if (type.row) {
        for (const v of freeTypeVars(type.row)) {
          vars.add(v);
        }
      }
      return vars;
    }

    case "TTuple":
      return type.elements.reduce(
        (acc, t) => acc.union(freeTypeVars(t)),
        new Set<string>()
      );
  }
};
```

This is used for:

- **Occurs check**: Preventing infinite types
- **Generalization**: Finding which variables to quantify
- **Substitution**: Knowing what to replace

---

## Type Schemes

A **type scheme** represents a polymorphic type with quantified variables:

```typescript
export type Scheme = {
  readonly vars: readonly string[];      // Quantified variables
  readonly constraints: readonly Constraint[];  // Type class constraints
  readonly type: Type;                   // The type
};
```

Example: The identity function has scheme `∀a. a -> a`:

```typescript
{
  vars: ["a"],
  constraints: [],
  type: tfun(tvar("a"), tvar("a"))
}
```

The `vars` list says "these variables are universally quantified"—they can be instantiated to any type when the scheme is used.

---

## Summary

Types in Algow are represented as:

| Type      | Description            | Example            |
| --------- | ---------------------- | ------------------ |
| `TVar`    | Unknown type variable  | `t0`, `a`          |
| `TCon`    | Named type constructor | `number`, `List`   |
| `TFun`    | Function type          | `number -> string` |
| `TApp`    | Type application       | `List number`      |
| `TRecord` | Record with fields     | `{ x: number }`    |
| `TTuple`  | Fixed-size tuple       | `(number, string)` |

Type schemes add polymorphism:

- **Scheme**: `∀a b. constraints => type`
- **Quantified vars**: Can be instantiated to any type

The next chapter explains substitutions—how we track and propagate type information during inference.
