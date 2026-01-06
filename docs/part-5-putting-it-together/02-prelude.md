# Chapter 2: The Prelude

The **prelude** is a standard library included in every Algow program. It provides essential data types and functions without requiring imports.

---

## Design Philosophy

The prelude is:

- **Minimal**: Only essential types and functions
- **Built programmatically**: Defined as AST nodes, not parsed from source
- **Type-safe**: Same type checking as user code
- **Automatically injected**: Wrapped around user expressions

---

## Data Types

The prelude defines three fundamental algebraic data types:

### Maybe

For optional values:

```
data Maybe a = Nothing | Just a
```

Defined as:

```typescript
export const maybe = ast.dataDecl(
  "Maybe",
  ["a"],
  [
    ast.conDecl("Nothing", []),
    ast.conDecl("Just", [ast.tyvar("a")])
  ],
);
```

Usage:

```
head Nil           -- Nothing
head (Cons 1 Nil)  -- Just 1
```

### Either

For values that can be one of two types:

```
data Either a b = Left a | Right b
```

Defined as:

```typescript
export const either = ast.dataDecl(
  "Either",
  ["a", "b"],
  [
    ast.conDecl("Left", [ast.tyvar("a")]),
    ast.conDecl("Right", [ast.tyvar("b")])
  ],
);
```

Usage:

```
Left "error"    -- Either string b
Right 42        -- Either a number
```

### List

The fundamental sequence type:

```
data List a = Nil | Cons a (List a)
```

Defined as:

```typescript
export const list = ast.dataDecl(
  "List",
  ["a"],
  [
    ast.conDecl("Nil", []),
    ast.conDecl("Cons", [
      ast.tyvar("a"),
      ast.tyapp(ast.tycon("List"), ast.tyvar("a"))
    ])
  ],
);
```

Usage:

```
Nil                        -- List a
Cons 1 Nil                -- List number
Cons 1 (Cons 2 (Cons 3 Nil))  -- List number
1 :: 2 :: 3 :: Nil        -- Same, using :: syntax
```

---

## List Functions

### map

Transform each element:

```
map : (a -> b) -> List a -> List b
```

```typescript
export const map = ast.letRec(
  [
    ast.recBinding(
      "map",
      ast.abs(
        "f",
        ast.abs(
          "xs",
          ast.match(ast.var_("xs"), [
            ast.case_(ast.pcon("Nil", []), ast.var_("Nil")),
            ast.case_(
              ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
              ast.app(
                ast.app(ast.var_("Cons"), ast.app(ast.var_("f"), ast.var_("x"))),
                ast.app(ast.app(ast.var_("map"), ast.var_("f")), ast.var_("rest")),
              ),
            ),
          ]),
        ),
      ),
    ),
  ],
  ast.var_("map"),
);
```

Usage:

```
map (fn x => x * 2) (1 :: 2 :: 3 :: Nil)
-- Result: 2 :: 4 :: 6 :: Nil
```

### filter

Keep elements matching a predicate:

```
filter : (a -> boolean) -> List a -> List a
```

Usage:

```
filter (fn x => x > 0) (-1 :: 2 :: -3 :: 4 :: Nil)
-- Result: 2 :: 4 :: Nil
```

### foldr

Right fold—process from the right:

```
foldr : (a -> b -> b) -> b -> List a -> b
```

Usage:

```
foldr (fn x => fn acc => x + acc) 0 (1 :: 2 :: 3 :: Nil)
-- Result: 6  (1 + (2 + (3 + 0)))
```

### foldl

Left fold—process from the left:

```
foldl : (b -> a -> b) -> b -> List a -> b
```

Usage:

```
foldl (fn acc => fn x => acc - x) 0 (1 :: 2 :: 3 :: Nil)
-- Result: -6  (((0 - 1) - 2) - 3)
```

### head

Get the first element (if any):

```
head : List a -> Maybe a
```

```typescript
export const head = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil", []), ast.var_("Nothing")),
    ast.case_(
      ast.pcon("Cons", [ast.pvar("x"), ast.pwildcard()]),
      ast.app(ast.var_("Just"), ast.var_("x")),
    ),
  ]),
);
```

Usage:

```
head Nil              -- Nothing
head (1 :: 2 :: Nil)  -- Just 1
```

### tail

Get everything except the first element:

```
tail : List a -> Maybe (List a)
```

Usage:

```
tail Nil              -- Nothing
tail (1 :: 2 :: Nil)  -- Just (2 :: Nil)
```

### length

Count elements:

```
length : List a -> number
```

Usage:

```
length Nil                -- 0
length (1 :: 2 :: 3 :: Nil)  -- 3
```

### isEmpty

Check if empty:

```
isEmpty : List a -> boolean
```

Usage:

```
isEmpty Nil       -- true
isEmpty (1 :: Nil)  -- false
```

### reverse

Reverse a list:

```
reverse : List a -> List a
```

Implemented using `foldl`:

```typescript
export const reverse = ast.abs(
  "xs",
  ast.app(
    ast.app(
      ast.app(
        foldl,
        ast.abs(
          "acc",
          ast.abs("x", ast.app(ast.app(ast.var_("Cons"), ast.var_("x")), ast.var_("acc"))),
        ),
      ),
      ast.var_("Nil"),
    ),
    ast.var_("xs"),
  ),
);
```

### concat

Concatenate two lists:

```
concat : List a -> List a -> List a
```

Implemented using `foldr`:

```typescript
export const concat = ast.abs(
  "xs",
  ast.abs(
    "ys",
    ast.app(
      ast.app(ast.app(foldr, ast.var_("Cons")), ast.var_("ys")),
      ast.var_("xs")
    )
  ),
);
```

---

## Utility Functions

### id

The identity function:

```
id : a -> a
```

```typescript
export const id = ast.abs("x", ast.var_("x"));
```

### const

Create a constant function:

```
const : a -> b -> a
```

```typescript
export const const_ = ast.abs("x", ast.abs("_", ast.var_("x")));
```

### compose

Function composition:

```
compose : (b -> c) -> (a -> b) -> a -> c
```

```typescript
export const compose = ast.abs(
  "f",
  ast.abs(
    "g",
    ast.abs("x", ast.app(ast.var_("f"), ast.app(ast.var_("g"), ast.var_("x"))))
  ),
);
```

Usage:

```
let double = fn x => x * 2 in
let addOne = fn x => x + 1 in
compose double addOne 5  -- 12  (double (addOne 5))
```

### flip

Swap arguments:

```
flip : (a -> b -> c) -> b -> a -> c
```

```typescript
export const flip = ast.abs(
  "f",
  ast.abs(
    "a",
    ast.abs("b", ast.app(ast.app(ast.var_("f"), ast.var_("b")), ast.var_("a")))
  ),
);
```

---

## Injecting the Prelude

The `wrapWithPrelude` function wraps user code with prelude bindings:

```typescript
export const wrapWithPrelude = (expr: ast.Expr): ast.Expr => {
  // Wrap with simple functions (non-recursive)
  let result: ast.Expr = ast.let_("flip", flip, expr);
  result = ast.let_("compose", compose, result);
  result = ast.let_("const", const_, result);
  result = ast.let_("id", id, result);
  result = ast.let_("concat", concat, result);
  result = ast.let_("reverse", reverse, result);

  // Wrap with recursive functions
  result = ast.letRec([ast.recBinding("foldl", foldl.bindings[0]!.value)], result);
  result = ast.letRec([ast.recBinding("foldr", foldr.bindings[0]!.value)], result);
  result = ast.letRec([ast.recBinding("length", length.bindings[0]!.value)], result);
  result = ast.let_("isEmpty", isEmpty, result);
  result = ast.let_("tail", tail, result);
  result = ast.let_("head", head, result);
  result = ast.letRec([ast.recBinding("filter", filter.bindings[0]!.value)], result);
  result = ast.letRec([ast.recBinding("map", map.bindings[0]!.value)], result);

  return result;
};
```

The user's expression becomes the innermost body of nested let expressions.

### Before Wrapping

```
1 + 2
```

### After Wrapping

```
let rec map = ... in
let rec filter = ... in
let head = ... in
let tail = ... in
let isEmpty = ... in
let rec length = ... in
let rec foldr = ... in
let rec foldl = ... in
let reverse = ... in
let concat = ... in
let id = ... in
let const = ... in
let compose = ... in
let flip = ... in
1 + 2
```

---

## Declaration Processing

Data type declarations need separate processing:

```typescript
const prelude = processDeclarations(preludeDeclarations);
const { typeEnv, registry, constructorNames } = processDeclarations(
  parseResult.program.declarations,
  prelude,
);
```

`processDeclarations` extracts:

- Type schemes for constructors (e.g., `Just : ∀a. a -> Maybe a`)
- Registry mapping type names to constructors
- List of constructor names

The prelude's declarations are processed first, then user declarations extend them.

---

## Why Build Programmatically?

The prelude is defined as AST nodes rather than source code because:

1. **No parsing overhead**: AST is ready immediately
2. **No span tracking needed**: Prelude code doesn't have source locations
3. **Guaranteed correct**: AST construction is type-safe
4. **Easy to extend**: Just add more AST nodes
5. **Testable**: Each function can be tested in isolation

---

## Summary

The prelude provides essential types and functions:

**Data Types**:

- `Maybe a` for optional values
- `Either a b` for sum types
- `List a` for sequences

**Functions**:

- List: `map`, `filter`, `foldr`, `foldl`, `head`, `tail`, `length`, `isEmpty`, `reverse`, `concat`
- Utility: `id`, `const`, `compose`, `flip`

All prelude code is:

- Defined as AST nodes
- Type-checked like user code
- Injected via `wrapWithPrelude`

The next chapter covers the diagnostic system for error reporting.
