# Algow Language Syntax Specification

This document defines the complete syntax for the Algow language.

## Lexical Elements

### Keywords

```
let, rec, in, if, then, else, match, when, end, type, true, false, as, and, module, use
```

### Operators

```
+  -  *  /           -- arithmetic (binary)
-                    -- negation (unary, desugars to 0 - x)
<  <=  >  >=         -- comparison
==  !=               -- equality
&&  ||               -- logical (short-circuit)
|>                   -- pipe (flip application)
::                   -- cons (desugars to Cons x xs)
->                   -- lambda arrow / function type
=                    -- binding
|                    -- or-pattern / ADT union
,                    -- tuple/record separator
.                    -- field access / qualified access
_                    -- wildcard pattern
:                    -- type annotation
```

### Delimiters

```
(  )                 -- grouping, tuples
{  }                 -- records
[  ]                 -- list literals
```

### Identifiers

```
lowercase    = [a-z_][a-zA-Z0-9_]*     -- variables: x, foo, bar_baz
uppercase    = [A-Z][a-zA-Z0-9_]*      -- constructors/modules: Nil, Just, List
```

### Literals

```
number       = [0-9]+(\.[0-9]+)?       -- 42, 3.14
string       = "([^"\\]|\\.)*"         -- "hello", "line\n"
boolean      = true | false
```

### Comments

```
line_comment  = --[^\n]*               -- single line
block_comment = {-...-}                -- multi-line (can nest)
```

### Whitespace

Whitespace (spaces, tabs, newlines) is ignored except as token separator.
Newlines are significant only for separating top-level declarations.

---

## Grammar

### Program

```
program      = module_decl* use_decl* declaration* expression?

module_decl  = "module" UPPER declaration* "end"

use_decl     = "use" UPPER import_spec? ("as" UPPER)?

import_spec  = "(..)"                              -- import all
             | "(" import_item ("," import_item)* ")"

import_item  = LOWER                               -- function
             | UPPER                               -- type (no constructors)
             | UPPER "(..)"                        -- type with all constructors
             | UPPER "(" UPPER ("," UPPER)* ")"    -- type with specific constructors

declaration  = type_decl
             | let_decl
```

### Type Declarations

```
type_decl    = "type" UPPER type_var* "=" constructor ("|" constructor)*

constructor  = UPPER type_atom*

type_var     = LOWER
```

Examples:

```
type Bool = True | False
type Maybe a = Nothing | Just a
type List a = Nil | Cons a (List a)
type Either a b = Left a | Right b
```

### Let Declarations

```
let_decl     = "let" "rec"? LOWER param* (":" type)? "=" expression
               ("and" LOWER param* (":" type)? "=" expression)*
             | "let" pattern "=" expression    -- destructuring (non-recursive only)

param        = LOWER
             | "(" LOWER ":" type ")"          -- annotated parameter

pattern      = LOWER                           -- variable
             | "_"                             -- wildcard
             | literal                         -- number, string, boolean
             | UPPER pattern*                  -- constructor
             | UPPER "." UPPER pattern*        -- qualified constructor
             | "(" pattern ("," pattern)+ ")"  -- tuple
             | "{" field_pat ("," field_pat)* "}"  -- record
             | pattern "as" LOWER              -- as-pattern
             | pattern "|" pattern             -- or-pattern

field_pat    = LOWER "=" pattern
             | LOWER                              -- punning: { x } means { x = x }
```

Examples:

```
let x = 42
let add x y = x + y
let rec fact n = if n == 0 then 1 else n * fact (n - 1)

-- Mutual recursion with 'and'
let rec isEven n = if n == 0 then true else isOdd (n - 1)
and isOdd n = if n == 0 then false else isEven (n - 1)

-- Type annotations
let add : number -> number -> number = x y -> x + y
let apply (f : number -> number) (x : number) = f x

-- Destructuring (desugars to match expressions)
let (x, y) = (10, 20)
let {name = n, age = a} = person
let ((a, b), c) = nested

-- Wildcard to ignore values
let _ = sideEffect in result
let (x, _) = pair in x          -- extract first, ignore second
```

### Expressions

```
expression   = let_expr
             | if_expr
             | match_expr
             | lambda_expr
             | binary_expr

let_expr     = "let" "rec"? LOWER param* (":" type)? "=" expression "in" expression
               ("and" LOWER param* (":" type)? "=" expression)*
             | "let" pattern "=" expression "in" expression  -- destructuring

if_expr      = "if" expression "then" expression "else" expression

match_expr   = "match" expression match_arm+ "end"

match_arm    = "when" pattern guard? "->" expression

guard        = "if" expression                    -- pattern guard (must be boolean)

lambda_expr  = LOWER+ "->" expression             -- x -> body or x y z -> body
             | "(" LOWER ":" type ")" "->" expression  -- annotated lambda
```

### Binary Expressions (by precedence, low to high)

```
binary_expr  = pipe_expr

pipe_expr        = cons_expr ("|>" cons_expr)*
cons_expr        = or_expr ("::" or_expr)*        -- right-associative
or_expr          = and_expr ("||" and_expr)*
and_expr         = equality_expr ("&&" equality_expr)*
equality_expr    = comparison_expr (("==" | "!=") comparison_expr)*
comparison_expr  = additive_expr (("<" | "<=" | ">" | ">=") additive_expr)*
additive_expr    = multiplicative_expr (("+" | "-") multiplicative_expr)*
multiplicative_expr = unary_expr (("*" | "/") unary_expr)*
unary_expr       = "-" unary_expr | app_expr   -- negation or application

app_expr     = primary_expr+                   -- function application
```

Note: `x |> f` desugars to `f x` (flip application).
Note: `x :: xs` desugars to `Cons x xs`.
Note: `a && b` desugars to `if a then b else false` (short-circuit).
Note: `a || b` desugars to `if a then true else b` (short-circuit).
Note: `-x` desugars to `0 - x` (unary negation).

### Primary Expressions

```
primary_expr = LOWER                           -- variable
             | UPPER                           -- constructor
             | UPPER "." LOWER                 -- qualified variable
             | UPPER "." UPPER                 -- qualified constructor
             | literal                         -- number, string, boolean
             | "(" expression ")"              -- grouping
             | tuple_expr
             | record_expr
             | list_expr
             | field_access
             | tuple_index

tuple_expr   = "(" expression "," expression ("," expression)* ")"

record_expr  = "{" field ("," field)* "}"
field        = LOWER "=" expression
             | LOWER                             -- punning: { x } means { x = x }

list_expr    = "[" (expression ("," expression)*)? "]"
```

Note: `[1, 2, 3]` desugars to `Cons 1 (Cons 2 (Cons 3 Nil))`.

```
field_access = primary_expr "." LOWER
tuple_index  = primary_expr "." NUMBER         -- tuple.0, tuple.1
```

Note: `tuple.0.1` parses as `tuple.(0.1)` due to float literal parsing.
Use parentheses for chained indexing: `(tuple.0).1`

### Types (in annotations and data declarations)

```
type         = type_app ("->" type)?           -- function type (right-associative)

type_app     = type_atom+                      -- left-associative

type_atom    = LOWER                           -- type variable: a, b
             | UPPER                           -- type constructor: List, Maybe
             | "(" type ")"                    -- grouping
```

---

## Operator Precedence (low to high)

| Precedence | Operators            | Associativity |
| ---------- | -------------------- | ------------- |
| 1          | `\|>` (pipe)         | left          |
| 2          | `::` (cons)          | right         |
| 3          | `\|\|`               | left          |
| 4          | `&&`                 | left          |
| 5          | `==` `!=`            | left          |
| 6          | `<` `<=` `>` `>=`    | left          |
| 7          | `+` `-`              | left          |
| 8          | `*` `/`              | left          |
| 9          | function application | left          |
| 10         | `.` (field access)   | left          |

---

## Complete Examples

### Lambda Expressions

```
-- Single parameter
x -> x + 1

-- Multiple parameters (curried syntax)
x y -> x + y              -- desugars to x -> y -> x + y
x y z -> x + y + z        -- desugars to x -> y -> z -> x + y + z

-- Used inline with higher-order functions
map (x -> x * 2) list
filter (n -> n > 0) numbers
foldr (x acc -> x + acc) 0 list

-- With pipe operator (reads left-to-right)
list |> map (x -> x * 2)
numbers |> filter (n -> n > 0) |> map (x -> x * 2)

-- Annotated lambda
(x : number) -> x + 1
```

### Named Functions

```
-- Identity function (inferred: a -> a)
let id x = x

-- Constant function (inferred: a -> b -> a)
let const x y = x

-- Function composition (inferred: (b -> c) -> (a -> b) -> a -> c)
let compose f g x = f (g x)
```

### Recursive Functions

```
-- Factorial (inferred: number -> number)
let rec fact n =
  if n == 0 then 1
  else n * fact (n - 1)

-- Fibonacci (inferred: number -> number)
let rec fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

-- Mutual recursion
let rec isEven n = if n == 0 then true else isOdd (n - 1)
and isOdd n = if n == 0 then false else isEven (n - 1)

-- Using pattern guards
let abs n = match n
  when x if x < 0 -> 0 - x
  when x -> x
end

let sign n = match n
  when x if x > 0 -> 1
  when x if x < 0 -> 0 - 1
  when _ -> 0
end

-- Using as-patterns to bind whole and parts
type List a = Nil | Cons a (List a)

let rec duplicate xs = match xs
  when Nil -> Nil
  when Cons x rest as whole -> Cons whole (duplicate rest)
end
```

### Algebraic Data Types

```
type Maybe a = Nothing | Just a

type List a = Nil | Cons a (List a)

-- Safe head (inferred: List a -> Maybe a)
let head xs = match xs
  when Nil -> Nothing
  when Cons x _ -> Just x
end

-- Map over list (inferred: (a -> b) -> List a -> List b)
let rec map f xs = match xs
  when Nil -> Nil
  when Cons x rest -> Cons (f x) (map f rest)
end

-- Filter list (inferred: (a -> boolean) -> List a -> List a)
let rec filter p xs = match xs
  when Nil -> Nil
  when Cons x rest ->
      if p x then Cons x (filter p rest)
      else filter p rest
end

-- Fold right (inferred: (a -> b -> b) -> b -> List a -> b)
let rec foldr f z xs = match xs
  when Nil -> z
  when Cons x rest -> f x (foldr f z rest)
end
```

### Or-Patterns

```
-- Match multiple patterns with the same result
let isSmall n = match n
  when 0 | 1 | 2 -> true
  when _ -> false
end

-- With constructors
let getValue x = match x
  when Nothing | Just Nothing -> 0
  when Just (Just n) -> n
end

-- Variables must bind the same names in all alternatives
let extract x = match x
  when Just x | Right x -> x
  when _ -> 0
end
```

### Records

```
-- Record creation and access
let person = { name = "Alice", age = 30 }
let greeting = "Hello, " + person.name

-- Field punning: { x } is shorthand for { x = x }
let x = 10
let y = 20
let point = { x, y }           -- same as { x = x, y = y }

-- Mixed punning and explicit fields
let z = 30
let point3d = { x, y, z = z + 1 }

-- Record pattern matching
let getAge p = match p
  when { age = a } -> a
end

-- Pattern punning: { x } matches field 'x' and binds to variable 'x'
let getCoords p = match p
  when { x, y } -> (x, y)         -- same as { x = x, y = y }
end

-- Row-polymorphic function (inferred: { x: a | r } -> a)
let getX r = r.x
```

### Tuples

```
-- Tuple creation
let point = (10, 20)
let triple = (1, "hello", true)

-- Tuple indexing (0-based)
point.0              -- 10
point.1              -- 20
triple.2             -- true

-- For chained indexing, use parentheses (t.0.1 parses as t.(0.1))
let nested = ((1, 2), 3)
(nested.0).1         -- 2

-- Tuple pattern matching
let fst p = match p
  when (a, _) -> a
end

let snd p = match p
  when (_, b) -> b
end

-- Swap elements (inferred: (a, b) -> (b, a))
let swap p = match p
  when (x, y) -> (y, x)
end
```

### Modules

```
-- Define a module
module List
  type List a = Nil | Cons a (List a)

  let rec map f xs = match xs
    when Nil -> Nil
    when Cons x rest -> Cons (f x) (map f rest)
  end

  let rec filter p xs = match xs
    when Nil -> Nil
    when Cons x rest ->
      if p x then Cons x (filter p rest)
      else filter p rest
  end
end

-- Import everything from a module
use List (..)

-- Import specific items
use List (List, map)

-- Import type with all constructors
use List (List(..))

-- Import type with specific constructors
use List (List(Nil, Cons))

-- Import with alias
use List as L

-- Use qualified access
List.map (x -> x + 1) xs
List.Nil
List.Cons 1 List.Nil

-- Qualified patterns
match xs
  when List.Nil -> 0
  when List.Cons x _ -> x
end
```

### Full Program Example

```
-- Define modules
module Maybe
  type Maybe a = Nothing | Just a

  let map f m = match m
    when Nothing -> Nothing
    when Just x -> Just (f x)
  end
end

module List
  type List a = Nil | Cons a (List a)

  let rec length xs = match xs
    when Nil -> 0
    when Cons _ rest -> 1 + length rest
  end

  let rec sum xs = match xs
    when Nil -> 0
    when Cons x rest -> x + sum rest
  end

  let rec map f xs = match xs
    when Nil -> Nil
    when Cons x rest -> Cons (f x) (map f rest)
  end
end

-- Import what we need
use Maybe (Maybe(..))
use List (..)

-- Main expression using list literal (desugars to Cons chains)
[1, 2, 3, 4, 5]
  |> map (x -> x * 2)
  |> sum
```

---

## Notes

1. **No semicolons required** - Newlines separate top-level declarations
2. **Type inference** - All types are inferred by Algorithm W; annotations are optional
3. **Row polymorphism** - Records support extensible row types (inferred automatically)
4. **Curried functions** - All functions take one argument; multi-arg is sugar for nested lambdas
5. **Lightweight lambdas** - Just `x -> body` or `x y -> body` for curried multi-param
6. **Pipe operator** - `x |> f` desugars to `f x`, enabling left-to-right data flow
7. **Module system** - Organize code into modules with qualified access and selective imports
