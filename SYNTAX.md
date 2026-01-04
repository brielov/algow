# Algow Language Syntax Specification

This document defines the complete syntax for the Algow language.

## Lexical Elements

### Keywords

```
let, rec, in, if, then, else, match, with, end, data, true, false
```

### Operators

```
+  -  *  /           -- arithmetic (binary)
-                    -- negation (unary, desugars to 0 - x)
++                   -- string concatenation
<  <=  >  >=         -- comparison
==  !=               -- equality
&&  ||               -- logical (short-circuit)
|>                   -- pipe (flip application)
=>                   -- lambda arrow
=                    -- binding
|                    -- pattern separator / ADT union
,                    -- tuple/record separator
.                    -- field access
_                    -- wildcard pattern
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
uppercase    = [A-Z][a-zA-Z0-9_]*      -- constructors: Nil, Just, Cons
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
program      = declaration* expression?

declaration  = data_decl
             | let_decl
```

### Data Declarations

```
data_decl    = "data" UPPER type_var* "=" constructor ("|" constructor)*

constructor  = UPPER type_atom*

type_var     = LOWER
```

Examples:

```
data Bool = True | False
data Maybe a = Nothing | Just a
data List a = Nil | Cons a (List a)
data Either a b = Left a | Right b
```

### Let Declarations

```
let_decl     = "let" "rec"? LOWER pattern* "=" expression
             | "let" pattern "=" expression    -- destructuring (non-recursive only)

pattern      = LOWER                           -- variable
             | "_"                             -- wildcard
             | literal                         -- number, string, boolean
             | UPPER pattern*                  -- constructor
             | "(" pattern ("," pattern)+ ")"  -- tuple
             | "{" field_pat ("," field_pat)* "}"  -- record

field_pat    = LOWER "=" pattern
             | LOWER                              -- punning: { x } means { x = x }
```

Examples:

```
let x = 42
let add x y = x + y
let rec fact n = if n == 0 then 1 else n * fact (n - 1)

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

let_expr     = "let" "rec"? LOWER pattern* "=" expression "in" expression
             | "let" pattern "=" expression "in" expression  -- destructuring

if_expr      = "if" expression "then" expression "else" expression

match_expr   = "match" expression "with" match_arm+ "end"

match_arm    = "|" pattern guard? "=>" expression

guard        = "if" expression                    -- pattern guard (must be boolean)

pattern      = pattern_atom "as" LOWER            -- as-pattern
             | pattern_atom

lambda_expr  = LOWER "=>" expression
```

### Binary Expressions (by precedence, low to high)

```
binary_expr  = pipe_expr

pipe_expr        = or_expr ("|>" or_expr)*
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
Note: `a && b` desugars to `if a then b else false` (short-circuit).
Note: `a || b` desugars to `if a then true else b` (short-circuit).
Note: `-x` desugars to `0 - x` (unary negation).

### Primary Expressions

```
primary_expr = LOWER                           -- variable
             | UPPER                           -- constructor
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

### Types (in data declarations only)

```
type         = type_app

type_app     = type_atom+                        -- left-associative

type_atom    = LOWER                             -- type variable: a, b
             | UPPER                             -- type constructor: List, Maybe
             | "(" type ")"                      -- grouping
```

Note: Types only appear in data declarations (e.g., `Cons a (List a)`).
All expression types are inferred by Algorithm W.

---

## Operator Precedence (low to high)

| Precedence | Operators            | Associativity |
| ---------- | -------------------- | ------------- |
| 1          | `\|>` (pipe)         | left          |
| 2          | `\|\|`               | left          |
| 3          | `&&`                 | left          |
| 4          | `==` `!=`            | left          |
| 5          | `<` `<=` `>` `>=`    | left          |
| 6          | `+` `-` `++`         | left          |
| 7          | `*` `/`              | left          |
| 8          | function application | left          |
| 9          | `.` (field access)   | left          |

---

## Complete Examples

### Lambda Expressions

```
-- Single parameter
x => x + 1

-- Multiple parameters (curried)
x => y => x + y

-- Used inline with higher-order functions
map (x => x * 2) list
filter (n => n > 0) numbers
foldr (x => acc => x + acc) 0 list

-- With pipe operator (reads left-to-right)
list |> map (x => x * 2)
numbers |> filter (n => n > 0) |> map (x => x * 2)
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

-- Using pattern guards
let abs n = match n with
  | x if x < 0 => 0 - x
  | x => x
end

let sign n = match n with
  | x if x > 0 => 1
  | x if x < 0 => 0 - 1
  | _ => 0
end

-- Using as-patterns to bind whole and parts
data List a = Nil | Cons a (List a)

let rec duplicate xs = match xs with
  | Nil => Nil
  | Cons x rest as whole => Cons whole (duplicate rest)
end
```

### Algebraic Data Types

```
data Maybe a = Nothing | Just a

data List a = Nil | Cons a (List a)

-- Safe head (inferred: List a -> Maybe a)
let head xs = match xs with
  | Nil => Nothing
  | Cons x _ => Just x
end

-- Map over list (inferred: (a -> b) -> List a -> List b)
let rec map f xs = match xs with
  | Nil => Nil
  | Cons x rest => Cons (f x) (map f rest)
end

-- Filter list (inferred: (a -> boolean) -> List a -> List a)
let rec filter p xs = match xs with
  | Nil => Nil
  | Cons x rest =>
      if p x then Cons x (filter p rest)
      else filter p rest
end

-- Fold right (inferred: (a -> b -> b) -> b -> List a -> b)
let rec foldr f z xs = match xs with
  | Nil => z
  | Cons x rest => f x (foldr f z rest)
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
let getAge p = match p with
  | { age = a } => a
end

-- Pattern punning: { x } matches field 'x' and binds to variable 'x'
let getCoords p = match p with
  | { x, y } => (x, y)         -- same as { x = x, y = y }
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
let fst p = match p with
  | (a, _) => a
end

let snd p = match p with
  | (_, b) => b
end

-- Swap elements (inferred: (a, b) -> (b, a))
let swap p = match p with
  | (x, y) => (y, x)
end
```

### Full Program Example

```
-- Define data types
data List a = Nil | Cons a (List a)
data Maybe a = Nothing | Just a

-- List utilities (types are fully inferred)
let rec length xs = match xs with
  | Nil => 0
  | Cons _ rest => 1 + length rest
end

let rec sum xs = match xs with
  | Nil => 0
  | Cons x rest => x + sum rest
end

let rec map f xs = match xs with
  | Nil => Nil
  | Cons x rest => Cons (f x) (map f rest)
end

-- Main expression using list literal (desugars to Cons chains)
[1, 2, 3, 4, 5]
  |> map (x => x * 2)
  |> sum
```

---

## Notes

1. **No semicolons required** - Newlines separate top-level declarations
2. **No type annotations** - All types are inferred by Algorithm W
3. **Row polymorphism** - Records support extensible row types (inferred automatically)
4. **Curried functions** - All functions take one argument; multi-arg is sugar for nested lambdas
5. **Lightweight lambdas** - No keyword needed, just `x => body` or `x => y => body` for curried
6. **Pipe operator** - `x |> f` desugars to `f x`, enabling left-to-right data flow
