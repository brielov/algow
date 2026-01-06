# Appendix A: Language Reference

This appendix provides a complete reference for the Algow language syntax.

---

## Lexical Structure

### Comments

```
-- Line comment (to end of line)

{- Block comment
   can span multiple lines -}
```

### Identifiers

**Lowercase identifiers** (variables, function names):

```
x, foo, my_var, x1, camelCase
```

Must start with lowercase letter or underscore.

**Uppercase identifiers** (constructors, type names):

```
Nothing, Just, Cons, MyType
```

Must start with uppercase letter.

### Literals

**Numbers**:

```
42          -- integer
3.14        -- floating point
0.5         -- decimal starting with 0
```

**Strings**:

```
"hello"
"hello\nworld"    -- escape sequences
"tab:\there"
```

Escape sequences: `\n`, `\t`, `\r`, `\\`, `\"`

**Booleans**:

```
true
false
```

---

## Keywords

| Keyword | Usage                  |
| ------- | ---------------------- |
| `let`   | Variable binding       |
| `rec`   | Recursive binding      |
| `and`   | Mutual recursion       |
| `in`    | Body of let expression |
| `if`    | Conditional            |
| `then`  | Then branch            |
| `else`  | Else branch            |
| `match` | Pattern matching       |
| `with`  | Pattern clauses        |
| `end`   | End of match           |
| `data`  | Data type declaration  |
| `as`    | As-pattern             |
| `true`  | Boolean literal        |
| `false` | Boolean literal        |
| `fn`    | Lambda expression      |

---

## Operators

### Arithmetic

| Operator | Meaning        | Type                         |
| -------- | -------------- | ---------------------------- |
| `+`      | Addition       | `number -> number -> number` |
| `-`      | Subtraction    | `number -> number -> number` |
| `*`      | Multiplication | `number -> number -> number` |
| `/`      | Division       | `number -> number -> number` |

Note: `+` is also overloaded for string concatenation.

### Comparison

| Operator | Meaning          | Type                         |
| -------- | ---------------- | ---------------------------- |
| `<`      | Less than        | `Ord a => a -> a -> boolean` |
| `<=`     | Less or equal    | `Ord a => a -> a -> boolean` |
| `>`      | Greater than     | `Ord a => a -> a -> boolean` |
| `>=`     | Greater or equal | `Ord a => a -> a -> boolean` |

Works on numbers and strings.

### Equality

| Operator | Meaning   | Type                        |
| -------- | --------- | --------------------------- |
| `==`     | Equal     | `Eq a => a -> a -> boolean` |
| `!=`     | Not equal | `Eq a => a -> a -> boolean` |

Works on all types except functions.

### Boolean

| Operator | Meaning     | Type                            |
| -------- | ----------- | ------------------------------- |
| `&&`     | Logical AND | `boolean -> boolean -> boolean` |
| `\|\|`   | Logical OR  | `boolean -> boolean -> boolean` |

Short-circuit evaluation.

### Special

| Operator | Meaning | Desugars To             |
| -------- | ------- | ----------------------- |
| `\|>`    | Pipe    | `x \|> f` → `f x`       |
| `::`     | Cons    | `x :: xs` → `Cons x xs` |

---

## Precedence and Associativity

From lowest to highest precedence:

| Level | Operators            | Associativity |
| ----- | -------------------- | ------------- |
| 1     | `\|\|`               | Left          |
| 2     | `&&`                 | Left          |
| 3     | `==`, `!=`           | Left          |
| 4     | `<`, `<=`, `>`, `>=` | Left          |
| 5     | `::`                 | Right         |
| 6     | `+`, `-`             | Left          |
| 7     | `*`, `/`             | Left          |
| 8     | `\|>`                | Left          |
| 9     | Function application | Left          |
| 10    | `.` (field access)   | Left          |

---

## Expressions

### Literals

```
42              -- number
"hello"         -- string
true            -- boolean
false           -- boolean
```

### Variables

```
x
myFunction
```

### Lambda (Anonymous Function)

```
fn x => x + 1
fn x => fn y => x + y
fn (x : number) => x + 1    -- with type annotation
fn (x : number) : number => x + 1  -- return type too
```

### Function Application

```
f x             -- apply f to x
f x y           -- curried: (f x) y
add 1 2         -- multiple arguments
```

### Let Binding

```
let x = 1 in x + 1

let add x y = x + y in add 1 2

let point = { x = 1, y = 2 } in point.x
```

### Recursive Binding

```
let rec fact n =
  if n == 0 then 1
  else n * fact (n - 1)
in fact 5
```

### Mutual Recursion

```
let rec isEven n = if n == 0 then true else isOdd (n - 1)
and isOdd n = if n == 0 then false else isEven (n - 1)
in isEven 10
```

### Conditional

```
if condition then expr1 else expr2
```

Both branches must have the same type. The `else` branch is required.

### Pattern Matching

```
match expr with
| pattern1 => body1
| pattern2 => body2
| pattern3 if guard => body3
end
```

### Tuples

```
(1, 2)              -- pair
(1, "hello", true)  -- triple
pair.0              -- first element
triple.2            -- third element (0-indexed)
```

### Records

```
{ x = 1, y = 2 }        -- record literal
{ x = 1, y = 2 }.x      -- field access
point.x                 -- field access
```

---

## Patterns

### Variable Pattern

```
x       -- binds value to x
```

### Wildcard Pattern

```
_       -- matches anything, doesn't bind
```

### Literal Pattern

```
0       -- matches 0
"hello" -- matches "hello"
true    -- matches true
```

### Constructor Pattern

```
Nothing             -- nullary constructor
Just x              -- constructor with argument
Cons head tail      -- constructor with multiple args
```

### Tuple Pattern

```
(x, y)              -- destructure pair
(a, b, c)           -- destructure triple
```

### Record Pattern

```
{ x = a, y = b }    -- destructure record fields
{ x = a }           -- partial match (ignores y)
```

### As-Pattern

```
Cons x rest as whole    -- binds whole list AND destructures
```

### Or-Pattern

```
Nothing | Just Nothing  -- match either alternative
```

All alternatives must bind the same variables with the same types.

### Guards

```
| x if x > 0 => "positive"
| x if x < 0 => "negative"
| _ => "zero"
```

---

## Data Types

### Declaration Syntax

```
data TypeName params = Constructor1 args | Constructor2 args | ...
```

### Examples

```
-- No parameters
data Bool = True | False

-- One parameter
data Maybe a = Nothing | Just a

-- Two parameters
data Either a b = Left a | Right b

-- Recursive type
data List a = Nil | Cons a (List a)

-- Multiple constructors
data Color = Red | Green | Blue

-- Type in constructor
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

### Using Constructors

Constructors are functions:

```
Just        -- a -> Maybe a
Cons        -- a -> List a -> List a
Left        -- a -> Either a b
```

Apply them like functions:

```
Just 42                     -- Maybe number
Cons 1 (Cons 2 Nil)        -- List number
Left "error"               -- Either string b
```

---

## Type Annotations

### In Let Bindings

```
let (x : number) = 42 in x

let (add : number -> number -> number) = fn x => fn y => x + y in add 1 2
```

### In Lambdas

```
fn (x : number) => x + 1

fn (x : number) : number => x + 1   -- parameter and return type

fn (x : number) => fn (y : number) : number => x + y
```

### Type Syntax

| Syntax        | Meaning                   |
| ------------- | ------------------------- |
| `number`      | Number type               |
| `string`      | String type               |
| `boolean`     | Boolean type              |
| `a`           | Type variable             |
| `a -> b`      | Function type             |
| `(a, b)`      | Tuple type                |
| `Maybe a`     | Type application          |
| `List number` | Concrete type application |

---

## Prelude

The prelude is automatically available in every program.

### Data Types

```
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
data List a = Nil | Cons a (List a)
```

### List Functions

| Function  | Type                                 | Description            |
| --------- | ------------------------------------ | ---------------------- |
| `map`     | `(a -> b) -> List a -> List b`       | Transform each element |
| `filter`  | `(a -> boolean) -> List a -> List a` | Keep matching elements |
| `foldr`   | `(a -> b -> b) -> b -> List a -> b`  | Right fold             |
| `foldl`   | `(b -> a -> b) -> b -> List a -> b`  | Left fold              |
| `head`    | `List a -> Maybe a`                  | First element          |
| `tail`    | `List a -> Maybe (List a)`           | All but first          |
| `length`  | `List a -> number`                   | Count elements         |
| `isEmpty` | `List a -> boolean`                  | Check if empty         |
| `reverse` | `List a -> List a`                   | Reverse list           |
| `concat`  | `List a -> List a -> List a`         | Concatenate lists      |

### Utility Functions

| Function  | Type                             | Description          |
| --------- | -------------------------------- | -------------------- |
| `id`      | `a -> a`                         | Identity function    |
| `const`   | `a -> b -> a`                    | Constant function    |
| `compose` | `(b -> c) -> (a -> b) -> a -> c` | Function composition |
| `flip`    | `(a -> b -> c) -> b -> a -> c`   | Swap arguments       |

---

## Complete Grammar

```
program     ::= declaration* expression?

declaration ::= 'data' UPPER typevar* '=' constructor ('|' constructor)*

constructor ::= UPPER type*

expression  ::= letExpr
              | ifExpr
              | matchExpr
              | lambdaExpr
              | binaryExpr
              | application
              | atom

letExpr     ::= 'let' binding 'in' expression
              | 'let' 'rec' binding ('and' binding)* 'in' expression

binding     ::= LOWER+ '=' expression

ifExpr      ::= 'if' expression 'then' expression 'else' expression

matchExpr   ::= 'match' expression 'with' case+ 'end'

case        ::= '|' pattern guard? '=>' expression

guard       ::= 'if' expression

lambdaExpr  ::= 'fn' parameter+ '=>' expression

parameter   ::= LOWER
              | '(' LOWER ':' type ')'

application ::= atom+

atom        ::= literal
              | LOWER
              | UPPER
              | '(' expression ')'
              | '(' expression (',' expression)+ ')'
              | '{' field (',' field)* '}'
              | atom '.' (LOWER | NUMBER)

field       ::= LOWER '=' expression

pattern     ::= patternOr

patternOr   ::= patternAs ('|' patternAs)*

patternAs   ::= patternAtom ('as' LOWER)?

patternAtom ::= '_'
              | LOWER
              | literal
              | UPPER patternAtom*
              | '(' pattern (',' pattern)* ')'
              | '{' patternField (',' patternField)* '}'

patternField ::= LOWER '=' pattern

type        ::= typeAtom ('->' type)?

typeAtom    ::= 'number'
              | 'string'
              | 'boolean'
              | LOWER
              | UPPER typeAtom*
              | '(' type (',' type)* ')'
              | '{' typeField (',' typeField)* '}'

typeField   ::= LOWER ':' type
```
