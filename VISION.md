# The Algow Language Specification

**Version 1.0**

---

## Table of Contents

1. [Overview](#1-overview)
2. [Design Principles](#2-design-principles)
3. [Lexical Structure](#3-lexical-structure)
4. [Surface Syntax](#4-surface-syntax)
5. [Core Calculus](#5-core-calculus)
6. [Type System](#6-type-system)
7. [Elaboration](#7-elaboration)
8. [Pattern Matching](#8-pattern-matching)
9. [Modules](#9-modules)
10. [Foreign Interface](#10-foreign-interface)
11. [Standard Library](#11-standard-library)
12. [Compiler Pipeline](#12-compiler-pipeline)
13. [References](#13-references)

---

## 1. Overview

Algow is a statically-typed, strict, pure functional programming language in the ML tradition. It features:

- Hindley-Milner type inference with principal types
- Row-polymorphic records
- Algebraic data types with exhaustive pattern matching
- First-class functions with automatic currying
- A simple module system
- Platform-injectable foreign function interface

The language is designed to be implementable from textbook algorithms without hacks or ad-hoc extensions.

---

## 2. Design Principles

### 2.1 Evaluation Strategy

Algow is **strict** (call-by-value). Function arguments are evaluated before the function body.

```
let x = diverge () in 42    -- diverges, does not return 42
```

### 2.2 Purity

The core language is pure. Side effects are introduced only through foreign functions provided by the platform.

### 2.3 Orthogonality

Language features compose without special cases:

- Modules are namespaces (foreign or not)
- Foreign declarations are bindings without implementations
- Operators elaborate to function calls
- All sugar desugars to a small core

### 2.4 Phase Separation

Each compiler phase has strict boundaries:

- Parsing produces surface AST (no types)
- Binding resolves names (no types)
- Exhaustiveness checks patterns (no elaboration)
- Elaboration and typing are interleaved
- Core IR is minimal and explicit

---

## 3. Lexical Structure

### 3.1 Character Set

Source files are UTF-8 encoded.

### 3.2 Whitespace and Comments

```
whitespace   = ' ' | '\t' | '\n' | '\r'
line_comment = '--' [^\n]* '\n'
block_comment = '{-' (block_comment | .)* '-}'   -- nestable
```

Whitespace separates tokens. Newlines are significant only as declaration separators at the top level.

### 3.3 Identifiers

```
lower_ident  = [a-z_][a-zA-Z0-9_]*
upper_ident  = [A-Z][a-zA-Z0-9_]*
```

- `lower_ident`: variables, type variables, field names, function names
- `upper_ident`: constructors, type constructors, module names

### 3.4 Keywords

```
let    rec    in     if     then   else   match  when   end
type   module use    as     and    foreign
true   false
```

Keywords cannot be used as identifiers.

### 3.5 Literals

```
int_lit      = '-'? [0-9]+
float_lit    = '-'? [0-9]+ '.' [0-9]+
string_lit   = '"' ([^"\\] | escape)* '"'
char_lit     = '\'' ([^'\\] | escape) '\''

escape       = '\\' ('n' | 'r' | 't' | '\\' | '"' | '\'' | 'u{' hex+ '}')
hex          = [0-9a-fA-F]
```

### 3.6 Operators and Punctuation

```
-- Arithmetic
+  -  *  /  %

-- Comparison
<  <=  >  >=  ==  !=

-- Logical
&&  ||

-- Other operators
|>  ::  ++

-- Punctuation
(  )  {  }  [  ]
,  .  :  =  |  _  ->
```

### 3.7 Token Types

```
token ::=
  | LOWER_IDENT string
  | UPPER_IDENT string
  | INT_LIT int
  | FLOAT_LIT float
  | STRING_LIT string
  | CHAR_LIT char
  | KEYWORD keyword
  | OPERATOR operator
  | PUNCT punctuation
  | EOF
```

Each token carries source location `(line, column, offset)` for error reporting.

---

## 4. Surface Syntax

### 4.1 Grammar Notation

```
x*       zero or more x
x+       one or more x
x?       zero or one x
x | y    x or y
'...'    literal token
```

### 4.2 Programs

```
program      ::= module_decl* use_decl* decl* expr?
```

### 4.3 Module Declarations

```
module_decl  ::= 'module' UPPER_IDENT decl* 'end'

use_decl     ::= 'use' module_path import_spec? ('as' UPPER_IDENT)?

module_path  ::= UPPER_IDENT ('.' UPPER_IDENT)*

import_spec  ::= '(' '..' ')'
               | '(' import_item (',' import_item)* ')'

import_item  ::= LOWER_IDENT
               | UPPER_IDENT
               | UPPER_IDENT '(' '..' ')'
               | UPPER_IDENT '(' UPPER_IDENT (',' UPPER_IDENT)* ')'
```

### 4.4 Declarations

```
decl         ::= type_decl
               | let_decl
               | foreign_decl

type_decl    ::= 'type' UPPER_IDENT LOWER_IDENT* '=' ctor_def ('|' ctor_def)*

ctor_def     ::= UPPER_IDENT type_atom*

let_decl     ::= 'let' 'rec'? binding ('and' binding)*

binding      ::= LOWER_IDENT param* (':' type)? '=' expr
               | pattern '=' expr

param        ::= LOWER_IDENT
               | '(' LOWER_IDENT ':' type ')'

foreign_decl ::= 'foreign' LOWER_IDENT ':' type
```

### 4.5 Expressions

```
expr         ::= let_expr
               | if_expr
               | match_expr
               | lambda_expr
               | binary_expr

let_expr     ::= 'let' 'rec'? binding ('and' binding)* 'in' expr

if_expr      ::= 'if' expr 'then' expr 'else' expr

match_expr   ::= 'match' expr match_arm+ 'end'

match_arm    ::= 'when' pattern guard? '->' expr

guard        ::= 'if' expr

lambda_expr  ::= param+ '->' expr

binary_expr  ::= unary_expr (binary_op unary_expr)*

unary_expr   ::= '-' unary_expr
               | app_expr

app_expr     ::= primary_expr+

primary_expr ::= LOWER_IDENT
               | UPPER_IDENT
               | qualified_name
               | literal
               | '(' expr ')'
               | '(' expr ',' expr (',' expr)* ')'
               | '{' field_def (',' field_def)* '}'
               | '[' (expr (',' expr)*)? ']'
               | primary_expr '.' LOWER_IDENT
               | primary_expr '.' INT_LIT

qualified_name ::= UPPER_IDENT '.' (LOWER_IDENT | UPPER_IDENT)

field_def    ::= LOWER_IDENT '=' expr
               | LOWER_IDENT

literal      ::= INT_LIT | FLOAT_LIT | STRING_LIT | CHAR_LIT
               | 'true' | 'false' | '(' ')'
```

### 4.6 Patterns

```
pattern      ::= pattern_term ('|' pattern_term)*

pattern_term ::= pattern_atom ('as' LOWER_IDENT)?

pattern_atom ::= '_'
               | LOWER_IDENT
               | literal
               | UPPER_IDENT pattern_atom*
               | qualified_ctor pattern_atom*
               | '(' pattern (',' pattern)+ ')'
               | '{' field_pat (',' field_pat)* '}'
               | '(' pattern ')'

qualified_ctor ::= UPPER_IDENT '.' UPPER_IDENT

field_pat    ::= LOWER_IDENT '=' pattern
               | LOWER_IDENT
```

### 4.7 Types

```
type         ::= type_app ('->' type)?

type_app     ::= type_atom+

type_atom    ::= LOWER_IDENT
               | UPPER_IDENT
               | '(' type ')'
               | '(' type (',' type)+ ')'
               | '{' row '}'

row          ::= field_type (',' field_type)* ('|' LOWER_IDENT)?
               | LOWER_IDENT

field_type   ::= LOWER_IDENT ':' type
```

### 4.8 Operator Precedence

| Level | Operators         | Associativity |
| ----- | ----------------- | ------------- |
| 1     | `\|>`             | left          |
| 2     | `::`              | right         |
| 3     | `\|\|`            | left          |
| 4     | `&&`              | left          |
| 5     | `==` `!=`         | none          |
| 6     | `<` `<=` `>` `>=` | none          |
| 7     | `++`              | right         |
| 8     | `+` `-`           | left          |
| 9     | `*` `/` `%`       | left          |
| 10    | application       | left          |
| 11    | `.`               | left          |

---

## 5. Core Calculus

The core calculus is the target of elaboration. It is small, explicit, and directly typeable.

### 5.1 Syntax

```
Variables       x, y, z
Constructors    C, D
Labels          l, m
Type variables  α, β, ρ
Type ctors      T

Expressions:
e ::= x                              -- variable
    | λx. e                          -- abstraction
    | e₁ e₂                          -- application
    | let x = e₁ in e₂               -- let binding
    | let rec x₁ = e₁ and ... in e   -- recursive let
    | C                              -- constructor
    | case e of { Cᵢ x̄ᵢ → eᵢ }       -- case analysis
    | { l₁ = e₁, ..., lₙ = eₙ }      -- record construction
    | e.l                            -- field projection
    | lit                            -- literal

Literals:
lit ::= n                            -- integer
      | f                            -- float
      | "s"                          -- string
      | 'c'                          -- char
      | true | false                 -- boolean

Core Patterns (in case):
p ::= C x₁ ... xₙ                    -- constructor with n variable bindings
```

### 5.2 Design Constraints

1. **No nested patterns.** All pattern matching is single-level constructor matching.
2. **No guards.** Guards are compiled away.
3. **No or-patterns.** Expanded during elaboration.
4. **No wildcards.** Replaced with fresh variables.
5. **No syntactic sugar.** Everything is explicit.

---

## 6. Type System

### 6.1 Types

```
Types:
τ ::= α                              -- type variable
    | τ₁ → τ₂                        -- function type
    | T τ₁ ... τₙ                    -- type constructor application
    | { ρ }                          -- record type

Rows:
ρ ::= ∅                              -- empty row
    | l : τ, ρ                       -- row extension
    | α                              -- row variable

Type Schemes:
σ ::= τ                              -- monotype
    | ∀ᾱ. τ                          -- polytype (ᾱ is a vector of type variables)
```

### 6.2 Primitive Types

```
Int     : *
Float   : *
String  : *
Char    : *
Bool    : *       -- equivalent to ADT: type Bool = True | False
Unit    : *       -- equivalent to ADT: type Unit = Unit
```

### 6.3 Kinds

```
κ ::= *                              -- type of values
    | κ → κ                          -- type constructor

Primitive kinds:
Int    : *
Float  : *
String : *
Char   : *
Bool   : *
Unit   : *
List   : * → *
Maybe  : * → *
Either : * → * → *
```

For Algow (no higher-kinded types), kind checking reduces to arity checking.

### 6.4 Type Environments

```
Γ ::= ∅                              -- empty environment
    | Γ, x : σ                       -- term binding
```

### 6.5 Typing Judgments

The main judgment is `Γ ⊢ e : τ` — "under environment Γ, expression e has type τ."

### 6.6 Typing Rules

#### Variables

```
    x : σ ∈ Γ    τ = inst(σ)
    ─────────────────────────  [Var]
           Γ ⊢ x : τ
```

#### Abstraction

```
       Γ, x : τ₁ ⊢ e : τ₂
    ─────────────────────────  [Abs]
      Γ ⊢ λx. e : τ₁ → τ₂
```

#### Application

```
    Γ ⊢ e₁ : τ₁ → τ₂    Γ ⊢ e₂ : τ₁
    ────────────────────────────────  [App]
            Γ ⊢ e₁ e₂ : τ₂
```

#### Let (non-recursive)

```
    Γ ⊢ e₁ : τ₁    σ = gen(Γ, τ₁)    Γ, x : σ ⊢ e₂ : τ₂
    ─────────────────────────────────────────────────────  [Let]
                  Γ ⊢ let x = e₁ in e₂ : τ₂
```

#### Let (recursive)

```
    Γ, x : τ₁ ⊢ e₁ : τ₁    σ = gen(Γ, τ₁)    Γ, x : σ ⊢ e₂ : τ₂
    ─────────────────────────────────────────────────────────────  [LetRec]
                    Γ ⊢ let rec x = e₁ in e₂ : τ₂
```

#### Constructor

```
    C : σ ∈ Γ    τ = inst(σ)
    ────────────────────────  [Ctor]
          Γ ⊢ C : τ
```

For an ADT `type T α₁...αₙ = ... | C τ₁...τₘ | ...`:

```
C : ∀α₁...αₙ. τ₁ → ... → τₘ → T α₁...αₙ
```

#### Case

```
    Γ ⊢ e : T τ̄
    ∀i: Γ, x̄ᵢ : τ̄ᵢ ⊢ eᵢ : τ    (where Cᵢ : τ̄ᵢ → T τ̄)
    ────────────────────────────────────────────────────  [Case]
           Γ ⊢ case e of { Cᵢ x̄ᵢ → eᵢ } : τ
```

#### Record Construction

```
    Γ ⊢ e₁ : τ₁    ...    Γ ⊢ eₙ : τₙ
    ─────────────────────────────────────────────────────  [Record]
    Γ ⊢ { l₁ = e₁, ..., lₙ = eₙ } : { l₁ : τ₁, ..., lₙ : τₙ }
```

#### Field Projection

```
    Γ ⊢ e : { l : τ, ρ }
    ─────────────────────  [Proj]
        Γ ⊢ e.l : τ
```

#### Literals

```
    ────────────────  [Int]
    Γ ⊢ n : Int

    ────────────────  [Float]
    Γ ⊢ f : Float

    ────────────────  [String]
    Γ ⊢ "s" : String

    ────────────────  [Char]
    Γ ⊢ 'c' : Char

    ────────────────  [True]
    Γ ⊢ true : Bool

    ────────────────  [False]
    Γ ⊢ false : Bool
```

### 6.7 Generalization and Instantiation

**Free type variables:**

```
ftv(α)           = {α}
ftv(τ₁ → τ₂)     = ftv(τ₁) ∪ ftv(τ₂)
ftv(T τ̄)         = ⋃ ftv(τᵢ)
ftv({ ρ })       = ftv(ρ)
ftv(∅)           = ∅
ftv(l : τ, ρ)    = ftv(τ) ∪ ftv(ρ)
ftv(Γ)           = ⋃ { ftv(σ) | x : σ ∈ Γ }
ftv(∀ᾱ. τ)       = ftv(τ) \ {ᾱ}
```

**Generalization:**

```
gen(Γ, τ) = ∀ᾱ. τ    where ᾱ = ftv(τ) \ ftv(Γ)
```

**Instantiation:**

```
inst(∀α₁...αₙ. τ) = τ[α₁ := β₁, ..., αₙ := βₙ]    where β₁...βₙ are fresh
```

### 6.8 Constraint-Based Inference

Rather than implementing Algorithm W directly, we use constraint-based inference.

#### Constraints

```
C ::= τ₁ ≡ τ₂                        -- type equality
    | C₁ ∧ C₂                        -- conjunction
    | ∃α. C                          -- existential
    | ε                              -- empty constraint
```

#### Constraint Generation

`Γ ⊢ e : τ ⇒ C` — "under Γ, e has type τ generating constraints C"

```
    x : σ ∈ Γ    τ = inst(σ)
    ─────────────────────────  [CG-Var]
       Γ ⊢ x : τ ⇒ ε


    Γ, x : α ⊢ e : τ ⇒ C    (α fresh)
    ──────────────────────────────────  [CG-Abs]
       Γ ⊢ λx. e : α → τ ⇒ C


    Γ ⊢ e₁ : τ₁ ⇒ C₁    Γ ⊢ e₂ : τ₂ ⇒ C₂    (α fresh)
    ───────────────────────────────────────────────────  [CG-App]
        Γ ⊢ e₁ e₂ : α ⇒ C₁ ∧ C₂ ∧ (τ₁ ≡ τ₂ → α)


    Γ ⊢ e₁ : τ₁ ⇒ C₁
    S = solve(C₁)
    σ = gen(SΓ, Sτ₁)
    Γ, x : σ ⊢ e₂ : τ₂ ⇒ C₂
    ────────────────────────────────────────  [CG-Let]
        Γ ⊢ let x = e₁ in e₂ : τ₂ ⇒ C₂


    Γ ⊢ e : τ ⇒ C    (α, ρ fresh)
    ─────────────────────────────────────────  [CG-Proj]
    Γ ⊢ e.l : α ⇒ C ∧ (τ ≡ { l : α, ρ })
```

### 6.9 Unification

Unification solves constraints by computing a most general substitution.

**Substitutions:**

```
S ::= ∅                              -- identity
    | S, α ↦ τ                       -- type substitution
    | S, α ↦ ρ                       -- row substitution
```

**Unification algorithm:**

```
unify(α, τ)           = { α ↦ τ }           if α ∉ ftv(τ)
unify(τ, α)           = { α ↦ τ }           if α ∉ ftv(τ)
unify(τ₁ → τ₂, τ₃ → τ₄) = S₂ ∘ S₁
                          where S₁ = unify(τ₁, τ₃)
                                S₂ = unify(S₁τ₂, S₁τ₄)
unify(T τ̄, T τ̄')      = unify*(τ̄, τ̄')
unify({ ρ₁ }, { ρ₂ }) = unifyRow(ρ₁, ρ₂)
unify(τ, τ)           = ∅
unify(_, _)           = error
```

### 6.10 Row Unification (Rémy)

```
unifyRow(α, ρ)              = { α ↦ ρ }     if α ∉ ftv(ρ)
unifyRow(ρ, α)              = { α ↦ ρ }     if α ∉ ftv(ρ)
unifyRow(∅, ∅)              = ∅
unifyRow(l : τ₁, ρ₁,  l : τ₂, ρ₂) = S₂ ∘ S₁
                              where S₁ = unify(τ₁, τ₂)
                                    S₂ = unifyRow(S₁ρ₁, S₁ρ₂)
unifyRow(l : τ₁, ρ₁,  ρ₂)   = S₂ ∘ S₁
                              where (τ₂, ρ₂') = extract(l, ρ₂)
                                    S₁ = unify(τ₁, τ₂)
                                    S₂ = unifyRow(S₁ρ₁, S₁ρ₂')
unifyRow(_, _)              = error
```

**Row extraction:**

```
extract(l, l : τ, ρ)     = (τ, ρ)
extract(l, l' : τ', ρ)   = (τ, l' : τ', ρ')    where (τ, ρ') = extract(l, ρ)
extract(l, α)            = (β, l : β, α')       where β, α' fresh
extract(l, ∅)            = error                -- field not found
```

---

## 7. Elaboration

Elaboration transforms Surface AST to Typed Core. It interleaves desugaring with type inference.

### 7.1 Syntax-Directed Desugaring

These transformations require no type information:

```
-- Multi-parameter lambda
⟦ x y z -> e ⟧ = λx. λy. λz. ⟦e⟧

-- Multi-parameter let
⟦ let f x y = e₁ in e₂ ⟧ = let f = λx. λy. ⟦e₁⟧ in ⟦e₂⟧

-- If expression
⟦ if e₁ then e₂ else e₃ ⟧ = case ⟦e₁⟧ of { True → ⟦e₂⟧; False → ⟦e₃⟧ }

-- Logical and (short-circuit)
⟦ e₁ && e₂ ⟧ = case ⟦e₁⟧ of { True → ⟦e₂⟧; False → false }

-- Logical or (short-circuit)
⟦ e₁ || e₂ ⟧ = case ⟦e₁⟧ of { True → true; False → ⟦e₂⟧ }

-- Pipe operator
⟦ e₁ |> e₂ ⟧ = ⟦e₂⟧ ⟦e₁⟧

-- Cons operator
⟦ e₁ :: e₂ ⟧ = Cons ⟦e₁⟧ ⟦e₂⟧

-- List literal
⟦ [] ⟧ = Nil
⟦ [e₁, ..., eₙ] ⟧ = Cons ⟦e₁⟧ (... (Cons ⟦eₙ⟧ Nil))

-- Tuple literal
⟦ (e₁, e₂, ..., eₙ) ⟧ = { _0 = ⟦e₁⟧, _1 = ⟦e₂⟧, ..., _(n-1) = ⟦eₙ⟧ }

-- Tuple indexing
⟦ e.0 ⟧ = ⟦e⟧._0
⟦ e.n ⟧ = ⟦e⟧._n

-- Unit
⟦ () ⟧ = {}

-- Unary negation
⟦ -e ⟧ = case infer(e) of
           Int   → Int.neg ⟦e⟧
           Float → Float.neg ⟦e⟧
```

### 7.2 Type-Directed Desugaring

These require type information:

#### Arithmetic Operators

```
⟦ e₁ + e₂ ⟧ =
  let τ₁ = infer(e₁), τ₂ = infer(e₂) in
  case (τ₁, τ₂) of
    (Int, Int)     → Int.add ⟦e₁⟧ ⟦e₂⟧
    (Float, Float) → Float.add ⟦e₁⟧ ⟦e₂⟧
    (Int, Float)   → Float.add (Int.toFloat ⟦e₁⟧) ⟦e₂⟧
    (Float, Int)   → Float.add ⟦e₁⟧ (Int.toFloat ⟦e₂⟧)
    _              → error "numeric types required"
```

Similarly for `-`, `*`, `/`.

#### Modulo

```
⟦ e₁ % e₂ ⟧ =
  require infer(e₁) = Int, infer(e₂) = Int
  Int.mod ⟦e₁⟧ ⟦e₂⟧
```

#### String Concatenation

```
⟦ e₁ ++ e₂ ⟧ =
  let τ₁ = infer(e₁), τ₂ = infer(e₂) in
  case (τ₁, τ₂) of
    (String, String) → String.concat ⟦e₁⟧ ⟦e₂⟧
    (List α, List β) → require α = β; List.append ⟦e₁⟧ ⟦e₂⟧
    _                → error
```

#### Comparison Operators

```
⟦ e₁ == e₂ ⟧ =
  let τ = infer(e₁) in
  require ¬isFunction(τ)
  eqAt(τ, ⟦e₁⟧, ⟦e₂⟧)

eqAt(Int, e₁, e₂)      = Int.eq e₁ e₂
eqAt(Float, e₁, e₂)    = Float.eq e₁ e₂
eqAt(String, e₁, e₂)   = String.eq e₁ e₂
eqAt(Char, e₁, e₂)     = Char.eq e₁ e₂
eqAt(Bool, e₁, e₂)     = Bool.eq e₁ e₂
eqAt({ ρ }, e₁, e₂)    = record equality (field-wise)
eqAt(T τ̄, e₁, e₂)      = ADT equality (tag + field-wise)
```

#### Ordering Operators

```
⟦ e₁ < e₂ ⟧ =
  let τ = infer(e₁) in
  require τ ∈ {Int, Float, Char, String}
  ltAt(τ, ⟦e₁⟧, ⟦e₂⟧)
```

### 7.3 Record Punning

```
⟦ { x, y, z = e } ⟧ = { x = x, y = y, z = ⟦e⟧ }
```

---

## 8. Pattern Matching

### 8.1 Exhaustiveness Checking (Maranget)

Pattern matching is validated before elaboration using the pattern matrix algorithm.

#### Pattern Matrix

A pattern matrix P is a list of rows, where each row is a list of patterns:

```
P = [ [p₁₁, p₁₂, ..., p₁ₙ],
      [p₂₁, p₂₂, ..., p₂ₙ],
      ... ]
```

#### Usefulness

A pattern vector q is _useful_ with respect to P if there exists a value matched by q but not by any row in P.

```
useful(P, q):
  if P is empty:
    return True                          -- q matches, nothing else does

  if q = (_, _, ..., _):                 -- all wildcards
    return P is empty

  let i = first non-wildcard column in q
  let c = head constructor/literal at q[i]

  if c is a constructor C with arity k:
    let P' = specialize(P, i, C)
    let q' = specialize_vec(q, i, C)
    return useful(P', q')

  else:  -- q[i] is wildcard
    let Σ = constructors of type at column i
    if all C ∈ Σ appear in column i of P:
      return any(useful(specialize(P, i, C), specialize_vec(q, i, C)) for C in Σ)
    else:
      return useful(default(P, i), default_vec(q, i))
```

#### Specialization

```
specialize(P, i, C):
  for each row r in P:
    if r[i] matches C x₁...xₖ:
      yield r[0..i-1] ++ [x₁, ..., xₖ] ++ r[i+1..]
    elif r[i] is wildcard:
      yield r[0..i-1] ++ [_, ..., _] (k times) ++ r[i+1..]
    else:
      skip row
```

#### Default Matrix

```
default(P, i):
  for each row r in P:
    if r[i] is wildcard:
      yield r[0..i-1] ++ r[i+1..]
```

#### Exhaustiveness

A match is exhaustive iff `useful(P, [_, _, ..., _])` is False.

#### Redundancy

Row i is redundant iff `useful(P[0..i-1], P[i])` is False.

### 8.2 Pattern Compilation

After validation, patterns are compiled to decision trees.

#### Decision Trees

```
DTree ::= Leaf e                         -- matched, evaluate e
        | Fail                           -- no match (unreachable if exhaustive)
        | Switch x [(C, vars, DTree)]    -- inspect x, branch by constructor
```

#### Compilation

```
compile([], action):
  return Leaf action

compile([([], action), ...], _):
  return Leaf action                     -- first row matches

compile(matrix, actions):
  let i = select_column(matrix)          -- heuristic: most constructors
  let x = scrutinee_var(i)
  let Σ = constructors appearing in column i

  let branches = for C in Σ:
    let (rows', actions') = specialize_with_actions(matrix, actions, i, C)
    (C, fresh_vars(arity(C)), compile(rows', actions'))

  let default_tree =
    let (rows', actions') = default_with_actions(matrix, actions, i)
    if rows' is empty: Fail
    else: compile(rows', actions')

  return Switch x (branches ++ default_tree)
```

### 8.3 Or-Pattern Expansion

```
⟦ when p₁ | p₂ -> e ⟧ = ⟦ when p₁ -> e ⟧ ++ ⟦ when p₂ -> e ⟧
```

Or-patterns are expanded to multiple rows before matrix construction.

**Constraint:** Variables bound in p₁ must equal variables bound in p₂.

### 8.4 As-Pattern Compilation

```
⟦ when (p as x) -> e ⟧ =
  compile p, then wrap result:
    let x = scrutinee in ⟦e⟧
```

### 8.5 Guard Compilation

```
⟦ when p if g -> e₁; ... ⟧ =
  compile to:
    match scrutinee with p →
      if g then e₁
      else <continue to next arm>
```

Guards introduce potential fallthrough, handled by nested case/if.

---

## 9. Modules

### 9.1 Semantics

Modules are **namespaces**. They do not generate new types or have runtime representation.

### 9.2 Module Contents

A module may contain:

- Type declarations
- Let declarations (values, functions)
- Foreign declarations

### 9.3 Visibility

All declarations in a module are public. There is no hiding mechanism.

### 9.4 Import Resolution

```
use M (..)                -- import all from M
use M (x, y, T(..))       -- import specific items
use M as N                -- alias module
```

Import resolution:

1. Find module M in scope
2. For each item in import spec:
   - If `x`: add `x` to value namespace
   - If `T`: add `T` to type namespace
   - If `T(..)`: add `T` and all constructors of T
   - If `T(C₁, C₂)`: add `T` and specified constructors
3. If alias, additionally bind `N` → `M` in module namespace

### 9.5 Qualified Access

```
M.x       -- value x from module M
M.T       -- type T from module M (in type context)
M.C       -- constructor C from module M
```

### 9.6 Shadowing

- Local bindings shadow imports
- Later imports shadow earlier imports
- Qualified access bypasses shadowing

---

## 10. Foreign Interface

### 10.1 Foreign Declarations

```
foreign name : type
```

A foreign declaration introduces a binding with a known type but no implementation.

### 10.2 Semantics

- **Binding:** Foreign names enter the symbol table normally
- **Typing:** Foreign types are trusted (no inference)
- **Elaboration:** Foreign calls pass through unchanged
- **Codegen:** Backend must provide implementations

### 10.3 Foreign in Modules

Foreign declarations may appear inside modules:

```ml
module String
  foreign length : String -> Int
  foreign concat : String -> String -> String

  let isEmpty s = length s == 0    -- uses foreign
end
```

### 10.4 Platform Contract

Each backend defines a mapping:

```
ForeignImpl : QualifiedName → Implementation
```

Missing implementations are link-time errors.

Example (JavaScript):

```javascript
const foreignImpls = {
  "String.length": (s) => s.length,
  "String.concat": (a, b) => a + b,
  "IO.print": (s) => console.log(s),
  // ...
};
```

### 10.5 Foreign Types

Foreign declarations may reference abstract types that have no Algow definition:

```ml
foreign currentTime : Unit -> Int    -- platform provides meaning of "time"
```

The type checker trusts the declared signature.

---

## 11. Standard Library

The standard library is written in Algow with foreign primitives. It is loaded before user code.

### 11.1 Primitive Modules

#### Int

```ml
module Int
  foreign add : Int -> Int -> Int
  foreign sub : Int -> Int -> Int
  foreign mul : Int -> Int -> Int
  foreign div : Int -> Int -> Int      -- truncating division
  foreign mod : Int -> Int -> Int
  foreign neg : Int -> Int
  foreign abs : Int -> Int
  foreign eq : Int -> Int -> Bool
  foreign lt : Int -> Int -> Bool
  foreign le : Int -> Int -> Bool
  foreign gt : Int -> Int -> Bool
  foreign ge : Int -> Int -> Bool
  foreign toFloat : Int -> Float
  foreign toString : Int -> String
  foreign fromString : String -> Maybe Int

  let min a b = if lt a b then a else b
  let max a b = if gt a b then a else b
  let clamp lo hi x = max lo (min hi x)
end
```

#### Float

```ml
module Float
  foreign add : Float -> Float -> Float
  foreign sub : Float -> Float -> Float
  foreign mul : Float -> Float -> Float
  foreign div : Float -> Float -> Float
  foreign neg : Float -> Float
  foreign abs : Float -> Float
  foreign eq : Float -> Float -> Bool
  foreign lt : Float -> Float -> Bool
  foreign le : Float -> Float -> Bool
  foreign gt : Float -> Float -> Bool
  foreign ge : Float -> Float -> Bool
  foreign floor : Float -> Int
  foreign ceil : Float -> Int
  foreign round : Float -> Int
  foreign sqrt : Float -> Float
  foreign pow : Float -> Float -> Float
  foreign sin : Float -> Float
  foreign cos : Float -> Float
  foreign tan : Float -> Float
  foreign log : Float -> Float
  foreign exp : Float -> Float
  foreign toString : Float -> String
  foreign fromString : String -> Maybe Float

  let pi = 3.14159265358979323846
  let e = 2.71828182845904523536
end
```

#### String

```ml
module String
  foreign length : String -> Int
  foreign concat : String -> String -> String
  foreign substring : Int -> Int -> String -> String
  foreign charAt : Int -> String -> Maybe Char
  foreign toList : String -> List Char
  foreign fromList : List Char -> String
  foreign eq : String -> String -> Bool
  foreign lt : String -> String -> Bool
  foreign split : String -> String -> List String
  foreign join : String -> List String -> String
  foreign trim : String -> String
  foreign toUpper : String -> String
  foreign toLower : String -> String
  foreign contains : String -> String -> Bool
  foreign startsWith : String -> String -> Bool
  foreign endsWith : String -> String -> Bool
  foreign replace : String -> String -> String -> String

  let isEmpty s = length s == 0
  let reverse s = s |> toList |> List.reverse |> fromList
end
```

#### Char

```ml
module Char
  foreign toInt : Char -> Int
  foreign fromInt : Int -> Maybe Char
  foreign toString : Char -> String
  foreign eq : Char -> Char -> Bool
  foreign lt : Char -> Char -> Bool
  foreign isDigit : Char -> Bool
  foreign isAlpha : Char -> Bool
  foreign isAlphaNum : Char -> Bool
  foreign isSpace : Char -> Bool
  foreign isUpper : Char -> Bool
  foreign isLower : Char -> Bool
  foreign toUpper : Char -> Char
  foreign toLower : Char -> Char
end
```

#### Bool

```ml
module Bool
  let not b = if b then false else true
  let eq a b = if a then b else not b
end
```

### 11.2 Data Structure Modules

#### List

```ml
module List
  type List a = Nil | Cons a (List a)

  let isEmpty xs = match xs
    when Nil -> true
    when _ -> false
  end

  let rec length xs = match xs
    when Nil -> 0
    when Cons _ rest -> 1 + length rest
  end

  let head xs = match xs
    when Nil -> Nothing
    when Cons x _ -> Just x
  end

  let tail xs = match xs
    when Nil -> Nothing
    when Cons _ rest -> Just rest
  end

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

  let rec foldl f acc xs = match xs
    when Nil -> acc
    when Cons x rest -> foldl f (f acc x) rest
  end

  let rec foldr f acc xs = match xs
    when Nil -> acc
    when Cons x rest -> f x (foldr f acc rest)
  end

  let reverse xs = foldl (acc x -> Cons x acc) Nil xs

  let rec append xs ys = match xs
    when Nil -> ys
    when Cons x rest -> Cons x (append rest ys)
  end

  let concat xss = foldr append Nil xss

  let rec take n xs =
    if n <= 0 then Nil
    else match xs
      when Nil -> Nil
      when Cons x rest -> Cons x (take (n - 1) rest)
    end

  let rec drop n xs =
    if n <= 0 then xs
    else match xs
      when Nil -> Nil
      when Cons _ rest -> drop (n - 1) rest
    end

  let rec zip xs ys = match (xs, ys)
    when (Cons x xrest, Cons y yrest) -> Cons (x, y) (zip xrest yrest)
    when _ -> Nil
  end

  let rec any p xs = match xs
    when Nil -> false
    when Cons x rest -> if p x then true else any p rest
  end

  let rec all p xs = match xs
    when Nil -> true
    when Cons x rest -> if p x then all p rest else false
  end

  let rec find p xs = match xs
    when Nil -> Nothing
    when Cons x rest -> if p x then Just x else find p rest
  end
end
```

#### Maybe

```ml
module Maybe
  type Maybe a = Nothing | Just a

  let isJust m = match m
    when Just _ -> true
    when Nothing -> false
  end

  let isNothing m = match m
    when Nothing -> true
    when Just _ -> false
  end

  let map f m = match m
    when Nothing -> Nothing
    when Just x -> Just (f x)
  end

  let flatMap f m = match m
    when Nothing -> Nothing
    when Just x -> f x
  end

  let withDefault default m = match m
    when Nothing -> default
    when Just x -> x
  end

  let toList m = match m
    when Nothing -> Nil
    when Just x -> Cons x Nil
  end
end
```

#### Either

```ml
module Either
  type Either a b = Left a | Right b

  let isLeft e = match e
    when Left _ -> true
    when Right _ -> false
  end

  let isRight e = match e
    when Left _ -> false
    when Right _ -> true
  end

  let map f e = match e
    when Left a -> Left a
    when Right b -> Right (f b)
  end

  let mapLeft f e = match e
    when Left a -> Left (f a)
    when Right b -> Right b
  end

  let flatMap f e = match e
    when Left a -> Left a
    when Right b -> f b
  end

  let withDefault default e = match e
    when Left _ -> default
    when Right b -> b
  end

  let fromMaybe err m = match m
    when Nothing -> Left err
    when Just x -> Right x
  end
end
```

### 11.3 IO Module

```ml
module IO
  foreign print : String -> Unit
  foreign printLine : String -> Unit
  foreign readLine : Unit -> String
  foreign readFile : String -> String
  foreign writeFile : String -> String -> Unit
  foreign appendFile : String -> String -> Unit
  foreign fileExists : String -> Bool
  foreign deleteFile : String -> Unit
  foreign args : Unit -> List String
  foreign exit : Int -> Unit
  foreign getEnv : String -> Maybe String
end
```

### 11.4 Debug Module

```ml
module Debug
  foreign log : a -> a                  -- prints value, returns it
  foreign trace : String -> a -> a      -- prints label + value, returns value
  foreign panic : String -> a           -- crashes with message
end
```

### 11.5 Prelude (Implicit Imports)

The following are implicitly imported into every module:

```ml
use Bool (true, false)
use Maybe (Maybe(..))
use Either (Either(..))
use List (List(..))
```

---

## 12. Compiler Pipeline

### 12.1 Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      Source Text                        │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│  PHASE 1: Lexing                                        │
│  ─────────────────                                      │
│  Input:  string                                         │
│  Output: Token[]                                        │
│  Errors: Invalid characters, unterminated strings       │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│  PHASE 2: Parsing                                       │
│  ───────────────                                        │
│  Input:  Token[]                                        │
│  Output: SurfaceAST                                     │
│  Errors: Syntax errors                                  │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│  PHASE 3: Binding & Kind Checking                       │
│  ────────────────────────────────                       │
│  Input:  SurfaceAST                                     │
│  Output: BoundAST, SymbolTable, ADTRegistry             │
│  Errors: Unbound names, duplicate definitions,          │
│          kind/arity errors                              │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│  PHASE 4: Exhaustiveness Checking                       │
│  ────────────────────────────────                       │
│  Input:  BoundAST, ADTRegistry                          │
│  Output: Warnings/Errors                                │
│  Errors: Non-exhaustive patterns, redundant patterns    │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│  PHASE 5: Elaboration + Type Inference                  │
│  ─────────────────────────────────────                  │
│  Input:  BoundAST, SymbolTable                          │
│  Output: TypedCoreAST                                   │
│  Errors: Type mismatches, unification failures          │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│  PHASE 6: Optimization                                  │
│  ────────────────────                                   │
│  Input:  TypedCoreAST                                   │
│  Output: TypedCoreAST (optimized)                       │
│  Transformations: β-reduction, inlining, DCE            │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│  PHASE 7: Code Generation                               │
│  ────────────────────────                               │
│  Input:  TypedCoreAST, ForeignImpls                     │
│  Output: Target code (JS, bytecode, WASM, etc.)         │
│  Errors: Missing foreign implementations                │
└─────────────────────────────────────────────────────────┘
```

### 12.2 Phase Invariants

| Phase          | Input        | Output          | May Not                      |
| -------------- | ------------ | --------------- | ---------------------------- |
| Lexing         | string       | Token[]         | Access syntax or semantics   |
| Parsing        | Token[]      | SurfaceAST      | Resolve names or check types |
| Binding        | SurfaceAST   | BoundAST        | Check types or desugar       |
| Exhaustiveness | BoundAST     | Errors/Warnings | Transform AST or check types |
| Elaboration    | BoundAST     | TypedCoreAST    | Leave surface syntax         |
| Optimization   | TypedCoreAST | TypedCoreAST    | Change semantics             |
| Codegen        | TypedCoreAST | Target          | Know about surface syntax    |

### 12.3 Data Structures

#### Token

```typescript
type Token = {
  tag: TokenTag;
  value?: string | number;
  loc: SourceLocation;
};

type SourceLocation = {
  file: string;
  line: number;
  column: number;
  offset: number;
};
```

#### Surface AST

```typescript
type SurfaceExpr =
  | { tag: "Var"; name: string; loc: Loc }
  | { tag: "Constructor"; name: string; loc: Loc }
  | { tag: "Qualified"; module: string; name: string; loc: Loc }
  | { tag: "Literal"; value: Literal; loc: Loc }
  | { tag: "Lambda"; params: Param[]; body: SurfaceExpr; loc: Loc }
  | { tag: "App"; func: SurfaceExpr; arg: SurfaceExpr; loc: Loc }
  | { tag: "Let"; rec: boolean; bindings: Binding[]; body: SurfaceExpr; loc: Loc }
  | { tag: "If"; cond: SurfaceExpr; then_: SurfaceExpr; else_: SurfaceExpr; loc: Loc }
  | { tag: "Match"; scrutinee: SurfaceExpr; arms: MatchArm[]; loc: Loc }
  | { tag: "Binary"; op: BinaryOp; left: SurfaceExpr; right: SurfaceExpr; loc: Loc }
  | { tag: "Unary"; op: UnaryOp; operand: SurfaceExpr; loc: Loc }
  | { tag: "Tuple"; elements: SurfaceExpr[]; loc: Loc }
  | { tag: "Record"; fields: Field[]; loc: Loc }
  | { tag: "FieldAccess"; record: SurfaceExpr; field: string; loc: Loc }
  | { tag: "List"; elements: SurfaceExpr[]; loc: Loc };
```

#### Bound AST

Same structure as Surface AST, but identifiers are replaced with symbols:

```typescript
type Symbol = {
  id: number; // unique identifier
  name: string; // original name (for error messages)
  kind: SymbolKind; // Value | Type | Constructor | Module
};
```

#### Core AST

```typescript
type CoreExpr =
  | { tag: "Var"; symbol: Symbol; type: Type }
  | { tag: "Lambda"; param: Symbol; body: CoreExpr; type: Type }
  | { tag: "App"; func: CoreExpr; arg: CoreExpr; type: Type }
  | { tag: "Let"; rec: boolean; bindings: CoreBinding[]; body: CoreExpr; type: Type }
  | { tag: "Case"; scrutinee: CoreExpr; branches: CoreBranch[]; type: Type }
  | { tag: "Record"; fields: Map<string, CoreExpr>; type: Type }
  | { tag: "FieldAccess"; record: CoreExpr; field: string; type: Type }
  | { tag: "Constructor"; name: string; type: Type }
  | { tag: "Literal"; value: Literal; type: Type }
  | { tag: "Foreign"; module: string; name: string; type: Type };

type CoreBranch = {
  constructor: string;
  bindings: Symbol[];
  body: CoreExpr;
};
```

#### Types

```typescript
type Type =
  | { tag: "Var"; id: number }
  | { tag: "Arrow"; param: Type; result: Type }
  | { tag: "Constructor"; name: string; args: Type[] }
  | { tag: "Record"; row: Row };

type Row =
  | { tag: "Empty" }
  | { tag: "Extend"; label: string; type: Type; rest: Row }
  | { tag: "RowVar"; id: number };

type TypeScheme = {
  quantified: number[]; // bound type variable IDs
  body: Type;
};
```

---

## 13. References

### Type Systems

- **Hindley, R.** (1969). "The Principal Type-Scheme of an Object in Combinatory Logic"
- **Milner, R.** (1978). "A Theory of Type Polymorphism in Programming"
- **Damas, L. & Milner, R.** (1982). "Principal Type-Schemes for Functional Programs"
- **Pottier, F. & Rémy, D.** (2005). "The Essence of ML Type Inference"
- **Rémy, D.** (1989). "Type Inference for Records in a Natural Extension of ML"
- **Pierce, B.** (2002). _Types and Programming Languages_
- **Harper, R.** (2016). _Practical Foundations for Programming Languages_

### Pattern Matching

- **Maranget, L.** (2007). "Warnings for Pattern Matching"
- **Maranget, L.** (2008). "Compiling Pattern Matching to Good Decision Trees"
- **Wadler, P.** (1987). "Efficient Compilation of Pattern-Matching"

### Parsing

- **Pratt, V.** (1973). "Top Down Operator Precedence"
- **Aho, A., Lam, M., Sethi, R., Ullman, J.** (2006). _Compilers: Principles, Techniques, and Tools_

### Compilation

- **Appel, A.** (1992). _Compiling with Continuations_
- **Appel, A.** (1998). _Modern Compiler Implementation in ML_
- **Flanagan, C. et al.** (1993). "The Essence of Compiling with Continuations"
- **Peyton Jones, S.** (1987). _The Implementation of Functional Programming Languages_

### Semantics

- **Plotkin, G.** (1981). "A Structural Approach to Operational Semantics"

---

## Appendix A: Complete Grammar

```
program      ::= module_decl* use_decl* decl* expr?

module_decl  ::= 'module' UPPER decl* 'end'

use_decl     ::= 'use' module_path import_spec? ('as' UPPER)?

module_path  ::= UPPER ('.' UPPER)*

import_spec  ::= '(' '..' ')'
               | '(' import_item (',' import_item)* ')'

import_item  ::= LOWER | UPPER | UPPER '(' '..' ')' | UPPER '(' UPPER (',' UPPER)* ')'

decl         ::= type_decl | let_decl | foreign_decl

type_decl    ::= 'type' UPPER LOWER* '=' ctor_def ('|' ctor_def)*

ctor_def     ::= UPPER type_atom*

let_decl     ::= 'let' 'rec'? binding ('and' binding)*

binding      ::= LOWER param* (':' type)? '=' expr
               | pattern '=' expr

param        ::= LOWER | '(' LOWER ':' type ')'

foreign_decl ::= 'foreign' LOWER ':' type

expr         ::= let_expr | if_expr | match_expr | lambda_expr | binary_expr

let_expr     ::= 'let' 'rec'? binding ('and' binding)* 'in' expr

if_expr      ::= 'if' expr 'then' expr 'else' expr

match_expr   ::= 'match' expr match_arm+ 'end'

match_arm    ::= 'when' pattern guard? '->' expr

guard        ::= 'if' expr

lambda_expr  ::= param+ '->' expr

binary_expr  ::= unary_expr (binary_op unary_expr)*

unary_expr   ::= '-' unary_expr | app_expr

app_expr     ::= primary_expr+

primary_expr ::= LOWER | UPPER | qualified_name | literal
               | '(' expr ')' | '(' expr ',' expr (',' expr)* ')'
               | '{' field_def (',' field_def)* '}'
               | '[' (expr (',' expr)*)? ']'
               | primary_expr '.' LOWER
               | primary_expr '.' INT

qualified_name ::= UPPER '.' (LOWER | UPPER)

field_def    ::= LOWER '=' expr | LOWER

literal      ::= INT | FLOAT | STRING | CHAR | 'true' | 'false' | '(' ')'

pattern      ::= pattern_term ('|' pattern_term)*

pattern_term ::= pattern_atom ('as' LOWER)?

pattern_atom ::= '_' | LOWER | literal | UPPER pattern_atom* | UPPER '.' UPPER pattern_atom*
               | '(' pattern (',' pattern)+ ')' | '{' field_pat (',' field_pat)* '}'
               | '(' pattern ')'

field_pat    ::= LOWER '=' pattern | LOWER

type         ::= type_app ('->' type)?

type_app     ::= type_atom+

type_atom    ::= LOWER | UPPER | '(' type ')' | '(' type (',' type)+ ')' | '{' row '}'

row          ::= field_type (',' field_type)* ('|' LOWER)? | LOWER

field_type   ::= LOWER ':' type

binary_op    ::= '|>' | '::' | '||' | '&&' | '==' | '!=' | '<' | '<=' | '>' | '>='
               | '++' | '+' | '-' | '*' | '/' | '%'
```

---

## Appendix B: Operator Elaboration Summary

| Surface    | Condition        | Core                                    |
| ---------- | ---------------- | --------------------------------------- |
| `a + b`    | `Int, Int`       | `Int.add a b`                           |
| `a + b`    | `Float, Float`   | `Float.add a b`                         |
| `a + b`    | `Int, Float`     | `Float.add (Int.toFloat a) b`           |
| `a + b`    | `Float, Int`     | `Float.add a (Int.toFloat b)`           |
| `a - b`    | (same as +)      | `_.sub`                                 |
| `a * b`    | (same as +)      | `_.mul`                                 |
| `a / b`    | (same as +)      | `_.div`                                 |
| `a % b`    | `Int, Int`       | `Int.mod a b`                           |
| `-a`       | `Int`            | `Int.neg a`                             |
| `-a`       | `Float`          | `Float.neg a`                           |
| `a ++ b`   | `String, String` | `String.concat a b`                     |
| `a ++ b`   | `List α, List α` | `List.append a b`                       |
| `a == b`   | non-function     | `_.eq a b` (type-specific)              |
| `a != b`   | non-function     | `Bool.not (_.eq a b)`                   |
| `a < b`    | `Int`            | `Int.lt a b`                            |
| `a < b`    | `Float`          | `Float.lt a b`                          |
| `a < b`    | `Char`           | `Char.lt a b`                           |
| `a < b`    | `String`         | `String.lt a b`                         |
| `a <= b`   | (similar)        | `_.le a b`                              |
| `a > b`    | (similar)        | `_.gt a b`                              |
| `a >= b`   | (similar)        | `_.ge a b`                              |
| `a && b`   | `Bool, Bool`     | `case a of { True → b; False → false }` |
| `a \|\| b` | `Bool, Bool`     | `case a of { True → true; False → b }`  |
| `a \|> f`  | any              | `f a`                                   |
| `a :: b`   | `α, List α`      | `Cons a b`                              |

---

## Appendix C: Example Program

```ml
-- A complete Algow program

module Math
  let square x = x * x

  let rec factorial n =
    if n <= 0 then 1
    else n * factorial (n - 1)

  let rec fibonacci n =
    if n <= 1 then n
    else fibonacci (n - 1) + fibonacci (n - 2)
end

module Main
  use IO (printLine, readLine)
  use String (trim)
  use Maybe (..)
  use Math (..)

  let parseInt s = Int.fromString (trim s)

  let main =
    let _ = printLine "Enter a number:" in
    let input = readLine () in
    match parseInt input
      when Nothing -> printLine "Invalid number"
      when Just n ->
        let result = factorial n in
        printLine ("Factorial: " ++ Int.toString result)
    end
end

Main.main
```

---

_End of Specification_
