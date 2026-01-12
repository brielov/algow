# Algow Language Specification

This document is the authoritative reference for the Algow compiler implementation.
Every design decision, data structure, and algorithm is specified here.

---

## Table of Contents

1. [Philosophy](#1-philosophy)
2. [Compiler Pipeline](#2-compiler-pipeline)
3. [Lexical Structure](#3-lexical-structure)
4. [Surface Syntax](#4-surface-syntax)
5. [Core Language](#5-core-language)
6. [Desugaring Rules](#6-desugaring-rules)
7. [Name Resolution](#7-name-resolution)
8. [Type System](#8-type-system)
9. [Pattern Compilation](#9-pattern-compilation)
10. [Intermediate Representation](#10-intermediate-representation)
11. [Optimization](#11-optimization)
12. [Code Generation](#12-code-generation)
13. [References](#13-references)

---

## 1. Philosophy

Algow is implemented using **established, textbook algorithms**. We do not invent new techniques.
Every phase uses canonical approaches from the academic literature.

Guiding principles:

- **Clarity over cleverness**: Code should be readable and teachable
- **Separation of concerns**: Each phase does one thing well
- **Academic rigor**: Use named algorithms with proper references
- **No hacks**: If something is hard, use the known solution

---

## 2. Compiler Pipeline

```
Source Code
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ LEXER                                                                       │
│ Algorithm: Maximal munch                                                    │
│ Input:     Source text                                                      │
│ Output:    Token stream with attached comments (trivia)                     │
└─────────────────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ PARSER                                                                      │
│ Algorithm: Pratt parsing (Top-Down Operator Precedence)                     │
│ Reference: Pratt, "Top Down Operator Precedence" (1973)                     │
│ Input:     Token stream                                                     │
│ Output:    Surface AST                                                      │
└─────────────────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ DESUGARING                                                                  │
│ Algorithm: Source-to-source transformation                                  │
│ Input:     Surface AST                                                      │
│ Output:    Core AST                                                         │
└─────────────────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ NAME RESOLUTION                                                             │
│ Algorithm: Symbol table construction                                        │
│ Input:     Core AST                                                         │
│ Output:    Core AST with unique identifiers                                 │
└─────────────────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ TYPE INFERENCE                                                              │
│ Algorithm: Algorithm W (Damas-Milner)                                       │
│ Reference: Damas & Milner, "Principal type-schemes for functional          │
│            programs" (1982)                                                 │
│ Input:     Core AST                                                         │
│ Output:    Typed Core AST                                                   │
└─────────────────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ PATTERN COMPILATION                                                         │
│ Algorithm: Decision trees                                                   │
│ Reference: Maranget, "Compiling Pattern Matching to Good Decision          │
│            Trees" (2008)                                                    │
│ Input:     Typed Core AST                                                   │
│ Output:    Typed Core AST (patterns simplified)                             │
└─────────────────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ IR LOWERING                                                                 │
│ Algorithm: A-Normal Form conversion                                         │
│ Reference: Flanagan et al., "The Essence of Compiling with                  │
│            Continuations" (1993)                                            │
│ Input:     Typed Core AST                                                   │
│ Output:    ANF IR                                                           │
└─────────────────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ OPTIMIZATION                                                                │
│ Algorithm: Rewrite rules (algebraic transformations)                        │
│ Reference: Appel, "Compiling with Continuations" (1992)                     │
│            Peyton Jones, "The Implementation of Functional Programming      │
│            Languages" (1987)                                                │
│ Input:     ANF IR                                                           │
│ Output:    ANF IR (optimized)                                               │
└─────────────────────────────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│ CODE GENERATION                                                             │
│ Algorithm: Direct translation                                               │
│ Input:     ANF IR (optimized)                                               │
│ Output:    JavaScript                                                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 3. Lexical Structure

### 3.1 Token Categories

```
Token
  -- Literals
  = TokInt Int                    -- 42
  | TokFloat Float                -- 3.14
  | TokString String              -- "hello"
  | TokChar Char                  -- 'a'

  -- Identifiers
  | TokLower String               -- foo, _bar (variables, type variables)
  | TokUpper String               -- Just, Nothing (constructors, type constructors)

  -- Keywords
  | TokLet                        -- let
  | TokRec                        -- rec
  | TokAnd                        -- and
  | TokIn                         -- in
  | TokIf                         -- if
  | TokThen                       -- then
  | TokElse                       -- else
  | TokMatch                      -- match
  | TokWhen                       -- when
  | TokEnd                        -- end
  | TokType                       -- type
  | TokModule                     -- module
  | TokUse                        -- use
  | TokForeign                    -- foreign
  | TokAs                         -- as
  | TokDo                         -- do
  | TokTrue                       -- true
  | TokFalse                      -- false

  -- Operators & Punctuation
  | TokPlus                       -- +
  | TokMinus                      -- -
  | TokStar                       -- *
  | TokSlash                      -- /
  | TokEq                         -- =
  | TokEqEq                       -- ==
  | TokNe                         -- !=
  | TokLt                         -- <
  | TokLe                         -- <=
  | TokGt                         -- >
  | TokGe                         -- >=
  | TokAnd2                       -- &&
  | TokOr2                        -- ||
  | TokArrow                      -- ->
  | TokLeftArrow                  -- <-
  | TokPipe                       -- |>
  | TokBar                        -- |
  | TokColon                      -- :
  | TokColon2                     -- ::
  | TokComma                      -- ,
  | TokDot                        -- .
  | TokUnderscore                 -- _
  | TokLParen                     -- (
  | TokRParen                     -- )
  | TokLBracket                   -- [
  | TokRBracket                   -- ]
  | TokLBrace                     -- {
  | TokRBrace                     -- }

  -- Special
  | TokEof
```

### 3.2 Trivia (Comments & Whitespace)

Comments attach to the nearest token for formatter support:

```
Trivia = {
  leading  : List Comment,
  trailing : List Comment
}

Comment
  = LineComment String            -- -- comment
  | BlockComment String           -- {- comment -}

Located a = {
  value    : a,
  span     : Span,
  trivia   : Trivia
}

Span = { start : Int, stop : Int }  -- Note: 'end' is a reserved keyword
```

---

## 4. Surface Syntax

The Surface AST preserves all syntactic sugar. This is what the parser produces.

### 4.1 Surface Expressions

```
Surface.Expr
  = SVar String                                   -- x
  | SLit Literal                                  -- 42, "hello"
  | SApp Surface.Expr Surface.Expr                -- f x
  | SAbs (List String) Surface.Expr               -- x y -> e (multi-param)
  | SLet String Surface.Expr Surface.Expr         -- let x = e1 in e2
  | SLetRec (List (String, Surface.Expr))         -- let rec f = e1 and g = e2
            Surface.Expr
  | SIf Surface.Expr Surface.Expr Surface.Expr    -- if c then t else f
  | SMatch Surface.Expr (List Surface.Case)       -- match e when ... end
  | SCon String                                   -- Just, Cons
  | STuple (List Surface.Expr)                    -- (a, b, c)
  | SRecord (List (String, Surface.Expr))         -- { x = 1, y = 2 }
  | SRecordUpdate Surface.Expr                    -- { r | x = 1 }
                  (List (String, Surface.Expr))
  | SField Surface.Expr String                    -- e.field
  | SList (List Surface.Expr)                     -- [1, 2, 3]
  | SPipe Surface.Expr Surface.Expr               -- e |> f
  | SCons Surface.Expr Surface.Expr               -- x :: xs
  | SBinOp String Surface.Expr Surface.Expr       -- a + b
  | SDo (List Surface.DoStmt)                     -- do { x <- e; ... } end
  | SAnnot Surface.Expr Surface.Type              -- (e : T)
```

### 4.2 Surface Patterns

```
Surface.Pattern
  = SPWild                                        -- _
  | SPVar String                                  -- x
  | SPLit Literal                                 -- 42, "hello"
  | SPCon String (List Surface.Pattern)           -- Just x
  | SPTuple (List Surface.Pattern)                -- (a, b)
  | SPRecord (List (String, Surface.Pattern))     -- { x, y }
  | SPAs String Surface.Pattern                   -- x as p
  | SPOr Surface.Pattern Surface.Pattern          -- p1 | p2
  | SPCons Surface.Pattern Surface.Pattern        -- x :: xs
  | SPList (List Surface.Pattern)                 -- [a, b, c]
```

### 4.3 Surface Cases and Do-Statements

```
Surface.Case = {
  pattern : Surface.Pattern,
  guard   : Maybe Surface.Expr,
  body    : Surface.Expr
}

Surface.DoStmt
  = DoBindPattern Surface.Pattern Surface.Expr    -- (a, b) <- e
  | DoLet Surface.Pattern Surface.Expr            -- let x = e
  | DoExpr Surface.Expr                           -- e
```

### 4.4 Surface Types

```
Surface.Type
  = STVar String                                  -- a (type variable)
  | STCon String                                  -- Int, String
  | STApp Surface.Type Surface.Type               -- Maybe Int
  | STFun Surface.Type Surface.Type               -- a -> b
  | STTuple (List Surface.Type)                   -- (Int, String)
  | STRecord (List (String, Surface.Type))        -- { x : Int, y : String }
```

### 4.5 Surface Declarations

```
Surface.Decl
  = SDeclType String (List String)                -- type Maybe a = ... (ADT)
              (List Surface.ConDecl)
  | SDeclTypeAlias String (List String)           -- type Point = { x : int, y : int }
                   Surface.Type                   -- type alias (record, function, tuple, etc.)
  | SDeclLet String Surface.Expr                  -- let x = e
  | SDeclLetRec (List (String, Surface.Expr))     -- let rec f = ... and g = ...
  | SDeclForeign String Surface.Type              -- foreign foo : Int -> Int
  | SDeclModule String (List String)              -- module Foo use ... end
                (List Surface.Decl)

Surface.ConDecl = {
  name   : String,
  fields : List Surface.Type
}
```

**Type Alias Examples:**

```
-- Record type alias
type Point = { x : int, y : int }
type Located a = { value : a, span : Span }

-- Function type alias
type Predicate a = a -> boolean
type BinaryOp a = a -> a -> a

-- Tuple type alias
type Pair a b = (a, b)

-- Parametric type alias
type StringMap a = Map string a
```

Type aliases are expanded during type checking. They do not create new types,
only alternative names for existing type expressions.

---

## 5. Core Language

The Core AST is the minimal, canonical representation. All surface sugar is removed.

### 5.1 Names

```
Name = {
  id       : Int,      -- Unique identifier (assigned during name resolution)
  original : String    -- Original name (for error messages & debugging)
}
```

### 5.2 Core Expressions

```
Core.Expr
  = CVar Name                                     -- variable
  | CLit Literal                                  -- literal
  | CApp Core.Expr Core.Expr                      -- application (f x)
  | CAbs Name Core.Expr                           -- lambda (\x -> e), single param
  | CLet Name Core.Expr Core.Expr                 -- let x = e1 in e2
  | CLetRec (List (Name, Core.Expr)) Core.Expr    -- let rec (mutual recursion)
  | CMatch Core.Expr (List Core.Case)             -- pattern match
  | CCon String                                   -- constructor
  | CTuple (List Core.Expr)                       -- (a, b, c)
  | CRecord (List (String, Core.Expr))            -- { x = 1, y = 2 }
  | CRecordUpdate Core.Expr                       -- { r | x = 1 }
                  (List (String, Core.Expr))
  | CField Core.Expr String                       -- e.field
  | CForeign String String                        -- foreign call (module, name)

Literal
  = LInt Int
  | LFloat Float
  | LString String
  | LChar Char
  | LBool Bool
```

### 5.3 Core Patterns

```
Core.Pattern
  = CPWild                                        -- _
  | CPVar Name                                    -- x (binds)
  | CPLit Literal                                 -- 42, "hello"
  | CPCon String (List Core.Pattern)              -- Just x
  | CPTuple (List Core.Pattern)                   -- (a, b)
  | CPRecord (List (String, Core.Pattern))        -- { x, y }
  | CPAs Name Core.Pattern                        -- x as p
  | CPOr Core.Pattern Core.Pattern                -- p1 | p2
```

### 5.4 Core Cases

```
Core.Case = {
  pattern : Core.Pattern,
  guard   : Maybe Core.Expr,
  body    : Core.Expr
}
```

### 5.5 Core Declarations

```
Core.Decl
  = CDeclType String (List String)                -- type declaration
              (List Core.ConDecl)
  | CDeclLet Name Core.Expr                       -- let binding
  | CDeclLetRec (List (Name, Core.Expr))          -- recursive bindings
  | CDeclForeign Name String String Core.Type     -- foreign (name, module, jsName, type)

Core.ConDecl = {
  name   : String,
  fields : List Core.Type
}
```

### 5.6 Core Types

```
Core.Type
  = CTVar String                                  -- type variable
  | CTCon String                                  -- type constructor
  | CTApp Core.Type Core.Type                     -- type application
  | CTFun Core.Type Core.Type                     -- function type
  | CTTuple (List Core.Type)                      -- tuple type
  | CTRecord (List (String, Core.Type))           -- record type
```

---

## 6. Desugaring Rules

Desugaring transforms Surface AST to Core AST via structural recursion.

### 6.1 Expression Desugaring

```
D⟦_⟧ : Surface.Expr -> Core.Expr

D⟦ SVar x ⟧                    = CVar x
D⟦ SLit l ⟧                    = CLit l
D⟦ SApp e1 e2 ⟧                = CApp (D⟦e1⟧) (D⟦e2⟧)
D⟦ SCon c ⟧                    = CCon c
D⟦ STuple es ⟧                 = CTuple (map D es)
D⟦ SRecord fs ⟧                = CRecord (map (λ(n,e) -> (n, D⟦e⟧)) fs)
D⟦ SRecordUpdate r fs ⟧        = CRecordUpdate (D⟦r⟧) (map (λ(n,e) -> (n, D⟦e⟧)) fs)
D⟦ SField e f ⟧                = CField (D⟦e⟧) f

-- Multi-param lambda desugars to nested single-param lambdas
D⟦ SAbs [] e ⟧                 = D⟦e⟧
D⟦ SAbs (x:xs) e ⟧             = CAbs x (D⟦ SAbs xs e ⟧)

-- Let and let rec
D⟦ SLet x e1 e2 ⟧              = CLet x (D⟦e1⟧) (D⟦e2⟧)
D⟦ SLetRec bs e ⟧              = CLetRec (map (λ(n,e) -> (n, D⟦e⟧)) bs) (D⟦e⟧)

-- If desugars to match on boolean
D⟦ SIf c t f ⟧                 = CMatch (D⟦c⟧) [
                                   Case (CPLit (LBool true)) Nothing (D⟦t⟧),
                                   Case (CPLit (LBool false)) Nothing (D⟦f⟧)
                                 ]

-- Match
D⟦ SMatch e cases ⟧            = CMatch (D⟦e⟧) (map D_case cases)

-- Pipe desugars to application (flip)
D⟦ SPipe e1 e2 ⟧               = CApp (D⟦e2⟧) (D⟦e1⟧)

-- Cons desugars to Cons constructor application
D⟦ SCons e1 e2 ⟧               = CApp (CApp (CCon "Cons") (D⟦e1⟧)) (D⟦e2⟧)

-- List literal desugars to nested Cons
D⟦ SList [] ⟧                  = CCon "Nil"
D⟦ SList (e:es) ⟧              = CApp (CApp (CCon "Cons") (D⟦e⟧)) (D⟦ SList es ⟧)

-- Binary operators desugar to function application
D⟦ SBinOp op e1 e2 ⟧           = CApp (CApp (CVar op) (D⟦e1⟧)) (D⟦e2⟧)

-- Type annotation is erased (used only during type checking)
D⟦ SAnnot e t ⟧                = D⟦e⟧

-- Do-notation desugars to flatMap
D⟦ SDo stmts ⟧                 = D_do⟦stmts⟧
```

### 6.2 Do-Notation Desugaring

```
D_do⟦_⟧ : List Surface.DoStmt -> Core.Expr

-- Final expression
D_do⟦ [DoExpr e] ⟧             = D⟦e⟧

-- Bind with simple variable
D_do⟦ DoBindPattern (SPVar x) e : rest ⟧
                               = CApp (CApp (CVar "flatMap")
                                           (CAbs x (D_do⟦rest⟧)))
                                      (D⟦e⟧)

-- Bind with pattern (needs intermediate match)
D_do⟦ DoBindPattern p e : rest ⟧
                               = CApp (CApp (CVar "flatMap")
                                           (CAbs $tmp
                                             (CMatch (CVar $tmp)
                                               [Case (D_pat⟦p⟧) Nothing (D_do⟦rest⟧)])))
                                      (D⟦e⟧)
  where $tmp = fresh()

-- Let with simple variable
D_do⟦ DoLet (SPVar x) e : rest ⟧
                               = CLet x (D⟦e⟧) (D_do⟦rest⟧)

-- Let with pattern
D_do⟦ DoLet p e : rest ⟧       = CMatch (D⟦e⟧)
                                   [Case (D_pat⟦p⟧) Nothing (D_do⟦rest⟧)]

-- Expression statement (discard result)
D_do⟦ DoExpr e : rest ⟧        = CApp (CApp (CVar "flatMap")
                                           (CAbs _ (D_do⟦rest⟧)))
                                      (D⟦e⟧)
```

### 6.3 Pattern Desugaring

```
D_pat⟦_⟧ : Surface.Pattern -> Core.Pattern

D_pat⟦ SPWild ⟧                = CPWild
D_pat⟦ SPVar x ⟧               = CPVar x
D_pat⟦ SPLit l ⟧               = CPLit l
D_pat⟦ SPCon c ps ⟧            = CPCon c (map D_pat ps)
D_pat⟦ SPTuple ps ⟧            = CPTuple (map D_pat ps)
D_pat⟦ SPRecord fs ⟧           = CPRecord (map (λ(n,p) -> (n, D_pat⟦p⟧)) fs)
D_pat⟦ SPAs x p ⟧              = CPAs x (D_pat⟦p⟧)
D_pat⟦ SPOr p1 p2 ⟧            = CPOr (D_pat⟦p1⟧) (D_pat⟦p2⟧)

-- Cons pattern desugars to Cons constructor pattern
D_pat⟦ SPCons p1 p2 ⟧          = CPCon "Cons" [D_pat⟦p1⟧, D_pat⟦p2⟧]

-- List pattern desugars to nested Cons pattern
D_pat⟦ SPList [] ⟧             = CPCon "Nil" []
D_pat⟦ SPList (p:ps) ⟧         = CPCon "Cons" [D_pat⟦p⟧, D_pat⟦ SPList ps ⟧]
```

---

## 7. Name Resolution

Name resolution assigns unique identifiers to all bindings and resolves variable references.

### 7.1 Environment

```
Env = Map String Name

resolve : Env -> String -> Either Error Name
extend  : Env -> String -> (Env, Name)
```

### 7.2 Algorithm

For each binding:

1. Generate fresh unique ID
2. Create Name with ID and original string
3. Extend environment
4. Resolve body with extended environment

For each variable reference:

1. Look up in environment
2. If found, replace with Name (including unique ID)
3. If not found, report "unbound variable" error

---

## 8. Type System

Algow uses the Hindley-Milner type system with Algorithm W for inference.

### 8.1 Types

```
Type
  = TVar String                      -- Type variable (α, β, ...)
  | TCon String                      -- Type constructor (Int, String, ...)
  | TApp Type Type                   -- Type application (Maybe Int)
  | TFun Type Type                   -- Function type (a -> b)
  | TTuple (List Type)               -- Tuple type (Int, String)
  | TRecord (List (String, Type))    -- Record type { x : Int }

Scheme = Forall (List String) Type   -- Polymorphic type scheme
```

### 8.2 Type Environment

```
TypeEnv = Map Name Scheme
```

### 8.3 Substitution

```
Subst = Map String Type

apply : Subst -> Type -> Type
apply s (TVar x)     = lookup x s |> withDefault (TVar x)
apply s (TCon c)     = TCon c
apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)
apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
apply s (TTuple ts)  = TTuple (map (apply s) ts)
apply s (TRecord fs) = TRecord (map (λ(n,t) -> (n, apply s t)) fs)

compose : Subst -> Subst -> Subst
compose s1 s2 = map (apply s1) s2 `union` s1
```

### 8.4 Unification

```
unify : Type -> Type -> Either Error Subst

unify (TVar x) t             = if x `occurs` t then Left "infinite type"
                               else Right (singleton x t)
unify t (TVar x)             = unify (TVar x) t
unify (TCon a) (TCon b)      = if a == b then Right empty
                               else Left "type mismatch"
unify (TApp a1 b1) (TApp a2 b2)
                             = do s1 <- unify a1 a2
                                  s2 <- unify (apply s1 b1) (apply s1 b2)
                                  pure (compose s2 s1)
unify (TFun a1 b1) (TFun a2 b2)
                             = do s1 <- unify a1 a2
                                  s2 <- unify (apply s1 b1) (apply s1 b2)
                                  pure (compose s2 s1)
unify (TTuple ts1) (TTuple ts2)
                             = unifyMany ts1 ts2
unify _ _                    = Left "type mismatch"
```

### 8.5 Algorithm W

```
W : TypeEnv -> Core.Expr -> Either Error (Subst, Type)

W env (CVar x)     = case lookup x env of
                       Nothing -> Left "unbound variable"
                       Just s  -> Right (empty, instantiate s)

W env (CLit l)     = Right (empty, typeOfLit l)

W env (CApp e1 e2) = do (s1, t1) <- W env e1
                        (s2, t2) <- W (apply s1 env) e2
                        tv <- fresh
                        s3 <- unify (apply s2 t1) (TFun t2 tv)
                        pure (compose s3 (compose s2 s1), apply s3 tv)

W env (CAbs x e)   = do tv <- fresh
                        let env' = extend env x (Forall [] tv)
                        (s, t) <- W env' e
                        pure (s, TFun (apply s tv) t)

W env (CLet x e1 e2)
                   = do (s1, t1) <- W env e1
                        let env' = apply s1 env
                        let sc = generalize env' t1
                        (s2, t2) <- W (extend env' x sc) e2
                        pure (compose s2 s1, t2)

W env (CLetRec bs e)
                   = -- See Damas-Milner paper for recursive case
                     ...

W env (CMatch e cases)
                   = -- Infer type of scrutinee, check each case
                     ...
```

### 8.6 Generalization & Instantiation

```
-- Free type variables
ftv : Type -> Set String
ftv (TVar x)     = singleton x
ftv (TCon _)     = empty
ftv (TApp a b)   = ftv a `union` ftv b
ftv (TFun a b)   = ftv a `union` ftv b
ftv (TTuple ts)  = unions (map ftv ts)
ftv (TRecord fs) = unions (map (ftv . snd) fs)

ftvEnv : TypeEnv -> Set String
ftvEnv env = unions (map (ftvScheme . snd) (toList env))

ftvScheme : Scheme -> Set String
ftvScheme (Forall vs t) = ftv t `difference` fromList vs

-- Generalize: create polymorphic scheme
generalize : TypeEnv -> Type -> Scheme
generalize env t = Forall (toList (ftv t `difference` ftvEnv env)) t

-- Instantiate: replace quantified variables with fresh ones
instantiate : Scheme -> Type
instantiate (Forall vs t) = apply (fromList (zip vs freshVars)) t
```

---

## 9. Pattern Compilation

Pattern matching is compiled to efficient decision trees.

Reference: Maranget, "Compiling Pattern Matching to Good Decision Trees" (2008)

### 9.1 Overview

A match expression:

```
match e
  when p1 -> e1
  when p2 -> e2
  when p3 -> e3
end
```

Is compiled to a decision tree that:

1. Minimizes redundant tests
2. Detects exhaustiveness
3. Warns on redundant patterns

### 9.2 Decision Trees

```
Decision
  = Fail                              -- Match failure (non-exhaustive)
  | Leaf Core.Expr                    -- Success, evaluate expression
  | Switch Core.Expr                  -- Test scrutinee
           (List (String, Decision))  -- Constructor -> subtree
           (Maybe Decision)           -- Default case
```

### 9.3 Algorithm

The algorithm builds a decision tree by:

1. Selecting a column (scrutinee component) to test
2. Grouping patterns by their head constructor
3. Recursively building subtrees for each group

Heuristics for column selection affect tree size and quality.

---

## 10. Intermediate Representation

The IR uses A-Normal Form (ANF), where all intermediate values are named.

Reference: Flanagan et al., "The Essence of Compiling with Continuations" (1993)

### 10.1 ANF Structure

```
-- Atoms: trivial expressions (no computation)
Atom
  = AVar Name
  | ALit Literal
  | ACon String

-- Compound expressions
IR.Expr
  = IRAtom Atom                               -- trivial
  | IRApp Atom Atom                           -- f x (both atoms)
  | IRLet Name IR.Binding IR.Expr             -- let x = binding in e
  | IRLetRec (List (Name, IR.Binding)) IR.Expr
  | IRMatch Atom (List IR.Case)               -- match on atom

IR.Binding
  = IRBApp Atom Atom                          -- function application
  | IRBTuple (List Atom)                      -- tuple construction
  | IRBRecord (List (String, Atom))           -- record construction
  | IRBField Atom String                      -- field access
  | IRBLambda Name IR.Expr                    -- lambda
  | IRBForeign String String (List Atom)      -- foreign call

IR.Case = {
  pattern : IR.Pattern,
  body    : IR.Expr
}

IR.Pattern
  = IRPWild
  | IRPVar Name
  | IRPLit Literal
  | IRPCon String (List IR.Pattern)
  | IRPTuple (List IR.Pattern)
```

### 10.2 ANF Conversion

The key operation is `normalize`, which ensures all subexpressions are atoms:

```
normalize : Core.Expr -> (Atom -> IR.Expr) -> IR.Expr

normalize (CVar x) k     = k (AVar x)
normalize (CLit l) k     = k (ALit l)
normalize (CCon c) k     = k (ACon c)
normalize (CApp e1 e2) k = normalize e1 (λa1 ->
                           normalize e2 (λa2 ->
                           let x = fresh() in
                           IRLet x (IRBApp a1 a2) (k (AVar x))))
...
```

---

## 11. Optimization

Optimization transforms ANF IR to equivalent but more efficient ANF IR.
All transformations preserve semantics.

Reference: Appel, "Compiling with Continuations" (1992)

### 11.1 Overview

Optimizations are applied as rewrite rules. Each rule has:

- A pattern to match
- A condition (optional)
- A replacement

Rules are applied repeatedly until a fixed point is reached.

### 11.2 Dead Code Elimination

Remove let bindings whose bound variable is never used.

```
DCE⟦ let x = e1 in e2 ⟧ = e2    when x ∉ fv(e2)
```

Where `fv(e)` denotes the free variables of expression `e`.

### 11.3 Constant Folding

Evaluate constant expressions at compile time.

```
CF⟦ x + y ⟧ = n           when x, y are literals and n = x + y
CF⟦ x - y ⟧ = n           when x, y are literals and n = x - y
CF⟦ x * y ⟧ = n           when x, y are literals and n = x * y
CF⟦ x / y ⟧ = n           when x, y are literals, y ≠ 0, and n = x / y

CF⟦ x == y ⟧ = true       when x, y are literals and x = y
CF⟦ x == y ⟧ = false      when x, y are literals and x ≠ y

CF⟦ true && e ⟧ = e
CF⟦ false && e ⟧ = false
CF⟦ true || e ⟧ = true
CF⟦ false || e ⟧ = e

CF⟦ not true ⟧ = false
CF⟦ not false ⟧ = true
```

### 11.4 Constant Propagation

Substitute known constant values for variables.

```
CP⟦ let x = lit in e ⟧ = e[x := lit]    when x is used only in trivial contexts
```

Note: Must be careful with sharing - only propagate if it doesn't duplicate work.

### 11.5 Beta Reduction

Substitute function arguments directly.

```
β⟦ (λx. e) v ⟧ = e[x := v]    when v is an atom (variable or literal)
```

For non-atomic arguments, introduce a let binding:

```
β⟦ (λx. e) expr ⟧ = let y = expr in e[x := y]    when expr is not atomic
```

### 11.6 Eta Reduction

Simplify redundant lambda wrappers.

```
η⟦ λx. f x ⟧ = f    when x ∉ fv(f)
```

### 11.7 Inlining

Replace function calls with the function body.

```
INLINE⟦ let f = λx. body in ... f arg ... ⟧
  = let f = λx. body in ... body[x := arg] ...

  when:
    - f is called exactly once, OR
    - body is "small" (heuristic: < N AST nodes), OR
    - f is marked for inlining
```

Inlining criteria (heuristics):

- Function body size < threshold (e.g., 10 nodes)
- Function called exactly once
- Function is a simple wrapper

### 11.8 Case-of-Known-Constructor

When matching on a known constructor, select the branch directly.

```
COKC⟦ match Con(v1, ..., vn)
        when Con(x1, ..., xn) -> e
        ...
      end ⟧
  = e[x1 := v1, ..., xn := vn]
```

### 11.9 Case-of-Case

Hoist inner match out of scrutinee position.

```
COC⟦ match (match e when p1 -> e1 when p2 -> e2 end)
       when q1 -> r1
       when q2 -> r2
     end ⟧
  = match e
      when p1 -> match e1 when q1 -> r1 when q2 -> r2 end
      when p2 -> match e2 when q1 -> r1 when q2 -> r2 end
    end
```

Note: May duplicate code. Apply only when beneficial.

### 11.10 Tail Call Optimization

Convert tail calls to loops to avoid stack overflow.

A call `f(args)` is in tail position if it is the last action before returning.

```
-- Before TCO
let rec loop = n acc ->
  if n == 0 then acc
  else loop (n - 1) (acc + n)

-- After TCO (conceptual)
let loop = n acc ->
  while true do
    if n == 0 then return acc
    else { n := n - 1; acc := acc + n }
```

Implementation: Detect tail-recursive functions and generate iterative code.

### 11.11 Let Floating

Move let bindings to optimize sharing and reduce closure size.

**Let floating inward** (reduce closure capture):

```
LFI⟦ let x = e1 in λy. e2 ⟧ = λy. let x = e1 in e2
  when x ∉ fv(e1) contains no free variables captured by the lambda
```

**Let floating outward** (enable further optimizations):

```
LFO⟦ (let x = e1 in e2) arg ⟧ = let x = e1 in (e2 arg)
  when x ∉ fv(arg)
```

### 11.12 Optimization Ordering

Optimizations should be applied in phases:

1. **Simplification** (repeated until fixed point):
   - Constant folding
   - Constant propagation
   - Beta reduction
   - Eta reduction
   - Dead code elimination

2. **Inlining** (selective):
   - Inline small functions
   - Inline single-use functions

3. **Case optimizations**:
   - Case-of-known-constructor
   - Case-of-case (careful with code duplication)

4. **Let floating**:
   - Float inward to reduce closures
   - Float outward selectively

5. **Tail call optimization**:
   - Detect and transform tail-recursive functions

6. **Final cleanup**:
   - Dead code elimination (again, after other opts)

### 11.13 Preserving Semantics

All optimizations must preserve:

- **Termination behavior**: If original terminates, optimized terminates
- **Result value**: Same observable result
- **Side effect order**: For foreign calls (no reordering)

---

## 12. Code Generation

JavaScript code is generated directly from optimized ANF IR.

### 12.1 Runtime Representation

| Algow       | JavaScript                  |
| ----------- | --------------------------- |
| Int, Float  | `number`                    |
| String      | `string`                    |
| Char        | `string` (length 1)         |
| Bool        | `boolean`                   |
| Tuple       | `Array`                     |
| Record      | `Object`                    |
| Constructor | `Array` (tagged, see below) |
| Function    | `function`                  |

### 12.2 Constructor Representation

**Default representation**: Constructors are represented as arrays with a numeric tag at index 0:

```
[tag, arg1, arg2, ...]
```

Tags are assigned per type definition, starting from 0:

```
type Maybe a = Nothing | Just a

Nothing  →  [0]
Just 42  →  [1, 42]
```

```
type Either a b = Left a | Right b

Left "error"   →  [0, "error"]
Right { x: 1 } →  [1, { x: 1 }]
```

**Special case: List**

`List` is the most common data structure in functional programming. For performance,
it uses an optimized object-based linked list representation instead of tagged arrays:

```
type List a = Nil | Cons a (List a)

Nil              →  null
Cons x xs        →  { h: x, t: xs }
```

Examples:

```
Nil                        →  null
Cons 1 Nil                 →  { h: 1, t: null }
Cons 1 (Cons 2 Nil)        →  { h: 1, t: { h: 2, t: null } }
[1, 2, 3]                  →  { h: 1, t: { h: 2, t: { h: 3, t: null } } }
```

**Why this optimization:**

1. `xs === null` is faster than `xs[0] === 0` (no array index)
2. V8 optimizes objects with consistent shape (hidden classes)
3. No tag overhead - Nil is just `null`
4. Less memory per node (no array wrapper, no tag)
5. More idiomatic JavaScript

**Pattern matching on List:**

```javascript
// match xs when Nil -> e1 when Cons h t -> e2 end
if (xs === null) {
  // Nil branch: e1
} else {
  const h = xs.h;
  const t = xs.t;
  // Cons branch: e2
}
```

This is the **only** special-cased type. All user-defined ADTs use the default
`[tag, ...args]` representation.

**Tag assignment rule**: Constructors are numbered in declaration order.

```
type Token
  = TokInt Int      -- tag 0
  | TokFloat Float  -- tag 1
  | TokString String -- tag 2
  | TokEof          -- tag 3
```

**Pattern matching** uses numeric comparison:

```javascript
// match x when Nothing -> ... when Just v -> ... end
if (x[0] === 0) {
  // Nothing branch
} else {
  const v = x[1];
  // Just branch
}

// Or with switch for many cases:
switch (x[0]) {
  case 0:
    /* Nil */ break;
  case 1:
    /* Cons */ const h = x[1];
    const t = x[2];
    break;
}
```

**Benefits of array representation**:

1. Minimal memory footprint
2. Fast integer tag comparison
3. No string allocation
4. V8 optimizes small integer-tagged arrays
5. Cache-friendly sequential access

**Debug mode** (optional): For development, can emit verbose objects:

```javascript
// --debug flag
{ $type: "Maybe", $tag: "Just", $args: [42] }
```

### 12.3 Code Generation Rules

```
G⟦_⟧ : IR.Expr -> JavaScript

G⟦ IRAtom (AVar x) ⟧         = x
G⟦ IRAtom (ALit l) ⟧         = literal(l)
G⟦ IRAtom (ACon c tag) ⟧     = [tag]                    -- nullary constructor

G⟦ IRLet x b e ⟧             = const x = G_bind⟦b⟧; G⟦e⟧

G⟦ IRMatch a cases ⟧         = switch (a[0]) { ... }    -- dispatch on tag

G_bind⟦ IRBApp f x ⟧         = f(x)                     -- direct call
G_bind⟦ IRBCon tag as ⟧      = [tag, G⟦a1⟧, G⟦a2⟧, ...] -- constructor
G_bind⟦ IRBTuple as ⟧        = [G⟦a1⟧, G⟦a2⟧, ...]
G_bind⟦ IRBRecord fs ⟧       = { f1: G⟦a1⟧, f2: G⟦a2⟧, ... }
G_bind⟦ IRBLambda x e ⟧      = (x) => G⟦e⟧
G_bind⟦ IRBForeign m n as ⟧  = $foreign.m.n(G⟦a1⟧)(G⟦a2⟧)...
```

**Pattern matching codegen:**

```javascript
// match x when Nil -> e1 when Cons h t -> e2 end
switch (x[0]) {
  case 0: return G⟦e1⟧;
  case 1: { const h = x[1]; const t = x[2]; return G⟦e2⟧; }
}
```

---

## 13. References

1. **Pratt, Vaughan R.** "Top Down Operator Precedence." _POPL_ (1973).

2. **Damas, Luis, and Robin Milner.** "Principal type-schemes for functional programs." _POPL_ (1982).

3. **Augustsson, Lennart.** "Compiling Pattern Matching." _Conference on Functional Programming Languages and Computer Architecture_ (1985).

4. **Peyton Jones, Simon L.** _The Implementation of Functional Programming Languages._ Prentice Hall (1987).

5. **Appel, Andrew W.** _Compiling with Continuations._ Cambridge University Press (1992).

6. **Flanagan, Cormac, et al.** "The Essence of Compiling with Continuations." _PLDI_ (1993).

7. **Appel, Andrew W.** _Modern Compiler Implementation in ML._ Cambridge University Press (1998).

8. **Pierce, Benjamin C.** _Types and Programming Languages._ MIT Press (2002).

9. **Maranget, Luc.** "Compiling Pattern Matching to Good Decision Trees." _ML Workshop_ (2008).

---

## Appendix A: Implementation Order

Recommended order for self-hosting:

1. **Core types** (`Core.Expr`, `Core.Pattern`, `Core.Type`)
2. **Lexer** (with trivia support)
3. **Parser** (Surface AST output)
4. **Desugaring** (Surface → Core)
5. **Name resolution** (Core → Core with unique Names)
6. **Type inference** (Algorithm W)
7. **Pattern compilation** (decision trees)
8. **IR lowering** (Core → ANF)
9. **Optimization** (ANF → ANF)
10. **Code generation** (ANF → JavaScript)

Each phase should be a pure function with clear input/output types.

---

## Appendix B: File Structure

```
src/
  lexer.alg          -- Lexer
  parser.alg         -- Parser (Pratt)
  surface.alg        -- Surface AST types
  core.alg           -- Core AST types
  desugar.alg        -- Surface -> Core
  resolve.alg        -- Name resolution
  types.alg          -- Type definitions
  infer.alg          -- Type inference (Algorithm W)
  patterns.alg       -- Pattern compilation
  ir.alg             -- IR types
  lower.alg          -- Core -> IR
  optimize.alg       -- IR optimizations
  codegen.alg        -- IR -> JavaScript
  main.alg           -- Entry point
```

---

## Appendix C: Code Generation Quality

The generated JavaScript must be **highly optimized** - minimal and fast.
This is not optional. Poor codegen defeats the purpose of compilation.

### C.1 Quality Goals

**JavaScript is a compilation target, not source code.** Optimize for speed, not readability.

1. **No unnecessary indirection**: Direct function calls, not wrappers
2. **Minimal temporaries**: Inline trivial expressions
3. **Zero runtime overhead for known types**: Use type information to eliminate dispatch
4. **Smallest possible output**: Fewer bytes = faster parse/load time
5. **Prefer speed over readability**: The output is never meant to be read by humans

### C.2 Anti-Patterns to Avoid

**BAD: Using $apply for known functions**

```javascript
// WRONG - we know double is a function
const _t348 = $apply(double, 5);

// CORRECT
const _t348 = double(5);
```

**BAD: Excessive temporaries**

```javascript
// WRONG
const _t502 = sumList(rest);
const _t503 = x + _t502;
return _t503;

// CORRECT
return x + sumList(rest);
```

**BAD: Verbose constructor application**

```javascript
// WRONG - building [1,2,3]
const _t1 = $apply($con("Cons"), 1);
const _t2 = $apply($con("Cons"), 2);
const _t3 = $apply($con("Cons"), 3);
const _t4 = $apply(_t3, $con("Nil"));
const _t5 = $apply(_t2, _t4);
const list = $apply(_t1, _t5);

// CORRECT - direct linked list construction
const list = { h: 1, t: { h: 2, t: { h: 3, t: null } } };
```

**BAD: IIFE for match expressions**

```javascript
// WRONG
const result = ((_s) => {
  if (_s[0] === 0) return def;
  else {
    const x = _s[1];
    return x;
  }
})(m);

// CORRECT (inline when possible)
const result = m[0] === 0 ? def : m[1];

// OR with switch for many branches
let result;
switch (m[0]) {
  case 0:
    result = def;
    break; // Nothing
  case 1:
    result = m[1];
    break; // Just
}
```

### C.3 Type-Directed Optimizations

The type checker provides information that enables better codegen:

| Type Information        | Optimization              |
| ----------------------- | ------------------------- |
| Function type           | Direct call, no `$apply`  |
| Known constructor arity | Inline construction       |
| Primitive types         | Use JS operators directly |
| Monomorphic call        | No dictionary passing     |

### C.4 Inlining Heuristics for Codegen

Inline an expression if:

- It's a literal
- It's a variable used exactly once
- It's a simple field access
- It's a primitive operation

Do NOT inline if:

- Expression has side effects (foreign calls)
- Expression is large and used multiple times
- Inlining would duplicate computation

### C.5 Target Output Quality

**Input (Algow):**

```
let sumList = xs ->
  match xs
    when Nil -> 0
    when Cons x rest -> x + sumList rest
  end

let nums = [1, 2, 3, 4, 5]
let total = sumList nums
```

**Expected Output (JavaScript):**

```javascript
const sumList = (xs) => {
  if (xs === null) return 0;
  return xs.h + sumList(xs.t);
};

const nums = { h: 1, t: { h: 2, t: { h: 3, t: { h: 4, t: { h: 5, t: null } } } } };
const total = sumList(nums);
```

**NOT Acceptable:**

```javascript
sumList = (xs) => {
  const _t504 = ((_s) => {
    if (_s === null) {
      return 0;
    } else {
      const x = _s.h;
      const rest = _s.t;
      const _t502 = $apply(sumList, rest);
      const _t503 = x + _t502;
      return _t503;
    }
  })(xs);
  return _t504;
};
```

### C.6 Metrics

Code generation quality should be measured by:

1. **Runtime performance**: Benchmark execution time (PRIMARY METRIC)
2. **Output size**: Bytes of generated JS (smaller = faster parse/load)
3. **Startup time**: How fast does the generated code load?
4. **Memory usage**: Heap allocation during execution

Readability is **not** a metric. JavaScript is a compilation target.

The self-hosted compiler should generate JS that runs **as fast as possible**.
Target: within 1.5x of hand-optimized JavaScript for equivalent algorithms.
