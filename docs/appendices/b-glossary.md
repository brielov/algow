# Appendix B: Glossary

Terms and definitions used throughout this book, with references to relevant chapters.

---

## A

**Abstract Syntax Tree (AST)**
A tree representation of source code structure, with nodes for each language construct (expressions, patterns, declarations). Unlike a parse tree, an AST abstracts away syntactic details like parentheses. See Part 1, Chapter 5.

**Algorithm W**
The standard algorithm for Hindley-Milner type inference, developed by Robin Milner. It combines constraint generation with unification to infer types. See Part 2, Chapter 5.

**A-Normal Form (ANF)**
An intermediate representation where all arguments to operations must be atomic (variables or literals). Complex expressions are bound to temporary variables. See Part 3, Chapter 2.

**Application**
Applying a function to an argument: `f x`. In a curried language, multiple arguments are applied one at a time: `f x y` is `(f x) y`.

**Arity**
The number of arguments a function or constructor takes. `Just` has arity 1; `Cons` has arity 2; `Nothing` has arity 0.

---

## B

**Binder / Binding**
The process of connecting variable uses to their definitions (name resolution). Also, a let-binding introduces a new variable. See Part 1, Chapter 6.

**Binding Power**
In Pratt parsing, a number representing operator precedence. Higher binding power means tighter binding. See Part 1, Chapter 4.

**Boolean**
A type with two values: `true` and `false`.

---

## C

**Closure**
A function value that captures its lexical environment. When a function is created, it "closes over" the variables in scope. See Part 4, Chapter 2.

**Codegen / Code Generation**
The final compiler phase that produces executable code (JavaScript in our case) from the IR. See Part 3, Chapter 5.

**Compiler**
A program that translates source code to another form (machine code, JavaScript, etc.). Compare with interpreter.

**Cons**
The list constructor that adds an element to the front: `Cons 1 Nil` creates a single-element list. The `::` operator desugars to `Cons`.

**Constant Folding**
An optimization that evaluates constant expressions at compile time: `1 + 2` becomes `3`. See Part 3, Chapter 4.

**Constructor**
A function that creates values of an algebraic data type. `Just`, `Nothing`, `Cons`, `Nil` are constructors.

**Constraint**
In type inference, a requirement that two types must unify, or that a type must belong to a type class. See Part 2, Chapter 8.

**Currying**
A technique where a function of multiple arguments is represented as nested single-argument functions. `add x y` is really `(add x) y`.

---

## D

**Data Declaration**
A statement that defines a new algebraic data type with its constructors: `data Maybe a = Nothing | Just a`.

**Dead Code Elimination (DCE)**
An optimization that removes code that has no effect on the program's output. See Part 3, Chapter 4.

**Desugaring**
Transforming syntactic sugar into more primitive forms. `x |> f` desugars to `f x`. See Part 1, Chapter 4.

**Diagnostic**
An error, warning, or informational message about the source code. See Part 5, Chapter 3.

**Discriminated Union**
A TypeScript pattern where a union of object types shares a common field (the discriminator) that distinguishes them. Used for AST nodes.

---

## E

**Environment**
A mapping from variable names to their values (interpreter) or types (type checker). Extended when entering scopes.

**Exhaustiveness Checking**
Verifying that pattern matching covers all possible cases. See Part 2, Chapter 7.

**Expression**
A piece of code that evaluates to a value: literals, variables, function applications, let expressions, etc.

---

## F

**Free Type Variable**
A type variable that isn't bound by any outer quantifier. In `a -> b`, both `a` and `b` are free.

**Free Variable**
A variable used in an expression but not defined within it. In `fn x => x + y`, `y` is free.

**Frontend**
The compiler phases that analyze source code: lexing, parsing, binding, type checking. Compare with backend.

---

## G

**Generalization**
Converting a type to a type scheme by quantifying over free type variables. `a -> a` becomes `∀a. a -> a`. See Part 2, Chapter 6.

**Guard**
An additional condition on a pattern: `| x if x > 0 => ...`. The pattern only matches if the guard is true.

---

## H

**Higher-Order Function**
A function that takes or returns other functions: `map`, `filter`, `compose`.

**Hindley-Milner (HM)**
A type system with parametric polymorphism and complete type inference, developed independently by Hindley and Milner. See Part 2.

---

## I

**Immutability**
The property of data that cannot be changed after creation. All Algow values are immutable.

**Infix**
An operator that appears between its operands: `1 + 2`. Compare with prefix.

**Instantiation**
Replacing quantified type variables with fresh type variables when using a polymorphic value. See Part 2, Chapter 6.

**Intermediate Representation (IR)**
A program representation between the source AST and target code. Easier to optimize and generate code from. See Part 3, Chapter 1.

**Interpreter**
A program that executes source code directly by walking the AST. Compare with compiler. See Part 4.

---

## J

**JSON-RPC**
A remote procedure call protocol using JSON. The transport protocol for LSP messages.

---

## L

**Lambda / Lambda Expression**
An anonymous function: `fn x => x + 1`. Also called a lambda abstraction or closure.

**Let Polymorphism**
The ability for let-bound values to be polymorphic, enabling code like `let id = fn x => x in (id 1, id "hello")`. See Part 2, Chapter 6.

**Levenshtein Distance**
A measure of the difference between two strings, counting insertions, deletions, and substitutions. Used for "did you mean?" suggestions. See Part 5, Chapter 3.

**Lexer / Lexical Analysis**
The compiler phase that converts source text to tokens. See Part 1, Chapter 1.

**Lexical Scoping**
Scoping where variable references are resolved based on the program text structure, not runtime call stack. See Part 4, Chapter 2.

**Lowering**
Transforming from a higher-level representation (AST) to a lower-level one (IR). See Part 3, Chapter 3.

**LSP (Language Server Protocol)**
A protocol for communication between editors and language tools, providing features like hover, go-to-definition, and autocomplete. See Part 5, Chapter 4.

---

## M

**Monomorphic**
Having a single concrete type, not polymorphic. Lambda parameters are monomorphic.

---

## N

**Name Resolution**
See Binder/Binding.

**Nil**
The empty list constructor: `Nil : List a`.

---

## O

**Occurs Check**
During unification, checking that a type variable doesn't occur in the type it's being unified with. Prevents infinite types like `t = t -> t`. See Part 2, Chapter 4.

---

## P

**Parametric Polymorphism**
Polymorphism where code works uniformly for all types. `id : a -> a` works the same way for numbers, strings, lists, etc.

**Parse Tree**
A tree that directly mirrors the grammar structure, including all syntactic details. Compare with AST.

**Parser**
The compiler phase that converts tokens to an AST. See Part 1, Chapters 3-4.

**Pattern Matching**
Destructuring values and testing their structure: `match xs with | Nil => 0 | Cons x rest => 1 end`. See Part 2, Chapter 7 and Part 4, Chapter 3.

**Pipeline**
The sequence of compiler phases from source to output. See Part 5, Chapter 1.

**Polymorphism**
The ability for code to work with multiple types. See Parametric Polymorphism.

**Pratt Parsing**
A top-down operator precedence parsing technique using binding power. See Part 1, Chapter 4.

**Prefix**
An operator that appears before its operand: `-x`. Compare with infix.

**Prelude**
A standard library of types and functions automatically available in every program. See Part 5, Chapter 2.

**Principal Type**
The most general type that can be assigned to an expression. HM type inference always finds the principal type.

---

## R

**Record**
A compound value with named fields: `{ x = 1, y = 2 }`.

**Recursion**
A function calling itself. Requires `let rec` in Algow.

**Row Polymorphism**
Type system feature allowing polymorphism over record fields. A function accepting `{ x : a | ρ }` works on any record with at least field `x`. See Part 2, Chapter 2.

**Runtime**
The system that executes compiled code, including helper functions like `$apply`. See Part 3, Chapter 6.

---

## S

**Scheme (Type Scheme)**
A type with quantified variables: `∀a. a -> a`. Represents polymorphic types. See Part 2, Chapter 6.

**Scope**
The region of a program where a variable is visible. Variables in inner scopes can shadow outer variables.

**Scrutinee**
The expression being matched in a pattern match: in `match xs with ...`, `xs` is the scrutinee.

**Shadowing**
When an inner binding hides an outer binding of the same name.

**Span**
The source location of an AST node, stored as byte offsets (start, end).

**Substitution**
A mapping from type variables to types. Applied to replace variables with their resolved types. See Part 2, Chapter 3.

**Syntactic Sugar**
Convenient syntax that translates to more primitive forms: `x :: xs` is sugar for `Cons x xs`.

---

## T

**Tail Call Optimization (TCO)**
Optimizing tail-recursive functions to use constant stack space. See Part 3, Chapter 4.

**Token**
A unit of source code: a keyword, identifier, number, operator, etc. See Part 1, Chapters 1-2.

**Transport**
The communication channel for LSP messages (stdio, WebSocket, Web Worker). See Part 5, Chapter 4.

**Tree-Walking Interpreter**
An interpreter that directly traverses the AST to execute the program. See Part 4.

**Tuple**
An ordered collection of fixed size: `(1, "hello")`.

**Type**
A classification of values: `number`, `string`, `a -> b`, `List a`, etc.

**Type Application**
Applying a type constructor to arguments: `List number`, `Maybe string`.

**Type Checker**
The compiler phase that verifies type correctness and infers types. See Part 2.

**Type Class**
A constraint requiring a type to support certain operations: `Eq` for equality, `Ord` for comparison. See Part 2, Chapter 8.

**Type Constructor**
A type that takes parameters: `List`, `Maybe`, `Either`. `List` alone isn't a complete type; `List number` is.

**Type Environment**
A mapping from variable names to type schemes. Extended when entering scopes.

**Type Inference**
Automatically determining types without explicit annotations.

**Type Variable**
A placeholder for an unknown type, written as lowercase letters: `a`, `b`, `t0`.

---

## U

**Unification**
Finding a substitution that makes two types equal. Core algorithm in type inference. See Part 2, Chapter 4.

**Unit**
The type with exactly one value, often written `()`. Algow doesn't have an explicit unit type.

---

## V

**Value**
The result of evaluating an expression: numbers, strings, closures, constructors, etc.

---

## W

**Wildcard**
The pattern `_` that matches anything without binding.

---

## Cross-Reference by Chapter

| Concept | Primary Chapter |
|---------|-----------------|
| Lexer, Tokens | Part 1, Ch 1-2 |
| Parser, Pratt Parsing | Part 1, Ch 3-4 |
| AST | Part 1, Ch 5 |
| Binder, Name Resolution | Part 1, Ch 6 |
| Types, Type Variables | Part 2, Ch 1-2 |
| Substitution | Part 2, Ch 3 |
| Unification | Part 2, Ch 4 |
| Algorithm W | Part 2, Ch 5 |
| Generalization, Let Polymorphism | Part 2, Ch 6 |
| Pattern Matching (types) | Part 2, Ch 7 |
| Type Classes | Part 2, Ch 8 |
| IR, ANF | Part 3, Ch 1-2 |
| Lowering | Part 3, Ch 3 |
| Optimizations, TCO | Part 3, Ch 4 |
| Code Generation | Part 3, Ch 5 |
| Runtime | Part 3, Ch 6 |
| Evaluation | Part 4, Ch 1 |
| Closures | Part 4, Ch 2 |
| Pattern Matching (runtime) | Part 4, Ch 3 |
| Pipeline | Part 5, Ch 1 |
| Prelude | Part 5, Ch 2 |
| Diagnostics | Part 5, Ch 3 |
| LSP | Part 5, Ch 4 |
