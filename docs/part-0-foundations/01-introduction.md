# Chapter 1: Introduction

## What Is a Compiler?

When you write code, you're writing for two audiences: humans and computers. The code needs to be readable by other programmers (and by future you), but it also needs to do something when you run it.

Here's the problem: computers don't understand programming languages. They understand machine code—sequences of numbers that tell the processor exactly what to do. Nobody wants to write machine code directly. It's tedious, error-prone, and nearly impossible to read.

That's where compilers come in.

A **compiler** is a program that translates code from one language to another. Usually, it translates from a high-level language (like Python, JavaScript, or Algow) to a lower-level language (like machine code or JavaScript).

```
Your Code ──> [Compiler] ──> Machine Code (or JavaScript, or bytecode, etc.)
```

An **interpreter** is similar, but instead of producing translated code, it executes your program directly. It reads your code and performs the actions immediately.

```
Your Code ──> [Interpreter] ──> Results
```

Many language implementations use both techniques. Python compiles your code to bytecode, then interprets that bytecode. Java compiles to bytecode, then uses a Just-In-Time (JIT) compiler to produce machine code. Algow offers both: an interpreter for quick execution and a compiler that produces JavaScript.

---

## Why Build a Compiler?

You might wonder: with so many programming languages already existing, why would anyone build a new one? And why should you learn how?

### Practical Reasons

1. **Domain-Specific Languages**: Many systems benefit from custom languages. Build tools have configuration languages (Makefiles, package.json). Games have scripting languages. Databases have query languages. Knowing how to build a language means you can create the perfect tool for your problem.

2. **Understanding Your Tools**: When you know how compilers work, you understand why programming languages behave the way they do. Why does JavaScript have weird type coercion? Why can't you use a variable before it's declared in some languages? Why do some error messages make no sense? Understanding compilers gives you answers.

3. **Better Debugging**: Compiler errors become less mysterious when you know what the compiler is trying to do. You'll read error messages differently, and you'll write code that's easier for the compiler to analyze.

### Educational Reasons

1. **Data Structures and Algorithms**: Compilers use trees, graphs, hash tables, and sophisticated algorithms. Building one is an excellent way to practice these skills in a real context.

2. **Software Architecture**: A compiler is a multi-stage pipeline where each stage transforms data for the next. This pattern appears everywhere in software: web servers, data processing systems, game engines.

3. **Type Systems**: Understanding type inference helps you use TypeScript, Rust, Haskell, and other typed languages more effectively. You'll know why the compiler makes certain decisions and how to work with it instead of fighting it.

### Personal Reasons

Building a compiler is satisfying. You start with text—just characters in a file—and end up with something that runs. You've created a language. Programs that didn't exist before can now be written and executed. That's powerful.

---

## The Compilation Pipeline

A compiler isn't one monolithic program. It's a sequence of stages, each performing a specific transformation. Here's the pipeline we'll build:

```
Source Code
    │
    ▼
┌─────────┐
│  Lexer  │  Turns characters into tokens
└────┬────┘
     │
     ▼
┌─────────┐
│ Parser  │  Turns tokens into a tree (AST)
└────┬────┘
     │
     ▼
┌─────────┐
│ Binder  │  Resolves variable names
└────┬────┘
     │
     ▼
┌─────────┐
│ Checker │  Infers types, catches errors
└────┬────┘
     │
     ├──────────────┬─────────────────┐
     ▼              ▼                 ▼
┌──────────┐  ┌───────────┐    ┌───────────┐
│Interpreter│  │ IR/Lower  │    │ Type Only │
└──────────┘  └─────┬─────┘    └───────────┘
     │              │                 │
     ▼              ▼                 ▼
  Result       ┌─────────┐        Inferred
               │Optimizer│          Type
               └────┬────┘
                    │
                    ▼
               ┌─────────┐
               │ Backend │
               └────┬────┘
                    │
                    ▼
               JavaScript
```

Let's walk through each stage:

### 1. Lexer (Tokenization)

The lexer reads raw text and breaks it into **tokens**. Tokens are the smallest meaningful units: keywords, numbers, operators, identifiers.

```
Input:  "let x = 1 + 2"

Output: [LET] [IDENT "x"] [EQUALS] [NUMBER 1] [PLUS] [NUMBER 2]
```

The lexer handles details like:
- Skipping whitespace and comments
- Recognizing multi-character tokens (`==`, `<=`, `|>`)
- Distinguishing keywords (`let`, `if`) from identifiers (`foo`, `myVar`)

### 2. Parser

The parser reads tokens and builds an **Abstract Syntax Tree (AST)**. The AST represents the structure of your program as a tree, where each node is an expression or statement.

```
Input:  [LET] [IDENT "x"] [EQUALS] [NUMBER 1] [PLUS] [NUMBER 2]

Output:
        Let
       /   \
     "x"   BinOp(+)
           /     \
        Num(1)  Num(2)
```

The parser handles:
- Operator precedence (`1 + 2 * 3` means `1 + (2 * 3)`)
- Nested expressions (`(1 + 2) * 3`)
- Complex syntax (`if`/`then`/`else`, `match`/`with`, function definitions)

### 3. Binder (Name Resolution)

The binder connects variable **uses** to their **definitions**. When you write `x + 1`, the binder figures out which `x` you mean—is it a function parameter? A let binding? A built-in?

```
let x = 1 in
let y = 2 in
x + y
^   ^
└───┴── The binder tracks which definition each variable refers to
```

This stage builds a symbol table that later stages use.

### 4. Checker (Type Inference)

The checker analyzes your program and figures out the type of every expression. In Algow, you don't need to write type annotations—the checker infers them automatically.

```
let add x y = x + y

Checker infers: add : number -> number -> number
```

If there's a type error (like adding a number to a string), the checker reports it. This catches bugs before your code runs.

### 5. Interpreter (Direct Execution)

If you want to run your program immediately, the interpreter walks the AST and executes each node. It maintains an environment (a mapping from variable names to values) and produces results.

```
let x = 1 + 2 in x * 3

Interpreter:
  1. Evaluate 1 + 2 → 3
  2. Bind x = 3
  3. Evaluate x * 3 → 9
  4. Result: 9
```

### 6. Lowering (AST to IR)

If you want to compile instead of interpret, the next step is lowering. This transforms the AST into an **Intermediate Representation (IR)**. Our IR uses **A-Normal Form (ANF)**, where every intermediate value has a name.

```
AST:  f (g x)

IR:   let t0 = g x
      in let t1 = f t0
      in t1
```

This makes code generation simpler because the evaluation order is explicit.

### 7. Optimizer

The optimizer transforms the IR to make it faster or smaller. We implement:
- **Constant folding**: `1 + 2` becomes `3` at compile time
- **Dead code elimination**: Remove unused bindings
- **Tail call optimization**: Mark tail-recursive calls for efficient execution

### 8. Backend (Code Generation)

Finally, the backend generates JavaScript from the optimized IR. It translates each IR node into equivalent JavaScript code.

```
IR:   let t0 = 1 + 2
      in let t1 = t0 * 3
      in t1

JS:   const t0 = 1 + 2;
      const t1 = t0 * 3;
      t1;
```

The generated JavaScript can run anywhere JavaScript runs—browsers, Node.js, Bun, Deno.

---

## Our Language: Algow

Algow is a small functional programming language with type inference. It's inspired by ML, Haskell, and OCaml, but designed to be simple enough to implement in a tutorial while still being powerful enough to be interesting.

### Features

**Type Inference**: The compiler figures out types without annotations.

```
-- No type annotation needed; compiler infers: number -> number
let double x = x * 2
```

**Functions**: Functions are values. They can be passed as arguments, returned from other functions, and stored in data structures.

```
let apply f x = f x

let addOne = fn x => x + 1

apply addOne 5  -- Result: 6
```

**Pattern Matching**: Destructure data elegantly.

```
match list with
| Nil => 0
| Cons x rest => 1 + length rest
end
```

**Algebraic Data Types**: Define your own types.

```
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
data List a = Nil | Cons a (List a)
```

**Recursion**: Define recursive functions with `let rec`.

```
let rec length list =
  match list with
  | Nil => 0
  | Cons _ rest => 1 + length rest
  end
```

### A Complete Example

Here's a program that filters a list to keep only even numbers:

```
-- Define a filter function
let rec filter pred list =
  match list with
  | Nil => Nil
  | Cons x rest =>
    if pred x then Cons x (filter pred rest)
    else filter pred rest
  end

-- Check if a number is even
let isEven n = n - (n / 2) * 2 == 0

-- Create a list
let numbers = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

-- Filter it
filter isEven numbers
-- Result: 2 :: 4 :: Nil (represented as Cons 2 (Cons 4 Nil))
```

---

## How This Book Works

Each chapter focuses on one piece of the compiler. We'll:

1. **Explain the concept** in plain language
2. **Show the complete code** with detailed annotations
3. **Walk through examples** step by step
4. **Highlight tricky parts** that might cause confusion

The code is real. It's the actual implementation of Algow, not pseudocode or simplified versions. When we show a function, it's the function from the repository.

### Annotations

Code examples include extensive comments explaining what each part does:

```typescript
// This is a real function from the compiler
const next = (state: LexerState): Token => {
  // Skip whitespace and comments before the actual token
  // This loop continues until we find something meaningful
  while (true) {
    // Check if we've reached the end of input
    if (state.pos >= state.source.length) {
      // Return an End-of-File token
      return [TokenKind.Eof, state.pos, state.pos];
    }
    // ... more code with more explanations
  }
};
```

### Prerequisites

This book assumes you can program. You don't need to know TypeScript specifically—we cover the TypeScript features we use. You don't need to know functional programming—we cover those concepts too.

If you can write functions, use variables, understand loops and conditionals, and work with basic data structures (arrays, objects/dictionaries), you have enough background.

---

## What's Next

In the next chapter, we'll cover the TypeScript features you need to read the compiler code. Even if you're familiar with TypeScript, skim it—we use some specific patterns (like discriminated unions) that are central to the implementation.

After that, we'll cover functional programming concepts that appear throughout the codebase. Then we'll dive into the compiler itself, starting with the lexer.

Let's begin.
