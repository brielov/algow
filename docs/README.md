# Building a Compiler from Scratch

## A Comprehensive Guide to the Algow Compiler

Welcome! This book will teach you how to build a complete programming language implementation from the ground up. By the end, you'll understand every line of code in a real, working compiler that includes:

- A **lexer** that breaks source code into tokens
- A **parser** that builds a tree structure from those tokens
- A **type checker** that catches errors before your code runs
- An **interpreter** that executes programs directly
- A **compiler** that translates programs to JavaScript

No computer science degree required. No academic papers to decipher. Just clear explanations, annotated code, and a genuine desire to understand how programming languages work.

---

## Who This Book Is For

You're a programmer who:

- Can write code in any language (JavaScript, Python, Ruby, etc.)
- Understands basic concepts like variables, functions, and loops
- Is curious about how programming languages actually work
- Wants to learn compiler concepts without wading through academic jargon

You don't need:

- A computer science degree
- Experience with TypeScript (we'll teach you what you need)
- Knowledge of functional programming (we'll cover that too)
- Any prior compiler experience

---

## What You'll Learn

### The Big Picture

When you write code like `let x = 1 + 2`, your computer doesn't understand it directly. Something has to translate that human-readable text into actions the computer can perform. That "something" is a compiler (or interpreter), and it works in stages:

```
Source Code: "let x = 1 + 2"
     |
     v
[Lexer] ──────────> Tokens: LET, X, EQUALS, 1, PLUS, 2
     |
     v
[Parser] ─────────> Tree: Let(x, BinOp(+, 1, 2))
     |
     v
[Type Checker] ───> "This is valid, x has type number"
     |
     v
[Interpreter] ────> Runs it: x = 3
     or
[Compiler] ───────> JavaScript: const x = 1 + 2;
```

Each stage takes the output of the previous stage and transforms it. This book explains each stage in detail, showing you the actual code and walking through how it works.

### Specific Skills

By completing this book, you'll be able to:

1. **Read and understand compiler source code** - The patterns you'll learn apply to any language implementation
2. **Implement a lexer** - Break text into meaningful tokens
3. **Build a Pratt parser** - An elegant technique for handling operator precedence
4. **Design an AST** - Structure your program representation
5. **Implement type inference** - The famous Algorithm W, explained step by step
6. **Write an interpreter** - Execute programs directly from the AST
7. **Generate code** - Transform your AST into JavaScript
8. **Build a language server** - Add IDE features like autocomplete and go-to-definition

---

## The Language: Algow

We're building a compiler for **Algow**, a small but complete functional programming language. Here's what Algow code looks like:

```
-- A simple function
let double x = x * 2

-- Recursive functions
let rec factorial n =
  if n == 0 then 1
  else n * factorial (n - 1)

-- Custom data types
data Maybe a = Nothing | Just a

-- Pattern matching
let safeDivide x y =
  if y == 0 then Nothing
  else Just (x / y)

match safeDivide 10 2 with
| Nothing => "Can't divide by zero"
| Just result => "Result: " + toString result
end
```

Algow features:

- **Type inference** - No type annotations needed; the compiler figures them out
- **First-class functions** - Pass functions as values, return them from functions
- **Pattern matching** - Destructure data elegantly
- **Algebraic data types** - Define your own data structures
- **Immutability** - No mutation, no surprises

---

## How This Book Is Organized

The book is divided into five parts, plus appendices:

### Part 0: Foundations

Before we dive into compiler code, we need to establish some groundwork:

1. **Introduction** - What compilers do and why they matter
2. **TypeScript Primer** - The TypeScript syntax you'll need to read the code
3. **Functional Programming Concepts** - Key ideas that appear throughout

### Part 1: Frontend

The frontend turns source code into a structured representation:

1. **The Lexer** - Breaking text into tokens
2. **Tokens and Spans** - How we represent and locate tokens
3. **Parser Introduction** - What parsing is and why it's tricky
4. **Pratt Parsing** - An elegant parsing technique
5. **Abstract Syntax Tree** - The tree structure representing programs
6. **Name Resolution** - Connecting variable uses to their definitions

### Part 2: Type System

The type checker catches errors and enables advanced features:

1. **Types Introduction** - What types are and why we want them
2. **Type Representation** - How we represent types in code
3. **Substitution** - The key operation in type inference
4. **Unification** - Making two types equal
5. **Algorithm W** - The complete inference algorithm
6. **Let Polymorphism** - Why `let` is special
7. **Pattern Matching Types** - Type checking patterns
8. **Type Classes** - Constraints like `Eq` and `Ord`

### Part 3: Backend

The backend transforms our representation into executable code:

1. **IR Introduction** - Why we need an intermediate form
2. **A-Normal Form** - Our intermediate representation
3. **Lowering** - Converting AST to IR
4. **Optimizations** - Making the code faster
5. **Code Generation** - Producing JavaScript
6. **Runtime** - Helper functions for generated code

### Part 4: Interpreter

An alternative to compilation: running programs directly:

1. **Evaluation** - Walking the tree to produce values
2. **Closures** - How functions capture their environment
3. **Pattern Matching at Runtime** - Matching values against patterns

### Part 5: Putting It Together

How all the pieces connect:

1. **The Pipeline** - The complete flow from source to output
2. **The Prelude** - Built-in functions and types
3. **Diagnostics** - Helpful error messages
4. **Language Server** - IDE integration

### Appendices

Reference material:

- **Language Reference** - Complete syntax documentation
- **Glossary** - Terms and definitions
- **Further Reading** - Where to go next

---

## How to Read This Book

### Sequential Reading

The book is designed to be read in order. Each chapter builds on previous ones. If you skip ahead, you might miss concepts that later chapters assume you know.

### Running the Code

This isn't just theory. The code is real and you can run it:

```bash
# Clone the repository
git clone https://github.com/brielov/algow.git
cd algow

# Install dependencies
bun install

# Run a program
bun run src/index.ts examples/demo.alg

# Run inline expressions
bun run src/index.ts -e "1 + 2"

# See the inferred type
bun run src/index.ts -t -e "fn x => x"

# Compile to JavaScript
bun run src/index.ts -c examples/demo.alg
```

### Following Along

Each chapter that discusses code will:

1. Explain **why** we need that code
2. Show the **complete source** with annotations
3. Walk through **key functions** step by step
4. Highlight **tricky parts** that might confuse you

When you see annotated code, take your time. Read each comment. Make sure you understand before moving on.

### Asking Questions

If something doesn't make sense, that's on us, not you. This material is genuinely complex, and explaining it clearly is hard. If you're confused:

1. Re-read the previous section
2. Look up the term in the glossary
3. Open an issue on the repository

---

## A Note on Complexity

Let's be honest: compilers are complex. There's no way around it.

But complexity isn't the same as difficulty. A jigsaw puzzle with 1,000 pieces is complex, but each individual piece is simple. Compilers are similar. There are many parts, but each part does something understandable.

Our job is to show you one piece at a time, explain how it fits with the others, and build up your understanding gradually. You don't need to be a genius. You just need to be patient and willing to work through the details.

Ready? Let's build a compiler.

---

## Table of Contents

### Part 0: Foundations

- [Chapter 1: Introduction](part-0-foundations/01-introduction.md)
- [Chapter 2: TypeScript Primer](part-0-foundations/02-typescript-primer.md)
- [Chapter 3: Functional Programming Concepts](part-0-foundations/03-functional-concepts.md)

### Part 1: Frontend

- [Chapter 1: The Lexer](part-1-frontend/01-lexer.md)
- [Chapter 2: Tokens and Spans](part-1-frontend/02-tokens-and-spans.md)
- [Chapter 3: Parser Introduction](part-1-frontend/03-parser-intro.md)
- [Chapter 4: Pratt Parsing](part-1-frontend/04-pratt-parsing.md)
- [Chapter 5: Abstract Syntax Tree](part-1-frontend/05-ast.md)
- [Chapter 6: Name Resolution](part-1-frontend/06-binder.md)

### Part 2: Type System

- [Chapter 1: Types Introduction](part-2-type-system/01-types-intro.md)
- [Chapter 2: Type Representation](part-2-type-system/02-type-representation.md)
- [Chapter 3: Substitution](part-2-type-system/03-substitution.md)
- [Chapter 4: Unification](part-2-type-system/04-unification.md)
- [Chapter 5: Algorithm W](part-2-type-system/05-algorithm-w.md)
- [Chapter 6: Let Polymorphism](part-2-type-system/06-let-polymorphism.md)
- [Chapter 7: Pattern Matching Types](part-2-type-system/07-pattern-matching.md)
- [Chapter 8: Type Classes](part-2-type-system/08-type-classes.md)

### Part 3: Backend

- [Chapter 1: IR Introduction](part-3-backend/01-ir-intro.md)
- [Chapter 2: A-Normal Form](part-3-backend/02-anf.md)
- [Chapter 3: Lowering](part-3-backend/03-lowering.md)
- [Chapter 4: Optimizations](part-3-backend/04-optimizations.md)
- [Chapter 5: Code Generation](part-3-backend/05-codegen.md)
- [Chapter 6: Runtime](part-3-backend/06-runtime.md)

### Part 4: Interpreter

- [Chapter 1: Evaluation](part-4-interpreter/01-evaluation.md)
- [Chapter 2: Closures](part-4-interpreter/02-closures.md)
- [Chapter 3: Pattern Matching at Runtime](part-4-interpreter/03-pattern-matching-runtime.md)

### Part 5: Putting It Together

- [Chapter 1: The Pipeline](part-5-putting-it-together/01-pipeline.md)
- [Chapter 2: The Prelude](part-5-putting-it-together/02-prelude.md)
- [Chapter 3: Diagnostics](part-5-putting-it-together/03-diagnostics.md)
- [Chapter 4: Language Server](part-5-putting-it-together/04-lsp.md)

### Appendices

- [Appendix A: Language Reference](appendices/a-language-reference.md)
- [Appendix B: Glossary](appendices/b-glossary.md)
- [Appendix C: Further Reading](appendices/c-further-reading.md)
