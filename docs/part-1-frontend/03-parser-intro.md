# Chapter 3: Parser Introduction

The lexer gives us tokens. Now we need to understand what they mean together. That's the parser's job: turning a flat sequence of tokens into a tree structure that represents the program.

---

## What Is Parsing?

Consider this expression:

```
1 + 2 * 3
```

The lexer gives us five tokens:

```
[Number "1"] [Plus] [Number "2"] [Star] [Number "3"]
```

But what does this mean? Is it `(1 + 2) * 3` or `1 + (2 * 3)`?

Mathematics tells us multiplication has higher **precedence** than addition, so it should be `1 + (2 * 3) = 7`. The parser's job is to discover this structure:

```
    BinOp(+)
    /      \
  1       BinOp(*)
          /     \
         2       3
```

This tree structure is called an **Abstract Syntax Tree (AST)**. "Abstract" because it captures the meaning, not the exact characters. The parentheses `()` aren't in the tree—they influenced the structure, but they're not needed once the structure is determined.

---

## Why Trees?

Programs have hierarchical structure. A function contains expressions. An if-expression contains a condition, a then-branch, and an else-branch. Each of those can contain more expressions.

Trees naturally represent this hierarchy:

```
let x = if a > b then a else b
```

Becomes:

```
Let
├── name: "x"
└── value: If
           ├── cond: BinOp(>)
           │         ├── left: Var "a"
           │         └── right: Var "b"
           ├── then: Var "a"
           └── else: Var "b"
```

Each node in the tree has children, which can have their own children. This recursive structure mirrors the recursive nature of the language.

---

## The Challenge: Operator Precedence

Parsing would be straightforward if expressions were always fully parenthesized:

```
(1 + (2 * 3))
```

The parentheses tell us exactly how things group. But nobody wants to write all those parentheses, so languages have **precedence rules**:

- `*` binds tighter than `+`
- `1 + 2 * 3` means `1 + (2 * 3)`, not `(1 + 2) * 3`

And **associativity rules**:

- `1 - 2 - 3` means `(1 - 2) - 3` (left-associative)
- `a :: b :: c` means `a :: (b :: c)` (right-associative for cons)

The parser must handle these implicitly, producing the correct tree without explicit parentheses.

---

## Parsing Approaches

There are several ways to build a parser:

### 1. Parser Generators

Tools like Yacc, Bison, or ANTLR take a grammar description and generate parser code. You write:

```
expr : expr '+' term
     | term
     ;

term : term '*' factor
     | factor
     ;

factor : NUMBER
       | '(' expr ')'
       ;
```

The tool generates code that parses according to this grammar.

**Pros**: Handles complex grammars correctly; well-studied algorithms
**Cons**: External tool required; error messages can be poor; harder to customize

### 2. Recursive Descent

Write functions that correspond to grammar rules. Each function parses one rule:

```typescript
const parseExpr = (): Expr => {
  let left = parseTerm();
  while (currentToken === "+") {
    advance();
    const right = parseTerm();
    left = { kind: "BinOp", op: "+", left, right };
  }
  return left;
};

const parseTerm = (): Expr => {
  let left = parseFactor();
  while (currentToken === "*") {
    advance();
    const right = parseFactor();
    left = { kind: "BinOp", op: "*", left, right };
  }
  return left;
};
```

**Pros**: Simple to understand; easy to customize; good error messages
**Cons**: Precedence requires separate functions for each level; gets verbose

### 3. Pratt Parsing

A variation of recursive descent that handles precedence elegantly. Instead of one function per precedence level, we use one function with a **binding power** parameter:

```typescript
const parseExpr = (minBp: number): Expr => {
  let left = parsePrefix();

  while (true) {
    const bp = getBindingPower(currentToken);
    if (bp <= minBp) break;

    left = parseInfix(left, bp);
  }

  return left;
};
```

**Pros**: Concise; handles precedence naturally; easy to add operators
**Cons**: The algorithm can seem magical at first

Algow uses Pratt parsing. The next chapter explains it in detail.

---

## The Grammar of Algow

Before we parse, we need to know what we're parsing. Here's Algow's grammar informally:

### Expressions

```
Expression:
  | Number                          -- 42, 3.14
  | String                          -- "hello"
  | Boolean                         -- true, false
  | Variable                        -- x, myFunc
  | Constructor                     -- Just, Nothing
  | Lambda                          -- fn x => body
  | Application                     -- f x
  | BinaryOp                        -- a + b
  | If                              -- if c then t else e
  | Let                             -- let x = e in body
  | LetRec                          -- let rec f = e in body
  | Match                           -- match e with patterns end
  | Tuple                           -- (a, b, c)
  | Record                          -- { x = 1, y = 2 }
  | FieldAccess                     -- record.field
  | TupleIndex                      -- tuple.0
```

### Patterns

```
Pattern:
  | Variable                        -- x
  | Wildcard                        -- _
  | Constructor with args           -- Just x, Cons h t
  | Literal                         -- 42, "hello"
  | Tuple                           -- (a, b)
  | Record                          -- { x, y }
  | As-pattern                      -- Cons h t as list
  | Or-pattern                      -- Nothing | Just Nothing
```

### Declarations

```
Declaration:
  | Data                            -- data Maybe a = Nothing | Just a
  | Let binding                     -- let x = expr
  | Let rec binding                 -- let rec f x = expr (and more...)
```

---

## Parse Trees vs Abstract Syntax Trees

A **parse tree** (or concrete syntax tree) includes every token:

```
expr
├── term
│   └── factor
│       └── 1
├── +
└── expr
    ├── term
    │   ├── factor
    │   │   └── 2
    │   ├── *
    │   └── factor
    │       └── 3
    └── (no more terms)
```

An **abstract syntax tree** (AST) simplifies this:

```
BinOp(+)
├── 1
└── BinOp(*)
    ├── 2
    └── 3
```

The AST:
- Omits punctuation that doesn't affect meaning (parentheses, commas)
- Collapses grammar rules into meaningful nodes
- Directly represents the program structure

We build an AST, not a parse tree.

---

## Error Handling

What happens when the input is invalid?

```
let x =
```

This is incomplete—there's no value for `x`. The parser needs to:

1. **Detect** the error (expected expression, found EOF)
2. **Report** it clearly (with position and context)
3. **Recover** if possible (continue to find more errors)

### Error Recovery

Our parser collects diagnostics rather than throwing exceptions:

```typescript
type ParseResult = {
  program: Program;
  diagnostics: Diagnostic[];
};
```

This lets us report multiple errors in one pass. The user doesn't have to fix one error, re-run, find the next error, repeat.

Recovery strategies include:
- **Synchronization**: Skip tokens until we find a known starting point (like the next `let` or `data`)
- **Insertion**: Pretend a missing token was there
- **Substitution**: Treat an unexpected token as something else

---

## The Parser's Interface

Our parser has a simple interface:

```typescript
// Parse a source string
const parse = (source: string): ParseResult;

// The result
type ParseResult = {
  program: Program;      // The AST
  diagnostics: Diagnostic[];  // Errors and warnings
};

// A program is declarations + a final expression
type Program = {
  dataDecls: DataDecl[];     // data type declarations
  topLevelBindings: TopLevelBinding[];  // let bindings
  expr: Expr | null;         // optional final expression
};
```

### Example Usage

```typescript
const source = `
data Maybe a = Nothing | Just a

let map f m =
  match m with
  | Nothing => Nothing
  | Just x => Just (f x)
  end

map (fn x => x + 1) (Just 5)
`;

const { program, diagnostics } = parse(source);

if (diagnostics.length > 0) {
  for (const d of diagnostics) {
    console.error(`Error: ${d.message}`);
  }
} else {
  // Process the AST
  console.log("Parsed successfully!");
}
```

---

## The Parser's State

Like the lexer, the parser maintains state:

```typescript
type ParserState = {
  source: string;          // Original source code
  lexer: LexerState;       // Lexer state
  current: Token;          // Current token
  previous: Token;         // Previous token (for spans)
  diagnostics: Diagnostic[];  // Collected errors
};
```

The parser:
- Reads tokens from the lexer
- Tracks current and previous tokens
- Collects diagnostics as it goes

### Advancing Through Tokens

```typescript
// Move to the next token
const advance = (state: ParserState): void => {
  state.previous = state.current;
  state.current = nextToken(state.lexer);
};

// Check the current token's kind
const check = (state: ParserState, kind: TokenKind): boolean =>
  state.current[0] === kind;

// Consume a token of a specific kind, or report error
const expect = (state: ParserState, kind: TokenKind, message: string): void => {
  if (check(state, kind)) {
    advance(state);
  } else {
    error(state, message);
  }
};
```

---

## Operator Precedence Table

Before diving into Pratt parsing, here's Algow's precedence from lowest to highest:

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1 (lowest) | `\|>` (pipe) | Left |
| 2 | `\|\|` (or) | Left |
| 3 | `&&` (and) | Left |
| 4 | `::` (cons) | Right |
| 5 | `==` `!=` | Left |
| 6 | `<` `<=` `>` `>=` | Left |
| 7 | `+` `-` `++` | Left |
| 8 | `*` `/` | Left |
| 9 | Function application | Left |
| 10 (highest) | `.` (field access) | Left |

Higher precedence means tighter binding:
- `1 + 2 * 3` → `1 + (2 * 3)` because `*` (8) > `+` (7)
- `f x + g y` → `(f x) + (g y)` because application (9) > `+` (7)

---

## Summary

Parsing transforms tokens into an AST:

1. **Tokens** are the input (from the lexer)
2. **The AST** is the output (tree structure)
3. **Precedence** determines how operators group
4. **Associativity** determines direction of grouping
5. **Error handling** collects diagnostics without stopping
6. **Pratt parsing** handles precedence elegantly

The next chapter dives into Pratt parsing—the algorithm we use to handle all these concerns.
