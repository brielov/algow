# Chapter 2: Tokens and Spans

In the previous chapter, we saw how the lexer breaks source code into tokens. Now let's examine tokens more closely: what they represent, how they encode source positions, and why this matters.

---

## What Is a Token?

A token is the smallest meaningful unit in a programming language. Consider this code:

```
let count = 42 + x
```

The lexer produces these tokens:

| Token Kind | Text    | Position |
| ---------- | ------- | -------- |
| Let        | "let"   | 0-3      |
| Lower      | "count" | 4-9      |
| Eq         | "="     | 10-11    |
| Number     | "42"    | 12-14    |
| Plus       | "+"     | 15-16    |
| Lower      | "x"     | 17-18    |

Each token has three pieces of information:

1. **Kind**: What type of token is it?
2. **Text**: What characters make up the token?
3. **Position**: Where is it in the source?

---

## Token Representation

Algow represents tokens as tuples:

```typescript
type Token = readonly [TokenKind, number, number];
//                     ^          ^       ^
//                     |          |       |
//                     kind       start   end (exclusive)
```

For example, the token `let` at the start of `let x = 1` would be:

```typescript
[TokenKind.Let, 0, 3]
//  kind: Let
//  start: 0  (position of 'l')
//  end: 3    (position after 't')
```

### Why Not Store the Text?

We could have defined tokens as:

```typescript
// Alternative design (NOT what we use)
type Token = {
  kind: TokenKind;
  text: string;
  start: number;
  end: number;
};
```

But this has a cost: every token allocates a new string. For a large file with thousands of tokens, that's thousands of allocations.

Our design is **zero-copy**: the token doesn't contain the text, just positions. When we need the text, we extract it from the source:

```typescript
const text = source.slice(start, end);
```

This is called **lazy evaluation**—we only do the work when we need the result.

---

## Token Kinds Explained

The `TokenKind` enum categorizes every possible token. Let's group them:

### Literals

Literals are values written directly in source code:

```typescript
Number,   // 42, 3.14, 0, 100
String,   // "hello", "world"
True,     // true
False,    // false
```

### Identifiers

Identifiers are names:

```typescript
Lower,    // x, myVar, map, filter (lowercase start)
Upper,    // Just, Nothing, Cons (uppercase start)
```

The distinction matters:

- `Lower` identifiers are variables and function names
- `Upper` identifiers are constructors (like `Just` and `Nothing`)

### Keywords

Keywords are reserved words with special meaning:

```typescript
Let,      // let - variable binding
Rec,      // rec - recursive function
In,       // in - let body
If,       // if - conditional
Then,     // then - if true branch
Else,     // else - if false branch
Match,    // match - pattern matching
With,     // with - patterns start
End,      // end - match block end
Data,     // data - type declaration
As,       // as - as-pattern
AndKw,    // and - mutual recursion
```

Keywords look like identifiers but have special meaning. The lexer distinguishes them by checking against a keyword map.

### Operators

Operators perform operations:

```typescript
// Arithmetic
Plus,       // +
Minus,      // -
Star,       // *
Slash,      // /

// Comparison
Lt,         // <
Le,         // <=
Gt,         // >
Ge,         // >=
EqEq,       // ==
Ne,         // !=

// Logical
And,        // &&
Or,         // ||

// Other
Pipe,       // |>  (pipe operator)
ColonColon, // ::  (cons)
```

### Punctuation and Delimiters

These structure the code:

```typescript
// Assignment and arrows
Eq,         // =  (binding)
Arrow,      // => (lambda and match arms)
Colon,      // :  (type annotation)

// Pattern matching
Bar,        // |  (pattern separator)
Underscore, // _  (wildcard)

// Grouping
LParen,     // (
RParen,     // )
LBrace,     // {
RBrace,     // }
LBracket,   // [
RBracket,   // ]

// Separators
Comma,      // ,
Dot,        // .  (field access)
```

### Special Tokens

```typescript
Eof,        // End of file - marks the end of input
Error,      // Invalid character or sequence
```

---

## Spans and Source Positions

A **span** is a region of source code defined by start and end positions:

```
let x = 42
^-------^
start=0  end=10
```

Spans are crucial for:

1. **Error messages**: Pointing to where the error occurred
2. **IDE features**: Highlighting, go-to-definition, hover
3. **Debugging**: Mapping runtime errors back to source

### Byte Offsets

Our positions are **byte offsets**—the number of bytes from the start of the file. This is simpler than line/column positions and works well with string slicing.

```
"let x = 42"
 ^^^
 0 1 2 (byte offsets)
```

When we need line/column numbers (for error messages or LSP), we convert:

```typescript
// Convert byte offset to line/column (1-indexed)
const offsetToPosition = (source: string, offset: number) => {
  let line = 1;
  let column = 1;

  for (let i = 0; i < offset && i < source.length; i++) {
    if (source.charCodeAt(i) === 0x0a) {  // newline
      line++;
      column = 1;
    } else {
      column++;
    }
  }

  return { line, column };
};
```

### Spans in the AST

AST nodes also have spans, stored as:

```typescript
type Span = {
  start: number;
  end: number;
};
```

For example, a binary operation span covers the entire expression:

```
1 + 2 * 3
^-------^
BinOp span: 0-9
```

Spans can be nested:

```
1 + 2 * 3
    ^---^
    inner BinOp: 4-9
```

---

## The Slice Function

To extract token text from the source, we use `slice`:

```typescript
export const slice = (state: LexerState, start: number, end: number): string =>
  state.source.slice(start, end);
```

JavaScript's `slice` is exclusive on the end, matching our convention:

```typescript
"hello".slice(0, 3)  // "hel" (positions 0, 1, 2)
```

### When Do We Slice?

We slice when we need the actual text:

1. **Keywords**: Check if an identifier is a keyword
2. **Numbers**: Parse the numeric value
3. **Strings**: Get the string content
4. **Error messages**: Show what was unexpected

During parsing, we mostly work with token kinds, not text. This keeps things efficient.

---

## Exclusive End Positions

Our end positions are **exclusive**—they point one past the last character:

```
"let"
 ^^^
 012  (positions)

Token: [Let, 0, 3]
         start=0, end=3
```

Why exclusive?

1. **Consistent with slice**: `source.slice(start, end)` uses exclusive end
2. **Length is easy**: `end - start` gives the length
3. **Empty spans work**: `start === end` means zero length
4. **Adjacency is clear**: Token at `[0, 3]` is followed by token at `[3, ...]`

### Example: Adjacent Tokens

```
let x
^^^|^
0123 5

"let": [0, 3]
"x":   [4, 5]
```

The tokens don't overlap. Position 3 is the space, which was skipped.

---

## Error Positions

When something goes wrong, positions tell us where:

```typescript
const reportError = (
  source: string,
  start: number,
  end: number,
  message: string
): void => {
  const { line, column } = offsetToPosition(source, start);
  const text = source.slice(start, end);

  console.error(`Error at ${line}:${column}: ${message}`);
  console.error(`  Found: "${text}"`);
};
```

Example output:

```
Error at 1:5: Expected '=' after variable name
  Found: "+"
```

Good error messages include:

- **Position**: Line and column
- **Context**: What we found
- **Expectation**: What we wanted

---

## The atLineStart Flag

The lexer tracks whether we're at the start of a line:

```typescript
type LexerState = {
  // ...
  atLineStart: boolean;
};
```

This helps the parser detect top-level declarations. In Algow, new declarations start at the beginning of a line:

```
let x = 1   -- declaration 1
let y = 2   -- declaration 2
```

Without this flag, the parser might think `let y = 2` is part of the first declaration.

---

## Token Classification Utilities

Sometimes we need to classify tokens:

```typescript
// Is this token a literal value?
const isLiteral = (kind: TokenKind): boolean =>
  kind === TokenKind.Number ||
  kind === TokenKind.String ||
  kind === TokenKind.True ||
  kind === TokenKind.False;

// Is this an infix operator?
const isInfixOp = (kind: TokenKind): boolean =>
  kind === TokenKind.Plus ||
  kind === TokenKind.Minus ||
  kind === TokenKind.Star ||
  kind === TokenKind.Slash ||
  kind === TokenKind.Lt ||
  kind === TokenKind.Le ||
  kind === TokenKind.Gt ||
  kind === TokenKind.Ge ||
  kind === TokenKind.EqEq ||
  kind === TokenKind.Ne ||
  kind === TokenKind.And ||
  kind === TokenKind.Or ||
  kind === TokenKind.PlusPlus ||
  kind === TokenKind.Pipe ||
  kind === TokenKind.ColonColon;
```

The parser uses these to make decisions about how to proceed.

---

## Summary

Tokens and spans are foundational:

1. **Tokens** are `[kind, start, end]` tuples
2. **Spans** are `{start, end}` regions in source
3. **Positions** are byte offsets from the start
4. **End positions** are exclusive (one past the last byte)
5. **Text extraction** is lazy (via `slice`)
6. **Error reporting** uses positions for context

The next chapter introduces parsing: transforming this stream of tokens into a tree structure that represents the program.
