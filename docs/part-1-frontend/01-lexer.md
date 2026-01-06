# Chapter 1: The Lexer

The lexer is the first stage of our compiler. Its job is simple: take a string of source code and break it into **tokens**—the smallest meaningful units of the language.

---

## What Is Tokenization?

When you write code like:

```
let x = 1 + 2
```

You see characters arranged into words, numbers, and symbols. The lexer's job is to identify these pieces:

```
"let"  →  Keyword LET
"x"    →  Identifier "x"
"="    →  Operator EQUALS
"1"    →  Number 1
"+"    →  Operator PLUS
"2"    →  Number 2
```

Each piece is a **token**. Tokens are the vocabulary of our language. Just as English sentences are made of words, programs are made of tokens.

### Why Separate Lexing from Parsing?

We could have the parser read characters directly. But separating lexing has advantages:

1. **Simpler parser**: The parser works with tokens, not characters. It doesn't need to know about whitespace, comments, or multi-character operators.

2. **Better error messages**: If we encounter an invalid character, we can report it immediately with a clear message.

3. **Cleaner code**: Each stage has one job. The lexer handles character-level details; the parser handles structure.

---

## Our Lexer Design

Algow's lexer has some distinctive features:

### Zero-Copy Tokens

Most lexers create a new string for each token. Ours doesn't. Instead, tokens are tuples of `[kind, start, end]`:

- `kind`: What type of token (number, keyword, operator)
- `start`: Position in source where it begins
- `end`: Position where it ends

When we need the actual text, we extract it from the source string using `slice(start, end)`. This avoids allocating strings during the hot path of lexing.

### Cursor-Based Scanning

The lexer maintains a single position (cursor) that moves through the source. We peek at characters without advancing, then advance when we've identified what we're looking at.

### Character Codes

Instead of comparing characters like `ch === "a"`, we compare numeric character codes: `ch === 0x61`. This is faster (integer comparison vs. string comparison) and standard practice in high-performance lexers.

---

## The Complete Lexer Source

Let's walk through the entire lexer, section by section.

### Token Kinds

```typescript
/**
 * All possible token types in Algow.
 * This enum assigns a unique number to each kind.
 */
export enum TokenKind {
  // End of file - every source eventually ends
  Eof,

  // Literals - values written directly in source
  Number,   // 42, 3.14
  String,   // "hello"

  // Identifiers - names for things
  Lower,    // lowercase: variables like x, map, myFunc
  Upper,    // uppercase: constructors like Just, Cons, Nothing

  // Keywords - reserved words with special meaning
  Let,      // let
  Rec,      // rec (for recursive functions)
  In,       // in
  If,       // if
  Then,     // then
  Else,     // else
  Match,    // match
  With,     // with
  End,      // end
  Data,     // data (for type declarations)
  True,     // true
  False,    // false
  As,       // as (for as-patterns)
  AndKw,    // and (for mutual recursion, not && operator)

  // Operators - symbols with meaning
  Plus,       // +
  Minus,      // -
  Star,       // *
  Slash,      // /
  Lt,         // <
  Le,         // <=
  Gt,         // >
  Ge,         // >=
  EqEq,       // ==
  Ne,         // !=
  Pipe,       // |>  (pipe operator)
  Arrow,      // =>  (arrow for lambdas and match arms)
  Eq,         // =   (single equals for binding)
  Bar,        // |   (for match patterns)
  Comma,      // ,
  Dot,        // .   (field access)
  Underscore, // _   (wildcard pattern)
  ColonColon, // ::  (cons operator)
  And,        // &&  (logical and)
  Or,         // ||  (logical or)
  Colon,      // :   (type annotations)

  // Delimiters - grouping symbols
  LParen,   // (
  RParen,   // )
  LBrace,   // {
  RBrace,   // }
  LBracket, // [
  RBracket, // ]

  // Error - for invalid input
  Error,
}
```

Each token kind is a unique number. TypeScript enums start at 0 and increment, so `Eof = 0`, `Number = 1`, etc. We don't rely on specific values—just that each kind is distinct.

### Token Representation

```typescript
/**
 * A token is a tuple: [kind, start, end]
 *
 * - kind:  Which type of token (from TokenKind)
 * - start: Byte offset where token begins in source
 * - end:   Byte offset where token ends (exclusive)
 *
 * The 'readonly' makes the tuple immutable.
 */
export type Token = readonly [TokenKind, number, number];
```

This is our zero-copy design. The token doesn't contain the actual text—just enough information to find it in the source.

Example: For source `"let x = 1"`, the token for `let` would be `[TokenKind.Let, 0, 3]`. The text is at positions 0, 1, 2 (the 3 is exclusive).

### Lexer State

```typescript
/**
 * The lexer state tracks where we are in the source.
 *
 * - source:      The complete source code string
 * - pos:         Current cursor position (byte offset)
 * - atLineStart: True if we're at the beginning of a line
 */
export type LexerState = {
  readonly source: string;  // The source never changes
  pos: number;              // This advances as we scan
  atLineStart: boolean;     // Used for detecting top-level declarations
};
```

The state is minimal: just the source and our position in it. The `atLineStart` flag helps the parser detect where new statements begin.

### Character Codes

```typescript
/**
 * Character codes used for comparisons.
 * Using hex notation makes it clear these are character codes, not arbitrary numbers.
 */

const EOF = -1;  // Returned when we're past the end of source

// Whitespace characters
const TAB = 0x09;    // '\t' - horizontal tab
const LF = 0x0a;     // '\n' - newline (line feed)
const CR = 0x0d;     // '\r' - carriage return
const SPACE = 0x20;  // ' '  - space

// Digit range
const DIGIT_0 = 0x30;  // '0'
const DIGIT_9 = 0x39;  // '9'

// Letter ranges
const UPPER_A = 0x41;  // 'A'
const UPPER_Z = 0x5a;  // 'Z'
const LOWER_A = 0x61;  // 'a'
const LOWER_Z = 0x7a;  // 'z'

// Special characters we care about
const UNDERSCORE = 0x5f;    // '_'
const DOUBLE_QUOTE = 0x22;  // '"'
const BACKSLASH = 0x5c;     // '\'
const PLUS = 0x2b;          // '+'
const MINUS = 0x2d;         // '-'
const STAR = 0x2a;          // '*'
const SLASH = 0x2f;         // '/'
const LT = 0x3c;            // '<'
const GT = 0x3e;            // '>'
const EQ = 0x3d;            // '='
const BANG = 0x21;          // '!'
const PIPE = 0x7c;          // '|'
const COMMA = 0x2c;         // ','
const DOT = 0x2e;           // '.'
const LPAREN = 0x28;        // '('
const RPAREN = 0x29;        // ')'
const LBRACE = 0x7b;        // '{'
const RBRACE = 0x7d;        // '}'
const LBRACKET = 0x5b;      // '['
const RBRACKET = 0x5d;      // ']'
const COLON = 0x3a;         // ':'
const AMPERSAND = 0x26;     // '&'
```

Why hex codes? Several reasons:

1. **Performance**: Comparing integers is faster than comparing strings
2. **Clarity**: `0x41` makes it obvious we're dealing with character codes
3. **Convention**: This is standard practice in production lexers

The patterns are easy to remember:

- Digits: 0x30-0x39 (48-57 in decimal)
- Uppercase: 0x41-0x5a (65-90)
- Lowercase: 0x61-0x7a (97-122)

### Keyword Map

```typescript
/**
 * Maps keyword strings to their token kinds.
 * Used to distinguish keywords from regular identifiers.
 */
const keywords: ReadonlyMap<string, TokenKind> = new Map([
  ["let", TokenKind.Let],
  ["rec", TokenKind.Rec],
  ["in", TokenKind.In],
  ["if", TokenKind.If],
  ["then", TokenKind.Then],
  ["else", TokenKind.Else],
  ["match", TokenKind.Match],
  ["with", TokenKind.With],
  ["end", TokenKind.End],
  ["data", TokenKind.Data],
  ["true", TokenKind.True],
  ["false", TokenKind.False],
  ["as", TokenKind.As],
  ["and", TokenKind.AndKw],
]);
```

When we scan what looks like an identifier, we check if it's actually a keyword. This is more flexible than having separate logic for each keyword.

### Character Classification

```typescript
/**
 * Check if a character code is a digit (0-9).
 */
const isDigit = (ch: number): boolean =>
  ch >= DIGIT_0 && ch <= DIGIT_9;

/**
 * Check if a character code is a lowercase letter (a-z).
 */
const isLower = (ch: number): boolean =>
  ch >= LOWER_A && ch <= LOWER_Z;

/**
 * Check if a character code is an uppercase letter (A-Z).
 */
const isUpper = (ch: number): boolean =>
  ch >= UPPER_A && ch <= UPPER_Z;

/**
 * Check if a character can continue an identifier.
 * Identifiers can contain letters, digits, and underscores after the first character.
 */
const isIdentContinue = (ch: number): boolean =>
  isLower(ch) || isUpper(ch) || isDigit(ch) || ch === UNDERSCORE;
```

These helper functions classify characters. Range checks are efficient: we just compare numbers.

### Creating and Operating on Lexer State

```typescript
/**
 * Create a new lexer state from source code.
 * The lexer starts at position 0, at the beginning of a line.
 */
export const createLexer = (source: string): LexerState => ({
  source,
  pos: 0,
  atLineStart: true,
});

/**
 * Returns the character code at the current position, or -1 for EOF.
 * Does not advance the cursor.
 */
export const peek = (state: LexerState): number =>
  state.pos < state.source.length
    ? state.source.charCodeAt(state.pos)
    : EOF;

/**
 * Returns the character code at position + offset, or -1 for EOF.
 * Used for lookahead without advancing.
 */
export const peekAt = (state: LexerState, offset: number): number => {
  const idx = state.pos + offset;
  return idx < state.source.length
    ? state.source.charCodeAt(idx)
    : EOF;
};

/**
 * Advances the cursor by one character.
 */
export const advance = (state: LexerState): void => {
  state.pos++;
};

/**
 * Extracts the text for a token from the source.
 * This is the only place we allocate strings for token text.
 */
export const slice = (state: LexerState, start: number, end: number): string =>
  state.source.slice(start, end);
```

These are the fundamental operations:

- `peek`: Look at current character without moving
- `peekAt`: Look ahead by some offset
- `advance`: Move forward by one
- `slice`: Extract text when needed

The pattern is: peek to identify, then advance and collect.

### Skipping Whitespace and Comments

```typescript
/**
 * Skip whitespace and comments, tracking newlines.
 * This is called before scanning each token.
 */
const skipWhitespaceAndComments = (state: LexerState): void => {
  // Reset the line-start flag (will be set if we see a newline)
  state.atLineStart = false;

  while (true) {
    const ch = peek(state);

    // Horizontal whitespace (not newline)
    if (ch === SPACE || ch === TAB) {
      advance(state);
      continue;
    }

    // Newlines - we track these for the parser
    if (ch === LF || ch === CR) {
      state.atLineStart = true;  // Mark that next token is at line start
      advance(state);
      continue;
    }

    // Line comment: -- to end of line
    if (ch === MINUS && peekAt(state, 1) === MINUS) {
      advance(state);  // skip first -
      advance(state);  // skip second -
      // Skip until newline or EOF
      while (peek(state) !== LF && peek(state) !== EOF) {
        advance(state);
      }
      continue;
    }

    // Block comment: {- ... -}
    if (ch === LBRACE && peekAt(state, 1) === MINUS) {
      scanBlockComment(state);
      continue;
    }

    // Not whitespace or comment, we're done
    break;
  }
};
```

This function runs before each token. It skips over:

- Spaces and tabs
- Newlines (but remembers we crossed one)
- Line comments (`--` to end of line)
- Block comments (`{- ... -}`)

### Block Comments (with Nesting)

```typescript
/**
 * Scan a block comment, handling nesting.
 * Algow supports nested block comments: {- outer {- inner -} outer -}
 */
const scanBlockComment = (state: LexerState): void => {
  advance(state);  // skip {
  advance(state);  // skip -
  let depth = 1;   // Track nesting depth

  while (depth > 0 && peek(state) !== EOF) {
    const ch = peek(state);

    // Start of nested comment
    if (ch === LBRACE && peekAt(state, 1) === MINUS) {
      advance(state);
      advance(state);
      depth++;  // One level deeper
    }
    // End of comment
    else if (ch === MINUS && peekAt(state, 1) === RBRACE) {
      advance(state);
      advance(state);
      depth--;  // One level shallower
    }
    else {
      advance(state);  // Regular character, keep going
    }
  }
};
```

Nested comments are useful for commenting out code that contains comments. The `depth` variable tracks how many levels deep we are.

### Scanning Numbers

```typescript
/**
 * Scan a number literal.
 * Supports integers (42) and decimals (3.14).
 */
const scanNumber = (state: LexerState, start: number): Token => {
  // Consume all digits of the integer part
  while (isDigit(peek(state))) {
    advance(state);
  }

  // Check for decimal part
  // We need lookahead to distinguish 1.2 (decimal) from 1.foo (field access)
  if (peek(state) === DOT && isDigit(peekAt(state, 1))) {
    advance(state);  // consume the .
    // Consume all digits of the decimal part
    while (isDigit(peek(state))) {
      advance(state);
    }
  }

  return [TokenKind.Number, start, state.pos];
};
```

The tricky part is the decimal point. When we see `.`, we need to check if it's followed by a digit. If not, the `.` might be a field access operator, so we leave it for the next token.

### Scanning Strings

```typescript
/**
 * Scan a string literal.
 * Strings are delimited by double quotes and support escape sequences.
 */
const scanString = (state: LexerState, start: number): Token => {
  advance(state);  // skip opening "

  // Scan until closing quote or EOF
  while (peek(state) !== DOUBLE_QUOTE && peek(state) !== EOF) {
    // Handle escape sequences
    if (peek(state) === BACKSLASH) {
      advance(state);  // skip the backslash
      // The next character is the escaped character (skip it too)
    }
    advance(state);
  }

  // Check if we found the closing quote
  if (peek(state) === EOF) {
    // Unterminated string - return error token
    return [TokenKind.Error, start, state.pos];
  }

  advance(state);  // consume closing "
  return [TokenKind.String, start, state.pos];
};
```

The lexer doesn't interpret escape sequences—it just recognizes them. The interpreter or code generator will handle converting `\n` to an actual newline.

### Scanning Identifiers and Keywords

```typescript
/**
 * Scan a lowercase identifier or keyword.
 * Also handles the special case of standalone underscore (_).
 */
const scanLowerOrKeyword = (state: LexerState, start: number): Token => {
  // Special case: standalone underscore is the wildcard token
  if (peek(state) === UNDERSCORE && !isIdentContinue(peekAt(state, 1))) {
    advance(state);
    return [TokenKind.Underscore, start, state.pos];
  }

  // Consume identifier characters
  while (isIdentContinue(peek(state))) {
    advance(state);
  }

  // Check if it's a keyword
  const text = state.source.slice(start, state.pos);
  const keyword = keywords.get(text);
  if (keyword !== undefined) {
    return [keyword, start, state.pos];
  }

  // It's a regular identifier
  return [TokenKind.Lower, start, state.pos];
};

/**
 * Scan an uppercase identifier (constructor name).
 */
const scanUpper = (state: LexerState, start: number): Token => {
  while (isIdentContinue(peek(state))) {
    advance(state);
  }
  return [TokenKind.Upper, start, state.pos];
};
```

The difference between lowercase and uppercase matters:

- Lowercase: Variables, function names, keywords
- Uppercase: Type constructors like `Just`, `Cons`, `Nothing`

### Scanning Operators

```typescript
/**
 * Scan operators and punctuation.
 * Many operators are multiple characters (<=, ==, |>, etc.).
 */
const scanOperator = (state: LexerState, start: number, ch: number): Token => {
  advance(state);  // consume the first character

  switch (ch) {
    case PLUS:
      return [TokenKind.Plus, start, state.pos];  // +

    case MINUS:
      return [TokenKind.Minus, start, state.pos];  // -

    case STAR:
      return [TokenKind.Star, start, state.pos];  // *

    case SLASH:
      return [TokenKind.Slash, start, state.pos];  // /

    case LT:
      // < or <=
      if (peek(state) === EQ) {
        advance(state);
        return [TokenKind.Le, start, state.pos];  // <=
      }
      return [TokenKind.Lt, start, state.pos];  // <

    case GT:
      // > or >=
      if (peek(state) === EQ) {
        advance(state);
        return [TokenKind.Ge, start, state.pos];  // >=
      }
      return [TokenKind.Gt, start, state.pos];  // >

    case EQ:
      // = or == or =>
      if (peek(state) === EQ) {
        advance(state);
        return [TokenKind.EqEq, start, state.pos];  // ==
      }
      if (peek(state) === GT) {
        advance(state);
        return [TokenKind.Arrow, start, state.pos];  // =>
      }
      return [TokenKind.Eq, start, state.pos];  // =

    case BANG:
      // != (! alone is an error)
      if (peek(state) === EQ) {
        advance(state);
        return [TokenKind.Ne, start, state.pos];  // !=
      }
      return [TokenKind.Error, start, state.pos];  // Invalid

    case PIPE:
      // |> or || or |
      if (peek(state) === GT) {
        advance(state);
        return [TokenKind.Pipe, start, state.pos];  // |>
      }
      if (peek(state) === PIPE) {
        advance(state);
        return [TokenKind.Or, start, state.pos];  // ||
      }
      return [TokenKind.Bar, start, state.pos];  // |

    case AMPERSAND:
      // && (& alone is an error)
      if (peek(state) === AMPERSAND) {
        advance(state);
        return [TokenKind.And, start, state.pos];  // &&
      }
      return [TokenKind.Error, start, state.pos];  // Invalid

    case COMMA:
      return [TokenKind.Comma, start, state.pos];

    case DOT:
      return [TokenKind.Dot, start, state.pos];

    case LPAREN:
      return [TokenKind.LParen, start, state.pos];

    case RPAREN:
      return [TokenKind.RParen, start, state.pos];

    case LBRACE:
      return [TokenKind.LBrace, start, state.pos];

    case RBRACE:
      return [TokenKind.RBrace, start, state.pos];

    case LBRACKET:
      return [TokenKind.LBracket, start, state.pos];

    case RBRACKET:
      return [TokenKind.RBracket, start, state.pos];

    case COLON:
      // : or ::
      if (peek(state) === COLON) {
        advance(state);
        return [TokenKind.ColonColon, start, state.pos];  // ::
      }
      return [TokenKind.Colon, start, state.pos];  // :

    default:
      return [TokenKind.Error, start, state.pos];  // Unknown character
  }
};
```

The pattern is: check the first character, then look ahead to see if it's part of a multi-character operator. This is called **maximal munch**—we take the longest match possible.

### The Main Function: Getting the Next Token

```typescript
/**
 * Returns the next token as [kind, start, end].
 * This is the main entry point for the lexer.
 */
export const nextToken = (state: LexerState): Token => {
  // First, skip whitespace and comments
  skipWhitespaceAndComments(state);

  const start = state.pos;
  const ch = peek(state);

  // End of file
  if (ch === EOF) {
    return [TokenKind.Eof, start, start];
  }

  // Number literal (starts with digit)
  if (isDigit(ch)) {
    return scanNumber(state, start);
  }

  // String literal (starts with ")
  if (ch === DOUBLE_QUOTE) {
    return scanString(state, start);
  }

  // Lowercase identifier or keyword (starts with lowercase or _)
  if (isLower(ch) || ch === UNDERSCORE) {
    return scanLowerOrKeyword(state, start);
  }

  // Uppercase identifier/constructor (starts with uppercase)
  if (isUpper(ch)) {
    return scanUpper(state, start);
  }

  // Operators and punctuation
  return scanOperator(state, start, ch);
};
```

This is the function that ties everything together. The logic is:

1. Skip whitespace and comments
2. Check what the current character is
3. Dispatch to the appropriate scanner
4. Return the token

---

## Using the Lexer

Here's how the lexer is used in practice:

```typescript
import { createLexer, nextToken, slice, TokenKind } from "./lexer";

const source = "let x = 1 + 2";
const state = createLexer(source);

// Get all tokens
while (true) {
  const [kind, start, end] = nextToken(state);

  if (kind === TokenKind.Eof) break;

  console.log(`${TokenKind[kind]}: "${slice(state, start, end)}"`);
}

// Output:
// Let: "let"
// Lower: "x"
// Eq: "="
// Number: "1"
// Plus: "+"
// Number: "2"
```

---

## Key Insights

### The Zero-Copy Design

Traditional lexers allocate a string for each token's text. Ours stores only positions. This means:

- Lexing is faster (no memory allocation)
- Tokens are smaller (just three numbers)
- Text extraction is lazy (only done when needed)

The trade-off is that you need to keep the source string around. In practice, this is fine—we always have the source during compilation.

### Character Codes vs Strings

Comparing `ch === 0x61` is faster than `str === "a"` because:

- Integer comparison is a single CPU instruction
- String comparison involves checking lengths and characters

This optimization matters when processing millions of characters.

### Maximal Munch

When we see `=`, we look ahead to check for `==` or `=>`. We always take the longest possible token. This is the standard approach in lexers—it's called **maximal munch** or **longest match**.

### Error Tokens

Instead of throwing an exception on invalid input, we return an `Error` token. This lets the parser continue and potentially report multiple errors. Error recovery is important for a good developer experience.

---

## Summary

The lexer transforms source text into tokens:

1. **Tokens** are `[kind, start, end]` tuples
2. **Scanning** uses peek/advance to move through characters
3. **Character codes** make comparisons fast
4. **Whitespace and comments** are skipped between tokens
5. **Keywords** are distinguished from identifiers via a map

The next chapter explores tokens and spans in more detail, explaining how we track source positions for error messages.
