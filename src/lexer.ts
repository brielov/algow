/**
 * Lexer for the Algow language.
 *
 * Uses a cursor-based approach where tokens are returned as [kind, start, end]
 * tuples. Minimal string allocation during lexing - only for keyword lookup.
 * Token text can be extracted from the source using start/end positions.
 *
 * Character comparisons use hex codes for clarity:
 *   0x09 = tab, 0x0a = newline, 0x0d = carriage return, 0x20 = space
 *   0x30-0x39 = '0'-'9', 0x41-0x5a = 'A'-'Z', 0x61-0x7a = 'a'-'z'
 */

// =============================================================================
// TOKEN KINDS
// =============================================================================

export enum TokenKind {
  // End of file
  Eof,

  // Literals
  Int,
  Float,
  String,
  Char,

  // Identifiers
  Lower, // lowercase identifier (variable)
  Upper, // uppercase identifier (constructor)

  // Keywords
  Let,
  Rec,
  In,
  If,
  Then,
  Else,
  Match,
  End,
  Type,
  When,
  True,
  False,
  As,
  AndKw, // and (for mutual recursion)
  Module, // module
  Use, // use
  Foreign, // foreign
  Do, // do

  // Operators
  Plus, // +
  Minus, // -
  Star, // *
  Slash, // /
  Lt, // <
  Le, // <=
  Gt, // >
  Ge, // >=
  EqEq, // ==
  Ne, // !=
  Pipe, // |>
  Arrow, // ->
  LeftArrow, // <-
  Eq, // =
  Bar, // |
  Comma, // ,
  Dot, // .
  Underscore, // _
  And, // &&
  Or, // ||

  Colon, // :
  ColonColon, // ::

  // Delimiters
  LParen, // (
  RParen, // )
  LBrace, // {
  RBrace, // }
  LBracket, // [
  RBracket, // ]

  // Error
  Error,
}

// Token tuple: [kind, start, end]
export type Token = readonly [TokenKind, number, number];

// =============================================================================
// LEXER STATE
// =============================================================================

export type LexerState = {
  readonly source: string;
  pos: number;
  atLineStart: boolean;
};

// =============================================================================
// CHARACTER CODES (hex notation)
// =============================================================================

const EOF = -1;

// Whitespace
const TAB = 0x09;
const LF = 0x0a;
const CR = 0x0d;
const SPACE = 0x20;

// Digits
const DIGIT_0 = 0x30;
const DIGIT_9 = 0x39;

// Uppercase letters
const UPPER_A = 0x41;
const UPPER_Z = 0x5a;

// Lowercase letters
const LOWER_A = 0x61;
const LOWER_Z = 0x7a;

// Special characters
const UNDERSCORE = 0x5f; // _
const DOUBLE_QUOTE = 0x22; // "
const SINGLE_QUOTE = 0x27; // '
const BACKSLASH = 0x5c; // \
const PLUS = 0x2b; // +
const MINUS = 0x2d; // -
const STAR = 0x2a; // *
const SLASH = 0x2f; // /
const LT = 0x3c; // <
const GT = 0x3e; // >
const EQ = 0x3d; // =
const BANG = 0x21; // !
const PIPE = 0x7c; // |
const COMMA = 0x2c; // ,
const DOT = 0x2e; // .
const LPAREN = 0x28; // (
const RPAREN = 0x29; // )
const LBRACE = 0x7b; // {
const RBRACE = 0x7d; // }
const LBRACKET = 0x5b; // [
const RBRACKET = 0x5d; // ]
const COLON = 0x3a; // :
const AMPERSAND = 0x26; // &

// =============================================================================
// KEYWORD MAP
// =============================================================================

const keywords: ReadonlyMap<string, TokenKind> = new Map([
  ["let", TokenKind.Let],
  ["rec", TokenKind.Rec],
  ["in", TokenKind.In],
  ["if", TokenKind.If],
  ["then", TokenKind.Then],
  ["else", TokenKind.Else],
  ["match", TokenKind.Match],
  ["end", TokenKind.End],
  ["type", TokenKind.Type],
  ["when", TokenKind.When],
  ["true", TokenKind.True],
  ["false", TokenKind.False],
  ["as", TokenKind.As],
  ["and", TokenKind.AndKw],
  ["module", TokenKind.Module],
  ["use", TokenKind.Use],
  ["foreign", TokenKind.Foreign],
  ["do", TokenKind.Do],
]);

// =============================================================================
// CHARACTER CLASSIFICATION
// =============================================================================

const isDigit = (ch: number): boolean => ch >= DIGIT_0 && ch <= DIGIT_9;

const isLower = (ch: number): boolean => ch >= LOWER_A && ch <= LOWER_Z;

const isUpper = (ch: number): boolean => ch >= UPPER_A && ch <= UPPER_Z;

const isIdentContinue = (ch: number): boolean =>
  isLower(ch) || isUpper(ch) || isDigit(ch) || ch === UNDERSCORE;

// =============================================================================
// LEXER FUNCTIONS
// =============================================================================

/**
 * Create a new lexer state from source code.
 */
export const createLexer = (source: string): LexerState => ({
  source,
  pos: 0,
  atLineStart: true,
});

/**
 * Returns the character code at the current position, or -1 for EOF.
 */
export const peek = (state: LexerState): number =>
  state.pos < state.source.length ? state.source.charCodeAt(state.pos) : EOF;

/**
 * Returns the character code at position + offset, or -1 for EOF.
 */
export const peekAt = (state: LexerState, offset: number): number => {
  const idx = state.pos + offset;
  return idx < state.source.length ? state.source.charCodeAt(idx) : EOF;
};

/**
 * Advances the cursor by one character.
 */
export const advance = (state: LexerState): void => {
  state.pos++;
};

/**
 * Extracts the text for a token from the source.
 */
export const slice = (state: LexerState, start: number, end: number): string =>
  state.source.slice(start, end);

/**
 * Skip whitespace and comments, tracking newlines.
 */
const skipWhitespaceAndComments = (state: LexerState): void => {
  state.atLineStart = false;

  while (true) {
    const ch = peek(state);

    // Whitespace (not newline)
    if (ch === SPACE || ch === TAB) {
      advance(state);
      continue;
    }

    // Newline - track for top-level declaration separation
    if (ch === LF || ch === CR) {
      state.atLineStart = true;
      advance(state);
      continue;
    }

    // Line comment: --
    if (ch === MINUS && peekAt(state, 1) === MINUS) {
      advance(state);
      advance(state);
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

    break;
  }
};

const scanBlockComment = (state: LexerState): void => {
  advance(state); // {
  advance(state); // -
  let depth = 1;

  while (depth > 0 && peek(state) !== EOF) {
    const ch = peek(state);
    if (ch === LBRACE && peekAt(state, 1) === MINUS) {
      advance(state);
      advance(state);
      depth++;
    } else if (ch === MINUS && peekAt(state, 1) === RBRACE) {
      advance(state);
      advance(state);
      depth--;
    } else {
      advance(state);
    }
  }
};

const scanNumber = (state: LexerState, start: number): Token => {
  while (isDigit(peek(state))) {
    advance(state);
  }

  // Check for decimal part - if present, it's a Float
  if (peek(state) === DOT && isDigit(peekAt(state, 1))) {
    advance(state); // .
    while (isDigit(peek(state))) {
      advance(state);
    }
    return [TokenKind.Float, start, state.pos];
  }

  return [TokenKind.Int, start, state.pos];
};

const scanString = (state: LexerState, start: number): Token => {
  advance(state); // opening "

  while (peek(state) !== DOUBLE_QUOTE && peek(state) !== EOF) {
    if (peek(state) === BACKSLASH) {
      advance(state); // skip escape character
    }
    advance(state);
  }

  if (peek(state) === EOF) {
    return [TokenKind.Error, start, state.pos];
  }

  advance(state); // closing "
  return [TokenKind.String, start, state.pos];
};

const scanChar = (state: LexerState, start: number): Token => {
  advance(state); // opening '

  if (peek(state) === EOF) {
    return [TokenKind.Error, start, state.pos];
  }

  // Handle escape sequences
  if (peek(state) === BACKSLASH) {
    advance(state); // backslash
    if (peek(state) === EOF) {
      return [TokenKind.Error, start, state.pos];
    }
    advance(state); // escaped character (n, t, r, ', \, etc.)
  } else {
    advance(state); // regular character
  }

  // Expect closing quote
  if (peek(state) !== SINGLE_QUOTE) {
    return [TokenKind.Error, start, state.pos];
  }

  advance(state); // closing '
  return [TokenKind.Char, start, state.pos];
};

const scanLowerOrKeyword = (state: LexerState, start: number): Token => {
  // Handle standalone underscore as wildcard
  if (peek(state) === UNDERSCORE && !isIdentContinue(peekAt(state, 1))) {
    advance(state);
    return [TokenKind.Underscore, start, state.pos];
  }

  while (isIdentContinue(peek(state))) {
    advance(state);
  }

  // Check if it's a keyword
  const text = state.source.slice(start, state.pos);
  const keyword = keywords.get(text);
  if (keyword !== undefined) {
    return [keyword, start, state.pos];
  }

  return [TokenKind.Lower, start, state.pos];
};

const scanUpper = (state: LexerState, start: number): Token => {
  while (isIdentContinue(peek(state))) {
    advance(state);
  }
  return [TokenKind.Upper, start, state.pos];
};

const scanOperator = (state: LexerState, start: number, ch: number): Token => {
  advance(state);

  switch (ch) {
    case PLUS:
      return [TokenKind.Plus, start, state.pos];

    case MINUS:
      if (peek(state) === GT) {
        advance(state);
        return [TokenKind.Arrow, start, state.pos];
      }
      return [TokenKind.Minus, start, state.pos];

    case STAR:
      return [TokenKind.Star, start, state.pos];

    case SLASH:
      return [TokenKind.Slash, start, state.pos];

    case LT:
      if (peek(state) === MINUS) {
        advance(state);
        return [TokenKind.LeftArrow, start, state.pos];
      }
      if (peek(state) === EQ) {
        advance(state);
        return [TokenKind.Le, start, state.pos];
      }
      return [TokenKind.Lt, start, state.pos];

    case GT:
      if (peek(state) === EQ) {
        advance(state);
        return [TokenKind.Ge, start, state.pos];
      }
      return [TokenKind.Gt, start, state.pos];

    case EQ:
      if (peek(state) === EQ) {
        advance(state);
        return [TokenKind.EqEq, start, state.pos];
      }
      return [TokenKind.Eq, start, state.pos];

    case BANG:
      if (peek(state) === EQ) {
        advance(state);
        return [TokenKind.Ne, start, state.pos];
      }
      return [TokenKind.Error, start, state.pos];

    case PIPE:
      if (peek(state) === GT) {
        advance(state);
        return [TokenKind.Pipe, start, state.pos];
      }
      if (peek(state) === PIPE) {
        advance(state);
        return [TokenKind.Or, start, state.pos];
      }
      return [TokenKind.Bar, start, state.pos];

    case AMPERSAND:
      if (peek(state) === AMPERSAND) {
        advance(state);
        return [TokenKind.And, start, state.pos];
      }
      return [TokenKind.Error, start, state.pos];

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
      if (peek(state) === COLON) {
        advance(state);
        return [TokenKind.ColonColon, start, state.pos];
      }
      return [TokenKind.Colon, start, state.pos];

    default:
      return [TokenKind.Error, start, state.pos];
  }
};

/**
 * Returns the next token as [kind, start, end].
 * No string allocation happens here - text can be extracted via slice().
 */
export const nextToken = (state: LexerState): Token => {
  skipWhitespaceAndComments(state);

  const start = state.pos;
  const ch = peek(state);

  if (ch === EOF) {
    return [TokenKind.Eof, start, start];
  }

  // Number literal
  if (isDigit(ch)) {
    return scanNumber(state, start);
  }

  // String literal
  if (ch === DOUBLE_QUOTE) {
    return scanString(state, start);
  }

  // Character literal
  if (ch === SINGLE_QUOTE) {
    return scanChar(state, start);
  }

  // Identifier or keyword
  if (isLower(ch) || ch === UNDERSCORE) {
    return scanLowerOrKeyword(state, start);
  }

  // Constructor (uppercase identifier)
  if (isUpper(ch)) {
    return scanUpper(state, start);
  }

  // Operators and punctuation
  return scanOperator(state, start, ch);
};
