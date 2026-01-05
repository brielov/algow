# Chapter 4: Pratt Parsing

Pratt parsing is an elegant technique for parsing expressions with operator precedence. Also known as "top-down operator precedence parsing," it was invented by Vaughan Pratt in 1973 and remains one of the cleanest ways to handle infix operators.

This chapter explains the algorithm in detail and walks through our implementation.

---

## The Problem with Recursive Descent

A simple recursive descent parser handles precedence by having one function per precedence level:

```typescript
// Lowest precedence: addition
const parseAdditive = (): Expr => {
  let left = parseMultiplicative();
  while (currentToken === "+" || currentToken === "-") {
    const op = currentToken;
    advance();
    const right = parseMultiplicative();
    left = { kind: "BinOp", op, left, right };
  }
  return left;
};

// Higher precedence: multiplication
const parseMultiplicative = (): Expr => {
  let left = parsePrimary();
  while (currentToken === "*" || currentToken === "/") {
    const op = currentToken;
    advance();
    const right = parsePrimary();
    left = { kind: "BinOp", op, left, right };
  }
  return left;
};

// Highest precedence: literals and parentheses
const parsePrimary = (): Expr => {
  if (currentToken === "(") {
    advance();
    const expr = parseAdditive();
    expect(")");
    return expr;
  }
  return parseNumber();
};
```

This works, but it's verbose. Each precedence level needs its own function. With 10 precedence levels, you have 10 nearly identical functions.

Pratt parsing solves this elegantly with a single function that takes precedence as a parameter.

---

## The Key Insight: Binding Power

Instead of separate functions per precedence level, we assign each operator a **binding power** (also called precedence). Higher binding power means tighter binding.

```typescript
const enum Bp {
  None = 0,
  Pipe = 10,          // |>
  Or = 12,            // ||
  And = 14,           // &&
  Cons = 15,          // ::
  Equality = 20,      // == !=
  Comparison = 30,    // < <= > >=
  Additive = 40,      // + - ++
  Multiplicative = 50, // * /
  Application = 60,    // function application
  FieldAccess = 70,    // .
}
```

When we see `1 + 2 * 3`:
- `+` has binding power 40
- `*` has binding power 50
- Since 50 > 40, `*` binds tighter: `1 + (2 * 3)`

---

## The Algorithm

The Pratt parsing algorithm has two key functions:

### 1. parsePrecedence

This is the heart of the algorithm:

```typescript
/**
 * Parse an expression with minimum binding power.
 *
 * The key insight: we keep parsing infix operators as long as their
 * binding power is greater than our minimum. When we see a weaker
 * operator, we return and let our caller handle it.
 */
const parsePrecedence = (state: ParserState, minBp: number): ast.Expr => {
  // First, parse a prefix expression (literal, variable, unary op, etc.)
  let left = parsePrefix(state);

  // Then, as long as the next operator binds tighter than minBp,
  // parse it as an infix operator on our left expression
  while (true) {
    const bp = infixBindingPower(state);
    if (bp <= minBp) break;  // Operator too weak, return to caller
    left = parseInfix(state, left, bp);
  }

  return left;
};

// Entry point: parse with minimum binding power of 0
const parseExpr = (state: ParserState): ast.Expr =>
  parsePrecedence(state, Bp.None);
```

### 2. parsePrefix

Handles expressions that start a new expression context:

```typescript
/**
 * Parse a prefix expression—something that can start an expression.
 * Literals, variables, parentheses, unary operators, etc.
 */
const parsePrefix = (state: ParserState): ast.Expr => {
  const token = state.current;
  const kind = token[0];
  const start = token[1];

  switch (kind) {
    // ─────────────────────────────────────────────────────────────
    // Literals: values written directly in source
    // ─────────────────────────────────────────────────────────────

    case TokenKind.Number: {
      advance(state);
      // Parse the text as a floating-point number
      return ast.num(parseFloat(text(state, token)), tokenSpan(token));
    }

    case TokenKind.String: {
      advance(state);
      // Parse the string content (handling escape sequences)
      return ast.str(
        parseStringContent(state, text(state, token), token[1]),
        tokenSpan(token)
      );
    }

    case TokenKind.True:
      advance(state);
      return ast.bool(true, tokenSpan(token));

    case TokenKind.False:
      advance(state);
      return ast.bool(false, tokenSpan(token));

    // ─────────────────────────────────────────────────────────────
    // Identifiers and lambdas
    // ─────────────────────────────────────────────────────────────

    case TokenKind.Lower: {
      advance(state);
      const name = text(state, token);
      const paramSpan = tokenSpan(token);

      // Check for lambda: x => body
      if (at(state, TokenKind.Arrow)) {
        advance(state);
        const body = parseExpr(state);
        const end = body.span?.end ?? state.current[1];
        return ast.abs(name, body, span(start, end), paramSpan);
      }

      // Just a variable
      return ast.var_(name, tokenSpan(token));
    }

    case TokenKind.Upper: {
      // Constructor name (like Just, Cons, Nothing)
      advance(state);
      return ast.var_(text(state, token), tokenSpan(token));
    }

    // ─────────────────────────────────────────────────────────────
    // Grouped expressions and tuples
    // ─────────────────────────────────────────────────────────────

    case TokenKind.LParen:
      return parseParenOrTuple(state);

    // ─────────────────────────────────────────────────────────────
    // Unary negation: -x desugars to 0 - x
    // ─────────────────────────────────────────────────────────────

    case TokenKind.Minus: {
      advance(state);
      // Parse with high precedence so -x * y = (-x) * y
      const operand = parsePrecedence(state, Bp.Multiplicative + 1);
      const end = operand.span?.end ?? state.current[1];
      return ast.binOp("-", ast.num(0, span(start, start)), operand, span(start, end));
    }

    // ─────────────────────────────────────────────────────────────
    // Compound expressions
    // ─────────────────────────────────────────────────────────────

    case TokenKind.LBrace:
      return parseRecord(state);

    case TokenKind.LBracket:
      return parseListLiteral(state);

    case TokenKind.If:
      return parseIf(state);

    case TokenKind.Match:
      return parseMatch(state);

    case TokenKind.Let:
      return parseLetExpr(state);

    // ─────────────────────────────────────────────────────────────
    // Error: unexpected token
    // ─────────────────────────────────────────────────────────────

    default: {
      error(state, `unexpected token: ${TokenKind[kind]}`);
      advance(state);
      return ast.num(0);  // Return placeholder to continue parsing
    }
  }
};
```

### 3. parseInfix

Handles operators between expressions:

```typescript
/**
 * Parse an infix expression. We already have the left operand;
 * now we need to handle the operator and right operand.
 */
const parseInfix = (state: ParserState, left: ast.Expr, bp: number): ast.Expr => {
  const kind = state.current[0];
  const start = left.span?.start ?? 0;

  // Helper to parse a standard binary operator
  const binOp = (op: ast.Op): ast.Expr => {
    advance(state);  // Consume the operator
    const right = parsePrecedence(state, bp);  // Parse right operand
    const end = right.span?.end ?? state.current[1];
    return ast.binOp(op, left, right, span(start, end));
  };

  switch (kind) {
    // ─────────────────────────────────────────────────────────────
    // Arithmetic operators
    // ─────────────────────────────────────────────────────────────

    case TokenKind.Plus:
      return binOp("+");

    case TokenKind.Minus:
      return binOp("-");

    case TokenKind.Star:
      return binOp("*");

    case TokenKind.Slash:
      return binOp("/");

    case TokenKind.PlusPlus:
      return binOp("++");  // String concatenation

    // ─────────────────────────────────────────────────────────────
    // Comparison operators
    // ─────────────────────────────────────────────────────────────

    case TokenKind.Lt:
      return binOp("<");

    case TokenKind.Le:
      return binOp("<=");

    case TokenKind.Gt:
      return binOp(">");

    case TokenKind.Ge:
      return binOp(">=");

    case TokenKind.EqEq:
      return binOp("==");

    case TokenKind.Ne:
      return binOp("!=");

    // ─────────────────────────────────────────────────────────────
    // Logical operators (desugar to if expressions)
    // ─────────────────────────────────────────────────────────────

    case TokenKind.And: {
      // a && b  →  if a then b else false
      advance(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return ast.if_(left, right, ast.bool(false), span(start, end));
    }

    case TokenKind.Or: {
      // a || b  →  if a then true else b
      advance(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return ast.if_(left, ast.bool(true), right, span(start, end));
    }

    // ─────────────────────────────────────────────────────────────
    // Pipe operator: x |> f  →  f x
    // ─────────────────────────────────────────────────────────────

    case TokenKind.Pipe: {
      advance(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      // Swap: apply right (the function) to left (the argument)
      return ast.app(right, left, span(start, end));
    }

    // ─────────────────────────────────────────────────────────────
    // Cons operator: x :: xs  →  Cons x xs (right-associative)
    // ─────────────────────────────────────────────────────────────

    case TokenKind.ColonColon: {
      advance(state);
      // Use bp - 1 for right-associativity: a :: b :: c  →  a :: (b :: c)
      const right = parsePrecedence(state, bp - 1);
      const end = right.span?.end ?? state.current[1];
      return ast.app(ast.app(ast.var_("Cons"), left), right, span(start, end));
    }

    // ─────────────────────────────────────────────────────────────
    // Field access: record.field or tuple.0
    // ─────────────────────────────────────────────────────────────

    case TokenKind.Dot: {
      advance(state);
      if (at(state, TokenKind.Number)) {
        // Tuple indexing: tuple.0
        const indexToken = state.current;
        advance(state);
        const index = parseInt(text(state, indexToken), 10);
        const end = indexToken[2];
        return ast.tupleIndex(left, index, span(start, end));
      }
      // Record field access: record.field
      const fieldToken = expect(state, TokenKind.Lower, "expected field name");
      const field = fieldToken ? text(state, fieldToken) : "?";
      const end = fieldToken ? fieldToken[2] : state.current[1];
      return ast.fieldAccess(left, field, span(start, end));
    }

    // ─────────────────────────────────────────────────────────────
    // Function application: f x (no explicit operator)
    // ─────────────────────────────────────────────────────────────

    default: {
      // No operator—this is function application
      const right = parsePrefix(state);
      const end = right.span?.end ?? state.current[1];
      return ast.app(left, right, span(start, end));
    }
  }
};
```

### 4. infixBindingPower

Determines the binding power of potential infix operators:

```typescript
/**
 * Get the binding power of the current token as an infix operator.
 * Returns 0 (None) if the token isn't an infix operator.
 */
const infixBindingPower = (state: ParserState): number => {
  const kind = state.current[0];

  switch (kind) {
    case TokenKind.Pipe:
      return Bp.Pipe;

    case TokenKind.Or:
      return Bp.Or;

    case TokenKind.And:
      return Bp.And;

    case TokenKind.ColonColon:
      return Bp.Cons;

    case TokenKind.EqEq:
    case TokenKind.Ne:
      return Bp.Equality;

    case TokenKind.Lt:
    case TokenKind.Le:
    case TokenKind.Gt:
    case TokenKind.Ge:
      return Bp.Comparison;

    case TokenKind.Plus:
    case TokenKind.Minus:
    case TokenKind.PlusPlus:
      return Bp.Additive;

    case TokenKind.Star:
    case TokenKind.Slash:
      return Bp.Multiplicative;

    case TokenKind.Dot:
      return Bp.FieldAccess;

    // Function application: no operator, but these tokens can start an argument
    case TokenKind.Lower:
    case TokenKind.Upper:
    case TokenKind.Number:
    case TokenKind.String:
    case TokenKind.True:
    case TokenKind.False:
    case TokenKind.LParen:
    case TokenKind.LBrace:
      // Don't treat as application if at start of new line
      if (state.lexer.atLineStart) {
        return Bp.None;
      }
      return Bp.Application;

    default:
      return Bp.None;
  }
};
```

---

## How It Works: A Step-by-Step Example

Let's trace through parsing `1 + 2 * 3`:

### Initial State
```
Tokens: [Num 1] [Plus] [Num 2] [Star] [Num 3] [Eof]
                ^
           current
```

### Step 1: parseExpr calls parsePrecedence(0)

```typescript
parsePrecedence(state, 0)  // minBp = 0
```

### Step 2: parsePrefix parses "1"

```typescript
left = parsePrefix(state)  // Returns Num(1)
```

State after:
```
Tokens: [Num 1] [Plus] [Num 2] [Star] [Num 3] [Eof]
                  ^
             current
```

### Step 3: Check infix binding power

```typescript
bp = infixBindingPower(state)  // Plus → 40
```

Is 40 > 0? Yes! So we continue.

### Step 4: parseInfix handles Plus

```typescript
parseInfix(state, left=Num(1), bp=40)
```

This calls:
```typescript
right = parsePrecedence(state, 40)  // Recursive call with minBp=40
```

### Step 5: Recursive parsePrecedence(40)

This parses the right side with minBp=40.

First, parsePrefix returns Num(2):
```
Tokens: [Num 1] [Plus] [Num 2] [Star] [Num 3] [Eof]
                                 ^
                            current
```

Then, check binding power of Star:
```typescript
bp = infixBindingPower(state)  // Star → 50
```

Is 50 > 40? Yes! So we continue parsing `2 * 3` as a unit.

### Step 6: parseInfix handles Star

```typescript
parseInfix(state, left=Num(2), bp=50)
```

This calls parsePrecedence(50), which:
1. Parses "3" as Num(3)
2. Checks next token (Eof), binding power 0
3. Is 0 > 50? No! Returns Num(3)

### Step 7: Build 2 * 3

```typescript
// Back in the Star case:
right = parsePrecedence(state, 50)  // Returns Num(3)
return BinOp("*", Num(2), Num(3))   // Returns 2 * 3
```

### Step 8: Return to Plus parsing

```typescript
// Back in the first parsePrecedence(40):
right = BinOp("*", Num(2), Num(3))
return BinOp("+", Num(1), BinOp("*", Num(2), Num(3)))
```

### Final Result

```
    BinOp(+)
    /      \
 Num(1)  BinOp(*)
         /     \
      Num(2)  Num(3)
```

Exactly what we wanted: `1 + (2 * 3)`.

---

## Right Associativity

Some operators are right-associative. For `a :: b :: c`, we want `a :: (b :: c)`, not `(a :: b) :: c`.

The trick is simple: use `bp - 1` when parsing the right operand:

```typescript
case TokenKind.ColonColon: {
  advance(state);
  // Use bp - 1 for right-associativity
  const right = parsePrecedence(state, bp - 1);
  // ...
}
```

With `bp - 1`, the recursive call has a lower minimum binding power. So when it sees another `::` with the same binding power, it doesn't return—it keeps parsing right.

**Left-associative** (use `bp`):
```
a + b + c  →  (a + b) + c
```

**Right-associative** (use `bp - 1`):
```
a :: b :: c  →  a :: (b :: c)
```

---

## Desugaring in the Parser

Some syntax is transformed ("desugared") into simpler forms:

### Logical Operators

```typescript
// a && b  →  if a then b else false
case TokenKind.And: {
  advance(state);
  const right = parsePrecedence(state, bp);
  return ast.if_(left, right, ast.bool(false), ...);
}

// a || b  →  if a then true else b
case TokenKind.Or: {
  advance(state);
  const right = parsePrecedence(state, bp);
  return ast.if_(left, ast.bool(true), right, ...);
}
```

This gives us short-circuit evaluation for free—`if` expressions don't evaluate the unused branch.

### Pipe Operator

```typescript
// x |> f  →  f x
case TokenKind.Pipe: {
  advance(state);
  const right = parsePrecedence(state, bp);
  return ast.app(right, left, ...);  // Swap: function is right, arg is left
}
```

The pipe makes left-to-right data flow readable:
```
data |> process |> format |> output
```

Desugars to:
```
output (format (process data))
```

### Cons Operator

```typescript
// x :: xs  →  Cons x xs
case TokenKind.ColonColon: {
  advance(state);
  const right = parsePrecedence(state, bp - 1);  // Right-associative
  return ast.app(ast.app(ast.var_("Cons"), left), right, ...);
}
```

### List Literals

```typescript
// [1, 2, 3]  →  Cons 1 (Cons 2 (Cons 3 Nil))
const parseListLiteral = (state: ParserState): ast.Expr => {
  // ... parse elements ...

  // Build from right to left
  let result: ast.Expr = ast.var_("Nil", ...);
  for (let i = elements.length - 1; i >= 0; i--) {
    const elem = elements[i]!;
    result = ast.app(ast.app(ast.var_("Cons"), elem), result, ...);
  }

  return result;
};
```

---

## Function Application

Function application has no explicit operator. When we see:

```
f x y
```

We parse it as `(f x) y`. The key is in `infixBindingPower`:

```typescript
case TokenKind.Lower:
case TokenKind.Upper:
case TokenKind.Number:
case TokenKind.LParen:
  // These tokens can start an argument
  if (state.lexer.atLineStart) {
    return Bp.None;  // But not at line start (new statement)
  }
  return Bp.Application;  // High precedence (60)
```

When we see an identifier or other expression-starting token, we treat it as function application with high precedence. This makes:

```
f x + g y
```

Parse as `(f x) + (g y)`, not `f (x + g) y`.

The `atLineStart` check prevents treating new lines as continued application:

```
let x = f
let y = g   -- This is a new binding, not f applied to "let"
```

---

## Summary

Pratt parsing handles precedence elegantly:

1. **Binding power** assigns precedence to operators
2. **parsePrecedence(minBp)** parses as long as operators are stronger than minBp
3. **parsePrefix** handles expression starters (literals, identifiers, unary ops)
4. **parseInfix** handles operators between expressions
5. **Right associativity** uses `bp - 1` for the right operand
6. **Desugaring** transforms complex syntax into simpler forms

The result is a clean, extensible parser where adding a new operator is just:
1. Add it to `infixBindingPower` with its precedence
2. Add a case in `parseInfix` to build the AST node

The next chapter examines the AST structure we've been building.
