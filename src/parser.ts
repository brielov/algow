/**
 * Pratt Parser for the Algow language.
 *
 * Uses top-down operator precedence parsing (Pratt parsing) which elegantly
 * handles operator precedence and associativity. The parser collects diagnostics
 * instead of throwing errors, enabling better error recovery and future LSP support.
 *
 * Key concepts:
 * - Binding power: Determines operator precedence (higher = binds tighter)
 * - Prefix parselets: Handle tokens that start an expression (literals, identifiers, prefix ops)
 * - Infix parselets: Handle tokens that appear between expressions (binary ops, field access)
 */

import * as ast from "./ast";
import type { Diagnostic } from "./diagnostics";
import { createLexer, type LexerState, nextToken, slice, type Token, TokenKind } from "./lexer";

export type { Diagnostic, DiagnosticSeverity } from "./diagnostics";

// =============================================================================
// PARSE RESULT
// =============================================================================

export type ParseResult = {
  readonly program: Program;
  readonly diagnostics: readonly Diagnostic[];
};

export type Program = {
  readonly declarations: readonly ast.DataDecl[];
  readonly bindings: readonly TopLevelBinding[];
  readonly expr: ast.Expr | null;
};

export type TopLevelBinding = {
  readonly name: string;
  readonly nameSpan: ast.Span;
  readonly params: readonly { name: string; span: ast.Span }[];
  readonly body: ast.Expr;
  readonly recursive: boolean;
};

// =============================================================================
// PARSER STATE
// =============================================================================

type ParserState = {
  readonly lexer: LexerState;
  readonly diagnostics: Diagnostic[];
  current: Token;
};

// =============================================================================
// BINDING POWER (PRECEDENCE)
// =============================================================================

const enum Bp {
  None = 0,
  Pipe = 10, // |>
  Cons = 15, // :: (right-associative)
  Equality = 20, // == !=
  Comparison = 30, // < <= > >=
  Additive = 40, // + -
  Multiplicative = 50, // * /
  Application = 60, // function application
  FieldAccess = 70, // .
}

// =============================================================================
// STATE HELPERS
// =============================================================================

const createParser = (source: string): ParserState => {
  const lexer = createLexer(source);
  return {
    lexer,
    diagnostics: [],
    current: nextToken(lexer),
  };
};

const at = (state: ParserState, kind: TokenKind): boolean => state.current[0] === kind;

const atAny = (state: ParserState, ...kinds: TokenKind[]): boolean =>
  kinds.includes(state.current[0]);

const advance = (state: ParserState): Token => {
  const prev = state.current;
  state.current = nextToken(state.lexer);
  return prev;
};

const expect = (state: ParserState, kind: TokenKind, message: string): Token | null => {
  if (at(state, kind)) {
    return advance(state);
  }
  error(state, message);
  return null;
};

const text = (state: ParserState, token: Token): string => slice(state.lexer, token[1], token[2]);

const error = (state: ParserState, message: string): void => {
  state.diagnostics.push({
    start: state.current[1],
    end: state.current[2],
    message,
    severity: "error",
  });
};

/** Create a span from token positions */
const span = (start: number, end: number): ast.Span => ({ start, end });

/** Create a span from a token */
const tokenSpan = (token: Token): ast.Span => span(token[1], token[2]);

const atNewStatement = (state: ParserState): boolean =>
  state.lexer.atLineStart && !at(state, TokenKind.Bar);

// =============================================================================
// ERROR RECOVERY
// =============================================================================

const synchronize = (state: ParserState): void => {
  while (!at(state, TokenKind.Eof)) {
    if (atAny(state, TokenKind.Let, TokenKind.Data, TokenKind.If, TokenKind.Match, TokenKind.End)) {
      return;
    }
    advance(state);
  }
};

// =============================================================================
// MAIN PARSE FUNCTION
// =============================================================================

export const parse = (source: string): ParseResult => {
  const state = createParser(source);
  const declarations: ast.DataDecl[] = [];
  const bindings: TopLevelBinding[] = [];
  let expr: ast.Expr | null = null;

  while (!at(state, TokenKind.Eof)) {
    if (at(state, TokenKind.Data)) {
      const decl = parseDataDecl(state);
      if (decl) declarations.push(decl);
    } else if (at(state, TokenKind.Let)) {
      const result = parseLetBindingOrExpr(state);
      if (result.kind === "binding") {
        bindings.push(result.binding);
      } else {
        expr = result.expr;
        break;
      }
    } else {
      expr = parseExpr(state);
      break;
    }
  }

  return {
    program: { declarations, bindings, expr },
    diagnostics: state.diagnostics,
  };
};

// =============================================================================
// LET BINDING OR EXPRESSION
// =============================================================================

type LetResult = { kind: "binding"; binding: TopLevelBinding } | { kind: "expr"; expr: ast.Expr };

const parseLetBindingOrExpr = (state: ParserState): LetResult => {
  advance(state); // 'let'

  const recursive = at(state, TokenKind.Rec);
  if (recursive) advance(state);

  const nameToken = expect(state, TokenKind.Lower, "expected binding name");
  if (!nameToken) {
    synchronize(state);
    return { kind: "expr", expr: ast.num(0) };
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);

  const params: { name: string; span: ast.Span }[] = [];
  while (at(state, TokenKind.Lower)) {
    const paramToken = advance(state);
    params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
  }

  if (!expect(state, TokenKind.Eq, "expected '=' after parameters")) {
    synchronize(state);
    return { kind: "expr", expr: ast.num(0) };
  }

  const body = parseExpr(state);

  if (at(state, TokenKind.In)) {
    advance(state); // 'in'

    let value = body;
    for (let i = params.length - 1; i >= 0; i--) {
      const p = params[i]!;
      value = ast.abs(p.name, value, undefined, p.span);
    }

    const continuation = parseExpr(state);
    const expr = recursive
      ? ast.letRec(name, value, continuation, undefined, nameSpan)
      : ast.let_(name, value, continuation, undefined, nameSpan);

    return { kind: "expr", expr };
  }

  return { kind: "binding", binding: { name, nameSpan, params, body, recursive } };
};

// =============================================================================
// DATA DECLARATIONS
// =============================================================================

const parseDataDecl = (state: ParserState): ast.DataDecl | null => {
  advance(state); // 'data'

  const nameToken = expect(state, TokenKind.Upper, "expected type name");
  if (!nameToken) {
    synchronize(state);
    return null;
  }
  const name = text(state, nameToken);

  const typeParams: string[] = [];
  while (at(state, TokenKind.Lower)) {
    typeParams.push(text(state, advance(state)));
  }

  if (!expect(state, TokenKind.Eq, "expected '=' after type parameters")) {
    synchronize(state);
    return null;
  }

  const constructors: ast.ConDecl[] = [];
  const first = parseConstructor(state);
  if (first) constructors.push(first);

  while (at(state, TokenKind.Bar)) {
    advance(state);
    const con = parseConstructor(state);
    if (con) constructors.push(con);
  }

  return ast.dataDecl(name, typeParams, constructors);
};

const parseConstructor = (state: ParserState): ast.ConDecl | null => {
  const nameToken = expect(state, TokenKind.Upper, "expected constructor name");
  if (!nameToken) return null;
  const name = text(state, nameToken);

  const fields: ast.TypeExpr[] = [];
  while (
    atAny(state, TokenKind.Lower, TokenKind.Upper, TokenKind.LParen) &&
    !atNewStatement(state)
  ) {
    const field = parseTypeAtom(state);
    if (field) fields.push(field);
    else break;
  }

  return ast.conDecl(name, fields);
};

const parseTypeAtom = (state: ParserState): ast.TypeExpr | null => {
  if (at(state, TokenKind.Lower)) {
    return ast.tyvar(text(state, advance(state)));
  }

  if (at(state, TokenKind.Upper)) {
    let type: ast.TypeExpr = ast.tycon(text(state, advance(state)));

    while (
      atAny(state, TokenKind.Lower, TokenKind.Upper, TokenKind.LParen) &&
      !atNewStatement(state)
    ) {
      const arg = parseTypeAtomSimple(state);
      if (arg) {
        type = ast.tyapp(type, arg);
      } else {
        break;
      }
    }

    return type;
  }

  if (at(state, TokenKind.LParen)) {
    advance(state);
    const inner = parseTypeAtom(state);
    expect(state, TokenKind.RParen, "expected ')' after type");
    return inner;
  }

  return null;
};

const parseTypeAtomSimple = (state: ParserState): ast.TypeExpr | null => {
  if (at(state, TokenKind.Lower)) {
    return ast.tyvar(text(state, advance(state)));
  }

  if (at(state, TokenKind.Upper)) {
    return ast.tycon(text(state, advance(state)));
  }

  if (at(state, TokenKind.LParen)) {
    advance(state);
    const inner = parseTypeAtom(state);
    expect(state, TokenKind.RParen, "expected ')' after type");
    return inner;
  }

  return null;
};

// =============================================================================
// EXPRESSIONS (PRATT PARSER)
// =============================================================================

const parseExpr = (state: ParserState): ast.Expr => parsePrecedence(state, Bp.None);

const parsePrecedence = (state: ParserState, minBp: number): ast.Expr => {
  let left = parsePrefix(state);

  while (true) {
    const bp = infixBindingPower(state);
    if (bp <= minBp) break;
    left = parseInfix(state, left, bp);
  }

  return left;
};

const parsePrefix = (state: ParserState): ast.Expr => {
  const token = state.current;
  const kind = token[0];
  const start = token[1];

  switch (kind) {
    case TokenKind.Number: {
      advance(state);
      return ast.num(parseFloat(text(state, token)), tokenSpan(token));
    }

    case TokenKind.String: {
      advance(state);
      return ast.str(parseStringContent(text(state, token)), tokenSpan(token));
    }

    case TokenKind.True:
      advance(state);
      return ast.bool(true, tokenSpan(token));

    case TokenKind.False:
      advance(state);
      return ast.bool(false, tokenSpan(token));

    case TokenKind.Lower: {
      advance(state);
      const name = text(state, token);
      const paramSpan = tokenSpan(token);

      if (at(state, TokenKind.Arrow)) {
        advance(state);
        const body = parseExpr(state);
        const end = body.span?.end ?? state.current[1];
        return ast.abs(name, body, span(start, end), paramSpan);
      }

      return ast.var_(name, tokenSpan(token));
    }

    case TokenKind.Upper: {
      advance(state);
      return ast.var_(text(state, token), tokenSpan(token));
    }

    case TokenKind.LParen:
      return parseParenOrTuple(state);

    case TokenKind.LBrace:
      return parseRecord(state);

    case TokenKind.If:
      return parseIf(state);

    case TokenKind.Match:
      return parseMatch(state);

    case TokenKind.Let:
      return parseLetExpr(state);

    default: {
      error(state, `unexpected token: ${TokenKind[kind]}`);
      advance(state);
      return ast.num(0);
    }
  }
};

const parseInfix = (state: ParserState, left: ast.Expr, bp: number): ast.Expr => {
  const kind = state.current[0];
  const start = left.span?.start ?? 0;

  const binOp = (op: ast.Op): ast.Expr => {
    advance(state);
    const right = parsePrecedence(state, bp);
    const end = right.span?.end ?? state.current[1];
    return ast.binOp(op, left, right, span(start, end));
  };

  switch (kind) {
    case TokenKind.Plus:
      return binOp("+");
    case TokenKind.Minus:
      return binOp("-");
    case TokenKind.Star:
      return binOp("*");
    case TokenKind.Slash:
      return binOp("/");
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

    case TokenKind.Pipe: {
      advance(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return ast.app(right, left, span(start, end));
    }

    case TokenKind.ColonColon: {
      // :: is right-associative, so use bp - 1 for right side
      advance(state);
      const right = parsePrecedence(state, bp - 1);
      const end = right.span?.end ?? state.current[1];
      // Desugar a :: b to Cons a b
      return ast.app(ast.app(ast.var_("Cons"), left), right, span(start, end));
    }

    case TokenKind.Dot: {
      advance(state);
      const fieldToken = expect(state, TokenKind.Lower, "expected field name after '.'");
      const field = fieldToken ? text(state, fieldToken) : "?";
      const end = fieldToken ? fieldToken[2] : state.current[1];
      return ast.fieldAccess(left, field, span(start, end));
    }

    default: {
      const right = parsePrefix(state);
      const end = right.span?.end ?? state.current[1];
      return ast.app(left, right, span(start, end));
    }
  }
};

const infixBindingPower = (state: ParserState): number => {
  const kind = state.current[0];

  switch (kind) {
    case TokenKind.Pipe:
      return Bp.Pipe;

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
      return Bp.Additive;

    case TokenKind.Star:
    case TokenKind.Slash:
      return Bp.Multiplicative;

    case TokenKind.Dot:
      return Bp.FieldAccess;

    case TokenKind.Lower:
    case TokenKind.Upper:
    case TokenKind.Number:
    case TokenKind.String:
    case TokenKind.True:
    case TokenKind.False:
    case TokenKind.LParen:
    case TokenKind.LBrace:
      if (state.lexer.atLineStart) {
        return Bp.None;
      }
      return Bp.Application;

    default:
      return Bp.None;
  }
};

// =============================================================================
// COMPOUND EXPRESSIONS
// =============================================================================

const parseParenOrTuple = (state: ParserState): ast.Expr => {
  const start = state.current[1];
  advance(state); // (

  if (at(state, TokenKind.RParen)) {
    advance(state);
    error(state, "empty parentheses");
    return ast.num(0);
  }

  const first = parseExpr(state);

  if (at(state, TokenKind.Comma)) {
    const elements: ast.Expr[] = [first];
    while (at(state, TokenKind.Comma)) {
      advance(state);
      elements.push(parseExpr(state));
    }
    const endToken = expect(state, TokenKind.RParen, "expected ')' after tuple");
    const end = endToken ? endToken[2] : state.current[1];
    return ast.tuple(elements, span(start, end));
  }

  expect(state, TokenKind.RParen, "expected ')' after expression");
  return first;
};

const parseRecord = (state: ParserState): ast.Expr => {
  const start = state.current[1];
  advance(state); // {

  const fields: ast.RecordField[] = [];

  if (!at(state, TokenKind.RBrace)) {
    do {
      if (at(state, TokenKind.Comma)) advance(state);

      const nameToken = expect(state, TokenKind.Lower, "expected field name");
      if (!nameToken) break;
      const name = text(state, nameToken);
      const fieldStart = nameToken[1];

      expect(state, TokenKind.Eq, "expected '=' after field name");
      const value = parseExpr(state);
      const fieldEnd = value.span?.end ?? state.current[1];

      fields.push(ast.field(name, value, span(fieldStart, fieldEnd)));
    } while (at(state, TokenKind.Comma));
  }

  const endToken = expect(state, TokenKind.RBrace, "expected '}' after record");
  const end = endToken ? endToken[2] : state.current[1];
  return ast.record(fields, span(start, end));
};

const parseIf = (state: ParserState): ast.Expr => {
  const start = state.current[1];
  advance(state); // if

  const cond = parseExpr(state);
  expect(state, TokenKind.Then, "expected 'then' after condition");
  const thenBranch = parseExpr(state);
  expect(state, TokenKind.Else, "expected 'else' after 'then' branch");
  const elseBranch = parseExpr(state);
  const end = elseBranch.span?.end ?? state.current[1];

  return ast.if_(cond, thenBranch, elseBranch, span(start, end));
};

const parseMatch = (state: ParserState): ast.Expr => {
  const start = state.current[1];
  advance(state); // match

  const scrutinee = parseExpr(state);
  expect(state, TokenKind.With, "expected 'with' after match expression");

  const cases: ast.Case[] = [];
  while (at(state, TokenKind.Bar)) {
    const caseStart = state.current[1];
    advance(state);
    const pattern = parsePattern(state);
    expect(state, TokenKind.Arrow, "expected '=>' after pattern");
    const body = parseExpr(state);
    const caseEnd = body.span?.end ?? state.current[1];
    cases.push(ast.case_(pattern, body, span(caseStart, caseEnd)));
  }

  const endToken = expect(state, TokenKind.End, "expected 'end' after match cases");
  const end = endToken ? endToken[2] : state.current[1];

  return ast.match(scrutinee, cases, span(start, end));
};

const parseLetExpr = (state: ParserState): ast.Expr => {
  const start = state.current[1];
  advance(state); // let

  const recursive = at(state, TokenKind.Rec);
  if (recursive) advance(state);

  const nameToken = expect(state, TokenKind.Lower, "expected binding name");
  if (!nameToken) {
    return ast.num(0);
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);

  const params: { name: string; span: ast.Span }[] = [];
  while (at(state, TokenKind.Lower)) {
    const paramToken = advance(state);
    params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
  }

  expect(state, TokenKind.Eq, "expected '=' after name");

  let value = parseExpr(state);

  for (let i = params.length - 1; i >= 0; i--) {
    const p = params[i]!;
    value = ast.abs(p.name, value, undefined, p.span);
  }

  expect(state, TokenKind.In, "expected 'in' after let value");

  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];

  return recursive
    ? ast.letRec(name, value, body, span(start, end), nameSpan)
    : ast.let_(name, value, body, span(start, end), nameSpan);
};

// =============================================================================
// PATTERNS
// =============================================================================

const parsePattern = (state: ParserState): ast.Pattern => {
  const token = state.current;
  const kind = token[0];
  const start = token[1];

  switch (kind) {
    case TokenKind.Underscore:
      advance(state);
      return ast.pwildcard(tokenSpan(token));

    case TokenKind.Lower:
      advance(state);
      return ast.pvar(text(state, token), tokenSpan(token));

    case TokenKind.Upper: {
      advance(state);
      const name = text(state, token);
      const args: ast.Pattern[] = [];

      while (isPatternStart(state)) {
        args.push(parsePatternAtom(state));
      }

      const end =
        args.length > 0 ? (args[args.length - 1]!.span?.end ?? state.current[1]) : token[2];
      return ast.pcon(name, args, span(start, end));
    }

    case TokenKind.Number: {
      advance(state);
      return ast.plit(parseFloat(text(state, token)), tokenSpan(token));
    }

    case TokenKind.String: {
      advance(state);
      return ast.plit(parseStringContent(text(state, token)), tokenSpan(token));
    }

    case TokenKind.True:
      advance(state);
      return ast.plit(true, tokenSpan(token));

    case TokenKind.False:
      advance(state);
      return ast.plit(false, tokenSpan(token));

    case TokenKind.LParen:
      return parseTuplePattern(state);

    case TokenKind.LBrace:
      return parseRecordPattern(state);

    default:
      error(state, `unexpected token in pattern: ${TokenKind[kind]}`);
      advance(state);
      return ast.pwildcard();
  }
};

const parsePatternAtom = (state: ParserState): ast.Pattern => {
  const token = state.current;
  const kind = token[0];

  switch (kind) {
    case TokenKind.Underscore:
      advance(state);
      return ast.pwildcard(tokenSpan(token));

    case TokenKind.Lower:
      advance(state);
      return ast.pvar(text(state, token), tokenSpan(token));

    case TokenKind.Upper:
      advance(state);
      return ast.pcon(text(state, token), [], tokenSpan(token));

    case TokenKind.Number: {
      advance(state);
      return ast.plit(parseFloat(text(state, token)), tokenSpan(token));
    }

    case TokenKind.String: {
      advance(state);
      return ast.plit(parseStringContent(text(state, token)), tokenSpan(token));
    }

    case TokenKind.True:
      advance(state);
      return ast.plit(true, tokenSpan(token));

    case TokenKind.False:
      advance(state);
      return ast.plit(false, tokenSpan(token));

    case TokenKind.LParen:
      return parseTuplePattern(state);

    case TokenKind.LBrace:
      return parseRecordPattern(state);

    default:
      error(state, `unexpected token in pattern: ${TokenKind[kind]}`);
      return ast.pwildcard();
  }
};

const isPatternStart = (state: ParserState): boolean =>
  atAny(
    state,
    TokenKind.Underscore,
    TokenKind.Lower,
    TokenKind.Upper,
    TokenKind.Number,
    TokenKind.String,
    TokenKind.True,
    TokenKind.False,
    TokenKind.LParen,
    TokenKind.LBrace,
  );

const parseTuplePattern = (state: ParserState): ast.Pattern => {
  const start = state.current[1];
  advance(state); // (

  if (at(state, TokenKind.RParen)) {
    advance(state);
    error(state, "empty pattern");
    return ast.pwildcard();
  }

  const first = parsePattern(state);

  if (at(state, TokenKind.Comma)) {
    const elements: ast.Pattern[] = [first];
    while (at(state, TokenKind.Comma)) {
      advance(state);
      elements.push(parsePattern(state));
    }
    const endToken = expect(state, TokenKind.RParen, "expected ')' after tuple pattern");
    const end = endToken ? endToken[2] : state.current[1];
    return ast.ptuple(elements, span(start, end));
  }

  expect(state, TokenKind.RParen, "expected ')' after pattern");
  return first;
};

const parseRecordPattern = (state: ParserState): ast.Pattern => {
  const start = state.current[1];
  advance(state); // {

  const fields: ast.PRecordField[] = [];

  if (!at(state, TokenKind.RBrace)) {
    do {
      if (at(state, TokenKind.Comma)) advance(state);

      const nameToken = expect(state, TokenKind.Lower, "expected field name");
      if (!nameToken) break;
      const name = text(state, nameToken);
      const fieldStart = nameToken[1];

      expect(state, TokenKind.Eq, "expected '=' after field name");
      const pattern = parsePattern(state);
      const fieldEnd = pattern.span?.end ?? state.current[1];

      fields.push(ast.pfield(name, pattern, span(fieldStart, fieldEnd)));
    } while (at(state, TokenKind.Comma));
  }

  const endToken = expect(state, TokenKind.RBrace, "expected '}' after record pattern");
  const end = endToken ? endToken[2] : state.current[1];
  return ast.precord(fields, span(start, end));
};

// =============================================================================
// HELPERS
// =============================================================================

const parseStringContent = (quoted: string): string => {
  const inner = quoted.slice(1, -1);
  let result = "";
  let i = 0;

  while (i < inner.length) {
    if (inner[i] === "\\") {
      i++;
      if (i >= inner.length) break;
      switch (inner[i]) {
        case "n":
          result += "\n";
          break;
        case "t":
          result += "\t";
          break;
        case "r":
          result += "\r";
          break;
        case "\\":
          result += "\\";
          break;
        case '"':
          result += '"';
          break;
        default:
          result += inner[i];
      }
    } else {
      result += inner[i];
    }
    i++;
  }

  return result;
};

// =============================================================================
// PROGRAM TO EXPRESSION
// =============================================================================

/**
 * Convert top-level bindings to a single expression for type inference.
 * Wraps bindings in nested let/letrec expressions.
 */
export const programToExpr = (program: Program): ast.Expr | null => {
  if (!program.expr && program.bindings.length === 0) {
    return null;
  }

  let expr = program.expr ?? ast.num(0);

  for (let i = program.bindings.length - 1; i >= 0; i--) {
    const binding = program.bindings[i]!;

    let value = binding.body;
    for (let j = binding.params.length - 1; j >= 0; j--) {
      const p = binding.params[j]!;
      value = ast.abs(p.name, value, undefined, p.span);
    }

    if (binding.recursive) {
      expr = ast.letRec(binding.name, value, expr, undefined, binding.nameSpan);
    } else {
      expr = ast.let_(binding.name, value, expr, undefined, binding.nameSpan);
    }
  }

  return expr;
};
