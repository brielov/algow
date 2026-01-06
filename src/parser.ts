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
  readonly modules: readonly ast.ModuleDecl[];
  readonly uses: readonly ast.UseDecl[];
  readonly declarations: readonly ast.DataDecl[];
  readonly bindings: readonly TopLevelBinding[];
  readonly expr: ast.Expr | null;
};

export type TopLevelBinding = {
  readonly name: string;
  readonly nameSpan: ast.Span;
  readonly params: readonly { name: string; span: ast.Span; type?: ast.TypeExpr }[];
  readonly returnType?: ast.TypeExpr;
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
  Or = 12, // ||
  And = 14, // &&
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
// LAMBDA DETECTION AND PARSING
// =============================================================================

/**
 * Check if we're at the start of a lambda: lowercase+ ->
 * Uses lookahead without consuming tokens.
 */
const isLambdaStart = (state: ParserState): boolean => {
  // Save lexer state
  const savedPos = state.lexer.pos;
  const savedCurrent = state.current;

  // Skip lowercase identifiers
  while (at(state, TokenKind.Lower)) {
    advance(state);
  }

  // Check if we hit an arrow
  const isLambda = at(state, TokenKind.Arrow);

  // Restore lexer state
  state.lexer.pos = savedPos;
  state.current = savedCurrent;

  return isLambda;
};

/**
 * Parse a lambda: x y z -> body
 * Produces nested Abs nodes for currying.
 */
const parseLambda = (state: ParserState): ast.Expr => {
  const start = state.current[1];
  const params: { name: string; span: ast.Span }[] = [];

  // Collect all parameters
  while (at(state, TokenKind.Lower) && !at(state, TokenKind.Arrow)) {
    const token = advance(state);
    params.push({ name: text(state, token), span: tokenSpan(token) });
  }

  expect(state, TokenKind.Arrow, "expected '->'");
  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];

  // Build curried lambdas from right to left
  let result: ast.Expr = body;
  for (let i = params.length - 1; i >= 0; i--) {
    const p = params[i]!;
    const absStart = i === 0 ? start : p.span.start;
    result = ast.abs(p.name, result, span(absStart, end), p.span);
  }

  return result;
};

// =============================================================================
// ERROR RECOVERY
// =============================================================================

const synchronize = (state: ParserState): void => {
  while (!at(state, TokenKind.Eof)) {
    if (atAny(state, TokenKind.Let, TokenKind.Type, TokenKind.If, TokenKind.Match, TokenKind.End)) {
      return;
    }
    advance(state);
  }
};

// =============================================================================
// PARAMETER PARSING
// =============================================================================

type Param = { name: string; span: ast.Span; type?: ast.TypeExpr };

/**
 * Parse function parameters: name | (name : type)
 * Returns array of parameters with optional type annotations.
 */
const parseParams = (state: ParserState): Param[] => {
  const params: Param[] = [];

  while (at(state, TokenKind.Lower) || at(state, TokenKind.LParen)) {
    if (at(state, TokenKind.LParen)) {
      advance(state); // (
      const paramToken = expect(state, TokenKind.Lower, "expected parameter name");
      if (!paramToken) break;
      const paramName = text(state, paramToken);
      let paramType: ast.TypeExpr | undefined;

      if (at(state, TokenKind.Colon)) {
        advance(state); // :
        paramType = parseType(state) ?? undefined;
      }

      expect(state, TokenKind.RParen, "expected ')' after parameter");
      params.push({ name: paramName, span: tokenSpan(paramToken), type: paramType });
    } else {
      const paramToken = advance(state);
      params.push({ name: text(state, paramToken), span: tokenSpan(paramToken) });
    }
  }

  return params;
};

/**
 * Wrap body in nested lambdas for currying.
 */
const wrapInLambdas = (params: readonly Param[], body: ast.Expr): ast.Expr => {
  let result = body;
  for (let i = params.length - 1; i >= 0; i--) {
    const p = params[i]!;
    result = ast.abs(p.name, result, undefined, p.span, p.type);
  }
  return result;
};

// =============================================================================
// MAIN PARSE FUNCTION
// =============================================================================

export const parse = (source: string): ParseResult => {
  const state = createParser(source);
  const modules: ast.ModuleDecl[] = [];
  const uses: ast.UseDecl[] = [];
  const declarations: ast.DataDecl[] = [];
  const bindings: TopLevelBinding[] = [];
  let expr: ast.Expr | null = null;

  // Parse module declarations first
  while (at(state, TokenKind.Module)) {
    const mod = parseModuleDecl(state);
    if (mod) modules.push(mod);
  }

  // Parse use statements
  while (at(state, TokenKind.Use)) {
    const use = parseUseDecl(state);
    if (use) uses.push(use);
  }

  // Parse data declarations, bindings, and expression
  while (!at(state, TokenKind.Eof)) {
    if (at(state, TokenKind.Type)) {
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
    program: { modules, uses, declarations, bindings, expr },
    diagnostics: state.diagnostics,
  };
};

// =============================================================================
// LET BINDING OR EXPRESSION
// =============================================================================

type LetResult = { kind: "binding"; binding: TopLevelBinding } | { kind: "expr"; expr: ast.Expr };

const parseLetBindingOrExpr = (state: ParserState): LetResult => {
  const start = state.current[1];
  advance(state); // 'let'

  const recursive = at(state, TokenKind.Rec);
  if (recursive) advance(state);

  // Check for destructuring pattern (tuple, record, wildcard, or constructor)
  // Only allowed for non-recursive let, and always produces an expression (not a binding)
  if (
    !recursive &&
    atAny(state, TokenKind.LParen, TokenKind.LBrace, TokenKind.Underscore, TokenKind.Upper)
  ) {
    const pattern = parsePattern(state);

    expect(state, TokenKind.Eq, "expected '=' after pattern");
    const value = parseExpr(state);
    expect(state, TokenKind.In, "expected 'in' after let value");
    const body = parseExpr(state);
    const end = body.span?.end ?? state.current[1];

    // Desugar to: match value when pattern -> body end
    return { kind: "expr", expr: ast.match(value, [ast.case_(pattern, body)], span(start, end)) };
  }

  const nameToken = expect(state, TokenKind.Lower, "expected binding name");
  if (!nameToken) {
    synchronize(state);
    return { kind: "expr", expr: ast.num(0) };
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = parseParams(state);

  // Check for return type: let f x y : number = ...
  let returnType: ast.TypeExpr | undefined;
  if (at(state, TokenKind.Colon)) {
    advance(state); // :
    returnType = parseType(state) ?? undefined;
  }

  if (!expect(state, TokenKind.Eq, "expected '=' after parameters")) {
    synchronize(state);
    return { kind: "expr", expr: ast.num(0) };
  }

  const body = parseExpr(state);

  if (at(state, TokenKind.In) || (recursive && at(state, TokenKind.AndKw))) {
    const value = wrapInLambdas(params, body);

    if (recursive) {
      // Parse additional bindings with 'and'
      const bindings: ast.RecBinding[] = [ast.recBinding(name, value, nameSpan, returnType)];
      while (at(state, TokenKind.AndKw)) {
        advance(state); // 'and'
        bindings.push(parseRecBinding(state));
      }

      expect(state, TokenKind.In, "expected 'in' after let rec bindings");
      const continuation = parseExpr(state);
      return { kind: "expr", expr: ast.letRec(bindings, continuation) };
    }

    advance(state); // 'in'
    const continuation = parseExpr(state);
    return {
      kind: "expr",
      expr: ast.let_(name, value, continuation, undefined, nameSpan, returnType),
    };
  }

  return { kind: "binding", binding: { name, nameSpan, params, returnType, body, recursive } };
};

// =============================================================================
// TYPE DECLARATIONS
// =============================================================================

const parseDataDecl = (state: ParserState): ast.DataDecl | null => {
  advance(state); // 'type'

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

// =============================================================================
// MODULE DECLARATIONS
// =============================================================================

const parseModuleDecl = (state: ParserState): ast.ModuleDecl | null => {
  const start = state.current[1];
  advance(state); // 'module'

  const nameToken = expect(state, TokenKind.Upper, "expected module name");
  if (!nameToken) {
    synchronize(state);
    return null;
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);

  const declarations: ast.DataDecl[] = [];
  const bindings: ast.RecBinding[] = [];

  while (!at(state, TokenKind.End) && !at(state, TokenKind.Eof)) {
    if (at(state, TokenKind.Type)) {
      const decl = parseDataDecl(state);
      if (decl) declarations.push(decl);
    } else if (at(state, TokenKind.Let)) {
      const binding = parseModuleBinding(state);
      if (binding) bindings.push(binding);
    } else {
      error(state, "expected 'data' or 'let' declaration in module");
      advance(state);
    }
  }

  const endToken = expect(state, TokenKind.End, "expected 'end' after module body");
  const end = endToken ? endToken[2] : state.current[1];

  return ast.moduleDecl(name, declarations, bindings, span(start, end), nameSpan);
};

/**
 * Parse a binding inside a module: let name params = expr
 * Unlike top-level bindings, these always produce a RecBinding.
 */
const parseModuleBinding = (state: ParserState): ast.RecBinding | null => {
  advance(state); // 'let'

  const recursive = at(state, TokenKind.Rec);
  if (recursive) advance(state);

  const nameToken = expect(state, TokenKind.Lower, "expected binding name");
  if (!nameToken) {
    synchronize(state);
    return null;
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = parseParams(state);

  let returnType: ast.TypeExpr | undefined;
  if (at(state, TokenKind.Colon)) {
    advance(state);
    returnType = parseType(state) ?? undefined;
  }

  expect(state, TokenKind.Eq, "expected '=' after parameters");
  const value = wrapInLambdas(params, parseExpr(state));

  return ast.recBinding(name, value, nameSpan, returnType);
};

// =============================================================================
// USE DECLARATIONS
// =============================================================================

const parseUseDecl = (state: ParserState): ast.UseDecl | null => {
  const start = state.current[1];
  advance(state); // 'use'

  const moduleToken = expect(state, TokenKind.Upper, "expected module name");
  if (!moduleToken) {
    synchronize(state);
    return null;
  }
  const moduleName = text(state, moduleToken);
  const moduleSpan = tokenSpan(moduleToken);

  let imports: ast.ImportSpec | null = null;
  let alias: string | undefined;
  let aliasSpan: ast.Span | undefined;

  // Check for import list: (..) or (items)
  if (at(state, TokenKind.LParen)) {
    advance(state); // (

    if (at(state, TokenKind.Dot)) {
      // Check for (..)
      advance(state); // first .
      if (at(state, TokenKind.Dot)) {
        advance(state); // second .
        imports = ast.importAll();
      } else {
        error(state, "expected '..' for import all");
      }
      expect(state, TokenKind.RParen, "expected ')' after '..'");
    } else {
      // Parse specific import items
      const items = parseImportItems(state);
      imports = ast.importSpecific(items);
      expect(state, TokenKind.RParen, "expected ')' after import list");
    }
  }

  // Check for 'as Alias'
  if (at(state, TokenKind.As)) {
    advance(state); // 'as'
    const aliasToken = expect(state, TokenKind.Upper, "expected alias name");
    if (aliasToken) {
      alias = text(state, aliasToken);
      aliasSpan = tokenSpan(aliasToken);
    }
  }

  const end = state.current[1];
  return ast.useDecl(moduleName, imports, alias, span(start, end), moduleSpan, aliasSpan);
};

/**
 * Parse import items: Maybe, Maybe(..), Maybe(Just, Nothing), map, filter
 */
const parseImportItems = (state: ParserState): ast.ImportItem[] => {
  const items: ast.ImportItem[] = [];

  do {
    if (at(state, TokenKind.Comma)) advance(state);

    if (at(state, TokenKind.Upper)) {
      // Type or Type(..) or Type(Con1, Con2)
      const nameToken = advance(state);
      const name = text(state, nameToken);
      const nameSpan = tokenSpan(nameToken);
      let constructors: readonly string[] | "all" | undefined;

      if (at(state, TokenKind.LParen)) {
        advance(state); // (
        if (at(state, TokenKind.Dot)) {
          // Type(..)
          advance(state); // first .
          if (at(state, TokenKind.Dot)) {
            advance(state); // second .
            constructors = "all";
          } else {
            error(state, "expected '..' for all constructors");
          }
        } else {
          // Type(Con1, Con2)
          const cons: string[] = [];
          do {
            if (at(state, TokenKind.Comma)) advance(state);
            const conToken = expect(state, TokenKind.Upper, "expected constructor name");
            if (conToken) cons.push(text(state, conToken));
          } while (at(state, TokenKind.Comma));
          constructors = cons;
        }
        expect(state, TokenKind.RParen, "expected ')' after constructors");
      }

      items.push(ast.importItem(name, constructors, nameSpan));
    } else if (at(state, TokenKind.Lower)) {
      // Value/function
      const nameToken = advance(state);
      items.push(ast.importItem(text(state, nameToken), undefined, tokenSpan(nameToken)));
    } else {
      break;
    }
  } while (at(state, TokenKind.Comma));

  return items;
};

/**
 * Parse a type expression including function types.
 * Type := TypeAtom | TypeAtom '->' Type
 */
const parseType = (state: ParserState): ast.TypeExpr | null => {
  const left = parseTypeAtom(state);
  if (!left) return null;

  // Check for function type: a -> b
  if (at(state, TokenKind.Arrow)) {
    advance(state); // ->
    const right = parseType(state); // Right-associative
    if (!right) return left;
    return ast.tyfun(left, right);
  }

  return left;
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
      return ast.str(parseStringContent(state, text(state, token), token[1]), tokenSpan(token));
    }

    case TokenKind.True:
      advance(state);
      return ast.bool(true, tokenSpan(token));

    case TokenKind.False:
      advance(state);
      return ast.bool(false, tokenSpan(token));

    case TokenKind.Lower: {
      // Check if this is a multi-param lambda: x y z -> body
      if (isLambdaStart(state)) {
        return parseLambda(state);
      }

      advance(state);
      const name = text(state, token);
      return ast.var_(name, tokenSpan(token));
    }

    case TokenKind.Upper: {
      advance(state);
      return ast.var_(text(state, token), tokenSpan(token));
    }

    case TokenKind.LParen:
      return parseParenOrTuple(state);

    // Unary negation: -x desugars to 0 - x
    case TokenKind.Minus: {
      advance(state);
      // Parse with high precedence to bind tightly (higher than multiplicative)
      const operand = parsePrecedence(state, Bp.Multiplicative + 1);
      const end = operand.span?.end ?? state.current[1];
      return ast.binOp("-", ast.num(0, span(start, start)), operand, span(start, end));
    }

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

    // Logical operators desugar to if expressions for short-circuit evaluation:
    // a && b  →  if a then b else false
    // a || b  →  if a then true else b
    case TokenKind.And: {
      advance(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return ast.if_(left, right, ast.bool(false), span(start, end));
    }

    case TokenKind.Or: {
      advance(state);
      const right = parsePrecedence(state, bp);
      const end = right.span?.end ?? state.current[1];
      return ast.if_(left, ast.bool(true), right, span(start, end));
    }

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

      // Check for qualified access: Module.name (left is uppercase Var)
      if (
        left.kind === "Var" &&
        left.name.length > 0 &&
        left.name[0]!.toUpperCase() === left.name[0]
      ) {
        // This could be Module.member
        if (at(state, TokenKind.Lower) || at(state, TokenKind.Upper)) {
          const memberToken = advance(state);
          const member = text(state, memberToken);
          const memberSpan = tokenSpan(memberToken);
          const end = memberToken[2];
          return ast.qualifiedVar(left.name, member, span(start, end), left.span, memberSpan);
        }
      }

      // Tuple indexing: tuple.0, tuple.1, etc.
      if (at(state, TokenKind.Number)) {
        const indexToken = state.current;
        advance(state);
        const indexStr = text(state, indexToken);
        const index = parseInt(indexStr, 10);
        if (!Number.isInteger(index) || index < 0) {
          state.diagnostics.push({
            start: indexToken[1],
            end: indexToken[2],
            message: "tuple index must be a non-negative integer",
            severity: "error",
          });
        }
        const end = indexToken[2];
        return ast.tupleIndex(left, index, span(start, end));
      }
      // Record field access: record.field
      const fieldToken = expect(state, TokenKind.Lower, "expected field name or index after '.'");
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

  // Check if this is an annotated lambda: (name : type) -> body
  // We look for: Lower Colon
  if (at(state, TokenKind.Lower)) {
    const savedPos = state.lexer.pos;
    const savedCurrent = state.current;
    const nameToken = advance(state);

    if (at(state, TokenKind.Colon)) {
      // This is (name : type) -> body
      advance(state); // :
      const paramType = parseType(state);
      expect(state, TokenKind.RParen, "expected ')' after type");

      if (at(state, TokenKind.Arrow)) {
        advance(state); // ->
        const body = parseExpr(state);
        const end = body.span?.end ?? state.current[1];
        return ast.abs(
          text(state, nameToken),
          body,
          span(start, end),
          tokenSpan(nameToken),
          paramType ?? undefined,
        );
      }

      // Not followed by ->, this is an error
      error(state, "expected '->' after annotated parameter");
      return ast.num(0);
    }

    // Not an annotated lambda, restore and continue
    state.lexer.pos = savedPos;
    state.current = savedCurrent;
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
      const fieldEnd = nameToken[2];

      // Support field punning: { x, y } is shorthand for { x = x, y = y }
      if (at(state, TokenKind.Eq)) {
        advance(state);
        const value = parseExpr(state);
        const valueEnd = value.span?.end ?? state.current[1];
        fields.push(ast.field(name, value, span(fieldStart, valueEnd)));
      } else {
        // Punning: { x } means { x = x }
        const value = ast.var_(name, span(fieldStart, fieldEnd));
        fields.push(ast.field(name, value, span(fieldStart, fieldEnd)));
      }
    } while (at(state, TokenKind.Comma));
  }

  const endToken = expect(state, TokenKind.RBrace, "expected '}' after record");
  const end = endToken ? endToken[2] : state.current[1];
  return ast.record(fields, span(start, end));
};

/**
 * Parse a list literal: [1, 2, 3]
 * Desugars to: Cons 1 (Cons 2 (Cons 3 Nil))
 */
const parseListLiteral = (state: ParserState): ast.Expr => {
  const start = state.current[1];
  advance(state); // [

  const elements: ast.Expr[] = [];

  if (!at(state, TokenKind.RBracket)) {
    do {
      if (at(state, TokenKind.Comma)) advance(state);
      elements.push(parseExpr(state));
    } while (at(state, TokenKind.Comma));
  }

  const endToken = expect(state, TokenKind.RBracket, "expected ']' after list");
  const end = endToken ? endToken[2] : state.current[1];

  // Desugar [x1, x2, x3] to Cons x1 (Cons x2 (Cons x3 Nil))
  // Build from right to left
  let result: ast.Expr = ast.var_("Nil", span(end - 1, end));
  for (let i = elements.length - 1; i >= 0; i--) {
    const elem = elements[i]!;
    const elemStart = elem.span?.start ?? start;
    result = ast.app(ast.app(ast.var_("Cons"), elem), result, span(elemStart, end));
  }

  return result;
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

  const cases: ast.Case[] = [];
  while (at(state, TokenKind.When)) {
    const caseStart = state.current[1];
    advance(state); // when
    let pattern = parsePattern(state);

    // Check for or-pattern: when pat1 | pat2 | pat3 -> ...
    if (at(state, TokenKind.Bar)) {
      const alternatives: ast.Pattern[] = [pattern];
      while (at(state, TokenKind.Bar)) {
        advance(state);
        alternatives.push(parsePattern(state));
      }
      const orEnd = alternatives[alternatives.length - 1]?.span?.end ?? state.current[1];
      pattern = ast.por(alternatives, span(caseStart, orEnd));
    }

    // Parse optional guard: when pattern if condition -> body
    let guard: ast.Expr | undefined;
    if (at(state, TokenKind.If)) {
      advance(state);
      guard = parseExpr(state);
    }

    expect(state, TokenKind.Arrow, "expected '->' after pattern");
    const body = parseExpr(state);
    const caseEnd = body.span?.end ?? state.current[1];
    cases.push(ast.case_(pattern, body, guard, span(caseStart, caseEnd)));
  }

  const endToken = expect(state, TokenKind.End, "expected 'end' after match cases");
  const end = endToken ? endToken[2] : state.current[1];

  return ast.match(scrutinee, cases, span(start, end));
};

/**
 * Parse a single recursive binding: name params = expr
 * Returns { name, nameSpan, value } where value has params wrapped as lambdas
 */
const parseRecBinding = (state: ParserState): ast.RecBinding => {
  const nameToken = expect(state, TokenKind.Lower, "expected binding name");
  if (!nameToken) {
    return ast.recBinding("_error_", ast.num(0));
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = parseParams(state);

  let returnType: ast.TypeExpr | undefined;
  if (at(state, TokenKind.Colon)) {
    advance(state);
    returnType = parseType(state) ?? undefined;
  }

  expect(state, TokenKind.Eq, "expected '=' after name");
  const value = wrapInLambdas(params, parseExpr(state));

  return ast.recBinding(name, value, nameSpan, returnType);
};

const parseLetExpr = (state: ParserState): ast.Expr => {
  const start = state.current[1];
  advance(state); // let

  const recursive = at(state, TokenKind.Rec);
  if (recursive) advance(state);

  // Check for destructuring pattern (tuple, record, wildcard, or constructor)
  // Only allowed for non-recursive let
  if (
    !recursive &&
    atAny(state, TokenKind.LParen, TokenKind.LBrace, TokenKind.Underscore, TokenKind.Upper)
  ) {
    return parseLetDestructuring(state, start);
  }

  if (recursive) {
    // Parse first binding
    const bindings: ast.RecBinding[] = [parseRecBinding(state)];

    // Parse additional bindings with 'and'
    while (at(state, TokenKind.AndKw)) {
      advance(state); // 'and'
      bindings.push(parseRecBinding(state));
    }

    expect(state, TokenKind.In, "expected 'in' after let rec bindings");

    const body = parseExpr(state);
    const end = body.span?.end ?? state.current[1];

    return ast.letRec(bindings, body, span(start, end));
  }

  // Non-recursive let
  const nameToken = expect(state, TokenKind.Lower, "expected binding name");
  if (!nameToken) {
    return ast.num(0);
  }
  const name = text(state, nameToken);
  const nameSpan = tokenSpan(nameToken);
  const params = parseParams(state);

  let returnType: ast.TypeExpr | undefined;
  if (at(state, TokenKind.Colon)) {
    advance(state);
    returnType = parseType(state) ?? undefined;
  }

  expect(state, TokenKind.Eq, "expected '=' after name");
  const value = wrapInLambdas(params, parseExpr(state));

  expect(state, TokenKind.In, "expected 'in' after let value");

  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];

  return ast.let_(name, value, body, span(start, end), nameSpan, returnType);
};

/**
 * Parse let destructuring: let pattern = expr in body
 * Desugars to: match expr when pattern -> body end
 */
const parseLetDestructuring = (state: ParserState, start: number): ast.Expr => {
  const pattern = parsePattern(state);

  expect(state, TokenKind.Eq, "expected '=' after pattern");

  const value = parseExpr(state);

  expect(state, TokenKind.In, "expected 'in' after let value");

  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];

  // Desugar to: match value when pattern -> body end
  return ast.match(value, [ast.case_(pattern, body)], span(start, end));
};

// =============================================================================
// PATTERNS
// =============================================================================

const PATTERN_STARTS = new Set([
  TokenKind.Underscore,
  TokenKind.Lower,
  TokenKind.Upper,
  TokenKind.Number,
  TokenKind.String,
  TokenKind.True,
  TokenKind.False,
  TokenKind.LParen,
  TokenKind.LBrace,
]);

const parsePattern = (state: ParserState, allowArgs = true): ast.Pattern => {
  const pattern = parsePatternCore(state, allowArgs);

  // Check for as-pattern: pattern as name
  if (at(state, TokenKind.As)) {
    advance(state);
    const nameToken = expect(state, TokenKind.Lower, "expected name after 'as'");
    if (!nameToken) return pattern;
    const name = text(state, nameToken);
    const nameSpan = tokenSpan(nameToken);
    const start = pattern.span?.start ?? nameToken[1];
    const end = nameToken[2];
    return ast.pas(pattern, name, span(start, end), nameSpan);
  }

  return pattern;
};

const parsePatternCore = (state: ParserState, allowArgs = true): ast.Pattern => {
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
      const nameSpan = tokenSpan(token);

      // Check for qualified constructor: Module.Constructor
      if (at(state, TokenKind.Dot)) {
        advance(state); // .
        const conToken = expect(state, TokenKind.Upper, "expected constructor name after '.'");
        if (!conToken) return ast.pwildcard();
        const conName = text(state, conToken);
        const conSpan = tokenSpan(conToken);

        if (!allowArgs) {
          const end = conToken[2];
          return ast.qualifiedPCon(name, conName, [], span(start, end), nameSpan, conSpan);
        }

        const args: ast.Pattern[] = [];
        while (PATTERN_STARTS.has(state.current[0])) {
          args.push(parsePatternCore(state, false));
        }
        const end = args[args.length - 1]?.span?.end ?? conToken[2];
        return ast.qualifiedPCon(name, conName, args, span(start, end), nameSpan, conSpan);
      }

      // Regular constructor pattern
      if (!allowArgs) return ast.pcon(name, [], nameSpan, nameSpan);

      const args: ast.Pattern[] = [];
      while (PATTERN_STARTS.has(state.current[0])) {
        args.push(parsePatternCore(state, false));
      }
      const end = args[args.length - 1]?.span?.end ?? token[2];
      return ast.pcon(name, args, span(start, end), nameSpan);
    }

    case TokenKind.Number:
      advance(state);
      return ast.plit(parseFloat(text(state, token)), tokenSpan(token));

    case TokenKind.String:
      advance(state);
      return ast.plit(parseStringContent(state, text(state, token), token[1]), tokenSpan(token));

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
      if (allowArgs) advance(state);
      return ast.pwildcard();
  }
};

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
      const fieldEnd = nameToken[2];

      // Support field punning: { x, y } is shorthand for { x = x, y = y }
      if (at(state, TokenKind.Eq)) {
        advance(state);
        const pattern = parsePattern(state);
        const patternEnd = pattern.span?.end ?? state.current[1];
        fields.push(ast.pfield(name, pattern, span(fieldStart, patternEnd)));
      } else {
        // Punning: { x } means { x = x } (binds field to variable of same name)
        const pattern = ast.pvar(name, span(fieldStart, fieldEnd));
        fields.push(ast.pfield(name, pattern, span(fieldStart, fieldEnd)));
      }
    } while (at(state, TokenKind.Comma));
  }

  const endToken = expect(state, TokenKind.RBrace, "expected '}' after record pattern");
  const end = endToken ? endToken[2] : state.current[1];
  return ast.precord(fields, span(start, end));
};

// =============================================================================
// HELPERS
// =============================================================================

const parseStringContent = (state: ParserState, quoted: string, tokenStart: number): string => {
  const inner = quoted.slice(1, -1);
  let result = "";
  let i = 0;

  while (i < inner.length) {
    if (inner[i] === "\\") {
      const escapeStart = tokenStart + 1 + i; // +1 for opening quote
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
          state.diagnostics.push({
            message: `Unknown escape sequence: \\${inner[i]}`,
            start: escapeStart,
            end: escapeStart + 2,
            severity: "error",
          });
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
      value = ast.abs(p.name, value, undefined, p.span, p.type);
    }

    if (binding.recursive) {
      expr = ast.letRec(
        [ast.recBinding(binding.name, value, binding.nameSpan, binding.returnType)],
        expr,
      );
    } else {
      expr = ast.let_(binding.name, value, expr, undefined, binding.nameSpan, binding.returnType);
    }
  }

  return expr;
};
