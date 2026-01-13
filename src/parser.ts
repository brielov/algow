/**
 * Pratt Parser for the Algow language.
 *
 * Produces Surface AST with all syntactic sugar preserved.
 * Desugaring happens in a separate phase (desugar.ts).
 */

import type { Diagnostic } from "./diagnostics";
import { createLexer, type LexerState, nextToken, slice, type Token, TokenKind } from "./lexer";
import * as S from "./surface";

export type { Diagnostic, DiagnosticSeverity } from "./diagnostics";

// =============================================================================
// PARSE RESULT
// =============================================================================

export type ParseResult = {
  readonly program: S.SProgram;
  readonly diagnostics: readonly Diagnostic[];
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
  Cons = 11, // ::
  Or = 12, // ||
  And = 14, // &&
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

const span = (start: number, end: number): S.Span => ({ start, end });
const tokenSpan = (token: Token): S.Span => span(token[1], token[2]);
const atNewStatement = (state: ParserState): boolean =>
  state.lexer.atLineStart && !at(state, TokenKind.Bar);

// =============================================================================
// ERROR RECOVERY
// =============================================================================

const synchronize = (state: ParserState): void => {
  while (!at(state, TokenKind.Eof)) {
    if (
      atAny(
        state,
        TokenKind.Let,
        TokenKind.Type,
        TokenKind.If,
        TokenKind.Match,
        TokenKind.End,
        TokenKind.Do,
      )
    ) {
      return;
    }
    advance(state);
  }
};

// =============================================================================
// LAMBDA DETECTION AND PARSING
// =============================================================================

const isLambdaStart = (state: ParserState): boolean => {
  const savedPos = state.lexer.pos;
  const savedCurrent = state.current;

  while (at(state, TokenKind.Lower)) {
    advance(state);
  }

  const isLambda = at(state, TokenKind.Arrow);

  state.lexer.pos = savedPos;
  state.current = savedCurrent;

  return isLambda;
};

const parseLambda = (state: ParserState): S.SAbs => {
  const start = state.current[1];
  const params: string[] = [];

  while (at(state, TokenKind.Lower) && !at(state, TokenKind.Arrow)) {
    const token = advance(state);
    params.push(text(state, token));
  }

  expect(state, TokenKind.Arrow, "expected '->'");
  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];

  return S.sabs(params, body, span(start, end));
};

// =============================================================================
// MAIN PARSE FUNCTION
// =============================================================================

export const parse = (source: string): ParseResult => {
  const state = createParser(source);
  const decls: S.SDecl[] = [];
  let expr: S.SExpr | null = null;

  // Parse module declarations first
  while (at(state, TokenKind.Module)) {
    const mod = parseModuleDecl(state);
    if (mod) decls.push(mod);
  }

  // Parse top-level use statements
  while (at(state, TokenKind.Use)) {
    const use = parseUseDecl(state);
    if (use) decls.push(use);
  }

  // Parse declarations and final expression
  while (!at(state, TokenKind.Eof)) {
    if (at(state, TokenKind.Type)) {
      const decl = parseTypeDecl(state);
      if (decl) decls.push(decl);
    } else if (at(state, TokenKind.Module)) {
      const mod = parseModuleDecl(state);
      if (mod) decls.push(mod);
    } else if (at(state, TokenKind.Use)) {
      const use = parseUseDecl(state);
      if (use) decls.push(use);
    } else if (at(state, TokenKind.Let)) {
      const result = parseLetDeclOrExpr(state);
      if (result.kind === "decl") {
        decls.push(result.decl);
      } else {
        expr = result.expr;
        break;
      }
    } else if (at(state, TokenKind.Foreign)) {
      const decl = parseForeignDecl(state);
      if (decl) decls.push(decl);
    } else {
      expr = parseExpr(state);
      break;
    }
  }

  return {
    program: { decls, expr },
    diagnostics: state.diagnostics,
  };
};

// =============================================================================
// DECLARATIONS
// =============================================================================

const parseModuleDecl = (state: ParserState): S.SDeclModule | null => {
  const start = state.current[1];
  advance(state); // 'module'

  const nameToken = expect(state, TokenKind.Upper, "expected module name");
  if (!nameToken) {
    synchronize(state);
    return null;
  }
  const name = text(state, nameToken);

  const uses: string[] = [];
  const innerDecls: S.SDecl[] = [];

  // Parse use statements within module
  while (at(state, TokenKind.Use)) {
    const use = parseUseDecl(state);
    if (use) uses.push(use.module);
  }

  while (!at(state, TokenKind.End) && !at(state, TokenKind.Eof)) {
    if (at(state, TokenKind.Type)) {
      const decl = parseTypeDecl(state);
      if (decl) innerDecls.push(decl);
    } else if (at(state, TokenKind.Let)) {
      const result = parseLetDeclOrExpr(state);
      if (result.kind === "decl") {
        innerDecls.push(result.decl);
      }
    } else if (at(state, TokenKind.Foreign)) {
      const decl = parseForeignDecl(state);
      if (decl) innerDecls.push(decl);
    } else {
      error(state, "expected declaration in module");
      advance(state);
    }
  }

  const endToken = expect(state, TokenKind.End, "expected 'end' after module body");
  const end = endToken ? endToken[2] : state.current[1];

  return S.sdeclmodule(name, uses, innerDecls, span(start, end));
};

const parseUseDecl = (state: ParserState): S.SDeclUse | null => {
  const start = state.current[1];
  advance(state); // 'use'

  const moduleToken = expect(state, TokenKind.Upper, "expected module name");
  if (!moduleToken) {
    synchronize(state);
    return null;
  }
  const moduleName = text(state, moduleToken);

  // Parse import specs: (bar, baz) or (..)
  // Names can be lowercase (functions) or uppercase (constructors)
  let imports: readonly string[] | "all" = "all";
  if (at(state, TokenKind.LParen)) {
    advance(state); // '('
    if (at(state, TokenKind.DotDot)) {
      advance(state); // '..'
      imports = "all";
    } else {
      const names: string[] = [];
      while (!at(state, TokenKind.RParen) && !at(state, TokenKind.Eof)) {
        // Accept both lowercase (functions) and uppercase (constructors)
        if (at(state, TokenKind.Lower) || at(state, TokenKind.Upper)) {
          names.push(text(state, advance(state)));
        } else {
          error(state, "expected name");
          break;
        }
        if (at(state, TokenKind.Comma)) {
          advance(state);
        } else {
          break;
        }
      }
      imports = names;
    }
    expect(state, TokenKind.RParen, "expected ')' after imports");
  }

  const end = state.current[1];
  return S.sdecluse(moduleName, imports, span(start, end));
};

const parseTypeDecl = (state: ParserState): S.SDeclType | S.SDeclTypeAlias | null => {
  const start = state.current[1];
  advance(state); // 'type'

  const nameToken = expect(state, TokenKind.Upper, "expected type name");
  if (!nameToken) {
    synchronize(state);
    return null;
  }
  const name = text(state, nameToken);

  // Parse type parameters
  const params: string[] = [];
  while (at(state, TokenKind.Lower)) {
    params.push(text(state, advance(state)));
  }

  expect(state, TokenKind.Eq, "expected '=' after type name");

  // Check if this is an ADT or type alias
  // ADT: type Foo = Bar | Baz
  // Alias: type Foo = { x: Int } or type Foo = Int or type Foo = (A, B)
  if (at(state, TokenKind.Upper) && isConstructorDecl(state)) {
    // ADT
    const constructors: S.SConDecl[] = [];
    do {
      if (at(state, TokenKind.Bar)) advance(state);
      const con = parseConstructor(state);
      if (con) constructors.push(con);
    } while (at(state, TokenKind.Bar));

    return S.sdecltype(name, params, constructors, span(start, state.current[1]));
  } else {
    // Type alias
    const aliasType = parseType(state);
    if (!aliasType) {
      error(state, "expected type after '='");
      return null;
    }
    return S.sdecltypealias(name, params, aliasType, span(start, state.current[1]));
  }
};

const isConstructorDecl = (state: ParserState): boolean => {
  // A constructor declaration looks like: Name [fields...] [| ...]
  // A type alias with upper starts like: UpperName followed by type application
  const savedPos = state.lexer.pos;
  const savedCurrent = state.current;

  // Advance past the name
  advance(state);

  // Skip any fields (type atoms)
  while (
    atAny(state, TokenKind.Lower, TokenKind.Upper, TokenKind.LParen, TokenKind.LBrace) &&
    !atNewStatement(state)
  ) {
    if (at(state, TokenKind.LParen)) {
      let depth = 1;
      advance(state);
      while (depth > 0 && !at(state, TokenKind.Eof)) {
        if (at(state, TokenKind.LParen)) depth++;
        else if (at(state, TokenKind.RParen)) depth--;
        advance(state);
      }
    } else {
      advance(state);
    }
  }

  // If we hit |, it's an ADT
  const isADT = at(state, TokenKind.Bar) || at(state, TokenKind.Eof) || atNewStatement(state);

  state.lexer.pos = savedPos;
  state.current = savedCurrent;

  return isADT;
};

const parseConstructor = (state: ParserState): S.SConDecl | null => {
  const nameToken = expect(state, TokenKind.Upper, "expected constructor name");
  if (!nameToken) return null;
  const name = text(state, nameToken);

  const fields: S.SType[] = [];
  while (
    atAny(state, TokenKind.Lower, TokenKind.Upper, TokenKind.LParen, TokenKind.LBrace) &&
    !atNewStatement(state) &&
    !at(state, TokenKind.Bar)
  ) {
    const field = parseTypeAtom(state);
    if (field) fields.push(field);
    else break;
  }

  return { name, fields };
};

const parseForeignDecl = (state: ParserState): S.SDeclForeign | null => {
  const start = state.current[1];
  advance(state); // 'foreign'

  // Check for 'async' modifier
  const isAsync = at(state, TokenKind.Async);
  if (isAsync) {
    advance(state); // 'async'
  }

  let name: string;

  // Support operator syntax: foreign (+) : type
  if (at(state, TokenKind.LParen)) {
    advance(state); // '('
    const opToken = state.current;
    if (isOperatorToken(opToken[0])) {
      advance(state);
      name = text(state, opToken);
    } else {
      error(state, "expected operator in parentheses");
      synchronize(state);
      return null;
    }
    if (!expect(state, TokenKind.RParen, "expected ')' after operator")) {
      synchronize(state);
      return null;
    }
  } else if (at(state, TokenKind.Upper)) {
    // Support qualified names: foreign Module.name : type
    const moduleToken = advance(state);
    const moduleName = text(state, moduleToken);
    if (!expect(state, TokenKind.Dot, "expected '.' after module name")) {
      synchronize(state);
      return null;
    }
    const funcToken = expect(state, TokenKind.Lower, "expected function name after '.'");
    if (!funcToken) {
      synchronize(state);
      return null;
    }
    name = `${moduleName}.${text(state, funcToken)}`;
  } else {
    const nameToken = expect(state, TokenKind.Lower, "expected foreign function name");
    if (!nameToken) {
      synchronize(state);
      return null;
    }
    name = text(state, nameToken);
  }

  if (!expect(state, TokenKind.Colon, "expected ':' after foreign function name")) {
    synchronize(state);
    return null;
  }

  const type = parseType(state);
  if (!type) {
    error(state, "expected type after ':'");
    synchronize(state);
    return null;
  }

  return S.sdeclforeign(name, type, isAsync, span(start, state.current[1]));
};

const isOperatorToken = (kind: TokenKind): boolean =>
  kind === TokenKind.Plus ||
  kind === TokenKind.Minus ||
  kind === TokenKind.Star ||
  kind === TokenKind.Slash ||
  kind === TokenKind.Lt ||
  kind === TokenKind.Le ||
  kind === TokenKind.Gt ||
  kind === TokenKind.Ge ||
  kind === TokenKind.EqEq ||
  kind === TokenKind.Ne;

type LetResult =
  | { kind: "decl"; decl: S.SDeclLet | S.SDeclLetRec }
  | { kind: "expr"; expr: S.SExpr };

/**
 * Parse pattern destructuring in let: let (a, b) = expr in body
 * Desugars to: match expr when (a, b) -> body end
 */
const parseLetPattern = (state: ParserState, start: number, _recursive: boolean): LetResult => {
  // Parse the pattern (must be a tuple or other pattern starting with '(')
  const pattern = parsePattern(state);

  expect(state, TokenKind.Eq, "expected '=' after pattern");

  const value = parseExpr(state);

  // Must have 'in' for pattern destructuring (always an expression, not a declaration)
  if (!at(state, TokenKind.In)) {
    error(state, "expected 'in' after pattern binding value");
    return { kind: "expr", expr: S.sint(0) };
  }
  advance(state); // 'in'

  const body = parseExpr(state);
  const end = body.span?.end ?? state.current[1];

  // Desugar to match expression: match value when pattern -> body end
  const matchExpr = S.smatch(value, [{ pattern, guard: null, body }], span(start, end));
  return { kind: "expr", expr: matchExpr };
};

const parseLetDeclOrExpr = (state: ParserState): LetResult => {
  const start = state.current[1];
  advance(state); // 'let'

  const recursive = at(state, TokenKind.Rec);
  if (recursive) advance(state);

  // Check for pattern destructuring: let (a, b) = ...
  if (at(state, TokenKind.LParen)) {
    return parseLetPattern(state, start, recursive);
  }

  const nameToken = expect(state, TokenKind.Lower, "expected binding name");
  if (!nameToken) {
    synchronize(state);
    return { kind: "expr", expr: S.sint(0) };
  }
  const firstName = text(state, nameToken);

  // Parse parameters
  const params: string[] = [];
  while (at(state, TokenKind.Lower) || at(state, TokenKind.LParen)) {
    if (at(state, TokenKind.LParen)) {
      // Skip typed parameter for now
      advance(state);
      const paramToken = expect(state, TokenKind.Lower, "expected parameter name");
      if (paramToken) params.push(text(state, paramToken));
      if (at(state, TokenKind.Colon)) {
        advance(state);
        parseType(state); // Skip type annotation
      }
      expect(state, TokenKind.RParen, "expected ')'");
    } else {
      params.push(text(state, advance(state)));
    }
  }

  // Skip return type annotation
  if (at(state, TokenKind.Colon)) {
    advance(state);
    parseType(state);
  }

  expect(state, TokenKind.Eq, "expected '=' after parameters");

  const value = parseExpr(state);

  // Wrap in lambda if there are parameters
  const wrappedValue = params.length > 0 ? S.sabs(params, value) : value;

  // Check for 'and' (mutual recursion) or 'in' (let expression)
  if (at(state, TokenKind.AndKw)) {
    // Mutual recursion: let rec f = ... and g = ...
    const bindings: { name: string; value: S.SExpr }[] = [{ name: firstName, value: wrappedValue }];

    while (at(state, TokenKind.AndKw)) {
      advance(state); // 'and'
      const andNameToken = expect(state, TokenKind.Lower, "expected binding name");
      if (!andNameToken) break;
      const andName = text(state, andNameToken);

      const andParams: string[] = [];
      while (at(state, TokenKind.Lower) || at(state, TokenKind.LParen)) {
        if (at(state, TokenKind.LParen)) {
          advance(state);
          const paramToken = expect(state, TokenKind.Lower, "expected parameter name");
          if (paramToken) andParams.push(text(state, paramToken));
          if (at(state, TokenKind.Colon)) {
            advance(state);
            parseType(state);
          }
          expect(state, TokenKind.RParen, "expected ')'");
        } else {
          andParams.push(text(state, advance(state)));
        }
      }

      if (at(state, TokenKind.Colon)) {
        advance(state);
        parseType(state);
      }

      expect(state, TokenKind.Eq, "expected '=' after parameters");
      const andValue = parseExpr(state);
      const wrappedAndValue = andParams.length > 0 ? S.sabs(andParams, andValue) : andValue;
      bindings.push({ name: andName, value: wrappedAndValue });
    }

    // Check for 'in' to make it an expression
    if (at(state, TokenKind.In)) {
      advance(state);
      const body = parseExpr(state);
      return {
        kind: "expr",
        expr: S.sletrec(bindings, body, span(start, body.span?.end ?? state.current[1])),
      };
    }

    return { kind: "decl", decl: S.sdeclletrec(bindings, span(start, state.current[1])) };
  }

  if (at(state, TokenKind.In)) {
    advance(state);
    const body = parseExpr(state);
    if (recursive) {
      return {
        kind: "expr",
        expr: S.sletrec(
          [{ name: firstName, value: wrappedValue }],
          body,
          span(start, body.span?.end ?? state.current[1]),
        ),
      };
    }
    return {
      kind: "expr",
      expr: S.slet(firstName, wrappedValue, body, span(start, body.span?.end ?? state.current[1])),
    };
  }

  // Top-level declaration
  if (recursive) {
    return {
      kind: "decl",
      decl: S.sdeclletrec(
        [{ name: firstName, value: wrappedValue }],
        span(start, state.current[1]),
      ),
    };
  }
  return { kind: "decl", decl: S.sdecllet(firstName, wrappedValue, span(start, state.current[1])) };
};

// =============================================================================
// TYPES
// =============================================================================

const parseType = (state: ParserState): S.SType | null => {
  const left = parseTypeApp(state);
  if (!left) return null;

  if (at(state, TokenKind.Arrow)) {
    advance(state);
    const right = parseType(state);
    if (!right) {
      error(state, "expected type after '->'");
      return left;
    }
    return S.stfun(left, right);
  }

  return left;
};

const parseTypeApp = (state: ParserState): S.SType | null => {
  let left = parseTypeAtom(state);
  if (!left) return null;

  while (
    atAny(state, TokenKind.Lower, TokenKind.Upper, TokenKind.LParen, TokenKind.LBrace) &&
    !atNewStatement(state)
  ) {
    const arg = parseTypeAtom(state);
    if (!arg) break;
    left = S.stapp(left, arg);
  }

  return left;
};

// Built-in type names that are lowercase but should be type constructors
const BUILTIN_TYPE_NAMES = new Set(["int", "float", "string", "char", "bool", "unit"]);

const parseTypeAtom = (state: ParserState): S.SType | null => {
  if (at(state, TokenKind.Lower)) {
    const token = advance(state);
    const name = text(state, token);
    // Built-in types are constructors, not variables
    if (BUILTIN_TYPE_NAMES.has(name)) {
      return S.stcon(name, tokenSpan(token));
    }
    return S.stvar(name, tokenSpan(token));
  }

  if (at(state, TokenKind.Upper)) {
    const token = advance(state);
    return S.stcon(text(state, token), tokenSpan(token));
  }

  if (at(state, TokenKind.LParen)) {
    advance(state);
    const first = parseType(state);
    if (!first) {
      expect(state, TokenKind.RParen, "expected ')'");
      return null;
    }

    if (at(state, TokenKind.Comma)) {
      const elements: S.SType[] = [first];
      while (at(state, TokenKind.Comma)) {
        advance(state);
        const elem = parseType(state);
        if (elem) elements.push(elem);
      }
      expect(state, TokenKind.RParen, "expected ')'");
      return S.sttuple(elements);
    }

    expect(state, TokenKind.RParen, "expected ')'");
    return first;
  }

  if (at(state, TokenKind.LBrace)) {
    advance(state);
    const fields: { name: string; type: S.SType }[] = [];

    if (!at(state, TokenKind.RBrace)) {
      const firstName = expect(state, TokenKind.Lower, "expected field name");
      if (firstName) {
        expect(state, TokenKind.Colon, "expected ':'");
        const firstType = parseType(state);
        if (firstType) fields.push({ name: text(state, firstName), type: firstType });

        while (at(state, TokenKind.Comma)) {
          advance(state);
          const fieldName = expect(state, TokenKind.Lower, "expected field name");
          if (fieldName) {
            expect(state, TokenKind.Colon, "expected ':'");
            const fieldType = parseType(state);
            if (fieldType) fields.push({ name: text(state, fieldName), type: fieldType });
          }
        }
      }
    }

    expect(state, TokenKind.RBrace, "expected '}'");
    return S.strecord(fields);
  }

  return null;
};

// =============================================================================
// EXPRESSIONS (PRATT PARSER)
// =============================================================================

const parseExpr = (state: ParserState): S.SExpr => parsePrecedence(state, Bp.None, true);
const parseExprNoLambda = (state: ParserState): S.SExpr => parsePrecedence(state, Bp.None, false);

const parsePrecedence = (state: ParserState, minBp: number, allowLambda: boolean): S.SExpr => {
  let left = allowLambda ? parsePrefix(state) : parsePrefixNoLambda(state);

  while (true) {
    const bp = infixBindingPower(state);
    if (bp <= minBp) break;
    left = parseInfix(state, left, bp, allowLambda);
  }

  return left;
};

const parsePrefixImpl = (state: ParserState, allowLambda: boolean): S.SExpr => {
  const token = state.current;
  const kind = token[0];
  const start = token[1];

  switch (kind) {
    case TokenKind.Int: {
      advance(state);
      return S.sint(parseInt(text(state, token), 10), tokenSpan(token));
    }

    case TokenKind.Float: {
      advance(state);
      return S.sfloat(parseFloat(text(state, token)), tokenSpan(token));
    }

    case TokenKind.String: {
      advance(state);
      return S.sstring(parseStringContent(text(state, token)), tokenSpan(token));
    }

    case TokenKind.Char: {
      advance(state);
      return S.schar(parseCharContent(text(state, token)), tokenSpan(token));
    }

    case TokenKind.True:
      advance(state);
      return S.sbool(true, tokenSpan(token));

    case TokenKind.False:
      advance(state);
      return S.sbool(false, tokenSpan(token));

    case TokenKind.Lower: {
      if (allowLambda && isLambdaStart(state)) {
        return parseLambda(state);
      }
      advance(state);
      return S.svar(text(state, token), tokenSpan(token));
    }

    case TokenKind.Upper: {
      advance(state);
      return S.scon(text(state, token), tokenSpan(token));
    }

    case TokenKind.LParen:
      return parseParenOrTuple(state);

    case TokenKind.Minus: {
      advance(state);
      const operand = parsePrecedence(state, Bp.Multiplicative + 1, false);
      const end = operand.span?.end ?? state.current[1];
      // Unary negation: -x is SBinOp("-", 0, x)
      return S.sbinop("-", S.sint(0, span(start, start)), operand, span(start, end));
    }

    case TokenKind.LBrace:
      return parseRecord(state);

    case TokenKind.LBracket:
      return parseListLiteral(state);

    case TokenKind.If:
      return parseIf(state);

    case TokenKind.Match:
      return parseMatch(state);

    case TokenKind.Do:
      return parseDo(state);

    case TokenKind.Let:
      return parseLetExpr(state);

    default: {
      error(state, `unexpected token: ${TokenKind[kind]}`);
      advance(state);
      return S.sint(0);
    }
  }
};

const parsePrefix = (state: ParserState): S.SExpr => parsePrefixImpl(state, true);
const parsePrefixNoLambda = (state: ParserState): S.SExpr => parsePrefixImpl(state, false);

const parseInfix = (
  state: ParserState,
  left: S.SExpr,
  bp: number,
  allowLambda: boolean,
): S.SExpr => {
  const kind = state.current[0];
  const start = left.span?.start ?? 0;

  switch (kind) {
    case TokenKind.Plus:
    case TokenKind.Minus:
    case TokenKind.Star:
    case TokenKind.Slash:
    case TokenKind.Lt:
    case TokenKind.Le:
    case TokenKind.Gt:
    case TokenKind.Ge:
    case TokenKind.EqEq:
    case TokenKind.Ne:
    case TokenKind.And:
    case TokenKind.Or: {
      const opToken = advance(state);
      const op = text(state, opToken);
      const right = parsePrecedence(state, bp, allowLambda);
      const end = right.span?.end ?? state.current[1];
      return S.sbinop(op, left, right, span(start, end));
    }

    case TokenKind.Pipe: {
      advance(state);
      const right = parsePrecedence(state, bp, allowLambda);
      const end = right.span?.end ?? state.current[1];
      return S.spipe(left, right, span(start, end));
    }

    case TokenKind.ColonColon: {
      advance(state);
      // Right-associative
      const right = parsePrecedence(state, bp - 1, allowLambda);
      const end = right.span?.end ?? state.current[1];
      return S.scons(left, right, span(start, end));
    }

    case TokenKind.Dot: {
      advance(state);

      // Tuple indexing: tuple.0
      if (at(state, TokenKind.Int)) {
        const indexToken = advance(state);
        const field = text(state, indexToken);
        return S.sfield(left, field, span(start, indexToken[2]));
      }

      // Record field access: record.field OR qualified constructor: Module.Constructor
      let fieldToken: Token | null;
      if (at(state, TokenKind.Lower)) {
        fieldToken = advance(state);
      } else if (at(state, TokenKind.Upper)) {
        // Allow uppercase for qualified constructors (Module.Constructor)
        fieldToken = advance(state);
      } else {
        error(state, "expected field name after '.'");
        fieldToken = null;
      }
      const field = fieldToken ? text(state, fieldToken) : "?";
      const end = fieldToken ? fieldToken[2] : state.current[1];
      return S.sfield(left, field, span(start, end));
    }

    default: {
      // Function application
      const right = parsePrecedence(state, bp, false);
      const end = right.span?.end ?? state.current[1];
      return S.sapp(left, right, span(start, end));
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
    case TokenKind.Or:
      return Bp.Or;
    case TokenKind.And:
      return Bp.And;
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
    default:
      // Application - but not across new statements (newlines at start of line)
      if (
        !atNewStatement(state) &&
        atAny(
          state,
          TokenKind.Int,
          TokenKind.Float,
          TokenKind.String,
          TokenKind.Char,
          TokenKind.True,
          TokenKind.False,
          TokenKind.Lower,
          TokenKind.Upper,
          TokenKind.LParen,
          TokenKind.LBrace,
          TokenKind.LBracket,
        )
      ) {
        return Bp.Application;
      }
      return Bp.None;
  }
};

// =============================================================================
// EXPRESSION HELPERS
// =============================================================================

const parseParenOrTuple = (state: ParserState): S.SExpr => {
  const start = state.current[1];
  advance(state); // (

  if (at(state, TokenKind.RParen)) {
    // Unit: ()
    const end = state.current[2];
    advance(state);
    return S.stuple([], span(start, end));
  }

  const first = parseExpr(state);

  if (at(state, TokenKind.Comma)) {
    // Tuple: (a, b, c)
    const elements: S.SExpr[] = [first];
    while (at(state, TokenKind.Comma)) {
      advance(state);
      elements.push(parseExpr(state));
    }
    const endToken = expect(state, TokenKind.RParen, "expected ')'");
    const end = endToken ? endToken[2] : state.current[1];
    return S.stuple(elements, span(start, end));
  }

  // Check for type annotation: (e : T)
  if (at(state, TokenKind.Colon)) {
    advance(state);
    const type = parseType(state);
    const endToken = expect(state, TokenKind.RParen, "expected ')'");
    const end = endToken ? endToken[2] : state.current[1];
    if (type) {
      return S.sannot(first, type, span(start, end));
    }
  }

  expect(state, TokenKind.RParen, "expected ')'");
  return first;
};

const parseRecord = (state: ParserState): S.SExpr => {
  const start = state.current[1];
  advance(state); // {

  if (at(state, TokenKind.RBrace)) {
    const end = state.current[2];
    advance(state);
    return S.srecord([], span(start, end));
  }

  // Check for record update: { r | x = 1 }
  const firstToken = state.current;
  if (at(state, TokenKind.Lower)) {
    const firstName = text(state, advance(state));

    if (at(state, TokenKind.Bar)) {
      // Record update
      advance(state);
      const fields: { name: string; value: S.SExpr }[] = [];

      do {
        if (at(state, TokenKind.Comma)) advance(state);
        const fieldName = expect(state, TokenKind.Lower, "expected field name");
        if (!fieldName) break;
        expect(state, TokenKind.Eq, "expected '='");
        const fieldValue = parseExpr(state);
        fields.push({ name: text(state, fieldName), value: fieldValue });
      } while (at(state, TokenKind.Comma));

      const endToken = expect(state, TokenKind.RBrace, "expected '}'");
      const end = endToken ? endToken[2] : state.current[1];
      return S.srecordUpdate(S.svar(firstName, tokenSpan(firstToken)), fields, span(start, end));
    }

    // Regular record starting with a field
    const fields: { name: string; value: S.SExpr }[] = [];

    if (at(state, TokenKind.Eq)) {
      advance(state);
      const firstValue = parseExpr(state);
      fields.push({ name: firstName, value: firstValue });
    } else {
      // Punning: { x } means { x = x }
      fields.push({ name: firstName, value: S.svar(firstName, tokenSpan(firstToken)) });
    }

    while (at(state, TokenKind.Comma)) {
      advance(state);
      const fieldName = expect(state, TokenKind.Lower, "expected field name");
      if (!fieldName) break;

      if (at(state, TokenKind.Eq)) {
        advance(state);
        const fieldValue = parseExpr(state);
        fields.push({ name: text(state, fieldName), value: fieldValue });
      } else {
        // Punning
        fields.push({
          name: text(state, fieldName),
          value: S.svar(text(state, fieldName), tokenSpan(fieldName)),
        });
      }
    }

    const endToken = expect(state, TokenKind.RBrace, "expected '}'");
    const end = endToken ? endToken[2] : state.current[1];
    return S.srecord(fields, span(start, end));
  }

  error(state, "expected field name in record");
  return S.srecord([], span(start, state.current[1]));
};

const parseListLiteral = (state: ParserState): S.SList => {
  const start = state.current[1];
  advance(state); // [

  const elements: S.SExpr[] = [];

  if (!at(state, TokenKind.RBracket)) {
    do {
      if (at(state, TokenKind.Comma)) advance(state);
      elements.push(parseExpr(state));
    } while (at(state, TokenKind.Comma));
  }

  const endToken = expect(state, TokenKind.RBracket, "expected ']'");
  const end = endToken ? endToken[2] : state.current[1];

  return S.slist(elements, span(start, end));
};

const parseIf = (state: ParserState): S.SIf => {
  const start = state.current[1];
  advance(state); // if

  const cond = parseExpr(state);
  expect(state, TokenKind.Then, "expected 'then'");
  const thenBranch = parseExpr(state);
  expect(state, TokenKind.Else, "expected 'else'");
  const elseBranch = parseExpr(state);
  const end = elseBranch.span?.end ?? state.current[1];

  return S.sif(cond, thenBranch, elseBranch, span(start, end));
};

const parseMatch = (state: ParserState): S.SMatch => {
  const start = state.current[1];
  advance(state); // match

  const scrutinee = parseExpr(state);
  const cases: S.SCase[] = [];

  while (at(state, TokenKind.When)) {
    advance(state); // when
    let pattern = parsePattern(state);

    // Or-pattern: when p1 | p2 -> ...
    while (at(state, TokenKind.Bar)) {
      advance(state);
      const right = parsePattern(state);
      pattern = S.spor(pattern, right);
    }

    // Guard: when pattern if condition -> body
    let guard: S.SExpr | null = null;
    if (at(state, TokenKind.If)) {
      advance(state);
      guard = parseExprNoLambda(state);
    }

    expect(state, TokenKind.Arrow, "expected '->'");
    const body = parseExpr(state);
    cases.push({ pattern, guard, body });
  }

  const endToken = expect(state, TokenKind.End, "expected 'end'");
  const end = endToken ? endToken[2] : state.current[1];

  return S.smatch(scrutinee, cases, span(start, end));
};

const parseDo = (state: ParserState): S.SDo => {
  const start = state.current[1];
  advance(state); // do

  const stmts: S.SDoStmt[] = [];

  while (!at(state, TokenKind.End) && !at(state, TokenKind.Eof)) {
    if (at(state, TokenKind.Let)) {
      advance(state);
      const pattern = parsePattern(state);
      expect(state, TokenKind.Eq, "expected '='");
      const value = parseExpr(state);
      stmts.push({ kind: "DoLet", pattern, expr: value });
    } else if (
      atAny(
        state,
        TokenKind.Underscore,
        TokenKind.Lower,
        TokenKind.Upper,
        TokenKind.LParen,
        TokenKind.LBrace,
      )
    ) {
      // Try pattern <- expr
      const savedPos = state.lexer.pos;
      const savedCurrent = state.current;
      const savedDiagCount = state.diagnostics.length;

      const pattern = parsePattern(state);

      if (at(state, TokenKind.LeftArrow)) {
        advance(state);
        const bindExpr = parseExpr(state);
        stmts.push({ kind: "DoBindPattern", pattern, expr: bindExpr });
      } else {
        // Backtrack and parse as expression
        state.lexer.pos = savedPos;
        state.current = savedCurrent;
        state.diagnostics.length = savedDiagCount;
        const expr = parseExpr(state);
        stmts.push({ kind: "DoExpr", expr });
      }
    } else {
      const expr = parseExpr(state);
      stmts.push({ kind: "DoExpr", expr });
    }
  }

  const endToken = expect(state, TokenKind.End, "expected 'end'");
  const end = endToken ? endToken[2] : state.current[1];

  return S.sdo(stmts, span(start, end));
};

const parseLetExpr = (state: ParserState): S.SExpr => {
  const result = parseLetDeclOrExpr(state);
  if (result.kind === "expr") {
    return result.expr;
  }
  // Should not happen in expression context, but handle gracefully
  error(state, "expected 'in' after let binding");
  return S.sint(0);
};

// =============================================================================
// PATTERNS
// =============================================================================

const parsePattern = (state: ParserState): S.SPattern => {
  let left = parsePatternAtom(state);

  // Cons pattern: p :: ps
  if (at(state, TokenKind.ColonColon)) {
    advance(state);
    const right = parsePattern(state);
    return S.spcons(left, right);
  }

  // As pattern: pattern as name
  if (at(state, TokenKind.As)) {
    advance(state);
    const nameToken = expect(state, TokenKind.Lower, "expected name after 'as'");
    if (nameToken) {
      return S.spas(text(state, nameToken), left, span(left.span?.start ?? 0, nameToken[2]));
    }
  }

  return left;
};

/**
 * Parse a simple pattern atom - used for constructor arguments.
 * Uppercase identifiers are parsed as nullary constructors (no arguments).
 * Use parentheses to group constructor patterns with arguments.
 */
const parseSimplePatternAtom = (state: ParserState): S.SPattern => {
  const token = state.current;
  const kind = token[0];

  // For uppercase identifiers, return nullary constructor (no greedy arg collection)
  if (kind === TokenKind.Upper) {
    advance(state);
    return S.spcon(text(state, token), [], tokenSpan(token));
  }

  // For everything else, use the full parsePatternAtom
  return parsePatternAtom(state);
};

const parsePatternAtom = (state: ParserState): S.SPattern => {
  const token = state.current;
  const kind = token[0];

  switch (kind) {
    case TokenKind.Underscore:
      advance(state);
      return S.spwild(tokenSpan(token));

    case TokenKind.Lower:
      advance(state);
      return S.spvar(text(state, token), tokenSpan(token));

    case TokenKind.Upper: {
      advance(state);
      const name = text(state, token);
      const args: S.SPattern[] = [];

      // Collect arguments using simple patterns - nested constructors are nullary
      // unless parenthesized
      while (
        atAny(
          state,
          TokenKind.Underscore,
          TokenKind.Lower,
          TokenKind.Upper,
          TokenKind.Int,
          TokenKind.String,
          TokenKind.Char,
          TokenKind.True,
          TokenKind.False,
          TokenKind.LParen,
          TokenKind.LBrace,
          TokenKind.LBracket,
        ) &&
        !atNewStatement(state)
      ) {
        args.push(parseSimplePatternAtom(state));
      }

      return S.spcon(name, args, span(token[1], state.current[1]));
    }

    case TokenKind.Int:
      advance(state);
      return S.split({ kind: "int", value: parseInt(text(state, token), 10) }, tokenSpan(token));

    case TokenKind.Float:
      advance(state);
      return S.split({ kind: "float", value: parseFloat(text(state, token)) }, tokenSpan(token));

    case TokenKind.String:
      advance(state);
      return S.split(
        { kind: "string", value: parseStringContent(text(state, token)) },
        tokenSpan(token),
      );

    case TokenKind.Char:
      advance(state);
      return S.split(
        { kind: "char", value: parseCharContent(text(state, token)) },
        tokenSpan(token),
      );

    case TokenKind.True:
      advance(state);
      return S.split({ kind: "bool", value: true }, tokenSpan(token));

    case TokenKind.False:
      advance(state);
      return S.split({ kind: "bool", value: false }, tokenSpan(token));

    case TokenKind.LParen: {
      const start = token[1];
      advance(state);

      if (at(state, TokenKind.RParen)) {
        const end = state.current[2];
        advance(state);
        return S.sptuple([], span(start, end));
      }

      const first = parsePattern(state);

      if (at(state, TokenKind.Comma)) {
        const elements: S.SPattern[] = [first];
        while (at(state, TokenKind.Comma)) {
          advance(state);
          elements.push(parsePattern(state));
        }
        const endToken = expect(state, TokenKind.RParen, "expected ')'");
        return S.sptuple(elements, span(start, endToken ? endToken[2] : state.current[1]));
      }

      expect(state, TokenKind.RParen, "expected ')'");
      return first;
    }

    case TokenKind.LBrace: {
      const start = token[1];
      advance(state);

      const fields: { name: string; pattern: S.SPattern }[] = [];

      if (!at(state, TokenKind.RBrace)) {
        do {
          if (at(state, TokenKind.Comma)) advance(state);
          const fieldName = expect(state, TokenKind.Lower, "expected field name");
          if (!fieldName) break;
          const name = text(state, fieldName);

          if (at(state, TokenKind.Eq)) {
            advance(state);
            const pattern = parsePattern(state);
            fields.push({ name, pattern });
          } else {
            // Punning: { x } means { x = x }
            fields.push({ name, pattern: S.spvar(name, tokenSpan(fieldName)) });
          }
        } while (at(state, TokenKind.Comma));
      }

      const endToken = expect(state, TokenKind.RBrace, "expected '}'");
      return S.sprecord(fields, span(start, endToken ? endToken[2] : state.current[1]));
    }

    case TokenKind.LBracket: {
      const start = token[1];
      advance(state);

      const elements: S.SPattern[] = [];

      if (!at(state, TokenKind.RBracket)) {
        do {
          if (at(state, TokenKind.Comma)) advance(state);
          elements.push(parsePattern(state));
        } while (at(state, TokenKind.Comma));
      }

      const endToken = expect(state, TokenKind.RBracket, "expected ']'");
      return S.splist(elements, span(start, endToken ? endToken[2] : state.current[1]));
    }

    default:
      error(state, `unexpected token in pattern: ${TokenKind[kind]}`);
      advance(state);
      return S.spwild();
  }
};

// =============================================================================
// STRING/CHAR PARSING HELPERS
// =============================================================================

const parseStringContent = (raw: string): string => {
  // Remove quotes and handle escapes with single-pass processing
  const content = raw.slice(1, -1);
  let result = "";
  let i = 0;
  while (i < content.length) {
    if (content[i] === "\\" && i + 1 < content.length) {
      switch (content[i + 1]) {
        case "n":
          result += "\n";
          break;
        case "t":
          result += "\t";
          break;
        case "r":
          result += "\r";
          break;
        case '"':
          result += '"';
          break;
        case "\\":
          result += "\\";
          break;
        default:
          result += content[i + 1];
          break;
      }
      i += 2;
    } else {
      result += content[i];
      i++;
    }
  }
  return result;
};

const parseCharContent = (raw: string): string => {
  const content = raw.slice(1, -1);
  if (content.startsWith("\\")) {
    switch (content[1]) {
      case "n":
        return "\n";
      case "t":
        return "\t";
      case "r":
        return "\r";
      case "'":
        return "'";
      case "\\":
        return "\\";
      default:
        return content[1] ?? "";
    }
  }
  return content;
};
