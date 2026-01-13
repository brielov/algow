/**
 * Algow Code Formatter
 *
 * Opinionated formatter similar to gofmt - no configuration.
 * Parses source, pretty-prints AST, and preserves comments.
 */

import { TokenKind, createLexer, nextTokenWithComments, slice, getLineNumber } from "./lexer";
import { parse } from "./parser";
import type { Comment, SProgram, SDecl, SExpr, SPattern, SType, SCase, SConDecl } from "./surface";
import type { Diagnostic } from "./diagnostics";

// =============================================================================
// Types
// =============================================================================

export type FormatResult = {
  readonly formatted: string;
  readonly diagnostics: readonly Diagnostic[];
};

// =============================================================================
// Comment Collection
// =============================================================================

type CommentInfo = Comment & { line: number };

const collectComments = (source: string): CommentInfo[] => {
  const comments: CommentInfo[] = [];
  const lexer = createLexer(source);

  while (true) {
    const { token, leadingComments } = nextTokenWithComments(lexer);

    for (const comment of leadingComments) {
      const [kind, start, end] = comment;
      comments.push({
        kind: kind === TokenKind.LineComment ? "line" : "block",
        text: slice(lexer, start, end),
        span: { start, end },
        line: getLineNumber(source, start),
      });
    }

    if (token[0] === TokenKind.Eof) break;
  }

  return comments;
};

// =============================================================================
// Precedence (for parenthesization)
// =============================================================================

const enum Prec {
  Lowest = 0,
  Pipe = 10,
  Cons = 11,
  Or = 12,
  And = 14,
  Equality = 20,
  Comparison = 30,
  Additive = 40,
  Multiplicative = 50,
  Application = 60,
  Atom = 100,
}

const binopPrec = (op: string): Prec => {
  switch (op) {
    case "||":
      return Prec.Or;
    case "&&":
      return Prec.And;
    case "==":
    case "!=":
      return Prec.Equality;
    case "<":
    case "<=":
    case ">":
    case ">=":
      return Prec.Comparison;
    case "+":
    case "-":
      return Prec.Additive;
    case "*":
    case "/":
      return Prec.Multiplicative;
    default:
      return Prec.Lowest;
  }
};

// =============================================================================
// Formatter Context
// =============================================================================

type FormatContext = {
  readonly source: string;
  readonly comments: CommentInfo[];
  commentIndex: number;
  currentLine: number;
};

const createContext = (source: string, comments: CommentInfo[]): FormatContext => ({
  source,
  comments,
  commentIndex: 0,
  currentLine: 0,
});

// =============================================================================
// Comment Emission
// =============================================================================

/**
 * Get comments that should appear before a given source position.
 */
const getCommentsBefore = (ctx: FormatContext, pos: number): string[] => {
  const result: string[] = [];

  while (ctx.commentIndex < ctx.comments.length) {
    const comment = ctx.comments[ctx.commentIndex]!;
    if (comment.span.start >= pos) break;
    result.push(comment.text);
    ctx.commentIndex++;
  }

  return result;
};

/**
 * Get any remaining comments.
 */
const getRemainingComments = (ctx: FormatContext): string[] => {
  const result: string[] = [];

  while (ctx.commentIndex < ctx.comments.length) {
    result.push(ctx.comments[ctx.commentIndex]!.text);
    ctx.commentIndex++;
  }

  return result;
};

// =============================================================================
// Pretty Printers
// =============================================================================

const formatProgram = (ctx: FormatContext, program: SProgram): string => {
  const lines: string[] = [];

  for (const decl of program.decls) {
    if (decl.span) {
      const comments = getCommentsBefore(ctx, decl.span.start);
      for (const c of comments) {
        lines.push(c);
      }
    }
    lines.push(formatDecl(ctx, decl, 0));
    lines.push("");
  }

  if (program.expr) {
    if (program.expr.span) {
      const comments = getCommentsBefore(ctx, program.expr.span.start);
      for (const c of comments) {
        lines.push(c);
      }
    }
    lines.push(formatExpr(ctx, program.expr, 0, Prec.Lowest));
  }

  // Remaining comments
  const remaining = getRemainingComments(ctx);
  for (const c of remaining) {
    lines.push(c);
  }

  // Clean up trailing blank lines
  while (lines.length > 0 && lines[lines.length - 1] === "") {
    lines.pop();
  }

  return lines.join("\n") + "\n";
};

const ind = (level: number): string => "  ".repeat(level);

/**
 * Indent all lines of a multi-line string.
 */
const indentLines = (text: string, level: number): string => {
  if (level === 0) return text;
  const prefix = ind(level);
  return text
    .split("\n")
    .map((line, i) => (i === 0 ? line : prefix + line))
    .join("\n");
};

const formatDecl = (ctx: FormatContext, decl: SDecl, level: number): string => {
  switch (decl.kind) {
    case "SDeclType": {
      const params = decl.params.length > 0 ? " " + decl.params.join(" ") : "";
      const cons = decl.constructors.map(formatConDecl).join(" | ");
      return `${ind(level)}type ${decl.name}${params} = ${cons}`;
    }

    case "SDeclTypeAlias": {
      const params = decl.params.length > 0 ? " " + decl.params.join(" ") : "";
      return `${ind(level)}type ${decl.name}${params} = ${formatType(decl.type)}`;
    }

    case "SDeclLet": {
      const value = formatExpr(ctx, decl.value, 0, Prec.Lowest);
      const indentedValue = indentLines(value, level + 1);
      return `${ind(level)}let ${decl.name} =\n${ind(level + 1)}${indentedValue}`;
    }

    case "SDeclLetRec": {
      const [first, ...rest] = decl.bindings;
      if (!first) return "";
      const lines = [
        `${ind(level)}let rec ${first.name} = ${formatExpr(ctx, first.value, level, Prec.Lowest)}`,
      ];
      for (const b of rest) {
        lines.push(`${ind(level)}and ${b.name} = ${formatExpr(ctx, b.value, level, Prec.Lowest)}`);
      }
      return lines.join("\n");
    }

    case "SDeclForeign":
      return `${ind(level)}foreign ${decl.name} : ${formatType(decl.type)}`;

    case "SDeclModule": {
      const lines = [`${ind(level)}module ${decl.name}`];
      for (const use of decl.uses) {
        lines.push(`${ind(level + 1)}use ${use}`);
      }
      for (const d of decl.decls) {
        lines.push(formatDecl(ctx, d, level + 1));
      }
      lines.push(`${ind(level)}end`);
      return lines.join("\n");
    }

    case "SDeclUse": {
      const imports = decl.imports === "all" ? "(..)" : `(${decl.imports.join(", ")})`;
      return `${ind(level)}use ${decl.module} ${imports}`;
    }
  }
};

const formatConDecl = (con: SConDecl): string => {
  if (con.fields.length === 0) return con.name;
  const fields = con.fields.map(formatTypeAtom).join(" ");
  return `${con.name} ${fields}`;
};

// =============================================================================
// Expression Formatting
// =============================================================================

/**
 * Get comments before a position and format them with proper indentation.
 */
const emitCommentsBefore = (ctx: FormatContext, pos: number | undefined, level: number): string => {
  if (pos === undefined) return "";
  const comments = getCommentsBefore(ctx, pos);
  if (comments.length === 0) return "";
  return comments.map((c) => `${ind(level)}${c}\n`).join("");
};

const formatExpr = (ctx: FormatContext, expr: SExpr, level: number, outerPrec: Prec): string => {
  const result = formatExprInner(ctx, expr, level);
  const innerPrec = exprPrec(expr);
  return innerPrec < outerPrec ? `(${result})` : result;
};

const exprPrec = (expr: SExpr): Prec => {
  switch (expr.kind) {
    case "SLet":
    case "SLetRec":
    case "SIf":
    case "SMatch":
    case "SDo":
    case "SAbs":
    case "SAnnot":
      return Prec.Lowest;
    case "SPipe":
      return Prec.Pipe;
    case "SCons":
      return Prec.Cons;
    case "SBinOp":
      return binopPrec(expr.op);
    case "SApp":
      return Prec.Application;
    default:
      return Prec.Atom;
  }
};

/**
 * Check if an expression should be formatted on multiple lines.
 */
const isMultilineExpr = (expr: SExpr): boolean => {
  switch (expr.kind) {
    case "SMatch":
    case "SDo":
      return true;
    default:
      return false;
  }
};

const formatExprInner = (ctx: FormatContext, expr: SExpr, level: number): string => {
  switch (expr.kind) {
    case "SVar":
      return expr.name;

    case "SLit":
      return formatLiteral(expr.value);

    case "SCon":
      return expr.name;

    case "SApp":
      return formatApp(ctx, expr, level);

    case "SAbs": {
      const params = expr.params.map((p) => p.name).join(" ");
      // Put complex bodies on new line
      if (isMultilineExpr(expr.body)) {
        const body = formatExpr(ctx, expr.body, level + 1, Prec.Lowest);
        return `${params} ->\n${ind(level + 1)}${body}`;
      }
      const body = formatExpr(ctx, expr.body, level, Prec.Lowest);
      return `${params} -> ${body}`;
    }

    case "SLet": {
      // Emit comments before the let binding's body
      const bodyComments = emitCommentsBefore(ctx, expr.body.span?.start, level);
      const value = formatExpr(ctx, expr.value, level, Prec.Lowest);
      const body = formatExpr(ctx, expr.body, level, Prec.Lowest);
      return `let ${expr.name} = ${value} in\n${bodyComments}${ind(level)}${body}`;
    }

    case "SLetRec": {
      const [first, ...rest] = expr.bindings;
      if (!first) return formatExpr(ctx, expr.body, level, Prec.Lowest);
      let result = `let rec ${first.name} = ${formatExpr(ctx, first.value, level, Prec.Lowest)}`;
      for (const b of rest) {
        result += `\n${ind(level)}and ${b.name} = ${formatExpr(ctx, b.value, level, Prec.Lowest)}`;
      }
      const bodyComments = emitCommentsBefore(ctx, expr.body.span?.start, level);
      result += `\n${ind(level)}in\n${bodyComments}${ind(level)}${formatExpr(ctx, expr.body, level, Prec.Lowest)}`;
      return result;
    }

    case "SIf": {
      const cond = formatExpr(ctx, expr.cond, level, Prec.Lowest);
      const thenBr = formatExpr(ctx, expr.thenBranch, level, Prec.Lowest);
      const elseBr = formatExpr(ctx, expr.elseBranch, level, Prec.Lowest);
      return `if ${cond} then ${thenBr} else ${elseBr}`;
    }

    case "SMatch": {
      const scrut = formatExpr(ctx, expr.scrutinee, level, Prec.Lowest);
      const cases = expr.cases.map((c) => formatCase(ctx, c, level + 1)).join("\n");
      return `match ${scrut}\n${cases}\n${ind(level)}end`;
    }

    case "STuple":
      return `(${expr.elements.map((e) => formatExpr(ctx, e, level, Prec.Lowest)).join(", ")})`;

    case "SRecord": {
      if (expr.fields.length === 0) return "{}";
      const fields = expr.fields
        .map((f) => `${f.name} = ${formatExpr(ctx, f.value, level, Prec.Lowest)}`)
        .join(", ");
      return `{ ${fields} }`;
    }

    case "SRecordUpdate": {
      const rec = formatExpr(ctx, expr.record, level, Prec.Lowest);
      const fields = expr.fields
        .map((f) => `${f.name} = ${formatExpr(ctx, f.value, level, Prec.Lowest)}`)
        .join(", ");
      return `{ ${rec} | ${fields} }`;
    }

    case "SField":
      return `${formatExpr(ctx, expr.record, level, Prec.Atom)}.${expr.field}`;

    case "SList":
      if (expr.elements.length === 0) return "[]";
      return `[${expr.elements.map((e) => formatExpr(ctx, e, level, Prec.Lowest)).join(", ")}]`;

    case "SPipe":
      return `${formatExpr(ctx, expr.left, level, Prec.Pipe)} |> ${formatExpr(ctx, expr.right, level, Prec.Pipe + 1)}`;

    case "SCons":
      return `${formatExpr(ctx, expr.head, level, Prec.Cons + 1)} :: ${formatExpr(ctx, expr.tail, level, Prec.Cons)}`;

    case "SBinOp": {
      const prec = binopPrec(expr.op);
      return `${formatExpr(ctx, expr.left, level, prec)} ${expr.op} ${formatExpr(ctx, expr.right, level, prec + 1)}`;
    }

    case "SDo": {
      const stmts = expr.stmts
        .map((s) => {
          switch (s.kind) {
            case "DoBindPattern":
              return `${ind(level + 1)}${formatPattern(s.pattern)} <- ${formatExpr(ctx, s.expr, level + 1, Prec.Lowest)}`;
            case "DoLet":
              return `${ind(level + 1)}let ${formatPattern(s.pattern)} = ${formatExpr(ctx, s.expr, level + 1, Prec.Lowest)}`;
            case "DoExpr":
              return `${ind(level + 1)}${formatExpr(ctx, s.expr, level + 1, Prec.Lowest)}`;
          }
        })
        .join("\n");
      return `do\n${stmts}\n${ind(level)}end`;
    }

    case "SAnnot":
      return `(${formatExpr(ctx, expr.expr, level, Prec.Lowest)} : ${formatType(expr.type)})`;
  }
};

const formatLiteral = (lit: {
  readonly kind: "int" | "float" | "string" | "char" | "bool";
  readonly value: number | string | boolean;
}): string => {
  switch (lit.kind) {
    case "int":
    case "float":
      return String(lit.value);
    case "string":
      return JSON.stringify(lit.value);
    case "char":
      return `'${lit.value === "'" ? "\\'" : lit.value === "\\" ? "\\\\" : lit.value}'`;
    case "bool":
      return lit.value ? "true" : "false";
  }
};

const formatApp = (
  ctx: FormatContext,
  expr: Extract<SExpr, { kind: "SApp" }>,
  level: number,
): string => {
  const parts: SExpr[] = [];
  let current: SExpr = expr;
  while (current.kind === "SApp") {
    parts.unshift(current.arg);
    current = current.func;
  }
  parts.unshift(current);
  return parts.map((e) => formatExpr(ctx, e, level, Prec.Application + 1)).join(" ");
};

const formatCase = (ctx: FormatContext, c: SCase, level: number): string => {
  const pat = formatPattern(c.pattern);
  const guard = c.guard ? ` if ${formatExpr(ctx, c.guard, level, Prec.Lowest)}` : "";
  const body = formatExpr(ctx, c.body, level, Prec.Lowest);
  return `${ind(level)}when ${pat}${guard} -> ${body}`;
};

// =============================================================================
// Pattern Formatting
// =============================================================================

const formatPattern = (pat: SPattern): string => {
  switch (pat.kind) {
    case "SPWild":
      return "_";
    case "SPVar":
      return pat.name;
    case "SPLit":
      return formatLiteral(pat.value);
    case "SPCon":
      if (pat.args.length === 0) return pat.name;
      return `${pat.name} ${pat.args.map(formatPatternAtom).join(" ")}`;
    case "SPTuple":
      return `(${pat.elements.map(formatPattern).join(", ")})`;
    case "SPRecord": {
      if (pat.fields.length === 0) return "{}";
      const fields = pat.fields
        .map((f) =>
          f.pattern.kind === "SPVar" && f.pattern.name === f.name
            ? f.name
            : `${f.name} = ${formatPattern(f.pattern)}`,
        )
        .join(", ");
      return `{ ${fields} }`;
    }
    case "SPAs":
      return `${formatPatternAtom(pat.pattern)} as ${pat.name}`;
    case "SPOr":
      return `${formatPattern(pat.left)} | ${formatPattern(pat.right)}`;
    case "SPCons":
      return `${formatPatternAtom(pat.head)} :: ${formatPattern(pat.tail)}`;
    case "SPList":
      return `[${pat.elements.map(formatPattern).join(", ")}]`;
  }
};

const formatPatternAtom = (pat: SPattern): string => {
  switch (pat.kind) {
    case "SPCon":
      return pat.args.length > 0 ? `(${formatPattern(pat)})` : formatPattern(pat);
    case "SPAs":
    case "SPOr":
    case "SPCons":
      return `(${formatPattern(pat)})`;
    default:
      return formatPattern(pat);
  }
};

// =============================================================================
// Type Formatting
// =============================================================================

const formatType = (type: SType): string => {
  switch (type.kind) {
    case "STVar":
      return type.name;
    case "STCon":
      return type.name;
    case "STApp":
      return `${formatType(type.func)} ${formatTypeAtom(type.arg)}`;
    case "STFun":
      return `${formatTypeAtom(type.param)} -> ${formatType(type.result)}`;
    case "STTuple":
      return `(${type.elements.map(formatType).join(", ")})`;
    case "STRecord": {
      if (type.fields.length === 0) return "{}";
      const fields = type.fields.map((f) => `${f.name} : ${formatType(f.type)}`).join(", ");
      return `{ ${fields} }`;
    }
  }
};

const formatTypeAtom = (type: SType): string => {
  switch (type.kind) {
    case "STApp":
    case "STFun":
      return `(${formatType(type)})`;
    default:
      return formatType(type);
  }
};

// =============================================================================
// Main API
// =============================================================================

export const format = (source: string): FormatResult => {
  const parseResult = parse(source);

  if (parseResult.diagnostics.some((d) => d.severity === "error")) {
    return {
      formatted: source,
      diagnostics: parseResult.diagnostics,
    };
  }

  const comments = collectComments(source);
  const ctx = createContext(source, comments);
  const formatted = formatProgram(ctx, parseResult.program);

  return { formatted, diagnostics: [] };
};
