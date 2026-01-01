import { describe, expect, it } from "bun:test";
import { createLexer, nextToken, slice, TokenKind } from "./lexer";
import { parse, programToExpr } from "./parser";
import { baseEnv, infer, mergeEnvs, mergeRegistries, processDataDecl, typeToString } from "./infer";
import * as ast from "./ast";

describe("Lexer", () => {
  it("lexes numbers", () => {
    const state = createLexer("42 3.14");
    let tok = nextToken(state);
    expect(tok[0]).toBe(TokenKind.Number);
    expect(slice(state, tok[1], tok[2])).toBe("42");

    tok = nextToken(state);
    expect(tok[0]).toBe(TokenKind.Number);
    expect(slice(state, tok[1], tok[2])).toBe("3.14");
  });

  it("lexes strings", () => {
    const state = createLexer('"hello" "world\\n"');
    let tok = nextToken(state);
    expect(tok[0]).toBe(TokenKind.String);
    expect(slice(state, tok[1], tok[2])).toBe('"hello"');

    tok = nextToken(state);
    expect(tok[0]).toBe(TokenKind.String);
  });

  it("lexes identifiers and keywords", () => {
    const state = createLexer("let rec foo Bar true false");
    expect(nextToken(state)[0]).toBe(TokenKind.Let);
    expect(nextToken(state)[0]).toBe(TokenKind.Rec);
    expect(nextToken(state)[0]).toBe(TokenKind.Lower);
    expect(nextToken(state)[0]).toBe(TokenKind.Upper);
    expect(nextToken(state)[0]).toBe(TokenKind.True);
    expect(nextToken(state)[0]).toBe(TokenKind.False);
  });

  it("lexes operators", () => {
    const state = createLexer("+ - * / < <= > >= == != |> => = | , . _");
    expect(nextToken(state)[0]).toBe(TokenKind.Plus);
    expect(nextToken(state)[0]).toBe(TokenKind.Minus);
    expect(nextToken(state)[0]).toBe(TokenKind.Star);
    expect(nextToken(state)[0]).toBe(TokenKind.Slash);
    expect(nextToken(state)[0]).toBe(TokenKind.Lt);
    expect(nextToken(state)[0]).toBe(TokenKind.Le);
    expect(nextToken(state)[0]).toBe(TokenKind.Gt);
    expect(nextToken(state)[0]).toBe(TokenKind.Ge);
    expect(nextToken(state)[0]).toBe(TokenKind.EqEq);
    expect(nextToken(state)[0]).toBe(TokenKind.Ne);
    expect(nextToken(state)[0]).toBe(TokenKind.Pipe);
    expect(nextToken(state)[0]).toBe(TokenKind.Arrow);
    expect(nextToken(state)[0]).toBe(TokenKind.Eq);
    expect(nextToken(state)[0]).toBe(TokenKind.Bar);
    expect(nextToken(state)[0]).toBe(TokenKind.Comma);
    expect(nextToken(state)[0]).toBe(TokenKind.Dot);
    expect(nextToken(state)[0]).toBe(TokenKind.Underscore);
  });

  it("skips line comments", () => {
    const state = createLexer("42 -- this is a comment\n43");
    expect(nextToken(state)[0]).toBe(TokenKind.Number);
    const tok = nextToken(state);
    expect(tok[0]).toBe(TokenKind.Number);
    expect(slice(state, tok[1], tok[2])).toBe("43");
  });

  it("skips block comments", () => {
    const state = createLexer("42 {- nested {- comment -} -} 43");
    expect(nextToken(state)[0]).toBe(TokenKind.Number);
    const tok = nextToken(state);
    expect(tok[0]).toBe(TokenKind.Number);
    expect(slice(state, tok[1], tok[2])).toBe("43");
  });
});

describe("Parser", () => {
  it("parses number literals", () => {
    const result = parse("42");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(ast.num(42));
  });

  it("parses string literals", () => {
    const result = parse('"hello"');
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(ast.str("hello"));
  });

  it("parses boolean literals", () => {
    expect(parse("true").program.expr).toEqual(ast.bool(true));
    expect(parse("false").program.expr).toEqual(ast.bool(false));
  });

  it("parses variables", () => {
    const result = parse("foo");
    expect(result.program.expr).toEqual(ast.var_("foo"));
  });

  it("parses binary operators with correct precedence", () => {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    const result = parse("1 + 2 * 3");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(
      ast.binOp("+", ast.num(1), ast.binOp("*", ast.num(2), ast.num(3))),
    );
  });

  it("parses pipe operator", () => {
    // x |> f should desugar to f x
    const result = parse("x |> f");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(ast.app(ast.var_("f"), ast.var_("x")));
  });

  it("parses chained pipe operators", () => {
    // x |> f |> g should desugar to g (f x)
    const result = parse("x |> f |> g");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(
      ast.app(ast.var_("g"), ast.app(ast.var_("f"), ast.var_("x"))),
    );
  });

  it("parses lambdas", () => {
    const result = parse("x => x + 1");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(ast.abs("x", ast.binOp("+", ast.var_("x"), ast.num(1))));
  });

  it("parses curried lambdas", () => {
    const result = parse("x => y => x + y");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(
      ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y")))),
    );
  });

  it("parses function application", () => {
    const result = parse("f x y");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(ast.app(ast.app(ast.var_("f"), ast.var_("x")), ast.var_("y")));
  });

  it("parses tuples", () => {
    const result = parse("(1, 2, 3)");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(ast.tuple(ast.num(1), ast.num(2), ast.num(3)));
  });

  it("parses records", () => {
    const result = parse("{ x = 1, y = 2 }");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(
      ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]),
    );
  });

  it("parses field access", () => {
    const result = parse("r.x");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(ast.fieldAccess(ast.var_("r"), "x"));
  });

  it("parses if expressions", () => {
    const result = parse("if x then 1 else 2");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(ast.if_(ast.var_("x"), ast.num(1), ast.num(2)));
  });

  it("parses let expressions", () => {
    const result = parse("let x = 1 in x + 1");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(
      ast.let_("x", ast.num(1), ast.binOp("+", ast.var_("x"), ast.num(1))),
    );
  });

  it("parses let with function sugar", () => {
    // let f x y = x + y in ... should desugar to let f = x => y => x + y in ...
    const result = parse("let f x y = x + y in f 1 2");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(
      ast.let_(
        "f",
        ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y")))),
        ast.app(ast.app(ast.var_("f"), ast.num(1)), ast.num(2)),
      ),
    );
  });

  it("parses letrec", () => {
    const result = parse("let rec f n = if n == 0 then 1 else n * f (n - 1) in f 5");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr?.kind).toBe("LetRec");
  });

  it("parses match expressions", () => {
    const result = parse(`
      match x with
        | Just y => y
        | Nothing => 0
      end
    `);
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.expr).toEqual(
      ast.match(ast.var_("x"), [
        ast.case_(ast.pcon("Just", ast.pvar("y")), ast.var_("y")),
        ast.case_(ast.pcon("Nothing"), ast.num(0)),
      ]),
    );
  });

  it("parses data declarations", () => {
    const result = parse("data Maybe a = Nothing | Just a");
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.declarations).toHaveLength(1);
    expect(result.program.declarations[0]).toEqual(
      ast.dataDecl("Maybe", ["a"], [
        ast.conDecl("Nothing", []),
        ast.conDecl("Just", [ast.tyvar("a")]),
      ]),
    );
  });

  it("parses top-level let bindings", () => {
    const result = parse(`
      let add x y = x + y
      let double x = x * 2
      add 1 2
    `);
    expect(result.diagnostics).toHaveLength(0);
    expect(result.program.bindings).toHaveLength(2);
    expect(result.program.bindings[0]!.name).toBe("add");
    expect(result.program.bindings[1]!.name).toBe("double");
  });
});

describe("Parser + Type Inference Integration", () => {
  it("infers types for parsed expressions", () => {
    const result = parse("x => x + 1");
    expect(result.diagnostics).toHaveLength(0);
    const expr = result.program.expr!;
    const { type, diagnostics } = infer(baseEnv, new Map(), expr);
    expect(diagnostics).toHaveLength(0);
    expect(typeToString(type)).toBe("number -> number");
  });

  it("infers types for parsed lambda", () => {
    const result = parse("x => y => x + y");
    expect(result.diagnostics).toHaveLength(0);
    const expr = result.program.expr!;
    const { type, diagnostics } = infer(baseEnv, new Map(), expr);
    expect(diagnostics).toHaveLength(0);
    // Type is polymorphic over Add typeclass, but defaults to t -> t -> t
    expect(typeToString(type)).toMatch(/^t\d+ -> t\d+ -> t\d+$/);
  });

  it("infers types with ADTs", () => {
    const result = parse(`
      data Maybe a = Nothing | Just a
      Just 42
    `);
    expect(result.diagnostics).toHaveLength(0);

    // Process the data declaration
    const [maybeEnv, maybeReg] = processDataDecl(result.program.declarations[0]!);
    const env = mergeEnvs(baseEnv, maybeEnv);
    const registry = mergeRegistries(maybeReg);

    const expr = programToExpr(result.program);
    const { type, diagnostics } = infer(env, registry, expr!);
    expect(diagnostics).toHaveLength(0);
    expect(typeToString(type)).toBe("Maybe number");
  });

  it("infers types for full programs with pipe", () => {
    // data List a = Nil | Cons a (List a)
    // let rec length xs = match xs with | Nil => 0 | Cons _ rest => 1 + length rest end
    // Cons 1 (Cons 2 Nil) |> length
    const result = parse(`
      data List a = Nil | Cons a (List a)
      let rec length xs = match xs with
        | Nil => 0
        | Cons _ rest => 1 + length rest
      end
      Cons 1 (Cons 2 Nil) |> length
    `);
    expect(result.diagnostics).toHaveLength(0);

    const [listEnv, listReg] = processDataDecl(result.program.declarations[0]!);
    const env = mergeEnvs(baseEnv, listEnv);
    const registry = mergeRegistries(listReg);

    const expr = programToExpr(result.program);
    const { type, diagnostics } = infer(env, registry, expr!);
    expect(diagnostics).toHaveLength(0);
    expect(typeToString(type)).toBe("number");
  });
});
