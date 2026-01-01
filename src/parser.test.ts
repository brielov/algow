import { describe, expect, it } from "bun:test";
import { parse, programToExpr } from "./parser";
import * as ast from "./ast";

/** Strip span properties from AST nodes for comparison */
const stripSpans = <T>(obj: T): T => {
  if (obj === null || typeof obj !== "object") return obj;
  if (Array.isArray(obj)) return obj.map(stripSpans) as T;
  const result: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(obj)) {
    if (key !== "span") {
      result[key] = stripSpans(value);
    }
  }
  return result as T;
};

describe("Parser", () => {
  describe("literals", () => {
    it("parses integer literals", () => {
      const result = parse("42");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.num(42));
    });

    it("parses floating point literals", () => {
      const result = parse("3.14");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.num(3.14));
    });

    it("parses zero", () => {
      const result = parse("0");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.num(0));
    });

    it("parses simple strings", () => {
      const result = parse('"hello"');
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.str("hello"));
    });

    it("parses empty strings", () => {
      const result = parse('""');
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.str(""));
    });

    it("parses strings with escape sequences", () => {
      const result = parse('"hello\\nworld"');
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.str("hello\nworld"));
    });

    it("parses strings with escaped quotes", () => {
      const result = parse('"say \\"hi\\""');
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.str('say "hi"'));
    });

    it("parses strings with escaped backslash", () => {
      const result = parse('"path\\\\file"');
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.str("path\\file"));
    });

    it("parses strings with tab escape", () => {
      const result = parse('"a\\tb"');
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.str("a\tb"));
    });

    it("parses strings with carriage return escape", () => {
      const result = parse('"a\\rb"');
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.str("a\rb"));
    });

    it("parses true boolean", () => {
      const result = parse("true");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.bool(true));
    });

    it("parses false boolean", () => {
      const result = parse("false");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.bool(false));
    });
  });

  describe("variables", () => {
    it("parses lowercase variables", () => {
      const result = parse("foo");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.var_("foo"));
    });

    it("parses uppercase variables (constructors)", () => {
      const result = parse("Nil");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.var_("Nil"));
    });

    it("parses variables with underscores", () => {
      const result = parse("foo_bar");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.var_("foo_bar"));
    });

    it("parses variables with digits", () => {
      const result = parse("x1");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.var_("x1"));
    });
  });

  describe("binary operators", () => {
    it("parses addition", () => {
      const result = parse("1 + 2");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp("+", ast.num(1), ast.num(2)));
    });

    it("parses subtraction", () => {
      const result = parse("5 - 3");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp("-", ast.num(5), ast.num(3)));
    });

    it("parses multiplication", () => {
      const result = parse("2 * 3");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp("*", ast.num(2), ast.num(3)));
    });

    it("parses division", () => {
      const result = parse("10 / 2");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp("/", ast.num(10), ast.num(2)));
    });

    it("parses less than", () => {
      const result = parse("1 < 2");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp("<", ast.num(1), ast.num(2)));
    });

    it("parses less than or equal", () => {
      const result = parse("1 <= 2");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp("<=", ast.num(1), ast.num(2)));
    });

    it("parses greater than", () => {
      const result = parse("2 > 1");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp(">", ast.num(2), ast.num(1)));
    });

    it("parses greater than or equal", () => {
      const result = parse("2 >= 1");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp(">=", ast.num(2), ast.num(1)));
    });

    it("parses equality", () => {
      const result = parse("1 == 1");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp("==", ast.num(1), ast.num(1)));
    });

    it("parses inequality", () => {
      const result = parse("1 != 2");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.binOp("!=", ast.num(1), ast.num(2)));
    });

    describe("precedence", () => {
      it("multiplication binds tighter than addition", () => {
        // 1 + 2 * 3 should parse as 1 + (2 * 3)
        const result = parse("1 + 2 * 3");
        expect(result.diagnostics).toHaveLength(0);
        expect(stripSpans(result.program.expr)).toEqual(
          ast.binOp("+", ast.num(1), ast.binOp("*", ast.num(2), ast.num(3))),
        );
      });

      it("division binds tighter than subtraction", () => {
        // 6 - 4 / 2 should parse as 6 - (4 / 2)
        const result = parse("6 - 4 / 2");
        expect(result.diagnostics).toHaveLength(0);
        expect(stripSpans(result.program.expr)).toEqual(
          ast.binOp("-", ast.num(6), ast.binOp("/", ast.num(4), ast.num(2))),
        );
      });

      it("addition binds tighter than comparison", () => {
        // 1 + 2 < 4 should parse as (1 + 2) < 4
        const result = parse("1 + 2 < 4");
        expect(result.diagnostics).toHaveLength(0);
        expect(stripSpans(result.program.expr)).toEqual(
          ast.binOp("<", ast.binOp("+", ast.num(1), ast.num(2)), ast.num(4)),
        );
      });

      it("comparison binds tighter than equality", () => {
        // 1 < 2 == true should parse as (1 < 2) == true
        const result = parse("1 < 2 == true");
        expect(result.diagnostics).toHaveLength(0);
        expect(stripSpans(result.program.expr)).toEqual(
          ast.binOp("==", ast.binOp("<", ast.num(1), ast.num(2)), ast.bool(true)),
        );
      });

      it("parentheses override precedence", () => {
        // (1 + 2) * 3 should parse as (1 + 2) * 3
        const result = parse("(1 + 2) * 3");
        expect(result.diagnostics).toHaveLength(0);
        expect(stripSpans(result.program.expr)).toEqual(
          ast.binOp("*", ast.binOp("+", ast.num(1), ast.num(2)), ast.num(3)),
        );
      });
    });

    describe("associativity", () => {
      it("addition is left-associative", () => {
        // 1 + 2 + 3 should parse as (1 + 2) + 3
        const result = parse("1 + 2 + 3");
        expect(result.diagnostics).toHaveLength(0);
        expect(stripSpans(result.program.expr)).toEqual(
          ast.binOp("+", ast.binOp("+", ast.num(1), ast.num(2)), ast.num(3)),
        );
      });

      it("multiplication is left-associative", () => {
        // 2 * 3 * 4 should parse as (2 * 3) * 4
        const result = parse("2 * 3 * 4");
        expect(result.diagnostics).toHaveLength(0);
        expect(stripSpans(result.program.expr)).toEqual(
          ast.binOp("*", ast.binOp("*", ast.num(2), ast.num(3)), ast.num(4)),
        );
      });
    });
  });

  describe("pipe operator", () => {
    it("parses simple pipe", () => {
      // x |> f should desugar to f x
      const result = parse("x |> f");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.app(ast.var_("f"), ast.var_("x")));
    });

    it("parses chained pipes", () => {
      // x |> f |> g should desugar to g (f x)
      const result = parse("x |> f |> g");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.app(ast.var_("g"), ast.app(ast.var_("f"), ast.var_("x"))),
      );
    });

    it("pipe has lowest precedence", () => {
      // 1 + 2 |> f should parse as f (1 + 2)
      const result = parse("1 + 2 |> f");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.app(ast.var_("f"), ast.binOp("+", ast.num(1), ast.num(2))),
      );
    });
  });

  describe("lambdas", () => {
    it("parses simple lambda", () => {
      const result = parse("x => x + 1");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.abs("x", ast.binOp("+", ast.var_("x"), ast.num(1))),
      );
    });

    it("parses curried lambda", () => {
      const result = parse("x => y => x + y");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y")))),
      );
    });

    it("parses lambda in parentheses", () => {
      const result = parse("(x => x)");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.abs("x", ast.var_("x")));
    });

    it("parses lambda with application in body", () => {
      const result = parse("f => f x");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.abs("f", ast.app(ast.var_("f"), ast.var_("x"))),
      );
    });
  });

  describe("function application", () => {
    it("parses single argument application", () => {
      const result = parse("f x");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.app(ast.var_("f"), ast.var_("x")));
    });

    it("parses curried application", () => {
      const result = parse("f x y");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.app(ast.app(ast.var_("f"), ast.var_("x")), ast.var_("y")),
      );
    });

    it("parses triple application", () => {
      const result = parse("f x y z");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.app(ast.app(ast.app(ast.var_("f"), ast.var_("x")), ast.var_("y")), ast.var_("z")),
      );
    });

    it("application binds tighter than operators", () => {
      // f x + g y should parse as (f x) + (g y)
      const result = parse("f x + g y");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.binOp(
          "+",
          ast.app(ast.var_("f"), ast.var_("x")),
          ast.app(ast.var_("g"), ast.var_("y")),
        ),
      );
    });

    it("parses application with parenthesized expression", () => {
      const result = parse("f (x + 1)");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.app(ast.var_("f"), ast.binOp("+", ast.var_("x"), ast.num(1))),
      );
    });

    it("parses application of lambda", () => {
      const result = parse("(x => x) 42");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.app(ast.abs("x", ast.var_("x")), ast.num(42)),
      );
    });
  });

  describe("tuples", () => {
    it("parses 2-tuple", () => {
      const result = parse("(1, 2)");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.expr?.kind).toBe("Tuple");
      expect((result.program.expr as ast.Tuple).elements.length).toBe(2);
    });

    it("parses 3-tuple", () => {
      const result = parse("(1, 2, 3)");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.expr?.kind).toBe("Tuple");
      expect((result.program.expr as ast.Tuple).elements.length).toBe(3);
    });

    it("parses tuple with mixed types", () => {
      const result = parse('(1, "hello", true)');
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.expr?.kind).toBe("Tuple");
      const tuple = result.program.expr as ast.Tuple;
      expect(tuple.elements[0]?.kind).toBe("Num");
      expect(tuple.elements[1]?.kind).toBe("Str");
      expect(tuple.elements[2]?.kind).toBe("Bool");
    });

    it("parses nested tuples", () => {
      const result = parse("((1, 2), 3)");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.expr?.kind).toBe("Tuple");
      const tuple = result.program.expr as ast.Tuple;
      expect(tuple.elements[0]?.kind).toBe("Tuple");
    });

    it("parses single element in parens as that element", () => {
      const result = parse("(42)");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.num(42));
    });

    it("reports error for empty parentheses", () => {
      const result = parse("()");
      expect(result.diagnostics).toHaveLength(1);
      expect(result.diagnostics[0]!.message).toContain("empty");
    });
  });

  describe("records", () => {
    it("parses simple record", () => {
      const result = parse("{ x = 1 }");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.record([ast.field("x", ast.num(1))]));
    });

    it("parses record with multiple fields", () => {
      const result = parse("{ x = 1, y = 2 }");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]),
      );
    });

    it("parses empty record", () => {
      const result = parse("{ }");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.record([]));
    });

    it("parses nested records", () => {
      const result = parse("{ inner = { value = 42 } }");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.record([ast.field("inner", ast.record([ast.field("value", ast.num(42))]))]),
      );
    });

    it("parses record with expression values", () => {
      const result = parse("{ sum = 1 + 2 }");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.record([ast.field("sum", ast.binOp("+", ast.num(1), ast.num(2)))]),
      );
    });
  });

  describe("field access", () => {
    it("parses simple field access", () => {
      const result = parse("r.x");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(ast.fieldAccess(ast.var_("r"), "x"));
    });

    it("parses chained field access", () => {
      const result = parse("r.x.y");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.fieldAccess(ast.fieldAccess(ast.var_("r"), "x"), "y"),
      );
    });

    it("parses field access on record literal", () => {
      const result = parse("{ x = 1 }.x");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.fieldAccess(ast.record([ast.field("x", ast.num(1))]), "x"),
      );
    });

    it("application then field access parses left-to-right", () => {
      // f r.x parses as (f r).x because application happens first
      const result = parse("f r.x");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.fieldAccess(ast.app(ast.var_("f"), ast.var_("r")), "x"),
      );
    });

    it("parentheses force field access first", () => {
      // f (r.x) parses as f (r.x)
      const result = parse("f (r.x)");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.app(ast.var_("f"), ast.fieldAccess(ast.var_("r"), "x")),
      );
    });
  });

  describe("if expressions", () => {
    it("parses simple if", () => {
      const result = parse("if true then 1 else 2");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.if_(ast.bool(true), ast.num(1), ast.num(2)),
      );
    });

    it("parses if with complex condition", () => {
      const result = parse("if x > 0 then 1 else 0");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.if_(ast.binOp(">", ast.var_("x"), ast.num(0)), ast.num(1), ast.num(0)),
      );
    });

    it("parses nested if", () => {
      const result = parse("if a then if b then 1 else 2 else 3");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.if_(ast.var_("a"), ast.if_(ast.var_("b"), ast.num(1), ast.num(2)), ast.num(3)),
      );
    });

    it("parses if with expressions in branches", () => {
      const result = parse("if cond then x + 1 else y * 2");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.if_(
          ast.var_("cond"),
          ast.binOp("+", ast.var_("x"), ast.num(1)),
          ast.binOp("*", ast.var_("y"), ast.num(2)),
        ),
      );
    });
  });

  describe("let expressions", () => {
    it("parses simple let", () => {
      const result = parse("let x = 1 in x + 1");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.let_("x", ast.num(1), ast.binOp("+", ast.var_("x"), ast.num(1))),
      );
    });

    it("parses let with function parameter sugar", () => {
      // let f x y = x + y in ... should desugar to let f = x => y => x + y in ...
      const result = parse("let f x y = x + y in f 1 2");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.let_(
          "f",
          ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y")))),
          ast.app(ast.app(ast.var_("f"), ast.num(1)), ast.num(2)),
        ),
      );
    });

    it("parses nested let", () => {
      const result = parse("let x = 1 in let y = 2 in x + y");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.let_(
          "x",
          ast.num(1),
          ast.let_("y", ast.num(2), ast.binOp("+", ast.var_("x"), ast.var_("y"))),
        ),
      );
    });

    it("parses let rec", () => {
      const result = parse("let rec f n = if n == 0 then 1 else n * f (n - 1) in f 5");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.expr?.kind).toBe("LetRec");
    });

    it("parses let rec with function sugar", () => {
      const result = parse("let rec sum n = if n == 0 then 0 else n + sum (n - 1) in sum 10");
      expect(result.diagnostics).toHaveLength(0);
      const expr = result.program.expr as ast.LetRec;
      expect(expr.kind).toBe("LetRec");
      expect(expr.name).toBe("sum");
    });
  });

  describe("match expressions", () => {
    it("parses simple match", () => {
      const result = parse(`
        match x with
          | Just y => y
          | Nothing => 0
        end
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.match(ast.var_("x"), [
          ast.case_(ast.pcon("Just", [ast.pvar("y")]), ast.var_("y")),
          ast.case_(ast.pcon("Nothing", []), ast.num(0)),
        ]),
      );
    });

    it("parses match with wildcard", () => {
      const result = parse(`
        match x with
          | _ => 42
        end
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.match(ast.var_("x"), [ast.case_(ast.pwildcard(), ast.num(42))]),
      );
    });

    it("parses match with literal patterns", () => {
      const result = parse(`
        match n with
          | 0 => true
          | _ => false
        end
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.match(ast.var_("n"), [
          ast.case_(ast.plit(0), ast.bool(true)),
          ast.case_(ast.pwildcard(), ast.bool(false)),
        ]),
      );
    });

    it("parses match with boolean patterns", () => {
      const result = parse(`
        match b with
          | true => 1
          | false => 0
        end
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.match(ast.var_("b"), [
          ast.case_(ast.plit(true), ast.num(1)),
          ast.case_(ast.plit(false), ast.num(0)),
        ]),
      );
    });

    it("parses match with nested constructor patterns", () => {
      const result = parse(`
        match xs with
          | Cons x (Cons y Nil) => x + y
          | _ => 0
        end
      `);
      expect(result.diagnostics).toHaveLength(0);
      const match = result.program.expr as ast.Match;
      expect(match.kind).toBe("Match");
      expect(match.cases.length).toBe(2);
      expect(match.cases[0]!.pattern.kind).toBe("PCon");
      expect(match.cases[1]!.pattern.kind).toBe("PWildcard");
    });

    it("parses match with tuple patterns", () => {
      const result = parse(`
        match pair with
          | (x, y) => x + y
        end
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.match(ast.var_("pair"), [
          ast.case_(
            ast.ptuple([ast.pvar("x"), ast.pvar("y")]),
            ast.binOp("+", ast.var_("x"), ast.var_("y")),
          ),
        ]),
      );
    });

    it("parses match with record patterns", () => {
      const result = parse(`
        match r with
          | { x = a, y = b } => a + b
        end
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.match(ast.var_("r"), [
          ast.case_(
            ast.precord([ast.pfield("x", ast.pvar("a")), ast.pfield("y", ast.pvar("b"))]),
            ast.binOp("+", ast.var_("a"), ast.var_("b")),
          ),
        ]),
      );
    });

    it("parses match with string patterns", () => {
      const result = parse(`
        match s with
          | "hello" => 1
          | _ => 0
        end
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.expr)).toEqual(
        ast.match(ast.var_("s"), [
          ast.case_(ast.plit("hello"), ast.num(1)),
          ast.case_(ast.pwildcard(), ast.num(0)),
        ]),
      );
    });
  });

  describe("data declarations", () => {
    it("parses simple data declaration", () => {
      const result = parse("data Bool = True | False");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.declarations).toHaveLength(1);
      expect(stripSpans(result.program.declarations[0])).toEqual(
        ast.dataDecl("Bool", [], [ast.conDecl("True", []), ast.conDecl("False", [])]),
      );
    });

    it("parses data declaration with type variable argument", () => {
      // This tests parseTypeAtomSimple TyVar case
      const result = parse("data Box a = MkBox a");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.declarations[0])).toEqual(
        ast.dataDecl("Box", ["a"], [ast.conDecl("MkBox", [ast.tyvar("a")])]),
      );
    });

    it("parses data declaration with type constructor argument", () => {
      // This tests parseTypeAtomSimple TyCon case
      const result = parse("data Wrap = MkWrap Int");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.declarations[0])).toEqual(
        ast.dataDecl("Wrap", [], [ast.conDecl("MkWrap", [ast.tycon("Int")])]),
      );
    });

    it("parses data declaration with parenthesized type", () => {
      // This tests parseTypeAtomSimple LParen case
      const result = parse("data Pair a b = MkPair (a) (b)");
      expect(result.diagnostics).toHaveLength(0);
      expect(stripSpans(result.program.declarations[0])).toEqual(
        ast.dataDecl("Pair", ["a", "b"], [ast.conDecl("MkPair", [ast.tyvar("a"), ast.tyvar("b")])]),
      );
    });

    it("parses data declaration with nested type application", () => {
      // This tests parseTypeAtom returning type applications
      const result = parse("data Tree a = Leaf | Node (Tree a) a (Tree a)");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.declarations).toHaveLength(1);
    });

    it("parses parameterized data declaration", () => {
      const result = parse("data Maybe a = Nothing | Just a");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.declarations).toHaveLength(1);
      expect(stripSpans(result.program.declarations[0])).toEqual(
        ast.dataDecl(
          "Maybe",
          ["a"],
          [ast.conDecl("Nothing", []), ast.conDecl("Just", [ast.tyvar("a")])],
        ),
      );
    });

    it("parses recursive data declaration", () => {
      const result = parse("data List a = Nil | Cons a (List a)");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.declarations).toHaveLength(1);
      expect(stripSpans(result.program.declarations[0])).toEqual(
        ast.dataDecl(
          "List",
          ["a"],
          [
            ast.conDecl("Nil", []),
            ast.conDecl("Cons", [ast.tyvar("a"), ast.tyapp(ast.tycon("List"), ast.tyvar("a"))]),
          ],
        ),
      );
    });

    it("parses multi-parameter data declaration", () => {
      const result = parse("data Either a b = Left a | Right b");
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.declarations).toHaveLength(1);
      expect(stripSpans(result.program.declarations[0])).toEqual(
        ast.dataDecl(
          "Either",
          ["a", "b"],
          [ast.conDecl("Left", [ast.tyvar("a")]), ast.conDecl("Right", [ast.tyvar("b")])],
        ),
      );
    });

    it("parses multiple data declarations", () => {
      const result = parse(`
        data Bool = True | False
        data Maybe a = Nothing | Just a
        42
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.declarations).toHaveLength(2);
      expect(stripSpans(result.program.expr)).toEqual(ast.num(42));
    });
  });

  describe("top-level bindings", () => {
    it("parses top-level let binding", () => {
      const result = parse(`
        let add x y = x + y
        add 1 2
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.bindings).toHaveLength(1);
      expect(result.program.bindings[0]!.name).toBe("add");
      expect(result.program.bindings[0]!.params).toEqual(["x", "y"]);
      expect(result.program.bindings[0]!.recursive).toBe(false);
    });

    it("parses top-level let rec binding", () => {
      const result = parse(`
        let rec fact n = if n == 0 then 1 else n * fact (n - 1)
        fact 5
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.bindings).toHaveLength(1);
      expect(result.program.bindings[0]!.name).toBe("fact");
      expect(result.program.bindings[0]!.recursive).toBe(true);
    });

    it("parses multiple top-level bindings", () => {
      const result = parse(`
        let add x y = x + y
        let double x = x * 2
        add 1 (double 3)
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.bindings).toHaveLength(2);
      expect(result.program.bindings[0]!.name).toBe("add");
      expect(result.program.bindings[1]!.name).toBe("double");
    });

    it("parses binding without parameters", () => {
      const result = parse(`
        let answer = 42
        answer
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.bindings).toHaveLength(1);
      expect(result.program.bindings[0]!.params).toEqual([]);
    });
  });

  describe("programToExpr", () => {
    it("converts program without bindings", () => {
      const result = parse("42");
      const expr = programToExpr(result.program);
      expect(stripSpans(expr)).toEqual(ast.num(42));
    });

    it("converts program with bindings", () => {
      const result = parse(`
        let x = 1
        let y = 2
        x + y
      `);
      const expr = programToExpr(result.program);
      expect(stripSpans(expr)).toEqual(
        ast.let_(
          "x",
          ast.num(1),
          ast.let_("y", ast.num(2), ast.binOp("+", ast.var_("x"), ast.var_("y"))),
        ),
      );
    });

    it("converts recursive bindings", () => {
      const result = parse(`
        let rec f x = f x
        f 1
      `);
      const expr = programToExpr(result.program);
      expect(expr?.kind).toBe("LetRec");
    });

    it("returns null for empty program", () => {
      const program = { declarations: [], bindings: [], expr: null };
      expect(programToExpr(program)).toBeNull();
    });

    it("converts bindings with function parameters", () => {
      const result = parse(`
        let add x y = x + y
        add 1 2
      `);
      const expr = programToExpr(result.program);
      expect(stripSpans(expr)).toEqual(
        ast.let_(
          "add",
          ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y")))),
          ast.app(ast.app(ast.var_("add"), ast.num(1)), ast.num(2)),
        ),
      );
    });
  });

  describe("error recovery", () => {
    it("reports error for missing expression", () => {
      const result = parse("let x =");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing 'then' in if", () => {
      const result = parse("if true 1 else 2");
      expect(result.diagnostics.length).toBeGreaterThan(0);
      expect(result.diagnostics[0]!.message).toContain("then");
    });

    it("reports error for missing 'else' in if", () => {
      const result = parse("if true then 1 2");
      expect(result.diagnostics.length).toBeGreaterThan(0);
      expect(result.diagnostics[0]!.message).toContain("else");
    });

    it("reports error for missing 'end' in match", () => {
      const result = parse("match x with | y => y");
      expect(result.diagnostics.length).toBeGreaterThan(0);
      expect(result.diagnostics[0]!.message).toContain("end");
    });

    it("reports error for unexpected token", () => {
      const result = parse("let = 1 in 2");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("synchronizes after error in top-level binding", () => {
      const result = parse(`
        let x =
        let y = 2
        y
      `);
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("synchronizes after error in data declaration", () => {
      const result = parse(`
        data Maybe =
        data List a = Nil | Cons a (List a)
        Nil
      `);
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing = in data declaration", () => {
      const result = parse("data Maybe a Nothing | Just a");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing constructor name", () => {
      const result = parse("data Maybe a = | Just a");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing = in let binding", () => {
      const result = parse("let x 1 in x");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing 'in' in nested let expression", () => {
      // Inside another expression, missing 'in' is an error
      const result = parse("(let x = 1 x)");
      expect(result.diagnostics.length).toBeGreaterThan(0);
      expect(result.diagnostics[0]!.message).toContain("in");
    });

    it("reports error for missing with in match", () => {
      const result = parse("match x | y => y end");
      expect(result.diagnostics.length).toBeGreaterThan(0);
      expect(result.diagnostics[0]!.message).toContain("with");
    });

    it("reports error for missing arrow in match case", () => {
      const result = parse("match x with | y y end");
      expect(result.diagnostics.length).toBeGreaterThan(0);
      expect(result.diagnostics[0]!.message).toContain("=>");
    });

    it("reports error for unexpected token in pattern", () => {
      const result = parse("match x with | + => 1 end");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing field name in record", () => {
      const result = parse("{ = 1 }");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing = in record field", () => {
      const result = parse("{ x 1 }");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing closing brace in record", () => {
      const result = parse("{ x = 1");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing closing paren in tuple", () => {
      const result = parse("(1, 2");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing field name in record pattern", () => {
      const result = parse("match r with | { = x } => x end");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing = in record pattern field", () => {
      const result = parse("match r with | { x y } => y end");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing closing brace in record pattern", () => {
      const result = parse("match r with | { x = y => y end");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for empty tuple pattern", () => {
      const result = parse("match x with | () => 1 end");
      expect(result.diagnostics.length).toBeGreaterThan(0);
      expect(result.diagnostics[0]!.message).toContain("empty");
    });

    it("reports error for missing closing paren in tuple pattern", () => {
      const result = parse("match x with | (a, b => a end");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing field after dot", () => {
      const result = parse("r.");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing closing paren in type", () => {
      const result = parse("data Foo = Bar (List a");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for missing binding name in let expression", () => {
      const result = parse("(let = 1 in 2)");
      expect(result.diagnostics.length).toBeGreaterThan(0);
    });
  });

  describe("pattern atom edge cases", () => {
    it("parses number literal in constructor pattern", () => {
      const result = parse("match x with | Foo 42 => 1 end");
      expect(result.diagnostics).toHaveLength(0);
      const match = result.program.expr as ast.Match;
      const pcon = match.cases[0]!.pattern as ast.PCon;
      expect(stripSpans(pcon.args[0])).toEqual(ast.plit(42));
    });

    it("parses string literal in constructor pattern", () => {
      const result = parse('match x with | Foo "hello" => 1 end');
      expect(result.diagnostics).toHaveLength(0);
      const match = result.program.expr as ast.Match;
      const pcon = match.cases[0]!.pattern as ast.PCon;
      expect(stripSpans(pcon.args[0])).toEqual(ast.plit("hello"));
    });

    it("parses true in constructor pattern", () => {
      const result = parse("match x with | Foo true => 1 end");
      expect(result.diagnostics).toHaveLength(0);
      const match = result.program.expr as ast.Match;
      const pcon = match.cases[0]!.pattern as ast.PCon;
      expect(stripSpans(pcon.args[0])).toEqual(ast.plit(true));
    });

    it("parses false in constructor pattern", () => {
      const result = parse("match x with | Foo false => 1 end");
      expect(result.diagnostics).toHaveLength(0);
      const match = result.program.expr as ast.Match;
      const pcon = match.cases[0]!.pattern as ast.PCon;
      expect(stripSpans(pcon.args[0])).toEqual(ast.plit(false));
    });

    it("parses tuple pattern in constructor pattern", () => {
      const result = parse("match x with | Foo (a, b) => a end");
      expect(result.diagnostics).toHaveLength(0);
      const match = result.program.expr as ast.Match;
      const pcon = match.cases[0]!.pattern as ast.PCon;
      expect(pcon.args[0]!.kind).toBe("PTuple");
    });

    it("parses record pattern in constructor pattern", () => {
      const result = parse("match x with | Foo { x = a } => a end");
      expect(result.diagnostics).toHaveLength(0);
      const match = result.program.expr as ast.Match;
      const pcon = match.cases[0]!.pattern as ast.PCon;
      expect(pcon.args[0]!.kind).toBe("PRecord");
    });

    it("parses nullary constructor in constructor pattern", () => {
      const result = parse("match x with | Foo Bar => 1 end");
      expect(result.diagnostics).toHaveLength(0);
      const match = result.program.expr as ast.Match;
      const pcon = match.cases[0]!.pattern as ast.PCon;
      expect(pcon.args[0]?.kind).toBe("PCon");
      expect((pcon.args[0] as ast.PCon).name).toBe("Bar");
    });
  });

  describe("let expression with function parameters", () => {
    it("parses let with single parameter", () => {
      const result = parse("(let f x = x in f 1)");
      expect(result.diagnostics).toHaveLength(0);
    });

    it("parses let with multiple parameters", () => {
      const result = parse("(let f x y z = x in f 1 2 3)");
      expect(result.diagnostics).toHaveLength(0);
    });
  });

  describe("complex programs", () => {
    it("parses fibonacci function", () => {
      const result = parse(`
        let rec fib n =
          if n <= 1
          then n
          else fib (n - 1) + fib (n - 2)
        in fib 10
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.expr?.kind).toBe("LetRec");
    });

    it("parses list operations", () => {
      const result = parse(`
        data List a = Nil | Cons a (List a)
        let rec length xs = match xs with
          | Nil => 0
          | Cons _ rest => 1 + length rest
        end
        Cons 1 (Cons 2 Nil) |> length
      `);
      expect(result.diagnostics).toHaveLength(0);
      expect(result.program.declarations).toHaveLength(1);
      expect(result.program.bindings).toHaveLength(1);
    });

    it("parses maybe operations", () => {
      const result = parse(`
        data Maybe a = Nothing | Just a
        let map f m = match m with
          | Nothing => Nothing
          | Just x => Just (f x)
        end
        map (x => x + 1) (Just 42)
      `);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("parses record operations", () => {
      const result = parse(`
        let point = { x = 3, y = 4 }
        let distance p = p.x * p.x + p.y * p.y
        distance point
      `);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("parses pipe chains with application", () => {
      const result = parse(`
        let double x = x * 2
        let inc x = x + 1
        5 |> double |> inc |> double
      `);
      expect(result.diagnostics).toHaveLength(0);
    });
  });
});
