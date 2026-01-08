import { describe, expect, it } from "bun:test";
import * as ast from "../src/ast";
import {
  evaluate,
  valueToString,
  vcon,
  vnum,
  vstr,
  vchar,
  vbool,
  vtuple,
  emptyEnv,
  RuntimeError,
  type Env,
} from "../src/eval";

// Base environment with constructors
const baseEnv: Env = new Map([
  ["Nil", vcon("Nil")],
  ["Cons", vcon("Cons")],
  ["Nothing", vcon("Nothing")],
  ["Just", vcon("Just")],
  ["Left", vcon("Left")],
  ["Right", vcon("Right")],
]);

describe("Interpreter", () => {
  describe("literals", () => {
    it("evaluates numbers", () => {
      expect(evaluate(emptyEnv, ast.num(42))).toEqual(vnum(42));
      expect(evaluate(emptyEnv, ast.num(3.14))).toEqual(vnum(3.14));
      expect(evaluate(emptyEnv, ast.num(-5))).toEqual(vnum(-5));
    });

    it("evaluates strings", () => {
      expect(evaluate(emptyEnv, ast.str("hello"))).toEqual(vstr("hello"));
      expect(evaluate(emptyEnv, ast.str(""))).toEqual(vstr(""));
    });

    it("evaluates booleans", () => {
      expect(evaluate(emptyEnv, ast.bool(true))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.bool(false))).toEqual(vbool(false));
    });

    it("evaluates characters", () => {
      expect(evaluate(emptyEnv, ast.char("a"))).toEqual(vchar("a"));
      expect(evaluate(emptyEnv, ast.char("\n"))).toEqual(vchar("\n"));
      expect(evaluate(emptyEnv, ast.char("\t"))).toEqual(vchar("\t"));
    });
  });

  describe("variables", () => {
    it("looks up variables from environment", () => {
      const env: Env = new Map([["x", vnum(42)]]);
      expect(evaluate(env, ast.var_("x"))).toEqual(vnum(42));
    });
  });

  describe("arithmetic", () => {
    it("adds numbers", () => {
      const result = evaluate(emptyEnv, ast.binOp("+", ast.num(1), ast.num(2)));
      expect(result).toEqual(vnum(3));
    });

    it("subtracts numbers", () => {
      const result = evaluate(emptyEnv, ast.binOp("-", ast.num(10), ast.num(3)));
      expect(result).toEqual(vnum(7));
    });

    it("multiplies numbers", () => {
      const result = evaluate(emptyEnv, ast.binOp("*", ast.num(4), ast.num(5)));
      expect(result).toEqual(vnum(20));
    });

    it("divides numbers", () => {
      const result = evaluate(emptyEnv, ast.binOp("/", ast.num(10), ast.num(2)));
      expect(result).toEqual(vnum(5));
    });

    it("throws on division by zero", () => {
      expect(() => evaluate(emptyEnv, ast.binOp("/", ast.num(1), ast.num(0)))).toThrow(
        RuntimeError,
      );
    });

    it("concatenates strings with +", () => {
      const result = evaluate(emptyEnv, ast.binOp("+", ast.str("hello"), ast.str(" world")));
      expect(result).toEqual(vstr("hello world"));
    });
  });

  describe("comparisons", () => {
    it("compares numbers with <", () => {
      expect(evaluate(emptyEnv, ast.binOp("<", ast.num(1), ast.num(2)))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("<", ast.num(2), ast.num(1)))).toEqual(vbool(false));
    });

    it("compares numbers with >", () => {
      expect(evaluate(emptyEnv, ast.binOp(">", ast.num(2), ast.num(1)))).toEqual(vbool(true));
    });

    it("compares numbers with <=", () => {
      expect(evaluate(emptyEnv, ast.binOp("<=", ast.num(1), ast.num(1)))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("<=", ast.num(1), ast.num(2)))).toEqual(vbool(true));
    });

    it("compares numbers with >=", () => {
      expect(evaluate(emptyEnv, ast.binOp(">=", ast.num(2), ast.num(2)))).toEqual(vbool(true));
    });

    it("compares with ==", () => {
      expect(evaluate(emptyEnv, ast.binOp("==", ast.num(1), ast.num(1)))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("==", ast.num(1), ast.num(2)))).toEqual(vbool(false));
      expect(evaluate(emptyEnv, ast.binOp("==", ast.str("a"), ast.str("a")))).toEqual(vbool(true));
    });

    it("compares with !=", () => {
      expect(evaluate(emptyEnv, ast.binOp("!=", ast.num(1), ast.num(2)))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("!=", ast.num(1), ast.num(1)))).toEqual(vbool(false));
    });

    it("concatenates strings with +", () => {
      expect(evaluate(emptyEnv, ast.binOp("+", ast.str("hello"), ast.str(" world")))).toEqual(
        vstr("hello world"),
      );
    });

    it("concatenates empty strings with +", () => {
      expect(evaluate(emptyEnv, ast.binOp("+", ast.str(""), ast.str("test")))).toEqual(
        vstr("test"),
      );
      expect(evaluate(emptyEnv, ast.binOp("+", ast.str("test"), ast.str("")))).toEqual(
        vstr("test"),
      );
    });

    it("compares chars with <", () => {
      expect(evaluate(emptyEnv, ast.binOp("<", ast.char("a"), ast.char("b")))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("<", ast.char("b"), ast.char("a")))).toEqual(vbool(false));
    });

    it("compares chars with ==", () => {
      expect(evaluate(emptyEnv, ast.binOp("==", ast.char("a"), ast.char("a")))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("==", ast.char("a"), ast.char("b")))).toEqual(
        vbool(false),
      );
    });

    it("compares chars with !=", () => {
      expect(evaluate(emptyEnv, ast.binOp("!=", ast.char("a"), ast.char("b")))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("!=", ast.char("a"), ast.char("a")))).toEqual(
        vbool(false),
      );
    });
  });

  describe("functions", () => {
    it("creates closures", () => {
      const result = evaluate(emptyEnv, ast.abs("x", ast.var_("x")));
      expect(result.kind).toBe("VClosure");
    });

    it("applies identity function", () => {
      const id = ast.abs("x", ast.var_("x"));
      const result = evaluate(emptyEnv, ast.app(id, ast.num(42)));
      expect(result).toEqual(vnum(42));
    });

    it("applies function with computation", () => {
      const addOne = ast.abs("x", ast.binOp("+", ast.var_("x"), ast.num(1)));
      const result = evaluate(emptyEnv, ast.app(addOne, ast.num(10)));
      expect(result).toEqual(vnum(11));
    });

    it("supports curried functions", () => {
      const add = ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y"))));
      const result = evaluate(emptyEnv, ast.app(ast.app(add, ast.num(3)), ast.num(4)));
      expect(result).toEqual(vnum(7));
    });

    it("captures environment in closures", () => {
      const expr = ast.let_(
        "x",
        ast.num(10),
        ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y"))),
      );
      const result = evaluate(emptyEnv, ast.app(expr, ast.num(5)));
      expect(result).toEqual(vnum(15));
    });
  });

  describe("let bindings", () => {
    it("binds value and evaluates body", () => {
      const expr = ast.let_("x", ast.num(5), ast.binOp("*", ast.var_("x"), ast.var_("x")));
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(25));
    });

    it("shadows outer bindings", () => {
      const env: Env = new Map([["x", vnum(100)]]);
      const expr = ast.let_("x", ast.num(5), ast.var_("x"));
      expect(evaluate(env, expr)).toEqual(vnum(5));
    });
  });

  describe("recursive let (letrec)", () => {
    it("supports recursive functions", () => {
      // factorial = fn n => if n == 0 then 1 else n * factorial (n - 1)
      const factorial = ast.letRec(
        [
          ast.recBinding(
            "fact",
            ast.abs(
              "n",
              ast.if_(
                ast.binOp("==", ast.var_("n"), ast.num(0)),
                ast.num(1),
                ast.binOp(
                  "*",
                  ast.var_("n"),
                  ast.app(ast.var_("fact"), ast.binOp("-", ast.var_("n"), ast.num(1))),
                ),
              ),
            ),
          ),
        ],
        ast.app(ast.var_("fact"), ast.num(5)),
      );
      expect(evaluate(emptyEnv, factorial)).toEqual(vnum(120));
    });

    it("supports mutual recursion", () => {
      // isEven n = if n == 0 then true else isOdd (n - 1)
      // isOdd n = if n == 0 then false else isEven (n - 1)
      const mutualRec = ast.letRec(
        [
          ast.recBinding(
            "isEven",
            ast.abs(
              "n",
              ast.if_(
                ast.binOp("==", ast.var_("n"), ast.num(0)),
                ast.bool(true),
                ast.app(ast.var_("isOdd"), ast.binOp("-", ast.var_("n"), ast.num(1))),
              ),
            ),
          ),
          ast.recBinding(
            "isOdd",
            ast.abs(
              "n",
              ast.if_(
                ast.binOp("==", ast.var_("n"), ast.num(0)),
                ast.bool(false),
                ast.app(ast.var_("isEven"), ast.binOp("-", ast.var_("n"), ast.num(1))),
              ),
            ),
          ),
        ],
        ast.tuple([
          ast.app(ast.var_("isEven"), ast.num(10)),
          ast.app(ast.var_("isOdd"), ast.num(10)),
        ]),
      );
      expect(evaluate(emptyEnv, mutualRec)).toEqual(vtuple([vbool(true), vbool(false)]));
    });
  });

  describe("conditionals", () => {
    it("evaluates then branch when true", () => {
      const expr = ast.if_(ast.bool(true), ast.num(1), ast.num(2));
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(1));
    });

    it("evaluates else branch when false", () => {
      const expr = ast.if_(ast.bool(false), ast.num(1), ast.num(2));
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(2));
    });

    it("works with computed conditions", () => {
      const expr = ast.if_(ast.binOp(">", ast.num(5), ast.num(3)), ast.str("yes"), ast.str("no"));
      expect(evaluate(emptyEnv, expr)).toEqual(vstr("yes"));
    });
  });

  describe("tuples", () => {
    it("evaluates tuple elements", () => {
      const expr = ast.tuple([ast.num(1), ast.str("hello"), ast.bool(true)]);
      const result = evaluate(emptyEnv, expr);
      expect(result.kind).toBe("VTuple");
      expect(valueToString(result)).toBe('(1, "hello", true)');
    });

    it("unwraps single-element tuples", () => {
      const result = evaluate(emptyEnv, ast.tuple([ast.num(42)]));
      expect(result).toEqual(vnum(42));
    });

    it("accesses tuple elements by index", () => {
      const tuple = ast.tuple([ast.num(42), ast.str("hello"), ast.bool(true)]);
      expect(evaluate(emptyEnv, ast.tupleIndex(tuple, 0))).toEqual(vnum(42));
      expect(evaluate(emptyEnv, ast.tupleIndex(tuple, 1))).toEqual(vstr("hello"));
      expect(evaluate(emptyEnv, ast.tupleIndex(tuple, 2))).toEqual(vbool(true));
    });

    it("accesses nested tuple elements", () => {
      const inner = ast.tuple([ast.num(1), ast.num(2)]);
      const outer = ast.tuple([inner, ast.num(3)]);
      expect(evaluate(emptyEnv, ast.tupleIndex(ast.tupleIndex(outer, 0), 1))).toEqual(vnum(2));
    });
  });

  describe("records", () => {
    it("evaluates record fields", () => {
      const expr = ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]);
      const result = evaluate(emptyEnv, expr);
      expect(result.kind).toBe("VRecord");
      expect(valueToString(result)).toBe("{ x: 1, y: 2 }");
    });

    it("accesses record fields", () => {
      const record = ast.record([ast.field("x", ast.num(42)), ast.field("y", ast.str("hello"))]);
      expect(evaluate(emptyEnv, ast.fieldAccess(record, "x"))).toEqual(vnum(42));
      expect(evaluate(emptyEnv, ast.fieldAccess(record, "y"))).toEqual(vstr("hello"));
    });
  });

  describe("data constructors", () => {
    it("creates nullary constructors", () => {
      expect(evaluate(baseEnv, ast.var_("Nothing"))).toEqual(vcon("Nothing"));
      expect(evaluate(baseEnv, ast.var_("Nil"))).toEqual(vcon("Nil"));
    });

    it("applies constructors to arguments", () => {
      const just42 = ast.app(ast.var_("Just"), ast.num(42));
      const result = evaluate(baseEnv, just42);
      expect(result).toEqual(vcon("Just", [vnum(42)]));
    });

    it("supports nested constructor application", () => {
      const cons = ast.app(ast.app(ast.var_("Cons"), ast.num(1)), ast.var_("Nil"));
      const result = evaluate(baseEnv, cons);
      expect(result).toEqual(vcon("Cons", [vnum(1), vcon("Nil")]));
    });
  });

  describe("pattern matching", () => {
    it("matches wildcard", () => {
      const expr = ast.match(ast.num(42), [ast.case_(ast.pwildcard(), ast.str("matched"))]);
      expect(evaluate(emptyEnv, expr)).toEqual(vstr("matched"));
    });

    it("matches variable and binds", () => {
      const expr = ast.match(ast.num(42), [ast.case_(ast.pvar("x"), ast.var_("x"))]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(42));
    });

    it("matches literal", () => {
      const expr = ast.match(ast.num(1), [
        ast.case_(ast.plit(1), ast.str("one")),
        ast.case_(ast.plit(2), ast.str("two")),
        ast.case_(ast.pwildcard(), ast.str("other")),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vstr("one"));
    });

    it("matches constructor", () => {
      const just42 = ast.app(ast.var_("Just"), ast.num(42));
      const expr = ast.match(just42, [
        ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.binOp("*", ast.var_("x"), ast.num(2))),
        ast.case_(ast.pcon("Nothing", []), ast.num(0)),
      ]);
      expect(evaluate(baseEnv, expr)).toEqual(vnum(84));
    });

    it("matches Nothing", () => {
      const expr = ast.match(ast.var_("Nothing"), [
        ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.var_("x")),
        ast.case_(ast.pcon("Nothing", []), ast.num(0)),
      ]);
      expect(evaluate(baseEnv, expr)).toEqual(vnum(0));
    });

    it("matches tuple pattern", () => {
      const tuple = ast.tuple([ast.num(1), ast.str("hello")]);
      const expr = ast.match(tuple, [
        ast.case_(ast.ptuple([ast.pvar("a"), ast.pvar("b")]), ast.var_("a")),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(1));
    });

    it("matches record pattern", () => {
      const record = ast.record([ast.field("x", ast.num(10)), ast.field("y", ast.num(20))]);
      const expr = ast.match(record, [
        ast.case_(ast.precord([ast.pfield("x", ast.pvar("a"))]), ast.var_("a")),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(10));
    });
  });

  describe("valueToString", () => {
    it("formats numbers", () => {
      expect(valueToString(vnum(42))).toBe("42");
    });

    it("formats strings with quotes", () => {
      expect(valueToString(vstr("hello"))).toBe('"hello"');
    });

    it("formats chars with single quotes", () => {
      expect(valueToString(vchar("a"))).toBe("'a'");
      expect(valueToString(vchar("\n"))).toBe("'\n'");
    });

    it("formats booleans", () => {
      expect(valueToString(vbool(true))).toBe("true");
      expect(valueToString(vbool(false))).toBe("false");
    });

    it("formats closures", () => {
      const closure = evaluate(emptyEnv, ast.abs("x", ast.var_("x")));
      expect(valueToString(closure)).toBe("<function>");
    });

    it("formats constructors", () => {
      expect(valueToString(vcon("Nothing"))).toBe("Nothing");
      expect(valueToString(vcon("Just", [vnum(42)]))).toBe("(Just 42)");
    });
  });

  describe("equality comparisons", () => {
    it("compares booleans with ==", () => {
      expect(evaluate(emptyEnv, ast.binOp("==", ast.bool(true), ast.bool(true)))).toEqual(
        vbool(true),
      );
      expect(evaluate(emptyEnv, ast.binOp("==", ast.bool(true), ast.bool(false)))).toEqual(
        vbool(false),
      );
    });

    it("compares constructors with ==", () => {
      const j1 = ast.app(ast.var_("Just"), ast.num(42));
      const j2 = ast.app(ast.var_("Just"), ast.num(42));
      const j3 = ast.app(ast.var_("Just"), ast.num(99));
      // Just 42 == Just 42
      expect(evaluate(baseEnv, ast.binOp("==", j1, j2))).toEqual(vbool(true));
      // Just 42 == Just 99
      expect(evaluate(baseEnv, ast.binOp("==", j1, j3))).toEqual(vbool(false));
      // Just 42 == Nothing
      expect(evaluate(baseEnv, ast.binOp("==", j1, ast.var_("Nothing")))).toEqual(vbool(false));
    });

    it("compares tuples with ==", () => {
      const t1 = ast.tuple([ast.num(1), ast.num(2)]);
      const t2 = ast.tuple([ast.num(1), ast.num(2)]);
      const t3 = ast.tuple([ast.num(1), ast.num(3)]);
      const t4 = ast.tuple([ast.num(1), ast.num(2), ast.num(3)]);
      expect(evaluate(emptyEnv, ast.binOp("==", t1, t2))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("==", t1, t3))).toEqual(vbool(false));
      // Different lengths
      expect(evaluate(emptyEnv, ast.binOp("==", t1, t4))).toEqual(vbool(false));
    });

    it("compares records with ==", () => {
      const r1 = ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]);
      const r2 = ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]);
      const r3 = ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(3))]);
      const r4 = ast.record([ast.field("x", ast.num(1))]);
      expect(evaluate(emptyEnv, ast.binOp("==", r1, r2))).toEqual(vbool(true));
      expect(evaluate(emptyEnv, ast.binOp("==", r1, r3))).toEqual(vbool(false));
      // Different number of fields
      expect(evaluate(emptyEnv, ast.binOp("==", r1, r4))).toEqual(vbool(false));
    });

    it("compares closures (always false)", () => {
      const f1 = ast.abs("x", ast.var_("x"));
      const f2 = ast.abs("x", ast.var_("x"));
      expect(evaluate(emptyEnv, ast.binOp("==", f1, f2))).toEqual(vbool(false));
    });
  });

  describe("pattern matching edge cases", () => {
    it("matches string literal pattern", () => {
      const expr = ast.match(ast.str("hello"), [
        ast.case_(ast.plit("hello"), ast.num(1)),
        ast.case_(ast.plit("world"), ast.num(2)),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(1));
    });

    it("matches boolean literal pattern", () => {
      const expr = ast.match(ast.bool(true), [
        ast.case_(ast.plit(true), ast.str("yes")),
        ast.case_(ast.plit(false), ast.str("no")),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vstr("yes"));
    });

    it("rejects constructor with wrong number of args", () => {
      const just42 = ast.app(ast.var_("Just"), ast.num(42));
      const expr = ast.match(just42, [
        // Try to match Just with 2 args (should fail)
        ast.case_(ast.pcon("Just", [ast.pvar("x"), ast.pvar("y")]), ast.num(0)),
        ast.case_(ast.pwildcard(), ast.num(99)),
      ]);
      expect(evaluate(baseEnv, expr)).toEqual(vnum(99));
    });

    it("rejects tuple with wrong number of elements", () => {
      const tuple = ast.tuple([ast.num(1), ast.num(2)]);
      const expr = ast.match(tuple, [
        // Try to match 2-tuple with 3-element pattern (should fail)
        ast.case_(ast.ptuple([ast.pvar("a"), ast.pvar("b"), ast.pvar("c")]), ast.num(0)),
        ast.case_(ast.pwildcard(), ast.num(99)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(99));
    });

    it("matches record with missing field pattern", () => {
      const record = ast.record([ast.field("x", ast.num(1))]);
      const expr = ast.match(record, [
        // Try to match record with pattern that expects a missing field
        ast.case_(ast.precord([ast.pfield("y", ast.pvar("a"))]), ast.num(0)),
        ast.case_(ast.pwildcard(), ast.num(99)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(99));
    });

    it("rejects literal pattern when value doesn't match", () => {
      const expr = ast.match(ast.num(5), [
        ast.case_(ast.plit(1), ast.str("one")),
        ast.case_(ast.plit(2), ast.str("two")),
        ast.case_(ast.pwildcard(), ast.str("other")),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vstr("other"));
    });

    it("rejects string literal pattern when value doesn't match", () => {
      const expr = ast.match(ast.str("foo"), [
        ast.case_(ast.plit("bar"), ast.num(1)),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(0));
    });

    it("rejects boolean literal pattern when value doesn't match", () => {
      const expr = ast.match(ast.bool(false), [
        ast.case_(ast.plit(true), ast.num(1)),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(0));
    });

    it("rejects literal pattern when type doesn't match", () => {
      // Matching a number against a string pattern
      const expr = ast.match(ast.num(42), [
        ast.case_(ast.plit("42"), ast.num(1)),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(0));
    });

    it("matches nested record pattern", () => {
      const record = ast.record([ast.field("x", ast.num(10)), ast.field("y", ast.num(20))]);
      const expr = ast.match(record, [
        ast.case_(
          ast.precord([ast.pfield("x", ast.pvar("a")), ast.pfield("y", ast.pvar("b"))]),
          ast.binOp("+", ast.var_("a"), ast.var_("b")),
        ),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(30));
    });

    it("rejects record pattern against non-record value", () => {
      // Matching a number against a record pattern
      const expr = ast.match(ast.num(42), [
        ast.case_(ast.precord([ast.pfield("x", ast.pvar("a"))]), ast.num(1)),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(0));
    });

    it("rejects tuple pattern against non-tuple value", () => {
      // Matching a number against a tuple pattern
      const expr = ast.match(ast.num(42), [
        ast.case_(ast.ptuple([ast.pvar("a"), ast.pvar("b")]), ast.num(1)),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(0));
    });

    it("rejects constructor pattern against non-constructor value", () => {
      // Matching a number against a constructor pattern
      const expr = ast.match(ast.num(42), [
        ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.num(1)),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(0));
    });

    it("fails nested constructor pattern match", () => {
      // Match Just(Just 1) against Just(Just 2)
      const nestedJust = ast.app(ast.var_("Just"), ast.app(ast.var_("Just"), ast.num(1)));
      const expr = ast.match(nestedJust, [
        // Try to match inner value as 2 (should fail)
        ast.case_(ast.pcon("Just", [ast.pcon("Just", [ast.plit(2)])]), ast.num(1)),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(baseEnv, expr)).toEqual(vnum(0));
    });

    it("matches first alternative in or-pattern", () => {
      // Match Nothing against (Nothing | Just Nothing)
      const expr = ast.match(ast.var_("Nothing"), [
        ast.case_(
          ast.por([ast.pcon("Nothing", []), ast.pcon("Just", [ast.pcon("Nothing", [])])]),
          ast.num(1),
        ),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(baseEnv, expr)).toEqual(vnum(1));
    });

    it("matches second alternative in or-pattern", () => {
      // Match Just(Nothing) against (Nothing | Just Nothing)
      const justNothing = ast.app(ast.var_("Just"), ast.var_("Nothing"));
      const expr = ast.match(justNothing, [
        ast.case_(
          ast.por([ast.pcon("Nothing", []), ast.pcon("Just", [ast.pcon("Nothing", [])])]),
          ast.num(1),
        ),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(baseEnv, expr)).toEqual(vnum(1));
    });

    it("rejects or-pattern when no alternative matches", () => {
      // Match Just(Just 1) against (Nothing | Just Nothing) - should fail
      const justJust = ast.app(ast.var_("Just"), ast.app(ast.var_("Just"), ast.num(1)));
      const expr = ast.match(justJust, [
        ast.case_(
          ast.por([ast.pcon("Nothing", []), ast.pcon("Just", [ast.pcon("Nothing", [])])]),
          ast.num(1),
        ),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(baseEnv, expr)).toEqual(vnum(0));
    });

    it("binds variables from or-pattern", () => {
      // Match Just 5 against (Just x | Right x) - x should be 5
      const just5 = ast.app(ast.var_("Just"), ast.num(5));
      const expr = ast.match(just5, [
        ast.case_(
          ast.por([ast.pcon("Just", [ast.pvar("x")]), ast.pcon("Right", [ast.pvar("x")])]),
          ast.var_("x"),
        ),
        ast.case_(ast.pwildcard(), ast.num(0)),
      ]);
      expect(evaluate(baseEnv, expr)).toEqual(vnum(5));
    });

    it("matches multiple literal alternatives", () => {
      // Match 1 against (0 | 1 | 2) => true
      const expr = ast.match(ast.num(1), [
        ast.case_(ast.por([ast.plit(0), ast.plit(1), ast.plit(2)]), ast.bool(true)),
        ast.case_(ast.pwildcard(), ast.bool(false)),
      ]);
      expect(evaluate(emptyEnv, expr)).toEqual(vbool(true));
    });
  });

  describe("qualified variable access", () => {
    it("evaluates qualified variable from environment", () => {
      // Simulate: use Math (..); Math.double 5
      // where double is in the environment
      const env: Env = new Map([["double", vnum(42)]]);
      const expr = ast.qualifiedVar("Math", "double");
      expect(evaluate(env, expr)).toEqual(vnum(42));
    });

    it("dereferences VRef from letrec binding", () => {
      // Simulate module binding through letrec:
      // letRec double = x -> x * 2 in Math.double
      // The binding creates a VRef that must be dereferenced
      const doubleBody = ast.binOp("*", ast.var_("x"), ast.num(2));
      const doubleFn = ast.abs("x", doubleBody);
      const expr = ast.letRec(
        [ast.recBinding("double", doubleFn)],
        ast.app(ast.qualifiedVar("Math", "double"), ast.num(5)),
      );
      expect(evaluate(emptyEnv, expr)).toEqual(vnum(10));
    });

    it("throws error for unimported qualified variable", () => {
      // Math.unknown where 'unknown' is not in the environment
      const expr = ast.qualifiedVar("Math", "unknown");
      expect(() => evaluate(emptyEnv, expr)).toThrow(RuntimeError);
    });
  });
});
