import { describe, expect, it } from "bun:test";
import * as ast from "./ast";
import {
  evaluate,
  valueToString,
  vcon,
  vnum,
  vstr,
  vbool,
  emptyEnv,
  evalPreludeFunction,
  RuntimeError,
  type Env,
} from "./eval";
import { functions } from "./prelude";

// Helper to build a list AST
const list = (...items: ast.Expr[]): ast.Expr => {
  let result: ast.Expr = ast.var_("Nil");
  for (let i = items.length - 1; i >= 0; i--) {
    result = ast.app(ast.app(ast.var_("Cons"), items[i]!), result);
  }
  return result;
};

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
        ast.app(ast.var_("fact"), ast.num(5)),
      );
      expect(evaluate(emptyEnv, factorial)).toEqual(vnum(120));
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

  describe("prelude functions", () => {
    // Load prelude into environment
    let env = baseEnv;
    env = evalPreludeFunction(env, "id", functions.id);
    env = evalPreludeFunction(env, "const", functions.const);
    env = evalPreludeFunction(env, "head", functions.head);
    env = evalPreludeFunction(env, "tail", functions.tail);
    env = evalPreludeFunction(env, "isEmpty", functions.isEmpty);
    env = evalPreludeFunction(env, "length", functions.length);
    env = evalPreludeFunction(env, "map", functions.map);
    env = evalPreludeFunction(env, "filter", functions.filter);
    env = evalPreludeFunction(env, "foldr", functions.foldr);
    env = evalPreludeFunction(env, "foldl", functions.foldl);
    env = evalPreludeFunction(env, "reverse", functions.reverse);
    env = evalPreludeFunction(env, "concat", functions.concat);

    const myList = list(ast.num(1), ast.num(2), ast.num(3));

    it("id returns its argument", () => {
      const result = evaluate(env, ast.app(ast.var_("id"), ast.num(42)));
      expect(result).toEqual(vnum(42));
    });

    it("const returns first argument", () => {
      const result = evaluate(env, ast.app(ast.app(ast.var_("const"), ast.num(1)), ast.num(2)));
      expect(result).toEqual(vnum(1));
    });

    it("head returns Just first element", () => {
      const result = evaluate(env, ast.app(ast.var_("head"), myList));
      expect(result).toEqual(vcon("Just", [vnum(1)]));
    });

    it("head returns Nothing for empty list", () => {
      const result = evaluate(env, ast.app(ast.var_("head"), ast.var_("Nil")));
      expect(result).toEqual(vcon("Nothing"));
    });

    it("isEmpty returns true for empty list", () => {
      const result = evaluate(env, ast.app(ast.var_("isEmpty"), ast.var_("Nil")));
      expect(result).toEqual(vbool(true));
    });

    it("isEmpty returns false for non-empty list", () => {
      const result = evaluate(env, ast.app(ast.var_("isEmpty"), myList));
      expect(result).toEqual(vbool(false));
    });

    it("length counts elements", () => {
      const result = evaluate(env, ast.app(ast.var_("length"), myList));
      expect(result).toEqual(vnum(3));
    });

    it("map transforms elements", () => {
      const double = ast.abs("x", ast.binOp("*", ast.var_("x"), ast.num(2)));
      const result = evaluate(env, ast.app(ast.app(ast.var_("map"), double), myList));
      expect(result).toEqual(
        vcon("Cons", [vnum(2), vcon("Cons", [vnum(4), vcon("Cons", [vnum(6), vcon("Nil")])])]),
      );
    });

    it("filter keeps matching elements", () => {
      const greaterThan1 = ast.abs("x", ast.binOp(">", ast.var_("x"), ast.num(1)));
      const result = evaluate(env, ast.app(ast.app(ast.var_("filter"), greaterThan1), myList));
      expect(result).toEqual(vcon("Cons", [vnum(2), vcon("Cons", [vnum(3), vcon("Nil")])]));
    });

    it("foldr accumulates from right", () => {
      const add = ast.abs("a", ast.abs("b", ast.binOp("+", ast.var_("a"), ast.var_("b"))));
      const result = evaluate(
        env,
        ast.app(ast.app(ast.app(ast.var_("foldr"), add), ast.num(0)), myList),
      );
      expect(result).toEqual(vnum(6));
    });

    it("reverse reverses the list", () => {
      const result = evaluate(env, ast.app(ast.var_("reverse"), myList));
      expect(result).toEqual(
        vcon("Cons", [vnum(3), vcon("Cons", [vnum(2), vcon("Cons", [vnum(1), vcon("Nil")])])]),
      );
    });
  });

  describe("valueToString", () => {
    it("formats numbers", () => {
      expect(valueToString(vnum(42))).toBe("42");
    });

    it("formats strings with quotes", () => {
      expect(valueToString(vstr("hello"))).toBe('"hello"');
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
});
