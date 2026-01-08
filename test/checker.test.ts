import { describe, expect, it } from "bun:test";
import * as ast from "../src/ast";
import { bindWithConstructors } from "../src/binder";
import {
  baseEnv,
  check,
  type CheckOutput,
  mergeEnvs,
  mergeRegistries,
  processDataDecl,
  processDeclarations,
  processModule,
  type TypeEnv,
  type ConstructorRegistry,
  typeToString,
} from "../src/checker";

// Helper to run the full pipeline (bind + check)
// Extracts constructor names from the registry (which maps type name -> constructor names)
const infer = (env: TypeEnv, registry: ConstructorRegistry, expr: ast.Expr): CheckOutput => {
  // Extract all constructor names from registry
  const constructors: string[] = [];
  for (const [, conNames] of registry) {
    for (const name of conNames) {
      constructors.push(name);
    }
  }
  const bindResult = bindWithConstructors(constructors, expr);
  const checkResult = check(env, registry, expr, bindResult.symbols);
  // Combine diagnostics from both phases
  return {
    ...checkResult,
    diagnostics: [...bindResult.diagnostics, ...checkResult.diagnostics],
  };
};

// Helper to create common ADT environments
const createMaybeEnv = () => {
  const maybeDecl = ast.dataDecl(
    "Maybe",
    ["a"],
    [ast.conDecl("Nothing", []), ast.conDecl("Just", [ast.tyvar("a")])],
  );
  return processDataDecl(maybeDecl);
};

const createListEnv = () => {
  const listDecl = ast.dataDecl(
    "List",
    ["a"],
    [
      ast.conDecl("Nil", []),
      ast.conDecl("Cons", [ast.tyvar("a"), ast.tyapp(ast.tycon("List"), ast.tyvar("a"))]),
    ],
  );
  return processDataDecl(listDecl);
};

const createEitherEnv = () => {
  const eitherDecl = ast.dataDecl(
    "Either",
    ["a", "b"],
    [ast.conDecl("Left", [ast.tyvar("a")]), ast.conDecl("Right", [ast.tyvar("b")])],
  );
  return processDataDecl(eitherDecl);
};

describe("Type Inference", () => {
  describe("literals", () => {
    it("infers number type", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.num(42));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers boolean type for true", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.bool(true));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("boolean");
    });

    it("infers boolean type for false", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.bool(false));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("boolean");
    });

    it("infers string type", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.str("hello"));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("string");
    });

    it("infers empty string type", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.str(""));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("string");
    });
  });

  describe("variables", () => {
    it("reports error for undefined variable", () => {
      const { diagnostics } = infer(baseEnv, new Map(), ast.var_("undefined_var"));
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("Unknown variable");
    });

    it("infers type from environment", () => {
      const [maybeEnv, maybeReg] = createMaybeEnv();
      const env = mergeEnvs(baseEnv, maybeEnv);
      const { type, diagnostics } = infer(env, maybeReg, ast.var_("Nothing"));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toMatch(/Maybe t\d+/);
    });

    it("instantiates polymorphic types with fresh variables", () => {
      const [maybeEnv] = createMaybeEnv();
      // Just : a -> Maybe a is polymorphic
      const { type: type1 } = infer(maybeEnv, new Map(), ast.var_("Just"));
      const { type: type2 } = infer(maybeEnv, new Map(), ast.var_("Just"));
      // Both should have function types, but potentially different variable names
      expect(typeToString(type1)).toContain("->");
      expect(typeToString(type2)).toContain("->");
    });
  });

  describe("lambdas", () => {
    it("infers identity function type", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.abs("x", ast.var_("x")));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toMatch(/t\d+ -> t\d+/);
    });

    it("infers type from body usage", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.abs("x", ast.binOp("+", ast.var_("x"), ast.num(1))),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number -> number");
    });

    it("infers curried function type", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y")))),
      );
      expect(diagnostics).toHaveLength(0);
      // Should be t -> t -> t with Add constraint
      expect(typeToString(type)).toMatch(/t\d+ -> t\d+ -> t\d+/);
    });

    it("infers lambda returning constant", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.abs("x", ast.num(42)));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toMatch(/t\d+ -> number/);
    });

    it("infers deeply nested lambdas", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.abs("a", ast.abs("b", ast.abs("c", ast.var_("b")))),
      );
      expect(diagnostics).toHaveLength(0);
      // Should be a -> b -> c -> b
      expect(typeToString(type)).toMatch(/t\d+ -> t\d+ -> t\d+ -> t\d+/);
    });
  });

  describe("function application", () => {
    it("infers application result type", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.app(ast.abs("x", ast.binOp("+", ast.var_("x"), ast.num(1))), ast.num(5)),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers curried application", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.app(
          ast.app(
            ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y")))),
            ast.num(1),
          ),
          ast.num(2),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers partial application", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.app(
          ast.abs("x", ast.abs("y", ast.binOp("+", ast.var_("x"), ast.var_("y")))),
          ast.num(1),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number -> number");
    });

    it("reports type error for wrong argument type", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.app(ast.abs("x", ast.binOp("+", ast.var_("x"), ast.num(1))), ast.str("hello")),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error when applying non-function", () => {
      const { diagnostics } = infer(baseEnv, new Map(), ast.app(ast.num(42), ast.num(1)));
      expect(diagnostics.length).toBeGreaterThan(0);
    });
  });

  describe("binary operators", () => {
    describe("arithmetic", () => {
      it("infers addition of numbers", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("+", ast.num(1), ast.num(2)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers string concatenation", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("+", ast.str("a"), ast.str("b")),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("string");
      });

      it("infers subtraction", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("-", ast.num(5), ast.num(3)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers multiplication", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("*", ast.num(2), ast.num(3)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers division", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("/", ast.num(10), ast.num(2)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers + string concatenation", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("+", ast.str("hello"), ast.str("world")),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("string");
      });

      it("reports error for subtraction with strings", () => {
        const { diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("-", ast.str("a"), ast.str("b")),
        );
        expect(diagnostics.length).toBeGreaterThan(0);
      });
    });

    describe("comparison", () => {
      it("infers less than", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("<", ast.num(1), ast.num(2)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });

      it("infers less than or equal", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("<=", ast.num(1), ast.num(2)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });

      it("infers greater than", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp(">", ast.num(2), ast.num(1)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });

      it("infers greater than or equal", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp(">=", ast.num(2), ast.num(1)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });

      it("allows string comparison", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("<", ast.str("a"), ast.str("b")),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });

      it("reports error for boolean comparison", () => {
        const { diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("<", ast.bool(true), ast.bool(false)),
        );
        expect(diagnostics.length).toBeGreaterThan(0);
      });
    });

    describe("equality", () => {
      it("infers equality on numbers", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("==", ast.num(1), ast.num(2)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });

      it("infers inequality on strings", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("!=", ast.str("a"), ast.str("b")),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });

      it("infers equality on booleans", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("==", ast.bool(true), ast.bool(false)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });

      it("reports error for mismatched types", () => {
        const { diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("==", ast.num(1), ast.str("1")),
        );
        expect(diagnostics.length).toBeGreaterThan(0);
      });
    });

    describe("complex expressions", () => {
      it("infers nested arithmetic", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("+", ast.binOp("*", ast.num(2), ast.num(3)), ast.num(4)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers combined comparison and arithmetic", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.binOp("<", ast.binOp("+", ast.num(1), ast.num(2)), ast.num(5)),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("boolean");
      });
    });
  });

  describe("if expressions", () => {
    it("infers if with number branches", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.if_(ast.bool(true), ast.num(1), ast.num(2)),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers if with string branches", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.if_(ast.bool(false), ast.str("yes"), ast.str("no")),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("string");
    });

    it("infers if with complex condition", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.if_(ast.binOp(">", ast.num(5), ast.num(3)), ast.str("big"), ast.str("small")),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("string");
    });

    it("infers nested if", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.if_(ast.bool(true), ast.if_(ast.bool(false), ast.num(1), ast.num(2)), ast.num(3)),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("reports error for non-boolean condition", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.if_(ast.num(1), ast.num(2), ast.num(3)),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
    });

    it("reports error for mismatched branch types", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.if_(ast.bool(true), ast.num(1), ast.str("two")),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
    });
  });

  describe("let expressions", () => {
    it("infers simple let", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_("x", ast.num(42), ast.var_("x")),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers let with usage in body", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_("x", ast.num(10), ast.binOp("+", ast.var_("x"), ast.num(5))),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers nested let", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "x",
          ast.num(1),
          ast.let_("y", ast.num(2), ast.binOp("+", ast.var_("x"), ast.var_("y"))),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("supports shadowing", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_("x", ast.num(1), ast.let_("x", ast.str("hello"), ast.var_("x"))),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("string");
    });

    describe("let polymorphism", () => {
      it("allows polymorphic usage of identity", () => {
        // let id = x => x in (id 1, id "hello")
        // This should type check because id is polymorphic
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.let_(
            "id",
            ast.abs("x", ast.var_("x")),
            ast.tuple([
              ast.app(ast.var_("id"), ast.num(1)),
              ast.app(ast.var_("id"), ast.str("hello")),
            ]),
          ),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("(number, string)");
      });

      it("generalizes function types", () => {
        // let f = x => x in f
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.let_("f", ast.abs("x", ast.var_("x")), ast.var_("f")),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toMatch(/t\d+ -> t\d+/);
      });
    });
  });

  describe("let rec expressions", () => {
    it("infers recursive function", () => {
      // let rec fact n = if n == 0 then 1 else n * fact (n - 1) in fact 5
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.letRec(
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
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers recursive list function", () => {
      const [listEnv, listReg] = createListEnv();
      const env = mergeEnvs(baseEnv, listEnv);

      // let rec length xs = match xs with | Nil => 0 | Cons _ rest => 1 + length rest end
      const { type, diagnostics } = infer(
        env,
        listReg,
        ast.letRec(
          [
            ast.recBinding(
              "length",
              ast.abs(
                "xs",
                ast.match(ast.var_("xs"), [
                  ast.case_(ast.pcon("Nil", []), ast.num(0)),
                  ast.case_(
                    ast.pcon("Cons", [ast.pwildcard(), ast.pvar("rest")]),
                    ast.binOp("+", ast.num(1), ast.app(ast.var_("length"), ast.var_("rest"))),
                  ),
                ]),
              ),
            ),
          ],
          ast.var_("length"),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toMatch(/List t\d+ -> number/);
    });

    it("allows recursive reference in body", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.letRec(
          [ast.recBinding("forever", ast.abs("x", ast.app(ast.var_("forever"), ast.var_("x"))))],
          ast.var_("forever"),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toMatch(/t\d+ -> t\d+/);
    });
  });

  describe("tuples", () => {
    it("infers 2-tuple", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.tuple([ast.num(1), ast.str("hello")]),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("(number, string)");
    });

    it("infers 3-tuple", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.tuple([ast.num(1), ast.str("hello"), ast.bool(true)]),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("(number, string, boolean)");
    });

    it("infers nested tuple", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.tuple([ast.tuple([ast.num(1), ast.num(2)]), ast.str("nested")]),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("((number, number), string)");
    });

    it("unwraps single-element tuple", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.tuple([ast.num(42)]));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("reports error for empty tuple", () => {
      const { diagnostics } = infer(baseEnv, new Map(), ast.tuple([]));
      expect(diagnostics.length).toBeGreaterThan(0);
    });
  });

  describe("records", () => {
    it("infers simple record", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.record([ast.field("x", ast.num(1))]),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("{ x: number }");
    });

    it("infers record with multiple fields", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.str("hello"))]),
      );
      expect(diagnostics).toHaveLength(0);
      const typeStr = typeToString(type);
      expect(typeStr).toContain("x: number");
      expect(typeStr).toContain("y: string");
    });

    it("infers empty record", () => {
      const { type, diagnostics } = infer(baseEnv, new Map(), ast.record([]));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("{  }");
    });

    it("infers nested record", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.record([ast.field("inner", ast.record([ast.field("value", ast.num(42))]))]),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toContain("inner:");
      expect(typeToString(type)).toContain("value: number");
    });
  });

  describe("field access", () => {
    it("infers field access on literal record", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.fieldAccess(ast.record([ast.field("x", ast.num(42))]), "x"),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers nested field access", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.fieldAccess(
          ast.record([ast.field("inner", ast.record([ast.field("value", ast.num(99))]))]),
          "inner",
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toContain("value: number");
    });

    it("reports error for missing field on closed record", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.fieldAccess(ast.record([ast.field("x", ast.num(1))]), "y"),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("no field");
    });

    it("reports error for field access on non-record", () => {
      const { diagnostics } = infer(baseEnv, new Map(), ast.fieldAccess(ast.num(42), "x"));
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("non-record");
    });

    describe("row polymorphism", () => {
      it("infers polymorphic field accessor", () => {
        // r => r.x should have type { x: t | ρ } -> t
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toContain("->");
        expect(typeToString(type)).toContain("x:");
      });

      it("applies polymorphic accessor to record with extra fields", () => {
        // (r => r.x) { x = 1, y = "hello" } should return number
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.app(
            ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
            ast.record([ast.field("x", ast.num(42)), ast.field("y", ast.str("hello"))]),
          ),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers function accessing multiple fields", () => {
        // r => r.x + r.y
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.abs(
            "r",
            ast.binOp(
              "+",
              ast.fieldAccess(ast.var_("r"), "x"),
              ast.fieldAccess(ast.var_("r"), "y"),
            ),
          ),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toContain("x:");
        expect(typeToString(type)).toContain("y:");
      });
    });
  });

  describe("tuple indexing", () => {
    it("infers first element of pair", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.tupleIndex(ast.tuple([ast.num(1), ast.str("hello")]), 0),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers second element of pair", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.tupleIndex(ast.tuple([ast.num(1), ast.str("hello")]), 1),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("string");
    });

    it("infers element of triple", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.tupleIndex(ast.tuple([ast.num(1), ast.str("hello"), ast.bool(true)]), 2),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("boolean");
    });

    it("reports error for out of bounds index", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.tupleIndex(ast.tuple([ast.num(1), ast.num(2)]), 5),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("out of bounds");
    });

    it("reports error for tuple index on non-tuple", () => {
      const { diagnostics } = infer(baseEnv, new Map(), ast.tupleIndex(ast.num(42), 0));
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("non-tuple");
    });

    it("infers nested tuple indexing", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.tupleIndex(
          ast.tupleIndex(ast.tuple([ast.tuple([ast.num(1), ast.str("inner")]), ast.bool(true)]), 0),
          1,
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("string");
    });
  });

  describe("pattern matching", () => {
    describe("basic patterns", () => {
      it("infers match with variable pattern", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.match(ast.num(42), [ast.case_(ast.pvar("x"), ast.var_("x"))]),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers match with wildcard pattern", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.match(ast.num(42), [ast.case_(ast.pwildcard(), ast.str("matched"))]),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("string");
      });

      it("infers match with literal pattern", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.match(ast.num(42), [
            ast.case_(ast.plit(0), ast.str("zero")),
            ast.case_(ast.pwildcard(), ast.str("other")),
          ]),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("string");
      });
    });

    describe("constructor patterns", () => {
      it("infers match on Maybe", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        const { type, diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.var_("x")),
            ast.case_(ast.pcon("Nothing", []), ast.num(0)),
          ]),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers match on List", () => {
        const [listEnv, listReg] = createListEnv();
        const env = mergeEnvs(baseEnv, listEnv);

        const { type, diagnostics } = infer(
          env,
          listReg,
          ast.match(ast.var_("Nil"), [
            ast.case_(ast.pcon("Nil", []), ast.num(0)),
            ast.case_(ast.pcon("Cons", [ast.pvar("x"), ast.pwildcard()]), ast.var_("x")),
          ]),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("reports error for unknown constructor", () => {
        const { diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.match(ast.num(42), [ast.case_(ast.pcon("Unknown", [ast.pvar("x")]), ast.var_("x"))]),
        );
        expect(diagnostics.length).toBeGreaterThan(0);
        expect(diagnostics[0]!.message).toContain("Unknown constructor");
      });

      it("reports error for wrong arity", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.pcon("Just", [ast.pvar("x"), ast.pvar("y")]), ast.var_("x")),
          ]),
        );
        expect(diagnostics.length).toBeGreaterThan(0);
      });
    });

    describe("tuple patterns", () => {
      it("infers match with tuple pattern", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.match(ast.tuple([ast.num(1), ast.str("hello")]), [
            ast.case_(ast.ptuple([ast.pvar("n"), ast.pvar("s")]), ast.var_("n")),
          ]),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("reports error for tuple arity mismatch", () => {
        const { diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.match(ast.tuple([ast.num(1), ast.num(2)]), [
            ast.case_(ast.ptuple([ast.pvar("a"), ast.pvar("b"), ast.pvar("c")]), ast.var_("a")),
          ]),
        );
        expect(diagnostics.length).toBeGreaterThan(0);
      });
    });

    describe("record patterns", () => {
      it("infers match with record pattern", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.match(ast.record([ast.field("x", ast.num(42)), ast.field("y", ast.str("hello"))]), [
            ast.case_(ast.precord([ast.pfield("x", ast.pvar("a"))]), ast.var_("a")),
          ]),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("infers nested record pattern", () => {
        const { type, diagnostics } = infer(
          baseEnv,
          new Map(),
          ast.match(
            ast.record([ast.field("inner", ast.record([ast.field("value", ast.num(99))]))]),
            [
              ast.case_(
                ast.precord([
                  ast.pfield("inner", ast.precord([ast.pfield("value", ast.pvar("v"))])),
                ]),
                ast.var_("v"),
              ),
            ],
          ),
        );
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });
    });

    describe("exhaustiveness checking", () => {
      it("reports non-exhaustive match on Maybe", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.var_("x")),
            // Missing: Nothing case
          ]),
        );
        expect(diagnostics.length).toBeGreaterThan(0);
        expect(diagnostics[0]!.message).toContain("Non-exhaustive");
        expect(diagnostics[0]!.message).toContain("Nothing");
      });

      it("accepts exhaustive match on Maybe", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.var_("x")),
            ast.case_(ast.pcon("Nothing", []), ast.num(0)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors).toHaveLength(0);
      });

      it("accepts wildcard as catch-all", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.var_("x")),
            ast.case_(ast.pwildcard(), ast.num(0)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors).toHaveLength(0);
      });

      it("accepts variable pattern as catch-all", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.pvar("m"), ast.num(42)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors).toHaveLength(0);
      });

      it("reports multiple missing constructors", () => {
        const [listEnv, listReg] = createListEnv();
        const env = mergeEnvs(baseEnv, listEnv);

        const { diagnostics } = infer(
          env,
          listReg,
          ast.match(ast.var_("Nil"), [
            // Missing both Nil and Cons
          ]),
        );
        expect(diagnostics.length).toBeGreaterThan(0);
      });

      it("reports missing nested pattern (Just Nothing)", () => {
        // Create Maybe (Maybe a) by using Maybe twice
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        // match x with
        // | Just (Just y) => y
        // | Nothing => 0
        // end
        // Missing: Just Nothing
        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.var_("Nothing"), [
            ast.case_(ast.pcon("Just", [ast.pcon("Just", [ast.pvar("y")])]), ast.var_("y")),
            ast.case_(ast.pcon("Nothing", []), ast.num(0)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors.length).toBeGreaterThan(0);
        expect(exhaustivenessErrors[0]!.message).toContain("Just Nothing");
      });

      it("accepts exhaustive nested patterns", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        // match x with
        // | Just (Just y) => y
        // | Just Nothing => 0
        // | Nothing => 0
        // end
        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.var_("Nothing"), [
            ast.case_(ast.pcon("Just", [ast.pcon("Just", [ast.pvar("y")])]), ast.var_("y")),
            ast.case_(ast.pcon("Just", [ast.pcon("Nothing", [])]), ast.num(0)),
            ast.case_(ast.pcon("Nothing", []), ast.num(0)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors).toHaveLength(0);
      });

      it("accepts nested wildcard as catch-all", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        // match x with
        // | Just (Just y) => y
        // | Just _ => 0     -- catches Just Nothing
        // | Nothing => 0
        // end
        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.var_("Nothing"), [
            ast.case_(ast.pcon("Just", [ast.pcon("Just", [ast.pvar("y")])]), ast.var_("y")),
            ast.case_(ast.pcon("Just", [ast.pwildcard()]), ast.num(0)),
            ast.case_(ast.pcon("Nothing", []), ast.num(0)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors).toHaveLength(0);
      });

      it("reports missing deeply nested pattern", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        // match x with
        // | Just (Just (Just z)) => z
        // | Just (Just Nothing) => 0
        // | Nothing => 0
        // end
        // Missing: Just Nothing (the middle layer)
        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.var_("Nothing"), [
            ast.case_(
              ast.pcon("Just", [ast.pcon("Just", [ast.pcon("Just", [ast.pvar("z")])])]),
              ast.var_("z"),
            ),
            ast.case_(ast.pcon("Just", [ast.pcon("Just", [ast.pcon("Nothing", [])])]), ast.num(0)),
            ast.case_(ast.pcon("Nothing", []), ast.num(0)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors.length).toBeGreaterThan(0);
        expect(exhaustivenessErrors[0]!.message).toContain("Just Nothing");
      });

      it("accepts exhaustive match with qualified patterns", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        // match Just 42 with
        // | Maybe.Just x => x
        // | Maybe.Nothing => 0
        // end
        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.qualifiedPCon("Maybe", "Just", [ast.pvar("x")]), ast.var_("x")),
            ast.case_(ast.qualifiedPCon("Maybe", "Nothing", []), ast.num(0)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors).toHaveLength(0);
      });

      it("reports non-exhaustive match with qualified patterns", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        // match Just 42 with
        // | Maybe.Just x => x
        // end (missing Maybe.Nothing)
        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.qualifiedPCon("Maybe", "Just", [ast.pvar("x")]), ast.var_("x")),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors.length).toBeGreaterThan(0);
        expect(exhaustivenessErrors[0]!.message).toContain("Nothing");
      });

      it("accepts mixed qualified and unqualified patterns", () => {
        const [maybeEnv, maybeReg] = createMaybeEnv();
        const env = mergeEnvs(baseEnv, maybeEnv);

        // match Just 42 with
        // | Maybe.Just x => x
        // | Nothing => 0
        // end
        const { diagnostics } = infer(
          env,
          maybeReg,
          ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
            ast.case_(ast.qualifiedPCon("Maybe", "Just", [ast.pvar("x")]), ast.var_("x")),
            ast.case_(ast.pcon("Nothing", []), ast.num(0)),
          ]),
        );
        const exhaustivenessErrors = diagnostics.filter((d) =>
          d.message.includes("Non-exhaustive"),
        );
        expect(exhaustivenessErrors).toHaveLength(0);
      });
    });

    describe("empty match", () => {
      it("reports error for match with no cases", () => {
        const { diagnostics } = infer(baseEnv, new Map(), ast.match(ast.num(42), []));
        expect(diagnostics.length).toBeGreaterThan(0);
        expect(diagnostics[0]!.message).toContain("at least one case");
      });
    });

    describe("pattern guards", () => {
      it("infers match with pattern guard", () => {
        // match 5 with | n if n > 0 => n | _ => 0 end
        const expr = ast.match(ast.num(5), [
          ast.case_(ast.pvar("n"), ast.var_("n"), ast.binOp(">", ast.var_("n"), ast.num(0))),
          ast.case_(ast.pwildcard(), ast.num(0)),
        ]);
        const { type, diagnostics } = infer(baseEnv, new Map(), expr);
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });

      it("reports error for non-boolean guard", () => {
        // match 5 with | n if n => n | _ => 0 end (guard is number, not boolean)
        const expr = ast.match(ast.num(5), [
          ast.case_(ast.pvar("n"), ast.var_("n"), ast.var_("n")),
          ast.case_(ast.pwildcard(), ast.num(0)),
        ]);
        const { diagnostics } = infer(baseEnv, new Map(), expr);
        expect(diagnostics.length).toBeGreaterThan(0);
        expect(diagnostics[0]!.message).toContain("Type mismatch");
      });

      it("guard can access pattern bindings", () => {
        // match (1, 2) with | (a, b) if a < b => a | _ => 0 end
        const expr = ast.match(ast.tuple([ast.num(1), ast.num(2)]), [
          ast.case_(
            ast.ptuple([ast.pvar("a"), ast.pvar("b")]),
            ast.var_("a"),
            ast.binOp("<", ast.var_("a"), ast.var_("b")),
          ),
          ast.case_(ast.pwildcard(), ast.num(0)),
        ]);
        const { type, diagnostics } = infer(baseEnv, new Map(), expr);
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });
    });

    describe("as-patterns", () => {
      it("infers match with as-pattern", () => {
        // match (1, 2) with | (a, b) as whole => whole end
        const expr = ast.match(ast.tuple([ast.num(1), ast.num(2)]), [
          ast.case_(
            ast.pas(ast.ptuple([ast.pvar("a"), ast.pvar("b")]), "whole"),
            ast.var_("whole"),
          ),
        ]);
        const { type, diagnostics } = infer(baseEnv, new Map(), expr);
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("(number, number)");
      });

      it("as-binding and inner bindings are both in scope", () => {
        // match (1, 2) with | (a, b) as whole => a end
        const expr = ast.match(ast.tuple([ast.num(1), ast.num(2)]), [
          ast.case_(
            ast.pas(ast.ptuple([ast.pvar("a"), ast.pvar("b")]), "whole"),
            ast.binOp("+", ast.var_("a"), ast.var_("b")),
          ),
        ]);
        const { type, diagnostics } = infer(baseEnv, new Map(), expr);
        expect(diagnostics).toHaveLength(0);
        expect(typeToString(type)).toBe("number");
      });
    });
  });

  describe("data declarations", () => {
    it("processes Maybe declaration", () => {
      const [env, registry] = createMaybeEnv();

      expect(env.has("Nothing")).toBe(true);
      expect(env.has("Just")).toBe(true);
      expect(registry.has("Maybe")).toBe(true);
      expect(registry.get("Maybe")).toEqual(["Nothing", "Just"]);
    });

    it("processes List declaration", () => {
      const [env, registry] = createListEnv();

      expect(env.has("Nil")).toBe(true);
      expect(env.has("Cons")).toBe(true);
      expect(registry.has("List")).toBe(true);
      expect(registry.get("List")).toEqual(["Nil", "Cons"]);
    });

    it("processes Either declaration", () => {
      const [env, registry] = createEitherEnv();

      expect(env.has("Left")).toBe(true);
      expect(env.has("Right")).toBe(true);
      expect(registry.has("Either")).toBe(true);
    });

    it("infers nullary constructor", () => {
      const [maybeEnv, maybeReg] = createMaybeEnv();
      const env = mergeEnvs(baseEnv, maybeEnv);

      const { type, diagnostics } = infer(env, maybeReg, ast.var_("Nothing"));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toMatch(/Maybe t\d+/);
    });

    it("infers unary constructor application", () => {
      const [maybeEnv, maybeReg] = createMaybeEnv();
      const env = mergeEnvs(baseEnv, maybeEnv);

      const { type, diagnostics } = infer(env, maybeReg, ast.app(ast.var_("Just"), ast.num(42)));
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("Maybe number");
    });

    it("infers binary constructor application", () => {
      const [listEnv, listReg] = createListEnv();
      const env = mergeEnvs(baseEnv, listEnv);

      const { type, diagnostics } = infer(
        env,
        listReg,
        ast.app(ast.app(ast.var_("Cons"), ast.num(1)), ast.var_("Nil")),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("List number");
    });
  });

  describe("type class constraints", () => {
    it("enforces Eq constraint", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.binOp("==", ast.abs("x", ast.var_("x")), ast.abs("y", ast.var_("y"))),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("Eq");
    });

    it("enforces Ord constraint", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.binOp("<", ast.abs("x", ast.var_("x")), ast.abs("y", ast.var_("y"))),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("Ord");
    });
  });

  describe("environment utilities", () => {
    it("merges multiple environments", () => {
      const [maybeEnv] = createMaybeEnv();
      const [listEnv] = createListEnv();
      const merged = mergeEnvs(maybeEnv, listEnv);

      expect(merged.has("Just")).toBe(true);
      expect(merged.has("Nothing")).toBe(true);
      expect(merged.has("Cons")).toBe(true);
      expect(merged.has("Nil")).toBe(true);
    });

    it("merges multiple registries", () => {
      const [, maybeReg] = createMaybeEnv();
      const [, listReg] = createListEnv();
      const merged = mergeRegistries(maybeReg, listReg);

      expect(merged.has("Maybe")).toBe(true);
      expect(merged.has("List")).toBe(true);
    });

    it("later environments override earlier ones", () => {
      const env1 = new Map([
        ["x", { vars: [], constraints: [], type: { kind: "TCon" as const, name: "number" } }],
      ]);
      const env2 = new Map([
        ["x", { vars: [], constraints: [], type: { kind: "TCon" as const, name: "string" } }],
      ]);
      const merged = mergeEnvs(env1, env2);

      const scheme = merged.get("x");
      expect(scheme?.type).toEqual({ kind: "TCon", name: "string" });
    });
  });

  describe("unification edge cases", () => {
    it("detects infinite type (occurs check)", () => {
      // f f should fail occurs check
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_("f", ast.abs("x", ast.app(ast.var_("x"), ast.var_("x"))), ast.var_("f")),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("Infinite type");
    });

    it("unifies identical types", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.if_(ast.bool(true), ast.num(1), ast.num(2)),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });
  });

  describe("complex programs", () => {
    it("infers fibonacci function", () => {
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.letRec(
          [
            ast.recBinding(
              "fib",
              ast.abs(
                "n",
                ast.if_(
                  ast.binOp("<=", ast.var_("n"), ast.num(1)),
                  ast.var_("n"),
                  ast.binOp(
                    "+",
                    ast.app(ast.var_("fib"), ast.binOp("-", ast.var_("n"), ast.num(1))),
                    ast.app(ast.var_("fib"), ast.binOp("-", ast.var_("n"), ast.num(2))),
                  ),
                ),
              ),
            ),
          ],
          ast.app(ast.var_("fib"), ast.num(10)),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers map-like function on custom ADT", () => {
      const [maybeEnv, maybeReg] = createMaybeEnv();
      const env = mergeEnvs(baseEnv, maybeEnv);

      // let mapMaybe f m = match m with | Nothing => Nothing | Just x => Just (f x) end
      const { type, diagnostics } = infer(
        env,
        maybeReg,
        ast.let_(
          "mapMaybe",
          ast.abs(
            "f",
            ast.abs(
              "m",
              ast.match(ast.var_("m"), [
                ast.case_(ast.pcon("Nothing", []), ast.var_("Nothing")),
                ast.case_(
                  ast.pcon("Just", [ast.pvar("x")]),
                  ast.app(ast.var_("Just"), ast.app(ast.var_("f"), ast.var_("x"))),
                ),
              ]),
            ),
          ),
          ast.var_("mapMaybe"),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toContain("->");
      expect(typeToString(type)).toContain("Maybe");
    });

    it("infers compose function", () => {
      // compose f g x = f (g x)
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "compose",
          ast.abs(
            "f",
            ast.abs(
              "g",
              ast.abs("x", ast.app(ast.var_("f"), ast.app(ast.var_("g"), ast.var_("x")))),
            ),
          ),
          ast.var_("compose"),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toContain("->");
    });

    it("infers flip function", () => {
      // flip f x y = f y x
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "flip",
          ast.abs(
            "f",
            ast.abs(
              "x",
              ast.abs("y", ast.app(ast.app(ast.var_("f"), ast.var_("y")), ast.var_("x"))),
            ),
          ),
          ast.var_("flip"),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toContain("->");
    });
  });

  describe("typeToString", () => {
    it("formats function types with parentheses correctly", () => {
      // (a -> b) -> c should show parentheses around a -> b
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.abs("f", ast.abs("x", ast.app(ast.var_("f"), ast.var_("x")))),
      );
      expect(diagnostics).toHaveLength(0);
      const str = typeToString(type);
      expect(str).toMatch(/\(t\d+ -> t\d+\) -> t\d+ -> t\d+/);
    });

    it("formats type applications", () => {
      const [listEnv, listReg] = createListEnv();
      const env = mergeEnvs(baseEnv, listEnv);

      const { type, diagnostics } = infer(
        env,
        listReg,
        ast.app(ast.app(ast.var_("Cons"), ast.num(1)), ast.var_("Nil")),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("List number");
    });
  });

  describe("tuple unification edge cases", () => {
    it("reports error for tuple arity mismatch in unification", () => {
      // if true then (1, 2) else (1, 2, 3) - mismatched tuple arities
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.if_(
          ast.bool(true),
          ast.tuple([ast.num(1), ast.num(2)]),
          ast.tuple([ast.num(1), ast.num(2), ast.num(3)]),
        ),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("arity");
    });

    it("unifies tuples element-wise", () => {
      // let f = x => if true then x else (1, 2) in f (3, 4)
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "f",
          ast.abs("x", ast.if_(ast.bool(true), ast.var_("x"), ast.tuple([ast.num(1), ast.num(2)]))),
          ast.app(ast.var_("f"), ast.tuple([ast.num(3), ast.num(4)])),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("(number, number)");
    });
  });

  describe("record unification edge cases", () => {
    it("unifies two open records with row variables", () => {
      // fn that accepts any record with x, applied to record with x and y
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "getX",
          ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
          ast.let_(
            "getY",
            ast.abs("r", ast.fieldAccess(ast.var_("r"), "y")),
            // Apply both to same record to unify row variables
            ast.let_(
              "both",
              ast.abs(
                "r",
                ast.tuple([
                  ast.app(ast.var_("getX"), ast.var_("r")),
                  ast.app(ast.var_("getY"), ast.var_("r")),
                ]),
              ),
              ast.app(
                ast.var_("both"),
                ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]),
              ),
            ),
          ),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("(number, number)");
    });

    it("reports error for missing field on closed record during unification", () => {
      // Trying to access field that doesn't exist after unification with closed record
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "f",
          ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
          ast.let_(
            "g",
            ast.abs("r", ast.fieldAccess(ast.var_("r"), "z")),
            // Try to use both functions on the same closed record
            ast.tuple([
              ast.app(ast.var_("f"), ast.record([ast.field("x", ast.num(1))])),
              ast.app(ast.var_("g"), ast.record([ast.field("y", ast.num(2))])),
            ]),
          ),
        ),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
    });

    it("handles open record unified with closed record", () => {
      // Function expects any record with x, but receives exactly {x}
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.app(
          ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
          ast.record([ast.field("x", ast.num(42))]),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("handles both records with unique extra fields", () => {
      // Complex case where both records contribute unique fields
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "combine",
          ast.abs("r1", ast.abs("r2", ast.if_(ast.bool(true), ast.var_("r1"), ast.var_("r2")))),
          ast.app(
            ast.app(
              ast.var_("combine"),
              ast.record([ast.field("x", ast.num(1)), ast.field("shared", ast.num(0))]),
            ),
            ast.record([ast.field("y", ast.num(2)), ast.field("shared", ast.num(0))]),
          ),
        ),
      );
      // This should fail because closed records have different fields
      expect(diagnostics.length).toBeGreaterThan(0);
    });
  });

  describe("pattern matching edge cases", () => {
    it("infers record pattern when expected type is variable", () => {
      // match x with | { a = v } => v end - x's type is inferred from pattern
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "extract",
          ast.abs(
            "x",
            ast.match(ast.var_("x"), [
              ast.case_(ast.precord([ast.pfield("a", ast.pvar("v"))]), ast.var_("v")),
            ]),
          ),
          ast.app(ast.var_("extract"), ast.record([ast.field("a", ast.num(42))])),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("infers tuple pattern when expected type is variable", () => {
      // match x with | (a, b) => a + b end - x's type is inferred from pattern
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "sum",
          ast.abs(
            "x",
            ast.match(ast.var_("x"), [
              ast.case_(
                ast.ptuple([ast.pvar("a"), ast.pvar("b")]),
                ast.binOp("+", ast.var_("a"), ast.var_("b")),
              ),
            ]),
          ),
          ast.app(ast.var_("sum"), ast.tuple([ast.num(1), ast.num(2)])),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("reports error for record pattern against non-record type", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.match(ast.num(42), [
          ast.case_(ast.precord([ast.pfield("x", ast.pvar("v"))]), ast.var_("v")),
        ]),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("record pattern");
    });

    it("reports error for tuple pattern against non-tuple type", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.match(ast.num(42), [
          ast.case_(ast.ptuple([ast.pvar("a"), ast.pvar("b")]), ast.var_("a")),
        ]),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("tuple pattern");
    });

    it("handles record pattern with field from open row", () => {
      // Pattern matching on open record that needs to extract field from row
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "getAB",
          ast.abs(
            "r",
            ast.match(ast.var_("r"), [
              ast.case_(
                ast.precord([ast.pfield("a", ast.pvar("x")), ast.pfield("b", ast.pvar("y"))]),
                ast.binOp("+", ast.var_("x"), ast.var_("y")),
              ),
            ]),
          ),
          ast.app(
            ast.var_("getAB"),
            ast.record([
              ast.field("a", ast.num(1)),
              ast.field("b", ast.num(2)),
              ast.field("c", ast.num(3)),
            ]),
          ),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("reports error for record pattern with non-existent field on closed record", () => {
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.match(ast.record([ast.field("x", ast.num(1))]), [
          ast.case_(ast.precord([ast.pfield("y", ast.pvar("v"))]), ast.var_("v")),
        ]),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("no field");
    });
  });

  describe("let polymorphism with complex types", () => {
    it("generalizes tuple types in let", () => {
      // let pair = (x => x, x => x) in pair
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "pair",
          ast.tuple([ast.abs("x", ast.var_("x")), ast.abs("y", ast.var_("y"))]),
          ast.var_("pair"),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toContain("->");
    });

    it("generalizes record types in let", () => {
      // let r = { id = x => x } in r
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_("r", ast.record([ast.field("id", ast.abs("x", ast.var_("x")))]), ast.var_("r")),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toContain("id:");
    });
  });

  describe("data declaration edge cases", () => {
    it("handles function types in constructor fields", () => {
      // data Fn a b = MkFn (a -> b)
      const fnDecl = ast.dataDecl(
        "Fn",
        ["a", "b"],
        [ast.conDecl("MkFn", [ast.tyfun(ast.tyvar("a"), ast.tyvar("b"))])],
      );
      const [fnEnv, fnReg] = processDataDecl(fnDecl);
      const env = mergeEnvs(baseEnv, fnEnv);

      const { type, diagnostics } = infer(
        env,
        fnReg,
        ast.app(ast.var_("MkFn"), ast.abs("x", ast.binOp("+", ast.var_("x"), ast.num(1)))),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("Fn number number");
    });
  });

  describe("infinite type detection", () => {
    it("reports error for infinite type (occurs check)", () => {
      // let rec f = f f in f
      // This applies f to itself, requiring a = a -> b
      // The occurs check detects that 'a' appears in 'a -> b'
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.letRec([ast.recBinding("f", ast.app(ast.var_("f"), ast.var_("f")))], ast.var_("f")),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("Infinite type");
    });
  });

  describe("record unification edge cases", () => {
    it("unifies open record with closed record with same fields", () => {
      // Function expecting open record, applied to closed record
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "getX",
          ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
          ast.let_(
            "result",
            ast.app(ast.var_("getX"), ast.record([ast.field("x", ast.num(1))])),
            ast.var_("result"),
          ),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("reports error when closed record missing required fields", () => {
      // Function expecting record with x and y, applied to record with only x
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "getXY",
          ast.abs(
            "r",
            ast.binOp(
              "+",
              ast.fieldAccess(ast.var_("r"), "x"),
              ast.fieldAccess(ast.var_("r"), "y"),
            ),
          ),
          ast.app(ast.var_("getXY"), ast.record([ast.field("x", ast.num(1))])),
        ),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
    });

    it("unifies two open records with same fields", () => {
      // Lines 721-722: Both open records, same fields, unify row variables
      // Function takes two records that must have same x field
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "sameX",
          ast.abs(
            "r1",
            ast.abs(
              "r2",
              ast.binOp(
                "+",
                ast.fieldAccess(ast.var_("r1"), "x"),
                ast.fieldAccess(ast.var_("r2"), "x"),
              ),
            ),
          ),
          ast.app(
            ast.app(
              ast.var_("sameX"),
              ast.record([ast.field("x", ast.num(1)), ast.field("z", ast.num(3))]),
            ),
            ast.record([ast.field("x", ast.num(2)), ast.field("z", ast.num(4))]),
          ),
        ),
      );
      expect(diagnostics).toHaveLength(0);
    });

    it("unifies open record with closed record having same fields", () => {
      // Lines 731-732: t1 closed, t2 open (same fields)
      // Function accesses x, applied to record with exactly x
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "getX",
          ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
          ast.app(ast.var_("getX"), ast.record([ast.field("x", ast.num(42))])),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("unifies open record with closed record having extra fields", () => {
      // Function expecting record with x, called with {x, y} closed record
      // This tests lines 745-747: t1 has extra fields, t2 open
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "getX",
          ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
          ast.app(
            ast.var_("getX"),
            ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]),
          ),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number");
    });

    it("reports error for closed record missing fields from open record", () => {
      // This tests lines 753-754: t2 has extra fields, t1 is closed
      // Function returns {x, y} but called with context expecting {x}
      const { diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "makeRecord",
          ast.abs("n", ast.record([ast.field("x", ast.var_("n")), ast.field("y", ast.var_("n"))])),
          ast.let_(
            "useRecord",
            ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")),
            // Apply useRecord to makeRecord, but access z field which doesn't exist
            ast.let_(
              "f",
              ast.abs(
                "r",
                ast.fieldAccess(ast.var_("r"), "z"), // access z
              ),
              ast.app(ast.var_("f"), ast.record([ast.field("x", ast.num(1))])),
            ),
          ),
        ),
      );
      expect(diagnostics.length).toBeGreaterThan(0);
    });

    it("handles type variable self-binding during unification", () => {
      // This tests line 632: bindVar with same variable
      // Identity function applied to itself requires unifying a with a
      const { type, diagnostics } = infer(
        baseEnv,
        new Map(),
        ast.let_(
          "id",
          ast.abs("x", ast.var_("x")),
          ast.let_("result", ast.app(ast.var_("id"), ast.var_("id")), ast.var_("result")),
        ),
      );
      expect(diagnostics).toHaveLength(0);
      // id id should be id with type a -> a (printed as "tN -> tN")
      expect(typeToString(type)).toMatch(/t\d+ -> t\d+/);
    });
  });

  describe("processDeclarations", () => {
    it("processes multiple data declarations", () => {
      // Test the processDeclarations utility function
      const maybeDecl = ast.dataDecl(
        "Maybe",
        ["a"],
        [ast.conDecl("Nothing", []), ast.conDecl("Just", [ast.tyvar("a")])],
      );
      const eitherDecl = ast.dataDecl(
        "Either",
        ["a", "b"],
        [ast.conDecl("Left", [ast.tyvar("a")]), ast.conDecl("Right", [ast.tyvar("b")])],
      );

      const result = processDeclarations([maybeDecl, eitherDecl]);
      expect(result.typeEnv.has("Nothing")).toBe(true);
      expect(result.typeEnv.has("Just")).toBe(true);
      expect(result.typeEnv.has("Left")).toBe(true);
      expect(result.typeEnv.has("Right")).toBe(true);
      expect(result.constructorNames).toContain("Nothing");
      expect(result.constructorNames).toContain("Just");
      expect(result.constructorNames).toContain("Left");
      expect(result.constructorNames).toContain("Right");
    });

    it("processes recursive type with multiple same-type fields", () => {
      // Regression test: type Expr = Lit number | Bin Expr Expr
      // Bin should have type: Expr -> Expr -> Expr (curried)
      const exprDecl = ast.dataDecl(
        "Expr",
        [],
        [
          ast.conDecl("Lit", [ast.tyvar("number")]),
          ast.conDecl("Bin", [ast.tycon("Expr"), ast.tycon("Expr")]),
        ],
      );

      const result = processDeclarations([exprDecl]);
      expect(result.typeEnv.has("Lit")).toBe(true);
      expect(result.typeEnv.has("Bin")).toBe(true);
      expect(result.constructorNames).toContain("Lit");
      expect(result.constructorNames).toContain("Bin");

      // Check that Bin has the correct type: Expr -> Expr -> Expr
      const binScheme = result.typeEnv.get("Bin");
      expect(binScheme).toBeDefined();
      expect(typeToString(binScheme!.type)).toBe("Expr -> Expr -> Expr");
    });

    it("processes parameterized recursive type", () => {
      // type Tree a = Leaf | Node a (Tree a) (Tree a)
      const treeDecl = ast.dataDecl(
        "Tree",
        ["a"],
        [
          ast.conDecl("Leaf", []),
          ast.conDecl("Node", [
            ast.tyvar("a"),
            ast.tyapp(ast.tycon("Tree"), ast.tyvar("a")),
            ast.tyapp(ast.tycon("Tree"), ast.tyvar("a")),
          ]),
        ],
      );

      const result = processDeclarations([treeDecl]);
      expect(result.typeEnv.has("Leaf")).toBe(true);
      expect(result.typeEnv.has("Node")).toBe(true);

      // Check that Node has the correct type: a -> Tree a -> Tree a -> Tree a
      const nodeScheme = result.typeEnv.get("Node");
      expect(nodeScheme).toBeDefined();
      expect(typeToString(nodeScheme!.type)).toBe("a -> Tree a -> Tree a -> Tree a");
    });
  });

  describe("type annotations", () => {
    it("accepts correct parameter annotation", () => {
      // let f (x : number) = x + 1 in f
      const expr = ast.let_(
        "f",
        ast.abs(
          "x",
          ast.binOp("+", ast.var_("x"), ast.num(1)),
          undefined,
          undefined,
          ast.tyvar("number"),
        ),
        ast.var_("f"),
      );
      const { type, diagnostics } = infer(baseEnv, new Map(), expr);
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number -> number");
    });

    it("accepts correct return type annotation", () => {
      // let f x : number = x in f
      const expr: ast.Let = {
        kind: "Let",
        name: "f",
        returnType: ast.tyvar("number"),
        value: ast.abs("x", ast.var_("x")),
        body: ast.var_("f"),
      };
      const { type, diagnostics } = infer(baseEnv, new Map(), expr);
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number -> number");
    });

    it("rejects mismatched return type annotation", () => {
      // let f (x : number) : string = x in f
      const expr: ast.Let = {
        kind: "Let",
        name: "f",
        returnType: ast.tyvar("string"),
        value: ast.abs("x", ast.var_("x"), undefined, undefined, ast.tyvar("number")),
        body: ast.var_("f"),
      };
      const { diagnostics } = infer(baseEnv, new Map(), expr);
      expect(diagnostics.length).toBeGreaterThan(0);
      expect(diagnostics[0]!.message).toContain("mismatch");
    });

    it("accepts polymorphic annotation", () => {
      // let id (x : a) : a = x in id
      const expr: ast.Let = {
        kind: "Let",
        name: "id",
        returnType: ast.tyvar("a"),
        value: ast.abs("x", ast.var_("x"), undefined, undefined, ast.tyvar("a")),
        body: ast.var_("id"),
      };
      const { type, diagnostics } = infer(baseEnv, new Map(), expr);
      expect(diagnostics).toHaveLength(0);
      // The type should be polymorphic (t -> t)
      expect(typeToString(type)).toMatch(/^t\d+ -> t\d+$/);
    });

    it("accepts function type annotation", () => {
      // let apply (f : number -> number) (x : number) = f x in apply
      const fType = ast.tyfun(ast.tyvar("number"), ast.tyvar("number"));
      const expr = ast.let_(
        "apply",
        ast.abs(
          "f",
          ast.abs(
            "x",
            ast.app(ast.var_("f"), ast.var_("x")),
            undefined,
            undefined,
            ast.tyvar("number"),
          ),
          undefined,
          undefined,
          fType,
        ),
        ast.var_("apply"),
      );
      const { type, diagnostics } = infer(baseEnv, new Map(), expr);
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("(number -> number) -> number -> number");
    });

    it("accepts annotated recursive binding", () => {
      // let rec fact (n : number) : number = if n <= 1 then 1 else n * fact (n - 1) in fact
      const factBody = ast.if_(
        ast.binOp("<=", ast.var_("n"), ast.num(1)),
        ast.num(1),
        ast.binOp(
          "*",
          ast.var_("n"),
          ast.app(ast.var_("fact"), ast.binOp("-", ast.var_("n"), ast.num(1))),
        ),
      );
      const recBinding: ast.RecBinding = {
        name: "fact",
        returnType: ast.tyvar("number"),
        value: ast.abs("n", factBody, undefined, undefined, ast.tyvar("number")),
      };
      const expr = ast.letRec([recBinding], ast.var_("fact"));
      const { type, diagnostics } = infer(baseEnv, new Map(), expr);
      expect(diagnostics).toHaveLength(0);
      expect(typeToString(type)).toBe("number -> number");
    });

    it("uses annotation to constrain inferred type", () => {
      // let f (x : number) = x in f "hello" -- should fail
      const expr = ast.let_(
        "f",
        ast.abs("x", ast.var_("x"), undefined, undefined, ast.tyvar("number")),
        ast.app(ast.var_("f"), ast.str("hello")),
      );
      const { diagnostics } = infer(baseEnv, new Map(), expr);
      expect(diagnostics.length).toBeGreaterThan(0);
    });
  });

  describe("foreign bindings", () => {
    it("processes module with foreign binding", () => {
      const mod = ast.moduleDecl(
        "String",
        [],
        [],
        [ast.foreignBinding("length", ast.tyfun(ast.tycon("string"), ast.tycon("number")))],
      );
      const info = processModule(mod);
      expect(info.typeEnv.has("length")).toBe(true);
      const lengthScheme = info.typeEnv.get("length")!;
      expect(typeToString(lengthScheme.type)).toBe("string -> number");
    });

    it("processes foreign binding with type parameters", () => {
      // foreign map : (a -> b) -> List a -> List b
      const mod = ast.moduleDecl(
        "List",
        [],
        [],
        [
          ast.foreignBinding(
            "map",
            ast.tyfun(
              ast.tyfun(ast.tyvar("a"), ast.tyvar("b")),
              ast.tyfun(
                ast.tyapp(ast.tycon("List"), ast.tyvar("a")),
                ast.tyapp(ast.tycon("List"), ast.tyvar("b")),
              ),
            ),
          ),
        ],
      );
      const info = processModule(mod);
      expect(info.typeEnv.has("map")).toBe(true);
      const mapScheme = info.typeEnv.get("map")!;
      // Should have quantified type variables
      expect(mapScheme.vars.length).toBe(2);
    });

    it("allows regular bindings to use foreign bindings", () => {
      // module String
      //   foreign length : string -> number
      //   let isEmpty s = length s == 0
      // end
      const mod = ast.moduleDecl(
        "String",
        [],
        [
          ast.recBinding(
            "isEmpty",
            ast.abs("s", ast.binOp("==", ast.app(ast.var_("length"), ast.var_("s")), ast.num(0))),
          ),
        ],
        [ast.foreignBinding("length", ast.tyfun(ast.tycon("string"), ast.tycon("number")))],
      );
      const info = processModule(mod);
      expect(info.typeEnv.has("isEmpty")).toBe(true);
      const isEmptyScheme = info.typeEnv.get("isEmpty")!;
      expect(typeToString(isEmptyScheme.type)).toBe("string -> boolean");
    });

    it("processes multiple foreign bindings", () => {
      const mod = ast.moduleDecl(
        "String",
        [],
        [],
        [
          ast.foreignBinding("length", ast.tyfun(ast.tycon("string"), ast.tycon("number"))),
          ast.foreignBinding(
            "concat",
            ast.tyfun(ast.tycon("string"), ast.tyfun(ast.tycon("string"), ast.tycon("string"))),
          ),
        ],
      );
      const info = processModule(mod);
      expect(info.typeEnv.has("length")).toBe(true);
      expect(info.typeEnv.has("concat")).toBe(true);
      expect(typeToString(info.typeEnv.get("concat")!.type)).toBe("string -> string -> string");
    });
  });
});
