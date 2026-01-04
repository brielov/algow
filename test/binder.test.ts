import { describe, expect, it } from "bun:test";
import * as ast from "../src/ast";
import {
  bind,
  bindWithConstructors,
  findAllOccurrences,
  findDefinitionAt,
  findReferenceAt,
  findReferences,
  goToDefinition,
  type BindResult,
} from "../src/binder";

describe("Binder", () => {
  describe("bind", () => {
    it("binds simple variable reference", () => {
      // let x = 1 in x
      const expr = ast.let_(
        "x",
        ast.num(1),
        ast.var_("x", { start: 13, end: 14 }),
        { start: 0, end: 14 },
        { start: 4, end: 5 },
      );
      const result = bind(expr);

      expect(result.diagnostics).toHaveLength(0);
      expect(result.symbols.definitions).toHaveLength(1);
      expect(result.symbols.references).toHaveLength(1);
      const def = result.symbols.definitions[0]!;
      const ref = result.symbols.references[0]!;
      expect(def.name).toBe("x");
      expect(ref.name).toBe("x");
      expect(ref.definition).toBe(def);
    });

    it("reports unbound variable error", () => {
      const expr = ast.var_("unknown", { start: 0, end: 7 });
      const result = bind(expr);

      expect(result.diagnostics).toHaveLength(1);
      expect(result.diagnostics[0]!.message).toBe("Unknown variable: unknown");
    });

    it("handles nested let bindings with shadowing", () => {
      // let x = 1 in let x = 2 in x
      const inner = ast.let_(
        "x",
        ast.num(2),
        ast.var_("x", { start: 25, end: 26 }),
        { start: 13, end: 26 },
        { start: 17, end: 18 },
      );
      const expr = ast.let_("x", ast.num(1), inner, { start: 0, end: 26 }, { start: 4, end: 5 });
      const result = bind(expr);

      expect(result.diagnostics).toHaveLength(0);
      expect(result.symbols.definitions).toHaveLength(2);
      // The reference should be to the inner x
      const ref = result.symbols.references[0]!;
      expect(ref.definition!.span.start).toBe(17);
    });

    it("binds lambda parameter", () => {
      // \x => x
      const expr = ast.abs(
        "x",
        ast.var_("x", { start: 6, end: 7 }),
        { start: 0, end: 7 },
        { start: 1, end: 2 },
      );
      const result = bind(expr);

      expect(result.diagnostics).toHaveLength(0);
      expect(result.symbols.definitions).toHaveLength(1);
      expect(result.symbols.definitions[0]!.kind).toBe("parameter");
    });

    it("binds recursive function", () => {
      // let rec f x = f x in f 1
      const body = ast.app(
        ast.var_("f", { start: 14, end: 15 }),
        ast.var_("x", { start: 16, end: 17 }),
      );
      const fn = ast.abs("x", body, undefined, { start: 10, end: 11 });
      const expr = ast.letRec(
        "f",
        fn,
        ast.app(ast.var_("f", { start: 21, end: 22 }), ast.num(1)),
        { start: 0, end: 25 },
        { start: 8, end: 9 },
      );
      const result = bind(expr);

      expect(result.diagnostics).toHaveLength(0);
      // f should be defined once, referenced twice
      const fDef = result.symbols.definitions.find((d) => d.name === "f");
      expect(fDef).toBeDefined();
      const fRefs = result.symbols.references.filter((r) => r.name === "f");
      expect(fRefs).toHaveLength(2);
    });

    it("binds if expression", () => {
      const expr = ast.if_(ast.bool(true), ast.num(1), ast.num(2));
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("binds binary operation", () => {
      const expr = ast.binOp("+", ast.num(1), ast.num(2));
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("binds tuple", () => {
      const expr = ast.tuple([ast.num(1), ast.num(2)]);
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("binds record", () => {
      const expr = ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]);
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("binds field access", () => {
      const rec = ast.record([ast.field("x", ast.num(1))]);
      const expr = ast.fieldAccess(rec, "x");
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("binds match expression with variable patterns", () => {
      // match x with y => y end
      const x = ast.var_("x", { start: 6, end: 7 });
      const yRef = ast.var_("y", { start: 18, end: 19 });
      const yPat = ast.pvar("y", { start: 13, end: 14 });
      const expr = ast.let_("x", ast.num(1), ast.match(x, [ast.case_(yPat, yRef)]), undefined, {
        start: 4,
        end: 5,
      });
      const result = bind(expr);

      expect(result.diagnostics).toHaveLength(0);
      // y should be bound in the pattern and referenced in the body
      const yDef = result.symbols.definitions.find((d) => d.name === "y");
      expect(yDef).toBeDefined();
    });

    it("binds match expression with wildcard pattern", () => {
      const expr = ast.let_(
        "x",
        ast.num(1),
        ast.match(ast.var_("x", { start: 10, end: 11 }), [ast.case_(ast.pwildcard(), ast.num(0))]),
        undefined,
        { start: 4, end: 5 },
      );
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("binds match expression with literal pattern", () => {
      const expr = ast.let_(
        "x",
        ast.num(1),
        ast.match(ast.var_("x", { start: 10, end: 11 }), [
          ast.case_(ast.plit(0), ast.num(0)),
          ast.case_(ast.pvar("n"), ast.var_("n", { start: 30, end: 31 })),
        ]),
        undefined,
        { start: 4, end: 5 },
      );
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("binds match expression with tuple pattern", () => {
      const expr = ast.let_(
        "p",
        ast.tuple([ast.num(1), ast.num(2)]),
        ast.match(ast.var_("p", { start: 20, end: 21 }), [
          ast.case_(
            ast.ptuple([
              ast.pvar("a", { start: 28, end: 29 }),
              ast.pvar("b", { start: 31, end: 32 }),
            ]),
            ast.binOp(
              "+",
              ast.var_("a", { start: 38, end: 39 }),
              ast.var_("b", { start: 42, end: 43 }),
            ),
          ),
        ]),
        undefined,
        { start: 4, end: 5 },
      );
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("binds match expression with record pattern", () => {
      const expr = ast.let_(
        "r",
        ast.record([ast.field("x", ast.num(1))]),
        ast.match(ast.var_("r", { start: 20, end: 21 }), [
          ast.case_(
            ast.precord([ast.pfield("x", ast.pvar("val", { start: 30, end: 33 }))]),
            ast.var_("val", { start: 40, end: 43 }),
          ),
        ]),
        undefined,
        { start: 4, end: 5 },
      );
      const result = bind(expr);
      expect(result.diagnostics).toHaveLength(0);
    });
  });

  describe("bindWithConstructors", () => {
    it("pre-populates constructors in scope", () => {
      const expr = ast.var_("Just", { start: 0, end: 4 });
      const result = bindWithConstructors(["Just", "Nothing"], expr);

      expect(result.diagnostics).toHaveLength(0);
      expect(result.symbols.references).toHaveLength(1);
      expect(result.symbols.references[0]!.definition).not.toBeNull();
      expect(result.symbols.references[0]!.definition!.kind).toBe("constructor");
    });

    it("binds constructor pattern in match", () => {
      // match x with Just n => n | Nothing => 0 end
      const expr = ast.let_(
        "x",
        ast.var_("Just", { start: 8, end: 12 }),
        ast.match(ast.var_("x", { start: 20, end: 21 }), [
          ast.case_(
            ast.pcon(
              "Just",
              [ast.pvar("n", { start: 30, end: 31 })],
              { start: 25, end: 31 },
              { start: 25, end: 29 },
            ),
            ast.var_("n", { start: 36, end: 37 }),
          ),
          ast.case_(
            ast.pcon("Nothing", [], { start: 41, end: 48 }, { start: 41, end: 48 }),
            ast.num(0),
          ),
        ]),
        undefined,
        { start: 4, end: 5 },
      );
      const result = bindWithConstructors(["Just", "Nothing"], expr);

      expect(result.diagnostics).toHaveLength(0);
      // Just and Nothing should be referenced in patterns
      const justRefs = result.symbols.references.filter((r) => r.name === "Just");
      expect(justRefs.length).toBeGreaterThanOrEqual(1);
    });

    it("handles constructor pattern without nameSpan", () => {
      const expr = ast.match(ast.var_("x", { start: 6, end: 7 }), [
        ast.case_(
          ast.pcon("Nothing", [], { start: 13, end: 20 }), // No nameSpan, uses span
          ast.num(0),
        ),
      ]);
      const result = bindWithConstructors(["Nothing", "x"], expr);
      expect(result.diagnostics).toHaveLength(0);
    });

    it("handles constructor pattern without any span", () => {
      const expr = ast.match(ast.var_("x", { start: 6, end: 7 }), [
        ast.case_(
          ast.pcon("Nothing", []), // No span at all
          ast.num(0),
        ),
      ]);
      const result = bindWithConstructors(["Nothing", "x"], expr);
      expect(result.diagnostics).toHaveLength(0);
    });
  });

  describe("Symbol Table Queries", () => {
    // Helper to create a bound expression for testing
    const createTestSymbolTable = (): BindResult => {
      // let x = 1 in let y = x in y
      const expr = ast.let_(
        "x",
        ast.num(1),
        ast.let_(
          "y",
          ast.var_("x", { start: 17, end: 18 }),
          ast.var_("y", { start: 22, end: 23 }),
          { start: 9, end: 23 },
          { start: 13, end: 14 },
        ),
        { start: 0, end: 23 },
        { start: 4, end: 5 },
      );
      return bind(expr);
    };

    describe("findDefinitionAt", () => {
      it("finds definition at position", () => {
        const result = createTestSymbolTable();
        const def = findDefinitionAt(result.symbols, 4);
        expect(def).not.toBeNull();
        expect(def!.name).toBe("x");
      });

      it("returns null for position outside definitions", () => {
        const result = createTestSymbolTable();
        const def = findDefinitionAt(result.symbols, 100);
        expect(def).toBeNull();
      });
    });

    describe("findReferenceAt", () => {
      it("finds reference at position", () => {
        const result = createTestSymbolTable();
        const ref = findReferenceAt(result.symbols, 17);
        expect(ref).not.toBeNull();
        expect(ref!.name).toBe("x");
      });

      it("returns null for position outside references", () => {
        const result = createTestSymbolTable();
        const ref = findReferenceAt(result.symbols, 100);
        expect(ref).toBeNull();
      });
    });

    describe("findReferences", () => {
      it("finds all references to a definition", () => {
        const result = createTestSymbolTable();
        const xDef = result.symbols.definitions.find((d) => d.name === "x")!;
        const refs = findReferences(result.symbols, xDef);
        expect(refs).toHaveLength(1);
        expect(refs[0]!.name).toBe("x");
      });

      it("returns empty array for definition with no references", () => {
        const expr = ast.let_("x", ast.num(1), ast.num(2), undefined, { start: 4, end: 5 });
        const result = bind(expr);
        const xDef = result.symbols.definitions[0]!;
        const refs = findReferences(result.symbols, xDef);
        expect(refs).toHaveLength(0);
      });
    });

    describe("goToDefinition", () => {
      it("returns definition when position is on a reference", () => {
        const result = createTestSymbolTable();
        const def = goToDefinition(result.symbols, 17); // Position of x reference
        expect(def).not.toBeNull();
        expect(def!.name).toBe("x");
      });

      it("returns definition when position is on a definition", () => {
        const result = createTestSymbolTable();
        const def = goToDefinition(result.symbols, 4); // Position of x definition
        expect(def).not.toBeNull();
        expect(def!.name).toBe("x");
      });

      it("returns null when position is not on any symbol", () => {
        const result = createTestSymbolTable();
        const def = goToDefinition(result.symbols, 100);
        expect(def).toBeNull();
      });
    });

    describe("findAllOccurrences", () => {
      it("finds all occurrences of a symbol", () => {
        const result = createTestSymbolTable();
        const occurrences = findAllOccurrences(result.symbols, 4); // Position of x definition
        expect(occurrences.definition).not.toBeNull();
        expect(occurrences.definition!.name).toBe("x");
        expect(occurrences.references).toHaveLength(1);
      });

      it("returns empty result for position with no symbol", () => {
        const result = createTestSymbolTable();
        const occurrences = findAllOccurrences(result.symbols, 100);
        expect(occurrences.definition).toBeNull();
        expect(occurrences.references).toHaveLength(0);
      });
    });
  });
});
