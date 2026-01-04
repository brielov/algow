// oxlint-disable no-thenable

import { describe, expect, it } from "bun:test";
import * as ast from "../src/ast";

describe("AST Helper Functions", () => {
  describe("literal constructors", () => {
    it("creates number literal", () => {
      const node = ast.num(42);
      expect(node).toEqual({ kind: "Num", value: 42 });
    });

    it("creates number literal with float", () => {
      const node = ast.num(3.14);
      expect(node).toEqual({ kind: "Num", value: 3.14 });
    });

    it("creates number literal with zero", () => {
      const node = ast.num(0);
      expect(node).toEqual({ kind: "Num", value: 0 });
    });

    it("creates number literal with negative", () => {
      const node = ast.num(-5);
      expect(node).toEqual({ kind: "Num", value: -5 });
    });

    it("creates boolean true", () => {
      const node = ast.bool(true);
      expect(node).toEqual({ kind: "Bool", value: true });
    });

    it("creates boolean false", () => {
      const node = ast.bool(false);
      expect(node).toEqual({ kind: "Bool", value: false });
    });

    it("creates string literal", () => {
      const node = ast.str("hello");
      expect(node).toEqual({ kind: "Str", value: "hello" });
    });

    it("creates empty string", () => {
      const node = ast.str("");
      expect(node).toEqual({ kind: "Str", value: "" });
    });
  });

  describe("variable and function constructors", () => {
    it("creates variable reference", () => {
      const node = ast.var_("foo");
      expect(node).toEqual({ kind: "Var", name: "foo" });
    });

    it("creates lambda abstraction", () => {
      const node = ast.abs("x", ast.var_("x"));
      expect(node).toEqual({
        kind: "Abs",
        param: "x",
        body: { kind: "Var", name: "x" },
      });
    });

    it("creates function application", () => {
      const node = ast.app(ast.var_("f"), ast.var_("x"));
      expect(node).toEqual({
        kind: "App",
        func: { kind: "Var", name: "f" },
        param: { kind: "Var", name: "x" },
      });
    });
  });

  describe("control flow constructors", () => {
    it("creates if expression", () => {
      const node = ast.if_(ast.bool(true), ast.num(1), ast.num(0));
      expect(node).toEqual({
        kind: "If",
        cond: { kind: "Bool", value: true },
        then: { kind: "Num", value: 1 },
        else: { kind: "Num", value: 0 },
      });
    });
  });

  describe("binding constructors", () => {
    it("creates let expression", () => {
      const node = ast.let_("x", ast.num(42), ast.var_("x"));
      expect(node).toEqual({
        kind: "Let",
        name: "x",
        value: { kind: "Num", value: 42 },
        body: { kind: "Var", name: "x" },
      });
    });

    it("creates letrec expression", () => {
      const node = ast.letRec("f", ast.var_("f"), ast.var_("f"));
      expect(node).toEqual({
        kind: "LetRec",
        name: "f",
        value: { kind: "Var", name: "f" },
        body: { kind: "Var", name: "f" },
      });
    });
  });

  describe("operator constructors", () => {
    it("creates binary operator", () => {
      const node = ast.binOp("+", ast.num(1), ast.num(2));
      expect(node).toEqual({
        kind: "BinOp",
        op: "+",
        left: { kind: "Num", value: 1 },
        right: { kind: "Num", value: 2 },
      });
    });

    it("creates all operators", () => {
      const ops: ast.Op[] = ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="];
      for (const op of ops) {
        const node = ast.binOp(op, ast.num(1), ast.num(2));
        expect(node.op).toBe(op);
      }
    });
  });

  describe("compound data constructors", () => {
    it("creates empty tuple", () => {
      const node = ast.tuple([]);
      expect(node.kind).toBe("Tuple");
      expect(node.elements).toEqual([]);
    });

    it("creates single element tuple", () => {
      const node = ast.tuple([ast.num(1)]);
      expect(node.kind).toBe("Tuple");
      expect(node.elements.length).toBe(1);
      expect(node.elements[0]?.kind).toBe("Num");
    });

    it("creates multi-element tuple", () => {
      const node = ast.tuple([ast.num(1), ast.str("hello"), ast.bool(true)]);
      expect(node.kind).toBe("Tuple");
      expect(node.elements.length).toBe(3);
      expect(node.elements[0]?.kind).toBe("Num");
      expect(node.elements[1]?.kind).toBe("Str");
      expect(node.elements[2]?.kind).toBe("Bool");
    });

    it("creates empty record", () => {
      const node = ast.record([]);
      expect(node).toEqual({ kind: "Record", fields: [] });
    });

    it("creates record with fields", () => {
      const node = ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]);
      expect(node).toEqual({
        kind: "Record",
        fields: [
          { name: "x", value: { kind: "Num", value: 1 } },
          { name: "y", value: { kind: "Num", value: 2 } },
        ],
      });
    });

    it("creates record field", () => {
      const node = ast.field("name", ast.str("value"));
      expect(node).toEqual({
        name: "name",
        value: { kind: "Str", value: "value" },
      });
    });

    it("creates field access", () => {
      const node = ast.fieldAccess(ast.var_("r"), "x");
      expect(node).toEqual({
        kind: "FieldAccess",
        record: { kind: "Var", name: "r" },
        field: "x",
      });
    });
  });

  describe("pattern constructors", () => {
    it("creates wildcard pattern", () => {
      const node = ast.pwildcard();
      expect(node.kind).toBe("PWildcard");
    });

    it("creates variable pattern", () => {
      const node = ast.pvar("x");
      expect(node.kind).toBe("PVar");
      expect(node.name).toBe("x");
    });

    it("creates constructor pattern without args", () => {
      const node = ast.pcon("Nothing", []);
      expect(node.kind).toBe("PCon");
      expect(node.name).toBe("Nothing");
      expect(node.args).toEqual([]);
    });

    it("creates constructor pattern with args", () => {
      const node = ast.pcon("Just", [ast.pvar("x")]);
      expect(node.kind).toBe("PCon");
      expect(node.name).toBe("Just");
      expect(node.args.length).toBe(1);
      expect(node.args[0]?.kind).toBe("PVar");
    });

    it("creates constructor pattern with multiple args", () => {
      const node = ast.pcon("Cons", [ast.pvar("x"), ast.pvar("xs")]);
      expect(node.kind).toBe("PCon");
      expect(node.name).toBe("Cons");
      expect(node.args.length).toBe(2);
    });

    it("creates literal pattern with number", () => {
      const node = ast.plit(42);
      expect(node).toEqual({ kind: "PLit", value: 42 });
    });

    it("creates literal pattern with string", () => {
      const node = ast.plit("hello");
      expect(node).toEqual({ kind: "PLit", value: "hello" });
    });

    it("creates literal pattern with boolean", () => {
      const node = ast.plit(true);
      expect(node).toEqual({ kind: "PLit", value: true });
    });

    it("creates empty record pattern", () => {
      const node = ast.precord([]);
      expect(node).toEqual({ kind: "PRecord", fields: [] });
    });

    it("creates record pattern with fields", () => {
      const node = ast.precord([ast.pfield("x", ast.pvar("a")), ast.pfield("y", ast.pvar("b"))]);
      expect(node).toEqual({
        kind: "PRecord",
        fields: [
          { name: "x", pattern: { kind: "PVar", name: "a" } },
          { name: "y", pattern: { kind: "PVar", name: "b" } },
        ],
      });
    });

    it("creates pattern field", () => {
      const node = ast.pfield("name", ast.pvar("x"));
      expect(node).toEqual({
        name: "name",
        pattern: { kind: "PVar", name: "x" },
      });
    });

    it("creates tuple pattern", () => {
      const node = ast.ptuple([ast.pvar("a"), ast.pvar("b")]);
      expect(node).toEqual({
        kind: "PTuple",
        elements: [
          { kind: "PVar", name: "a" },
          { kind: "PVar", name: "b" },
        ],
      });
    });

    it("creates case", () => {
      const node = ast.case_(ast.pvar("x"), ast.var_("x"));
      expect(node).toEqual({
        pattern: { kind: "PVar", name: "x" },
        body: { kind: "Var", name: "x" },
      });
    });

    it("creates match expression", () => {
      const node = ast.match(ast.var_("x"), [ast.case_(ast.pvar("y"), ast.var_("y"))]);
      expect(node).toEqual({
        kind: "Match",
        expr: { kind: "Var", name: "x" },
        cases: [
          {
            pattern: { kind: "PVar", name: "y" },
            body: { kind: "Var", name: "y" },
          },
        ],
      });
    });
  });

  describe("type expression constructors", () => {
    it("creates type variable", () => {
      const node = ast.tyvar("a");
      expect(node).toEqual({ kind: "TyVar", name: "a" });
    });

    it("creates type constructor", () => {
      const node = ast.tycon("Int");
      expect(node).toEqual({ kind: "TyCon", name: "Int" });
    });

    it("creates type application", () => {
      const node = ast.tyapp(ast.tycon("List"), ast.tyvar("a"));
      expect(node).toEqual({
        kind: "TyApp",
        con: { kind: "TyCon", name: "List" },
        arg: { kind: "TyVar", name: "a" },
      });
    });

    it("creates function type", () => {
      const node = ast.tyfun(ast.tyvar("a"), ast.tyvar("b"));
      expect(node).toEqual({
        kind: "TyFun",
        param: { kind: "TyVar", name: "a" },
        ret: { kind: "TyVar", name: "b" },
      });
    });
  });

  describe("data declaration constructors", () => {
    it("creates constructor declaration", () => {
      const node = ast.conDecl("Just", [ast.tyvar("a")]);
      expect(node).toEqual({
        name: "Just",
        fields: [{ kind: "TyVar", name: "a" }],
      });
    });

    it("creates nullary constructor declaration", () => {
      const node = ast.conDecl("Nothing", []);
      expect(node).toEqual({ name: "Nothing", fields: [] });
    });

    it("creates data declaration", () => {
      const node = ast.dataDecl(
        "Maybe",
        ["a"],
        [ast.conDecl("Nothing", []), ast.conDecl("Just", [ast.tyvar("a")])],
      );
      expect(node).toEqual({
        kind: "DataDecl",
        name: "Maybe",
        typeParams: ["a"],
        constructors: [
          { name: "Nothing", fields: [] },
          { name: "Just", fields: [{ kind: "TyVar", name: "a" }] },
        ],
      });
    });

    it("creates data declaration without type params", () => {
      const node = ast.dataDecl("Bool", [], [ast.conDecl("True", []), ast.conDecl("False", [])]);
      expect(node).toEqual({
        kind: "DataDecl",
        name: "Bool",
        typeParams: [],
        constructors: [
          { name: "True", fields: [] },
          { name: "False", fields: [] },
        ],
      });
    });

    it("creates data declaration with multiple type params", () => {
      const node = ast.dataDecl(
        "Either",
        ["a", "b"],
        [ast.conDecl("Left", [ast.tyvar("a")]), ast.conDecl("Right", [ast.tyvar("b")])],
      );
      expect(node.typeParams).toEqual(["a", "b"]);
    });

    it("creates recursive data declaration", () => {
      const node = ast.dataDecl(
        "List",
        ["a"],
        [
          ast.conDecl("Nil", []),
          ast.conDecl("Cons", [ast.tyvar("a"), ast.tyapp(ast.tycon("List"), ast.tyvar("a"))]),
        ],
      );
      expect(node.constructors[1]!.fields).toHaveLength(2);
    });
  });

  describe("complex AST construction", () => {
    it("builds nested lambdas", () => {
      // x => y => z => x
      const node = ast.abs("x", ast.abs("y", ast.abs("z", ast.var_("x"))));
      expect(node.kind).toBe("Abs");
      expect(node.param).toBe("x");
      expect((node.body as ast.Abs).param).toBe("y");
      expect(((node.body as ast.Abs).body as ast.Abs).param).toBe("z");
    });

    it("builds curried application", () => {
      // f x y z
      const node = ast.app(
        ast.app(ast.app(ast.var_("f"), ast.var_("x")), ast.var_("y")),
        ast.var_("z"),
      );
      expect(node.kind).toBe("App");
      expect((node.func as ast.App).kind).toBe("App");
    });

    it("builds complex match expression", () => {
      const node = ast.match(ast.var_("xs"), [
        ast.case_(ast.pcon("Nil", []), ast.num(0)),
        ast.case_(
          ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
          ast.binOp("+", ast.num(1), ast.app(ast.var_("length"), ast.var_("rest"))),
        ),
      ]);
      expect(node.cases).toHaveLength(2);
      expect(node.cases[0]!.pattern.kind).toBe("PCon");
      expect(node.cases[1]!.pattern.kind).toBe("PCon");
    });

    it("builds nested let expressions", () => {
      const node = ast.let_(
        "x",
        ast.num(1),
        ast.let_("y", ast.num(2), ast.let_("z", ast.num(3), ast.var_("z"))),
      );
      expect(node.kind).toBe("Let");
      expect((node.body as ast.Let).kind).toBe("Let");
      expect(((node.body as ast.Let).body as ast.Let).kind).toBe("Let");
    });

    it("builds nested record access", () => {
      // a.b.c.d
      const node = ast.fieldAccess(ast.fieldAccess(ast.fieldAccess(ast.var_("a"), "b"), "c"), "d");
      expect(node.kind).toBe("FieldAccess");
      expect(node.field).toBe("d");
    });

    it("builds complex binary expression", () => {
      // (1 + 2) * (3 - 4)
      const node = ast.binOp(
        "*",
        ast.binOp("+", ast.num(1), ast.num(2)),
        ast.binOp("-", ast.num(3), ast.num(4)),
      );
      expect(node.op).toBe("*");
      expect((node.left as ast.BinOp).op).toBe("+");
      expect((node.right as ast.BinOp).op).toBe("-");
    });

    it("builds factorial using letrec", () => {
      const node = ast.letRec(
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
      expect(node.kind).toBe("LetRec");
      expect(node.name).toBe("fact");
    });
  });

  describe("readonly immutability", () => {
    it("produces readonly expression structures", () => {
      const node = ast.tuple([ast.num(1), ast.num(2)]);
      // TypeScript would error if we try to modify:
      // node.elements = []; // Error: Cannot assign to 'elements'
      expect(node.elements).toHaveLength(2);
    });

    it("produces readonly pattern structures", () => {
      const node = ast.ptuple([ast.pvar("a"), ast.pvar("b")]);
      expect(node.elements).toHaveLength(2);
    });

    it("produces readonly type expression structures", () => {
      const node = ast.dataDecl("Test", ["a", "b"], []);
      expect(node.typeParams).toHaveLength(2);
    });
  });
});
