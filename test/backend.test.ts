import { describe, expect, it } from "bun:test";
import type { Type } from "../src/checker";
import * as ir from "../src/ir";
import { generateJS } from "../src/backend/js";
import { RUNTIME } from "../src/backend/runtime";

// Type helpers
const numType: Type = { kind: "TCon", name: "number" };
const strType: Type = { kind: "TCon", name: "string" };
const boolType: Type = { kind: "TCon", name: "boolean" };
const funType = (param: Type, ret: Type): Type => ({ kind: "TFun", param, ret });
const tupleType = (elements: Type[]): Type => ({ kind: "TTuple", elements });
const varType = (name: string): Type => ({ kind: "TVar", name });
const appType = (con: Type, arg: Type): Type => ({ kind: "TApp", con, arg });

describe("JavaScript Backend", () => {
  describe("generateJS", () => {
    it("includes runtime in output", () => {
      const expr = ir.irAtomExpr(ir.irLit(42, numType));
      const result = generateJS(expr, []);
      expect(result.code).toContain("$apply");
      expect(result.code).toContain("$con");
      expect(result.code).toContain("$eq");
    });

    it("generates number literal", () => {
      const expr = ir.irAtomExpr(ir.irLit(42, numType));
      const result = generateJS(expr, []);
      expect(result.code).toContain("const $result = 42;");
    });

    it("generates string literal", () => {
      const expr = ir.irAtomExpr(ir.irLit("hello", strType));
      const result = generateJS(expr, []);
      expect(result.code).toContain('const $result = "hello";');
    });

    it("generates boolean literal", () => {
      const expr = ir.irAtomExpr(ir.irLit(true, boolType));
      const result = generateJS(expr, []);
      expect(result.code).toContain("const $result = true;");
    });

    it("generates variable reference", () => {
      const binding = ir.irAtomBinding(ir.irLit(42, numType));
      const body = ir.irAtomExpr(ir.irVar("x", numType));
      const expr = ir.irLet("x", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("const x = 42;");
      expect(result.code).toContain("const $result = x;");
    });

    it("generates let binding", () => {
      const binding = ir.irAtomBinding(ir.irLit(1, numType));
      const body = ir.irAtomExpr(ir.irVar("x", numType));
      const expr = ir.irLet("x", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("const x = 1;");
    });

    it("generates recursive let binding", () => {
      const lambdaBody = ir.irAtomExpr(ir.irVar("n", numType));
      const fnType = funType(numType, numType);
      const binding = ir.irLambdaBinding("n", numType, lambdaBody, fnType);
      const body = ir.irAtomExpr(ir.irVar("f", fnType));
      const expr = ir.irLetRec([ir.irRecBinding("f", binding)], body);
      const result = generateJS(expr, []);
      // Recursive bindings use let + assignment
      expect(result.code).toContain("let f;");
      expect(result.code).toContain("f = (n) => n;");
    });

    it("generates non-lambda recursive let binding with const", () => {
      // A recursive binding that's not a lambda (e.g., a simple value)
      const binding = ir.irAtomBinding(ir.irLit(42, numType));
      const body = ir.irAtomExpr(ir.irVar("x", numType));
      const expr = ir.irLetRec([ir.irRecBinding("x", binding)], body);
      const result = generateJS(expr, []);
      // Non-lambda recursive bindings use const
      expect(result.code).toContain("const x = 42;");
    });
  });

  describe("Binary Operations", () => {
    it("generates addition", () => {
      const left = ir.irLit(1, numType);
      const right = ir.irLit(2, numType);
      const binding = ir.irBinOpBinding("+", left, right, numType, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(1 + 2)");
    });

    it("generates subtraction", () => {
      const binding = ir.irBinOpBinding(
        "-",
        ir.irLit(5, numType),
        ir.irLit(3, numType),
        numType,
        numType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(5 - 3)");
    });

    it("generates multiplication", () => {
      const binding = ir.irBinOpBinding(
        "*",
        ir.irLit(4, numType),
        ir.irLit(3, numType),
        numType,
        numType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(4 * 3)");
    });

    it("generates division", () => {
      const binding = ir.irBinOpBinding(
        "/",
        ir.irLit(10, numType),
        ir.irLit(2, numType),
        numType,
        numType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(10 / 2)");
    });

    it("generates comparison operators", () => {
      for (const op of ["<", ">", "<=", ">="] as const) {
        const binding = ir.irBinOpBinding(
          op,
          ir.irLit(1, numType),
          ir.irLit(2, numType),
          numType,
          boolType,
        );
        const body = ir.irAtomExpr(ir.irVar("_t", boolType));
        const expr = ir.irLet("_t", binding, body);
        const result = generateJS(expr, []);
        expect(result.code).toContain(`(1 ${op} 2)`);
      }
    });

    it("generates primitive equality with ===", () => {
      const binding = ir.irBinOpBinding(
        "==",
        ir.irLit(1, numType),
        ir.irLit(2, numType),
        numType,
        boolType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", boolType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(1 === 2)");
    });

    it("generates primitive inequality with !==", () => {
      const binding = ir.irBinOpBinding(
        "!=",
        ir.irLit(1, numType),
        ir.irLit(2, numType),
        numType,
        boolType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", boolType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(1 !== 2)");
    });

    it("uses $eq for complex type equality", () => {
      const tupleT = tupleType([numType, numType]);
      const binding = ir.irBinOpBinding(
        "==",
        ir.irVar("a", tupleT),
        ir.irVar("b", tupleT),
        tupleT,
        boolType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", boolType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("$eq(a, b)");
    });

    it("uses !$eq for complex type inequality", () => {
      const tupleT = tupleType([numType, numType]);
      const binding = ir.irBinOpBinding(
        "!=",
        ir.irVar("a", tupleT),
        ir.irVar("b", tupleT),
        tupleT,
        boolType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", boolType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("!$eq(a, b)");
    });

    it("uses $eq for type variables", () => {
      const tvar = varType("a");
      const binding = ir.irBinOpBinding(
        "==",
        ir.irVar("x", tvar),
        ir.irVar("y", tvar),
        tvar,
        boolType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", boolType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("$eq(x, y)");
    });

    it("uses $eq for function types", () => {
      const fnT = funType(numType, numType);
      const binding = ir.irBinOpBinding(
        "==",
        ir.irVar("f", fnT),
        ir.irVar("g", fnT),
        fnT,
        boolType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", boolType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("$eq(f, g)");
    });

    it("uses $eq for TApp types (custom data types)", () => {
      const maybeNum = appType({ kind: "TCon", name: "Maybe" }, numType);
      const binding = ir.irBinOpBinding(
        "==",
        ir.irVar("a", maybeNum),
        ir.irVar("b", maybeNum),
        maybeNum,
        boolType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", boolType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("$eq(a, b)");
    });

    it("uses $eq for record types", () => {
      const recT: Type = { kind: "TRecord", fields: new Map([["x", numType]]), row: null };
      const binding = ir.irBinOpBinding(
        "==",
        ir.irVar("a", recT),
        ir.irVar("b", recT),
        recT,
        boolType,
      );
      const body = ir.irAtomExpr(ir.irVar("_t", boolType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("$eq(a, b)");
    });
  });

  describe("Function Application", () => {
    it("generates function application", () => {
      const fnType_ = funType(numType, numType);
      const binding = ir.irAppBinding(ir.irVar("f", fnType_), ir.irLit(1, numType), numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      // Direct call since f is a known function (not a constructor)
      expect(result.code).toContain("f(1)");
    });
  });

  describe("Conditional Expressions", () => {
    it("generates simple if expression", () => {
      const thenBranch = ir.irAtomExpr(ir.irLit(1, numType));
      const elseBranch = ir.irAtomExpr(ir.irLit(2, numType));
      const binding = ir.irIfBinding(ir.irLit(true, boolType), thenBranch, elseBranch, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(true ? 1 : 2)");
    });

    it("generates if with complex branches using IIFE", () => {
      // Complex then branch: let x = 1 in x
      const innerBinding = ir.irAtomBinding(ir.irLit(1, numType));
      const thenBranch = ir.irLet("x", innerBinding, ir.irAtomExpr(ir.irVar("x", numType)));
      const elseBranch = ir.irAtomExpr(ir.irLit(2, numType));
      const binding = ir.irIfBinding(ir.irLit(true, boolType), thenBranch, elseBranch, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(() =>");
    });
  });

  describe("Tuples", () => {
    it("generates tuple as array", () => {
      const elements = [ir.irLit(1, numType), ir.irLit(2, numType)];
      const binding = ir.irTupleBinding(elements, tupleType([numType, numType]));
      const body = ir.irAtomExpr(ir.irVar("_t", tupleType([numType, numType])));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("[1, 2]");
    });
  });

  describe("Records", () => {
    it("generates record as object", () => {
      const recType: Type = {
        kind: "TRecord",
        fields: new Map([
          ["x", numType],
          ["y", numType],
        ]),
        row: null,
      };
      const fields = [
        ir.irRecordField("x", ir.irLit(1, numType)),
        ir.irRecordField("y", ir.irLit(2, numType)),
      ];
      const binding = ir.irRecordBinding(fields, recType);
      const body = ir.irAtomExpr(ir.irVar("_t", recType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("{ x: 1, y: 2 }");
    });

    it("generates field access", () => {
      const recType: Type = { kind: "TRecord", fields: new Map([["x", numType]]), row: null };
      const binding = ir.irFieldAccessBinding(ir.irVar("r", recType), "x", numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("r.x");
    });
  });

  describe("Lambda Expressions", () => {
    it("generates simple lambda", () => {
      const fnType_ = funType(numType, numType);
      const lambdaBody = ir.irAtomExpr(ir.irVar("x", numType));
      const binding = ir.irLambdaBinding("x", numType, lambdaBody, fnType_);
      const body = ir.irAtomExpr(ir.irVar("_fn", fnType_));
      const expr = ir.irLet("_fn", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("(x) => x");
    });

    it("generates lambda with complex body", () => {
      const fnType_ = funType(numType, numType);
      const innerBinding = ir.irAtomBinding(ir.irLit(1, numType));
      const lambdaBody = ir.irLet("y", innerBinding, ir.irAtomExpr(ir.irVar("y", numType)));
      const binding = ir.irLambdaBinding("x", numType, lambdaBody, fnType_);
      const body = ir.irAtomExpr(ir.irVar("_fn", fnType_));
      const expr = ir.irLet("_fn", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("const y = 1");
      expect(result.code).toContain("return y");
    });
  });

  describe("Constructors", () => {
    it("generates constructor as $con call", () => {
      const maybeType = appType({ kind: "TCon", name: "Maybe" }, numType);
      const binding = ir.irAtomBinding(ir.irVar("Nothing", maybeType));
      const body = ir.irAtomExpr(ir.irVar("x", maybeType));
      const expr = ir.irLet("x", binding, body);
      const result = generateJS(expr, ["Nothing", "Just"]);
      expect(result.code).toContain('$con("Nothing")');
    });

    it("does not treat regular variables as constructors", () => {
      const binding = ir.irAtomBinding(ir.irVar("x", numType));
      const body = ir.irAtomExpr(ir.irVar("y", numType));
      const expr = ir.irLet("y", binding, body);
      const result = generateJS(expr, ["Nothing", "Just"]);
      expect(result.code).toContain("const y = x;");
      expect(result.code).not.toContain('$con("x")');
    });
  });

  describe("Pattern Matching", () => {
    it("generates match with variable pattern", () => {
      const pattern = ir.irPVar("n", numType);
      const caseBody = ir.irAtomExpr(ir.irVar("n", numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irLit(42, numType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("if (true)");
      expect(result.code).toContain("const n = _s;");
    });

    it("generates match with wildcard pattern", () => {
      const pattern = ir.irPWildcard(numType);
      const caseBody = ir.irAtomExpr(ir.irLit(0, numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irLit(42, numType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("if (true)");
      expect(result.code).toContain("return 0;");
    });

    it("generates match with literal pattern", () => {
      const pattern = ir.irPLit(0, numType);
      const caseBody = ir.irAtomExpr(ir.irLit(1, numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irVar("x", numType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("_s === 0");
    });

    it("generates match with string literal pattern", () => {
      const pattern = ir.irPLit("hello", strType);
      const caseBody = ir.irAtomExpr(ir.irLit(1, numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irVar("x", strType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain('_s === "hello"');
    });

    it("generates match with boolean literal pattern", () => {
      const pattern = ir.irPLit(true, boolType);
      const caseBody = ir.irAtomExpr(ir.irLit(1, numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irVar("x", boolType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("_s === true");
    });

    it("generates match with constructor pattern", () => {
      const maybeType = appType({ kind: "TCon", name: "Maybe" }, numType);
      const argPattern = ir.irPVar("n", numType);
      const pattern = ir.irPCon("Just", [argPattern], maybeType);
      const caseBody = ir.irAtomExpr(ir.irVar("n", numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irVar("x", maybeType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, ["Just", "Nothing"]);
      // Uses switch-based dispatch for unique constructor tags
      expect(result.code).toContain("switch (_s.$tag)");
      expect(result.code).toContain('case "Just"');
      expect(result.code).toContain("const n = _s.$args[0]");
    });

    it("generates match with tuple pattern", () => {
      const tupleT = tupleType([numType, numType]);
      const pattern = ir.irPTuple([ir.irPVar("a", numType), ir.irPVar("b", numType)], tupleT);
      const caseBody = ir.irAtomExpr(ir.irVar("a", numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irVar("t", tupleT), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("const a = _s[0]");
      expect(result.code).toContain("const b = _s[1]");
    });

    it("generates match with tuple pattern containing literals", () => {
      const tupleT = tupleType([numType, numType]);
      const pattern = ir.irPTuple([ir.irPLit(1, numType), ir.irPLit(2, numType)], tupleT);
      const caseBody = ir.irAtomExpr(ir.irLit(42, numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irVar("t", tupleT), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("_s[0] === 1 && _s[1] === 2");
    });

    it("generates match with record pattern", () => {
      const recType: Type = { kind: "TRecord", fields: new Map([["x", numType]]), row: null };
      const pattern = ir.irPRecord([ir.irPRecordField("x", ir.irPVar("val", numType))], recType);
      const caseBody = ir.irAtomExpr(ir.irVar("val", numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irVar("r", recType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("const val = _s.x");
    });

    it("generates multiple match cases with else if", () => {
      const cases = [
        ir.irCase(ir.irPLit(0, numType), ir.irAtomExpr(ir.irLit(1, numType))),
        ir.irCase(ir.irPLit(1, numType), ir.irAtomExpr(ir.irLit(2, numType))),
        ir.irCase(ir.irPWildcard(numType), ir.irAtomExpr(ir.irLit(0, numType))),
      ];
      const binding = ir.irMatchBinding(ir.irVar("x", numType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("if (_s === 0)");
      expect(result.code).toContain("} else if (_s === 1)");
      expect(result.code).toContain("} else if (true)");
    });

    it("generates nested patterns in constructors", () => {
      const maybeType = appType({ kind: "TCon", name: "Maybe" }, numType);
      const innerPattern = ir.irPLit(42, numType);
      const pattern = ir.irPCon("Just", [innerPattern], maybeType);
      const caseBody = ir.irAtomExpr(ir.irLit(1, numType));
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irVar("x", maybeType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, ["Just", "Nothing"]);
      // Uses switch-based dispatch; nested literal check inside case
      expect(result.code).toContain("switch (_s.$tag)");
      expect(result.code).toContain('case "Just"');
      expect(result.code).toContain("_s.$args[0] === 42");
    });

    it("generates match with complex case body", () => {
      // Match case body that has let bindings
      const innerBinding = ir.irAtomBinding(ir.irLit(1, numType));
      const caseBody = ir.irLet("y", innerBinding, ir.irAtomExpr(ir.irVar("y", numType)));
      const pattern = ir.irPVar("x", numType);
      const cases = [ir.irCase(pattern, caseBody)];
      const binding = ir.irMatchBinding(ir.irLit(42, numType), cases, numType);
      const body = ir.irAtomExpr(ir.irVar("_t", numType));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("const y = 1");
      expect(result.code).toContain("return y;");
    });
  });

  describe("Closures", () => {
    it("generates closure binding", () => {
      const fnType_ = funType(numType, numType);
      const captures = [ir.irVar("y", numType)];
      const binding = ir.irClosureBinding("$fn_1", captures, fnType_);
      const body = ir.irAtomExpr(ir.irVar("_t", fnType_));
      const expr = ir.irLet("_t", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("{ $fn: $fn_1, $env: [y] }");
    });
  });

  describe("Variable name sanitization", () => {
    it("sanitizes invalid characters in variable names", () => {
      const binding = ir.irAtomBinding(ir.irLit(1, numType));
      const body = ir.irAtomExpr(ir.irVar("x-y", numType));
      const expr = ir.irLet("x-y", binding, body);
      const result = generateJS(expr, []);
      expect(result.code).toContain("const x_y = 1;");
    });
  });

  describe("Output format", () => {
    it("returns empty warnings array", () => {
      const expr = ir.irAtomExpr(ir.irLit(42, numType));
      const result = generateJS(expr, []);
      expect(result.warnings).toEqual([]);
    });

    it("includes console.log at end", () => {
      const expr = ir.irAtomExpr(ir.irLit(42, numType));
      const result = generateJS(expr, []);
      expect(result.code).toContain("console.log($result);");
    });
  });
});

describe("Runtime", () => {
  it("exports RUNTIME constant", () => {
    expect(typeof RUNTIME).toBe("string");
    expect(RUNTIME).toContain("$apply");
    expect(RUNTIME).toContain("$con");
    expect(RUNTIME).toContain("$eq");
  });

  // Regression test: escape sequences in runtime should be properly escaped
  // so they appear as literal \t, \n, \r in the output JavaScript
  it("contains properly escaped sequences in isSpace function", () => {
    // Bug: The template string in runtime.ts was interpreting escape sequences
    // like \t, \n, \r as actual tab/newline/carriage-return characters,
    // which caused syntax errors when the output was parsed as JavaScript
    expect(RUNTIME).toContain('c === "\\t"');
    expect(RUNTIME).toContain('c === "\\n"');
    expect(RUNTIME).toContain('c === "\\r"');
    // Should NOT contain literal tab/newline in the string comparison
    expect(RUNTIME).not.toMatch(/c === "\t"/);
    expect(RUNTIME).not.toMatch(/c === "\n"/);
    expect(RUNTIME).not.toMatch(/c === "\r"/);
  });
});

describe("Foreign Functions", () => {
  it("generates foreign function reference with $foreign lookup", () => {
    const fnType_ = funType(strType, numType);
    const foreignVar = ir.irForeignVar("String", "length", fnType_);
    const expr = ir.irAtomExpr(foreignVar);
    const result = generateJS(expr, []);
    expect(result.code).toContain('$foreign["String"]["length"]');
  });

  it("generates foreign function application", () => {
    const fnType_ = funType(strType, numType);
    const foreignVar = ir.irForeignVar("String", "length", fnType_);
    const binding = ir.irAppBinding(foreignVar, ir.irLit("hello", strType), numType);
    const body = ir.irAtomExpr(ir.irVar("_t", numType));
    const expr = ir.irLet("_t", binding, body);
    const result = generateJS(expr, []);
    expect(result.code).toContain('$foreign["String"]["length"]("hello")');
  });

  it("generates curried foreign function applications", () => {
    // slice "hello" 0 - returns a function
    const sliceType = funType(strType, funType(numType, funType(numType, strType)));
    const partialSliceType = funType(numType, funType(numType, strType));

    const foreignVar = ir.irForeignVar("String", "slice", sliceType);

    // First application: slice "hello"
    const binding1 = ir.irAppBinding(foreignVar, ir.irLit("hello", strType), partialSliceType);
    const body1 = ir.irAtomExpr(ir.irVar("_t1", partialSliceType));
    const expr = ir.irLet("_t1", binding1, body1);

    const result = generateJS(expr, []);
    expect(result.code).toContain('$foreign["String"]["slice"]("hello")');
  });

  it("includes $foreign in runtime", () => {
    const expr = ir.irAtomExpr(ir.irLit(42, numType));
    const result = generateJS(expr, []);
    expect(result.code).toContain("const $foreign = {");
    // Should contain String and Char modules
    expect(result.code).toContain("String: {");
    expect(result.code).toContain("Char: {");
  });
});

describe("TCO (Tail Call Optimization)", () => {
  it("generates valid JS for recursive function with pattern matching", () => {
    // This is a simpler test that just verifies the code is syntactically valid
    // The actual continue-in-IIFE bug would cause a runtime error
    const listType = appType({ kind: "TCon", name: "List" }, numType);

    // Simple pattern match without TCO (non-tail recursive)
    const matchBinding = ir.irMatchBinding(
      ir.irVar("xs", listType),
      [
        {
          pattern: ir.irPCon("Nil", [], listType),
          body: ir.irAtomExpr(ir.irLit(0, numType)),
        },
        {
          pattern: ir.irPCon(
            "Cons",
            [ir.irPVar("x", numType), ir.irPVar("rest", listType)],
            listType,
          ),
          body: ir.irAtomExpr(ir.irVar("x", numType)),
        },
      ],
      numType,
    );

    const body = ir.irLet("_match", matchBinding, ir.irAtomExpr(ir.irVar("_match", numType)));
    const lambda = ir.irLambdaBinding("xs", listType, body, funType(listType, numType));

    const expr = ir.irLetRec(
      [ir.irRecBinding("head", lambda)],
      ir.irAtomExpr(ir.irVar("head", funType(listType, numType))),
    );

    const result = generateJS(expr, ["Nil", "Cons"]);

    // Should use switch-based pattern matching (optimized)
    expect(result.code).toContain("switch (_s.$tag)");
    expect(result.code).toContain('case "Nil"');
    expect(result.code).toContain('case "Cons"');
  });
});
