import { describe, expect, it } from "bun:test";
import type { Type } from "../src/checker";
import * as ir from "../src/ir";

// Test type helpers
const numType: Type = { kind: "TCon", name: "number" };
const strType: Type = { kind: "TCon", name: "string" };
const boolType: Type = { kind: "TCon", name: "boolean" };
const funType = (param: Type, ret: Type): Type => ({ kind: "TFun", param, ret });
const tupleType = (elements: Type[]): Type => ({ kind: "TTuple", elements });

describe("IR Smart Constructors", () => {
  describe("Atoms", () => {
    it("creates literal with number", () => {
      const lit = ir.irLit(42, numType);
      expect(lit.kind).toBe("IRLit");
      expect(lit.value).toBe(42);
      expect(lit.type).toBe(numType);
    });

    it("creates literal with string", () => {
      const lit = ir.irLit("hello", strType);
      expect(lit.kind).toBe("IRLit");
      expect(lit.value).toBe("hello");
      expect(lit.type).toBe(strType);
    });

    it("creates literal with boolean", () => {
      const lit = ir.irLit(true, boolType);
      expect(lit.kind).toBe("IRLit");
      expect(lit.value).toBe(true);
      expect(lit.type).toBe(boolType);
    });

    it("creates variable reference", () => {
      const v = ir.irVar("x", numType);
      expect(v.kind).toBe("IRVar");
      expect(v.name).toBe("x");
      expect(v.type).toBe(numType);
    });
  });

  describe("Expressions", () => {
    it("creates atom expression", () => {
      const lit = ir.irLit(42, numType);
      const expr = ir.irAtomExpr(lit);
      expect(expr.kind).toBe("IRAtomExpr");
      expect(expr.atom).toBe(lit);
      expect(expr.type).toBe(numType);
    });

    it("creates let binding", () => {
      const binding = ir.irAtomBinding(ir.irLit(1, numType));
      const body = ir.irAtomExpr(ir.irVar("x", numType));
      const letExpr = ir.irLet("x", binding, body);

      expect(letExpr.kind).toBe("IRLet");
      expect(letExpr.name).toBe("x");
      expect(letExpr.binding).toBe(binding);
      expect(letExpr.body).toBe(body);
      expect(letExpr.type).toBe(numType);
    });

    it("creates recursive let binding", () => {
      const fnType = funType(numType, numType);
      const body = ir.irAtomExpr(ir.irVar("f", fnType));
      const lambdaBody = ir.irAtomExpr(ir.irVar("n", numType));
      const binding = ir.irLambdaBinding("n", numType, lambdaBody, fnType);
      const letRec = ir.irLetRec("f", binding, body);

      expect(letRec.kind).toBe("IRLetRec");
      expect(letRec.name).toBe("f");
      expect(letRec.binding).toBe(binding);
      expect(letRec.body).toBe(body);
    });
  });

  describe("Bindings", () => {
    it("creates atom binding", () => {
      const lit = ir.irLit(42, numType);
      const binding = ir.irAtomBinding(lit);
      expect(binding.kind).toBe("IRAtomBinding");
      expect(binding.atom).toBe(lit);
      expect(binding.type).toBe(numType);
    });

    it("creates application binding", () => {
      const fn = ir.irVar("f", funType(numType, numType));
      const arg = ir.irLit(42, numType);
      const binding = ir.irAppBinding(fn, arg, numType);

      expect(binding.kind).toBe("IRAppBinding");
      expect(binding.func).toBe(fn);
      expect(binding.arg).toBe(arg);
      expect(binding.type).toBe(numType);
    });

    it("creates binary operation binding", () => {
      const left = ir.irLit(1, numType);
      const right = ir.irLit(2, numType);
      const binding = ir.irBinOpBinding("+", left, right, numType, numType);

      expect(binding.kind).toBe("IRBinOpBinding");
      expect(binding.op).toBe("+");
      expect(binding.left).toBe(left);
      expect(binding.right).toBe(right);
      expect(binding.operandType).toBe(numType);
      expect(binding.type).toBe(numType);
    });

    it("creates if binding", () => {
      const cond = ir.irLit(true, boolType);
      const thenBranch = ir.irAtomExpr(ir.irLit(1, numType));
      const elseBranch = ir.irAtomExpr(ir.irLit(2, numType));
      const binding = ir.irIfBinding(cond, thenBranch, elseBranch, numType);

      expect(binding.kind).toBe("IRIfBinding");
      expect(binding.cond).toBe(cond);
      expect(binding.thenBranch).toBe(thenBranch);
      expect(binding.elseBranch).toBe(elseBranch);
      expect(binding.type).toBe(numType);
    });

    it("creates tuple binding", () => {
      const elements = [ir.irLit(1, numType), ir.irLit(2, numType)];
      const type = tupleType([numType, numType]);
      const binding = ir.irTupleBinding(elements, type);

      expect(binding.kind).toBe("IRTupleBinding");
      expect(binding.elements).toEqual(elements);
      expect(binding.type).toBe(type);
    });

    it("creates record binding", () => {
      const fields = [
        ir.irRecordField("x", ir.irLit(1, numType)),
        ir.irRecordField("y", ir.irLit(2, numType)),
      ];
      const type: Type = {
        kind: "TRecord",
        fields: new Map([
          ["x", numType],
          ["y", numType],
        ]),
        row: null,
      };
      const binding = ir.irRecordBinding(fields, type);

      expect(binding.kind).toBe("IRRecordBinding");
      expect(binding.fields).toEqual(fields);
      expect(binding.type).toBe(type);
    });

    it("creates record field", () => {
      const value = ir.irLit(42, numType);
      const field = ir.irRecordField("x", value);

      expect(field.name).toBe("x");
      expect(field.value).toBe(value);
    });

    it("creates field access binding", () => {
      const recordType: Type = { kind: "TRecord", fields: new Map([["x", numType]]), row: null };
      const record = ir.irVar("r", recordType);
      const binding = ir.irFieldAccessBinding(record, "x", numType);

      expect(binding.kind).toBe("IRFieldAccessBinding");
      expect(binding.record).toBe(record);
      expect(binding.field).toBe("x");
      expect(binding.type).toBe(numType);
    });

    it("creates match binding", () => {
      const scrutinee = ir.irVar("x", numType);
      const pattern = ir.irPVar("n", numType);
      const body = ir.irAtomExpr(ir.irVar("n", numType));
      const cases = [ir.irCase(pattern, body)];
      const binding = ir.irMatchBinding(scrutinee, cases, numType);

      expect(binding.kind).toBe("IRMatchBinding");
      expect(binding.scrutinee).toBe(scrutinee);
      expect(binding.cases).toEqual(cases);
      expect(binding.type).toBe(numType);
    });

    it("creates lambda binding", () => {
      const body = ir.irAtomExpr(ir.irVar("x", numType));
      const fnType = funType(numType, numType);
      const binding = ir.irLambdaBinding("x", numType, body, fnType);

      expect(binding.kind).toBe("IRLambdaBinding");
      expect(binding.param).toBe("x");
      expect(binding.paramType).toBe(numType);
      expect(binding.body).toBe(body);
      expect(binding.type).toBe(fnType);
    });

    it("creates closure binding", () => {
      const captures = [ir.irVar("y", numType)];
      const fnType = funType(numType, numType);
      const binding = ir.irClosureBinding("$fn_1", captures, fnType);

      expect(binding.kind).toBe("IRClosureBinding");
      expect(binding.funcId).toBe("$fn_1");
      expect(binding.captures).toEqual(captures);
      expect(binding.type).toBe(fnType);
    });
  });

  describe("Patterns", () => {
    it("creates variable pattern", () => {
      const pat = ir.irPVar("x", numType);
      expect(pat.kind).toBe("IRPVar");
      expect(pat.name).toBe("x");
      expect(pat.type).toBe(numType);
    });

    it("creates wildcard pattern", () => {
      const pat = ir.irPWildcard(numType);
      expect(pat.kind).toBe("IRPWildcard");
      expect(pat.type).toBe(numType);
    });

    it("creates constructor pattern", () => {
      const argPat = ir.irPVar("n", numType);
      const maybeType: Type = {
        kind: "TApp",
        con: { kind: "TCon", name: "Maybe" },
        arg: numType,
      };
      const pat = ir.irPCon("Just", [argPat], maybeType);

      expect(pat.kind).toBe("IRPCon");
      expect(pat.name).toBe("Just");
      expect(pat.args).toEqual([argPat]);
      expect(pat.type).toBe(maybeType);
    });

    it("creates literal pattern", () => {
      const pat = ir.irPLit(42, numType);
      expect(pat.kind).toBe("IRPLit");
      expect(pat.value).toBe(42);
      expect(pat.type).toBe(numType);
    });

    it("creates tuple pattern", () => {
      const elements = [ir.irPVar("a", numType), ir.irPVar("b", numType)];
      const type = tupleType([numType, numType]);
      const pat = ir.irPTuple(elements, type);

      expect(pat.kind).toBe("IRPTuple");
      expect(pat.elements).toEqual(elements);
      expect(pat.type).toBe(type);
    });

    it("creates record pattern", () => {
      const fields = [ir.irPRecordField("x", ir.irPVar("val", numType))];
      const type: Type = { kind: "TRecord", fields: new Map([["x", numType]]), row: null };
      const pat = ir.irPRecord(fields, type);

      expect(pat.kind).toBe("IRPRecord");
      expect(pat.fields).toEqual(fields);
      expect(pat.type).toBe(type);
    });

    it("creates record field pattern", () => {
      const pattern = ir.irPVar("val", numType);
      const field = ir.irPRecordField("x", pattern);

      expect(field.name).toBe("x");
      expect(field.pattern).toBe(pattern);
    });
  });

  describe("Cases", () => {
    it("creates match case", () => {
      const pattern = ir.irPVar("x", numType);
      const body = ir.irAtomExpr(ir.irVar("x", numType));
      const c = ir.irCase(pattern, body);

      expect(c.pattern).toBe(pattern);
      expect(c.body).toBe(body);
    });
  });

  describe("Functions and Programs", () => {
    it("creates lifted function", () => {
      const body = ir.irAtomExpr(ir.irVar("x", numType));
      const fn = ir.irFunction("$fn_1", "$env", "x", numType, body, [numType], numType);

      expect(fn.id).toBe("$fn_1");
      expect(fn.envParam).toBe("$env");
      expect(fn.param).toBe("x");
      expect(fn.paramType).toBe(numType);
      expect(fn.body).toBe(body);
      expect(fn.captureTypes).toEqual([numType]);
      expect(fn.returnType).toBe(numType);
    });

    it("creates IR program", () => {
      const main = ir.irAtomExpr(ir.irLit(42, numType));
      const body = ir.irAtomExpr(ir.irVar("x", numType));
      const fn = ir.irFunction("$fn_1", "$env", "x", numType, body, [], numType);
      const program = ir.irProgram([fn], main);

      expect(program.functions).toEqual([fn]);
      expect(program.main).toBe(main);
    });

    it("creates program with no functions", () => {
      const main = ir.irAtomExpr(ir.irLit(42, numType));
      const program = ir.irProgram([], main);

      expect(program.functions).toEqual([]);
      expect(program.main).toBe(main);
    });
  });
});
