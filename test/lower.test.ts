import { describe, expect, it } from "bun:test";
import * as ast from "../src/ast";
import type { CheckOutput, Type, TypeEnv } from "../src/checker";
import { lowerToIR } from "../src/lower";

// Create a minimal CheckOutput with empty substitution
const makeCheckOutput = (subst: Map<string, Type> = new Map()): CheckOutput => ({
  type: { kind: "TCon", name: "number" },
  subst,
  diagnostics: [],
  constraints: [],
  types: new Map(),
  spanTypes: new Map(),
});

// Create type environment with common bindings
const makeTypeEnv = (bindings: Record<string, Type> = {}): TypeEnv => {
  const env: TypeEnv = new Map();
  for (const [name, type] of Object.entries(bindings)) {
    env.set(name, { vars: [], constraints: [], type });
  }
  return env;
};

// Type helpers
const numType: Type = { kind: "TCon", name: "number" };
const strType: Type = { kind: "TCon", name: "string" };
const boolType: Type = { kind: "TCon", name: "boolean" };
const funType = (param: Type, ret: Type): Type => ({ kind: "TFun", param, ret });

describe("lowerToIR", () => {
  describe("Literals", () => {
    it("lowers number literals", () => {
      const expr = ast.num(42);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRAtomExpr");
      if (ir.kind === "IRAtomExpr") {
        expect(ir.atom.kind).toBe("IRLit");
        if (ir.atom.kind === "IRLit") {
          expect(ir.atom.value).toBe(42);
        }
      }
    });

    it("lowers string literals", () => {
      const expr = ast.str("hello");
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRAtomExpr");
      if (ir.kind === "IRAtomExpr") {
        expect(ir.atom.kind).toBe("IRLit");
        if (ir.atom.kind === "IRLit") {
          expect(ir.atom.value).toBe("hello");
        }
      }
    });

    it("lowers boolean literals", () => {
      const expr = ast.bool(true);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRAtomExpr");
      if (ir.kind === "IRAtomExpr") {
        expect(ir.atom.kind).toBe("IRLit");
        if (ir.atom.kind === "IRLit") {
          expect(ir.atom.value).toBe(true);
        }
      }
    });
  });

  describe("Variables", () => {
    it("lowers variable references", () => {
      const expr = ast.var_("x");
      const env = makeTypeEnv({ x: numType });
      const ir = lowerToIR(expr, env, makeCheckOutput());

      expect(ir.kind).toBe("IRAtomExpr");
      if (ir.kind === "IRAtomExpr") {
        expect(ir.atom.kind).toBe("IRVar");
        if (ir.atom.kind === "IRVar") {
          expect(ir.atom.name).toBe("x");
        }
      }
    });

    it("throws for unknown variable", () => {
      const expr = ast.var_("unknown");
      expect(() => lowerToIR(expr, makeTypeEnv(), makeCheckOutput())).toThrow(
        "Unknown variable during lowering: unknown",
      );
    });
  });

  describe("Let bindings", () => {
    it("lowers simple let binding", () => {
      // let x = 1 in x
      const expr = ast.let_("x", ast.num(1), ast.var_("x"));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.name).toBe("x");
        expect(ir.binding.kind).toBe("IRAtomBinding");
      }
    });

    it("lowers nested let bindings", () => {
      // let x = 1 in let y = 2 in x
      const expr = ast.let_("x", ast.num(1), ast.let_("y", ast.num(2), ast.var_("x")));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.name).toBe("x");
        expect(ir.body.kind).toBe("IRLet");
      }
    });
  });

  describe("Recursive bindings", () => {
    it("lowers letrec with lambda", () => {
      // let rec f = \x => f x in f
      const expr = ast.letRec(
        [ast.recBinding("f", ast.abs("x", ast.app(ast.var_("f"), ast.var_("x"))))],
        ast.var_("f"),
      );
      const env = makeTypeEnv();
      const ir = lowerToIR(expr, env, makeCheckOutput());

      // Should produce an IRLetRec somewhere in the result
      let found = false;
      let current = ir;
      while (current.kind === "IRLet" || current.kind === "IRLetRec") {
        if (current.kind === "IRLetRec") {
          found = true;
          break;
        }
        current = current.body;
      }
      expect(found).toBe(true);
    });
  });

  describe("Lambda expressions", () => {
    it("lowers lambda expression", () => {
      // \x => x
      const expr = ast.abs("x", ast.var_("x"));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      // Lambdas are wrapped in a let binding
      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRLambdaBinding");
      }
    });

    it("lowers nested lambda", () => {
      // \x => \y => x
      const inner = ast.abs("y", ast.var_("x"));
      const expr = ast.abs("x", inner);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRLambdaBinding");
        if (ir.binding.kind === "IRLambdaBinding") {
          // Body should also contain a lambda
          let foundInnerLambda = false;
          let body = ir.binding.body;
          while (body.kind === "IRLet" || body.kind === "IRLetRec") {
            if (body.kind === "IRLet" && body.binding.kind === "IRLambdaBinding") {
              foundInnerLambda = true;
            }
            body = body.body;
          }
          expect(foundInnerLambda).toBe(true);
        }
      }
    });
  });

  describe("Function application", () => {
    it("lowers simple application", () => {
      // f 1
      const expr = ast.app(ast.var_("f"), ast.num(1));
      const env = makeTypeEnv({ f: funType(numType, numType) });
      const ir = lowerToIR(expr, env, makeCheckOutput());

      // Should produce a let with IRAppBinding
      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRAppBinding");
      }
    });

    it("lowers curried application", () => {
      // f 1 2
      const expr = ast.app(ast.app(ast.var_("f"), ast.num(1)), ast.num(2));
      const innerFnType = funType(numType, numType);
      const outerFnType = funType(numType, innerFnType);
      const env = makeTypeEnv({ f: outerFnType });
      const ir = lowerToIR(expr, env, makeCheckOutput());

      // Should produce nested lets for each application
      expect(ir.kind).toBe("IRLet");
    });
  });

  describe("Binary operations", () => {
    it("lowers addition", () => {
      // 1 + 2
      const expr = ast.binOp("+", ast.num(1), ast.num(2));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRBinOpBinding");
        if (ir.binding.kind === "IRBinOpBinding") {
          expect(ir.binding.op).toBe("+");
        }
      }
    });

    it("lowers comparison operators", () => {
      // 1 < 2
      const expr = ast.binOp("<", ast.num(1), ast.num(2));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRBinOpBinding");
        if (ir.binding.kind === "IRBinOpBinding") {
          expect(ir.binding.op).toBe("<");
          // Result type should be boolean
          expect(ir.binding.type).toEqual(boolType);
        }
      }
    });

    it("handles string concatenation", () => {
      // "a" + "b"
      const expr = ast.binOp("+", ast.str("a"), ast.str("b"));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet" && ir.binding.kind === "IRBinOpBinding") {
        expect(ir.binding.type).toEqual(strType);
      }
    });

    it("normalizes complex operands to atoms", () => {
      // (1 + 2) + 3
      const left = ast.binOp("+", ast.num(1), ast.num(2));
      const expr = ast.binOp("+", left, ast.num(3));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      // First let should bind the result of (1 + 2)
      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRBinOpBinding");
        // Body should have another let for the outer addition
        expect(ir.body.kind).toBe("IRLet");
      }
    });
  });

  describe("If expressions", () => {
    it("lowers if expression", () => {
      // if true then 1 else 2
      const expr = ast.if_(ast.bool(true), ast.num(1), ast.num(2));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRIfBinding");
        if (ir.binding.kind === "IRIfBinding") {
          expect(ir.binding.thenBranch.kind).toBe("IRAtomExpr");
          expect(ir.binding.elseBranch.kind).toBe("IRAtomExpr");
        }
      }
    });

    it("normalizes complex condition", () => {
      // if 1 < 2 then 3 else 4
      const cond = ast.binOp("<", ast.num(1), ast.num(2));
      const expr = ast.if_(cond, ast.num(3), ast.num(4));
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      // First binding should be the comparison, then the if
      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRBinOpBinding");
        expect(ir.body.kind).toBe("IRLet");
        if (ir.body.kind === "IRLet") {
          expect(ir.body.binding.kind).toBe("IRIfBinding");
        }
      }
    });
  });

  describe("Tuples", () => {
    it("lowers tuple", () => {
      // (1, 2)
      const expr = ast.tuple([ast.num(1), ast.num(2)]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRTupleBinding");
        if (ir.binding.kind === "IRTupleBinding") {
          expect(ir.binding.elements).toHaveLength(2);
        }
      }
    });

    it("unwraps single-element tuple", () => {
      // (1) => 1
      const expr = ast.tuple([ast.num(1)]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRAtomExpr");
    });

    it("normalizes complex tuple elements", () => {
      // (1 + 2, 3)
      const expr = ast.tuple([ast.binOp("+", ast.num(1), ast.num(2)), ast.num(3)]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      // First binding for 1 + 2, then tuple binding
      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRBinOpBinding");
      }
    });
  });

  describe("Records", () => {
    it("lowers record", () => {
      // { x = 1, y = 2 }
      const expr = ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.num(2))]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRRecordBinding");
        if (ir.binding.kind === "IRRecordBinding") {
          expect(ir.binding.fields).toHaveLength(2);
          expect(ir.binding.fields[0]!.name).toBe("x");
          expect(ir.binding.fields[1]!.name).toBe("y");
        }
      }
    });

    it("lowers field access", () => {
      // r.x
      const recordType: Type = { kind: "TRecord", fields: new Map([["x", numType]]), row: null };
      const expr = ast.fieldAccess(ast.var_("r"), "x");
      const env = makeTypeEnv({ r: recordType });
      const ir = lowerToIR(expr, env, makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRFieldAccessBinding");
        if (ir.binding.kind === "IRFieldAccessBinding") {
          expect(ir.binding.field).toBe("x");
        }
      }
    });
  });

  describe("Pattern matching", () => {
    it("lowers match with variable pattern", () => {
      // match 1 with x => x end
      const expr = ast.match(ast.num(1), [ast.case_(ast.pvar("x"), ast.var_("x"))]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet") {
        expect(ir.binding.kind).toBe("IRMatchBinding");
        if (ir.binding.kind === "IRMatchBinding") {
          expect(ir.binding.cases).toHaveLength(1);
          expect(ir.binding.cases[0]!.pattern.kind).toBe("IRPVar");
        }
      }
    });

    it("lowers match with wildcard pattern", () => {
      // match 1 with _ => 0 end
      const expr = ast.match(ast.num(1), [ast.case_(ast.pwildcard(), ast.num(0))]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet" && ir.binding.kind === "IRMatchBinding") {
        expect(ir.binding.cases[0]!.pattern.kind).toBe("IRPWildcard");
      }
    });

    it("lowers match with literal patterns", () => {
      // match 1 with 0 => "zero" | _ => "other" end
      const expr = ast.match(ast.num(1), [
        ast.case_(ast.plit(0), ast.str("zero")),
        ast.case_(ast.pwildcard(), ast.str("other")),
      ]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet" && ir.binding.kind === "IRMatchBinding") {
        expect(ir.binding.cases).toHaveLength(2);
        expect(ir.binding.cases[0]!.pattern.kind).toBe("IRPLit");
        expect(ir.binding.cases[1]!.pattern.kind).toBe("IRPWildcard");
      }
    });

    it("lowers match with constructor pattern", () => {
      // match x with Just n => n | Nothing => 0 end
      const maybeType: Type = { kind: "TApp", con: { kind: "TCon", name: "Maybe" }, arg: numType };
      const env = makeTypeEnv({ x: maybeType });
      const expr = ast.match(ast.var_("x"), [
        ast.case_(ast.pcon("Just", [ast.pvar("n")]), ast.var_("n")),
        ast.case_(ast.pcon("Nothing", []), ast.num(0)),
      ]);
      const ir = lowerToIR(expr, env, makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet" && ir.binding.kind === "IRMatchBinding") {
        expect(ir.binding.cases).toHaveLength(2);
        expect(ir.binding.cases[0]!.pattern.kind).toBe("IRPCon");
        if (ir.binding.cases[0]!.pattern.kind === "IRPCon") {
          expect(ir.binding.cases[0]!.pattern.name).toBe("Just");
        }
      }
    });

    it("lowers match with tuple pattern", () => {
      // match (1, 2) with (a, b) => a end
      const _tupleType: Type = { kind: "TTuple", elements: [numType, numType] };
      const expr = ast.match(ast.tuple([ast.num(1), ast.num(2)]), [
        ast.case_(ast.ptuple([ast.pvar("a"), ast.pvar("b")]), ast.var_("a")),
      ]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      // The tuple gets bound first, then the match
      let foundMatch = false;
      let current = ir;
      while (current.kind === "IRLet" || current.kind === "IRLetRec") {
        if (current.kind === "IRLet" && current.binding.kind === "IRMatchBinding") {
          foundMatch = true;
          expect(current.binding.cases[0]!.pattern.kind).toBe("IRPTuple");
          break;
        }
        current = current.body;
      }
      expect(foundMatch).toBe(true);
    });

    it("lowers match with record pattern", () => {
      // match { x = 1 } with { x = n } => n end
      const _recordType: Type = { kind: "TRecord", fields: new Map([["x", numType]]), row: null };
      const expr = ast.match(ast.record([ast.field("x", ast.num(1))]), [
        ast.case_(ast.precord([ast.pfield("x", ast.pvar("n"))]), ast.var_("n")),
      ]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      // The record gets bound first, then the match
      let foundMatch = false;
      let current = ir;
      while (current.kind === "IRLet" || current.kind === "IRLetRec") {
        if (current.kind === "IRLet" && current.binding.kind === "IRMatchBinding") {
          foundMatch = true;
          expect(current.binding.cases[0]!.pattern.kind).toBe("IRPRecord");
          break;
        }
        current = current.body;
      }
      expect(foundMatch).toBe(true);
    });
  });

  describe("ANF properties", () => {
    it("ensures all binop arguments are atoms", () => {
      // Complex expression: (f x) + (g y)
      const left = ast.app(ast.var_("f"), ast.var_("x"));
      const right = ast.app(ast.var_("g"), ast.var_("y"));
      const expr = ast.binOp("+", left, right);
      const env = makeTypeEnv({
        f: funType(numType, numType),
        g: funType(numType, numType),
        x: numType,
        y: numType,
      });
      const ir = lowerToIR(expr, env, makeCheckOutput());

      // Walk to find the binop and verify its operands are atoms
      let current = ir;
      while (current.kind === "IRLet" || current.kind === "IRLetRec") {
        if (current.kind === "IRLet" && current.binding.kind === "IRBinOpBinding") {
          expect(current.binding.left.kind).toMatch(/^IR(Lit|Var)$/);
          expect(current.binding.right.kind).toMatch(/^IR(Lit|Var)$/);
          break;
        }
        current = current.body;
      }
    });

    it("ensures all application arguments are atoms", () => {
      // f (1 + 2)
      const arg = ast.binOp("+", ast.num(1), ast.num(2));
      const expr = ast.app(ast.var_("f"), arg);
      const env = makeTypeEnv({ f: funType(numType, numType) });
      const ir = lowerToIR(expr, env, makeCheckOutput());

      // Walk to find the app and verify both operands are atoms
      let current = ir;
      while (current.kind === "IRLet" || current.kind === "IRLetRec") {
        if (current.kind === "IRLet" && current.binding.kind === "IRAppBinding") {
          expect(current.binding.func.kind).toMatch(/^IR(Lit|Var)$/);
          expect(current.binding.arg.kind).toMatch(/^IR(Lit|Var)$/);
          break;
        }
        current = current.body;
      }
    });
  });

  describe("Type resolution", () => {
    it("applies substitution to types", () => {
      const subst = new Map<string, Type>();
      subst.set("a", numType);

      // x has type variable 'a' which should resolve to number
      const env: TypeEnv = new Map();
      env.set("x", { vars: [], constraints: [], type: { kind: "TVar", name: "a" } });

      const expr = ast.var_("x");
      const ir = lowerToIR(expr, env, makeCheckOutput(subst));

      expect(ir.kind).toBe("IRAtomExpr");
      if (ir.kind === "IRAtomExpr" && ir.atom.kind === "IRVar") {
        expect(ir.atom.type).toEqual(numType);
      }
    });

    it("applies substitution to tuple types", () => {
      const subst = new Map<string, Type>();
      subst.set("a", numType);

      // x has tuple type (a, a) which should resolve to (number, number)
      const tupleWithVars: Type = {
        kind: "TTuple",
        elements: [
          { kind: "TVar", name: "a" },
          { kind: "TVar", name: "a" },
        ],
      };
      const env: TypeEnv = new Map();
      env.set("x", { vars: [], constraints: [], type: tupleWithVars });

      const expr = ast.var_("x");
      const ir = lowerToIR(expr, env, makeCheckOutput(subst));

      expect(ir.kind).toBe("IRAtomExpr");
      if (ir.kind === "IRAtomExpr" && ir.atom.kind === "IRVar") {
        expect(ir.atom.type.kind).toBe("TTuple");
        if (ir.atom.type.kind === "TTuple") {
          expect(ir.atom.type.elements[0]).toEqual(numType);
          expect(ir.atom.type.elements[1]).toEqual(numType);
        }
      }
    });

    it("applies substitution to TApp types", () => {
      const subst = new Map<string, Type>();
      subst.set("a", numType);

      // x has type Maybe a
      const maybeA: Type = {
        kind: "TApp",
        con: { kind: "TCon", name: "Maybe" },
        arg: { kind: "TVar", name: "a" },
      };
      const env: TypeEnv = new Map();
      env.set("x", { vars: [], constraints: [], type: maybeA });

      const expr = ast.var_("x");
      const ir = lowerToIR(expr, env, makeCheckOutput(subst));

      expect(ir.kind).toBe("IRAtomExpr");
      if (ir.kind === "IRAtomExpr" && ir.atom.kind === "IRVar" && ir.atom.type.kind === "TApp") {
        expect(ir.atom.type.arg).toEqual(numType);
      }
    });
  });

  describe("Error handling", () => {
    it("throws for field access on non-record type", () => {
      const expr = ast.fieldAccess(ast.var_("x"), "field");
      const env = makeTypeEnv({ x: numType });
      expect(() => lowerToIR(expr, env, makeCheckOutput())).toThrow(
        "Expected record type with field field, got TCon",
      );
    });

    it("throws for field access on record without that field", () => {
      const recordType: Type = { kind: "TRecord", fields: new Map([["y", numType]]), row: null };
      const expr = ast.fieldAccess(ast.var_("r"), "x");
      const env = makeTypeEnv({ r: recordType });
      expect(() => lowerToIR(expr, env, makeCheckOutput())).toThrow(
        "Expected record type with field x, got TRecord",
      );
    });
  });

  describe("Boolean literal patterns", () => {
    it("lowers match with true literal pattern", () => {
      const expr = ast.match(ast.bool(true), [
        ast.case_(ast.plit(true), ast.num(1)),
        ast.case_(ast.plit(false), ast.num(0)),
      ]);
      const ir = lowerToIR(expr, makeTypeEnv(), makeCheckOutput());

      expect(ir.kind).toBe("IRLet");
      if (ir.kind === "IRLet" && ir.binding.kind === "IRMatchBinding") {
        expect(ir.binding.cases[0]!.pattern.kind).toBe("IRPLit");
        if (ir.binding.cases[0]!.pattern.kind === "IRPLit") {
          expect(ir.binding.cases[0]!.pattern.value).toBe(true);
          expect(ir.binding.cases[0]!.pattern.type).toEqual(boolType);
        }
      }
    });
  });

  describe("Qualified variable access", () => {
    it("lowers qualified variable when member is in type environment", () => {
      // Simulate: use Math (..); Math.double 5
      // where double is imported into the type environment
      const expr = ast.qualifiedVar("Math", "double");
      const env = makeTypeEnv({ double: funType(numType, numType) });
      const ir = lowerToIR(expr, env, makeCheckOutput());

      expect(ir.kind).toBe("IRAtomExpr");
      if (ir.kind === "IRAtomExpr") {
        expect(ir.atom.kind).toBe("IRVar");
        if (ir.atom.kind === "IRVar") {
          expect(ir.atom.name).toBe("double");
        }
      }
    });

    it("throws for qualified variable when member is not imported", () => {
      // Math.unknown where 'unknown' is not in the type environment
      const expr = ast.qualifiedVar("Math", "unknown");
      expect(() => lowerToIR(expr, makeTypeEnv(), makeCheckOutput())).toThrow(
        "Qualified access (Math.unknown) requires importing the module",
      );
    });
  });
});
