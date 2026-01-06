import { describe, expect, it } from "bun:test";
import * as ir from "../src/ir";
import {
  constantFolding,
  deadCodeElimination,
  optimize,
  tailCallOptimization,
} from "../src/optimize";

// Helper to create basic types
const numType = { kind: "TCon" as const, name: "number" };
const boolType = { kind: "TCon" as const, name: "boolean" };
const strType = { kind: "TCon" as const, name: "string" };

describe("Optimizations", () => {
  describe("Constant Folding", () => {
    it("folds arithmetic operations", () => {
      // let x = 1 + 2 in x
      const expr = ir.irLet(
        "x",
        ir.irBinOpBinding("+", ir.irLit(1, numType), ir.irLit(2, numType), numType, numType),
        ir.irAtomExpr(ir.irVar("x", numType)),
      );

      const result = constantFolding.run(expr);

      // Should fold to: let x = 3 in x
      expect(result.kind).toBe("IRLet");
      if (result.kind === "IRLet") {
        expect(result.binding.kind).toBe("IRAtomBinding");
        if (result.binding.kind === "IRAtomBinding") {
          expect(result.binding.atom.kind).toBe("IRLit");
          if (result.binding.atom.kind === "IRLit") {
            expect(result.binding.atom.value).toBe(3);
          }
        }
      }
    });

    it("folds multiplication", () => {
      // let x = 3 * 4 in x
      const expr = ir.irLet(
        "x",
        ir.irBinOpBinding("*", ir.irLit(3, numType), ir.irLit(4, numType), numType, numType),
        ir.irAtomExpr(ir.irVar("x", numType)),
      );

      const result = constantFolding.run(expr);

      if (result.kind === "IRLet" && result.binding.kind === "IRAtomBinding") {
        if (result.binding.atom.kind === "IRLit") {
          expect(result.binding.atom.value).toBe(12);
        }
      }
    });

    it("folds division", () => {
      // let x = 10 / 2 in x
      const expr = ir.irLet(
        "x",
        ir.irBinOpBinding("/", ir.irLit(10, numType), ir.irLit(2, numType), numType, numType),
        ir.irAtomExpr(ir.irVar("x", numType)),
      );

      const result = constantFolding.run(expr);

      if (result.kind === "IRLet" && result.binding.kind === "IRAtomBinding") {
        if (result.binding.atom.kind === "IRLit") {
          expect(result.binding.atom.value).toBe(5);
        }
      }
    });

    it("folds subtraction", () => {
      // let x = 10 - 3 in x
      const expr = ir.irLet(
        "x",
        ir.irBinOpBinding("-", ir.irLit(10, numType), ir.irLit(3, numType), numType, numType),
        ir.irAtomExpr(ir.irVar("x", numType)),
      );

      const result = constantFolding.run(expr);

      if (result.kind === "IRLet" && result.binding.kind === "IRAtomBinding") {
        if (result.binding.atom.kind === "IRLit") {
          expect(result.binding.atom.value).toBe(7);
        }
      }
    });

    it("folds comparison operations", () => {
      // let x = 5 < 10 in x
      const expr = ir.irLet(
        "x",
        ir.irBinOpBinding("<", ir.irLit(5, numType), ir.irLit(10, numType), numType, boolType),
        ir.irAtomExpr(ir.irVar("x", boolType)),
      );

      const result = constantFolding.run(expr);

      if (result.kind === "IRLet" && result.binding.kind === "IRAtomBinding") {
        if (result.binding.atom.kind === "IRLit") {
          expect(result.binding.atom.value).toBe(true);
        }
      }
    });

    it("folds equality operations", () => {
      // let x = 5 == 5 in x
      const expr = ir.irLet(
        "x",
        ir.irBinOpBinding("==", ir.irLit(5, numType), ir.irLit(5, numType), numType, boolType),
        ir.irAtomExpr(ir.irVar("x", boolType)),
      );

      const result = constantFolding.run(expr);

      if (result.kind === "IRLet" && result.binding.kind === "IRAtomBinding") {
        if (result.binding.atom.kind === "IRLit") {
          expect(result.binding.atom.value).toBe(true);
        }
      }
    });

    it("folds string concatenation", () => {
      // let x = "hello" + " world" in x
      const expr = ir.irLet(
        "x",
        ir.irBinOpBinding(
          "+",
          ir.irLit("hello", strType),
          ir.irLit(" world", strType),
          strType,
          strType,
        ),
        ir.irAtomExpr(ir.irVar("x", strType)),
      );

      const result = constantFolding.run(expr);

      if (result.kind === "IRLet" && result.binding.kind === "IRAtomBinding") {
        if (result.binding.atom.kind === "IRLit") {
          expect(result.binding.atom.value).toBe("hello world");
        }
      }
    });

    it("does not fold division by zero", () => {
      // let x = 10 / 0 in x
      const expr = ir.irLet(
        "x",
        ir.irBinOpBinding("/", ir.irLit(10, numType), ir.irLit(0, numType), numType, numType),
        ir.irAtomExpr(ir.irVar("x", numType)),
      );

      const result = constantFolding.run(expr);

      // Should NOT fold - binding should remain as IRBinOpBinding
      if (result.kind === "IRLet") {
        expect(result.binding.kind).toBe("IRBinOpBinding");
      }
    });

    it("does not fold when operands are not both literals", () => {
      // let y = x + 1 in y (where x is a variable)
      const expr = ir.irLet(
        "y",
        ir.irBinOpBinding("+", ir.irVar("x", numType), ir.irLit(1, numType), numType, numType),
        ir.irAtomExpr(ir.irVar("y", numType)),
      );

      const result = constantFolding.run(expr);

      if (result.kind === "IRLet") {
        expect(result.binding.kind).toBe("IRBinOpBinding");
      }
    });
  });

  describe("Dead Code Elimination", () => {
    it("removes unused let bindings", () => {
      // let unused = 100 in 42
      const expr = ir.irLet(
        "unused",
        ir.irAtomBinding(ir.irLit(100, numType)),
        ir.irAtomExpr(ir.irLit(42, numType)),
      );

      const result = deadCodeElimination.run(expr);

      // Should remove unused and return just 42
      expect(result.kind).toBe("IRAtomExpr");
      if (result.kind === "IRAtomExpr") {
        expect(result.atom.kind).toBe("IRLit");
        if (result.atom.kind === "IRLit") {
          expect(result.atom.value).toBe(42);
        }
      }
    });

    it("keeps used let bindings", () => {
      // let x = 100 in x
      const expr = ir.irLet(
        "x",
        ir.irAtomBinding(ir.irLit(100, numType)),
        ir.irAtomExpr(ir.irVar("x", numType)),
      );

      const result = deadCodeElimination.run(expr);

      // Should keep the binding since x is used
      expect(result.kind).toBe("IRLet");
    });

    it("removes multiple unused bindings", () => {
      // let a = 1 in let b = 2 in 42
      const expr = ir.irLet(
        "a",
        ir.irAtomBinding(ir.irLit(1, numType)),
        ir.irLet("b", ir.irAtomBinding(ir.irLit(2, numType)), ir.irAtomExpr(ir.irLit(42, numType))),
      );

      const result = deadCodeElimination.run(expr);

      // Should remove both a and b
      expect(result.kind).toBe("IRAtomExpr");
    });

    it("removes unused bindings in letrec", () => {
      // let rec unused = fn x => x in 42
      const funcType = { kind: "TFun" as const, param: numType, ret: numType };
      const expr = ir.irLetRec(
        [
          {
            name: "unused",
            binding: ir.irLambdaBinding(
              "x",
              numType,
              ir.irAtomExpr(ir.irVar("x", numType)),
              funcType,
            ),
          },
        ],
        ir.irAtomExpr(ir.irLit(42, numType)),
      );

      const result = deadCodeElimination.run(expr);

      // Should remove the unused recursive binding
      expect(result.kind).toBe("IRAtomExpr");
    });

    it("keeps used bindings in letrec", () => {
      // let rec f = fn x => x in f 10
      const funcType = { kind: "TFun" as const, param: numType, ret: numType };
      const expr = ir.irLetRec(
        [
          {
            name: "f",
            binding: ir.irLambdaBinding(
              "x",
              numType,
              ir.irAtomExpr(ir.irVar("x", numType)),
              funcType,
            ),
          },
        ],
        ir.irLet(
          "result",
          ir.irAppBinding(ir.irVar("f", funcType), ir.irLit(10, numType), numType),
          ir.irAtomExpr(ir.irVar("result", numType)),
        ),
      );

      const result = deadCodeElimination.run(expr);

      // Should keep f since it's used
      expect(result.kind).toBe("IRLetRec");
    });
  });

  describe("Combined Optimization Pipeline", () => {
    it("applies both constant folding and DCE", () => {
      // let unused = 1 + 2 in let x = 3 * 4 in x
      const expr = ir.irLet(
        "unused",
        ir.irBinOpBinding("+", ir.irLit(1, numType), ir.irLit(2, numType), numType, numType),
        ir.irLet(
          "x",
          ir.irBinOpBinding("*", ir.irLit(3, numType), ir.irLit(4, numType), numType, numType),
          ir.irAtomExpr(ir.irVar("x", numType)),
        ),
      );

      const result = optimize(expr);

      // Should fold 3*4 to 12, and remove unused binding
      expect(result.kind).toBe("IRLet");
      if (result.kind === "IRLet") {
        expect(result.name).toBe("x");
        expect(result.binding.kind).toBe("IRAtomBinding");
        if (result.binding.kind === "IRAtomBinding" && result.binding.atom.kind === "IRLit") {
          expect(result.binding.atom.value).toBe(12);
        }
      }
    });
  });

  describe("Tail-Call Optimization", () => {
    it("marks simple tail-recursive function", () => {
      // let rec f x = if x <= 0 then x else f (x - 1) in f 10
      const funcType = { kind: "TFun" as const, param: numType, ret: numType };

      // Build the tail call: f (x - 1)
      // As IR: let _t1 = x - 1 in let _t2 = f _t1 in _t2
      const tailCallExpr = ir.irLet(
        "_t1",
        ir.irBinOpBinding("-", ir.irVar("x", numType), ir.irLit(1, numType), numType, numType),
        ir.irLet(
          "_t2",
          ir.irAppBinding(ir.irVar("f", funcType), ir.irVar("_t1", numType), numType),
          ir.irAtomExpr(ir.irVar("_t2", numType)),
        ),
      );

      // Build the if: if x <= 0 then x else <tail call>
      const ifBinding = ir.irIfBinding(
        ir.irVar("_cond", boolType),
        ir.irAtomExpr(ir.irVar("x", numType)),
        tailCallExpr,
        numType,
      );

      // Full function body: let _cond = x <= 0 in let _result = if ... in _result
      const funcBody = ir.irLet(
        "_cond",
        ir.irBinOpBinding("<=", ir.irVar("x", numType), ir.irLit(0, numType), numType, boolType),
        ir.irLet("_result", ifBinding, ir.irAtomExpr(ir.irVar("_result", numType))),
      );

      const expr = ir.irLetRec(
        [
          {
            name: "f",
            binding: ir.irLambdaBinding("x", numType, funcBody, funcType),
          },
        ],
        ir.irLet(
          "result",
          ir.irAppBinding(ir.irVar("f", funcType), ir.irLit(10, numType), numType),
          ir.irAtomExpr(ir.irVar("result", numType)),
        ),
      );

      const result = tailCallOptimization.run(expr);

      // Should mark the function as tail-recursive
      expect(result.kind).toBe("IRLetRec");
      if (result.kind === "IRLetRec") {
        const binding = result.bindings[0]!.binding;
        expect(binding.kind).toBe("IRLambdaBinding");
        if (binding.kind === "IRLambdaBinding") {
          expect(binding.tailRecursive).toBeDefined();
          expect(binding.tailRecursive?.selfName).toBe("f");
          expect(binding.tailRecursive?.params).toEqual(["x"]);
        }
      }
    });

    it("does not mark non-tail-recursive function", () => {
      // let rec f x = if x <= 0 then 0 else x + f (x - 1) in f 10
      // The recursive call is NOT in tail position (it's an argument to +)
      const funcType = { kind: "TFun" as const, param: numType, ret: numType };

      // Non-tail call: x + f (x - 1)
      // The f call is used as an argument to +, not returned directly
      const recursiveCall = ir.irLet(
        "_t1",
        ir.irBinOpBinding("-", ir.irVar("x", numType), ir.irLit(1, numType), numType, numType),
        ir.irLet(
          "_t2",
          ir.irAppBinding(ir.irVar("f", funcType), ir.irVar("_t1", numType), numType),
          ir.irLet(
            "_t3",
            ir.irBinOpBinding(
              "+",
              ir.irVar("x", numType),
              ir.irVar("_t2", numType),
              numType,
              numType,
            ),
            ir.irAtomExpr(ir.irVar("_t3", numType)),
          ),
        ),
      );

      const ifBinding = ir.irIfBinding(
        ir.irVar("_cond", boolType),
        ir.irAtomExpr(ir.irLit(0, numType)),
        recursiveCall,
        numType,
      );

      const funcBody = ir.irLet(
        "_cond",
        ir.irBinOpBinding("<=", ir.irVar("x", numType), ir.irLit(0, numType), numType, boolType),
        ir.irLet("_result", ifBinding, ir.irAtomExpr(ir.irVar("_result", numType))),
      );

      const expr = ir.irLetRec(
        [
          {
            name: "f",
            binding: ir.irLambdaBinding("x", numType, funcBody, funcType),
          },
        ],
        ir.irLet(
          "result",
          ir.irAppBinding(ir.irVar("f", funcType), ir.irLit(10, numType), numType),
          ir.irAtomExpr(ir.irVar("result", numType)),
        ),
      );

      const result = tailCallOptimization.run(expr);

      // Should NOT mark as tail-recursive
      expect(result.kind).toBe("IRLetRec");
      if (result.kind === "IRLetRec") {
        const binding = result.bindings[0]!.binding;
        expect(binding.kind).toBe("IRLambdaBinding");
        if (binding.kind === "IRLambdaBinding") {
          expect(binding.tailRecursive).toBeUndefined();
        }
      }
    });
  });
});
