/**
 * Optimization Passes for the IR.
 *
 * These passes transform the IR to improve performance:
 * - Constant Folding: evaluate literal expressions at compile time
 * - Dead Code Elimination: remove unused bindings
 *
 * All passes preserve semantics - the optimized program produces
 * the same result as the original.
 */

import type { Op } from "./ast";
import * as ir from "./ir";

// =============================================================================
// OPTIMIZATION PASS INTERFACE
// =============================================================================

/**
 * An optimization pass transforms IR expressions.
 */
export type OptPass = {
  readonly name: string;
  run(expr: ir.IRExpr): ir.IRExpr;
};

// =============================================================================
// CONSTANT FOLDING
// =============================================================================

/**
 * Evaluate a binary operation on literal values at compile time.
 * Returns undefined if the operation cannot be folded.
 */
const evalBinOp = (
  op: Op,
  left: number | string | boolean,
  right: number | string | boolean,
): number | string | boolean | undefined => {
  // Arithmetic operations (numbers only)
  if (typeof left === "number" && typeof right === "number") {
    switch (op) {
      case "+":
        return left + right;
      case "-":
        return left - right;
      case "*":
        return left * right;
      case "/":
        return right !== 0 ? left / right : undefined; // Avoid division by zero
      case "<":
        return left < right;
      case ">":
        return left > right;
      case "<=":
        return left <= right;
      case ">=":
        return left >= right;
      case "==":
        return left === right;
      case "!=":
        return left !== right;
    }
  }

  // String concatenation
  if (typeof left === "string" && typeof right === "string") {
    switch (op) {
      case "++":
        return left + right;
      case "==":
        return left === right;
      case "!=":
        return left !== right;
      case "<":
        return left < right;
      case ">":
        return left > right;
      case "<=":
        return left <= right;
      case ">=":
        return left >= right;
    }
  }

  // Boolean equality
  if (typeof left === "boolean" && typeof right === "boolean") {
    switch (op) {
      case "==":
        return left === right;
      case "!=":
        return left !== right;
    }
  }

  return undefined;
};

/**
 * Transform bindings in an expression, returning a new expression.
 * Used by constant folding to replace operations with their results.
 */
const transformBindings = (
  expr: ir.IRExpr,
  transform: (binding: ir.IRBinding) => ir.IRBinding,
): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr;

    case "IRLet": {
      const newBinding = transform(transformBinding(expr.binding, transform));
      const newBody = transformBindings(expr.body, transform);
      return ir.irLet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: transform(transformBinding(b.binding, transform)),
      }));
      const newBody = transformBindings(expr.body, transform);
      return ir.irLetRec(newBindings, newBody);
    }
  }
};

/**
 * Transform nested expressions within a binding.
 */
const transformBinding = (
  binding: ir.IRBinding,
  transform: (binding: ir.IRBinding) => ir.IRBinding,
): ir.IRBinding => {
  switch (binding.kind) {
    case "IRAtomBinding":
    case "IRAppBinding":
    case "IRBinOpBinding":
    case "IRTupleBinding":
    case "IRRecordBinding":
    case "IRFieldAccessBinding":
    case "IRTupleIndexBinding":
    case "IRClosureBinding":
      return binding;

    case "IRIfBinding":
      return ir.irIfBinding(
        binding.cond,
        transformBindings(binding.thenBranch, transform),
        transformBindings(binding.elseBranch, transform),
        binding.type,
      );

    case "IRMatchBinding":
      return ir.irMatchBinding(
        binding.scrutinee,
        binding.cases.map((c) => ({
          pattern: c.pattern,
          guard: c.guard ? transformBindings(c.guard, transform) : undefined,
          body: transformBindings(c.body, transform),
        })),
        binding.type,
      );

    case "IRLambdaBinding":
      return ir.irLambdaBinding(
        binding.param,
        binding.paramType,
        transformBindings(binding.body, transform),
        binding.type,
      );
  }
};

/**
 * Constant Folding Pass
 *
 * Evaluates binary operations on literals at compile time.
 * For example: `1 + 2` becomes `3`
 */
export const constantFolding: OptPass = {
  name: "constant-folding",
  run(expr) {
    return transformBindings(expr, (binding) => {
      if (binding.kind === "IRBinOpBinding") {
        const { op, left, right, type } = binding;

        // Both operands must be literals
        if (left.kind === "IRLit" && right.kind === "IRLit") {
          const result = evalBinOp(op, left.value, right.value);
          if (result !== undefined) {
            return ir.irAtomBinding(ir.irLit(result, type));
          }
        }
      }

      // Also fold if conditions with literal conditions
      if (binding.kind === "IRIfBinding") {
        const { cond } = binding;
        if (cond.kind === "IRLit" && typeof cond.value === "boolean") {
          // Replace if with the appropriate branch, but we need to return
          // the branch as a binding. Since branches are expressions, we need
          // to handle this at the expression level instead.
          // For now, just return the binding unchanged.
          // The actual folding happens at the expression level.
        }
      }

      return binding;
    });
  },
};

// =============================================================================
// DEAD CODE ELIMINATION
// =============================================================================

/**
 * Collect all variable references in an expression.
 */
const collectUses = (expr: ir.IRExpr, uses: Map<string, number>): void => {
  switch (expr.kind) {
    case "IRAtomExpr":
      if (expr.atom.kind === "IRVar") {
        uses.set(expr.atom.name, (uses.get(expr.atom.name) ?? 0) + 1);
      }
      break;

    case "IRLet":
      collectUsesBinding(expr.binding, uses);
      collectUses(expr.body, uses);
      break;

    case "IRLetRec":
      for (const b of expr.bindings) {
        collectUsesBinding(b.binding, uses);
      }
      collectUses(expr.body, uses);
      break;
  }
};

/**
 * Collect variable references in a binding.
 */
const collectUsesBinding = (binding: ir.IRBinding, uses: Map<string, number>): void => {
  const addAtom = (atom: ir.IRAtom) => {
    if (atom.kind === "IRVar") {
      uses.set(atom.name, (uses.get(atom.name) ?? 0) + 1);
    }
  };

  switch (binding.kind) {
    case "IRAtomBinding":
      addAtom(binding.atom);
      break;

    case "IRAppBinding":
      addAtom(binding.func);
      addAtom(binding.arg);
      break;

    case "IRBinOpBinding":
      addAtom(binding.left);
      addAtom(binding.right);
      break;

    case "IRIfBinding":
      addAtom(binding.cond);
      collectUses(binding.thenBranch, uses);
      collectUses(binding.elseBranch, uses);
      break;

    case "IRTupleBinding":
      for (const elem of binding.elements) {
        addAtom(elem);
      }
      break;

    case "IRRecordBinding":
      for (const field of binding.fields) {
        addAtom(field.value);
      }
      break;

    case "IRFieldAccessBinding":
      addAtom(binding.record);
      break;

    case "IRTupleIndexBinding":
      addAtom(binding.tuple);
      break;

    case "IRMatchBinding":
      addAtom(binding.scrutinee);
      for (const c of binding.cases) {
        if (c.guard) collectUses(c.guard, uses);
        collectUses(c.body, uses);
      }
      break;

    case "IRLambdaBinding":
      collectUses(binding.body, uses);
      break;

    case "IRClosureBinding":
      for (const cap of binding.captures) {
        addAtom(cap);
      }
      break;
  }
};

/**
 * Check if a binding is pure (has no side effects).
 * In our language, everything is pure except for potential non-termination.
 */
const isPure = (_binding: ir.IRBinding): boolean => {
  // All our bindings are pure
  return true;
};

/**
 * Dead Code Elimination Pass
 *
 * Removes bindings that are never used.
 * Only removes pure bindings (those without side effects).
 */
export const deadCodeElimination: OptPass = {
  name: "dead-code-elimination",
  run(expr) {
    // Collect all variable uses
    const uses = new Map<string, number>();
    collectUses(expr, uses);

    // Remove unused bindings
    return removeUnused(expr, uses);
  },
};

/**
 * Remove unused bindings from an expression.
 */
const removeUnused = (expr: ir.IRExpr, uses: Map<string, number>): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr;

    case "IRLet": {
      const newBody = removeUnused(expr.body, uses);

      // Check if this binding is used
      const usageCount = uses.get(expr.name) ?? 0;

      // If unused and pure, skip this let and return the body
      if (usageCount === 0 && isPure(expr.binding)) {
        return newBody;
      }

      // Otherwise, keep the binding (with transformed body)
      const newBinding = removeUnusedBinding(expr.binding, uses);
      return ir.irLet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBody = removeUnused(expr.body, uses);

      // Filter out unused bindings
      const usedBindings = expr.bindings.filter((b) => {
        const usageCount = uses.get(b.name) ?? 0;
        return usageCount > 0 || !isPure(b.binding);
      });

      // If all bindings removed, return body
      if (usedBindings.length === 0) {
        return newBody;
      }

      const newBindings = usedBindings.map((b) => ({
        name: b.name,
        binding: removeUnusedBinding(b.binding, uses),
      }));

      return ir.irLetRec(newBindings, newBody);
    }
  }
};

/**
 * Remove unused bindings from nested expressions within a binding.
 */
const removeUnusedBinding = (binding: ir.IRBinding, uses: Map<string, number>): ir.IRBinding => {
  switch (binding.kind) {
    case "IRAtomBinding":
    case "IRAppBinding":
    case "IRBinOpBinding":
    case "IRTupleBinding":
    case "IRRecordBinding":
    case "IRFieldAccessBinding":
    case "IRTupleIndexBinding":
    case "IRClosureBinding":
      return binding;

    case "IRIfBinding":
      return ir.irIfBinding(
        binding.cond,
        removeUnused(binding.thenBranch, uses),
        removeUnused(binding.elseBranch, uses),
        binding.type,
      );

    case "IRMatchBinding":
      return ir.irMatchBinding(
        binding.scrutinee,
        binding.cases.map((c) => ({
          pattern: c.pattern,
          guard: c.guard ? removeUnused(c.guard, uses) : undefined,
          body: removeUnused(c.body, uses),
        })),
        binding.type,
      );

    case "IRLambdaBinding":
      return ir.irLambdaBinding(
        binding.param,
        binding.paramType,
        removeUnused(binding.body, uses),
        binding.type,
      );
  }
};

// =============================================================================
// OPTIMIZATION PIPELINE
// =============================================================================

/**
 * Default optimization passes in order of application.
 */
const defaultPasses: readonly OptPass[] = [constantFolding, deadCodeElimination];

/**
 * Run all optimization passes on an IR expression.
 *
 * @param expr The IR expression to optimize
 * @param passes Optional custom passes (defaults to standard optimizations)
 * @returns The optimized IR expression
 */
export const optimize = (
  expr: ir.IRExpr,
  passes: readonly OptPass[] = defaultPasses,
): ir.IRExpr => {
  let result = expr;
  for (const pass of passes) {
    result = pass.run(result);
  }
  return result;
};
