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

  // String operations
  if (typeof left === "string" && typeof right === "string") {
    switch (op) {
      case "+":
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
// TAIL-CALL OPTIMIZATION
// =============================================================================

/**
 * Collect all parameters from nested lambdas.
 * `a -> b -> c -> body` returns ["a", "b", "c"]
 */
const collectLambdaParams = (binding: ir.IRLambdaBinding): string[] => {
  const params: string[] = [binding.param];
  let body = binding.body;

  // Unwrap nested lambdas bound to fresh names
  while (body.kind === "IRLet" && body.binding.kind === "IRLambdaBinding") {
    params.push(body.binding.param);
    body = body.binding.body;
  }

  return params;
};

/**
 * Get the innermost body of nested lambdas.
 */
const getInnermostBody = (binding: ir.IRLambdaBinding): ir.IRExpr => {
  let body = binding.body;

  while (body.kind === "IRLet" && body.binding.kind === "IRLambdaBinding") {
    body = body.binding.body;
  }

  return body;
};

/**
 * Check if an expression is in tail position and only contains
 * tail calls to the specified function name.
 *
 * Returns true if all recursive calls are in tail position.
 * Returns false if there are non-tail recursive calls.
 * Returns null if there are no recursive calls.
 */
const checkTailCalls = (expr: ir.IRExpr, funcName: string, paramCount: number): boolean | null => {
  switch (expr.kind) {
    case "IRAtomExpr":
      // No recursive call in atoms
      return null;

    case "IRLet": {
      // Check if this is a tail call: let result = funcName arg1 arg2 ... in result
      const callInfo = extractTailCall(expr, funcName, paramCount);
      if (callInfo) {
        return true; // This is a valid tail call
      }

      // Check if binding contains non-tail recursive calls
      if (hasRecursiveCall(expr.binding, funcName)) {
        return false; // Recursive call not in tail position
      }

      // Check the body
      return checkTailCalls(expr.body, funcName, paramCount);
    }

    case "IRLetRec":
      // Nested letrec - check body, but bindings shouldn't have our function
      return checkTailCalls(expr.body, funcName, paramCount);
  }
};

/**
 * Extract tail call information if expr ends with a tail call to funcName.
 * Handles interleaved argument computations like:
 *   let _t1 = n - 1 in
 *   let _t2 = f _t1 in
 *   let _t3 = n * acc in
 *   let _t4 = _t2 _t3 in
 *   _t4
 */
const extractTailCall = (
  expr: ir.IRExpr,
  funcName: string,
  paramCount: number,
): ir.IRAtom[] | null => {
  // Collect all let bindings first
  const bindings: Array<{ name: string; binding: ir.IRBinding }> = [];
  let current: ir.IRExpr = expr;

  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  // Final expression should be a variable
  if (current.kind !== "IRAtomExpr" || current.atom.kind !== "IRVar") {
    return null;
  }

  const resultVar = current.atom.name;

  // Work backwards from the result to find the application chain
  const args: ir.IRAtom[] = [];
  let targetVar = resultVar;

  for (let i = bindings.length - 1; i >= 0; i--) {
    const { name, binding } = bindings[i]!;

    if (name === targetVar && binding.kind === "IRAppBinding") {
      // This is part of the application chain
      args.unshift(binding.arg);

      // Check what the function is
      if (binding.func.kind === "IRVar") {
        if (binding.func.name === funcName) {
          // Found the start of the tail call!
          if (args.length === paramCount) {
            return args;
          }
          return null; // Wrong arity
        }
        // Continue searching - this is a partial application
        targetVar = binding.func.name;
      } else {
        return null; // Function is not a variable
      }
    }
    // Skip non-matching bindings (argument computations)
  }

  return null;
};

/**
 * Check if a binding contains any call to funcName (non-tail position).
 */
const hasRecursiveCall = (binding: ir.IRBinding, funcName: string): boolean => {
  const checkAtom = (atom: ir.IRAtom): boolean => atom.kind === "IRVar" && atom.name === funcName;

  switch (binding.kind) {
    case "IRAtomBinding":
      return checkAtom(binding.atom);

    case "IRAppBinding":
      return checkAtom(binding.func) || checkAtom(binding.arg);

    case "IRBinOpBinding":
      return checkAtom(binding.left) || checkAtom(binding.right);

    case "IRIfBinding": {
      if (checkAtom(binding.cond)) return true;
      // Branches are in tail position, so we need to check them differently
      const thenResult = checkTailCalls(binding.thenBranch, funcName, 0);
      const elseResult = checkTailCalls(binding.elseBranch, funcName, 0);
      // If either branch has non-tail calls, return true
      return thenResult === false || elseResult === false;
    }

    case "IRTupleBinding":
      return binding.elements.some(checkAtom);

    case "IRRecordBinding":
      return binding.fields.some((f) => checkAtom(f.value));

    case "IRFieldAccessBinding":
      return checkAtom(binding.record);

    case "IRTupleIndexBinding":
      return checkAtom(binding.tuple);

    case "IRMatchBinding": {
      if (checkAtom(binding.scrutinee)) return true;
      // Case bodies are in tail position
      for (const c of binding.cases) {
        if (c.guard && hasRecursiveCallExpr(c.guard, funcName)) return true;
        const bodyResult = checkTailCalls(c.body, funcName, 0);
        if (bodyResult === false) return true;
      }
      return false;
    }

    case "IRLambdaBinding":
      // Lambdas capture funcName but don't call it in this context
      return false;

    case "IRClosureBinding":
      return binding.captures.some(checkAtom);
  }
};

/**
 * Check if an expression contains any call to funcName.
 */
const hasRecursiveCallExpr = (expr: ir.IRExpr, funcName: string): boolean => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr.atom.kind === "IRVar" && expr.atom.name === funcName;

    case "IRLet":
      return hasRecursiveCall(expr.binding, funcName) || hasRecursiveCallExpr(expr.body, funcName);

    case "IRLetRec":
      return (
        expr.bindings.some((b) => hasRecursiveCall(b.binding, funcName)) ||
        hasRecursiveCallExpr(expr.body, funcName)
      );
  }
};

/**
 * Check if a binding in a letrec is tail-recursive.
 */
const isTailRecursive = (name: string, binding: ir.IRBinding): { params: string[] } | null => {
  if (binding.kind !== "IRLambdaBinding") {
    return null;
  }

  const params = collectLambdaParams(binding);
  const innerBody = getInnermostBody(binding);

  // Check if the body only has tail calls
  const result = checkTailCallsInBody(innerBody, name, params.length);

  if (result) {
    return { params };
  }

  return null;
};

/**
 * Check the body of a function for tail-recursive calls.
 * Handles if/match expressions where branches are in tail position.
 */
const checkTailCallsInBody = (expr: ir.IRExpr, funcName: string, paramCount: number): boolean => {
  switch (expr.kind) {
    case "IRAtomExpr":
      // Base case - no recursive call, that's fine
      return true;

    case "IRLet": {
      // Check if this is a tail call
      const callInfo = extractTailCall(expr, funcName, paramCount);
      if (callInfo) {
        return true;
      }

      // Check binding for non-tail recursive calls
      const bindingCheck = checkBindingForTCO(expr.binding, funcName, paramCount);
      if (bindingCheck === false) {
        return false;
      }
      if (bindingCheck === true) {
        return true; // Binding itself is tail-recursive (if/match)
      }

      // Continue checking body
      return checkTailCallsInBody(expr.body, funcName, paramCount);
    }

    case "IRLetRec":
      return checkTailCallsInBody(expr.body, funcName, paramCount);
  }
};

/**
 * Check a binding for TCO eligibility.
 * Returns true if binding contains only tail calls.
 * Returns false if binding contains non-tail calls.
 * Returns null if binding doesn't involve funcName.
 */
const checkBindingForTCO = (
  binding: ir.IRBinding,
  funcName: string,
  paramCount: number,
): boolean | null => {
  switch (binding.kind) {
    case "IRIfBinding": {
      // Both branches must be tail-recursive
      const thenOk = checkTailCallsInBody(binding.thenBranch, funcName, paramCount);
      const elseOk = checkTailCallsInBody(binding.elseBranch, funcName, paramCount);
      if (!thenOk || !elseOk) return false;
      return true;
    }

    case "IRMatchBinding": {
      // All case bodies must be tail-recursive
      for (const c of binding.cases) {
        if (c.guard && hasRecursiveCallExpr(c.guard, funcName)) {
          return false;
        }
        if (!checkTailCallsInBody(c.body, funcName, paramCount)) {
          return false;
        }
      }
      return true;
    }

    case "IRAppBinding":
      // Non-tail application - check if it involves funcName
      if (binding.func.kind === "IRVar" && binding.func.name === funcName) {
        return false; // Non-tail call to self
      }
      return null;

    default:
      // Check if binding references funcName
      if (hasRecursiveCall(binding, funcName)) {
        return false;
      }
      return null;
  }
};

/**
 * Mark a lambda as tail-recursive by adding the marker.
 */
const markTailRecursive = (
  binding: ir.IRLambdaBinding,
  selfName: string,
  params: readonly string[],
): ir.IRLambdaBinding => ({
  ...binding,
  tailRecursive: { selfName, params },
});

/**
 * Tail-Call Optimization Pass
 *
 * Detects tail-recursive functions in letrec bindings and marks them
 * for loop-based code generation in the backend.
 */
export const tailCallOptimization: OptPass = {
  name: "tail-call-optimization",
  run(expr) {
    return transformTCO(expr);
  },
};

/**
 * Transform an expression, marking tail-recursive functions.
 */
const transformTCO = (expr: ir.IRExpr): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr;

    case "IRLet": {
      const newBinding = transformBindingTCO(expr.binding);
      const newBody = transformTCO(expr.body);
      return ir.irLet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      // Check each binding for tail recursion
      const newBindings = expr.bindings.map((b) => {
        const tcoInfo = isTailRecursive(b.name, b.binding);
        if (tcoInfo && b.binding.kind === "IRLambdaBinding") {
          const marked = markTailRecursive(b.binding, b.name, tcoInfo.params);
          return { name: b.name, binding: transformBindingTCO(marked) };
        }
        return { name: b.name, binding: transformBindingTCO(b.binding) };
      });
      const newBody = transformTCO(expr.body);
      return ir.irLetRec(newBindings, newBody);
    }
  }
};

/**
 * Transform bindings, recursing into nested expressions.
 */
const transformBindingTCO = (binding: ir.IRBinding): ir.IRBinding => {
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
        transformTCO(binding.thenBranch),
        transformTCO(binding.elseBranch),
        binding.type,
      );

    case "IRMatchBinding":
      return ir.irMatchBinding(
        binding.scrutinee,
        binding.cases.map((c) => ({
          pattern: c.pattern,
          guard: c.guard ? transformTCO(c.guard) : undefined,
          body: transformTCO(c.body),
        })),
        binding.type,
      );

    case "IRLambdaBinding":
      return ir.irLambdaBinding(
        binding.param,
        binding.paramType,
        transformTCO(binding.body),
        binding.type,
        binding.tailRecursive,
      );
  }
};

// =============================================================================
// OPTIMIZATION PIPELINE
// =============================================================================

/**
 * Default optimization passes in order of application.
 */
const defaultPasses: readonly OptPass[] = [
  constantFolding,
  deadCodeElimination,
  tailCallOptimization,
];

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
