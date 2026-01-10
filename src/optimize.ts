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
// CONSTANT FOLDING WITH PROPAGATION
// =============================================================================

/**
 * A constant value that can be tracked and propagated.
 */
type ConstValue = number | string | boolean;

/**
 * Environment mapping variable names to their known constant values.
 */
type ConstEnv = Map<string, ConstValue>;

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
 * Get the constant value of an atom, if known.
 * Looks up variables in the environment.
 */
const atomToConst = (atom: ir.IRAtom, env: ConstEnv): ConstValue | undefined => {
  if (atom.kind === "IRLit") {
    return atom.value;
  }
  if (atom.kind === "IRVar") {
    return env.get(atom.name);
  }
  return undefined;
};

/**
 * Replace an atom with a literal if its value is known.
 */
const substituteAtom = (atom: ir.IRAtom, env: ConstEnv): ir.IRAtom => {
  const value = atomToConst(atom, env);
  if (value !== undefined && atom.kind === "IRVar") {
    return ir.irLit(value, atom.type);
  }
  return atom;
};

/**
 * Extract the structure of an expression: a sequence of let bindings
 * followed by a final atom. Returns null if the structure is complex
 * (e.g., contains letrec).
 */
type ExprStructure = {
  readonly lets: ReadonlyArray<{ name: string; binding: ir.IRBinding }>;
  readonly result: ir.IRAtom;
};

const extractStructure = (expr: ir.IRExpr): ExprStructure | null => {
  const lets: Array<{ name: string; binding: ir.IRBinding }> = [];
  let current = expr;

  while (current.kind === "IRLet") {
    lets.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  if (current.kind === "IRAtomExpr") {
    return { lets, result: current.atom };
  }

  // IRLetRec - complex, don't extract
  return null;
};

/**
 * Prepend let bindings to an expression.
 */
const prependLets = (
  lets: ReadonlyArray<{ name: string; binding: ir.IRBinding }>,
  body: ir.IRExpr,
): ir.IRExpr => {
  let result = body;
  for (let i = lets.length - 1; i >= 0; i--) {
    const { name, binding } = lets[i]!;
    result = ir.irLet(name, binding, result);
  }
  return result;
};

/**
 * Fold constants in an expression with propagation.
 * Tracks known constant values and substitutes them.
 */
const foldExpr = (expr: ir.IRExpr, env: ConstEnv): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr": {
      // Substitute variable with constant if known
      const newAtom = substituteAtom(expr.atom, env);
      if (newAtom !== expr.atom) {
        return ir.irAtomExpr(newAtom);
      }
      return expr;
    }

    case "IRLet": {
      // First fold the binding
      const foldedBinding = foldBinding(expr.binding, env);

      // Check if the result is a constant atom
      if (foldedBinding.kind === "IRAtomBinding" && foldedBinding.atom.kind === "IRLit") {
        // Add to environment for propagation
        const newEnv = new Map(env);
        newEnv.set(expr.name, foldedBinding.atom.value);
        const foldedBody = foldExpr(expr.body, newEnv);
        return ir.irLet(expr.name, foldedBinding, foldedBody);
      }

      // Not a constant - continue without adding to env
      const foldedBody = foldExpr(expr.body, env);
      return ir.irLet(expr.name, foldedBinding, foldedBody);
    }

    case "IRLetRec": {
      // Recursive bindings are functions, not constants
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: foldBinding(b.binding, env),
      }));
      const foldedBody = foldExpr(expr.body, env);
      return ir.irLetRec(newBindings, foldedBody);
    }
  }
};

/**
 * Fold constants in a binding.
 */
const foldBinding = (binding: ir.IRBinding, env: ConstEnv): ir.IRBinding => {
  switch (binding.kind) {
    case "IRAtomBinding": {
      const newAtom = substituteAtom(binding.atom, env);
      if (newAtom !== binding.atom) {
        return ir.irAtomBinding(newAtom);
      }
      return binding;
    }

    case "IRBinOpBinding": {
      // Substitute both operands
      const left = substituteAtom(binding.left, env);
      const right = substituteAtom(binding.right, env);

      // Try to fold if both are constants
      const leftVal = atomToConst(left, env);
      const rightVal = atomToConst(right, env);

      if (leftVal !== undefined && rightVal !== undefined) {
        const result = evalBinOp(binding.op, leftVal, rightVal);
        if (result !== undefined) {
          return ir.irAtomBinding(ir.irLit(result, binding.type));
        }
      }

      // Return with substituted atoms
      if (left !== binding.left || right !== binding.right) {
        return ir.irBinOpBinding(binding.op, left, right, binding.operandType, binding.type);
      }
      return binding;
    }

    case "IRAppBinding": {
      const func = substituteAtom(binding.func, env);
      const arg = substituteAtom(binding.arg, env);
      if (func !== binding.func || arg !== binding.arg) {
        return ir.irAppBinding(func, arg, binding.type);
      }
      return binding;
    }

    case "IRIfBinding": {
      const cond = substituteAtom(binding.cond, env);
      const condVal = atomToConst(cond, env);

      // If condition is a known boolean, fold to the appropriate branch
      if (typeof condVal === "boolean") {
        const chosenBranch = condVal ? binding.thenBranch : binding.elseBranch;
        const foldedBranch = foldExpr(chosenBranch, env);

        // Extract the structure of the folded branch
        const structure = extractStructure(foldedBranch);
        if (structure) {
          // If the branch is just an atom, return it as a binding
          if (structure.lets.length === 0) {
            return ir.irAtomBinding(structure.result);
          }
          // Otherwise, we need to inline the lets - this is handled at the expression level
          // For now, return the if with the folded branches (dead code elimination will help)
        }

        // Can't inline directly, return folded branches
        return ir.irIfBinding(
          cond,
          foldExpr(binding.thenBranch, env),
          foldExpr(binding.elseBranch, env),
          binding.type,
        );
      }

      // Condition not constant - fold both branches
      return ir.irIfBinding(
        cond,
        foldExpr(binding.thenBranch, env),
        foldExpr(binding.elseBranch, env),
        binding.type,
      );
    }

    case "IRTupleBinding": {
      const elements = binding.elements.map((e) => substituteAtom(e, env));
      const changed = elements.some((e, i) => e !== binding.elements[i]);
      if (changed) {
        return ir.irTupleBinding(elements, binding.type);
      }
      return binding;
    }

    case "IRRecordBinding": {
      const fields = binding.fields.map((f) => ({
        name: f.name,
        value: substituteAtom(f.value, env),
      }));
      const changed = fields.some((f, i) => f.value !== binding.fields[i]?.value);
      if (changed) {
        return ir.irRecordBinding(fields, binding.type);
      }
      return binding;
    }

    case "IRFieldAccessBinding": {
      const record = substituteAtom(binding.record, env);
      if (record !== binding.record) {
        return ir.irFieldAccessBinding(record, binding.field, binding.type);
      }
      return binding;
    }

    case "IRTupleIndexBinding": {
      const tuple = substituteAtom(binding.tuple, env);
      if (tuple !== binding.tuple) {
        return ir.irTupleIndexBinding(tuple, binding.index, binding.type);
      }
      return binding;
    }

    case "IRMatchBinding": {
      const scrutinee = substituteAtom(binding.scrutinee, env);
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? foldExpr(c.guard, env) : undefined,
        body: foldExpr(c.body, env),
      }));
      return ir.irMatchBinding(scrutinee, cases, binding.type);
    }

    case "IRLambdaBinding": {
      // Don't propagate constants into lambda bodies - they have their own scope
      // But we still fold within the body
      return ir.irLambdaBinding(
        binding.param,
        binding.paramType,
        foldExpr(binding.body, new Map()),
        binding.type,
        binding.tailRecursive,
      );
    }

    case "IRClosureBinding": {
      const captures = binding.captures.map((c) => substituteAtom(c, env));
      const changed = captures.some((c, i) => c !== binding.captures[i]);
      if (changed) {
        return ir.irClosureBinding(binding.funcId, captures, binding.type);
      }
      return binding;
    }

    case "IRRecordUpdateBinding": {
      const base = substituteAtom(binding.base, env);
      const fields = binding.fields.map((f) => ({
        name: f.name,
        value: substituteAtom(f.value, env),
      }));
      const changed =
        base !== binding.base || fields.some((f, i) => f.value !== binding.fields[i]?.value);
      if (changed) {
        return ir.irRecordUpdateBinding(base, fields, binding.type);
      }
      return binding;
    }
  }
};

/**
 * Second pass: inline if-expressions with constant conditions.
 * This is done after the main fold pass to handle cases where
 * the branch needs to be inlined into the surrounding context.
 */
const inlineConstantIfs = (expr: ir.IRExpr): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr;

    case "IRLet": {
      const newBinding = inlineConstantIfsBinding(expr.binding);

      // Check if this is an if with constant condition
      if (newBinding.kind === "IRIfBinding" && newBinding.cond.kind === "IRLit") {
        const condVal = newBinding.cond.value;
        if (typeof condVal === "boolean") {
          const chosenBranch = condVal ? newBinding.thenBranch : newBinding.elseBranch;
          const structure = extractStructure(chosenBranch);

          if (structure) {
            // Inline the lets from the branch, then bind the result
            const foldedLets = structure.lets.map((l) => ({
              name: l.name,
              binding: inlineConstantIfsBinding(l.binding),
            }));
            const resultBinding = ir.irAtomBinding(structure.result);
            const newBody = inlineConstantIfs(expr.body);

            // Build: foldedLets... then let expr.name = result in newBody
            const innerLet = ir.irLet(expr.name, resultBinding, newBody);
            return prependLets(foldedLets, innerLet);
          }
        }
      }

      const newBody = inlineConstantIfs(expr.body);
      return ir.irLet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: inlineConstantIfsBinding(b.binding),
      }));
      const newBody = inlineConstantIfs(expr.body);
      return ir.irLetRec(newBindings, newBody);
    }
  }
};

const inlineConstantIfsBinding = (binding: ir.IRBinding): ir.IRBinding => {
  switch (binding.kind) {
    case "IRAtomBinding":
    case "IRAppBinding":
    case "IRBinOpBinding":
    case "IRTupleBinding":
    case "IRRecordBinding":
    case "IRRecordUpdateBinding":
    case "IRFieldAccessBinding":
    case "IRTupleIndexBinding":
    case "IRClosureBinding":
      return binding;

    case "IRIfBinding":
      return ir.irIfBinding(
        binding.cond,
        inlineConstantIfs(binding.thenBranch),
        inlineConstantIfs(binding.elseBranch),
        binding.type,
      );

    case "IRMatchBinding":
      return ir.irMatchBinding(
        binding.scrutinee,
        binding.cases.map((c) => ({
          pattern: c.pattern,
          guard: c.guard ? inlineConstantIfs(c.guard) : undefined,
          body: inlineConstantIfs(c.body),
        })),
        binding.type,
      );

    case "IRLambdaBinding":
      return ir.irLambdaBinding(
        binding.param,
        binding.paramType,
        inlineConstantIfs(binding.body),
        binding.type,
        binding.tailRecursive,
      );
  }
};

/**
 * Constant Folding Pass
 *
 * Evaluates constant expressions at compile time and propagates
 * known constant values through the program.
 *
 * Features:
 * - Folds binary operations: `1 + 2` → `3`
 * - Propagates constants: `let x = 3 in x + 2` → `let x = 3 in 5`
 * - Folds conditionals: `if true then a else b` → `a`
 * - Inlines folded branches when possible
 */
export const constantFolding: OptPass = {
  name: "constant-folding",
  run(expr) {
    // First pass: fold and propagate constants
    const folded = foldExpr(expr, new Map());
    // Second pass: inline constant if-expressions
    return inlineConstantIfs(folded);
  },
};

// =============================================================================
// COPY PROPAGATION AND ANF SIMPLIFICATION
// =============================================================================

/**
 * Environment mapping variable names to their replacement atoms.
 * Used for copy propagation.
 */
type CopyEnv = Map<string, ir.IRAtom>;

/**
 * Replace an atom using the copy environment.
 */
const copyAtom = (atom: ir.IRAtom, env: CopyEnv): ir.IRAtom => {
  if (atom.kind === "IRVar") {
    const replacement = env.get(atom.name);
    if (replacement) {
      // Recursively resolve chains: x -> y -> z
      return copyAtom(replacement, env);
    }
  }
  return atom;
};

/**
 * Simplify an expression by propagating copies and eliminating trivial lets.
 */
const simplifyExpr = (expr: ir.IRExpr, env: CopyEnv): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr": {
      const newAtom = copyAtom(expr.atom, env);
      if (newAtom !== expr.atom) {
        return ir.irAtomExpr(newAtom);
      }
      return expr;
    }

    case "IRLet": {
      const simplifiedBinding = simplifyBinding(expr.binding, env);

      // Check for trivial binding: let x = y in body
      // If binding is just an atom, we can propagate it
      if (simplifiedBinding.kind === "IRAtomBinding") {
        const newEnv = new Map(env);
        newEnv.set(expr.name, simplifiedBinding.atom);
        const simplifiedBody = simplifyExpr(expr.body, newEnv);

        // Check if body is just the variable we bound: let x = y in x => y
        if (
          simplifiedBody.kind === "IRAtomExpr" &&
          simplifiedBody.atom.kind === "IRVar" &&
          simplifiedBody.atom.name === expr.name
        ) {
          return ir.irAtomExpr(simplifiedBinding.atom);
        }

        // Keep the let but with simplified body (DCE will remove if unused)
        return ir.irLet(expr.name, simplifiedBinding, simplifiedBody);
      }

      // Non-trivial binding - just simplify the body
      const simplifiedBody = simplifyExpr(expr.body, env);
      return ir.irLet(expr.name, simplifiedBinding, simplifiedBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: simplifyBinding(b.binding, env),
      }));
      const simplifiedBody = simplifyExpr(expr.body, env);
      return ir.irLetRec(newBindings, simplifiedBody);
    }
  }
};

/**
 * Simplify atoms within a binding.
 */
const simplifyBinding = (binding: ir.IRBinding, env: CopyEnv): ir.IRBinding => {
  switch (binding.kind) {
    case "IRAtomBinding": {
      const newAtom = copyAtom(binding.atom, env);
      if (newAtom !== binding.atom) {
        return ir.irAtomBinding(newAtom);
      }
      return binding;
    }

    case "IRAppBinding": {
      const func = copyAtom(binding.func, env);
      const arg = copyAtom(binding.arg, env);
      if (func !== binding.func || arg !== binding.arg) {
        return ir.irAppBinding(func, arg, binding.type);
      }
      return binding;
    }

    case "IRBinOpBinding": {
      const left = copyAtom(binding.left, env);
      const right = copyAtom(binding.right, env);
      if (left !== binding.left || right !== binding.right) {
        return ir.irBinOpBinding(binding.op, left, right, binding.operandType, binding.type);
      }
      return binding;
    }

    case "IRIfBinding": {
      const cond = copyAtom(binding.cond, env);
      const thenBranch = simplifyExpr(binding.thenBranch, env);
      const elseBranch = simplifyExpr(binding.elseBranch, env);
      if (
        cond !== binding.cond ||
        thenBranch !== binding.thenBranch ||
        elseBranch !== binding.elseBranch
      ) {
        return ir.irIfBinding(cond, thenBranch, elseBranch, binding.type);
      }
      return binding;
    }

    case "IRTupleBinding": {
      const elements = binding.elements.map((e) => copyAtom(e, env));
      const changed = elements.some((e, i) => e !== binding.elements[i]);
      if (changed) {
        return ir.irTupleBinding(elements, binding.type);
      }
      return binding;
    }

    case "IRRecordBinding": {
      const fields = binding.fields.map((f) => ({
        name: f.name,
        value: copyAtom(f.value, env),
      }));
      const changed = fields.some((f, i) => f.value !== binding.fields[i]?.value);
      if (changed) {
        return ir.irRecordBinding(fields, binding.type);
      }
      return binding;
    }

    case "IRFieldAccessBinding": {
      const record = copyAtom(binding.record, env);
      if (record !== binding.record) {
        return ir.irFieldAccessBinding(record, binding.field, binding.type);
      }
      return binding;
    }

    case "IRTupleIndexBinding": {
      const tuple = copyAtom(binding.tuple, env);
      if (tuple !== binding.tuple) {
        return ir.irTupleIndexBinding(tuple, binding.index, binding.type);
      }
      return binding;
    }

    case "IRMatchBinding": {
      const scrutinee = copyAtom(binding.scrutinee, env);
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? simplifyExpr(c.guard, env) : undefined,
        body: simplifyExpr(c.body, env),
      }));
      return ir.irMatchBinding(scrutinee, cases, binding.type);
    }

    case "IRLambdaBinding": {
      // Don't propagate into lambda bodies - they have their own scope
      // But we do simplify the body
      return ir.irLambdaBinding(
        binding.param,
        binding.paramType,
        simplifyExpr(binding.body, new Map()),
        binding.type,
        binding.tailRecursive,
      );
    }

    case "IRClosureBinding": {
      const captures = binding.captures.map((c) => copyAtom(c, env));
      const changed = captures.some((c, i) => c !== binding.captures[i]);
      if (changed) {
        return ir.irClosureBinding(binding.funcId, captures, binding.type);
      }
      return binding;
    }

    case "IRRecordUpdateBinding": {
      const base = copyAtom(binding.base, env);
      const fields = binding.fields.map((f) => ({
        name: f.name,
        value: copyAtom(f.value, env),
      }));
      const changed =
        base !== binding.base || fields.some((f, i) => f.value !== binding.fields[i]?.value);
      if (changed) {
        return ir.irRecordUpdateBinding(base, fields, binding.type);
      }
      return binding;
    }
  }
};

/**
 * Copy Propagation and ANF Simplification Pass
 *
 * Simplifies the IR by:
 * - Propagating variable copies: `let x = y in x + 1` → `y + 1`
 * - Eliminating trivial lets: `let x = y in x` → `y`
 * - Resolving copy chains: `let x = y in let z = x in z` → `y`
 */
export const copyPropagation: OptPass = {
  name: "copy-propagation",
  run(expr) {
    return simplifyExpr(expr, new Map());
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

    case "IRRecordUpdateBinding":
      addAtom(binding.base);
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
 *
 * For recursive bindings (IRLetRec), we need special handling:
 * - Self-references within a binding don't count as "uses"
 * - Only references from the body or from OTHER bindings count
 */
export const deadCodeElimination: OptPass = {
  name: "dead-code-elimination",
  run(expr) {
    return removeUnusedExpr(expr);
  },
};

/**
 * Remove unused bindings from an expression, computing uses locally.
 * This properly handles recursive bindings by not counting self-references.
 * The outerUses parameter tracks variables used by enclosing scopes (for forward references).
 */
const removeUnusedExpr = (expr: ir.IRExpr, outerUses: Set<string> = new Set()): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr;

    case "IRLet": {
      const newBinding = removeUnusedInBinding(expr.binding);
      const newBody = removeUnusedExpr(expr.body, outerUses);

      // Check if this binding is used in the body OR by outer scopes
      const uses = new Map<string, number>();
      collectUses(newBody, uses);

      const usedInBody = (uses.get(expr.name) ?? 0) > 0;
      const usedByOuter = outerUses.has(expr.name);

      if (!usedInBody && !usedByOuter && isPure(newBinding)) {
        return newBody;
      }

      return ir.irLet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      // First, collect all uses from bindings (these become outer uses for the body)
      const bindingUses = new Set<string>();
      for (const b of expr.bindings) {
        const uses = new Map<string, number>();
        collectUsesBinding(b.binding, uses);
        for (const name of uses.keys()) {
          bindingUses.add(name);
        }
      }

      // Combine with existing outer uses
      const combinedOuter = new Set([...outerUses, ...bindingUses]);

      // First, recursively process all binding bodies
      const processedBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: removeUnusedInBinding(b.binding),
      }));
      const newBody = removeUnusedExpr(expr.body, combinedOuter);

      // Collect uses from the body only
      const bodyUses = new Map<string, number>();
      collectUses(newBody, bodyUses);

      // Find which bindings are used (directly or transitively)
      const usedSet = new Set<string>();

      // Start with bindings directly used in the body
      for (const b of processedBindings) {
        if ((bodyUses.get(b.name) ?? 0) > 0) {
          usedSet.add(b.name);
        }
      }

      // Transitively find all bindings used by used bindings
      // (e.g., if `length` uses `foldr`, and `length` is used, then `foldr` is used)
      let changed = true;
      while (changed) {
        changed = false;
        for (const b of processedBindings) {
          if (usedSet.has(b.name)) continue;

          // Check if any used binding references this one
          for (const usedName of usedSet) {
            const usedBinding = processedBindings.find((x) => x.name === usedName);
            if (usedBinding) {
              const bindingUses = new Map<string, number>();
              collectUsesBinding(usedBinding.binding, bindingUses);
              if ((bindingUses.get(b.name) ?? 0) > 0) {
                usedSet.add(b.name);
                changed = true;
                break;
              }
            }
          }
        }
      }

      // Filter to only used bindings
      const usedBindings = processedBindings.filter((b) => usedSet.has(b.name));

      if (usedBindings.length === 0) {
        return newBody;
      }

      return ir.irLetRec(usedBindings, newBody);
    }
  }
};

/**
 * Remove unused bindings inside a binding (recurse into nested expressions).
 */
const removeUnusedInBinding = (binding: ir.IRBinding): ir.IRBinding => {
  switch (binding.kind) {
    case "IRAtomBinding":
    case "IRAppBinding":
    case "IRBinOpBinding":
    case "IRTupleBinding":
    case "IRRecordBinding":
    case "IRRecordUpdateBinding":
    case "IRFieldAccessBinding":
    case "IRTupleIndexBinding":
    case "IRClosureBinding":
      return binding;

    case "IRIfBinding":
      return ir.irIfBinding(
        binding.cond,
        removeUnusedExpr(binding.thenBranch),
        removeUnusedExpr(binding.elseBranch),
        binding.type,
      );

    case "IRMatchBinding":
      return ir.irMatchBinding(
        binding.scrutinee,
        binding.cases.map((c) => ({
          pattern: c.pattern,
          guard: c.guard ? removeUnusedExpr(c.guard) : undefined,
          body: removeUnusedExpr(c.body),
        })),
        binding.type,
      );

    case "IRLambdaBinding":
      return ir.irLambdaBinding(
        binding.param,
        binding.paramType,
        removeUnusedExpr(binding.body),
        binding.type,
        binding.tailRecursive,
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
 * Extract tail call arguments if expr ends with a tail call to funcName.
 *
 * In ANF, a curried tail call like `f (n-1) (n*acc)` becomes:
 *   let _t1 = n - 1 in      // arg 1 computation
 *   let _t2 = f _t1 in      // partial application f arg1
 *   let _t3 = n * acc in    // arg 2 computation
 *   let _t4 = _t2 _t3 in    // final application
 *   _t4                     // result
 *
 * This function traces back from the result variable to find the
 * complete application chain and extract all arguments.
 *
 * @param expr The expression to analyze
 * @param funcName The recursive function name to look for
 * @param paramCount Expected number of parameters
 * @returns Array of argument atoms if valid tail call, null otherwise
 */
const extractTailCall = (
  expr: ir.IRExpr,
  funcName: string,
  paramCount: number,
): ir.IRAtom[] | null => {
  // Step 1: Flatten let bindings into an array
  const bindings: Array<{ name: string; binding: ir.IRBinding }> = [];
  let current: ir.IRExpr = expr;

  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  // Step 2: Final expression must be a variable (the tail call result)
  if (current.kind !== "IRAtomExpr" || current.atom.kind !== "IRVar") {
    return null;
  }

  const resultVar = current.atom.name;

  // Step 3: Trace back through application chain
  // Start from the result variable and follow the application bindings
  // backwards to find all arguments and the original function call
  const args: ir.IRAtom[] = [];
  let targetVar = resultVar;

  for (let i = bindings.length - 1; i >= 0; i--) {
    const { name, binding } = bindings[i]!;

    if (name === targetVar && binding.kind === "IRAppBinding") {
      // Found an application in the chain - collect the argument
      args.unshift(binding.arg);

      if (binding.func.kind === "IRVar") {
        if (binding.func.name === funcName) {
          // Reached the recursive call - verify arity
          return args.length === paramCount ? args : null;
        }
        // Continue tracing - this is an intermediate partial application
        targetVar = binding.func.name;
      } else {
        return null; // Function must be a variable
      }
    }
    // Non-matching bindings are argument computations - skip them
  }

  return null;
};

/**
 * Check if a binding contains any reference to funcName in non-tail position.
 *
 * This is used to detect recursive calls that are NOT in tail position.
 * For example, in `n * f(n-1)`, the call to f is not in tail position
 * because the result is used as input to multiplication.
 *
 * Special handling for if/match: their branches ARE tail positions,
 * so we recursively check those with checkTailCalls instead.
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

    case "IRRecordUpdateBinding":
      return checkAtom(binding.base) || binding.fields.some((f) => checkAtom(f.value));

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

  // First, check if the function actually calls itself
  // If there are no recursive calls, it's not tail-recursive
  if (!hasRecursiveCallExpr(innerBody, name)) {
    return null;
  }

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
    case "IRRecordUpdateBinding":
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
  copyPropagation,
  deadCodeElimination,
  tailCallOptimization,
];

/**
 * Check if two IR expressions are structurally identical.
 * Used to detect when optimization reaches a fixpoint.
 */
const irEqual = (a: ir.IRExpr, b: ir.IRExpr): boolean => {
  // Use JSON serialization for simple structural comparison
  // This is efficient enough for typical program sizes
  return JSON.stringify(a) === JSON.stringify(b);
};

/**
 * Run all optimization passes on an IR expression until no more changes occur.
 *
 * Iterates to fixpoint: keeps running passes until the IR stabilizes.
 * This allows optimizations to enable further optimizations (e.g., constant
 * folding may enable dead code elimination, which may enable more folding).
 *
 * @param expr The IR expression to optimize
 * @param passes Optional custom passes (defaults to standard optimizations)
 * @param maxIterations Maximum iterations to prevent infinite loops (default: 10)
 * @returns The optimized IR expression
 */
export const optimize = (
  expr: ir.IRExpr,
  passes: readonly OptPass[] = defaultPasses,
  maxIterations = 10,
): ir.IRExpr => {
  let result = expr;
  for (let iteration = 0; iteration < maxIterations; iteration++) {
    const before = result;
    for (const pass of passes) {
      result = pass.run(result);
    }
    // Fixpoint reached - no more changes
    if (irEqual(before, result)) {
      break;
    }
  }
  return result;
};
