/**
 * IR Optimization Passes (Section 11)
 *
 * Transforms the IR to improve performance:
 * - Constant Folding: evaluate literal expressions at compile time
 * - Dead Code Elimination: remove unused bindings
 * - Tail Call Optimization: mark tail-recursive functions
 */

import * as IR from "./ir";
import type { Name } from "./core";
import type { Literal } from "./surface";

// =============================================================================
// Type Helpers
// =============================================================================

/** Compare two Names for equality */
const nameEq = (a: Name, b: Name): boolean => a.id === b.id;

/** Get a string key for a Name (for use in Maps/Sets) */
const nameKey = (name: Name): string => `${name.id}:${name.original}`;

// =============================================================================
// Constant Folding
// =============================================================================

type ConstValue = number | string | boolean;
type ConstEnv = Map<string, ConstValue>;

/** Extract constant value from a Literal */
const literalToConst = (lit: Literal): ConstValue => {
  switch (lit.kind) {
    case "int":
    case "float":
      return lit.value;
    case "string":
    case "char":
      return lit.value;
    case "bool":
      return lit.value;
  }
};

const foldExpr = (expr: IR.IRExpr, env: ConstEnv): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const foldedBinding = foldBinding(expr.binding, env);

      // Track constant values
      if (foldedBinding.kind === "IRBAtom" && foldedBinding.atom.kind === "ALit") {
        const constVal = literalToConst(foldedBinding.atom.value);
        const newEnv = new Map(env);
        newEnv.set(nameKey(expr.name), constVal);
        return IR.irlet(expr.name, foldedBinding, foldExpr(expr.body, newEnv));
      }

      return IR.irlet(expr.name, foldedBinding, foldExpr(expr.body, env));
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: foldBinding(b.binding, env),
      }));
      return IR.irletrec(newBindings, foldExpr(expr.body, env));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? foldExpr(c.guard, env) : null,
        body: foldExpr(c.body, env),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const foldBinding = (binding: IR.IRBinding, _env: ConstEnv): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
      return binding;

    case "IRBApp":
      // Can't fold function applications at compile time
      return binding;

    case "IRBBinOp":
      // TODO: Could fold constant binary operations
      return binding;

    case "IRBTuple":
      return binding;

    case "IRBRecord":
      return binding;

    case "IRBRecordUpdate":
      return binding;

    case "IRBField":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, foldExpr(binding.body, new Map()), binding.type);

    case "IRBForeign":
      return binding;

    case "IRBMatch":
      return binding;
  }
};

// =============================================================================
// Dead Code Elimination
// =============================================================================

const collectUses = (expr: IR.IRExpr, uses: Set<string>): void => {
  switch (expr.kind) {
    case "IRAtom":
      if (expr.atom.kind === "AVar") {
        uses.add(nameKey(expr.atom.name));
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

    case "IRMatch":
      if (expr.scrutinee.kind === "AVar") {
        uses.add(nameKey(expr.scrutinee.name));
      }
      for (const c of expr.cases) {
        if (c.guard) collectUses(c.guard, uses);
        collectUses(c.body, uses);
      }
      break;
  }
};

const collectUsesBinding = (binding: IR.IRBinding, uses: Set<string>): void => {
  const addAtom = (atom: IR.Atom) => {
    if (atom.kind === "AVar") {
      uses.add(nameKey(atom.name));
    }
  };

  switch (binding.kind) {
    case "IRBAtom":
      addAtom(binding.atom);
      break;

    case "IRBApp":
      addAtom(binding.func);
      addAtom(binding.arg);
      break;

    case "IRBBinOp":
      addAtom(binding.left);
      addAtom(binding.right);
      break;

    case "IRBTuple":
      for (const elem of binding.elements) {
        addAtom(elem);
      }
      break;

    case "IRBRecord":
      for (const f of binding.fields) {
        addAtom(f.value);
      }
      break;

    case "IRBRecordUpdate":
      addAtom(binding.record);
      for (const f of binding.fields) {
        addAtom(f.value);
      }
      break;

    case "IRBField":
      addAtom(binding.record);
      break;

    case "IRBLambda":
      collectUses(binding.body, uses);
      break;

    case "IRBForeign":
      for (const arg of binding.args) {
        addAtom(arg);
      }
      break;

    case "IRBMatch":
      addAtom(binding.scrutinee);
      for (const c of binding.cases) {
        if (c.guard) collectUses(c.guard, uses);
        collectUses(c.body, uses);
      }
      break;
  }
};

const removeUnused = (expr: IR.IRExpr, uses: Set<string>): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBinding = removeUnusedBinding(expr.binding, uses);
      const newBody = removeUnused(expr.body, uses);

      // Remove if not used
      if (!uses.has(nameKey(expr.name))) {
        return newBody;
      }

      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const usedBindings = expr.bindings.filter((b) => uses.has(nameKey(b.name)));
      const newBindings = usedBindings.map((b) => ({
        name: b.name,
        binding: removeUnusedBinding(b.binding, uses),
      }));
      const newBody = removeUnused(expr.body, uses);

      if (newBindings.length === 0) {
        return newBody;
      }

      return IR.irletrec(newBindings, newBody);
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? removeUnused(c.guard, uses) : null,
        body: removeUnused(c.body, uses),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const removeUnusedBinding = (binding: IR.IRBinding, uses: Set<string>): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
    case "IRBMatch":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, removeUnused(binding.body, uses), binding.type);
  }
};

// =============================================================================
// Tail Call Optimization
// =============================================================================

/**
 * Collect all parameters from nested lambdas.
 */
const collectLambdaParams = (binding: IR.IRBLambda): Name[] => {
  const params: Name[] = [binding.param];
  let body = binding.body;

  while (body.kind === "IRLet" && body.binding.kind === "IRBLambda") {
    params.push(body.binding.param);
    body = body.binding.body;
  }

  return params;
};

/**
 * Get the innermost body of nested lambdas.
 */
const getInnermostBody = (binding: IR.IRBLambda): IR.IRExpr => {
  let body = binding.body;

  while (body.kind === "IRLet" && body.binding.kind === "IRBLambda") {
    body = body.binding.body;
  }

  return body;
};

/**
 * Check if an expression contains a call to the named function.
 */
const hasCallTo = (expr: IR.IRExpr, funcName: Name): boolean => {
  switch (expr.kind) {
    case "IRAtom":
      return false;

    case "IRLet":
      return hasCallToBinding(expr.binding, funcName) || hasCallTo(expr.body, funcName);

    case "IRLetRec":
      return (
        expr.bindings.some((b) => hasCallToBinding(b.binding, funcName)) ||
        hasCallTo(expr.body, funcName)
      );

    case "IRMatch":
      return expr.cases.some((c) => {
        if (c.guard && hasCallTo(c.guard, funcName)) return true;
        return hasCallTo(c.body, funcName);
      });
  }
};

const hasCallToBinding = (binding: IR.IRBinding, funcName: Name): boolean => {
  switch (binding.kind) {
    case "IRBAtom":
      return binding.atom.kind === "AVar" && nameEq(binding.atom.name, funcName);

    case "IRBApp":
      return (
        (binding.func.kind === "AVar" && nameEq(binding.func.name, funcName)) ||
        (binding.arg.kind === "AVar" && nameEq(binding.arg.name, funcName))
      );

    case "IRBBinOp":
      return (
        (binding.left.kind === "AVar" && nameEq(binding.left.name, funcName)) ||
        (binding.right.kind === "AVar" && nameEq(binding.right.name, funcName))
      );

    case "IRBTuple":
      return binding.elements.some((e) => e.kind === "AVar" && nameEq(e.name, funcName));

    case "IRBRecord":
      return binding.fields.some((f) => f.value.kind === "AVar" && nameEq(f.value.name, funcName));

    case "IRBRecordUpdate":
      return (
        (binding.record.kind === "AVar" && nameEq(binding.record.name, funcName)) ||
        binding.fields.some((f) => f.value.kind === "AVar" && nameEq(f.value.name, funcName))
      );

    case "IRBField":
      return binding.record.kind === "AVar" && nameEq(binding.record.name, funcName);

    case "IRBLambda":
      return hasCallTo(binding.body, funcName);

    case "IRBForeign":
      return binding.args.some((a) => a.kind === "AVar" && nameEq(a.name, funcName));

    case "IRBMatch":
      return (
        (binding.scrutinee.kind === "AVar" && nameEq(binding.scrutinee.name, funcName)) ||
        binding.cases.some(
          (c) => hasCallTo(c.body, funcName) || (c.guard && hasCallTo(c.guard, funcName)),
        )
      );
  }
};

/**
 * Check if all calls to funcName in the expression are in tail position.
 */
const isTailCallOnly = (expr: IR.IRExpr, funcName: Name): boolean => {
  switch (expr.kind) {
    case "IRAtom":
      return true;

    case "IRLet": {
      // Check if binding has non-tail calls
      if (hasNonTailCall(expr.binding, funcName)) {
        return false;
      }
      return isTailCallOnly(expr.body, funcName);
    }

    case "IRLetRec":
      return isTailCallOnly(expr.body, funcName);

    case "IRMatch":
      return expr.cases.every((c) => {
        if (c.guard && hasCallTo(c.guard, funcName)) return false;
        return isTailCallOnly(c.body, funcName);
      });
  }
};

const hasNonTailCall = (binding: IR.IRBinding, funcName: Name): boolean => {
  switch (binding.kind) {
    case "IRBAtom":
      // Atom referencing funcName is not a call, just a reference
      return false;

    case "IRBApp":
      // If func is funcName, check if arg references funcName
      if (binding.arg.kind === "AVar" && nameEq(binding.arg.name, funcName)) {
        return true;
      }
      return false;

    case "IRBBinOp":
      // Binary ops with funcName reference are non-tail calls
      return (
        (binding.left.kind === "AVar" && nameEq(binding.left.name, funcName)) ||
        (binding.right.kind === "AVar" && nameEq(binding.right.name, funcName))
      );

    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return false;

    case "IRBLambda":
      // Lambdas don't count as tail calls
      return false;

    case "IRBMatch":
      // Match bindings - scrutinee is not a tail position
      return binding.scrutinee.kind === "AVar" && nameEq(binding.scrutinee.name, funcName);
  }
};

/**
 * Mark a lambda as tail-recursive.
 */
const markTailRecursive = (
  binding: IR.IRBLambda,
  funcName: Name,
  params: readonly Name[],
): IR.IRBLambda => ({
  ...binding,
  tailRecursive: { funcName, params },
});

/**
 * Transform expression, marking tail-recursive functions.
 */
const transformTCO = (expr: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBinding = transformBindingTCO(expr.binding);
      return IR.irlet(expr.name, newBinding, transformTCO(expr.body));
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => {
        if (b.binding.kind === "IRBLambda") {
          const params = collectLambdaParams(b.binding);
          const innerBody = getInnermostBody(b.binding);

          // Check if this is tail-recursive
          if (hasCallTo(innerBody, b.name) && isTailCallOnly(innerBody, b.name)) {
            const marked = markTailRecursive(b.binding, b.name, params);
            return { name: b.name, binding: transformBindingTCO(marked) };
          }
        }
        return { name: b.name, binding: transformBindingTCO(b.binding) };
      });
      return IR.irletrec(newBindings, transformTCO(expr.body));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? transformTCO(c.guard) : null,
        body: transformTCO(c.body),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const transformBindingTCO = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
    case "IRBMatch":
      return binding;

    case "IRBLambda":
      return {
        ...binding,
        body: transformTCO(binding.body),
      };
  }
};

// =============================================================================
// Optimization Pipeline
// =============================================================================

/**
 * Optimize a single IR expression.
 */
const optimizeExpr = (expr: IR.IRExpr): IR.IRExpr => {
  // 1. Constant folding
  let result = foldExpr(expr, new Map());

  // 2. Dead code elimination
  const uses = new Set<string>();
  collectUses(result, uses);
  result = removeUnused(result, uses);

  // 3. Tail call optimization
  result = transformTCO(result);

  return result;
};

/**
 * Optimize a declaration.
 */
const optimizeDecl = (decl: IR.IRDecl): IR.IRDecl => {
  switch (decl.kind) {
    case "IRDeclType":
      return decl;

    case "IRDeclLet":
      return IR.irdecllet(decl.name, optimizeBinding(decl.binding));

    case "IRDeclLetRec": {
      const newBindings = decl.bindings.map((b) => ({
        name: b.name,
        binding: optimizeBinding(b.binding),
      }));
      return IR.irdeclletrec(newBindings);
    }
  }
};

const optimizeBinding = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
    case "IRBMatch":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, optimizeExpr(binding.body), binding.type);
  }
};

/**
 * Optimize an IR program.
 */
export const optimize = (program: IR.IRProgram): IR.IRProgram => {
  const decls = program.decls.map(optimizeDecl);
  const main = program.main ? optimizeExpr(program.main) : null;

  return { decls, main };
};
