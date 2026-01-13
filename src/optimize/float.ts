/**
 * Let Floating Transformations
 *
 * - Float-outward (let hoisting): Move bindings out of lambdas for sharing
 * - Float-inward (let sinking): Move bindings into branches where they're used
 */

import * as IR from "../ir";
import type { Name } from "../core";
import { nameKey, freeVars, collectFreeVarsBinding } from "./types";

// =============================================================================
// Binding Reference Detection
// =============================================================================

/** Check if a binding references a specific variable */
const bindingReferences = (binding: IR.IRBinding, varKey: string): boolean => {
  const free = new Set<string>();
  collectFreeVarsBinding(binding, new Set(), free);
  return free.has(varKey);
};

// =============================================================================
// Float Outward (Let Hoisting)
// =============================================================================

type HoistResult = {
  hoistedBindings: { name: Name; binding: IR.IRBinding }[];
  lambda: IR.IRBLambda;
};

/**
 * Try to hoist let bindings out of a lambda.
 * Only hoists bindings that don't reference the lambda parameter.
 */
const tryHoistFromLambda = (lambda: IR.IRBLambda): HoistResult | null => {
  const paramKey = nameKey(lambda.param);
  const hoisted: { name: Name; binding: IR.IRBinding }[] = [];

  let currentBody = lambda.body;

  while (currentBody.kind === "IRLet") {
    if (bindingReferences(currentBody.binding, paramKey)) {
      break;
    }

    hoisted.push({ name: currentBody.name, binding: currentBody.binding });
    currentBody = currentBody.body;
  }

  if (hoisted.length === 0) return null;

  const newLambda: IR.IRBLambda = {
    ...lambda,
    body: currentBody,
  };

  return { hoistedBindings: hoisted, lambda: newLambda };
};

/**
 * Float-outward: hoist bindings out of lambdas.
 */
export const floatOutward = (expr: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBody = floatOutward(expr.body);

      if (expr.binding.kind === "IRBLambda") {
        const hoisted = tryHoistFromLambda(expr.binding);
        if (hoisted) {
          let result: IR.IRExpr = IR.irlet(expr.name, hoisted.lambda, newBody);
          for (let i = hoisted.hoistedBindings.length - 1; i >= 0; i--) {
            const h = hoisted.hoistedBindings[i];
            if (h) {
              result = IR.irlet(h.name, h.binding, result);
            }
          }
          return floatOutward(result);
        }
      }

      const newBinding = floatOutwardBinding(expr.binding);
      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: floatOutwardBinding(b.binding),
      }));
      return IR.irletrec(newBindings, floatOutward(expr.body));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? floatOutward(c.guard) : null,
        body: floatOutward(c.body),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const floatOutwardBinding = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, floatOutward(binding.body), binding.type);

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? floatOutward(c.guard) : null,
        body: floatOutward(c.body),
      }));
      return IR.irbmatch(binding.scrutinee, cases, binding.type);
    }
  }
};

// =============================================================================
// Float Inward (Let Sinking)
// =============================================================================

/**
 * Try to sink a binding into a match expression.
 * Only sinks if the binding is used in some but not all branches.
 */
const trySinkIntoMatch = (
  bindingName: Name,
  binding: IR.IRBinding,
  match: IR.IRMatch,
): IR.IRExpr | null => {
  const bindingKey = nameKey(bindingName);

  const branchUsage = match.cases.map((c) => {
    const fv = freeVars(c.body);
    const guardFv = c.guard ? freeVars(c.guard) : new Set<string>();
    return fv.has(bindingKey) || guardFv.has(bindingKey);
  });

  const usedCount = branchUsage.filter(Boolean).length;

  // Don't sink if:
  // - Used in all branches (no benefit)
  // - Not used anywhere (will be eliminated by DCE)
  // - Binding is trivial (atom) - no benefit
  if (usedCount === 0 || usedCount === match.cases.length) return null;
  if (binding.kind === "IRBAtom") return null;

  const newCases = match.cases.map((c, i) => {
    if (branchUsage[i]) {
      return {
        pattern: c.pattern,
        guard: c.guard,
        body: IR.irlet(bindingName, binding, c.body),
      };
    }
    return c;
  });

  return IR.irmatch(match.scrutinee, newCases, match.type);
};

/**
 * Float-inward: sink bindings into branches where they're used.
 */
export const floatInward = (expr: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBody = floatInward(expr.body);

      if (newBody.kind === "IRMatch") {
        const sunk = trySinkIntoMatch(expr.name, expr.binding, newBody);
        if (sunk) {
          return floatInward(sunk);
        }
      }

      const newBinding = floatInwardBinding(expr.binding);
      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: floatInwardBinding(b.binding),
      }));
      return IR.irletrec(newBindings, floatInward(expr.body));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? floatInward(c.guard) : null,
        body: floatInward(c.body),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const floatInwardBinding = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, floatInward(binding.body), binding.type);

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? floatInward(c.guard) : null,
        body: floatInward(c.body),
      }));
      return IR.irbmatch(binding.scrutinee, cases, binding.type);
    }
  }
};
