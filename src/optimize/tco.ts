/**
 * Tail Call Optimization
 *
 * Detects and marks tail-recursive functions for efficient compilation.
 */

import * as IR from "../ir";
import type { Name } from "../core";
import { nameEq } from "./types";

// =============================================================================
// Lambda Parameter Collection
// =============================================================================

/** Collect all parameters from nested lambdas. */
const collectLambdaParams = (binding: IR.IRBLambda): Name[] => {
  const params: Name[] = [binding.param];
  let body = binding.body;

  while (body.kind === "IRLet" && body.binding.kind === "IRBLambda") {
    params.push(body.binding.param);
    body = body.binding.body;
  }

  return params;
};

/** Get the innermost body of nested lambdas. */
const getInnermostBody = (binding: IR.IRBLambda): IR.IRExpr => {
  let body = binding.body;

  while (body.kind === "IRLet" && body.binding.kind === "IRBLambda") {
    body = body.binding.body;
  }

  return body;
};

// =============================================================================
// Call Detection
// =============================================================================

/** Check if an expression contains a call to the named function. */
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

// =============================================================================
// Tail Position Analysis
// =============================================================================

/** Check if all calls to funcName in the expression are in tail position. */
const isTailCallOnly = (expr: IR.IRExpr, funcName: Name): boolean => {
  switch (expr.kind) {
    case "IRAtom":
      return true;

    case "IRLet": {
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

// =============================================================================
// TCO Transformation
// =============================================================================

/** Mark a lambda as tail-recursive. */
const markTailRecursive = (
  binding: IR.IRBLambda,
  funcName: Name,
  params: readonly Name[],
): IR.IRBLambda => ({
  ...binding,
  tailRecursive: { funcName, params },
});

/** Transform expression, marking tail-recursive functions. */
export const transformTCO = (expr: IR.IRExpr): IR.IRExpr => {
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
