/**
 * Case-of-Case Transformation
 *
 * Pushes outer match into branches of inner match to expose more
 * case-of-known-constructor optimization opportunities.
 */

import * as IR from "../ir";
import type { Name } from "../core";
import { nameEq } from "./types";

// =============================================================================
// Size Estimation
// =============================================================================

/** Estimate the "size" of an expression for code duplication heuristics */
const exprSize = (expr: IR.IRExpr): number => {
  switch (expr.kind) {
    case "IRAtom":
      return 1;
    case "IRLet":
      return 1 + bindingSize(expr.binding) + exprSize(expr.body);
    case "IRLetRec":
      return (
        1 + expr.bindings.reduce((acc, b) => acc + bindingSize(b.binding), 0) + exprSize(expr.body)
      );
    case "IRMatch":
      return (
        1 +
        expr.cases.reduce((acc, c) => acc + exprSize(c.body) + (c.guard ? exprSize(c.guard) : 0), 0)
      );
  }
};

const bindingSize = (binding: IR.IRBinding): number => {
  switch (binding.kind) {
    case "IRBAtom":
      return 1;
    case "IRBApp":
    case "IRBBinOp":
    case "IRBField":
      return 2;
    case "IRBTuple":
      return binding.elements.length;
    case "IRBRecord":
    case "IRBRecordUpdate":
      return binding.fields.length;
    case "IRBLambda":
      return 1 + exprSize(binding.body);
    case "IRBForeign":
      return 1 + binding.args.length;
    case "IRBMatch":
      return (
        1 +
        binding.cases.reduce(
          (acc, c) => acc + exprSize(c.body) + (c.guard ? exprSize(c.guard) : 0),
          0,
        )
      );
  }
};

// =============================================================================
// Constants
// =============================================================================

/** Maximum size of outer cases to allow duplication */
const MAX_OUTER_CASES_SIZE = 50;

/** Maximum number of inner branches to transform */
const MAX_INNER_BRANCHES = 8;

// =============================================================================
// Bind Result Helper
// =============================================================================

/**
 * Bind the result of an expression to a name and continue.
 */
const bindResultTo = (expr: IR.IRExpr, name: Name, cont: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return IR.irlet(name, IR.irbatom(expr.atom), cont);
    case "IRLet":
      return IR.irlet(expr.name, expr.binding, bindResultTo(expr.body, name, cont));
    case "IRLetRec":
      return IR.irletrec(expr.bindings, bindResultTo(expr.body, name, cont));
    case "IRMatch":
      return IR.irlet(name, IR.irbmatch(expr.scrutinee, expr.cases, expr.type), cont);
  }
};

// =============================================================================
// Case-of-Case Transformation
// =============================================================================

type OuterMatchInfo = {
  outerMatch: IR.IRMatch | IR.IRBMatch;
  context: (result: IR.IRExpr) => IR.IRExpr;
};

/**
 * Find an outer match expression that uses the given name as its scrutinee.
 */
const findOuterMatch = (expr: IR.IRExpr, targetName: Name): OuterMatchInfo | null => {
  switch (expr.kind) {
    case "IRAtom":
      return null;

    case "IRMatch":
      if (expr.scrutinee.kind === "AVar" && nameEq(expr.scrutinee.name, targetName)) {
        return {
          outerMatch: expr,
          context: (result) => result,
        };
      }
      return null;

    case "IRLet": {
      if (expr.binding.kind === "IRBMatch") {
        if (
          expr.binding.scrutinee.kind === "AVar" &&
          nameEq(expr.binding.scrutinee.name, targetName)
        ) {
          return {
            outerMatch: expr.binding,
            context: (result) => {
              return bindResultTo(result, expr.name, expr.body);
            },
          };
        }
      }

      if (nameEq(expr.name, targetName)) return null;

      const innerResult = findOuterMatch(expr.body, targetName);
      if (innerResult) {
        return {
          outerMatch: innerResult.outerMatch,
          context: (result) => IR.irlet(expr.name, expr.binding, innerResult.context(result)),
        };
      }
      return null;
    }

    case "IRLetRec": {
      if (expr.bindings.some((b) => nameEq(b.name, targetName))) return null;

      const innerResult = findOuterMatch(expr.body, targetName);
      if (innerResult) {
        return {
          outerMatch: innerResult.outerMatch,
          context: (result) => IR.irletrec(expr.bindings, innerResult.context(result)),
        };
      }
      return null;
    }
  }
};

/** Create a match expression from an IRBMatch or IRMatch, using a variable as scrutinee */
const createMatchExpr = (match: IR.IRMatch | IR.IRBMatch, varName: Name): IR.IRExpr => {
  const scrutinee = IR.avar(varName, match.scrutinee.type);
  return IR.irmatch(scrutinee, match.cases, match.type);
};

/**
 * Substitute the result of an inner match case into the outer match.
 */
const substituteInnerResult = (
  innerBody: IR.IRExpr,
  innerName: Name,
  outerMatch: IR.IRMatch | IR.IRBMatch,
  context: (result: IR.IRExpr) => IR.IRExpr,
): IR.IRExpr => {
  if (innerBody.kind === "IRAtom") {
    const newOuterMatch = IR.irmatch(innerBody.atom, outerMatch.cases, outerMatch.type);
    return context(newOuterMatch);
  }

  return bindResultTo(innerBody, innerName, context(createMatchExpr(outerMatch, innerName)));
};

/**
 * Try to apply case-of-case transformation.
 */
const tryCaseOfCase = (expr: IR.IRLet): IR.IRExpr | null => {
  if (expr.binding.kind !== "IRBMatch") return null;

  const innerMatch = expr.binding;
  const innerName = expr.name;

  if (innerMatch.cases.length > MAX_INNER_BRANCHES) return null;

  const outerMatchInfo = findOuterMatch(expr.body, innerName);
  if (!outerMatchInfo) return null;

  const { outerMatch, context } = outerMatchInfo;

  const outerSize = outerMatch.cases.reduce(
    (acc, c) => acc + exprSize(c.body) + (c.guard ? exprSize(c.guard) : 0),
    0,
  );
  if (outerSize > MAX_OUTER_CASES_SIZE) return null;

  const newCases: IR.IRCase[] = innerMatch.cases.map((innerCase) => {
    const newBody = substituteInnerResult(innerCase.body, innerName, outerMatch, context);
    return {
      pattern: innerCase.pattern,
      guard: innerCase.guard,
      body: newBody,
    };
  });

  return IR.irmatch(innerMatch.scrutinee, newCases, outerMatch.type);
};

// =============================================================================
// Apply Transformation
// =============================================================================

/** Apply case-of-case transformation throughout an expression. */
export const applyCaseOfCase = (expr: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBinding = applyCaseOfCaseBinding(expr.binding);
      const newBody = applyCaseOfCase(expr.body);
      const newExpr = IR.irlet(expr.name, newBinding, newBody);

      const transformed = tryCaseOfCase(newExpr);
      if (transformed) {
        return applyCaseOfCase(transformed);
      }
      return newExpr;
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: applyCaseOfCaseBinding(b.binding),
      }));
      return IR.irletrec(newBindings, applyCaseOfCase(expr.body));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? applyCaseOfCase(c.guard) : null,
        body: applyCaseOfCase(c.body),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const applyCaseOfCaseBinding = (binding: IR.IRBinding): IR.IRBinding => {
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
      return IR.irblambda(binding.param, applyCaseOfCase(binding.body), binding.type);

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? applyCaseOfCase(c.guard) : null,
        body: applyCaseOfCase(c.body),
      }));
      return IR.irbmatch(binding.scrutinee, cases, binding.type);
    }
  }
};
