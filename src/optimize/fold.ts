/**
 * Constant Folding, Beta Reduction, and Case-of-Known
 *
 * Evaluates expressions at compile time when possible.
 */

import * as IR from "../ir";
import type { Name } from "../core";
import type { Literal, Span } from "../surface";
import {
  type ExtendedEnv,
  type ConstEnv,
  type RenameEnv,
  nameKey,
  literalToConst,
  getExtendedValueFromBinding,
  toConstEnv,
  tryEtaReduce,
  bindResultTo,
} from "./types";
import { alphaRenameExpr } from "./alpha";

/**
 * Counter for generating fresh names during folding.
 * Uses negative IDs to avoid collision with resolved names.
 * These Names are never used for type lookup (optimization happens after
 * type checking and types are already embedded in IR nodes).
 */
let foldCounter = 0;
const syntheticSpan: Span = { fileId: 0, start: 0, end: 0 };
const freshName = (text: string, span: Span): Name => {
  const id = --foldCounter;
  return { id, nodeId: id, text, span: span ?? syntheticSpan };
};

// =============================================================================
// Known Value Types
// =============================================================================

type KnownValue =
  | { kind: "lit"; value: Literal }
  | { kind: "con"; name: string; args: readonly IR.Atom[] }
  | { kind: "tuple"; elements: readonly IR.Atom[] };

type MatchResult = {
  bindings: { name: Name; atom: IR.Atom }[];
};

// =============================================================================
// Fold Expression
// =============================================================================

export const foldExpr = (expr: IR.IRExpr, env: ExtendedEnv): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      // Try beta reduction: (λx. body) arg → let x = arg in body
      if (expr.binding.kind === "IRBApp" && expr.binding.func.kind === "AVar") {
        const lambdaVal = env.get(nameKey(expr.binding.func.name));
        if (lambdaVal && lambdaVal.kind === "lambda") {
          // Beta reduction with alpha-renaming to avoid name conflicts
          const freshParam = freshName(lambdaVal.param.text, lambdaVal.param.span);
          const renameEnv: RenameEnv = new Map();
          renameEnv.set(nameKey(lambdaVal.param), freshParam);
          const renamedBody = alphaRenameExpr(lambdaVal.body, renameEnv);
          const paramBinding = IR.irlet(
            freshParam,
            IR.irbatom(expr.binding.arg),
            bindResultTo(renamedBody, expr.name, expr.body),
          );
          // Recursively fold the result
          return foldExpr(paramBinding, env);
        }
      }

      const constEnv = toConstEnv(env);
      const foldedBinding = foldBinding(expr.binding, constEnv);

      // Track extended values
      const extValue = getExtendedValueFromBinding(foldedBinding);
      if (extValue) {
        const newEnv = new Map(env);
        newEnv.set(nameKey(expr.name), extValue);
        return IR.irlet(expr.name, foldedBinding, foldExpr(expr.body, newEnv));
      }

      return IR.irlet(expr.name, foldedBinding, foldExpr(expr.body, env));
    }

    case "IRLetRec": {
      const constEnv = toConstEnv(env);
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: foldBinding(b.binding, constEnv),
      }));
      return IR.irletrec(newBindings, foldExpr(expr.body, env));
    }

    case "IRMatch": {
      // First, fold the cases
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? foldExpr(c.guard, env) : null,
        body: foldExpr(c.body, env),
      }));

      // Try case-of-known-constructor optimization
      const simplified = tryCaseOfKnownExt(expr.scrutinee, cases, env);
      if (simplified) {
        return simplified;
      }

      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

// =============================================================================
// Case-of-Known-Constructor
// =============================================================================

/** Get the known value from an atom using extended environment */
const getKnownValueExt = (atom: IR.Atom, env: ExtendedEnv): KnownValue | null => {
  switch (atom.kind) {
    case "ALit":
      return { kind: "lit", value: atom.value };
    case "ACon":
      return { kind: "con", name: atom.name, args: [] };
    case "AVar": {
      const val = env.get(nameKey(atom.name));
      if (val === undefined) return null;

      switch (val.kind) {
        case "const":
          if (typeof val.value === "boolean") {
            return { kind: "lit", value: { kind: "bool", value: val.value } };
          }
          if (typeof val.value === "number") {
            return {
              kind: "lit",
              value: Number.isInteger(val.value)
                ? { kind: "int", value: val.value }
                : { kind: "float", value: val.value },
            };
          }
          if (typeof val.value === "string") {
            return { kind: "lit", value: { kind: "string", value: val.value } };
          }
          return null;

        case "con":
          return { kind: "con", name: val.name, args: val.args };

        case "tuple":
          return { kind: "tuple", elements: val.elements };

        case "atom":
          return getKnownValueExt(val.atom, env);

        case "lambda":
          return null;
      }
    }
  }
};

const isFullyKnown = (value: KnownValue, env: ExtendedEnv): boolean => {
  switch (value.kind) {
    case "lit":
      return true;
    case "con": {
      for (const arg of value.args) {
        const argValue = getKnownValueExt(arg, env);
        if (!argValue || !isFullyKnown(argValue, env)) return false;
      }
      return true;
    }
    case "tuple": {
      for (const elem of value.elements) {
        const elemValue = getKnownValueExt(elem, env);
        if (!elemValue || !isFullyKnown(elemValue, env)) return false;
      }
      return true;
    }
  }
};

const tryCaseOfKnownExt = (
  scrutinee: IR.Atom,
  cases: readonly IR.IRCase[],
  env: ExtendedEnv,
): IR.IRExpr | null => {
  const knownValue = getKnownValueExt(scrutinee, env);
  if (!knownValue) return null;
  if (!isFullyKnown(knownValue, env)) return null;

  for (const c of cases) {
    if (c.guard) continue;
    const matchResult = matchPattern(c.pattern, knownValue, scrutinee, env);
    if (matchResult) {
      return wrapWithBindings(matchResult.bindings, c.body);
    }
  }

  return null;
};

const matchPattern = (
  pattern: IR.IRPattern,
  value: KnownValue,
  scrutinee: IR.Atom,
  env: ExtendedEnv,
): MatchResult | null => {
  switch (pattern.kind) {
    case "IRPWild":
      return { bindings: [] };

    case "IRPVar":
      return { bindings: [{ name: pattern.name, atom: scrutinee }] };

    case "IRPLit":
      if (value.kind !== "lit") return null;
      if (literalsEqual(pattern.value, value.value)) {
        return { bindings: [] };
      }
      return null;

    case "IRPCon": {
      if (value.kind !== "con") return null;
      if (pattern.name !== value.name) return null;
      if (pattern.args.length !== value.args.length) return null;
      const bindings: { name: Name; atom: IR.Atom }[] = [];
      for (let i = 0; i < pattern.args.length; i++) {
        const patArg = pattern.args[i];
        const valArg = value.args[i];
        if (!patArg || !valArg) return null;
        const argValue = getKnownValueExt(valArg, env);
        if (!argValue) {
          if (patArg.kind === "IRPVar") {
            bindings.push({ name: patArg.name, atom: valArg });
            continue;
          }
          if (patArg.kind === "IRPWild") {
            continue;
          }
          return null;
        }
        const subMatch = matchPattern(patArg, argValue, valArg, env);
        if (!subMatch) return null;
        bindings.push(...subMatch.bindings);
      }
      return { bindings };
    }

    case "IRPTuple": {
      if (value.kind !== "tuple") return null;
      if (pattern.elements.length !== value.elements.length) return null;
      const tupleBindings: { name: Name; atom: IR.Atom }[] = [];
      for (let i = 0; i < pattern.elements.length; i++) {
        const patElem = pattern.elements[i];
        const valElem = value.elements[i];
        if (!patElem || !valElem) return null;
        const elemValue = getKnownValueExt(valElem, env);
        if (!elemValue) {
          if (patElem.kind === "IRPVar") {
            tupleBindings.push({ name: patElem.name, atom: valElem });
            continue;
          }
          if (patElem.kind === "IRPWild") {
            continue;
          }
          return null;
        }
        const subMatch = matchPattern(patElem, elemValue, valElem, env);
        if (!subMatch) return null;
        tupleBindings.push(...subMatch.bindings);
      }
      return { bindings: tupleBindings };
    }

    case "IRPRecord":
      return null;

    case "IRPAs": {
      const subMatch = matchPattern(pattern.pattern, value, scrutinee, env);
      if (!subMatch) return null;
      return { bindings: [{ name: pattern.name, atom: scrutinee }, ...subMatch.bindings] };
    }
  }
};

const literalsEqual = (a: Literal, b: Literal): boolean => {
  if (a.kind !== b.kind) return false;
  return a.value === b.value;
};

const wrapWithBindings = (
  bindings: { name: Name; atom: IR.Atom }[],
  body: IR.IRExpr,
): IR.IRExpr => {
  let result = body;
  for (let i = bindings.length - 1; i >= 0; i--) {
    const b = bindings[i];
    if (b) {
      result = IR.irlet(b.name, IR.irbatom(b.atom), result);
    }
  }
  return result;
};

// =============================================================================
// Binary Operation Folding
// =============================================================================

export const tryFoldBinOp = (
  op: string,
  left: IR.Atom,
  right: IR.Atom,
  env: ConstEnv,
): IR.Atom | null => {
  const getConstValue = (atom: IR.Atom): number | string | boolean | null => {
    if (atom.kind === "ALit") {
      return literalToConst(atom.value);
    }
    if (atom.kind === "AVar") {
      const val = env.get(nameKey(atom.name));
      return val !== undefined ? val : null;
    }
    return null;
  };

  const leftVal = getConstValue(left);
  const rightVal = getConstValue(right);

  if (leftVal === null || rightVal === null) return null;

  // Arithmetic operations
  if (typeof leftVal === "number" && typeof rightVal === "number") {
    let result: number | boolean | null = null;
    switch (op) {
      case "+":
        result = leftVal + rightVal;
        break;
      case "-":
        result = leftVal - rightVal;
        break;
      case "*":
        result = leftVal * rightVal;
        break;
      case "/":
        if (rightVal !== 0) result = Math.trunc(leftVal / rightVal);
        break;
      case "<":
        result = leftVal < rightVal;
        break;
      case "<=":
        result = leftVal <= rightVal;
        break;
      case ">":
        result = leftVal > rightVal;
        break;
      case ">=":
        result = leftVal >= rightVal;
        break;
      case "==":
        result = leftVal === rightVal;
        break;
      case "!=":
        result = leftVal !== rightVal;
        break;
    }
    if (result !== null) {
      if (typeof result === "boolean") {
        return IR.alit({ kind: "bool", value: result }, { kind: "TCon", name: "boolean" });
      }
      const isInt =
        Number.isInteger(leftVal) && Number.isInteger(rightVal) && Number.isInteger(result);
      return IR.alit(
        { kind: isInt ? "int" : "float", value: result },
        { kind: "TCon", name: isInt ? "int" : "float" },
      );
    }
  }

  // String concatenation
  if (typeof leftVal === "string" && typeof rightVal === "string" && op === "+") {
    return IR.alit({ kind: "string", value: leftVal + rightVal }, { kind: "TCon", name: "string" });
  }

  // String comparison
  if (typeof leftVal === "string" && typeof rightVal === "string") {
    let result: boolean | null = null;
    switch (op) {
      case "==":
        result = leftVal === rightVal;
        break;
      case "!=":
        result = leftVal !== rightVal;
        break;
      case "<":
        result = leftVal < rightVal;
        break;
      case "<=":
        result = leftVal <= rightVal;
        break;
      case ">":
        result = leftVal > rightVal;
        break;
      case ">=":
        result = leftVal >= rightVal;
        break;
    }
    if (result !== null) {
      return IR.alit({ kind: "bool", value: result }, { kind: "TCon", name: "boolean" });
    }
  }

  // Boolean operations
  if (typeof leftVal === "boolean" && typeof rightVal === "boolean") {
    let result: boolean | null = null;
    switch (op) {
      case "==":
        result = leftVal === rightVal;
        break;
      case "!=":
        result = leftVal !== rightVal;
        break;
      case "&&":
        result = leftVal && rightVal;
        break;
      case "||":
        result = leftVal || rightVal;
        break;
    }
    if (result !== null) {
      return IR.alit({ kind: "bool", value: result }, { kind: "TCon", name: "boolean" });
    }
  }

  // Short-circuit boolean operations
  if (typeof leftVal === "boolean") {
    if (op === "&&") {
      if (leftVal === false) {
        return IR.alit({ kind: "bool", value: false }, { kind: "TCon", name: "boolean" });
      }
      if (leftVal === true) return right;
    }
    if (op === "||") {
      if (leftVal === true) {
        return IR.alit({ kind: "bool", value: true }, { kind: "TCon", name: "boolean" });
      }
      if (leftVal === false) return right;
    }
  }

  return null;
};

// =============================================================================
// Fold Binding
// =============================================================================

export const foldBinding = (binding: IR.IRBinding, env: ConstEnv): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
      return binding;

    case "IRBApp":
      return binding;

    case "IRBBinOp": {
      const folded = tryFoldBinOp(binding.op, binding.left, binding.right, env);
      if (folded) {
        return IR.irbatom(folded);
      }
      return binding;
    }

    case "IRBTuple":
      return binding;

    case "IRBRecord":
      return binding;

    case "IRBRecordUpdate":
      return binding;

    case "IRBField":
      return binding;

    case "IRBLambda": {
      const foldedBody = foldExpr(binding.body, new Map());
      const etaReduced = tryEtaReduce(binding.param, foldedBody);
      if (etaReduced) {
        return etaReduced;
      }
      return IR.irblambda(binding.param, foldedBody, binding.type);
    }

    case "IRBForeign":
      return binding;

    case "IRBMatch":
      return binding;
  }
};
