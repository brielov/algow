/**
 * Shared Types and Utilities for IR Optimization
 */

import * as IR from "../ir";
import type { Name } from "../core";
import type { Literal } from "../surface";
import type { Type } from "../types";

// =============================================================================
// Name Helpers
// =============================================================================

/** Compare two Names for equality */
export const nameEq = (a: Name, b: Name): boolean => a.id === b.id;

/** Get a string key for a Name (for use in Maps/Sets) */
export const nameKey = (name: Name): string => `${name.id}:${name.text}`;

// =============================================================================
// Environment Types
// =============================================================================

export type ConstValue = number | string | boolean;
export type ConstEnv = Map<string, ConstValue>;

/** Extended value tracking for constructor applications and lambdas */
export type ExtendedValue =
  | { kind: "const"; value: ConstValue }
  | { kind: "con"; name: string; args: IR.Atom[] }
  | { kind: "tuple"; elements: IR.Atom[] }
  | { kind: "atom"; atom: IR.Atom }
  | { kind: "lambda"; param: Name; body: IR.IRExpr; type: Type };

export type ExtendedEnv = Map<string, ExtendedValue>;

export type InlineEnv = Map<string, IR.Atom>;

export type RenameEnv = Map<string, Name>;

// =============================================================================
// Literal Helpers
// =============================================================================

/** Extract constant value from a Literal */
export const literalToConst = (lit: Literal): ConstValue => {
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

/** Extract extended value to track from a binding */
export const getExtendedValueFromBinding = (binding: IR.IRBinding): ExtendedValue | null => {
  switch (binding.kind) {
    case "IRBAtom":
      if (binding.atom.kind === "ALit") {
        return { kind: "const", value: literalToConst(binding.atom.value) };
      }
      return { kind: "atom", atom: binding.atom };

    case "IRBApp":
      // Track constructor applications: Just(5) -> { kind: "con", name: "Just", args: [5] }
      if (binding.func.kind === "ACon") {
        return { kind: "con", name: binding.func.name, args: [binding.arg] };
      }
      return null;

    case "IRBTuple":
      return { kind: "tuple", elements: binding.elements as IR.Atom[] };

    case "IRBLambda":
      // Track lambdas for beta reduction
      return { kind: "lambda", param: binding.param, body: binding.body, type: binding.type };

    default:
      return null;
  }
};

/** Convert ExtendedEnv to ConstEnv for binary folding */
export const toConstEnv = (extEnv: ExtendedEnv): ConstEnv => {
  const constEnv: ConstEnv = new Map();
  for (const [k, v] of extEnv) {
    if (v.kind === "const") {
      constEnv.set(k, v.value);
    }
  }
  return constEnv;
};

// =============================================================================
// Free Variables
// =============================================================================

/** Collect free variables in an expression */
export const freeVars = (expr: IR.IRExpr): Set<string> => {
  const result = new Set<string>();
  collectFreeVars(expr, new Set(), result);
  return result;
};

export const collectFreeVars = (expr: IR.IRExpr, bound: Set<string>, free: Set<string>): void => {
  switch (expr.kind) {
    case "IRAtom":
      collectFreeVarsAtom(expr.atom, bound, free);
      break;

    case "IRLet": {
      collectFreeVarsBinding(expr.binding, bound, free);
      const newBound = new Set(bound);
      newBound.add(nameKey(expr.name));
      collectFreeVars(expr.body, newBound, free);
      break;
    }

    case "IRLetRec": {
      const newBound = new Set(bound);
      for (const b of expr.bindings) {
        newBound.add(nameKey(b.name));
      }
      for (const b of expr.bindings) {
        collectFreeVarsBinding(b.binding, newBound, free);
      }
      collectFreeVars(expr.body, newBound, free);
      break;
    }

    case "IRMatch":
      collectFreeVarsAtom(expr.scrutinee, bound, free);
      for (const c of expr.cases) {
        const patternBound = new Set(bound);
        collectPatternBound(c.pattern, patternBound);
        if (c.guard) collectFreeVars(c.guard, patternBound, free);
        collectFreeVars(c.body, patternBound, free);
      }
      break;
  }
};

const collectFreeVarsAtom = (atom: IR.Atom, bound: Set<string>, free: Set<string>): void => {
  if (atom.kind === "AVar") {
    const key = nameKey(atom.name);
    if (!bound.has(key)) {
      free.add(key);
    }
  }
};

const collectFreeVarsBinding = (
  binding: IR.IRBinding,
  bound: Set<string>,
  free: Set<string>,
): void => {
  switch (binding.kind) {
    case "IRBAtom":
      collectFreeVarsAtom(binding.atom, bound, free);
      break;

    case "IRBApp":
      collectFreeVarsAtom(binding.func, bound, free);
      collectFreeVarsAtom(binding.arg, bound, free);
      break;

    case "IRBBinOp":
      collectFreeVarsAtom(binding.left, bound, free);
      collectFreeVarsAtom(binding.right, bound, free);
      break;

    case "IRBTuple":
      for (const elem of binding.elements) {
        collectFreeVarsAtom(elem, bound, free);
      }
      break;

    case "IRBRecord":
      for (const f of binding.fields) {
        collectFreeVarsAtom(f.value, bound, free);
      }
      break;

    case "IRBRecordUpdate":
      collectFreeVarsAtom(binding.record, bound, free);
      for (const f of binding.fields) {
        collectFreeVarsAtom(f.value, bound, free);
      }
      break;

    case "IRBField":
      collectFreeVarsAtom(binding.record, bound, free);
      break;

    case "IRBLambda": {
      const newBound = new Set(bound);
      newBound.add(nameKey(binding.param));
      collectFreeVars(binding.body, newBound, free);
      break;
    }

    case "IRBForeign":
      for (const arg of binding.args) {
        collectFreeVarsAtom(arg, bound, free);
      }
      break;

    case "IRBMatch":
      collectFreeVarsAtom(binding.scrutinee, bound, free);
      for (const c of binding.cases) {
        const patternBound = new Set(bound);
        collectPatternBound(c.pattern, patternBound);
        if (c.guard) collectFreeVars(c.guard, patternBound, free);
        collectFreeVars(c.body, patternBound, free);
      }
      break;
  }
};

export const collectPatternBound = (pattern: IR.IRPattern, bound: Set<string>): void => {
  switch (pattern.kind) {
    case "IRPWild":
    case "IRPLit":
      break;

    case "IRPVar":
      bound.add(nameKey(pattern.name));
      break;

    case "IRPCon":
      for (const arg of pattern.args) {
        collectPatternBound(arg, bound);
      }
      break;

    case "IRPTuple":
      for (const elem of pattern.elements) {
        collectPatternBound(elem, bound);
      }
      break;

    case "IRPRecord":
      for (const f of pattern.fields) {
        collectPatternBound(f.pattern, bound);
      }
      break;

    case "IRPAs":
      bound.add(nameKey(pattern.name));
      collectPatternBound(pattern.pattern, bound);
      break;
  }
};

// =============================================================================
// Side Effect Detection
// =============================================================================

/** Check if a binding contains foreign calls (side effects) */
export const hasSideEffects = (binding: IR.IRBinding): boolean => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
      return false;

    // Applications are conservatively assumed to potentially have side effects
    // since we can't statically determine if the function being called is a
    // foreign function with side effects (like IO.printLine)
    case "IRBApp":
    case "IRBForeign":
      return true;

    case "IRBLambda":
      return exprHasSideEffects(binding.body);

    case "IRBMatch":
      return binding.cases.some(
        (c) => (c.guard && exprHasSideEffects(c.guard)) || exprHasSideEffects(c.body),
      );
  }
};

const exprHasSideEffects = (expr: IR.IRExpr): boolean => {
  switch (expr.kind) {
    case "IRAtom":
      return false;

    case "IRLet":
      return hasSideEffects(expr.binding) || exprHasSideEffects(expr.body);

    case "IRLetRec":
      return expr.bindings.some((b) => hasSideEffects(b.binding)) || exprHasSideEffects(expr.body);

    case "IRMatch":
      return expr.cases.some(
        (c) => (c.guard && exprHasSideEffects(c.guard)) || exprHasSideEffects(c.body),
      );
  }
};
