/**
 * Dead Code Elimination
 *
 * Removes unused bindings from expressions.
 */

import * as IR from "../ir";
import { nameKey, hasSideEffects } from "./types";

// =============================================================================
// Use Collection
// =============================================================================

export const collectUses = (expr: IR.IRExpr, uses: Set<string>): void => {
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

// =============================================================================
// Remove Unused Bindings
// =============================================================================

export const removeUnused = (expr: IR.IRExpr, uses: Set<string>): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBinding = removeUnusedBinding(expr.binding, uses);
      const newBody = removeUnused(expr.body, uses);

      // Remove if not used AND doesn't have side effects
      if (!uses.has(nameKey(expr.name)) && !hasSideEffects(expr.binding)) {
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
