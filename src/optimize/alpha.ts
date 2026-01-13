/**
 * Alpha Renaming for Safe Beta Reduction
 *
 * Renames bound variables to fresh names to avoid capture.
 */

import * as IR from "../ir";
import { freshName, type Name } from "../core";
import { type RenameEnv, nameKey } from "./types";

/** Freshen a name if it's in the rename environment, otherwise keep it */
const renameName = (name: Name, env: RenameEnv): Name => {
  const renamed = env.get(nameKey(name));
  return renamed ?? name;
};

/** Alpha-rename all bound variables in an expression with fresh names */
export const alphaRenameExpr = (expr: IR.IRExpr, env: RenameEnv): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return IR.iratom(alphaRenameAtom(expr.atom, env));

    case "IRLet": {
      const newBinding = alphaRenameBinding(expr.binding, env);
      // Create fresh name for this binding
      const freshN = freshName(expr.name.text, expr.name.span);
      const newEnv = new Map(env);
      newEnv.set(nameKey(expr.name), freshN);
      const newBody = alphaRenameExpr(expr.body, newEnv);
      return IR.irlet(freshN, newBinding, newBody);
    }

    case "IRLetRec": {
      // First, create fresh names for all bindings
      const newEnv = new Map(env);
      const freshNames: Name[] = [];
      for (const b of expr.bindings) {
        const freshN = freshName(b.name.text, b.name.span);
        freshNames.push(freshN);
        newEnv.set(nameKey(b.name), freshN);
      }
      // Then rename all bindings and body with the new env
      const newBindings = expr.bindings.map((b, i) => ({
        name: freshNames[i]!,
        binding: alphaRenameBinding(b.binding, newEnv),
      }));
      const newBody = alphaRenameExpr(expr.body, newEnv);
      return IR.irletrec(newBindings, newBody);
    }

    case "IRMatch": {
      const newScrutinee = alphaRenameAtom(expr.scrutinee, env);
      const newCases = expr.cases.map((c) => alphaRenameCase(c, env));
      return IR.irmatch(newScrutinee, newCases, expr.type);
    }
  }
};

const alphaRenameAtom = (atom: IR.Atom, env: RenameEnv): IR.Atom => {
  switch (atom.kind) {
    case "AVar":
      return IR.avar(renameName(atom.name, env), atom.type);
    case "ALit":
    case "ACon":
      return atom;
  }
};

export const alphaRenameBinding = (binding: IR.IRBinding, env: RenameEnv): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
      return IR.irbatom(alphaRenameAtom(binding.atom, env));

    case "IRBApp":
      return IR.irbapp(
        alphaRenameAtom(binding.func, env),
        alphaRenameAtom(binding.arg, env),
        binding.type,
      );

    case "IRBBinOp":
      return IR.irbbinop(
        binding.op,
        alphaRenameAtom(binding.left, env),
        alphaRenameAtom(binding.right, env),
        binding.type,
      );

    case "IRBTuple":
      return IR.irbtuple(
        binding.elements.map((e) => alphaRenameAtom(e, env)),
        binding.type,
      );

    case "IRBRecord":
      return IR.irbrecord(
        binding.fields.map((f) => ({ name: f.name, value: alphaRenameAtom(f.value, env) })),
        binding.type,
      );

    case "IRBRecordUpdate":
      return IR.irbrecordupdate(
        alphaRenameAtom(binding.record, env),
        binding.fields.map((f) => ({ name: f.name, value: alphaRenameAtom(f.value, env) })),
        binding.type,
      );

    case "IRBField":
      return IR.irbfield(alphaRenameAtom(binding.record, env), binding.field, binding.type);

    case "IRBLambda": {
      // Create fresh name for lambda parameter
      const freshParam = freshName(binding.param.text, binding.param.span);
      const newEnv = new Map(env);
      newEnv.set(nameKey(binding.param), freshParam);
      const newBody = alphaRenameExpr(binding.body, newEnv);
      return IR.irblambda(freshParam, newBody, binding.type);
    }

    case "IRBForeign":
      return IR.irbforeign(
        binding.module,
        binding.name,
        binding.args.map((a) => alphaRenameAtom(a, env)),
        binding.type,
        binding.isAsync,
      );

    case "IRBMatch": {
      const newScrutinee = alphaRenameAtom(binding.scrutinee, env);
      const newCases = binding.cases.map((c) => alphaRenameCase(c, env));
      return IR.irbmatch(newScrutinee, newCases, binding.type);
    }
  }
};

const alphaRenameCase = (c: IR.IRCase, env: RenameEnv): IR.IRCase => {
  // Pattern binds new variables - freshen them
  const patternEnv = new Map(env);
  const newPattern = alphaRenamePattern(c.pattern, patternEnv);
  const newGuard = c.guard ? alphaRenameExpr(c.guard, patternEnv) : null;
  const newBody = alphaRenameExpr(c.body, patternEnv);
  return { pattern: newPattern, guard: newGuard, body: newBody };
};

const alphaRenamePattern = (pattern: IR.IRPattern, env: RenameEnv): IR.IRPattern => {
  switch (pattern.kind) {
    case "IRPWild":
    case "IRPLit":
      return pattern;

    case "IRPVar": {
      const freshN = freshName(pattern.name.text, pattern.name.span);
      env.set(nameKey(pattern.name), freshN);
      return IR.irpvar(freshN, pattern.type);
    }

    case "IRPCon":
      return IR.irpcon(
        pattern.name,
        pattern.args.map((p) => alphaRenamePattern(p, env)),
        pattern.type,
      );

    case "IRPTuple":
      return IR.irptuple(pattern.elements.map((p) => alphaRenamePattern(p, env)));

    case "IRPRecord":
      return IR.irprecord(
        pattern.fields.map((f) => ({ name: f.name, pattern: alphaRenamePattern(f.pattern, env) })),
      );

    case "IRPAs": {
      const freshN = freshName(pattern.name.text, pattern.name.span);
      env.set(nameKey(pattern.name), freshN);
      const newSubPattern = alphaRenamePattern(pattern.pattern, env);
      return IR.irpas(freshN, newSubPattern, pattern.type);
    }
  }
};
