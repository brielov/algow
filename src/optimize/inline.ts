/**
 * Inlining Optimizations
 *
 * - Trivial binding inlining (let x = y → substitute y for x)
 * - Function inlining (inline small or single-use functions)
 */

import * as IR from "../ir";
import type { Name } from "../core";
import type { Span } from "../surface";
import {
  type InlineEnv,
  type RenameEnv,
  nameKey,
  collectFreeVars,
  hasSideEffects,
  bindResultTo,
} from "./types";
import { alphaRenameExpr } from "./alpha";

/**
 * Counter for generating fresh names during inlining.
 * Uses negative IDs to avoid collision with resolved names.
 * These Names are never used for type lookup (optimization happens after
 * type checking and types are already embedded in IR nodes).
 */
let inlineCounter = 0;
const syntheticSpan: Span = { fileId: 0, start: 0, end: 0 };
const freshName = (text: string, span: Span): Name => {
  const id = --inlineCounter;
  return { id, nodeId: id, text, span: span ?? syntheticSpan };
};

// =============================================================================
// Trivial Binding Inlining
// =============================================================================

/** Inline trivial atom bindings (let x = y in ... → substitute y for x) */
export const inlineExpr = (expr: IR.IRExpr, env: InlineEnv): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom": {
      if (expr.atom.kind === "AVar") {
        const inlined = env.get(nameKey(expr.atom.name));
        if (inlined) return IR.iratom(inlined);
      }
      return expr;
    }

    case "IRLet": {
      const newBinding = inlineBinding(expr.binding, env);

      // If binding is just an atom, add to inline env
      if (newBinding.kind === "IRBAtom") {
        const newEnv = new Map(env);
        newEnv.set(nameKey(expr.name), newBinding.atom);
        return inlineExpr(expr.body, newEnv);
      }

      const newBody = inlineExpr(expr.body, env);
      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: inlineBinding(b.binding, env),
      }));
      return IR.irletrec(newBindings, inlineExpr(expr.body, env));
    }

    case "IRMatch": {
      const newScrutinee = inlineAtom(expr.scrutinee, env);
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineExpr(c.guard, env) : null,
        body: inlineExpr(c.body, env),
      }));
      return IR.irmatch(newScrutinee, cases, expr.type);
    }
  }
};

const inlineAtom = (atom: IR.Atom, env: InlineEnv): IR.Atom => {
  if (atom.kind === "AVar") {
    const inlined = env.get(nameKey(atom.name));
    if (inlined) return inlined;
  }
  return atom;
};

export const inlineBinding = (binding: IR.IRBinding, env: InlineEnv): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
      return IR.irbatom(inlineAtom(binding.atom, env));

    case "IRBApp":
      return IR.irbapp(inlineAtom(binding.func, env), inlineAtom(binding.arg, env), binding.type);

    case "IRBBinOp":
      return IR.irbbinop(
        binding.op,
        inlineAtom(binding.left, env),
        inlineAtom(binding.right, env),
        binding.type,
      );

    case "IRBTuple":
      return IR.irbtuple(
        binding.elements.map((e) => inlineAtom(e, env)),
        binding.type,
      );

    case "IRBRecord":
      return IR.irbrecord(
        binding.fields.map((f) => ({ name: f.name, value: inlineAtom(f.value, env) })),
        binding.type,
      );

    case "IRBRecordUpdate":
      return IR.irbrecordupdate(
        inlineAtom(binding.record, env),
        binding.fields.map((f) => ({ name: f.name, value: inlineAtom(f.value, env) })),
        binding.type,
      );

    case "IRBField":
      return IR.irbfield(inlineAtom(binding.record, env), binding.field, binding.type);

    case "IRBLambda":
      return IR.irblambda(binding.param, inlineExpr(binding.body, env), binding.type);

    case "IRBForeign":
      return IR.irbforeign(
        binding.module,
        binding.name,
        binding.args.map((a) => inlineAtom(a, env)),
        binding.type,
        binding.isAsync,
      );

    case "IRBMatch": {
      const newScrutinee = inlineAtom(binding.scrutinee, env);
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineExpr(c.guard, env) : null,
        body: inlineExpr(c.body, env),
      }));
      return IR.irbmatch(newScrutinee, cases, binding.type);
    }
  }
};

// =============================================================================
// Function Inlining
// =============================================================================

/** Maximum body size for unconditional inlining */
const MAX_INLINE_SIZE = 15;

type UseCounts = {
  uses: Map<string, number>;
  calls: Map<string, number>;
};

/** Count how many times each variable is used and called in an expression */
const countUses = (expr: IR.IRExpr): UseCounts => {
  const uses = new Map<string, number>();
  const calls = new Map<string, number>();
  countUsesExpr(expr, uses, calls);
  return { uses, calls };
};

const countUsesExpr = (
  expr: IR.IRExpr,
  uses: Map<string, number>,
  calls: Map<string, number>,
): void => {
  switch (expr.kind) {
    case "IRAtom":
      countUsesAtom(expr.atom, uses);
      break;

    case "IRLet":
      countUsesBinding(expr.binding, uses, calls);
      countUsesExpr(expr.body, uses, calls);
      break;

    case "IRLetRec":
      for (const b of expr.bindings) {
        countUsesBinding(b.binding, uses, calls);
      }
      countUsesExpr(expr.body, uses, calls);
      break;

    case "IRMatch":
      countUsesAtom(expr.scrutinee, uses);
      for (const c of expr.cases) {
        if (c.guard) countUsesExpr(c.guard, uses, calls);
        countUsesExpr(c.body, uses, calls);
      }
      break;
  }
};

const countUsesAtom = (atom: IR.Atom, uses: Map<string, number>): void => {
  if (atom.kind === "AVar") {
    const key = nameKey(atom.name);
    uses.set(key, (uses.get(key) ?? 0) + 1);
  }
};

const countUsesBinding = (
  binding: IR.IRBinding,
  uses: Map<string, number>,
  calls: Map<string, number>,
): void => {
  switch (binding.kind) {
    case "IRBAtom":
      countUsesAtom(binding.atom, uses);
      break;

    case "IRBApp":
      countUsesAtom(binding.func, uses);
      if (binding.func.kind === "AVar") {
        const key = nameKey(binding.func.name);
        calls.set(key, (calls.get(key) ?? 0) + 1);
      }
      countUsesAtom(binding.arg, uses);
      break;

    case "IRBBinOp":
      countUsesAtom(binding.left, uses);
      countUsesAtom(binding.right, uses);
      break;

    case "IRBTuple":
      for (const e of binding.elements) countUsesAtom(e, uses);
      break;

    case "IRBRecord":
      for (const f of binding.fields) countUsesAtom(f.value, uses);
      break;

    case "IRBRecordUpdate":
      countUsesAtom(binding.record, uses);
      for (const f of binding.fields) countUsesAtom(f.value, uses);
      break;

    case "IRBField":
      countUsesAtom(binding.record, uses);
      break;

    case "IRBLambda":
      countUsesExpr(binding.body, uses, calls);
      break;

    case "IRBForeign":
      for (const a of binding.args) countUsesAtom(a, uses);
      break;

    case "IRBMatch":
      countUsesAtom(binding.scrutinee, uses);
      for (const c of binding.cases) {
        if (c.guard) countUsesExpr(c.guard, uses, calls);
        countUsesExpr(c.body, uses, calls);
      }
      break;
  }
};

/** Check if a lambda body references its own name (recursive) */
const isRecursive = (funcName: Name, binding: IR.IRBinding): boolean => {
  if (binding.kind !== "IRBLambda") return false;
  const fv = new Set<string>();
  collectFreeVars(binding.body, new Set([nameKey(binding.param)]), fv);
  return fv.has(nameKey(funcName));
};

type FunctionEnv = Map<string, { binding: IR.IRBLambda }>;

/** Inline functions that are used once or have small bodies. */
export const inlineFunctions = (expr: IR.IRExpr): IR.IRExpr => {
  const counts = countUses(expr);
  return inlineFunctionsExpr(expr, new Map(), counts);
};

const inlineFunctionsExpr = (expr: IR.IRExpr, env: FunctionEnv, counts: UseCounts): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const key = nameKey(expr.name);
      const useCount = counts.uses.get(key) ?? 0;
      const callCount = counts.calls.get(key) ?? 0;

      if (expr.binding.kind === "IRBLambda") {
        const lambda = expr.binding;
        const size = bindingSizeForInline(lambda);
        const recursive = isRecursive(expr.name, lambda);
        const sideEffects = hasSideEffects(lambda);

        const allUsesAreCalls = useCount === callCount;
        const shouldInline =
          !recursive && !sideEffects && (useCount === 1 || size <= MAX_INLINE_SIZE);

        if (shouldInline) {
          const newEnv = new Map(env);
          newEnv.set(key, { binding: lambda });
          const newBody = inlineFunctionsExpr(expr.body, newEnv, counts);

          if (useCount === 1 && allUsesAreCalls) {
            return newBody;
          }

          const processedLambda = inlineFunctionsBindingInner(lambda, env, counts);
          return IR.irlet(expr.name, processedLambda, newBody);
        }
      }

      // Try to inline the binding if it's an application
      const bindingResult = inlineFunctionsBindingWithResult(expr.binding, env, counts);
      const newBody = inlineFunctionsExpr(expr.body, env, counts);

      if (bindingResult.kind === "inlined") {
        // Transform: let x = f(arg) in body
        // Into: let param = arg in (bindResultTo inlinedBody x body)
        // This uses canonical IRLet instead of IRBMatch
        const { paramName, argAtom, body: inlinedBody } = bindingResult;
        const boundBody = bindResultTo(inlinedBody, expr.name, newBody);
        return IR.irlet(paramName, IR.irbatom(argAtom), boundBody);
      }

      return IR.irlet(expr.name, bindingResult.binding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: inlineFunctionsBindingInner(b.binding, env, counts),
      }));
      const newBody = inlineFunctionsExpr(expr.body, env, counts);
      return IR.irletrec(newBindings, newBody);
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineFunctionsExpr(c.guard, env, counts) : null,
        body: inlineFunctionsExpr(c.body, env, counts),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

/**
 * Result of inlining a function application.
 * When inlining happens, we need to return the parameter binding separately
 * so the caller can create canonical `let param = arg in ...` structure.
 */
type InlineBindingResult =
  | { kind: "unchanged"; binding: IR.IRBinding }
  | { kind: "inlined"; paramName: Name; argAtom: IR.Atom; body: IR.IRExpr };

/**
 * Process a binding without inlining applications.
 * Used for lambda bindings and letrec where inlining at the binding level is not appropriate.
 */
const inlineFunctionsBindingInner = (
  binding: IR.IRBinding,
  env: FunctionEnv,
  counts: UseCounts,
): IR.IRBinding => {
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
      return IR.irblambda(
        binding.param,
        inlineFunctionsExpr(binding.body, env, counts),
        binding.type,
      );

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineFunctionsExpr(c.guard, env, counts) : null,
        body: inlineFunctionsExpr(c.body, env, counts),
      }));
      return IR.irbmatch(binding.scrutinee, cases, binding.type);
    }
  }
};

const inlineFunctionsBindingWithResult = (
  binding: IR.IRBinding,
  env: FunctionEnv,
  counts: UseCounts,
): InlineBindingResult => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return { kind: "unchanged", binding };

    case "IRBApp": {
      if (binding.func.kind === "AVar") {
        const funcInfo = env.get(nameKey(binding.func.name));
        if (funcInfo) {
          const lambda = funcInfo.binding;
          const freshParam = freshName(lambda.param.text, lambda.param.span);
          const renameEnv: RenameEnv = new Map();
          renameEnv.set(nameKey(lambda.param), freshParam);
          const renamedBody = alphaRenameExpr(lambda.body, renameEnv);

          // Return inlined result with parameter binding info
          // Caller will create canonical: let param = arg in body
          return {
            kind: "inlined",
            paramName: freshParam,
            argAtom: binding.arg,
            body: inlineFunctionsExpr(renamedBody, env, counts),
          };
        }
      }
      return { kind: "unchanged", binding };
    }

    case "IRBLambda":
      return {
        kind: "unchanged",
        binding: IR.irblambda(
          binding.param,
          inlineFunctionsExpr(binding.body, env, counts),
          binding.type,
        ),
      };

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineFunctionsExpr(c.guard, env, counts) : null,
        body: inlineFunctionsExpr(c.body, env, counts),
      }));
      return { kind: "unchanged", binding: IR.irbmatch(binding.scrutinee, cases, binding.type) };
    }
  }
};

// =============================================================================
// Size Calculation for Inlining Decisions
// =============================================================================

const bindingSizeForInline = (binding: IR.IRBinding): number => {
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
      return 1 + exprSizeForInline(binding.body);
    case "IRBForeign":
      return 5 + binding.args.length;
    case "IRBMatch":
      return (
        3 +
        binding.cases.reduce(
          (acc, c) => acc + exprSizeForInline(c.body) + (c.guard ? exprSizeForInline(c.guard) : 0),
          0,
        )
      );
  }
};

const exprSizeForInline = (expr: IR.IRExpr): number => {
  switch (expr.kind) {
    case "IRAtom":
      return 1;
    case "IRLet":
      return 1 + bindingSizeForInline(expr.binding) + exprSizeForInline(expr.body);
    case "IRLetRec":
      return (
        1 +
        expr.bindings.reduce((acc, b) => acc + bindingSizeForInline(b.binding), 0) +
        exprSizeForInline(expr.body)
      );
    case "IRMatch":
      return (
        1 +
        expr.cases.reduce(
          (acc, c) => acc + exprSizeForInline(c.body) + (c.guard ? exprSizeForInline(c.guard) : 0),
          0,
        )
      );
  }
};
