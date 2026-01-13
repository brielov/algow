/**
 * Type Unification
 *
 * Implementation of the unification algorithm for type inference.
 */

import type { Span } from "../surface";
import { typeMismatch } from "../diagnostics";
import {
  type Type,
  type Subst,
  trecord,
  applySubst,
  composeSubst,
  ftv,
  typeToString,
} from "../types";
import { type CheckContext, addError } from "./context";

// =============================================================================
// Unification (Section 8.4)
// =============================================================================

export const unify = (ctx: CheckContext, t1: Type, t2: Type, span?: Span): Subst => {
  if (t1.kind === "TVar" && t2.kind === "TVar" && t1.name === t2.name) {
    return new Map();
  }

  if (t1.kind === "TVar") {
    return bindVar(ctx, t1.name, t2);
  }

  if (t2.kind === "TVar") {
    return bindVar(ctx, t2.name, t1);
  }

  if (t1.kind === "TCon" && t2.kind === "TCon" && t1.name === t2.name) {
    return new Map();
  }

  if (t1.kind === "TFun" && t2.kind === "TFun") {
    const s1 = unify(ctx, t1.param, t2.param, span);
    const s2 = unify(ctx, applySubst(s1, t1.ret), applySubst(s1, t2.ret), span);
    return composeSubst(s1, s2);
  }

  if (t1.kind === "TApp" && t2.kind === "TApp") {
    const s1 = unify(ctx, t1.con, t2.con, span);
    const s2 = unify(ctx, applySubst(s1, t1.arg), applySubst(s1, t2.arg), span);
    return composeSubst(s1, s2);
  }

  if (t1.kind === "TRecord" && t2.kind === "TRecord") {
    return unifyRecords(ctx, t1, t2, span);
  }

  if (t1.kind === "TTuple" && t2.kind === "TTuple") {
    if (t1.elements.length !== t2.elements.length) {
      addError(ctx, `Tuple arity mismatch: ${t1.elements.length} vs ${t2.elements.length}`, span);
      return new Map();
    }
    let subst: Subst = new Map();
    for (let i = 0; i < t1.elements.length; i++) {
      const s = unify(
        ctx,
        applySubst(subst, t1.elements[i]!),
        applySubst(subst, t2.elements[i]!),
        span,
      );
      subst = composeSubst(subst, s);
    }
    return subst;
  }

  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(typeMismatch(start, end, typeToString(t1), typeToString(t2)));
  return new Map();
};

const bindVar = (ctx: CheckContext, name: string, type: Type): Subst => {
  if (type.kind === "TVar" && type.name === name) {
    return new Map();
  }
  if (ftv(type).has(name)) {
    addError(ctx, `Infinite type: ${name} occurs in ${typeToString(type)}`);
    return new Map();
  }
  return new Map([[name, type]]);
};

const unifyRecords = (
  ctx: CheckContext,
  t1: { kind: "TRecord"; fields: ReadonlyMap<string, Type>; row: Type | null },
  t2: { kind: "TRecord"; fields: ReadonlyMap<string, Type>; row: Type | null },
  span?: Span,
): Subst => {
  let subst: Subst = new Map();

  // Unify common fields
  for (const [name, type1] of t1.fields) {
    const type2 = t2.fields.get(name);
    if (type2) {
      const s = unify(ctx, applySubst(subst, type1), applySubst(subst, type2), span);
      subst = composeSubst(subst, s);
    }
  }

  // Handle extra fields and row polymorphism
  const extra1 = [...t1.fields.entries()].filter(([n]) => !t2.fields.has(n));
  const extra2 = [...t2.fields.entries()].filter(([n]) => !t1.fields.has(n));

  if (extra1.length > 0 && t2.row) {
    const newRow = t1.row ?? null;
    const extraRecord = trecord(
      extra1.map(([n, t]) => [n, applySubst(subst, t)]),
      newRow,
    );
    const s = unify(ctx, applySubst(subst, t2.row), extraRecord, span);
    subst = composeSubst(subst, s);
  } else if (extra1.length > 0 && !t2.row) {
    addError(ctx, `Record missing fields: ${extra1.map(([n]) => n).join(", ")}`, span);
  }

  if (extra2.length > 0 && t1.row) {
    const newRow = t2.row ?? null;
    const extraRecord = trecord(
      extra2.map(([n, t]) => [n, applySubst(subst, t)]),
      newRow,
    );
    const s = unify(ctx, applySubst(subst, t1.row), extraRecord, span);
    subst = composeSubst(subst, s);
  } else if (extra2.length > 0 && !t1.row) {
    addError(ctx, `Record missing fields: ${extra2.map(([n]) => n).join(", ")}`, span);
  }

  // Unify row variables if both are open
  if (t1.row && t2.row && extra1.length === 0 && extra2.length === 0) {
    const s = unify(ctx, applySubst(subst, t1.row), applySubst(subst, t2.row), span);
    subst = composeSubst(subst, s);
  }

  return subst;
};
