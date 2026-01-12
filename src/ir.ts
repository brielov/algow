/**
 * Intermediate Representation (Section 10)
 *
 * A-Normal Form (ANF) IR where all intermediate values are named.
 * All arguments to operations are atoms (trivial expressions).
 */

import type { Literal } from "./surface";
import type { Name } from "./core";
import type { Type } from "./types";

// =============================================================================
// Atoms (Section 10.1)
// =============================================================================

export type Atom = AVar | ALit | ACon;

export type AVar = {
  readonly kind: "AVar";
  readonly name: Name;
  readonly type: Type;
};

export type ALit = {
  readonly kind: "ALit";
  readonly value: Literal;
  readonly type: Type;
};

export type ACon = {
  readonly kind: "ACon";
  readonly name: string;
  readonly type: Type;
};

// Atom constructors
export const avar = (name: Name, type: Type): AVar => ({ kind: "AVar", name, type });
export const alit = (value: Literal, type: Type): ALit => ({ kind: "ALit", value, type });
export const acon = (name: string, type: Type): ACon => ({ kind: "ACon", name, type });

// =============================================================================
// IR Expressions (Section 10.1)
// =============================================================================

export type IRExpr = IRAtom | IRLet | IRLetRec | IRMatch;

export type IRAtom = {
  readonly kind: "IRAtom";
  readonly atom: Atom;
};

export type IRLet = {
  readonly kind: "IRLet";
  readonly name: Name;
  readonly binding: IRBinding;
  readonly body: IRExpr;
};

export type IRLetRec = {
  readonly kind: "IRLetRec";
  readonly bindings: readonly { readonly name: Name; readonly binding: IRBinding }[];
  readonly body: IRExpr;
};

export type IRMatch = {
  readonly kind: "IRMatch";
  readonly scrutinee: Atom;
  readonly cases: readonly IRCase[];
  readonly type: Type;
};

// Expression constructors
export const iratom = (atom: Atom): IRAtom => ({ kind: "IRAtom", atom });
export const irlet = (name: Name, binding: IRBinding, body: IRExpr): IRLet => ({
  kind: "IRLet",
  name,
  binding,
  body,
});
export const irletrec = (
  bindings: readonly { name: Name; binding: IRBinding }[],
  body: IRExpr,
): IRLetRec => ({ kind: "IRLetRec", bindings, body });
export const irmatch = (scrutinee: Atom, cases: readonly IRCase[], type: Type): IRMatch => ({
  kind: "IRMatch",
  scrutinee,
  cases,
  type,
});

// =============================================================================
// IR Bindings (Section 10.1)
// =============================================================================

export type IRBinding =
  | IRBAtom
  | IRBApp
  | IRBBinOp
  | IRBTuple
  | IRBRecord
  | IRBRecordUpdate
  | IRBField
  | IRBLambda
  | IRBForeign
  | IRBMatch;

export type IRBAtom = {
  readonly kind: "IRBAtom";
  readonly atom: Atom;
};

export type IRBApp = {
  readonly kind: "IRBApp";
  readonly func: Atom;
  readonly arg: Atom;
  readonly type: Type;
};

export type IRBBinOp = {
  readonly kind: "IRBBinOp";
  readonly op: string;
  readonly left: Atom;
  readonly right: Atom;
  readonly type: Type;
};

export type IRBTuple = {
  readonly kind: "IRBTuple";
  readonly elements: readonly Atom[];
  readonly type: Type;
};

export type IRBRecord = {
  readonly kind: "IRBRecord";
  readonly fields: readonly { readonly name: string; readonly value: Atom }[];
  readonly type: Type;
};

export type IRBRecordUpdate = {
  readonly kind: "IRBRecordUpdate";
  readonly record: Atom;
  readonly fields: readonly { readonly name: string; readonly value: Atom }[];
  readonly type: Type;
};

export type IRBField = {
  readonly kind: "IRBField";
  readonly record: Atom;
  readonly field: string;
  readonly type: Type;
};

export type IRBLambda = {
  readonly kind: "IRBLambda";
  readonly param: Name;
  readonly body: IRExpr;
  readonly type: Type;
  // Tail recursion info (filled during optimization)
  readonly tailRecursive?: {
    readonly funcName: Name;
    readonly params: readonly Name[];
  };
};

export type IRBForeign = {
  readonly kind: "IRBForeign";
  readonly module: string;
  readonly name: string;
  readonly args: readonly Atom[];
  readonly type: Type;
};

export type IRBMatch = {
  readonly kind: "IRBMatch";
  readonly scrutinee: Atom;
  readonly cases: readonly IRCase[];
  readonly type: Type;
};

// Binding constructors
export const irbatom = (atom: Atom): IRBAtom => ({ kind: "IRBAtom", atom });
export const irbapp = (func: Atom, arg: Atom, type: Type): IRBApp => ({
  kind: "IRBApp",
  func,
  arg,
  type,
});
export const irbbinop = (op: string, left: Atom, right: Atom, type: Type): IRBBinOp => ({
  kind: "IRBBinOp",
  op,
  left,
  right,
  type,
});
export const irbtuple = (elements: readonly Atom[], type: Type): IRBTuple => ({
  kind: "IRBTuple",
  elements,
  type,
});
export const irbrecord = (
  fields: readonly { name: string; value: Atom }[],
  type: Type,
): IRBRecord => ({ kind: "IRBRecord", fields, type });
export const irbrecordupdate = (
  record: Atom,
  fields: readonly { name: string; value: Atom }[],
  type: Type,
): IRBRecordUpdate => ({ kind: "IRBRecordUpdate", record, fields, type });
export const irbfield = (record: Atom, field: string, type: Type): IRBField => ({
  kind: "IRBField",
  record,
  field,
  type,
});
export const irblambda = (param: Name, body: IRExpr, type: Type): IRBLambda => ({
  kind: "IRBLambda",
  param,
  body,
  type,
});
export const irbforeign = (
  module: string,
  name: string,
  args: readonly Atom[],
  type: Type,
): IRBForeign => ({ kind: "IRBForeign", module, name, args, type });

export const irbmatch = (scrutinee: Atom, cases: readonly IRCase[], type: Type): IRBMatch => ({
  kind: "IRBMatch",
  scrutinee,
  cases,
  type,
});

// =============================================================================
// IR Patterns (Section 10.1)
// =============================================================================

export type IRPattern = IRPWild | IRPVar | IRPLit | IRPCon | IRPTuple | IRPRecord | IRPAs;

export type IRPWild = {
  readonly kind: "IRPWild";
};

export type IRPVar = {
  readonly kind: "IRPVar";
  readonly name: Name;
  readonly type: Type;
};

export type IRPLit = {
  readonly kind: "IRPLit";
  readonly value: Literal;
};

export type IRPCon = {
  readonly kind: "IRPCon";
  readonly name: string;
  readonly args: readonly IRPattern[];
  readonly type: Type;
};

export type IRPTuple = {
  readonly kind: "IRPTuple";
  readonly elements: readonly IRPattern[];
};

export type IRPRecord = {
  readonly kind: "IRPRecord";
  readonly fields: readonly { readonly name: string; readonly pattern: IRPattern }[];
};

export type IRPAs = {
  readonly kind: "IRPAs";
  readonly name: Name;
  readonly pattern: IRPattern;
  readonly type: Type;
};

// Pattern constructors
export const irpwild = (): IRPWild => ({ kind: "IRPWild" });
export const irpvar = (name: Name, type: Type): IRPVar => ({ kind: "IRPVar", name, type });
export const irplit = (value: Literal): IRPLit => ({ kind: "IRPLit", value });
export const irpcon = (name: string, args: readonly IRPattern[], type: Type): IRPCon => ({
  kind: "IRPCon",
  name,
  args,
  type,
});
export const irptuple = (elements: readonly IRPattern[]): IRPTuple => ({
  kind: "IRPTuple",
  elements,
});
export const irprecord = (fields: readonly { name: string; pattern: IRPattern }[]): IRPRecord => ({
  kind: "IRPRecord",
  fields,
});
export const irpas = (name: Name, pattern: IRPattern, type: Type): IRPAs => ({
  kind: "IRPAs",
  name,
  pattern,
  type,
});

// =============================================================================
// IR Cases
// =============================================================================

export type IRCase = {
  readonly pattern: IRPattern;
  readonly guard: IRExpr | null;
  readonly body: IRExpr;
};

// =============================================================================
// IR Program
// =============================================================================

export type IRProgram = {
  readonly decls: readonly IRDecl[];
  readonly main: IRExpr | null;
};

export type IRDecl = IRDeclType | IRDeclLet | IRDeclLetRec;

export type IRDeclType = {
  readonly kind: "IRDeclType";
  readonly name: string;
  readonly constructors: readonly {
    readonly name: string;
    readonly tag: number;
    readonly arity: number;
  }[];
};

export type IRDeclLet = {
  readonly kind: "IRDeclLet";
  readonly name: Name;
  readonly binding: IRBinding;
};

export type IRDeclLetRec = {
  readonly kind: "IRDeclLetRec";
  readonly bindings: readonly { readonly name: Name; readonly binding: IRBinding }[];
};

// Declaration constructors
export const irdecltype = (
  name: string,
  constructors: readonly { name: string; tag: number; arity: number }[],
): IRDeclType => ({ kind: "IRDeclType", name, constructors });
export const irdecllet = (name: Name, binding: IRBinding): IRDeclLet => ({
  kind: "IRDeclLet",
  name,
  binding,
});
export const irdeclletrec = (
  bindings: readonly { name: Name; binding: IRBinding }[],
): IRDeclLetRec => ({ kind: "IRDeclLetRec", bindings });

// =============================================================================
// Utilities
// =============================================================================

export const atomType = (atom: Atom): Type => atom.type;

export const exprType = (expr: IRExpr): Type => {
  switch (expr.kind) {
    case "IRAtom":
      return atomType(expr.atom);
    case "IRLet":
      return exprType(expr.body);
    case "IRLetRec":
      return exprType(expr.body);
    case "IRMatch":
      return expr.type;
  }
};

export const bindingType = (binding: IRBinding): Type => {
  switch (binding.kind) {
    case "IRBAtom":
      return atomType(binding.atom);
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBLambda":
    case "IRBForeign":
    case "IRBMatch":
      return binding.type;
  }
};
