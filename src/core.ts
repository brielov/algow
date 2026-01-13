// Core AST - minimal, canonical representation after desugaring
// All surface sugar is removed. Names have unique IDs.

import type { Literal, Span } from "./surface";

// =============================================================================
// Names (Section 5.1)
// =============================================================================

// Unique identifier for bindings
export type Name = {
  readonly id: number;
  readonly original: string;
};

// Helper to create names (used by resolve.ts)
let _nextId = 0;
export function resetNameCounter(): void {
  _nextId = 0;
}
export function freshName(original: string): Name {
  return { id: _nextId++, original };
}

// =============================================================================
// Core Expressions (Section 5.2)
// =============================================================================

export type CExpr =
  | CVar
  | CLit
  | CApp
  | CAbs
  | CLet
  | CLetRec
  | CMatch
  | CCon
  | CTuple
  | CRecord
  | CRecordUpdate
  | CField
  | CForeign
  | CBinOp;

export type CVar = {
  readonly kind: "CVar";
  readonly name: Name;
  readonly span?: Span;
  // For qualified names (Module.member), track the spans separately
  readonly moduleSpan?: Span;
  readonly memberSpan?: Span;
};

export type CLit = {
  readonly kind: "CLit";
  readonly value: Literal;
  readonly span?: Span;
};

export type CApp = {
  readonly kind: "CApp";
  readonly func: CExpr;
  readonly arg: CExpr;
  readonly span?: Span;
};

// Single-param lambda (multi-param desugared to nested)
export type CAbs = {
  readonly kind: "CAbs";
  readonly param: Name;
  readonly body: CExpr;
  readonly span?: Span;
  readonly paramSpan?: Span;
};

export type CLet = {
  readonly kind: "CLet";
  readonly name: Name;
  readonly value: CExpr;
  readonly body: CExpr;
  readonly span?: Span;
  readonly nameSpan?: Span;
};

export type CLetRec = {
  readonly kind: "CLetRec";
  readonly bindings: readonly {
    readonly name: Name;
    readonly value: CExpr;
    readonly nameSpan?: Span;
  }[];
  readonly body: CExpr;
  readonly span?: Span;
};

export type CMatch = {
  readonly kind: "CMatch";
  readonly scrutinee: CExpr;
  readonly cases: readonly CCase[];
  readonly span?: Span;
};

export type CCase = {
  readonly pattern: CPattern;
  readonly guard: CExpr | null;
  readonly body: CExpr;
};

export type CCon = {
  readonly kind: "CCon";
  readonly name: string;
  readonly span?: Span;
};

export type CTuple = {
  readonly kind: "CTuple";
  readonly elements: readonly CExpr[];
  readonly span?: Span;
};

export type CRecord = {
  readonly kind: "CRecord";
  readonly fields: readonly { readonly name: string; readonly value: CExpr }[];
  readonly span?: Span;
};

export type CRecordUpdate = {
  readonly kind: "CRecordUpdate";
  readonly record: CExpr;
  readonly fields: readonly { readonly name: string; readonly value: CExpr }[];
  readonly span?: Span;
};

export type CField = {
  readonly kind: "CField";
  readonly record: CExpr;
  readonly field: string;
  readonly span?: Span;
};

// Foreign call (module, name)
export type CForeign = {
  readonly kind: "CForeign";
  readonly module: string;
  readonly name: string;
  readonly span?: Span;
};

// Binary operation (kept as primitive, not desugared to function call)
export type CBinOp = {
  readonly kind: "CBinOp";
  readonly op: string;
  readonly left: CExpr;
  readonly right: CExpr;
  readonly span?: Span;
};

// =============================================================================
// Core Patterns (Section 5.3)
// =============================================================================

export type CPattern = CPWild | CPVar | CPLit | CPCon | CPTuple | CPRecord | CPAs | CPOr;

export type CPWild = {
  readonly kind: "CPWild";
  readonly span?: Span;
};

export type CPVar = {
  readonly kind: "CPVar";
  readonly name: Name;
  readonly span?: Span;
};

export type CPLit = {
  readonly kind: "CPLit";
  readonly value: Literal;
  readonly span?: Span;
};

export type CPCon = {
  readonly kind: "CPCon";
  readonly name: string;
  readonly args: readonly CPattern[];
  readonly span?: Span;
};

export type CPTuple = {
  readonly kind: "CPTuple";
  readonly elements: readonly CPattern[];
  readonly span?: Span;
};

export type CPRecord = {
  readonly kind: "CPRecord";
  readonly fields: readonly { readonly name: string; readonly pattern: CPattern }[];
  readonly span?: Span;
};

export type CPAs = {
  readonly kind: "CPAs";
  readonly name: Name;
  readonly pattern: CPattern;
  readonly span?: Span;
};

export type CPOr = {
  readonly kind: "CPOr";
  readonly left: CPattern;
  readonly right: CPattern;
  readonly span?: Span;
};

// =============================================================================
// Core Types (Section 5.6)
// =============================================================================

export type CType = CTVar | CTCon | CTApp | CTFun | CTTuple | CTRecord;

export type CTVar = {
  readonly kind: "CTVar";
  readonly name: string;
};

export type CTCon = {
  readonly kind: "CTCon";
  readonly name: string;
};

export type CTApp = {
  readonly kind: "CTApp";
  readonly func: CType;
  readonly arg: CType;
};

export type CTFun = {
  readonly kind: "CTFun";
  readonly param: CType;
  readonly result: CType;
};

export type CTTuple = {
  readonly kind: "CTTuple";
  readonly elements: readonly CType[];
};

export type CTRecord = {
  readonly kind: "CTRecord";
  readonly fields: readonly { readonly name: string; readonly type: CType }[];
};

// =============================================================================
// Core Declarations (Section 5.5)
// =============================================================================

export type CDecl = CDeclType | CDeclLet | CDeclLetRec | CDeclForeign;

export type CDeclType = {
  readonly kind: "CDeclType";
  readonly name: string;
  readonly params: readonly string[];
  readonly constructors: readonly CConDecl[];
  readonly span?: Span;
};

export type CConDecl = {
  readonly name: string;
  readonly fields: readonly CType[];
  readonly span?: Span;
};

export type CDeclLet = {
  readonly kind: "CDeclLet";
  readonly name: Name;
  readonly value: CExpr;
  readonly span?: Span;
  readonly nameSpan?: Span;
};

export type CDeclLetRec = {
  readonly kind: "CDeclLetRec";
  readonly bindings: readonly {
    readonly name: Name;
    readonly value: CExpr;
    readonly nameSpan?: Span;
  }[];
  readonly span?: Span;
};

export type CDeclForeign = {
  readonly kind: "CDeclForeign";
  readonly name: Name;
  readonly module: string;
  readonly jsName: string;
  readonly type: CType;
  readonly isAsync: boolean;
  readonly span?: Span;
  readonly nameSpan?: Span;
};

// =============================================================================
// Program
// =============================================================================

export type CProgram = {
  readonly decls: readonly CDecl[];
  readonly expr: CExpr | null;
};

// =============================================================================
// Smart Constructors
// =============================================================================

export const cvar = (name: Name, span?: Span, moduleSpan?: Span, memberSpan?: Span): CVar => ({
  kind: "CVar",
  name,
  span,
  moduleSpan,
  memberSpan,
});
export const clit = (value: Literal, span?: Span): CLit => ({ kind: "CLit", value, span });
export const capp = (func: CExpr, arg: CExpr, span?: Span): CApp => ({
  kind: "CApp",
  func,
  arg,
  span,
});
export const cabs = (param: Name, body: CExpr, span?: Span, paramSpan?: Span): CAbs => ({
  kind: "CAbs",
  param,
  body,
  span,
  paramSpan,
});
export const clet = (
  name: Name,
  value: CExpr,
  body: CExpr,
  span?: Span,
  nameSpan?: Span,
): CLet => ({
  kind: "CLet",
  name,
  value,
  body,
  span,
  nameSpan,
});
export const cletrec = (
  bindings: readonly { name: Name; value: CExpr; nameSpan?: Span }[],
  body: CExpr,
  span?: Span,
): CLetRec => ({ kind: "CLetRec", bindings, body, span });
export const cmatch = (scrutinee: CExpr, cases: readonly CCase[], span?: Span): CMatch => ({
  kind: "CMatch",
  scrutinee,
  cases,
  span,
});
export const ccon = (name: string, span?: Span): CCon => ({ kind: "CCon", name, span });
export const ctuple = (elements: readonly CExpr[], span?: Span): CTuple => ({
  kind: "CTuple",
  elements,
  span,
});
export const crecord = (
  fields: readonly { name: string; value: CExpr }[],
  span?: Span,
): CRecord => ({ kind: "CRecord", fields, span });
export const crecordUpdate = (
  record: CExpr,
  fields: readonly { name: string; value: CExpr }[],
  span?: Span,
): CRecordUpdate => ({ kind: "CRecordUpdate", record, fields, span });
export const cfield = (record: CExpr, field: string, span?: Span): CField => ({
  kind: "CField",
  record,
  field,
  span,
});
export const cforeign = (module: string, name: string, span?: Span): CForeign => ({
  kind: "CForeign",
  module,
  name,
  span,
});
export const cbinop = (op: string, left: CExpr, right: CExpr, span?: Span): CBinOp => ({
  kind: "CBinOp",
  op,
  left,
  right,
  span,
});

// Pattern constructors
export const cpwild = (span?: Span): CPWild => ({ kind: "CPWild", span });
export const cpvar = (name: Name, span?: Span): CPVar => ({ kind: "CPVar", name, span });
export const cplit = (value: Literal, span?: Span): CPLit => ({ kind: "CPLit", value, span });
export const cpcon = (name: string, args: readonly CPattern[], span?: Span): CPCon => ({
  kind: "CPCon",
  name,
  args,
  span,
});
export const cptuple = (elements: readonly CPattern[], span?: Span): CPTuple => ({
  kind: "CPTuple",
  elements,
  span,
});
export const cprecord = (
  fields: readonly { name: string; pattern: CPattern }[],
  span?: Span,
): CPRecord => ({ kind: "CPRecord", fields, span });
export const cpas = (name: Name, pattern: CPattern, span?: Span): CPAs => ({
  kind: "CPAs",
  name,
  pattern,
  span,
});
export const cpor = (left: CPattern, right: CPattern, span?: Span): CPOr => ({
  kind: "CPOr",
  left,
  right,
  span,
});

// Type constructors
export const ctvar = (name: string): CTVar => ({ kind: "CTVar", name });
export const ctcon = (name: string): CTCon => ({ kind: "CTCon", name });
export const ctapp = (func: CType, arg: CType): CTApp => ({ kind: "CTApp", func, arg });
export const ctfun = (param: CType, result: CType): CTFun => ({ kind: "CTFun", param, result });
export const cttuple = (elements: readonly CType[]): CTTuple => ({ kind: "CTTuple", elements });
export const ctrecord = (fields: readonly { name: string; type: CType }[]): CTRecord => ({
  kind: "CTRecord",
  fields,
});

// Declaration constructors
export const cdecltype = (
  name: string,
  params: readonly string[],
  constructors: readonly CConDecl[],
  span?: Span,
): CDeclType => ({ kind: "CDeclType", name, params, constructors, span });
export const cdecllet = (name: Name, value: CExpr, span?: Span, nameSpan?: Span): CDeclLet => ({
  kind: "CDeclLet",
  name,
  value,
  span,
  nameSpan,
});
export const cdeclletrec = (
  bindings: readonly { name: Name; value: CExpr; nameSpan?: Span }[],
  span?: Span,
): CDeclLetRec => ({ kind: "CDeclLetRec", bindings, span });
export const cdeclforeign = (
  name: Name,
  module: string,
  jsName: string,
  type: CType,
  isAsync: boolean,
  span?: Span,
  nameSpan?: Span,
): CDeclForeign => ({ kind: "CDeclForeign", name, module, jsName, type, isAsync, span, nameSpan });
