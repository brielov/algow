// Core AST - minimal, canonical representation after desugaring
// All surface sugar is removed. Names have unique IDs.

import type { Literal, NodeId, Span } from "./surface";

// =============================================================================
// Names (Section 5.1)
// =============================================================================

/**
 * Unique identifier for bindings.
 * - id: unique number for this binding (used internally)
 * - nodeId: AST node ID for this binding site (used for type lookup)
 * - text: the name string
 * - span: source location
 */
export type Name = {
  readonly id: number;
  readonly nodeId: NodeId;
  readonly text: string;
  readonly span: Span;
};

// Helper to create names (used by resolve.ts and desugar.ts)
let _nextId = 0;
export function resetNameCounter(): void {
  _nextId = 0;
}
export function freshName(text: string, nodeId: NodeId, span: Span): Name {
  return { id: _nextId++, nodeId, text, span };
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
  readonly nodeId: NodeId;
  readonly name: Name;
  readonly span: Span;
  // For qualified names (Module.member), track the spans separately
  readonly moduleSpan?: Span;
  readonly memberSpan?: Span;
};

export type CLit = {
  readonly kind: "CLit";
  readonly nodeId: NodeId;
  readonly value: Literal;
  readonly span: Span;
};

export type CApp = {
  readonly kind: "CApp";
  readonly nodeId: NodeId;
  readonly func: CExpr;
  readonly arg: CExpr;
  readonly span: Span;
};

// Single-param lambda (multi-param desugared to nested)
export type CAbs = {
  readonly kind: "CAbs";
  readonly nodeId: NodeId;
  readonly param: Name;
  readonly body: CExpr;
  readonly span: Span;
  readonly paramSpan?: Span;
};

export type CLet = {
  readonly kind: "CLet";
  readonly nodeId: NodeId;
  readonly name: Name;
  readonly value: CExpr;
  readonly body: CExpr;
  readonly span: Span;
};

export type CLetRec = {
  readonly kind: "CLetRec";
  readonly nodeId: NodeId;
  readonly bindings: readonly {
    readonly name: Name;
    readonly value: CExpr;
  }[];
  readonly body: CExpr;
  readonly span: Span;
};

export type CMatch = {
  readonly kind: "CMatch";
  readonly nodeId: NodeId;
  readonly scrutinee: CExpr;
  readonly cases: readonly CCase[];
  readonly span: Span;
};

export type CCase = {
  readonly pattern: CPattern;
  readonly guard: CExpr | null;
  readonly body: CExpr;
};

export type CCon = {
  readonly kind: "CCon";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly span: Span;
};

export type CTuple = {
  readonly kind: "CTuple";
  readonly nodeId: NodeId;
  readonly elements: readonly CExpr[];
  readonly span: Span;
};

export type CRecord = {
  readonly kind: "CRecord";
  readonly nodeId: NodeId;
  readonly fields: readonly { readonly name: string; readonly value: CExpr }[];
  readonly span: Span;
};

export type CRecordUpdate = {
  readonly kind: "CRecordUpdate";
  readonly nodeId: NodeId;
  readonly record: CExpr;
  readonly fields: readonly { readonly name: string; readonly value: CExpr }[];
  readonly span: Span;
};

export type CField = {
  readonly kind: "CField";
  readonly nodeId: NodeId;
  readonly record: CExpr;
  readonly field: string;
  readonly span: Span;
};

// Foreign call (module, name)
export type CForeign = {
  readonly kind: "CForeign";
  readonly nodeId: NodeId;
  readonly module: string;
  readonly name: string;
  readonly span: Span;
};

// Binary operation (kept as primitive, not desugared to function call)
export type CBinOp = {
  readonly kind: "CBinOp";
  readonly nodeId: NodeId;
  readonly op: string;
  readonly left: CExpr;
  readonly right: CExpr;
  readonly span: Span;
};

// =============================================================================
// Core Patterns (Section 5.3)
// =============================================================================

export type CPattern = CPWild | CPVar | CPLit | CPCon | CPTuple | CPRecord | CPAs | CPOr;

export type CPWild = {
  readonly kind: "CPWild";
  readonly nodeId: NodeId;
  readonly span: Span;
};

export type CPVar = {
  readonly kind: "CPVar";
  readonly nodeId: NodeId;
  readonly name: Name;
  readonly span: Span;
};

export type CPLit = {
  readonly kind: "CPLit";
  readonly nodeId: NodeId;
  readonly value: Literal;
  readonly span: Span;
};

export type CPCon = {
  readonly kind: "CPCon";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly args: readonly CPattern[];
  readonly span: Span;
};

export type CPTuple = {
  readonly kind: "CPTuple";
  readonly nodeId: NodeId;
  readonly elements: readonly CPattern[];
  readonly span: Span;
};

export type CPRecord = {
  readonly kind: "CPRecord";
  readonly nodeId: NodeId;
  readonly fields: readonly { readonly name: string; readonly pattern: CPattern }[];
  readonly span: Span;
};

export type CPAs = {
  readonly kind: "CPAs";
  readonly nodeId: NodeId;
  readonly name: Name;
  readonly pattern: CPattern;
  readonly span: Span;
};

export type CPOr = {
  readonly kind: "CPOr";
  readonly nodeId: NodeId;
  readonly left: CPattern;
  readonly right: CPattern;
  readonly span: Span;
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
  readonly nodeId: NodeId;
  readonly name: string;
  readonly params: readonly string[];
  readonly constructors: readonly CConDecl[];
  readonly span: Span;
};

export type CConDecl = {
  readonly nodeId: NodeId;
  readonly name: string;
  readonly fields: readonly CType[];
  readonly span: Span;
};

export type CDeclLet = {
  readonly kind: "CDeclLet";
  readonly nodeId: NodeId;
  readonly name: Name;
  readonly value: CExpr;
  readonly span: Span;
};

export type CDeclLetRec = {
  readonly kind: "CDeclLetRec";
  readonly nodeId: NodeId;
  readonly bindings: readonly {
    readonly name: Name;
    readonly value: CExpr;
  }[];
  readonly span: Span;
};

export type CDeclForeign = {
  readonly kind: "CDeclForeign";
  readonly nodeId: NodeId;
  readonly name: Name;
  readonly module: string;
  readonly jsName: string;
  readonly type: CType;
  readonly isAsync: boolean;
  readonly span: Span;
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

export const cvar = (
  nodeId: NodeId,
  name: Name,
  span: Span,
  moduleSpan?: Span,
  memberSpan?: Span,
): CVar => ({
  kind: "CVar",
  nodeId,
  name,
  span,
  moduleSpan,
  memberSpan,
});
export const clit = (nodeId: NodeId, value: Literal, span: Span): CLit => ({
  kind: "CLit",
  nodeId,
  value,
  span,
});
export const capp = (nodeId: NodeId, func: CExpr, arg: CExpr, span: Span): CApp => ({
  kind: "CApp",
  nodeId,
  func,
  arg,
  span,
});
export const cabs = (
  nodeId: NodeId,
  param: Name,
  body: CExpr,
  span: Span,
  paramSpan?: Span,
): CAbs => ({
  kind: "CAbs",
  nodeId,
  param,
  body,
  span,
  paramSpan,
});
export const clet = (nodeId: NodeId, name: Name, value: CExpr, body: CExpr, span: Span): CLet => ({
  kind: "CLet",
  nodeId,
  name,
  value,
  body,
  span,
});
export const cletrec = (
  nodeId: NodeId,
  bindings: readonly { name: Name; value: CExpr }[],
  body: CExpr,
  span: Span,
): CLetRec => ({ kind: "CLetRec", nodeId, bindings, body, span });
export const cmatch = (
  nodeId: NodeId,
  scrutinee: CExpr,
  cases: readonly CCase[],
  span: Span,
): CMatch => ({
  kind: "CMatch",
  nodeId,
  scrutinee,
  cases,
  span,
});
export const ccon = (nodeId: NodeId, name: string, span: Span): CCon => ({
  kind: "CCon",
  nodeId,
  name,
  span,
});
export const ctuple = (nodeId: NodeId, elements: readonly CExpr[], span: Span): CTuple => ({
  kind: "CTuple",
  nodeId,
  elements,
  span,
});
export const crecord = (
  nodeId: NodeId,
  fields: readonly { name: string; value: CExpr }[],
  span: Span,
): CRecord => ({ kind: "CRecord", nodeId, fields, span });
export const crecordUpdate = (
  nodeId: NodeId,
  record: CExpr,
  fields: readonly { name: string; value: CExpr }[],
  span: Span,
): CRecordUpdate => ({ kind: "CRecordUpdate", nodeId, record, fields, span });
export const cfield = (nodeId: NodeId, record: CExpr, field: string, span: Span): CField => ({
  kind: "CField",
  nodeId,
  record,
  field,
  span,
});
export const cforeign = (nodeId: NodeId, module: string, name: string, span: Span): CForeign => ({
  kind: "CForeign",
  nodeId,
  module,
  name,
  span,
});
export const cbinop = (
  nodeId: NodeId,
  op: string,
  left: CExpr,
  right: CExpr,
  span: Span,
): CBinOp => ({
  kind: "CBinOp",
  nodeId,
  op,
  left,
  right,
  span,
});

// Pattern constructors
export const cpwild = (nodeId: NodeId, span: Span): CPWild => ({ kind: "CPWild", nodeId, span });
export const cpvar = (nodeId: NodeId, name: Name, span: Span): CPVar => ({
  kind: "CPVar",
  nodeId,
  name,
  span,
});
export const cplit = (nodeId: NodeId, value: Literal, span: Span): CPLit => ({
  kind: "CPLit",
  nodeId,
  value,
  span,
});
export const cpcon = (
  nodeId: NodeId,
  name: string,
  args: readonly CPattern[],
  span: Span,
): CPCon => ({
  kind: "CPCon",
  nodeId,
  name,
  args,
  span,
});
export const cptuple = (nodeId: NodeId, elements: readonly CPattern[], span: Span): CPTuple => ({
  kind: "CPTuple",
  nodeId,
  elements,
  span,
});
export const cprecord = (
  nodeId: NodeId,
  fields: readonly { name: string; pattern: CPattern }[],
  span: Span,
): CPRecord => ({ kind: "CPRecord", nodeId, fields, span });
export const cpas = (nodeId: NodeId, name: Name, pattern: CPattern, span: Span): CPAs => ({
  kind: "CPAs",
  nodeId,
  name,
  pattern,
  span,
});
export const cpor = (nodeId: NodeId, left: CPattern, right: CPattern, span: Span): CPOr => ({
  kind: "CPOr",
  nodeId,
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
export const ccondecl = (
  nodeId: NodeId,
  name: string,
  fields: readonly CType[],
  span: Span,
): CConDecl => ({ nodeId, name, fields, span });
export const cdecltype = (
  nodeId: NodeId,
  name: string,
  params: readonly string[],
  constructors: readonly CConDecl[],
  span: Span,
): CDeclType => ({ kind: "CDeclType", nodeId, name, params, constructors, span });
export const cdecllet = (nodeId: NodeId, name: Name, value: CExpr, span: Span): CDeclLet => ({
  kind: "CDeclLet",
  nodeId,
  name,
  value,
  span,
});
export const cdeclletrec = (
  nodeId: NodeId,
  bindings: readonly { name: Name; value: CExpr }[],
  span: Span,
): CDeclLetRec => ({ kind: "CDeclLetRec", nodeId, bindings, span });
export const cdeclforeign = (
  nodeId: NodeId,
  name: Name,
  module: string,
  jsName: string,
  type: CType,
  isAsync: boolean,
  span: Span,
): CDeclForeign => ({ kind: "CDeclForeign", nodeId, name, module, jsName, type, isAsync, span });
