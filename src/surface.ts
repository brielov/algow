// Surface AST - preserves all syntactic sugar
// This is what the parser produces, before desugaring

// =============================================================================
// Node Identity
// =============================================================================

/** Unique identifier for AST nodes across entire compilation */
export type NodeId = number;

/** Generator for unique node IDs */
export type NodeIdGenerator = {
  next(): NodeId;
};

/** Create a new node ID generator */
export const createNodeIdGenerator = (startId: number = 0): NodeIdGenerator => {
  let nextId = startId;
  return {
    next: () => nextId++,
  };
};

// =============================================================================
// Spans, Names, and Literals
// =============================================================================

/** File identifier for multi-file support */
export type FileId = number;

export type Span = {
  readonly fileId: FileId;
  readonly start: number;
  readonly end: number;
};

// UnresolvedName: A name as it appears in source code, before resolution
// Used in Surface AST where we haven't yet resolved which binding the name refers to
export type UnresolvedName = {
  readonly text: string;
  readonly span: Span;
};

export type Literal =
  | { readonly kind: "int"; readonly value: number }
  | { readonly kind: "float"; readonly value: number }
  | { readonly kind: "string"; readonly value: string }
  | { readonly kind: "char"; readonly value: string }
  | { readonly kind: "bool"; readonly value: boolean };

// =============================================================================
// Comments (for formatter)
// =============================================================================

export type Comment = {
  readonly kind: "line" | "block";
  readonly text: string;
  readonly span: Span;
};

// =============================================================================
// Surface Expressions (Section 4.1)
// =============================================================================

export type SExpr =
  | SVar
  | SLit
  | SApp
  | SAbs
  | SLet
  | SLetRec
  | SIf
  | SMatch
  | SCon
  | STuple
  | SRecord
  | SRecordUpdate
  | SField
  | SList
  | SPipe
  | SCons
  | SBinOp
  | SDo
  | SAnnot;

export type SVar = {
  readonly kind: "SVar";
  readonly nodeId: NodeId;
  readonly name: UnresolvedName;
  readonly span: Span;
};

export type SLit = {
  readonly kind: "SLit";
  readonly nodeId: NodeId;
  readonly value: Literal;
  readonly span: Span;
};

export type SApp = {
  readonly kind: "SApp";
  readonly nodeId: NodeId;
  readonly func: SExpr;
  readonly arg: SExpr;
  readonly span: Span;
};

// Parameter for lambda expressions
export type SParam = {
  readonly name: UnresolvedName;
};

// Multi-param lambda (desugars to nested single-param)
export type SAbs = {
  readonly kind: "SAbs";
  readonly nodeId: NodeId;
  readonly params: readonly SParam[];
  readonly body: SExpr;
  readonly span: Span;
};

export type SLet = {
  readonly kind: "SLet";
  readonly nodeId: NodeId;
  readonly name: UnresolvedName;
  readonly value: SExpr;
  readonly body: SExpr;
  readonly span: Span;
};

export type SLetRec = {
  readonly kind: "SLetRec";
  readonly nodeId: NodeId;
  readonly bindings: readonly {
    readonly name: UnresolvedName;
    readonly value: SExpr;
  }[];
  readonly body: SExpr;
  readonly span: Span;
};

export type SIf = {
  readonly kind: "SIf";
  readonly nodeId: NodeId;
  readonly cond: SExpr;
  readonly thenBranch: SExpr;
  readonly elseBranch: SExpr;
  readonly span: Span;
};

export type SMatch = {
  readonly kind: "SMatch";
  readonly nodeId: NodeId;
  readonly scrutinee: SExpr;
  readonly cases: readonly SCase[];
  readonly span: Span;
};

export type SCase = {
  readonly pattern: SPattern;
  readonly guard: SExpr | null;
  readonly body: SExpr;
};

export type SCon = {
  readonly kind: "SCon";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly span: Span;
};

export type STuple = {
  readonly kind: "STuple";
  readonly nodeId: NodeId;
  readonly elements: readonly SExpr[];
  readonly span: Span;
};

export type SRecord = {
  readonly kind: "SRecord";
  readonly nodeId: NodeId;
  readonly fields: readonly { readonly name: string; readonly value: SExpr }[];
  readonly span: Span;
};

export type SRecordUpdate = {
  readonly kind: "SRecordUpdate";
  readonly nodeId: NodeId;
  readonly record: SExpr;
  readonly fields: readonly { readonly name: string; readonly value: SExpr }[];
  readonly span: Span;
};

export type SField = {
  readonly kind: "SField";
  readonly nodeId: NodeId;
  readonly record: SExpr;
  readonly field: string;
  readonly span: Span;
  readonly fieldSpan: Span;
};

// List literal [1, 2, 3] - desugars to nested Cons
export type SList = {
  readonly kind: "SList";
  readonly nodeId: NodeId;
  readonly elements: readonly SExpr[];
  readonly span: Span;
};

// Pipe operator: e |> f - desugars to f(e)
export type SPipe = {
  readonly kind: "SPipe";
  readonly nodeId: NodeId;
  readonly left: SExpr;
  readonly right: SExpr;
  readonly span: Span;
};

// Cons operator: x :: xs - desugars to Cons x xs
export type SCons = {
  readonly kind: "SCons";
  readonly nodeId: NodeId;
  readonly head: SExpr;
  readonly tail: SExpr;
  readonly span: Span;
};

// Binary operator: a + b - desugars to (+) a b
export type SBinOp = {
  readonly kind: "SBinOp";
  readonly nodeId: NodeId;
  readonly op: string;
  readonly left: SExpr;
  readonly right: SExpr;
  readonly span: Span;
};

// Do-notation (Section 4.3)
// Required module qualifier: do[Maybe] ... end desugars to Maybe.flatMap
export type SDo = {
  readonly kind: "SDo";
  readonly nodeId: NodeId;
  readonly moduleName: string;
  readonly stmts: readonly SDoStmt[];
  readonly span: Span;
};

export type SDoStmt =
  | { readonly kind: "DoBindPattern"; readonly pattern: SPattern; readonly expr: SExpr }
  | { readonly kind: "DoLet"; readonly pattern: SPattern; readonly expr: SExpr }
  | { readonly kind: "DoExpr"; readonly expr: SExpr };

// Type annotation: (e : T)
export type SAnnot = {
  readonly kind: "SAnnot";
  readonly nodeId: NodeId;
  readonly expr: SExpr;
  readonly type: SType;
  readonly span: Span;
};

// =============================================================================
// Surface Patterns (Section 4.2)
// =============================================================================

export type SPattern =
  | SPWild
  | SPVar
  | SPLit
  | SPCon
  | SPTuple
  | SPRecord
  | SPAs
  | SPOr
  | SPCons
  | SPList;

export type SPWild = {
  readonly kind: "SPWild";
  readonly nodeId: NodeId;
  readonly span: Span;
};

export type SPVar = {
  readonly kind: "SPVar";
  readonly nodeId: NodeId;
  readonly name: UnresolvedName;
  readonly span: Span;
};

export type SPLit = {
  readonly kind: "SPLit";
  readonly nodeId: NodeId;
  readonly value: Literal;
  readonly span: Span;
};

export type SPCon = {
  readonly kind: "SPCon";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly args: readonly SPattern[];
  readonly span: Span;
};

export type SPTuple = {
  readonly kind: "SPTuple";
  readonly nodeId: NodeId;
  readonly elements: readonly SPattern[];
  readonly span: Span;
};

export type SPRecord = {
  readonly kind: "SPRecord";
  readonly nodeId: NodeId;
  readonly fields: readonly { readonly name: string; readonly pattern: SPattern }[];
  readonly span: Span;
};

export type SPAs = {
  readonly kind: "SPAs";
  readonly nodeId: NodeId;
  readonly name: UnresolvedName;
  readonly pattern: SPattern;
  readonly span: Span;
};

export type SPOr = {
  readonly kind: "SPOr";
  readonly nodeId: NodeId;
  readonly left: SPattern;
  readonly right: SPattern;
  readonly span: Span;
};

// Cons pattern: x :: xs - desugars to Cons pattern
export type SPCons = {
  readonly kind: "SPCons";
  readonly nodeId: NodeId;
  readonly head: SPattern;
  readonly tail: SPattern;
  readonly span: Span;
};

// List pattern: [a, b, c] - desugars to nested Cons patterns
export type SPList = {
  readonly kind: "SPList";
  readonly nodeId: NodeId;
  readonly elements: readonly SPattern[];
  readonly span: Span;
};

// =============================================================================
// Surface Types (Section 4.4)
// =============================================================================

export type SType = STVar | STCon | STApp | STFun | STTuple | STRecord;

export type STVar = {
  readonly kind: "STVar";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly span: Span;
};

export type STCon = {
  readonly kind: "STCon";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly span: Span;
};

export type STApp = {
  readonly kind: "STApp";
  readonly nodeId: NodeId;
  readonly func: SType;
  readonly arg: SType;
  readonly span: Span;
};

export type STFun = {
  readonly kind: "STFun";
  readonly nodeId: NodeId;
  readonly param: SType;
  readonly result: SType;
  readonly span: Span;
};

export type STTuple = {
  readonly kind: "STTuple";
  readonly nodeId: NodeId;
  readonly elements: readonly SType[];
  readonly span: Span;
};

export type STRecord = {
  readonly kind: "STRecord";
  readonly nodeId: NodeId;
  readonly fields: readonly { readonly name: string; readonly type: SType }[];
  readonly span: Span;
};

// =============================================================================
// Surface Declarations (Section 4.5)
// =============================================================================

export type SDecl =
  | SDeclType
  | SDeclTypeAlias
  | SDeclLet
  | SDeclLetRec
  | SDeclForeign
  | SDeclModule
  | SDeclUse;

// ADT declaration: type Maybe a = Nothing | Just a
export type SDeclType = {
  readonly kind: "SDeclType";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly params: readonly string[];
  readonly constructors: readonly SConDecl[];
  readonly span: Span;
};

export type SConDecl = {
  readonly nodeId: NodeId;
  readonly name: string;
  readonly fields: readonly SType[];
  readonly span: Span;
};

// Type alias: type Point = { x : int, y : int }
export type SDeclTypeAlias = {
  readonly kind: "SDeclTypeAlias";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly params: readonly string[];
  readonly type: SType;
  readonly span: Span;
};

// Let binding: let x = e
export type SDeclLet = {
  readonly kind: "SDeclLet";
  readonly nodeId: NodeId;
  readonly name: UnresolvedName;
  readonly value: SExpr;
  readonly span: Span;
};

// Recursive let: let rec f = ... and g = ...
export type SDeclLetRec = {
  readonly kind: "SDeclLetRec";
  readonly nodeId: NodeId;
  readonly bindings: readonly {
    readonly name: UnresolvedName;
    readonly value: SExpr;
  }[];
  readonly span: Span;
};

// Foreign binding: foreign foo : Int -> Int
// Async foreign: foreign async foo : Int -> Int
export type SDeclForeign = {
  readonly kind: "SDeclForeign";
  readonly nodeId: NodeId;
  readonly name: UnresolvedName;
  readonly type: SType;
  readonly isAsync: boolean;
  readonly span: Span;
};

// Module declaration: module Foo use ... end
export type SDeclModule = {
  readonly kind: "SDeclModule";
  readonly nodeId: NodeId;
  readonly name: string;
  readonly uses: readonly string[];
  readonly decls: readonly SDecl[];
  readonly span: Span;
};

// Use declaration: use Foo (bar, baz) or use Foo (..)
export type SDeclUse = {
  readonly kind: "SDeclUse";
  readonly nodeId: NodeId;
  readonly module: string;
  readonly imports: readonly string[] | "all"; // specific names or ".." for all
  readonly span: Span;
};

// =============================================================================
// Program
// =============================================================================

export type SProgram = {
  readonly decls: readonly SDecl[];
  readonly expr: SExpr | null;
};

// =============================================================================
// Smart Constructors
// =============================================================================

// Name constructor
export const unresolvedName = (text: string, span: Span): UnresolvedName => ({ text, span });

// Expression constructors
export const svar = (nodeId: NodeId, name: UnresolvedName): SVar => ({
  kind: "SVar",
  nodeId,
  name,
  span: name.span,
});
export const slit = (nodeId: NodeId, value: Literal, span: Span): SLit => ({
  kind: "SLit",
  nodeId,
  value,
  span,
});
export const sint = (nodeId: NodeId, value: number, span: Span): SLit =>
  slit(nodeId, { kind: "int", value }, span);
export const sfloat = (nodeId: NodeId, value: number, span: Span): SLit =>
  slit(nodeId, { kind: "float", value }, span);
export const sstring = (nodeId: NodeId, value: string, span: Span): SLit =>
  slit(nodeId, { kind: "string", value }, span);
export const schar = (nodeId: NodeId, value: string, span: Span): SLit =>
  slit(nodeId, { kind: "char", value }, span);
export const sbool = (nodeId: NodeId, value: boolean, span: Span): SLit =>
  slit(nodeId, { kind: "bool", value }, span);
export const sapp = (nodeId: NodeId, func: SExpr, arg: SExpr, span: Span): SApp => ({
  kind: "SApp",
  nodeId,
  func,
  arg,
  span,
});
export const sparam = (name: UnresolvedName): SParam => ({ name });
export const sabs = (nodeId: NodeId, params: readonly SParam[], body: SExpr, span: Span): SAbs => ({
  kind: "SAbs",
  nodeId,
  params,
  body,
  span,
});
export const slet = (
  nodeId: NodeId,
  name: UnresolvedName,
  value: SExpr,
  body: SExpr,
  span: Span,
): SLet => ({
  kind: "SLet",
  nodeId,
  name,
  value,
  body,
  span,
});
export const sletrec = (
  nodeId: NodeId,
  bindings: readonly { name: UnresolvedName; value: SExpr }[],
  body: SExpr,
  span: Span,
): SLetRec => ({ kind: "SLetRec", nodeId, bindings, body, span });
export const sif = (
  nodeId: NodeId,
  cond: SExpr,
  thenBranch: SExpr,
  elseBranch: SExpr,
  span: Span,
): SIf => ({
  kind: "SIf",
  nodeId,
  cond,
  thenBranch,
  elseBranch,
  span,
});
export const smatch = (
  nodeId: NodeId,
  scrutinee: SExpr,
  cases: readonly SCase[],
  span: Span,
): SMatch => ({
  kind: "SMatch",
  nodeId,
  scrutinee,
  cases,
  span,
});
export const scon = (nodeId: NodeId, name: string, span: Span): SCon => ({
  kind: "SCon",
  nodeId,
  name,
  span,
});
export const stuple = (nodeId: NodeId, elements: readonly SExpr[], span: Span): STuple => ({
  kind: "STuple",
  nodeId,
  elements,
  span,
});
export const srecord = (
  nodeId: NodeId,
  fields: readonly { name: string; value: SExpr }[],
  span: Span,
): SRecord => ({ kind: "SRecord", nodeId, fields, span });
export const srecordUpdate = (
  nodeId: NodeId,
  record: SExpr,
  fields: readonly { name: string; value: SExpr }[],
  span: Span,
): SRecordUpdate => ({ kind: "SRecordUpdate", nodeId, record, fields, span });
export const sfield = (
  nodeId: NodeId,
  record: SExpr,
  field: string,
  span: Span,
  fieldSpan: Span,
): SField => ({
  kind: "SField",
  nodeId,
  record,
  field,
  span,
  fieldSpan,
});
export const slist = (nodeId: NodeId, elements: readonly SExpr[], span: Span): SList => ({
  kind: "SList",
  nodeId,
  elements,
  span,
});
export const spipe = (nodeId: NodeId, left: SExpr, right: SExpr, span: Span): SPipe => ({
  kind: "SPipe",
  nodeId,
  left,
  right,
  span,
});
export const scons = (nodeId: NodeId, head: SExpr, tail: SExpr, span: Span): SCons => ({
  kind: "SCons",
  nodeId,
  head,
  tail,
  span,
});
export const sbinop = (
  nodeId: NodeId,
  op: string,
  left: SExpr,
  right: SExpr,
  span: Span,
): SBinOp => ({
  kind: "SBinOp",
  nodeId,
  op,
  left,
  right,
  span,
});
export const sdo = (
  nodeId: NodeId,
  moduleName: string,
  stmts: readonly SDoStmt[],
  span: Span,
): SDo => ({
  kind: "SDo",
  nodeId,
  moduleName,
  stmts,
  span,
});
export const sannot = (nodeId: NodeId, expr: SExpr, type: SType, span: Span): SAnnot => ({
  kind: "SAnnot",
  nodeId,
  expr,
  type,
  span,
});

// Pattern constructors
export const spwild = (nodeId: NodeId, span: Span): SPWild => ({ kind: "SPWild", nodeId, span });
export const spvar = (nodeId: NodeId, name: UnresolvedName): SPVar => ({
  kind: "SPVar",
  nodeId,
  name,
  span: name.span,
});
export const split = (nodeId: NodeId, value: Literal, span: Span): SPLit => ({
  kind: "SPLit",
  nodeId,
  value,
  span,
});
export const spcon = (
  nodeId: NodeId,
  name: string,
  args: readonly SPattern[],
  span: Span,
): SPCon => ({
  kind: "SPCon",
  nodeId,
  name,
  args,
  span,
});
export const sptuple = (nodeId: NodeId, elements: readonly SPattern[], span: Span): SPTuple => ({
  kind: "SPTuple",
  nodeId,
  elements,
  span,
});
export const sprecord = (
  nodeId: NodeId,
  fields: readonly { name: string; pattern: SPattern }[],
  span: Span,
): SPRecord => ({ kind: "SPRecord", nodeId, fields, span });
export const spas = (
  nodeId: NodeId,
  name: UnresolvedName,
  pattern: SPattern,
  span: Span,
): SPAs => ({
  kind: "SPAs",
  nodeId,
  name,
  pattern,
  span,
});
export const spor = (nodeId: NodeId, left: SPattern, right: SPattern, span: Span): SPOr => ({
  kind: "SPOr",
  nodeId,
  left,
  right,
  span,
});
export const spcons = (nodeId: NodeId, head: SPattern, tail: SPattern, span: Span): SPCons => ({
  kind: "SPCons",
  nodeId,
  head,
  tail,
  span,
});
export const splist = (nodeId: NodeId, elements: readonly SPattern[], span: Span): SPList => ({
  kind: "SPList",
  nodeId,
  elements,
  span,
});

// Type constructors
export const stvar = (nodeId: NodeId, name: string, span: Span): STVar => ({
  kind: "STVar",
  nodeId,
  name,
  span,
});
export const stcon = (nodeId: NodeId, name: string, span: Span): STCon => ({
  kind: "STCon",
  nodeId,
  name,
  span,
});
export const stapp = (nodeId: NodeId, func: SType, arg: SType, span: Span): STApp => ({
  kind: "STApp",
  nodeId,
  func,
  arg,
  span,
});
export const stfun = (nodeId: NodeId, param: SType, result: SType, span: Span): STFun => ({
  kind: "STFun",
  nodeId,
  param,
  result,
  span,
});
export const sttuple = (nodeId: NodeId, elements: readonly SType[], span: Span): STTuple => ({
  kind: "STTuple",
  nodeId,
  elements,
  span,
});
export const strecord = (
  nodeId: NodeId,
  fields: readonly { name: string; type: SType }[],
  span: Span,
): STRecord => ({ kind: "STRecord", nodeId, fields, span });

// Declaration constructors
export const sdecltype = (
  nodeId: NodeId,
  name: string,
  params: readonly string[],
  constructors: readonly SConDecl[],
  span: Span,
): SDeclType => ({ kind: "SDeclType", nodeId, name, params, constructors, span });
export const sconDecl = (
  nodeId: NodeId,
  name: string,
  fields: readonly SType[],
  span: Span,
): SConDecl => ({
  nodeId,
  name,
  fields,
  span,
});
export const sdecltypealias = (
  nodeId: NodeId,
  name: string,
  params: readonly string[],
  type: SType,
  span: Span,
): SDeclTypeAlias => ({ kind: "SDeclTypeAlias", nodeId, name, params, type, span });
export const sdecllet = (
  nodeId: NodeId,
  name: UnresolvedName,
  value: SExpr,
  span: Span,
): SDeclLet => ({
  kind: "SDeclLet",
  nodeId,
  name,
  value,
  span,
});
export const sdeclletrec = (
  nodeId: NodeId,
  bindings: readonly { name: UnresolvedName; value: SExpr }[],
  span: Span,
): SDeclLetRec => ({ kind: "SDeclLetRec", nodeId, bindings, span });
export const sdeclforeign = (
  nodeId: NodeId,
  name: UnresolvedName,
  type: SType,
  isAsync: boolean,
  span: Span,
): SDeclForeign => ({
  kind: "SDeclForeign",
  nodeId,
  name,
  type,
  isAsync,
  span,
});
export const sdeclmodule = (
  nodeId: NodeId,
  name: string,
  uses: readonly string[],
  decls: readonly SDecl[],
  span: Span,
): SDeclModule => ({ kind: "SDeclModule", nodeId, name, uses, decls, span });
export const sdecluse = (
  nodeId: NodeId,
  module: string,
  imports: readonly string[] | "all",
  span: Span,
): SDeclUse => ({ kind: "SDeclUse", nodeId, module, imports, span });
