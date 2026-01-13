// Surface AST - preserves all syntactic sugar
// This is what the parser produces, before desugaring

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
  readonly name: UnresolvedName;
  readonly span: Span;
};

export type SLit = {
  readonly kind: "SLit";
  readonly value: Literal;
  readonly span: Span;
};

export type SApp = {
  readonly kind: "SApp";
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
  readonly params: readonly SParam[];
  readonly body: SExpr;
  readonly span: Span;
};

export type SLet = {
  readonly kind: "SLet";
  readonly name: UnresolvedName;
  readonly value: SExpr;
  readonly body: SExpr;
  readonly span: Span;
};

export type SLetRec = {
  readonly kind: "SLetRec";
  readonly bindings: readonly {
    readonly name: UnresolvedName;
    readonly value: SExpr;
  }[];
  readonly body: SExpr;
  readonly span: Span;
};

export type SIf = {
  readonly kind: "SIf";
  readonly cond: SExpr;
  readonly thenBranch: SExpr;
  readonly elseBranch: SExpr;
  readonly span: Span;
};

export type SMatch = {
  readonly kind: "SMatch";
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
  readonly name: string;
  readonly span: Span;
};

export type STuple = {
  readonly kind: "STuple";
  readonly elements: readonly SExpr[];
  readonly span: Span;
};

export type SRecord = {
  readonly kind: "SRecord";
  readonly fields: readonly { readonly name: string; readonly value: SExpr }[];
  readonly span: Span;
};

export type SRecordUpdate = {
  readonly kind: "SRecordUpdate";
  readonly record: SExpr;
  readonly fields: readonly { readonly name: string; readonly value: SExpr }[];
  readonly span: Span;
};

export type SField = {
  readonly kind: "SField";
  readonly record: SExpr;
  readonly field: string;
  readonly span: Span;
  readonly fieldSpan: Span;
};

// List literal [1, 2, 3] - desugars to nested Cons
export type SList = {
  readonly kind: "SList";
  readonly elements: readonly SExpr[];
  readonly span: Span;
};

// Pipe operator: e |> f - desugars to f(e)
export type SPipe = {
  readonly kind: "SPipe";
  readonly left: SExpr;
  readonly right: SExpr;
  readonly span: Span;
};

// Cons operator: x :: xs - desugars to Cons x xs
export type SCons = {
  readonly kind: "SCons";
  readonly head: SExpr;
  readonly tail: SExpr;
  readonly span: Span;
};

// Binary operator: a + b - desugars to (+) a b
export type SBinOp = {
  readonly kind: "SBinOp";
  readonly op: string;
  readonly left: SExpr;
  readonly right: SExpr;
  readonly span: Span;
};

// Do-notation (Section 4.3)
export type SDo = {
  readonly kind: "SDo";
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
  readonly span: Span;
};

export type SPVar = {
  readonly kind: "SPVar";
  readonly name: UnresolvedName;
  readonly span: Span;
};

export type SPLit = {
  readonly kind: "SPLit";
  readonly value: Literal;
  readonly span: Span;
};

export type SPCon = {
  readonly kind: "SPCon";
  readonly name: string;
  readonly args: readonly SPattern[];
  readonly span: Span;
};

export type SPTuple = {
  readonly kind: "SPTuple";
  readonly elements: readonly SPattern[];
  readonly span: Span;
};

export type SPRecord = {
  readonly kind: "SPRecord";
  readonly fields: readonly { readonly name: string; readonly pattern: SPattern }[];
  readonly span: Span;
};

export type SPAs = {
  readonly kind: "SPAs";
  readonly name: UnresolvedName;
  readonly pattern: SPattern;
  readonly span: Span;
};

export type SPOr = {
  readonly kind: "SPOr";
  readonly left: SPattern;
  readonly right: SPattern;
  readonly span: Span;
};

// Cons pattern: x :: xs - desugars to Cons pattern
export type SPCons = {
  readonly kind: "SPCons";
  readonly head: SPattern;
  readonly tail: SPattern;
  readonly span: Span;
};

// List pattern: [a, b, c] - desugars to nested Cons patterns
export type SPList = {
  readonly kind: "SPList";
  readonly elements: readonly SPattern[];
  readonly span: Span;
};

// =============================================================================
// Surface Types (Section 4.4)
// =============================================================================

export type SType = STVar | STCon | STApp | STFun | STTuple | STRecord;

export type STVar = {
  readonly kind: "STVar";
  readonly name: string;
  readonly span: Span;
};

export type STCon = {
  readonly kind: "STCon";
  readonly name: string;
  readonly span: Span;
};

export type STApp = {
  readonly kind: "STApp";
  readonly func: SType;
  readonly arg: SType;
  readonly span: Span;
};

export type STFun = {
  readonly kind: "STFun";
  readonly param: SType;
  readonly result: SType;
  readonly span: Span;
};

export type STTuple = {
  readonly kind: "STTuple";
  readonly elements: readonly SType[];
  readonly span: Span;
};

export type STRecord = {
  readonly kind: "STRecord";
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
  readonly name: string;
  readonly params: readonly string[];
  readonly constructors: readonly SConDecl[];
  readonly span: Span;
};

export type SConDecl = {
  readonly name: string;
  readonly fields: readonly SType[];
  readonly span: Span;
};

// Type alias: type Point = { x : int, y : int }
export type SDeclTypeAlias = {
  readonly kind: "SDeclTypeAlias";
  readonly name: string;
  readonly params: readonly string[];
  readonly type: SType;
  readonly span: Span;
};

// Let binding: let x = e
export type SDeclLet = {
  readonly kind: "SDeclLet";
  readonly name: UnresolvedName;
  readonly value: SExpr;
  readonly span: Span;
};

// Recursive let: let rec f = ... and g = ...
export type SDeclLetRec = {
  readonly kind: "SDeclLetRec";
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
  readonly name: UnresolvedName;
  readonly type: SType;
  readonly isAsync: boolean;
  readonly span: Span;
};

// Module declaration: module Foo use ... end
export type SDeclModule = {
  readonly kind: "SDeclModule";
  readonly name: string;
  readonly uses: readonly string[];
  readonly decls: readonly SDecl[];
  readonly span: Span;
};

// Use declaration: use Foo (bar, baz) or use Foo (..)
export type SDeclUse = {
  readonly kind: "SDeclUse";
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

export const svar = (name: UnresolvedName): SVar => ({ kind: "SVar", name, span: name.span });
export const slit = (value: Literal, span: Span): SLit => ({ kind: "SLit", value, span });
export const sint = (value: number, span: Span): SLit => slit({ kind: "int", value }, span);
export const sfloat = (value: number, span: Span): SLit => slit({ kind: "float", value }, span);
export const sstring = (value: string, span: Span): SLit => slit({ kind: "string", value }, span);
export const schar = (value: string, span: Span): SLit => slit({ kind: "char", value }, span);
export const sbool = (value: boolean, span: Span): SLit => slit({ kind: "bool", value }, span);
export const sapp = (func: SExpr, arg: SExpr, span: Span): SApp => ({
  kind: "SApp",
  func,
  arg,
  span,
});
export const sparam = (name: UnresolvedName): SParam => ({ name });
export const sabs = (params: readonly SParam[], body: SExpr, span: Span): SAbs => ({
  kind: "SAbs",
  params,
  body,
  span,
});
export const slet = (name: UnresolvedName, value: SExpr, body: SExpr, span: Span): SLet => ({
  kind: "SLet",
  name,
  value,
  body,
  span,
});
export const sletrec = (
  bindings: readonly { name: UnresolvedName; value: SExpr }[],
  body: SExpr,
  span: Span,
): SLetRec => ({ kind: "SLetRec", bindings, body, span });
export const sif = (cond: SExpr, thenBranch: SExpr, elseBranch: SExpr, span: Span): SIf => ({
  kind: "SIf",
  cond,
  thenBranch,
  elseBranch,
  span,
});
export const smatch = (scrutinee: SExpr, cases: readonly SCase[], span: Span): SMatch => ({
  kind: "SMatch",
  scrutinee,
  cases,
  span,
});
export const scon = (name: string, span: Span): SCon => ({ kind: "SCon", name, span });
export const stuple = (elements: readonly SExpr[], span: Span): STuple => ({
  kind: "STuple",
  elements,
  span,
});
export const srecord = (
  fields: readonly { name: string; value: SExpr }[],
  span: Span,
): SRecord => ({ kind: "SRecord", fields, span });
export const srecordUpdate = (
  record: SExpr,
  fields: readonly { name: string; value: SExpr }[],
  span: Span,
): SRecordUpdate => ({ kind: "SRecordUpdate", record, fields, span });
export const sfield = (record: SExpr, field: string, span: Span, fieldSpan: Span): SField => ({
  kind: "SField",
  record,
  field,
  span,
  fieldSpan,
});
export const slist = (elements: readonly SExpr[], span: Span): SList => ({
  kind: "SList",
  elements,
  span,
});
export const spipe = (left: SExpr, right: SExpr, span: Span): SPipe => ({
  kind: "SPipe",
  left,
  right,
  span,
});
export const scons = (head: SExpr, tail: SExpr, span: Span): SCons => ({
  kind: "SCons",
  head,
  tail,
  span,
});
export const sbinop = (op: string, left: SExpr, right: SExpr, span: Span): SBinOp => ({
  kind: "SBinOp",
  op,
  left,
  right,
  span,
});
export const sdo = (stmts: readonly SDoStmt[], span: Span): SDo => ({ kind: "SDo", stmts, span });
export const sannot = (expr: SExpr, type: SType, span: Span): SAnnot => ({
  kind: "SAnnot",
  expr,
  type,
  span,
});

// Pattern constructors
export const spwild = (span: Span): SPWild => ({ kind: "SPWild", span });
export const spvar = (name: UnresolvedName): SPVar => ({ kind: "SPVar", name, span: name.span });
export const split = (value: Literal, span: Span): SPLit => ({ kind: "SPLit", value, span });
export const spcon = (name: string, args: readonly SPattern[], span: Span): SPCon => ({
  kind: "SPCon",
  name,
  args,
  span,
});
export const sptuple = (elements: readonly SPattern[], span: Span): SPTuple => ({
  kind: "SPTuple",
  elements,
  span,
});
export const sprecord = (
  fields: readonly { name: string; pattern: SPattern }[],
  span: Span,
): SPRecord => ({ kind: "SPRecord", fields, span });
export const spas = (name: UnresolvedName, pattern: SPattern, span: Span): SPAs => ({
  kind: "SPAs",
  name,
  pattern,
  span,
});
export const spor = (left: SPattern, right: SPattern, span: Span): SPOr => ({
  kind: "SPOr",
  left,
  right,
  span,
});
export const spcons = (head: SPattern, tail: SPattern, span: Span): SPCons => ({
  kind: "SPCons",
  head,
  tail,
  span,
});
export const splist = (elements: readonly SPattern[], span: Span): SPList => ({
  kind: "SPList",
  elements,
  span,
});

// Type constructors
export const stvar = (name: string, span: Span): STVar => ({ kind: "STVar", name, span });
export const stcon = (name: string, span: Span): STCon => ({ kind: "STCon", name, span });
export const stapp = (func: SType, arg: SType, span: Span): STApp => ({
  kind: "STApp",
  func,
  arg,
  span,
});
export const stfun = (param: SType, result: SType, span: Span): STFun => ({
  kind: "STFun",
  param,
  result,
  span,
});
export const sttuple = (elements: readonly SType[], span: Span): STTuple => ({
  kind: "STTuple",
  elements,
  span,
});
export const strecord = (
  fields: readonly { name: string; type: SType }[],
  span: Span,
): STRecord => ({ kind: "STRecord", fields, span });

// Declaration constructors
export const sdecltype = (
  name: string,
  params: readonly string[],
  constructors: readonly SConDecl[],
  span: Span,
): SDeclType => ({ kind: "SDeclType", name, params, constructors, span });
export const sdecltypealias = (
  name: string,
  params: readonly string[],
  type: SType,
  span: Span,
): SDeclTypeAlias => ({ kind: "SDeclTypeAlias", name, params, type, span });
export const sdecllet = (name: UnresolvedName, value: SExpr, span: Span): SDeclLet => ({
  kind: "SDeclLet",
  name,
  value,
  span,
});
export const sdeclletrec = (
  bindings: readonly { name: UnresolvedName; value: SExpr }[],
  span: Span,
): SDeclLetRec => ({ kind: "SDeclLetRec", bindings, span });
export const sdeclforeign = (
  name: UnresolvedName,
  type: SType,
  isAsync: boolean,
  span: Span,
): SDeclForeign => ({
  kind: "SDeclForeign",
  name,
  type,
  isAsync,
  span,
});
export const sdeclmodule = (
  name: string,
  uses: readonly string[],
  decls: readonly SDecl[],
  span: Span,
): SDeclModule => ({ kind: "SDeclModule", name, uses, decls, span });
export const sdecluse = (
  module: string,
  imports: readonly string[] | "all",
  span: Span,
): SDeclUse => ({ kind: "SDeclUse", module, imports, span });
