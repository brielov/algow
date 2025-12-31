// oxlint-disable no-thenable

export type Expr =
	| Con
	| Let
	| LetRec
	| Var
	| Abs
	| App
	| Tuple
	| Record
	| FieldAccess
	| If
	| BinOp
	| Match;

export type Con = Num | Bool | Str;

export type Num = {
	readonly kind: "Num";
	readonly value: number;
};

export type Bool = {
	readonly kind: "Bool";
	readonly value: boolean;
};

export type Str = {
	readonly kind: "Str";
	readonly value: string;
};

export type Let = {
	readonly kind: "Let";
	readonly name: string;
	readonly value: Expr;
	readonly body: Expr;
};

export type LetRec = {
	readonly kind: "LetRec";
	readonly name: string;
	readonly value: Expr;
	readonly body: Expr;
};

export type Var = {
	readonly kind: "Var";
	readonly name: string;
};

export type Abs = {
	readonly kind: "Abs";
	readonly param: string;
	readonly body: Expr;
};

export type App = {
	readonly kind: "App";
	readonly func: Expr;
	readonly param: Expr;
};

export type Tuple = {
	readonly kind: "Tuple";
	readonly elements: readonly Expr[];
};

export type Record = {
	readonly kind: "Record";
	readonly fields: readonly RecordField[];
};

export type RecordField = {
	readonly name: string;
	readonly value: Expr;
};

export type FieldAccess = {
	readonly kind: "FieldAccess";
	readonly record: Expr;
	readonly field: string;
};

export type If = {
	readonly kind: "If";
	readonly cond: Expr;
	readonly then: Expr;
	readonly else: Expr;
};

export type BinOp = {
	readonly kind: "BinOp";
	readonly op: Op;
	readonly left: Expr;
	readonly right: Expr;
};

export type Op = "+" | "-" | "/" | "*" | "<" | "<=" | ">" | ">=" | "==" | "!=";

export type Pattern = PVar | PWildcard | PCon | PLit | PRecord;

export type PVar = {
	readonly kind: "PVar";
	readonly name: string;
};

export type PWildcard = {
	readonly kind: "PWildcard";
};

export type PCon = {
	readonly kind: "PCon";
	readonly name: string;
	readonly args: readonly Pattern[];
};

export type PLit = {
	readonly kind: "PLit";
	readonly value: number | string | boolean;
};

export type PRecordField = {
	readonly name: string;
	readonly pattern: Pattern;
};

export type PRecord = {
	readonly kind: "PRecord";
	readonly fields: readonly PRecordField[];
};

export type Case = {
	readonly pattern: Pattern;
	readonly body: Expr;
};

export type Match = {
	readonly kind: "Match";
	readonly expr: Expr;
	readonly cases: readonly Case[];
};

export type TypeExpr = TyVar | TyCon | TyApp | TyFun;

export type TyVar = {
	readonly kind: "TyVar";
	readonly name: string;
};

export type TyCon = {
	readonly kind: "TyCon";
	readonly name: string;
};

export type TyApp = {
	readonly kind: "TyApp";
	readonly con: TypeExpr;
	readonly arg: TypeExpr;
};

export type TyFun = {
	readonly kind: "TyFun";
	readonly param: TypeExpr;
	readonly ret: TypeExpr;
};

export type ConDecl = {
	readonly name: string;
	readonly fields: readonly TypeExpr[];
};

export type DataDecl = {
	readonly kind: "DataDecl";
	readonly name: string;
	readonly typeParams: readonly string[];
	readonly constructors: readonly ConDecl[];
};

export const num = (value: number): Num => ({ kind: "Num", value });
export const bool = (value: boolean): Bool => ({ kind: "Bool", value });
export const str = (value: string): Str => ({ kind: "Str", value });

export const if_ = (cond: Expr, then: Expr, else_: Expr): If => ({
	kind: "If",
	cond,
	then,
	else: else_,
});

export const let_ = (name: string, value: Expr, body: Expr): Let => ({
	kind: "Let",
	name,
	value,
	body,
});

export const letRec = (name: string, value: Expr, body: Expr): LetRec => ({
	kind: "LetRec",
	name,
	value,
	body,
});

export const var_ = (name: string): Var => ({ kind: "Var", name });

export const abs = (param: string, body: Expr): Abs => ({
	kind: "Abs",
	param,
	body,
});

export const app = (func: Expr, param: Expr): App => ({
	kind: "App",
	func,
	param,
});

export const tuple = (...elements: readonly Expr[]): Tuple => ({
	kind: "Tuple",
	elements,
});

export const record = (fields: readonly RecordField[]): Record => ({
	kind: "Record",
	fields,
});
export const field = (name: string, value: Expr): RecordField => ({
	name,
	value,
});
export const fieldAccess = (record: Expr, field: string): FieldAccess => ({
	kind: "FieldAccess",
	record,
	field,
});

export const binOp = (op: Op, left: Expr, right: Expr): BinOp => ({
	kind: "BinOp",
	op,
	left,
	right,
});

export const pwildcard: PWildcard = { kind: "PWildcard" };
export const pvar = (name: string): PVar => ({ kind: "PVar", name });
export const pcon = (name: string, ...args: readonly Pattern[]): PCon => ({
	kind: "PCon",
	args,
	name,
});
export const plit = (value: string | number | boolean): PLit => ({
	kind: "PLit",
	value,
});
export const precord = (fields: readonly PRecordField[]): PRecord => ({
	kind: "PRecord",
	fields,
});
export const pfield = (name: string, pattern: Pattern): PRecordField => ({
	name,
	pattern,
});
export const case_ = (pattern: Pattern, body: Expr): Case => ({
	pattern,
	body,
});
export const match = (expr: Expr, cases: readonly Case[]): Match => ({
	kind: "Match",
	cases,
	expr,
});

export const tyvar = (name: string): TyVar => ({ kind: "TyVar", name });
export const tycon = (name: string): TyCon => ({ kind: "TyCon", name });
export const tyapp = (con: TypeExpr, arg: TypeExpr): TyApp => ({
	kind: "TyApp",
	con,
	arg,
});
export const tyfun = (param: TypeExpr, ret: TypeExpr): TyFun => ({
	kind: "TyFun",
	param,
	ret,
});

export const conDecl = (
	name: string,
	fields: readonly TypeExpr[],
): ConDecl => ({
	name,
	fields,
});

export const dataDecl = (
	name: string,
	typeParams: string[],
	constructors: ConDecl[],
): DataDecl => ({ kind: "DataDecl", name, typeParams, constructors });
