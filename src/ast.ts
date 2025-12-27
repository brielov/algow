// oxlint-disable no-thenable

export type Expr = Con | Let | LetRec | Var | Abs | App | If | BinOp;
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

export const binOp = (op: Op, left: Expr, right: Expr): BinOp => ({
	kind: "BinOp",
	op,
	left,
	right,
});
