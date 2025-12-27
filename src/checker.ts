import * as ast from "./ast";

type Type =
	| {
			readonly kind: "TVar";
			readonly name: string;
	  }
	| {
			readonly kind: "TCon";
			readonly name: string;
	  }
	| {
			readonly kind: "TFun";
			readonly param: Type;
			readonly ret: Type;
	  };

const tvar = (name: string): Type => ({ kind: "TVar", name });
const tcon = (name: string): Type => ({ kind: "TCon", name });
const tfun = (param: Type, ret: Type): Type => ({ kind: "TFun", param, ret });

const tNum = tcon("number");
const tStr = tcon("string");
const tBool = tcon("boolean");

type Constraint = {
	readonly class_: string;
	readonly type: Type;
};

type Scheme = {
	readonly vars: readonly string[];
	readonly constraints: readonly Constraint[];
	readonly type: Type;
};

const scheme = (
	vars: readonly string[],
	type: Type,
	constraints: readonly Constraint[] = [],
): Scheme => ({
	vars,
	constraints,
	type,
});

type TypeEnv = Map<string, Scheme>;
type Subst = Map<string, Type>;

const instances: Map<string, Set<string>> = new Map([
	["Eq", new Set(["number", "string", "boolean"])],
	["Ord", new Set(["number", "string"])],
	["Add", new Set(["number", "string"])],
]);

const applySubst = (subst: Subst, type: Type): Type => {
	switch (type.kind) {
		case "TCon":
			return type;
		case "TVar":
			return subst.get(type.name) ?? type;
		case "TFun":
			return tfun(applySubst(subst, type.param), applySubst(subst, type.ret));
	}
};

const applySubstScheme = (subst: Subst, scheme_: Scheme): Scheme => {
	const filtered: Subst = new Map();
	for (const [name, type] of subst) {
		if (!scheme_.vars.includes(name)) {
			filtered.set(name, type);
		}
	}
	return scheme(scheme_.vars, applySubst(filtered, scheme_.type));
};

const applySubstEnv = (subst: Subst, env: TypeEnv): TypeEnv => {
	const result: TypeEnv = new Map();
	for (const [name, scheme] of env) {
		result.set(name, applySubstScheme(subst, scheme));
	}
	return result;
};

const applySubstConstraint = (subst: Subst, c: Constraint): Constraint => ({
	class_: c.class_,
	type: applySubst(subst, c.type),
});

const applySubstConstraints = (
	subst: Subst,
	cs: readonly Constraint[],
): Constraint[] => cs.map((c) => applySubstConstraint(subst, c));

const composeSubst = (s1: Subst, s2: Subst): Subst => {
	const result: Subst = new Map();

	for (const [name, type] of s1) {
		result.set(name, applySubst(s2, type));
	}

	for (const [name, type] of s2) {
		if (!result.has(name)) {
			result.set(name, type);
		}
	}

	return result;
};

const unify = (t1: Type, t2: Type): Subst => {
	if (t1.kind === "TVar" && t2.kind === "TVar" && t1.name === t2.name) {
		return new Map();
	}
	if (t1.kind === "TVar") {
		return bindVar(t1.name, t2);
	}
	if (t2.kind === "TVar") {
		return bindVar(t2.name, t1);
	}
	if (t1.kind === "TCon" && t2.kind === "TCon" && t1.name === t2.name) {
		return new Map();
	}
	if (t1.kind === "TFun" && t2.kind === "TFun") {
		const s1 = unify(t1.param, t2.param);
		const s2 = unify(applySubst(s1, t1.ret), applySubst(s1, t2.ret));
		return composeSubst(s1, s2);
	}

	throw new TypeError(
		`Cannot unify ${typeToString(t1)} with ${typeToString(t2)}`,
	);
};

const bindVar = (name: string, type: Type): Subst => {
	if (type.kind === "TVar" && type.name === name) {
		return new Map();
	}
	if (freeTypeVars(type).has(name)) {
		throw new TypeError(
			`Infinite type: ${name} appears in ${typeToString(type)}`,
		);
	}
	return new Map([[name, type]]);
};

let typeVarCounter = 0;

const freshTypeVar = (): Type => {
	return tvar(`t${typeVarCounter++}`);
};

const instantiate = (scheme: Scheme): Type => {
	const freshVars = new Map<string, Type>();
	for (const name of scheme.vars) {
		freshVars.set(name, freshTypeVar());
	}
	return applySubst(freshVars, scheme.type);
};

const generalize = (env: TypeEnv, type: Type): Scheme => {
	const typeVars = freeTypeVars(type);
	const envVars = freeEnvVars(env);
	const vars = typeVars.difference(envVars);
	return scheme([...vars], type);
};

const freeTypeVars = (type: Type): Set<string> => {
	switch (type.kind) {
		case "TCon":
			return new Set();
		case "TVar":
			return new Set([type.name]);
		case "TFun":
			return freeTypeVars(type.param).union(freeTypeVars(type.ret));
	}
};

const freeSchemeVars = (scheme: Scheme): Set<string> => {
	return freeTypeVars(scheme.type).difference(new Set(scheme.vars));
};

const freeEnvVars = (type: TypeEnv): Set<string> => {
	let result = new Set<string>();
	for (const scheme of type.values()) {
		result = result.union(freeSchemeVars(scheme));
	}
	return result;
};

const typeToString = (type: Type): string => {
	switch (type.kind) {
		case "TVar":
			return type.name;
		case "TCon":
			return type.name;
		case "TFun": {
			const param =
				type.param.kind === "TFun"
					? `(${typeToString(type.param)})`
					: typeToString(type.param);
			return `${param} -> ${typeToString(type.ret)}`;
		}
	}
};

const solveConstraints = (constraints: readonly Constraint[]): void => {
	for (const c of constraints) {
		if (c.type.kind === "TVar") {
			continue;
		}

		if (c.type.kind === "TCon") {
			const classInstances = instances.get(c.class_);
			if (!classInstances?.has(c.type.name)) {
				throw new TypeError(
					`Type '${c.type.name}' does not satisfy ${c.class_}`,
				);
			}
		}

		if (c.type.kind === "TFun") {
			throw new TypeError(`Function types does not satisfy ${c.class_}`);
		}
	}
};

export const infer = (env: TypeEnv, expr: ast.Expr): InferResult => {
	typeVarCounter = 0;
	const [subst, type, constraints] = inferExpr(env, expr);
	const finalConstraints = applySubstConstraints(subst, constraints);
	solveConstraints(finalConstraints);
	return [subst, type, finalConstraints];
};

type InferResult = [Subst, Type, readonly Constraint[]];

const inferExpr = (env: TypeEnv, expr: ast.Expr): InferResult => {
	switch (expr.kind) {
		case "Abs":
			return inferAbs(env, expr);
		case "App":
			return inferApp(env, expr);
		case "BinOp":
			return inferBinOp(env, expr);
		case "Bool":
			return inferBool();
		case "If":
			return inferIf(env, expr);
		case "Let":
			return inferLet(env, expr);
		case "LetRec":
			return inferLetRec(env, expr);
		case "Num":
			return inferNum();
		case "Str":
			return inferStr();
		case "Var":
			return inferVar(env, expr);
	}
};

const inferBool = (): InferResult => [new Map(), tBool, []];
const inferStr = (): InferResult => [new Map(), tStr, []];
const inferNum = (): InferResult => [new Map(), tNum, []];

const inferAbs = (env: TypeEnv, expr: ast.Abs): InferResult => {
	const paramType = freshTypeVar();
	const newEnv = new Map(env);
	newEnv.set(expr.param, scheme([], paramType));
	const [subst, bodyType, constraints] = inferExpr(newEnv, expr.body);
	return [subst, tfun(applySubst(subst, paramType), bodyType), constraints];
};

const inferApp = (env: TypeEnv, expr: ast.App): InferResult => {
	const [s1, funcType, c1] = inferExpr(env, expr.func);
	const [s2, paramType, c2] = inferExpr(applySubstEnv(s1, env), expr.param);
	const returnType = freshTypeVar();
	const s3 = unify(applySubst(s2, funcType), tfun(paramType, returnType));
	const subst = composeSubst(composeSubst(s1, s2), s3);
	const constraints = applySubstConstraints(subst, [...c1, ...c2]);
	return [subst, applySubst(s3, returnType), constraints];
};

const inferBinOp = (env: TypeEnv, expr: ast.BinOp): InferResult => {
	const [s1, leftType, c1] = inferExpr(env, expr.left);
	const [s2, rightType, c2] = inferExpr(applySubstEnv(s1, env), expr.right);
	const s3 = unify(applySubst(s2, leftType), rightType);
	const operandType = applySubst(s3, rightType);
	const subst = composeSubst(composeSubst(s1, s2), s3);
	const constraints = [
		...applySubstConstraints(subst, c1),
		...applySubstConstraints(subst, c2),
	];
	switch (expr.op) {
		case "+": {
			constraints.push({ class_: "Add", type: operandType });
			return [subst, operandType, constraints];
		}
		case "-":
		case "/":
		case "*": {
			const s4 = unify(operandType, tNum);
			return [composeSubst(subst, s4), tNum, constraints];
		}
		case "<":
		case ">":
		case "<=":
		case ">=": {
			constraints.push({ class_: "Ord", type: operandType });
			return [subst, tBool, constraints];
		}
		case "==":
		case "!=": {
			constraints.push({ class_: "Eq", type: operandType });
			return [subst, tBool, constraints];
		}
	}
};

const inferIf = (env: TypeEnv, expr: ast.If): InferResult => {
	const [s1, condType, c1] = inferExpr(env, expr.cond);
	const s2 = unify(condType, tBool);
	let subst = composeSubst(s1, s2);
	const [s3, thenType, c2] = inferExpr(applySubstEnv(subst, env), expr.then);
	subst = composeSubst(subst, s3);
	const [s4, elseType, c3] = inferExpr(applySubstEnv(subst, env), expr.else);
	subst = composeSubst(subst, s4);
	const s5 = unify(applySubst(s4, thenType), elseType);
	const finalSubst = composeSubst(subst, s5);
	const constraints = applySubstConstraints(finalSubst, [...c1, ...c2, ...c3]);
	return [finalSubst, applySubst(s5, elseType), constraints];
};

const inferLet = (env: TypeEnv, expr: ast.Let): InferResult => {
	const [s1, valueType, c1] = inferExpr(env, expr.value);
	const env1 = applySubstEnv(s1, env);
	const scheme = generalize(env1, applySubst(s1, valueType));
	const env2 = new Map(env1);
	env2.set(expr.name, scheme);
	const [s2, bodyType, c2] = inferExpr(env2, expr.body);
	const subst = composeSubst(s1, s2);
	const constraints = applySubstConstraints(subst, [...c1, ...c2]);
	return [subst, bodyType, constraints];
};

const inferLetRec = (env: TypeEnv, expr: ast.LetRec): InferResult => {
	const newEnv = new Map(env);
	newEnv.set(expr.name, scheme([], freshTypeVar()));
	const newExpr = ast.let_(expr.name, expr.value, expr.body);
	return inferLet(newEnv, newExpr);
};

const inferVar = (env: TypeEnv, expr: ast.Var): InferResult => {
	const scheme = env.get(expr.name);
	if (!scheme) {
		throw new TypeError(`Unknown variable: ${expr.name}`);
	}
	return [new Map(), instantiate(scheme), []];
};
