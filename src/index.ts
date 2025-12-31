import * as ast from "./ast";
import { baseEnv, infer, mergeEnvs, mergeRegistries, processDataDecl, typeToString } from "./infer";

// Define List using data declaration
const listDecl = ast.dataDecl(
  "List",
  ["a"],
  [
    ast.conDecl("Nil", []),
    ast.conDecl("Cons", [ast.tyvar("a"), ast.tyapp(ast.tycon("List"), ast.tyvar("a"))]),
  ],
);

// Define Maybe using data declaration
const maybeDecl = ast.dataDecl(
  "Maybe",
  ["a"],
  [ast.conDecl("Nothing", []), ast.conDecl("Just", [ast.tyvar("a")])],
);

// data Either a b = Left a | Right b
const eitherDecl = ast.dataDecl(
  "Either",
  ["a", "b"],
  [ast.conDecl("Left", [ast.tyvar("a")]), ast.conDecl("Right", [ast.tyvar("b")])],
);

// Process declarations and merge with base env (for map, filter, etc.)
const [listEnv, listReg] = processDataDecl(listDecl);
const [maybeEnv, maybeReg] = processDataDecl(maybeDecl);
const [eitherEnv, eitherReg] = processDataDecl(eitherDecl);
const env = mergeEnvs(baseEnv, listEnv, maybeEnv, eitherEnv);
const registry = mergeRegistries(listReg, maybeReg, eitherReg);

function printType(expr: ast.Expr): void {
  const [, type] = infer(env, registry, expr);
  console.log(typeToString(type));
}

// Test: Nil from declaration
printType(ast.var_("Nil"));

// Test: Cons 1 Nil
printType(ast.app(ast.app(ast.var_("Cons"), ast.num(1)), ast.var_("Nil")));

// Test: Just "hello"
printType(ast.app(ast.var_("Just"), ast.str("hello")));

// Test: pattern match on declared types
printType(
  ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
    ast.case_(ast.pcon("Just", ast.pvar("x")), ast.var_("x")),
    ast.case_(ast.pcon("Nothing"), ast.num(0)),
  ]),
);

// Test: Left 42 => Either number t0
printType(ast.app(ast.var_("Left"), ast.num(42)));

// Test: Right "hello" => Either t0 string
printType(ast.app(ast.var_("Right"), ast.str("hello")));

// Test: simple record { x: 1, y: "hello" }
printType(ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.str("hello"))]));

// Test: field access { x: 42, y: "test" }.x => number
printType(
  ast.fieldAccess(ast.record([ast.field("x", ast.num(42)), ast.field("y", ast.str("test"))]), "x"),
);

// Test: field access on y => string
printType(
  ast.fieldAccess(ast.record([ast.field("x", ast.num(42)), ast.field("y", ast.str("test"))]), "y"),
);

// Test: pattern match on record - extract x field
printType(
  ast.match(ast.record([ast.field("x", ast.num(42)), ast.field("y", ast.str("hello"))]), [
    ast.case_(ast.precord([ast.pfield("x", ast.pvar("a"))]), ast.var_("a")),
  ]),
);

// Test: pattern match on record - extract both fields, return y
printType(
  ast.match(ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.bool(true))]), [
    ast.case_(ast.precord([ast.pfield("x", ast.pvar("a")), ast.pfield("y", ast.pvar("b"))]), ast.var_("b")),
  ]),
);

// Test: nested record pattern
printType(
  ast.match(
    ast.record([ast.field("inner", ast.record([ast.field("value", ast.num(99))]))]),
    [ast.case_(ast.precord([ast.pfield("inner", ast.precord([ast.pfield("value", ast.pvar("v"))]))]), ast.var_("v"))],
  ),
);

printType(
  ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
    ast.case_(ast.pcon("Just", ast.pvar("x")), ast.var_("x")),
    // Missing: Nothing case
  ]),
);
