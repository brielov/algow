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
    ast.case_(
      ast.precord([ast.pfield("x", ast.pvar("a")), ast.pfield("y", ast.pvar("b"))]),
      ast.var_("b"),
    ),
  ]),
);

// Test: nested record pattern
printType(
  ast.match(ast.record([ast.field("inner", ast.record([ast.field("value", ast.num(99))]))]), [
    ast.case_(
      ast.precord([ast.pfield("inner", ast.precord([ast.pfield("value", ast.pvar("v"))]))]),
      ast.var_("v"),
    ),
  ]),
);

// Test: tuple (1, "hello", true) => (number, string, boolean)
printType(ast.tuple(ast.num(1), ast.str("hello"), ast.bool(true)));

// Test: 2-tuple (42, "test") => (number, string)
printType(ast.tuple(ast.num(42), ast.str("test")));

// Test: tuple pattern matching - extract first element
printType(
  ast.match(ast.tuple(ast.num(1), ast.str("hello")), [
    ast.case_(ast.ptuple([ast.pvar("a"), ast.pvar("b")]), ast.var_("a")),
  ]),
);

// Test: tuple pattern matching - extract second element
printType(
  ast.match(ast.tuple(ast.num(1), ast.str("hello")), [
    ast.case_(ast.ptuple([ast.pvar("a"), ast.pvar("b")]), ast.var_("b")),
  ]),
);

// Test: nested tuple pattern
printType(
  ast.match(ast.tuple(ast.tuple(ast.num(1), ast.num(2)), ast.str("nested")), [
    ast.case_(
      ast.ptuple([ast.ptuple([ast.pvar("x"), ast.pvar("y")]), ast.pvar("z")]),
      ast.var_("x"),
    ),
  ]),
);

// Test: row polymorphism - fn r => r.x (should work now!)
printType(ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")));

// Test: row polymorphism - applying field accessor to a record
printType(ast.app(ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")), ast.record([ast.field("x", ast.num(42)), ast.field("y", ast.str("hello"))])));

// Test: row polymorphism - fn r => r.x + r.y (multiple field access)
printType(ast.abs("r", ast.binOp("+", ast.fieldAccess(ast.var_("r"), "x"), ast.fieldAccess(ast.var_("r"), "y"))));

printType(
  ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
    ast.case_(ast.pcon("Just", ast.pvar("x")), ast.var_("x")),
    // Missing: Nothing case
  ]),
);
