import * as ast from "./ast";
import { infer, mergeEnvs, mergeRegistries, processDataDecl, typeToString } from "./infer";
import { declarations } from "./prelude";

// Process prelude declarations
let env = new Map() as ReturnType<typeof processDataDecl>[0];
let registry = new Map() as ReturnType<typeof processDataDecl>[1];
for (const decl of declarations) {
  const [e, r] = processDataDecl(decl);
  env = mergeEnvs(env, e);
  registry = mergeRegistries(registry, r);
}

function printType(label: string, expr: ast.Expr): void {
  const { type, diagnostics } = infer(env, registry, expr);
  if (diagnostics.length > 0) {
    console.log(`${label}: ERROR - ${diagnostics.map((d) => d.message).join(", ")}`);
  } else {
    console.log(`${label}: ${typeToString(type)}`);
  }
}

// Test constructors
printType("Nil", ast.var_("Nil"));
printType("Cons 1 Nil", ast.app(ast.app(ast.var_("Cons"), ast.num(1)), ast.var_("Nil")));
printType("Just 'hello'", ast.app(ast.var_("Just"), ast.str("hello")));
printType("Left 42", ast.app(ast.var_("Left"), ast.num(42)));
printType("Right 'hello'", ast.app(ast.var_("Right"), ast.str("hello")));

// Test pattern matching
printType(
  "match Just 42",
  ast.match(ast.app(ast.var_("Just"), ast.num(42)), [
    ast.case_(ast.pcon("Just", ast.pvar("x")), ast.var_("x")),
    ast.case_(ast.pcon("Nothing"), ast.num(0)),
  ]),
);

// Test records
printType(
  "{ x: 1, y: 'hello' }",
  ast.record([ast.field("x", ast.num(1)), ast.field("y", ast.str("hello"))]),
);
printType("record.x", ast.fieldAccess(ast.record([ast.field("x", ast.num(42))]), "x"));

// Test row polymorphism
printType("fn r => r.x", ast.abs("r", ast.fieldAccess(ast.var_("r"), "x")));

// Test tuples
printType("(1, 'hello', true)", ast.tuple(ast.num(1), ast.str("hello"), ast.bool(true)));
