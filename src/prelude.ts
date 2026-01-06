/**
 * Prelude - standard data types and functions included in every program.
 *
 * Structured as modules:
 * - Maybe: Maybe type and operations
 * - Either: Either type
 * - List: List type and operations (map, filter, foldr, foldl, etc.)
 * - Core: Utility functions (id, const, compose, flip)
 */

import * as ast from "./ast";

// =============================================================================
// Data Declarations
// =============================================================================

/** type Maybe a = Nothing | Just a */
export const maybe = ast.dataDecl(
  "Maybe",
  ["a"],
  [ast.conDecl("Nothing", []), ast.conDecl("Just", [ast.tyvar("a")])],
);

/** type Either a b = Left a | Right b */
export const either = ast.dataDecl(
  "Either",
  ["a", "b"],
  [ast.conDecl("Left", [ast.tyvar("a")]), ast.conDecl("Right", [ast.tyvar("b")])],
);

/** type List a = Nil | Cons a (List a) */
export const list = ast.dataDecl(
  "List",
  ["a"],
  [
    ast.conDecl("Nil", []),
    ast.conDecl("Cons", [ast.tyvar("a"), ast.tyapp(ast.tycon("List"), ast.tyvar("a"))]),
  ],
);

export const declarations = [maybe, either, list] as const;

// =============================================================================
// Function Expressions (for use in modules)
// =============================================================================

/** map f xs = match xs when Nil -> Nil | Cons x rest -> Cons (f x) (map f rest) end */
const mapExpr = ast.abs(
  "f",
  ast.abs(
    "xs",
    ast.match(ast.var_("xs"), [
      ast.case_(ast.pcon("Nil", []), ast.var_("Nil")),
      ast.case_(
        ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
        ast.app(
          ast.app(ast.var_("Cons"), ast.app(ast.var_("f"), ast.var_("x"))),
          ast.app(ast.app(ast.var_("map"), ast.var_("f")), ast.var_("rest")),
        ),
      ),
    ]),
  ),
);

/** filter p xs = match xs when Nil -> Nil | Cons x rest -> if p x then Cons x (filter p rest) else filter p rest end */
const filterExpr = ast.abs(
  "p",
  ast.abs(
    "xs",
    ast.match(ast.var_("xs"), [
      ast.case_(ast.pcon("Nil", []), ast.var_("Nil")),
      ast.case_(
        ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
        ast.if_(
          ast.app(ast.var_("p"), ast.var_("x")),
          ast.app(
            ast.app(ast.var_("Cons"), ast.var_("x")),
            ast.app(ast.app(ast.var_("filter"), ast.var_("p")), ast.var_("rest")),
          ),
          ast.app(ast.app(ast.var_("filter"), ast.var_("p")), ast.var_("rest")),
        ),
      ),
    ]),
  ),
);

/** head xs = match xs when Nil -> Nothing | Cons x _ -> Just x end */
const headExpr = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil", []), ast.var_("Nothing")),
    ast.case_(
      ast.pcon("Cons", [ast.pvar("x"), ast.pwildcard()]),
      ast.app(ast.var_("Just"), ast.var_("x")),
    ),
  ]),
);

/** tail xs = match xs when Nil -> Nothing | Cons _ rest -> Just rest end */
const tailExpr = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil", []), ast.var_("Nothing")),
    ast.case_(
      ast.pcon("Cons", [ast.pwildcard(), ast.pvar("rest")]),
      ast.app(ast.var_("Just"), ast.var_("rest")),
    ),
  ]),
);

/** isEmpty xs = match xs when Nil -> true | Cons _ _ -> false end */
const isEmptyExpr = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil", []), ast.bool(true)),
    ast.case_(ast.pcon("Cons", [ast.pwildcard(), ast.pwildcard()]), ast.bool(false)),
  ]),
);

/** length xs = match xs when Nil -> 0 | Cons _ rest -> 1 + length rest end */
const lengthExpr = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil", []), ast.num(0)),
    ast.case_(
      ast.pcon("Cons", [ast.pwildcard(), ast.pvar("rest")]),
      ast.binOp("+", ast.num(1), ast.app(ast.var_("length"), ast.var_("rest"))),
    ),
  ]),
);

/** foldr f z xs = match xs when Nil -> z | Cons x rest -> f x (foldr f z rest) end */
const foldrExpr = ast.abs(
  "f",
  ast.abs(
    "z",
    ast.abs(
      "xs",
      ast.match(ast.var_("xs"), [
        ast.case_(ast.pcon("Nil", []), ast.var_("z")),
        ast.case_(
          ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
          ast.app(
            ast.app(ast.var_("f"), ast.var_("x")),
            ast.app(
              ast.app(ast.app(ast.var_("foldr"), ast.var_("f")), ast.var_("z")),
              ast.var_("rest"),
            ),
          ),
        ),
      ]),
    ),
  ),
);

/** foldl f z xs = match xs when Nil -> z | Cons x rest -> foldl f (f z x) rest end */
const foldlExpr = ast.abs(
  "f",
  ast.abs(
    "z",
    ast.abs(
      "xs",
      ast.match(ast.var_("xs"), [
        ast.case_(ast.pcon("Nil", []), ast.var_("z")),
        ast.case_(
          ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
          ast.app(
            ast.app(
              ast.app(ast.var_("foldl"), ast.var_("f")),
              ast.app(ast.app(ast.var_("f"), ast.var_("z")), ast.var_("x")),
            ),
            ast.var_("rest"),
          ),
        ),
      ]),
    ),
  ),
);

/** reverse xs = foldl (acc x -> Cons x acc) Nil xs */
const reverseExpr = ast.abs(
  "xs",
  ast.app(
    ast.app(
      ast.app(
        ast.var_("foldl"),
        ast.abs(
          "acc",
          ast.abs("x", ast.app(ast.app(ast.var_("Cons"), ast.var_("x")), ast.var_("acc"))),
        ),
      ),
      ast.var_("Nil"),
    ),
    ast.var_("xs"),
  ),
);

/** concat xs ys = foldr Cons ys xs */
const concatExpr = ast.abs(
  "xs",
  ast.abs(
    "ys",
    ast.app(ast.app(ast.app(ast.var_("foldr"), ast.var_("Cons")), ast.var_("ys")), ast.var_("xs")),
  ),
);

/** id x = x */
const idExpr = ast.abs("x", ast.var_("x"));

/** const x _ = x */
const constExpr = ast.abs("x", ast.abs("_", ast.var_("x")));

/** compose f g x = f (g x) */
const composeExpr = ast.abs(
  "f",
  ast.abs("g", ast.abs("x", ast.app(ast.var_("f"), ast.app(ast.var_("g"), ast.var_("x"))))),
);

/** flip f a b = f b a */
const flipExpr = ast.abs(
  "f",
  ast.abs("a", ast.abs("b", ast.app(ast.app(ast.var_("f"), ast.var_("b")), ast.var_("a")))),
);

// =============================================================================
// Prelude Function Names (for LSP completion)
// =============================================================================

export const preludeFunctionNames = [
  "map",
  "filter",
  "head",
  "tail",
  "isEmpty",
  "length",
  "foldr",
  "foldl",
  "reverse",
  "concat",
  "id",
  "const",
  "compose",
  "flip",
] as const;

// =============================================================================
// Function Expressions (for testing)
// =============================================================================

export const preludeFunctionExprs = {
  map: ast.letRec([ast.recBinding("map", mapExpr)], ast.var_("map")),
  filter: ast.letRec([ast.recBinding("filter", filterExpr)], ast.var_("filter")),
  head: headExpr,
  tail: tailExpr,
  isEmpty: isEmptyExpr,
  length: ast.letRec([ast.recBinding("length", lengthExpr)], ast.var_("length")),
  foldr: ast.letRec([ast.recBinding("foldr", foldrExpr)], ast.var_("foldr")),
  foldl: ast.letRec([ast.recBinding("foldl", foldlExpr)], ast.var_("foldl")),
  reverse: reverseExpr,
  concat: concatExpr,
  id: idExpr,
  const: constExpr,
  compose: composeExpr,
  flip: flipExpr,
} as const;

/**
 * Wrap an expression with all prelude function bindings.
 * This makes map, filter, foldr, foldl, etc. available in the expression.
 */
export const wrapWithPrelude = (expr: ast.Expr): ast.Expr => {
  // Wrap with simple functions (non-recursive)
  let result: ast.Expr = ast.let_("flip", flipExpr, expr);
  result = ast.let_("compose", composeExpr, result);
  result = ast.let_("const", constExpr, result);
  result = ast.let_("id", idExpr, result);
  result = ast.let_("concat", concatExpr, result);
  result = ast.let_("reverse", reverseExpr, result);

  // Wrap with recursive functions
  result = ast.letRec([ast.recBinding("foldl", foldlExpr)], result);
  result = ast.letRec([ast.recBinding("foldr", foldrExpr)], result);
  result = ast.letRec([ast.recBinding("length", lengthExpr)], result);
  result = ast.let_("isEmpty", isEmptyExpr, result);
  result = ast.let_("tail", tailExpr, result);
  result = ast.let_("head", headExpr, result);
  result = ast.letRec([ast.recBinding("filter", filterExpr)], result);
  result = ast.letRec([ast.recBinding("map", mapExpr)], result);

  return result;
};
