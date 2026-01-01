/**
 * Prelude - standard data types and functions included in every program.
 */

import * as ast from "./ast";

// =============================================================================
// Data Declarations
// =============================================================================

/** data Maybe a = Nothing | Just a */
export const maybe = ast.dataDecl("Maybe", ["a"], [
  ast.conDecl("Nothing", []),
  ast.conDecl("Just", [ast.tyvar("a")]),
]);

/** data Either a b = Left a | Right b */
export const either = ast.dataDecl("Either", ["a", "b"], [
  ast.conDecl("Left", [ast.tyvar("a")]),
  ast.conDecl("Right", [ast.tyvar("b")]),
]);

/** data List a = Nil | Cons a (List a) */
export const list = ast.dataDecl("List", ["a"], [
  ast.conDecl("Nil", []),
  ast.conDecl("Cons", [ast.tyvar("a"), ast.tyapp(ast.tycon("List"), ast.tyvar("a"))]),
]);

export const declarations = [maybe, either, list] as const;

// =============================================================================
// List Functions
// =============================================================================

/** map = fn f => fn xs => match xs with Nil => Nil | Cons x rest => Cons (f x) (map f rest) */
export const map = ast.letRec(
  "map",
  ast.abs(
    "f",
    ast.abs(
      "xs",
      ast.match(ast.var_("xs"), [
        ast.case_(ast.pcon("Nil"), ast.var_("Nil")),
        ast.case_(
          ast.pcon("Cons", ast.pvar("x"), ast.pvar("rest")),
          ast.app(
            ast.app(ast.var_("Cons"), ast.app(ast.var_("f"), ast.var_("x"))),
            ast.app(ast.app(ast.var_("map"), ast.var_("f")), ast.var_("rest")),
          ),
        ),
      ]),
    ),
  ),
  ast.var_("map"),
);

/** filter = fn p => fn xs => match xs with Nil => Nil | Cons x rest => if p x then Cons x (filter p rest) else filter p rest */
export const filter = ast.letRec(
  "filter",
  ast.abs(
    "p",
    ast.abs(
      "xs",
      ast.match(ast.var_("xs"), [
        ast.case_(ast.pcon("Nil"), ast.var_("Nil")),
        ast.case_(
          ast.pcon("Cons", ast.pvar("x"), ast.pvar("rest")),
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
  ),
  ast.var_("filter"),
);

/** head = fn xs => match xs with Nil => Nothing | Cons x _ => Just x */
export const head = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil"), ast.var_("Nothing")),
    ast.case_(ast.pcon("Cons", ast.pvar("x"), ast.pwildcard), ast.app(ast.var_("Just"), ast.var_("x"))),
  ]),
);

/** tail = fn xs => match xs with Nil => Nothing | Cons _ rest => Just rest */
export const tail = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil"), ast.var_("Nothing")),
    ast.case_(ast.pcon("Cons", ast.pwildcard, ast.pvar("rest")), ast.app(ast.var_("Just"), ast.var_("rest"))),
  ]),
);

/** isEmpty = fn xs => match xs with Nil => true | Cons _ _ => false */
export const isEmpty = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil"), ast.bool(true)),
    ast.case_(ast.pcon("Cons", ast.pwildcard, ast.pwildcard), ast.bool(false)),
  ]),
);

/** length = fn xs => match xs with Nil => 0 | Cons _ rest => 1 + length rest */
export const length = ast.letRec(
  "length",
  ast.abs(
    "xs",
    ast.match(ast.var_("xs"), [
      ast.case_(ast.pcon("Nil"), ast.num(0)),
      ast.case_(
        ast.pcon("Cons", ast.pwildcard, ast.pvar("rest")),
        ast.binOp("+", ast.num(1), ast.app(ast.var_("length"), ast.var_("rest"))),
      ),
    ]),
  ),
  ast.var_("length"),
);

/** foldr = fn f => fn z => fn xs => match xs with Nil => z | Cons x rest => f x (foldr f z rest) */
export const foldr = ast.letRec(
  "foldr",
  ast.abs(
    "f",
    ast.abs(
      "z",
      ast.abs(
        "xs",
        ast.match(ast.var_("xs"), [
          ast.case_(ast.pcon("Nil"), ast.var_("z")),
          ast.case_(
            ast.pcon("Cons", ast.pvar("x"), ast.pvar("rest")),
            ast.app(
              ast.app(ast.var_("f"), ast.var_("x")),
              ast.app(ast.app(ast.app(ast.var_("foldr"), ast.var_("f")), ast.var_("z")), ast.var_("rest")),
            ),
          ),
        ]),
      ),
    ),
  ),
  ast.var_("foldr"),
);

/** foldl = fn f => fn z => fn xs => match xs with Nil => z | Cons x rest => foldl f (f z x) rest */
export const foldl = ast.letRec(
  "foldl",
  ast.abs(
    "f",
    ast.abs(
      "z",
      ast.abs(
        "xs",
        ast.match(ast.var_("xs"), [
          ast.case_(ast.pcon("Nil"), ast.var_("z")),
          ast.case_(
            ast.pcon("Cons", ast.pvar("x"), ast.pvar("rest")),
            ast.app(
              ast.app(ast.app(ast.var_("foldl"), ast.var_("f")), ast.app(ast.app(ast.var_("f"), ast.var_("z")), ast.var_("x"))),
              ast.var_("rest"),
            ),
          ),
        ]),
      ),
    ),
  ),
  ast.var_("foldl"),
);

/** reverse = foldl (fn acc => fn x => Cons x acc) Nil */
export const reverse = ast.abs(
  "xs",
  ast.app(
    ast.app(
      ast.app(foldl, ast.abs("acc", ast.abs("x", ast.app(ast.app(ast.var_("Cons"), ast.var_("x")), ast.var_("acc"))))),
      ast.var_("Nil"),
    ),
    ast.var_("xs"),
  ),
);

/** concat = fn xs => fn ys => foldr Cons ys xs */
export const concat = ast.abs(
  "xs",
  ast.abs(
    "ys",
    ast.app(ast.app(ast.app(foldr, ast.var_("Cons")), ast.var_("ys")), ast.var_("xs")),
  ),
);

/** id = fn x => x */
export const id = ast.abs("x", ast.var_("x"));

/** const = fn x => fn _ => x */
export const const_ = ast.abs("x", ast.abs("_", ast.var_("x")));

/** compose = fn f => fn g => fn x => f (g x) */
export const compose = ast.abs(
  "f",
  ast.abs("g", ast.abs("x", ast.app(ast.var_("f"), ast.app(ast.var_("g"), ast.var_("x"))))),
);

/** flip = fn f => fn a => fn b => f b a */
export const flip = ast.abs(
  "f",
  ast.abs("a", ast.abs("b", ast.app(ast.app(ast.var_("f"), ast.var_("b")), ast.var_("a")))),
);

export const functions = {
  map,
  filter,
  head,
  tail,
  isEmpty,
  length,
  foldr,
  foldl,
  reverse,
  concat,
  id,
  const: const_,
  compose,
  flip,
} as const;
