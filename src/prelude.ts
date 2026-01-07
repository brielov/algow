/**
 * Prelude - standard modules included in every program.
 *
 * Modules:
 * - Maybe: Optional values
 * - Either: Sum type for errors/results
 * - List: Linked list with common operations
 * - Core: Utility functions (id, const, compose, flip)
 */

import * as ast from "./ast";

// =============================================================================
// DATA DECLARATIONS
// =============================================================================

const maybe = ast.dataDecl(
  "Maybe",
  ["a"],
  [ast.conDecl("Nothing", []), ast.conDecl("Just", [ast.tyvar("a")])],
);

const either = ast.dataDecl(
  "Either",
  ["a", "b"],
  [ast.conDecl("Left", [ast.tyvar("a")]), ast.conDecl("Right", [ast.tyvar("b")])],
);

const list = ast.dataDecl(
  "List",
  ["a"],
  [
    ast.conDecl("Nil", []),
    ast.conDecl("Cons", [ast.tyvar("a"), ast.tyapp(ast.tycon("List"), ast.tyvar("a"))]),
  ],
);

// =============================================================================
// FUNCTION EXPRESSIONS
// =============================================================================

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

const isEmptyExpr = ast.abs(
  "xs",
  ast.match(ast.var_("xs"), [
    ast.case_(ast.pcon("Nil", []), ast.bool(true)),
    ast.case_(ast.pcon("Cons", [ast.pwildcard(), ast.pwildcard()]), ast.bool(false)),
  ]),
);

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

const concatExpr = ast.abs(
  "xs",
  ast.abs(
    "ys",
    ast.app(ast.app(ast.app(ast.var_("foldr"), ast.var_("Cons")), ast.var_("ys")), ast.var_("xs")),
  ),
);

const idExpr = ast.abs("x", ast.var_("x"));
const constExpr = ast.abs("x", ast.abs("_", ast.var_("x")));
const composeExpr = ast.abs(
  "f",
  ast.abs("g", ast.abs("x", ast.app(ast.var_("f"), ast.app(ast.var_("g"), ast.var_("x"))))),
);
const flipExpr = ast.abs(
  "f",
  ast.abs("a", ast.abs("b", ast.app(ast.app(ast.var_("f"), ast.var_("b")), ast.var_("a")))),
);

// =============================================================================
// MODULES
// =============================================================================

export const maybeModule = ast.moduleDecl("Maybe", [maybe], []);

export const eitherModule = ast.moduleDecl("Either", [either], []);

export const listModule = ast.moduleDecl(
  "List",
  [list],
  [
    ast.recBinding("map", mapExpr),
    ast.recBinding("filter", filterExpr),
    ast.recBinding("head", headExpr),
    ast.recBinding("tail", tailExpr),
    ast.recBinding("isEmpty", isEmptyExpr),
    ast.recBinding("length", lengthExpr),
    ast.recBinding("foldr", foldrExpr),
    ast.recBinding("foldl", foldlExpr),
    ast.recBinding("reverse", reverseExpr),
    ast.recBinding("concat", concatExpr),
  ],
);

export const coreModule = ast.moduleDecl(
  "Core",
  [],
  [
    ast.recBinding("id", idExpr),
    ast.recBinding("const", constExpr),
    ast.recBinding("compose", composeExpr),
    ast.recBinding("flip", flipExpr),
  ],
);

/** All prelude modules */
export const modules = [maybeModule, eitherModule, listModule, coreModule] as const;
