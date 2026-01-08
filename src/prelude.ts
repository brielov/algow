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

const unit = ast.dataDecl("Unit", [], [ast.conDecl("Unit", [])]);

const ioError = ast.dataDecl(
  "IOError",
  [],
  [
    ast.conDecl("FileNotFound", [ast.tycon("string")]),
    ast.conDecl("PermissionDenied", [ast.tycon("string")]),
    ast.conDecl("IsDirectory", [ast.tycon("string")]),
    ast.conDecl("AlreadyExists", [ast.tycon("string")]),
    ast.conDecl("UnknownError", [ast.tycon("string")]),
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
    ast.case_(ast.pcon("Nil", []), ast.int(0)),
    ast.case_(
      ast.pcon("Cons", [ast.pwildcard(), ast.pvar("rest")]),
      ast.binOp("+", ast.int(1), ast.app(ast.var_("length"), ast.var_("rest"))),
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

// List.append : List a -> List a -> List a (combines two lists)
const appendExpr = ast.abs(
  "xs",
  ast.abs(
    "ys",
    ast.app(ast.app(ast.app(ast.var_("foldr"), ast.var_("Cons")), ast.var_("ys")), ast.var_("xs")),
  ),
);

// List.concat : List (List a) -> List a (flattens list of lists)
// let rec concat xss = match xss when Nil -> Nil when Cons xs rest -> append xs (concat rest) end
// where append xs ys = foldr Cons ys xs
const concatExpr = ast.abs(
  "xss",
  ast.match(ast.var_("xss"), [
    ast.case_(ast.pcon("Nil", []), ast.var_("Nil")),
    ast.case_(
      ast.pcon("Cons", [ast.pvar("xs"), ast.pvar("rest")]),
      // foldr Cons (concat rest) xs
      ast.app(
        ast.app(
          ast.app(ast.var_("foldr"), ast.var_("Cons")),
          ast.app(ast.var_("concat"), ast.var_("rest")),
        ),
        ast.var_("xs"),
      ),
    ),
  ]),
);

// List.take : Int -> List a -> List a
// let rec take n xs = if n <= 0 then Nil else match xs when Nil -> Nil when Cons x rest -> Cons x (take (n - 1) rest) end
const takeExpr = ast.abs(
  "n",
  ast.abs(
    "xs",
    ast.if_(
      ast.binOp("<=", ast.var_("n"), ast.int(0)),
      ast.var_("Nil"),
      ast.match(ast.var_("xs"), [
        ast.case_(ast.pcon("Nil", []), ast.var_("Nil")),
        ast.case_(
          ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
          ast.app(
            ast.app(ast.var_("Cons"), ast.var_("x")),
            ast.app(
              ast.app(ast.var_("take"), ast.binOp("-", ast.var_("n"), ast.int(1))),
              ast.var_("rest"),
            ),
          ),
        ),
      ]),
    ),
  ),
);

// List.drop : Int -> List a -> List a
// let rec drop n xs = if n <= 0 then xs else match xs when Nil -> Nil when Cons _ rest -> drop (n - 1) rest end
const dropExpr = ast.abs(
  "n",
  ast.abs(
    "xs",
    ast.if_(
      ast.binOp("<=", ast.var_("n"), ast.int(0)),
      ast.var_("xs"),
      ast.match(ast.var_("xs"), [
        ast.case_(ast.pcon("Nil", []), ast.var_("Nil")),
        ast.case_(
          ast.pcon("Cons", [ast.pwildcard(), ast.pvar("rest")]),
          ast.app(
            ast.app(ast.var_("drop"), ast.binOp("-", ast.var_("n"), ast.int(1))),
            ast.var_("rest"),
          ),
        ),
      ]),
    ),
  ),
);

// List.zip : List a -> List b -> List (a, b)
// let rec zip xs ys = match (xs, ys) when (Cons x xrest, Cons y yrest) -> Cons (x, y) (zip xrest yrest) when _ -> Nil end
const zipExpr = ast.abs(
  "xs",
  ast.abs(
    "ys",
    ast.match(ast.tuple([ast.var_("xs"), ast.var_("ys")]), [
      ast.case_(
        ast.ptuple([
          ast.pcon("Cons", [ast.pvar("x"), ast.pvar("xrest")]),
          ast.pcon("Cons", [ast.pvar("y"), ast.pvar("yrest")]),
        ]),
        ast.app(
          ast.app(ast.var_("Cons"), ast.tuple([ast.var_("x"), ast.var_("y")])),
          ast.app(ast.app(ast.var_("zip"), ast.var_("xrest")), ast.var_("yrest")),
        ),
      ),
      ast.case_(ast.pwildcard(), ast.var_("Nil")),
    ]),
  ),
);

// List.any : (a -> Bool) -> List a -> Bool
// let rec any p xs = match xs when Nil -> false when Cons x rest -> if p x then true else any p rest end
const anyExpr = ast.abs(
  "p",
  ast.abs(
    "xs",
    ast.match(ast.var_("xs"), [
      ast.case_(ast.pcon("Nil", []), ast.bool(false)),
      ast.case_(
        ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
        ast.if_(
          ast.app(ast.var_("p"), ast.var_("x")),
          ast.bool(true),
          ast.app(ast.app(ast.var_("any"), ast.var_("p")), ast.var_("rest")),
        ),
      ),
    ]),
  ),
);

// List.all : (a -> Bool) -> List a -> Bool
// let rec all p xs = match xs when Nil -> true when Cons x rest -> if p x then all p rest else false end
const allExpr = ast.abs(
  "p",
  ast.abs(
    "xs",
    ast.match(ast.var_("xs"), [
      ast.case_(ast.pcon("Nil", []), ast.bool(true)),
      ast.case_(
        ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
        ast.if_(
          ast.app(ast.var_("p"), ast.var_("x")),
          ast.app(ast.app(ast.var_("all"), ast.var_("p")), ast.var_("rest")),
          ast.bool(false),
        ),
      ),
    ]),
  ),
);

// List.find : (a -> Bool) -> List a -> Maybe a
// let rec find p xs = match xs when Nil -> Nothing when Cons x rest -> if p x then Just x else find p rest end
const findExpr = ast.abs(
  "p",
  ast.abs(
    "xs",
    ast.match(ast.var_("xs"), [
      ast.case_(ast.pcon("Nil", []), ast.var_("Nothing")),
      ast.case_(
        ast.pcon("Cons", [ast.pvar("x"), ast.pvar("rest")]),
        ast.if_(
          ast.app(ast.var_("p"), ast.var_("x")),
          ast.app(ast.var_("Just"), ast.var_("x")),
          ast.app(ast.app(ast.var_("find"), ast.var_("p")), ast.var_("rest")),
        ),
      ),
    ]),
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

// Bool.not : Bool -> Bool
// let not b = if b then false else true
const notExpr = ast.abs("b", ast.if_(ast.var_("b"), ast.bool(false), ast.bool(true)));

// Bool.eq : Bool -> Bool -> Bool
// let eq a b = if a then b else not b
const boolEqExpr = ast.abs(
  "a",
  ast.abs("b", ast.if_(ast.var_("a"), ast.var_("b"), ast.app(ast.var_("not"), ast.var_("b")))),
);

// =============================================================================
// MAYBE HELPER EXPRESSIONS
// =============================================================================

// Maybe.isJust : Maybe a -> Bool
const isJustExpr = ast.abs(
  "m",
  ast.match(ast.var_("m"), [
    ast.case_(ast.pcon("Nothing", []), ast.bool(false)),
    ast.case_(ast.pcon("Just", [ast.pwildcard()]), ast.bool(true)),
  ]),
);

// Maybe.isNothing : Maybe a -> Bool
const isNothingExpr = ast.abs(
  "m",
  ast.match(ast.var_("m"), [
    ast.case_(ast.pcon("Nothing", []), ast.bool(true)),
    ast.case_(ast.pcon("Just", [ast.pwildcard()]), ast.bool(false)),
  ]),
);

// Maybe.map : (a -> b) -> Maybe a -> Maybe b
const maybeMapExpr = ast.abs(
  "f",
  ast.abs(
    "m",
    ast.match(ast.var_("m"), [
      ast.case_(ast.pcon("Nothing", []), ast.var_("Nothing")),
      ast.case_(
        ast.pcon("Just", [ast.pvar("x")]),
        ast.app(ast.var_("Just"), ast.app(ast.var_("f"), ast.var_("x"))),
      ),
    ]),
  ),
);

// Maybe.flatMap : (a -> Maybe b) -> Maybe a -> Maybe b
const maybeFlatMapExpr = ast.abs(
  "f",
  ast.abs(
    "m",
    ast.match(ast.var_("m"), [
      ast.case_(ast.pcon("Nothing", []), ast.var_("Nothing")),
      ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.app(ast.var_("f"), ast.var_("x"))),
    ]),
  ),
);

// Maybe.withDefault : a -> Maybe a -> a
const maybeWithDefaultExpr = ast.abs(
  "def",
  ast.abs(
    "m",
    ast.match(ast.var_("m"), [
      ast.case_(ast.pcon("Nothing", []), ast.var_("def")),
      ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.var_("x")),
    ]),
  ),
);

// Maybe.toList : Maybe a -> List a
const maybeToListExpr = ast.abs(
  "m",
  ast.match(ast.var_("m"), [
    ast.case_(ast.pcon("Nothing", []), ast.var_("Nil")),
    ast.case_(
      ast.pcon("Just", [ast.pvar("x")]),
      ast.app(ast.app(ast.var_("Cons"), ast.var_("x")), ast.var_("Nil")),
    ),
  ]),
);

// =============================================================================
// EITHER HELPER EXPRESSIONS
// =============================================================================

// Either.isLeft : Either a b -> Bool
const isLeftExpr = ast.abs(
  "e",
  ast.match(ast.var_("e"), [
    ast.case_(ast.pcon("Left", [ast.pwildcard()]), ast.bool(true)),
    ast.case_(ast.pcon("Right", [ast.pwildcard()]), ast.bool(false)),
  ]),
);

// Either.isRight : Either a b -> Bool
const isRightExpr = ast.abs(
  "e",
  ast.match(ast.var_("e"), [
    ast.case_(ast.pcon("Left", [ast.pwildcard()]), ast.bool(false)),
    ast.case_(ast.pcon("Right", [ast.pwildcard()]), ast.bool(true)),
  ]),
);

// Either.map : (b -> c) -> Either a b -> Either a c
const eitherMapExpr = ast.abs(
  "f",
  ast.abs(
    "e",
    ast.match(ast.var_("e"), [
      ast.case_(ast.pcon("Left", [ast.pvar("a")]), ast.app(ast.var_("Left"), ast.var_("a"))),
      ast.case_(
        ast.pcon("Right", [ast.pvar("b")]),
        ast.app(ast.var_("Right"), ast.app(ast.var_("f"), ast.var_("b"))),
      ),
    ]),
  ),
);

// Either.mapLeft : (a -> c) -> Either a b -> Either c b
const eitherMapLeftExpr = ast.abs(
  "f",
  ast.abs(
    "e",
    ast.match(ast.var_("e"), [
      ast.case_(
        ast.pcon("Left", [ast.pvar("a")]),
        ast.app(ast.var_("Left"), ast.app(ast.var_("f"), ast.var_("a"))),
      ),
      ast.case_(ast.pcon("Right", [ast.pvar("b")]), ast.app(ast.var_("Right"), ast.var_("b"))),
    ]),
  ),
);

// Either.flatMap : (b -> Either a c) -> Either a b -> Either a c
const eitherFlatMapExpr = ast.abs(
  "f",
  ast.abs(
    "e",
    ast.match(ast.var_("e"), [
      ast.case_(ast.pcon("Left", [ast.pvar("a")]), ast.app(ast.var_("Left"), ast.var_("a"))),
      ast.case_(ast.pcon("Right", [ast.pvar("b")]), ast.app(ast.var_("f"), ast.var_("b"))),
    ]),
  ),
);

// Either.withDefault : b -> Either a b -> b
const eitherWithDefaultExpr = ast.abs(
  "def",
  ast.abs(
    "e",
    ast.match(ast.var_("e"), [
      ast.case_(ast.pcon("Left", [ast.pwildcard()]), ast.var_("def")),
      ast.case_(ast.pcon("Right", [ast.pvar("b")]), ast.var_("b")),
    ]),
  ),
);

// Either.fromMaybe : a -> Maybe b -> Either a b
const eitherFromMaybeExpr = ast.abs(
  "err",
  ast.abs(
    "m",
    ast.match(ast.var_("m"), [
      ast.case_(ast.pcon("Nothing", []), ast.app(ast.var_("Left"), ast.var_("err"))),
      ast.case_(ast.pcon("Just", [ast.pvar("x")]), ast.app(ast.var_("Right"), ast.var_("x"))),
    ]),
  ),
);

// =============================================================================
// MODULES
// =============================================================================

export const maybeModule = ast.moduleDecl(
  "Maybe",
  [maybe],
  [
    ast.recBinding("isJust", isJustExpr),
    ast.recBinding("isNothing", isNothingExpr),
    ast.recBinding("map", maybeMapExpr),
    ast.recBinding("flatMap", maybeFlatMapExpr),
    ast.recBinding("withDefault", maybeWithDefaultExpr),
    ast.recBinding("toList", maybeToListExpr),
  ],
);

export const eitherModule = ast.moduleDecl(
  "Either",
  [either],
  [
    ast.recBinding("isLeft", isLeftExpr),
    ast.recBinding("isRight", isRightExpr),
    ast.recBinding("map", eitherMapExpr),
    ast.recBinding("mapLeft", eitherMapLeftExpr),
    ast.recBinding("flatMap", eitherFlatMapExpr),
    ast.recBinding("withDefault", eitherWithDefaultExpr),
    ast.recBinding("fromMaybe", eitherFromMaybeExpr),
  ],
);

export const unitModule = ast.moduleDecl("Unit", [unit], []);

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
    ast.recBinding("append", appendExpr),
    ast.recBinding("concat", concatExpr),
    ast.recBinding("take", takeExpr),
    ast.recBinding("drop", dropExpr),
    ast.recBinding("zip", zipExpr),
    ast.recBinding("any", anyExpr),
    ast.recBinding("all", allExpr),
    ast.recBinding("find", findExpr),
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

// =============================================================================
// STRING MODULE (with foreign functions)
// =============================================================================

// Type helpers (must match types in checker.ts)
const tString = ast.tycon("string");
const tInt = ast.tycon("Int");
const tFloat = ast.tycon("Float");
const tBool = ast.tycon("boolean");
const tChar = ast.tycon("char");
const tUnit = ast.tycon("Unit");
const tIOError = ast.tycon("IOError");
const tMaybe = (t: ast.TypeExpr) => ast.tyapp(ast.tycon("Maybe"), t);
const tList = (t: ast.TypeExpr) => ast.tyapp(ast.tycon("List"), t);
const tEither = (a: ast.TypeExpr, b: ast.TypeExpr) =>
  ast.tyapp(ast.tyapp(ast.tycon("Either"), a), b);
const fn = (a: ast.TypeExpr, b: ast.TypeExpr) => ast.tyfun(a, b);
const tvar = (name: string) => ast.tyvar(name);

export const stringModule = ast.moduleDecl(
  "String",
  [],
  [],
  [
    // length : String -> Int
    ast.foreignBinding("length", fn(tString, tInt)),
    // concat : String -> String -> String
    ast.foreignBinding("concat", fn(tString, fn(tString, tString))),
    // substring : Int -> Int -> String -> String
    ast.foreignBinding("substring", fn(tInt, fn(tInt, fn(tString, tString)))),
    // charAt : Int -> String -> Maybe Char
    ast.foreignBinding("charAt", fn(tInt, fn(tString, tMaybe(tChar)))),
    // toList : String -> List Char
    ast.foreignBinding("toList", fn(tString, tList(tChar))),
    // fromList : List Char -> String
    ast.foreignBinding("fromList", fn(tList(tChar), tString)),
    // eq : String -> String -> Bool
    ast.foreignBinding("eq", fn(tString, fn(tString, tBool))),
    // lt : String -> String -> Bool
    ast.foreignBinding("lt", fn(tString, fn(tString, tBool))),
    // split : String -> String -> List String
    ast.foreignBinding("split", fn(tString, fn(tString, tList(tString)))),
    // join : String -> List String -> String
    ast.foreignBinding("join", fn(tString, fn(tList(tString), tString))),
    // trim : String -> String
    ast.foreignBinding("trim", fn(tString, tString)),
    // toUpper : String -> String
    ast.foreignBinding("toUpper", fn(tString, tString)),
    // toLower : String -> String
    ast.foreignBinding("toLower", fn(tString, tString)),
    // contains : String -> String -> Bool
    ast.foreignBinding("contains", fn(tString, fn(tString, tBool))),
    // startsWith : String -> String -> Bool
    ast.foreignBinding("startsWith", fn(tString, fn(tString, tBool))),
    // endsWith : String -> String -> Bool
    ast.foreignBinding("endsWith", fn(tString, fn(tString, tBool))),
    // replace : String -> String -> String -> String
    ast.foreignBinding("replace", fn(tString, fn(tString, fn(tString, tString)))),
  ],
);

// =============================================================================
// CHAR MODULE (with foreign functions)
// =============================================================================

export const charModule = ast.moduleDecl(
  "Char",
  [],
  [],
  [
    // toInt : Char -> Int
    ast.foreignBinding("toInt", fn(tChar, tInt)),
    // fromInt : Int -> Maybe Char
    ast.foreignBinding("fromInt", fn(tInt, tMaybe(tChar))),
    // toString : Char -> String
    ast.foreignBinding("toString", fn(tChar, tString)),
    // eq : Char -> Char -> Bool
    ast.foreignBinding("eq", fn(tChar, fn(tChar, tBool))),
    // lt : Char -> Char -> Bool
    ast.foreignBinding("lt", fn(tChar, fn(tChar, tBool))),
    // isDigit : Char -> Bool
    ast.foreignBinding("isDigit", fn(tChar, tBool)),
    // isAlpha : Char -> Bool
    ast.foreignBinding("isAlpha", fn(tChar, tBool)),
    // isAlphaNum : Char -> Bool
    ast.foreignBinding("isAlphaNum", fn(tChar, tBool)),
    // isSpace : Char -> Bool
    ast.foreignBinding("isSpace", fn(tChar, tBool)),
    // isUpper : Char -> Bool
    ast.foreignBinding("isUpper", fn(tChar, tBool)),
    // isLower : Char -> Bool
    ast.foreignBinding("isLower", fn(tChar, tBool)),
    // toUpper : Char -> Char
    ast.foreignBinding("toUpper", fn(tChar, tChar)),
    // toLower : Char -> Char
    ast.foreignBinding("toLower", fn(tChar, tChar)),
  ],
);

// =============================================================================
// INT MODULE (with foreign functions)
// =============================================================================

export const intModule = ast.moduleDecl(
  "Int",
  [],
  [],
  [
    // add : Int -> Int -> Int
    ast.foreignBinding("add", fn(tInt, fn(tInt, tInt))),
    // sub : Int -> Int -> Int
    ast.foreignBinding("sub", fn(tInt, fn(tInt, tInt))),
    // mul : Int -> Int -> Int
    ast.foreignBinding("mul", fn(tInt, fn(tInt, tInt))),
    // div : Int -> Int -> Int (truncating division)
    ast.foreignBinding("div", fn(tInt, fn(tInt, tInt))),
    // mod : Int -> Int -> Int
    ast.foreignBinding("mod", fn(tInt, fn(tInt, tInt))),
    // neg : Int -> Int
    ast.foreignBinding("neg", fn(tInt, tInt)),
    // abs : Int -> Int
    ast.foreignBinding("abs", fn(tInt, tInt)),
    // eq : Int -> Int -> Bool
    ast.foreignBinding("eq", fn(tInt, fn(tInt, tBool))),
    // lt : Int -> Int -> Bool
    ast.foreignBinding("lt", fn(tInt, fn(tInt, tBool))),
    // le : Int -> Int -> Bool
    ast.foreignBinding("le", fn(tInt, fn(tInt, tBool))),
    // gt : Int -> Int -> Bool
    ast.foreignBinding("gt", fn(tInt, fn(tInt, tBool))),
    // ge : Int -> Int -> Bool
    ast.foreignBinding("ge", fn(tInt, fn(tInt, tBool))),
    // toFloat : Int -> Float
    ast.foreignBinding("toFloat", fn(tInt, tFloat)),
    // toString : Int -> String
    ast.foreignBinding("toString", fn(tInt, tString)),
    // fromString : String -> Maybe Int
    ast.foreignBinding("fromString", fn(tString, tMaybe(tInt))),
  ],
);

// =============================================================================
// FLOAT MODULE (with foreign functions)
// =============================================================================

export const floatModule = ast.moduleDecl(
  "Float",
  [],
  [],
  [
    // add : Float -> Float -> Float
    ast.foreignBinding("add", fn(tFloat, fn(tFloat, tFloat))),
    // sub : Float -> Float -> Float
    ast.foreignBinding("sub", fn(tFloat, fn(tFloat, tFloat))),
    // mul : Float -> Float -> Float
    ast.foreignBinding("mul", fn(tFloat, fn(tFloat, tFloat))),
    // div : Float -> Float -> Float
    ast.foreignBinding("div", fn(tFloat, fn(tFloat, tFloat))),
    // neg : Float -> Float
    ast.foreignBinding("neg", fn(tFloat, tFloat)),
    // abs : Float -> Float
    ast.foreignBinding("abs", fn(tFloat, tFloat)),
    // eq : Float -> Float -> Bool
    ast.foreignBinding("eq", fn(tFloat, fn(tFloat, tBool))),
    // lt : Float -> Float -> Bool
    ast.foreignBinding("lt", fn(tFloat, fn(tFloat, tBool))),
    // le : Float -> Float -> Bool
    ast.foreignBinding("le", fn(tFloat, fn(tFloat, tBool))),
    // gt : Float -> Float -> Bool
    ast.foreignBinding("gt", fn(tFloat, fn(tFloat, tBool))),
    // ge : Float -> Float -> Bool
    ast.foreignBinding("ge", fn(tFloat, fn(tFloat, tBool))),
    // floor : Float -> Int
    ast.foreignBinding("floor", fn(tFloat, tInt)),
    // ceil : Float -> Int
    ast.foreignBinding("ceil", fn(tFloat, tInt)),
    // round : Float -> Int
    ast.foreignBinding("round", fn(tFloat, tInt)),
    // sqrt : Float -> Float
    ast.foreignBinding("sqrt", fn(tFloat, tFloat)),
    // pow : Float -> Float -> Float
    ast.foreignBinding("pow", fn(tFloat, fn(tFloat, tFloat))),
    // sin : Float -> Float
    ast.foreignBinding("sin", fn(tFloat, tFloat)),
    // cos : Float -> Float
    ast.foreignBinding("cos", fn(tFloat, tFloat)),
    // tan : Float -> Float
    ast.foreignBinding("tan", fn(tFloat, tFloat)),
    // log : Float -> Float
    ast.foreignBinding("log", fn(tFloat, tFloat)),
    // exp : Float -> Float
    ast.foreignBinding("exp", fn(tFloat, tFloat)),
    // toString : Float -> String
    ast.foreignBinding("toString", fn(tFloat, tString)),
    // fromString : String -> Maybe Float
    ast.foreignBinding("fromString", fn(tString, tMaybe(tFloat))),
  ],
);

// =============================================================================
// BOOL MODULE (pure Algow functions)
// =============================================================================

export const boolModule = ast.moduleDecl(
  "Bool",
  [],
  [ast.recBinding("not", notExpr), ast.recBinding("eq", boolEqExpr)],
);

// =============================================================================
// IO MODULE (with foreign functions)
// =============================================================================

export const ioModule = ast.moduleDecl(
  "IO",
  [ioError],
  [],
  [
    // print : String -> Unit
    ast.foreignBinding("print", fn(tString, tUnit)),
    // printLine : String -> Unit
    ast.foreignBinding("printLine", fn(tString, tUnit)),
    // readLine : Unit -> Either IOError String
    ast.foreignBinding("readLine", fn(tUnit, tEither(tIOError, tString))),
    // readFile : String -> Either IOError String
    ast.foreignBinding("readFile", fn(tString, tEither(tIOError, tString))),
    // writeFile : String -> String -> Either IOError Unit
    ast.foreignBinding("writeFile", fn(tString, fn(tString, tEither(tIOError, tUnit)))),
    // appendFile : String -> String -> Either IOError Unit
    ast.foreignBinding("appendFile", fn(tString, fn(tString, tEither(tIOError, tUnit)))),
    // fileExists : String -> Bool
    ast.foreignBinding("fileExists", fn(tString, tBool)),
    // deleteFile : String -> Either IOError Unit
    ast.foreignBinding("deleteFile", fn(tString, tEither(tIOError, tUnit))),
    // args : Unit -> List String
    ast.foreignBinding("args", fn(tUnit, tList(tString))),
    // exit : Int -> Unit
    ast.foreignBinding("exit", fn(tInt, tUnit)),
    // getEnv : String -> Maybe String
    ast.foreignBinding("getEnv", fn(tString, tMaybe(tString))),
  ],
);

// =============================================================================
// DEBUG MODULE (with polymorphic foreign functions)
// =============================================================================

export const debugModule = ast.moduleDecl(
  "Debug",
  [],
  [],
  [
    // log : a -> a (prints value, returns it)
    ast.foreignBinding("log", fn(tvar("a"), tvar("a"))),
    // trace : String -> a -> a (prints label + value, returns value)
    ast.foreignBinding("trace", fn(tString, fn(tvar("a"), tvar("a")))),
    // panic : String -> a (crashes with message)
    ast.foreignBinding("panic", fn(tString, tvar("a"))),
  ],
);

/** All prelude modules */
export const modules = [
  maybeModule,
  eitherModule,
  listModule,
  coreModule,
  stringModule,
  charModule,
  intModule,
  floatModule,
  unitModule,
  boolModule,
  ioModule,
  debugModule,
] as const;
