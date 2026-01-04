// oxlint-disable no-thenable

/**
 * Abstract Syntax Tree (AST) definitions for the Algow type checker.
 *
 * This module defines the structure of programs that our type checker can analyze.
 * The AST represents programs as tree structures where each node corresponds to
 * a language construct (expressions, patterns, types, etc.).
 *
 * The language supports:
 * - Primitive types: numbers, booleans, strings
 * - Functions: lambda abstractions and function application
 * - Let bindings: both regular (polymorphic) and recursive
 * - Pattern matching: with constructor, literal, record, and tuple patterns
 * - Algebraic data types: user-defined sum types like List, Maybe, Either
 * - Records: structural record types with field access
 * - Tuples: fixed-length heterogeneous collections
 *
 * Design notes:
 * - All types use a discriminated union pattern with a `kind` field for type-safe matching
 * - All fields are readonly to enforce immutability
 * - Helper functions are provided to construct AST nodes concisely
 * - Spans are optional to support programmatic AST construction (e.g., prelude)
 */

// =============================================================================
// SOURCE LOCATIONS
// =============================================================================

/**
 * A span represents a range in the source code.
 * Uses byte offsets for efficient lookup.
 */
export interface Span {
  readonly start: number;
  readonly end: number;
}

/**
 * Base interface for all AST nodes.
 * Span is optional to support programmatic AST construction (e.g., prelude).
 */
export interface Node {
  readonly span?: Span;
}

// =============================================================================
// EXPRESSIONS
// =============================================================================

/**
 * The root expression type - a discriminated union of all possible expressions.
 *
 * Each expression variant has a unique `kind` field that TypeScript uses to
 * narrow the type in switch statements and conditionals. This pattern is called
 * a "tagged union" or "discriminated union".
 */
export type Expr =
  | Con // Constants (literals)
  | Let // Let bindings (with polymorphism)
  | LetRec // Recursive let bindings
  | Var // Variable references
  | Abs // Lambda abstractions (anonymous functions)
  | App // Function application
  | Tuple // Tuples: (a, b, c)
  | Record // Records: { x: 1, y: "hello" }
  | FieldAccess // Field access: record.field
  | TupleIndex // Tuple indexing: tuple.0, tuple.1
  | If // Conditional expressions
  | BinOp // Binary operators: +, -, ==, <, etc.
  | Match; // Pattern matching expressions

// -----------------------------------------------------------------------------
// Constants (Literals)
// -----------------------------------------------------------------------------

/**
 * Constants are literal values that have a known type at parse time.
 * They form the leaves of the expression tree.
 */
export type Con = Num | Bool | Str;

/**
 * Numeric literal.
 * Example: 42, 3.14
 */
export interface Num extends Node {
  readonly kind: "Num";
  readonly value: number;
}

/**
 * Boolean literal.
 * Example: true, false
 */
export interface Bool extends Node {
  readonly kind: "Bool";
  readonly value: boolean;
}

/**
 * String literal.
 * Example: "hello world"
 */
export interface Str extends Node {
  readonly kind: "Str";
  readonly value: string;
}

// -----------------------------------------------------------------------------
// Bindings
// -----------------------------------------------------------------------------

/**
 * Let binding - introduces a new variable with polymorphic generalization.
 *
 * Syntax: let name = value in body
 * Example: let id = fn x => x in id 42
 *
 * The bound variable `name` is generalized (made polymorphic) before being
 * added to the environment for type checking `body`. This means:
 *   let id = fn x => x in (id 42, id "hello")
 * is valid because `id` has type `∀a. a -> a` and can be instantiated
 * at different types in the body.
 */
export interface Let extends Node {
  readonly kind: "Let";
  readonly name: string;
  readonly nameSpan?: Span;
  readonly value: Expr;
  readonly body: Expr;
}

/**
 * Recursive let binding - allows the bound value to reference itself.
 *
 * Syntax: letrec name = value in body
 * Example: letrec fact = fn n => if n == 0 then 1 else n * fact (n - 1) in fact 5
 *
 * Unlike regular let, the variable `name` is in scope within `value`,
 * enabling recursive function definitions. The implementation adds a
 * placeholder type variable for the binding before inferring the value.
 */
export interface LetRec extends Node {
  readonly kind: "LetRec";
  readonly name: string;
  readonly nameSpan?: Span;
  readonly value: Expr;
  readonly body: Expr;
}

// -----------------------------------------------------------------------------
// Variables and Functions
// -----------------------------------------------------------------------------

/**
 * Variable reference - looks up a name in the environment.
 *
 * Example: x, map, Cons
 *
 * Variables can refer to:
 * - Let-bound values
 * - Function parameters
 * - Constructor names (like Cons, Just, Nothing)
 * - Built-in functions (like map, filter)
 */
export interface Var extends Node {
  readonly kind: "Var";
  readonly name: string;
}

/**
 * Lambda abstraction - an anonymous function.
 *
 * Syntax: fn param => body
 * Example: fn x => x + 1
 *
 * In lambda calculus terms, this is λparam.body. Our language only supports
 * single-parameter functions; multi-parameter functions are represented as
 * curried functions (functions that return functions).
 *
 * Example of currying: fn x => fn y => x + y
 */
export interface Abs extends Node {
  readonly kind: "Abs";
  readonly param: string;
  readonly paramSpan?: Span;
  readonly body: Expr;
}

/**
 * Function application - applies a function to an argument.
 *
 * Syntax: func arg
 * Example: add 1, map f xs, Cons x xs
 *
 * Function application is left-associative:
 *   f x y z = ((f x) y) z
 *
 * This works with curried functions: if `add : number -> number -> number`,
 * then `add 1` has type `number -> number`.
 */
export interface App extends Node {
  readonly kind: "App";
  readonly func: Expr;
  readonly param: Expr;
}

// -----------------------------------------------------------------------------
// Compound Data Types
// -----------------------------------------------------------------------------

/**
 * Tuple expression - a fixed-length collection of heterogeneous values.
 *
 * Syntax: (a, b, c)
 * Example: (1, "hello", true) has type (number, string, boolean)
 *
 * Unlike lists, tuples:
 * - Have a fixed length known at compile time
 * - Can contain elements of different types
 * - Have positional access rather than head/tail
 */
export interface Tuple extends Node {
  readonly kind: "Tuple";
  readonly elements: readonly Expr[];
}

/**
 * Record expression - a collection of named fields.
 *
 * Syntax: { field1: value1, field2: value2 }
 * Example: { x: 1, y: "hello" } has type { x: number, y: string }
 *
 * Records provide structural typing: two records with the same field names
 * and types are considered compatible, regardless of field order.
 */
export interface Record extends Node {
  readonly kind: "Record";
  readonly fields: readonly RecordField[];
}

/**
 * A single field in a record expression.
 */
export interface RecordField extends Node {
  readonly name: string;
  readonly value: Expr;
}

/**
 * Field access expression - extracts a field from a record.
 *
 * Syntax: record.field
 * Example: person.name
 *
 * With row polymorphism, a function like `fn r => r.x` can work on any
 * record that has an `x` field, regardless of other fields. This gives
 * the type `{ x: t | ρ } -> t` where ρ represents the unknown remaining fields.
 */
export interface FieldAccess extends Node {
  readonly kind: "FieldAccess";
  readonly record: Expr;
  readonly field: string;
}

/**
 * Tuple index expression - extracts an element from a tuple by position.
 *
 * Syntax: tuple.0, tuple.1, tuple.2
 * Example: pair.0, triple.2
 *
 * The index must be a non-negative integer within the tuple's bounds.
 * The type checker validates that the index is valid for the tuple's arity.
 */
export interface TupleIndex extends Node {
  readonly kind: "TupleIndex";
  readonly tuple: Expr;
  readonly index: number;
}

// -----------------------------------------------------------------------------
// Control Flow
// -----------------------------------------------------------------------------

/**
 * Conditional expression - if-then-else.
 *
 * Syntax: if cond then thenBranch else elseBranch
 * Example: if x > 0 then "positive" else "non-positive"
 *
 * Both branches must have the same type, and the condition must be boolean.
 * This is an expression (returns a value), not a statement.
 */
export interface If extends Node {
  readonly kind: "If";
  readonly cond: Expr;
  readonly then: Expr;
  readonly else: Expr;
}

/**
 * Binary operator expression.
 *
 * Syntax: left op right
 * Example: x + y, a == b, n < 10
 *
 * Operators are typed using type classes:
 * - Arithmetic (+): requires Add class (number, string)
 * - Arithmetic (-, *, /): requires number
 * - Comparison (<, >, <=, >=): requires Ord class
 * - Equality (==, !=): requires Eq class
 */
export interface BinOp extends Node {
  readonly kind: "BinOp";
  readonly op: Op;
  readonly left: Expr;
  readonly right: Expr;
}

/**
 * Supported binary operators.
 *
 * Arithmetic: +, -, *, /
 * Comparison: <, <=, >, >=
 * Equality: ==, !=
 * String: ++ (concatenation)
 */
export type Op = "+" | "-" | "/" | "*" | "<" | "<=" | ">" | ">=" | "==" | "!=" | "++";

// =============================================================================
// PATTERNS
// =============================================================================

/**
 * Patterns are used in match expressions to destructure values and bind variables.
 *
 * Pattern matching is exhaustive: the type checker verifies that all possible
 * cases are covered for algebraic data types.
 */
export type Pattern = PVar | PWildcard | PCon | PLit | PRecord | PTuple;

/**
 * Variable pattern - binds the matched value to a name.
 *
 * Example: In `match x with y => y + 1`, `y` is a variable pattern
 * that binds the entire value of `x`.
 *
 * Variable patterns always match and make the whole match exhaustive
 * when used as the only or last pattern.
 */
export interface PVar extends Node {
  readonly kind: "PVar";
  readonly name: string;
}

/**
 * Wildcard pattern - matches anything but doesn't bind a name.
 *
 * Syntax: _
 * Example: match pair with (x, _) => x
 *
 * Use wildcards when you need to match a position but don't care about the value.
 * Like variable patterns, wildcards always match.
 */
export interface PWildcard extends Node {
  readonly kind: "PWildcard";
}

/**
 * Constructor pattern - matches and destructures algebraic data types.
 *
 * Syntax: ConstructorName arg1 arg2 ...
 * Examples:
 *   - `Nil` matches the empty list
 *   - `Cons x xs` matches a non-empty list, binding head to x and tail to xs
 *   - `Just value` matches Some variant of Maybe
 *
 * Constructor patterns can be nested: `Cons x (Cons y Nil)` matches
 * a list with exactly two elements.
 */
export interface PCon extends Node {
  readonly kind: "PCon";
  readonly name: string;
  readonly args: readonly Pattern[];
  readonly nameSpan?: Span; // Precise span of constructor name for LSP
}

/**
 * Literal pattern - matches a specific constant value.
 *
 * Example: match n with 0 => "zero" | _ => "non-zero"
 *
 * Literal patterns match by equality. They don't make a match exhaustive
 * by themselves (there are always other values of the same type).
 */
export interface PLit extends Node {
  readonly kind: "PLit";
  readonly value: number | string | boolean;
}

/**
 * A single field in a record pattern.
 */
export interface PRecordField extends Node {
  readonly name: string;
  readonly pattern: Pattern;
}

/**
 * Record pattern - destructures a record by matching specific fields.
 *
 * Syntax: { field1: pattern1, field2: pattern2 }
 * Example: { x: a, y: b } matches a record with x and y fields
 *
 * Record patterns are non-exhaustive (they only match specific fields)
 * and support row polymorphism (the record can have additional fields).
 */
export interface PRecord extends Node {
  readonly kind: "PRecord";
  readonly fields: readonly PRecordField[];
}

/**
 * Tuple pattern - destructures a tuple by position.
 *
 * Syntax: (pattern1, pattern2, ...)
 * Example: (x, y, z) matches a 3-tuple and binds each element
 *
 * The pattern must have the same arity as the tuple being matched.
 */
export interface PTuple extends Node {
  readonly kind: "PTuple";
  readonly elements: readonly Pattern[];
}

/**
 * A single case in a match expression - pairs a pattern with an expression.
 */
export interface Case extends Node {
  readonly pattern: Pattern;
  readonly body: Expr;
}

/**
 * Match expression - pattern matching on a value.
 *
 * Syntax: match expr with pattern1 => body1 | pattern2 => body2 | ...
 * Example:
 *   match maybeValue with
 *     Just x => x
 *     Nothing => 0
 *
 * The type checker:
 * 1. Infers the type of the scrutinee (expr)
 * 2. Checks each pattern against that type, collecting variable bindings
 * 3. Type-checks each body with the pattern's bindings in scope
 * 4. Unifies all body types to determine the result type
 * 5. Verifies exhaustiveness for algebraic data types
 */
export interface Match extends Node {
  readonly kind: "Match";
  readonly expr: Expr;
  readonly cases: readonly Case[];
}

// =============================================================================
// TYPE EXPRESSIONS (for data declarations)
// =============================================================================

/**
 * Type expressions are used in data declarations to specify constructor field types.
 *
 * These are distinct from the internal Type representation used during inference.
 * Type expressions come from the syntax (user-written), while Types are the
 * internal representation used by the inference algorithm.
 */
export type TypeExpr = TyVar | TyCon | TyApp | TyFun;

/**
 * Type variable - a placeholder for any type.
 *
 * Example: `a` in `data Maybe a = Just a | Nothing`
 *
 * Type variables enable parametric polymorphism: the same data structure
 * can work with any type.
 */
export interface TyVar extends Node {
  readonly kind: "TyVar";
  readonly name: string;
}

/**
 * Type constructor - a named type.
 *
 * Examples: number, string, boolean, List, Maybe
 *
 * Type constructors can be:
 * - Primitive types (number, string, boolean)
 * - User-defined types (List, Maybe, Either)
 */
export interface TyCon extends Node {
  readonly kind: "TyCon";
  readonly name: string;
}

/**
 * Type application - applying a parameterized type to an argument.
 *
 * Example: `List a` is TyApp(TyCon("List"), TyVar("a"))
 *
 * Type application is used for generic types like List, Maybe, etc.
 * Multiple parameters use nested application: `Either a b` is
 * TyApp(TyApp(TyCon("Either"), TyVar("a")), TyVar("b")).
 */
export interface TyApp extends Node {
  readonly kind: "TyApp";
  readonly con: TypeExpr;
  readonly arg: TypeExpr;
}

/**
 * Function type - the type of functions.
 *
 * Example: `a -> b` represents functions from a to b
 *
 * Function types are right-associative: `a -> b -> c` means `a -> (b -> c)`,
 * which represents a curried function taking a, then b, returning c.
 */
export interface TyFun extends Node {
  readonly kind: "TyFun";
  readonly param: TypeExpr;
  readonly ret: TypeExpr;
}

// =============================================================================
// DATA DECLARATIONS (Algebraic Data Types)
// =============================================================================

/**
 * Constructor declaration - defines one variant of a sum type.
 *
 * Example: In `data Maybe a = Nothing | Just a`:
 * - `Nothing` is ConDecl("Nothing", [])
 * - `Just` is ConDecl("Just", [TyVar("a")])
 */
export interface ConDecl extends Node {
  readonly name: string;
  readonly fields: readonly TypeExpr[];
}

/**
 * Data declaration - defines an algebraic data type (ADT).
 *
 * Syntax: data TypeName typeParams = Constructor1 fields1 | Constructor2 fields2 | ...
 *
 * Examples:
 * - data Bool = True | False
 * - data Maybe a = Nothing | Just a
 * - data List a = Nil | Cons a (List a)
 * - data Either a b = Left a | Right b
 *
 * Data declarations create:
 * 1. A new type constructor (the type name)
 * 2. Value constructors for each variant (functions that create values)
 *
 * The type checker processes data declarations to:
 * - Add constructor functions to the type environment
 * - Register constructors for exhaustiveness checking
 */
export interface DataDecl extends Node {
  readonly kind: "DataDecl";
  readonly name: string;
  readonly typeParams: readonly string[];
  readonly constructors: readonly ConDecl[];
}

// =============================================================================
// HELPER FUNCTIONS (Smart Constructors)
// =============================================================================

/**
 * These helper functions provide a concise way to construct AST nodes.
 * They act as "smart constructors" that fill in the `kind` field automatically.
 *
 * Usage example:
 *   Instead of: { kind: "Num", value: 42 }
 *   Write:      num(42)
 */

// --- Constants ---

export const num = (value: number, span?: Span): Num => ({ kind: "Num", value, span });
export const bool = (value: boolean, span?: Span): Bool => ({ kind: "Bool", value, span });
export const str = (value: string, span?: Span): Str => ({ kind: "Str", value, span });

// --- Control Flow ---

export const if_ = (cond: Expr, then: Expr, else_: Expr, span?: Span): If => ({
  kind: "If",
  cond,
  then,
  else: else_,
  span,
});

// --- Bindings ---

export const let_ = (name: string, value: Expr, body: Expr, span?: Span, nameSpan?: Span): Let => ({
  kind: "Let",
  name,
  nameSpan,
  value,
  body,
  span,
});

export const letRec = (
  name: string,
  value: Expr,
  body: Expr,
  span?: Span,
  nameSpan?: Span,
): LetRec => ({
  kind: "LetRec",
  name,
  nameSpan,
  value,
  body,
  span,
});

// --- Variables and Functions ---

export const var_ = (name: string, span?: Span): Var => ({ kind: "Var", name, span });

export const abs = (param: string, body: Expr, span?: Span, paramSpan?: Span): Abs => ({
  kind: "Abs",
  param,
  paramSpan,
  body,
  span,
});

export const app = (func: Expr, param: Expr, span?: Span): App => ({
  kind: "App",
  func,
  param,
  span,
});

// --- Compound Data ---

export const tuple = (elements: readonly Expr[], span?: Span): Tuple => ({
  kind: "Tuple",
  elements,
  span,
});

export const record = (fields: readonly RecordField[], span?: Span): Record => ({
  kind: "Record",
  fields,
  span,
});

export const field = (name: string, value: Expr, span?: Span): RecordField => ({
  name,
  value,
  span,
});

export const fieldAccess = (record: Expr, field: string, span?: Span): FieldAccess => ({
  kind: "FieldAccess",
  record,
  field,
  span,
});

export const tupleIndex = (tuple: Expr, index: number, span?: Span): TupleIndex => ({
  kind: "TupleIndex",
  tuple,
  index,
  span,
});

// --- Operators ---

export const binOp = (op: Op, left: Expr, right: Expr, span?: Span): BinOp => ({
  kind: "BinOp",
  op,
  left,
  right,
  span,
});

// --- Patterns ---

export const pwildcard = (span?: Span): PWildcard => ({ kind: "PWildcard", span });

export const pvar = (name: string, span?: Span): PVar => ({ kind: "PVar", name, span });

export const pcon = (
  name: string,
  args: readonly Pattern[],
  span?: Span,
  nameSpan?: Span,
): PCon => ({
  kind: "PCon",
  name,
  args,
  span,
  nameSpan,
});

export const plit = (value: string | number | boolean, span?: Span): PLit => ({
  kind: "PLit",
  value,
  span,
});

export const precord = (fields: readonly PRecordField[], span?: Span): PRecord => ({
  kind: "PRecord",
  fields,
  span,
});

export const pfield = (name: string, pattern: Pattern, span?: Span): PRecordField => ({
  name,
  pattern,
  span,
});

export const ptuple = (elements: readonly Pattern[], span?: Span): PTuple => ({
  kind: "PTuple",
  elements,
  span,
});

export const case_ = (pattern: Pattern, body: Expr, span?: Span): Case => ({
  pattern,
  body,
  span,
});

export const match = (expr: Expr, cases: readonly Case[], span?: Span): Match => ({
  kind: "Match",
  expr,
  cases,
  span,
});

// --- Type Expressions ---

export const tyvar = (name: string, span?: Span): TyVar => ({ kind: "TyVar", name, span });

export const tycon = (name: string, span?: Span): TyCon => ({ kind: "TyCon", name, span });

export const tyapp = (con: TypeExpr, arg: TypeExpr, span?: Span): TyApp => ({
  kind: "TyApp",
  con,
  arg,
  span,
});

export const tyfun = (param: TypeExpr, ret: TypeExpr, span?: Span): TyFun => ({
  kind: "TyFun",
  param,
  ret,
  span,
});

// --- Data Declarations ---

export const conDecl = (name: string, fields: readonly TypeExpr[], span?: Span): ConDecl => ({
  name,
  fields,
  span,
});

export const dataDecl = (
  name: string,
  typeParams: string[],
  constructors: ConDecl[],
  span?: Span,
): DataDecl => ({ kind: "DataDecl", name, typeParams, constructors, span });
