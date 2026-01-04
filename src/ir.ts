/**
 * Intermediate Representation (IR) in A-Normal Form (ANF).
 *
 * ANF is a restricted form where all intermediate values are named.
 * This simplifies code generation because:
 * 1. Arguments to complex operations are always atomic (literals or variables)
 * 2. Evaluation order is explicit in the let bindings
 * 3. Control flow is clear from the structure
 *
 * The IR is typed - every node carries its inferred type from the type checker.
 * This enables type-directed code generation and optimizations.
 *
 * Pipeline: AST → (lower) → IR → (closureConvert) → IRProgram → (backend) → JS/WASM
 */

import type { Op } from "./ast";
import type { Type } from "./checker";

// =============================================================================
// ATOMIC VALUES
// =============================================================================

/**
 * Atomic values require no computation - just a lookup or constant.
 * In ANF, arguments to operations must be atomic.
 */
export type IRAtom = IRLit | IRVar;

/**
 * Literal value with its type.
 */
export type IRLit = {
  readonly kind: "IRLit";
  readonly value: number | string | boolean;
  readonly type: Type;
};

/**
 * Variable reference with its type.
 */
export type IRVar = {
  readonly kind: "IRVar";
  readonly name: string;
  readonly type: Type;
};

// =============================================================================
// IR EXPRESSIONS (ANF)
// =============================================================================

/**
 * IR expressions in A-Normal Form.
 *
 * Key invariant: arguments to complex operations are always atoms.
 * Complex expressions are broken into sequences of let bindings.
 */
export type IRExpr = IRAtomExpr | IRLet | IRLetRec;

/**
 * Atom in terminal position (final value of an expression).
 */
export type IRAtomExpr = {
  readonly kind: "IRAtomExpr";
  readonly atom: IRAtom;
  readonly type: Type;
};

/**
 * Let binding in ANF.
 * The binding produces a value; body continues computation.
 */
export type IRLet = {
  readonly kind: "IRLet";
  readonly name: string;
  readonly binding: IRBinding;
  readonly body: IRExpr;
  readonly type: Type;
};

/**
 * Recursive let binding for recursive functions.
 */
export type IRLetRec = {
  readonly kind: "IRLetRec";
  readonly name: string;
  readonly binding: IRBinding;
  readonly body: IRExpr;
  readonly type: Type;
};

// =============================================================================
// BINDINGS (RHS of let)
// =============================================================================

/**
 * Bindings are the "complex" operations that produce values.
 * They appear on the RHS of let bindings.
 */
export type IRBinding =
  | IRAtomBinding
  | IRAppBinding
  | IRBinOpBinding
  | IRIfBinding
  | IRTupleBinding
  | IRRecordBinding
  | IRFieldAccessBinding
  | IRTupleIndexBinding
  | IRMatchBinding
  | IRLambdaBinding
  | IRClosureBinding;

/**
 * Trivial binding - just an atom (for uniformity).
 */
export type IRAtomBinding = {
  readonly kind: "IRAtomBinding";
  readonly atom: IRAtom;
  readonly type: Type;
};

/**
 * Function application.
 * Both func and arg must be atomic (ANF requirement).
 */
export type IRAppBinding = {
  readonly kind: "IRAppBinding";
  readonly func: IRAtom;
  readonly arg: IRAtom;
  readonly type: Type;
};

/**
 * Binary operation.
 * Both operands must be atomic.
 * Includes operandType for dispatch (e.g., number vs string for +).
 */
export type IRBinOpBinding = {
  readonly kind: "IRBinOpBinding";
  readonly op: Op;
  readonly left: IRAtom;
  readonly right: IRAtom;
  readonly operandType: Type; // For dispatch (left operand type)
  readonly type: Type; // Result type
};

/**
 * Conditional expression.
 * Condition must be atomic; branches are full expressions.
 */
export type IRIfBinding = {
  readonly kind: "IRIfBinding";
  readonly cond: IRAtom;
  readonly thenBranch: IRExpr;
  readonly elseBranch: IRExpr;
  readonly type: Type;
};

/**
 * Tuple construction.
 * All elements must be atomic.
 */
export type IRTupleBinding = {
  readonly kind: "IRTupleBinding";
  readonly elements: readonly IRAtom[];
  readonly type: Type;
};

/**
 * Record construction.
 * All field values must be atomic.
 */
export type IRRecordBinding = {
  readonly kind: "IRRecordBinding";
  readonly fields: readonly IRRecordField[];
  readonly type: Type;
};

export type IRRecordField = {
  readonly name: string;
  readonly value: IRAtom;
};

/**
 * Field access from a record.
 * Record must be atomic.
 */
export type IRFieldAccessBinding = {
  readonly kind: "IRFieldAccessBinding";
  readonly record: IRAtom;
  readonly field: string;
  readonly type: Type;
};

/**
 * Tuple index access.
 * Tuple must be atomic.
 */
export type IRTupleIndexBinding = {
  readonly kind: "IRTupleIndexBinding";
  readonly tuple: IRAtom;
  readonly index: number;
  readonly type: Type;
};

/**
 * Pattern match expression.
 * Scrutinee must be atomic.
 */
export type IRMatchBinding = {
  readonly kind: "IRMatchBinding";
  readonly scrutinee: IRAtom;
  readonly cases: readonly IRCase[];
  readonly type: Type;
};

/**
 * Lambda expression (before closure conversion).
 * Contains the function body as an expression.
 */
export type IRLambdaBinding = {
  readonly kind: "IRLambdaBinding";
  readonly param: string;
  readonly paramType: Type;
  readonly body: IRExpr;
  readonly type: Type; // Function type
};

/**
 * Explicit closure (after closure conversion).
 * Pairs a lifted function name with its captured environment.
 */
export type IRClosureBinding = {
  readonly kind: "IRClosureBinding";
  readonly funcId: string; // Reference to lifted function
  readonly captures: readonly IRAtom[]; // Captured free variables
  readonly type: Type;
};

// =============================================================================
// PATTERN MATCHING
// =============================================================================

/**
 * A case in pattern matching.
 */
export type IRCase = {
  readonly pattern: IRPattern;
  readonly body: IRExpr;
};

/**
 * IR patterns - simplified from AST patterns, with types.
 */
export type IRPattern = IRPVar | IRPWildcard | IRPCon | IRPLit | IRPTuple | IRPRecord;

export type IRPVar = {
  readonly kind: "IRPVar";
  readonly name: string;
  readonly type: Type;
};

export type IRPWildcard = {
  readonly kind: "IRPWildcard";
  readonly type: Type;
};

export type IRPCon = {
  readonly kind: "IRPCon";
  readonly name: string;
  readonly args: readonly IRPattern[];
  readonly type: Type;
};

export type IRPLit = {
  readonly kind: "IRPLit";
  readonly value: number | string | boolean;
  readonly type: Type;
};

export type IRPTuple = {
  readonly kind: "IRPTuple";
  readonly elements: readonly IRPattern[];
  readonly type: Type;
};

export type IRPRecord = {
  readonly kind: "IRPRecord";
  readonly fields: readonly IRPRecordField[];
  readonly type: Type;
};

export type IRPRecordField = {
  readonly name: string;
  readonly pattern: IRPattern;
};

// =============================================================================
// IR PROGRAM (after closure conversion)
// =============================================================================

/**
 * A lifted function (after closure conversion).
 * Top-level function with explicit environment parameter.
 */
export type IRFunction = {
  readonly id: string; // Unique function identifier
  readonly envParam: string; // Parameter name for captured environment
  readonly param: string; // Regular parameter
  readonly paramType: Type;
  readonly body: IRExpr;
  readonly captureTypes: readonly Type[]; // Types of captured variables
  readonly returnType: Type;
};

/**
 * Complete IR program ready for code generation.
 */
export type IRProgram = {
  readonly functions: readonly IRFunction[]; // Lifted functions
  readonly main: IRExpr; // Main expression
};

// =============================================================================
// SMART CONSTRUCTORS
// =============================================================================

// --- Atoms ---

export const irLit = (value: number | string | boolean, type: Type): IRLit => ({
  kind: "IRLit",
  value,
  type,
});

export const irVar = (name: string, type: Type): IRVar => ({
  kind: "IRVar",
  name,
  type,
});

// --- Expressions ---

export const irAtomExpr = (atom: IRAtom): IRAtomExpr => ({
  kind: "IRAtomExpr",
  atom,
  type: atom.type,
});

export const irLet = (name: string, binding: IRBinding, body: IRExpr): IRLet => ({
  kind: "IRLet",
  name,
  binding,
  body,
  type: body.type,
});

export const irLetRec = (name: string, binding: IRBinding, body: IRExpr): IRLetRec => ({
  kind: "IRLetRec",
  name,
  binding,
  body,
  type: body.type,
});

// --- Bindings ---

export const irAtomBinding = (atom: IRAtom): IRAtomBinding => ({
  kind: "IRAtomBinding",
  atom,
  type: atom.type,
});

export const irAppBinding = (func: IRAtom, arg: IRAtom, type: Type): IRAppBinding => ({
  kind: "IRAppBinding",
  func,
  arg,
  type,
});

export const irBinOpBinding = (
  op: Op,
  left: IRAtom,
  right: IRAtom,
  operandType: Type,
  type: Type,
): IRBinOpBinding => ({
  kind: "IRBinOpBinding",
  op,
  left,
  right,
  operandType,
  type,
});

export const irIfBinding = (
  cond: IRAtom,
  thenBranch: IRExpr,
  elseBranch: IRExpr,
  type: Type,
): IRIfBinding => ({
  kind: "IRIfBinding",
  cond,
  thenBranch,
  elseBranch,
  type,
});

export const irTupleBinding = (elements: readonly IRAtom[], type: Type): IRTupleBinding => ({
  kind: "IRTupleBinding",
  elements,
  type,
});

export const irRecordBinding = (fields: readonly IRRecordField[], type: Type): IRRecordBinding => ({
  kind: "IRRecordBinding",
  fields,
  type,
});

export const irRecordField = (name: string, value: IRAtom): IRRecordField => ({
  name,
  value,
});

export const irFieldAccessBinding = (
  record: IRAtom,
  field: string,
  type: Type,
): IRFieldAccessBinding => ({
  kind: "IRFieldAccessBinding",
  record,
  field,
  type,
});

export const irTupleIndexBinding = (
  tuple: IRAtom,
  index: number,
  type: Type,
): IRTupleIndexBinding => ({
  kind: "IRTupleIndexBinding",
  tuple,
  index,
  type,
});

export const irMatchBinding = (
  scrutinee: IRAtom,
  cases: readonly IRCase[],
  type: Type,
): IRMatchBinding => ({
  kind: "IRMatchBinding",
  scrutinee,
  cases,
  type,
});

export const irLambdaBinding = (
  param: string,
  paramType: Type,
  body: IRExpr,
  type: Type,
): IRLambdaBinding => ({
  kind: "IRLambdaBinding",
  param,
  paramType,
  body,
  type,
});

export const irClosureBinding = (
  funcId: string,
  captures: readonly IRAtom[],
  type: Type,
): IRClosureBinding => ({
  kind: "IRClosureBinding",
  funcId,
  captures,
  type,
});

// --- Patterns ---

export const irPVar = (name: string, type: Type): IRPVar => ({
  kind: "IRPVar",
  name,
  type,
});

export const irPWildcard = (type: Type): IRPWildcard => ({
  kind: "IRPWildcard",
  type,
});

export const irPCon = (name: string, args: readonly IRPattern[], type: Type): IRPCon => ({
  kind: "IRPCon",
  name,
  args,
  type,
});

export const irPLit = (value: number | string | boolean, type: Type): IRPLit => ({
  kind: "IRPLit",
  value,
  type,
});

export const irPTuple = (elements: readonly IRPattern[], type: Type): IRPTuple => ({
  kind: "IRPTuple",
  elements,
  type,
});

export const irPRecord = (fields: readonly IRPRecordField[], type: Type): IRPRecord => ({
  kind: "IRPRecord",
  fields,
  type,
});

export const irPRecordField = (name: string, pattern: IRPattern): IRPRecordField => ({
  name,
  pattern,
});

// --- Cases ---

export const irCase = (pattern: IRPattern, body: IRExpr): IRCase => ({
  pattern,
  body,
});

// --- Functions and Programs ---

export const irFunction = (
  id: string,
  envParam: string,
  param: string,
  paramType: Type,
  body: IRExpr,
  captureTypes: readonly Type[],
  returnType: Type,
): IRFunction => ({
  id,
  envParam,
  param,
  paramType,
  body,
  captureTypes,
  returnType,
});

export const irProgram = (functions: readonly IRFunction[], main: IRExpr): IRProgram => ({
  functions,
  main,
});
