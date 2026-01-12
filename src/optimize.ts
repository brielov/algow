/**
 * IR Optimization Passes (Section 11)
 *
 * Transforms the IR to improve performance:
 * - Constant Folding: evaluate literal expressions at compile time
 * - Dead Code Elimination: remove unused bindings
 * - Tail Call Optimization: mark tail-recursive functions
 */

import * as IR from "./ir";
import type { Name } from "./core";
import type { Literal } from "./surface";
import type { Type } from "./types";

// =============================================================================
// Type Helpers
// =============================================================================

/** Compare two Names for equality */
const nameEq = (a: Name, b: Name): boolean => a.id === b.id;

/** Get a string key for a Name (for use in Maps/Sets) */
const nameKey = (name: Name): string => `${name.id}:${name.original}`;

// =============================================================================
// Constant Folding
// =============================================================================

type ConstValue = number | string | boolean;
type ConstEnv = Map<string, ConstValue>;

// Extended value tracking for constructor applications and lambdas
type ExtendedValue =
  | { kind: "const"; value: ConstValue }
  | { kind: "con"; name: string; args: IR.Atom[] }
  | { kind: "tuple"; elements: IR.Atom[] }
  | { kind: "atom"; atom: IR.Atom }
  | { kind: "lambda"; param: Name; body: IR.IRExpr; type: Type };

type ExtendedEnv = Map<string, ExtendedValue>;

/** Extract constant value from a Literal */
const literalToConst = (lit: Literal): ConstValue => {
  switch (lit.kind) {
    case "int":
    case "float":
      return lit.value;
    case "string":
    case "char":
      return lit.value;
    case "bool":
      return lit.value;
  }
};

/** Extract extended value to track from a binding */
const getExtendedValueFromBinding = (binding: IR.IRBinding): ExtendedValue | null => {
  switch (binding.kind) {
    case "IRBAtom":
      if (binding.atom.kind === "ALit") {
        return { kind: "const", value: literalToConst(binding.atom.value) };
      }
      return { kind: "atom", atom: binding.atom };

    case "IRBApp":
      // Track constructor applications: Just(5) -> { kind: "con", name: "Just", args: [5] }
      if (binding.func.kind === "ACon") {
        return { kind: "con", name: binding.func.name, args: [binding.arg] };
      }
      return null;

    case "IRBTuple":
      return { kind: "tuple", elements: binding.elements as IR.Atom[] };

    case "IRBLambda":
      // Track lambdas for beta reduction
      return { kind: "lambda", param: binding.param, body: binding.body, type: binding.type };

    default:
      return null;
  }
};

/** Convert ExtendedEnv to ConstEnv for binary folding */
const toConstEnv = (extEnv: ExtendedEnv): ConstEnv => {
  const constEnv: ConstEnv = new Map();
  for (const [k, v] of extEnv) {
    if (v.kind === "const") {
      constEnv.set(k, v.value);
    }
  }
  return constEnv;
};

/**
 * Bind the result of an expression to a name and continue.
 * Used for beta reduction to capture the result of a lambda body.
 */
const bindResultTo = (expr: IR.IRExpr, name: Name, cont: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      // The result is the atom, bind it to name and continue
      return IR.irlet(name, IR.irbatom(expr.atom), cont);
    case "IRLet":
      // Recurse into body
      return IR.irlet(expr.name, expr.binding, bindResultTo(expr.body, name, cont));
    case "IRLetRec":
      return IR.irletrec(expr.bindings, bindResultTo(expr.body, name, cont));
    case "IRMatch":
      // For match, bind the whole match result to name
      // This creates: let name = match ... in cont
      return IR.irlet(name, IR.irbmatch(expr.scrutinee, expr.cases, expr.type), cont);
  }
};

const foldExpr = (expr: IR.IRExpr, env: ExtendedEnv): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      // Try beta reduction: (λx. body) arg → let x = arg in body
      if (expr.binding.kind === "IRBApp" && expr.binding.func.kind === "AVar") {
        const lambdaVal = env.get(nameKey(expr.binding.func.name));
        if (lambdaVal && lambdaVal.kind === "lambda") {
          // Beta reduction!
          const paramBinding = IR.irlet(
            lambdaVal.param,
            IR.irbatom(expr.binding.arg),
            bindResultTo(lambdaVal.body, expr.name, expr.body),
          );
          // Recursively fold the result
          return foldExpr(paramBinding, env);
        }
      }

      const constEnv = toConstEnv(env);
      const foldedBinding = foldBinding(expr.binding, constEnv);

      // Track extended values
      const extValue = getExtendedValueFromBinding(foldedBinding);
      if (extValue) {
        const newEnv = new Map(env);
        newEnv.set(nameKey(expr.name), extValue);
        return IR.irlet(expr.name, foldedBinding, foldExpr(expr.body, newEnv));
      }

      return IR.irlet(expr.name, foldedBinding, foldExpr(expr.body, env));
    }

    case "IRLetRec": {
      const constEnv = toConstEnv(env);
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: foldBinding(b.binding, constEnv),
      }));
      return IR.irletrec(newBindings, foldExpr(expr.body, env));
    }

    case "IRMatch": {
      // First, fold the cases
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? foldExpr(c.guard, env) : null,
        body: foldExpr(c.body, env),
      }));

      // Try case-of-known-constructor optimization
      const simplified = tryCaseOfKnownExt(expr.scrutinee, cases, env);
      if (simplified) {
        return simplified;
      }

      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

type KnownValue =
  | { kind: "lit"; value: Literal }
  | { kind: "con"; name: string; args: IR.Atom[] }
  | { kind: "tuple"; elements: IR.Atom[] };

type MatchResult = {
  bindings: { name: Name; atom: IR.Atom }[];
};

/** Get the known value from an atom using extended environment */
const getKnownValueExt = (atom: IR.Atom, env: ExtendedEnv): KnownValue | null => {
  switch (atom.kind) {
    case "ALit":
      return { kind: "lit", value: atom.value };
    case "ACon":
      // Nullary constructor
      return { kind: "con", name: atom.name, args: [] };
    case "AVar": {
      // Try to resolve through the extended environment
      const val = env.get(nameKey(atom.name));
      if (val === undefined) return null;

      switch (val.kind) {
        case "const":
          // Convert primitive value back to literal
          if (typeof val.value === "boolean") {
            return { kind: "lit", value: { kind: "bool", value: val.value } };
          }
          if (typeof val.value === "number") {
            return {
              kind: "lit",
              value: Number.isInteger(val.value)
                ? { kind: "int", value: val.value }
                : { kind: "float", value: val.value },
            };
          }
          if (typeof val.value === "string") {
            return { kind: "lit", value: { kind: "string", value: val.value } };
          }
          return null;

        case "con":
          // Constructor application: Just(5) etc.
          return { kind: "con", name: val.name, args: val.args };

        case "tuple":
          return { kind: "tuple", elements: val.elements };

        case "atom":
          // Recursively resolve if it's another atom
          return getKnownValueExt(val.atom, env);

        case "lambda":
          // Lambdas don't produce known values for match scrutinee
          return null;
      }
    }
  }
};

/**
 * Case-of-known-constructor optimization (extended version).
 * Uses ExtendedEnv to track constructor applications.
 */
const tryCaseOfKnownExt = (
  scrutinee: IR.Atom,
  cases: readonly IR.IRCase[],
  env: ExtendedEnv,
): IR.IRExpr | null => {
  // Get the known value from scrutinee (if any)
  const knownValue = getKnownValueExt(scrutinee, env);
  if (!knownValue) return null;

  // Find matching case
  for (const c of cases) {
    // Skip cases with guards - too complex for static analysis
    if (c.guard) continue;

    const matchResult = matchPattern(c.pattern, knownValue, scrutinee);
    if (matchResult) {
      // Create let bindings for pattern variables and return body
      return wrapWithBindings(matchResult.bindings, c.body);
    }
  }

  return null;
};

/** Try to match a pattern against a known value */
const matchPattern = (
  pattern: IR.IRPattern,
  value: KnownValue,
  scrutinee: IR.Atom,
): MatchResult | null => {
  switch (pattern.kind) {
    case "IRPWild":
      // Wildcard matches anything
      return { bindings: [] };

    case "IRPVar":
      // Variable pattern matches anything and binds
      return { bindings: [{ name: pattern.name, atom: scrutinee }] };

    case "IRPLit":
      // Literal pattern matches if values are equal
      if (value.kind !== "lit") return null;
      if (literalsEqual(pattern.value, value.value)) {
        return { bindings: [] };
      }
      return null;

    case "IRPCon": {
      // Constructor pattern matches if names match
      if (value.kind !== "con") return null;
      if (pattern.name !== value.name) return null;
      if (pattern.args.length !== value.args.length) return null;
      // Match sub-patterns against constructor arguments
      const bindings: { name: Name; atom: IR.Atom }[] = [];
      for (let i = 0; i < pattern.args.length; i++) {
        const patArg = pattern.args[i];
        const valArg = value.args[i];
        if (!patArg || !valArg) return null;
        const argValue = getKnownValueFromAtom(valArg);
        if (!argValue) return null; // Can't statically analyze argument
        const subMatch = matchPattern(patArg, argValue, valArg);
        if (!subMatch) return null;
        bindings.push(...subMatch.bindings);
      }
      return { bindings };
    }

    case "IRPTuple": {
      // Tuple pattern matches if all elements match
      if (value.kind !== "tuple") return null;
      if (pattern.elements.length !== value.elements.length) return null;
      const tupleBindings: { name: Name; atom: IR.Atom }[] = [];
      for (let i = 0; i < pattern.elements.length; i++) {
        const patElem = pattern.elements[i];
        const valElem = value.elements[i];
        if (!patElem || !valElem) return null;
        const elemValue = getKnownValueFromAtom(valElem);
        if (!elemValue) return null;
        const subMatch = matchPattern(patElem, elemValue, valElem);
        if (!subMatch) return null;
        tupleBindings.push(...subMatch.bindings);
      }
      return { bindings: tupleBindings };
    }

    case "IRPRecord":
      // Record patterns are more complex, skip for now
      return null;

    case "IRPAs":
      // As-pattern: bind the whole value and match sub-pattern
      const subMatch = matchPattern(pattern.pattern, value, scrutinee);
      if (!subMatch) return null;
      return { bindings: [{ name: pattern.name, atom: scrutinee }, ...subMatch.bindings] };
  }
};

/** Get known value directly from an atom (without env lookup) */
const getKnownValueFromAtom = (atom: IR.Atom): KnownValue | null => {
  switch (atom.kind) {
    case "ALit":
      return { kind: "lit", value: atom.value };
    case "ACon":
      return { kind: "con", name: atom.name, args: [] };
    case "AVar":
      return null;
  }
};

/** Check if two literals are equal */
const literalsEqual = (a: Literal, b: Literal): boolean => {
  if (a.kind !== b.kind) return false;
  return a.value === b.value;
};

/** Wrap an expression with let bindings */
const wrapWithBindings = (
  bindings: { name: Name; atom: IR.Atom }[],
  body: IR.IRExpr,
): IR.IRExpr => {
  let result = body;
  for (let i = bindings.length - 1; i >= 0; i--) {
    const b = bindings[i];
    if (b) {
      result = IR.irlet(b.name, IR.irbatom(b.atom), result);
    }
  }
  return result;
};

/** Try to fold a binary operation with constant operands */
const tryFoldBinOp = (op: string, left: IR.Atom, right: IR.Atom, env: ConstEnv): IR.Atom | null => {
  // Get constant values for operands
  const getConstValue = (atom: IR.Atom): ConstValue | null => {
    if (atom.kind === "ALit") {
      return literalToConst(atom.value);
    }
    if (atom.kind === "AVar") {
      const val = env.get(nameKey(atom.name));
      return val !== undefined ? val : null;
    }
    return null;
  };

  const leftVal = getConstValue(left);
  const rightVal = getConstValue(right);

  if (leftVal === null || rightVal === null) return null;

  // Arithmetic operations (numbers only)
  if (typeof leftVal === "number" && typeof rightVal === "number") {
    let result: number | boolean | null = null;
    switch (op) {
      case "+":
        result = leftVal + rightVal;
        break;
      case "-":
        result = leftVal - rightVal;
        break;
      case "*":
        result = leftVal * rightVal;
        break;
      case "/":
        if (rightVal !== 0) result = Math.trunc(leftVal / rightVal);
        break;
      case "<":
        result = leftVal < rightVal;
        break;
      case "<=":
        result = leftVal <= rightVal;
        break;
      case ">":
        result = leftVal > rightVal;
        break;
      case ">=":
        result = leftVal >= rightVal;
        break;
      case "==":
        result = leftVal === rightVal;
        break;
      case "!=":
        result = leftVal !== rightVal;
        break;
    }
    if (result !== null) {
      if (typeof result === "boolean") {
        return IR.alit({ kind: "bool", value: result }, { kind: "TCon", name: "boolean" });
      }
      // Preserve int vs float type
      const isInt =
        Number.isInteger(leftVal) && Number.isInteger(rightVal) && Number.isInteger(result);
      return IR.alit(
        { kind: isInt ? "int" : "float", value: result },
        { kind: "TCon", name: isInt ? "int" : "float" },
      );
    }
  }

  // String concatenation
  if (typeof leftVal === "string" && typeof rightVal === "string" && op === "+") {
    return IR.alit({ kind: "string", value: leftVal + rightVal }, { kind: "TCon", name: "string" });
  }

  // String comparison
  if (typeof leftVal === "string" && typeof rightVal === "string") {
    let result: boolean | null = null;
    switch (op) {
      case "==":
        result = leftVal === rightVal;
        break;
      case "!=":
        result = leftVal !== rightVal;
        break;
      case "<":
        result = leftVal < rightVal;
        break;
      case "<=":
        result = leftVal <= rightVal;
        break;
      case ">":
        result = leftVal > rightVal;
        break;
      case ">=":
        result = leftVal >= rightVal;
        break;
    }
    if (result !== null) {
      return IR.alit({ kind: "bool", value: result }, { kind: "TCon", name: "boolean" });
    }
  }

  // Boolean operations
  if (typeof leftVal === "boolean" && typeof rightVal === "boolean") {
    let result: boolean | null = null;
    switch (op) {
      case "==":
        result = leftVal === rightVal;
        break;
      case "!=":
        result = leftVal !== rightVal;
        break;
      case "&&":
        result = leftVal && rightVal;
        break;
      case "||":
        result = leftVal || rightVal;
        break;
    }
    if (result !== null) {
      return IR.alit({ kind: "bool", value: result }, { kind: "TCon", name: "boolean" });
    }
  }

  // Short-circuit boolean operations with one known operand
  if (typeof leftVal === "boolean") {
    // true && e → e, false && e → false
    if (op === "&&") {
      if (leftVal === false) {
        return IR.alit({ kind: "bool", value: false }, { kind: "TCon", name: "boolean" });
      }
      // true && e → e (return right operand)
      if (leftVal === true) return right;
    }
    // true || e → true, false || e → e
    if (op === "||") {
      if (leftVal === true) {
        return IR.alit({ kind: "bool", value: true }, { kind: "TCon", name: "boolean" });
      }
      // false || e → e (return right operand)
      if (leftVal === false) return right;
    }
  }

  return null;
};

/**
 * Try eta reduction: λx. f x → f (when x is not free in f)
 * Returns the binding for f if eta reduction applies, null otherwise.
 */
const tryEtaReduce = (param: Name, body: IR.IRExpr): IR.IRBinding | null => {
  // Pattern: let r = f(x) in r
  // where x is the param and f doesn't contain x
  if (body.kind !== "IRLet") return null;

  const binding = body.binding;
  if (binding.kind !== "IRBApp") return null;

  // Check that the argument is the lambda parameter
  if (binding.arg.kind !== "AVar") return null;
  if (!nameEq(binding.arg.name, param)) return null;

  // Check that the body is just returning the result
  if (body.body.kind !== "IRAtom") return null;
  if (body.body.atom.kind !== "AVar") return null;
  if (!nameEq(body.body.atom.name, body.name)) return null;

  // Check that the function doesn't contain the parameter
  if (atomContains(binding.func, param)) return null;

  // Eta reduction succeeds! Return the function as an atom binding
  return IR.irbatom(binding.func);
};

/** Check if an atom contains a reference to the given name */
const atomContains = (atom: IR.Atom, name: Name): boolean => {
  if (atom.kind === "AVar") {
    return nameEq(atom.name, name);
  }
  return false;
};

const foldBinding = (binding: IR.IRBinding, env: ConstEnv): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
      return binding;

    case "IRBApp":
      // Can't fold function applications at compile time
      return binding;

    case "IRBBinOp": {
      const folded = tryFoldBinOp(binding.op, binding.left, binding.right, env);
      if (folded) {
        return IR.irbatom(folded);
      }
      return binding;
    }

    case "IRBTuple":
      return binding;

    case "IRBRecord":
      return binding;

    case "IRBRecordUpdate":
      return binding;

    case "IRBField":
      return binding;

    case "IRBLambda": {
      const foldedBody = foldExpr(binding.body, new Map());

      // Try eta reduction: λx. f x → f (when x is not free in f)
      const etaReduced = tryEtaReduce(binding.param, foldedBody);
      if (etaReduced) {
        return etaReduced;
      }

      return IR.irblambda(binding.param, foldedBody, binding.type);
    }

    case "IRBForeign":
      return binding;

    case "IRBMatch":
      return binding;
  }
};

// =============================================================================
// Dead Code Elimination
// =============================================================================

const collectUses = (expr: IR.IRExpr, uses: Set<string>): void => {
  switch (expr.kind) {
    case "IRAtom":
      if (expr.atom.kind === "AVar") {
        uses.add(nameKey(expr.atom.name));
      }
      break;

    case "IRLet":
      collectUsesBinding(expr.binding, uses);
      collectUses(expr.body, uses);
      break;

    case "IRLetRec":
      for (const b of expr.bindings) {
        collectUsesBinding(b.binding, uses);
      }
      collectUses(expr.body, uses);
      break;

    case "IRMatch":
      if (expr.scrutinee.kind === "AVar") {
        uses.add(nameKey(expr.scrutinee.name));
      }
      for (const c of expr.cases) {
        if (c.guard) collectUses(c.guard, uses);
        collectUses(c.body, uses);
      }
      break;
  }
};

const collectUsesBinding = (binding: IR.IRBinding, uses: Set<string>): void => {
  const addAtom = (atom: IR.Atom) => {
    if (atom.kind === "AVar") {
      uses.add(nameKey(atom.name));
    }
  };

  switch (binding.kind) {
    case "IRBAtom":
      addAtom(binding.atom);
      break;

    case "IRBApp":
      addAtom(binding.func);
      addAtom(binding.arg);
      break;

    case "IRBBinOp":
      addAtom(binding.left);
      addAtom(binding.right);
      break;

    case "IRBTuple":
      for (const elem of binding.elements) {
        addAtom(elem);
      }
      break;

    case "IRBRecord":
      for (const f of binding.fields) {
        addAtom(f.value);
      }
      break;

    case "IRBRecordUpdate":
      addAtom(binding.record);
      for (const f of binding.fields) {
        addAtom(f.value);
      }
      break;

    case "IRBField":
      addAtom(binding.record);
      break;

    case "IRBLambda":
      collectUses(binding.body, uses);
      break;

    case "IRBForeign":
      for (const arg of binding.args) {
        addAtom(arg);
      }
      break;

    case "IRBMatch":
      addAtom(binding.scrutinee);
      for (const c of binding.cases) {
        if (c.guard) collectUses(c.guard, uses);
        collectUses(c.body, uses);
      }
      break;
  }
};

const removeUnused = (expr: IR.IRExpr, uses: Set<string>): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBinding = removeUnusedBinding(expr.binding, uses);
      const newBody = removeUnused(expr.body, uses);

      // Remove if not used
      if (!uses.has(nameKey(expr.name))) {
        return newBody;
      }

      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const usedBindings = expr.bindings.filter((b) => uses.has(nameKey(b.name)));
      const newBindings = usedBindings.map((b) => ({
        name: b.name,
        binding: removeUnusedBinding(b.binding, uses),
      }));
      const newBody = removeUnused(expr.body, uses);

      if (newBindings.length === 0) {
        return newBody;
      }

      return IR.irletrec(newBindings, newBody);
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? removeUnused(c.guard, uses) : null,
        body: removeUnused(c.body, uses),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const removeUnusedBinding = (binding: IR.IRBinding, uses: Set<string>): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
    case "IRBMatch":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, removeUnused(binding.body, uses), binding.type);
  }
};

// =============================================================================
// Tail Call Optimization
// =============================================================================

/**
 * Collect all parameters from nested lambdas.
 */
const collectLambdaParams = (binding: IR.IRBLambda): Name[] => {
  const params: Name[] = [binding.param];
  let body = binding.body;

  while (body.kind === "IRLet" && body.binding.kind === "IRBLambda") {
    params.push(body.binding.param);
    body = body.binding.body;
  }

  return params;
};

/**
 * Get the innermost body of nested lambdas.
 */
const getInnermostBody = (binding: IR.IRBLambda): IR.IRExpr => {
  let body = binding.body;

  while (body.kind === "IRLet" && body.binding.kind === "IRBLambda") {
    body = body.binding.body;
  }

  return body;
};

/**
 * Check if an expression contains a call to the named function.
 */
const hasCallTo = (expr: IR.IRExpr, funcName: Name): boolean => {
  switch (expr.kind) {
    case "IRAtom":
      return false;

    case "IRLet":
      return hasCallToBinding(expr.binding, funcName) || hasCallTo(expr.body, funcName);

    case "IRLetRec":
      return (
        expr.bindings.some((b) => hasCallToBinding(b.binding, funcName)) ||
        hasCallTo(expr.body, funcName)
      );

    case "IRMatch":
      return expr.cases.some((c) => {
        if (c.guard && hasCallTo(c.guard, funcName)) return true;
        return hasCallTo(c.body, funcName);
      });
  }
};

const hasCallToBinding = (binding: IR.IRBinding, funcName: Name): boolean => {
  switch (binding.kind) {
    case "IRBAtom":
      return binding.atom.kind === "AVar" && nameEq(binding.atom.name, funcName);

    case "IRBApp":
      return (
        (binding.func.kind === "AVar" && nameEq(binding.func.name, funcName)) ||
        (binding.arg.kind === "AVar" && nameEq(binding.arg.name, funcName))
      );

    case "IRBBinOp":
      return (
        (binding.left.kind === "AVar" && nameEq(binding.left.name, funcName)) ||
        (binding.right.kind === "AVar" && nameEq(binding.right.name, funcName))
      );

    case "IRBTuple":
      return binding.elements.some((e) => e.kind === "AVar" && nameEq(e.name, funcName));

    case "IRBRecord":
      return binding.fields.some((f) => f.value.kind === "AVar" && nameEq(f.value.name, funcName));

    case "IRBRecordUpdate":
      return (
        (binding.record.kind === "AVar" && nameEq(binding.record.name, funcName)) ||
        binding.fields.some((f) => f.value.kind === "AVar" && nameEq(f.value.name, funcName))
      );

    case "IRBField":
      return binding.record.kind === "AVar" && nameEq(binding.record.name, funcName);

    case "IRBLambda":
      return hasCallTo(binding.body, funcName);

    case "IRBForeign":
      return binding.args.some((a) => a.kind === "AVar" && nameEq(a.name, funcName));

    case "IRBMatch":
      return (
        (binding.scrutinee.kind === "AVar" && nameEq(binding.scrutinee.name, funcName)) ||
        binding.cases.some(
          (c) => hasCallTo(c.body, funcName) || (c.guard && hasCallTo(c.guard, funcName)),
        )
      );
  }
};

/**
 * Check if all calls to funcName in the expression are in tail position.
 */
const isTailCallOnly = (expr: IR.IRExpr, funcName: Name): boolean => {
  switch (expr.kind) {
    case "IRAtom":
      return true;

    case "IRLet": {
      // Check if binding has non-tail calls
      if (hasNonTailCall(expr.binding, funcName)) {
        return false;
      }
      return isTailCallOnly(expr.body, funcName);
    }

    case "IRLetRec":
      return isTailCallOnly(expr.body, funcName);

    case "IRMatch":
      return expr.cases.every((c) => {
        if (c.guard && hasCallTo(c.guard, funcName)) return false;
        return isTailCallOnly(c.body, funcName);
      });
  }
};

const hasNonTailCall = (binding: IR.IRBinding, funcName: Name): boolean => {
  switch (binding.kind) {
    case "IRBAtom":
      // Atom referencing funcName is not a call, just a reference
      return false;

    case "IRBApp":
      // If func is funcName, check if arg references funcName
      if (binding.arg.kind === "AVar" && nameEq(binding.arg.name, funcName)) {
        return true;
      }
      return false;

    case "IRBBinOp":
      // Binary ops with funcName reference are non-tail calls
      return (
        (binding.left.kind === "AVar" && nameEq(binding.left.name, funcName)) ||
        (binding.right.kind === "AVar" && nameEq(binding.right.name, funcName))
      );

    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return false;

    case "IRBLambda":
      // Lambdas don't count as tail calls
      return false;

    case "IRBMatch":
      // Match bindings - scrutinee is not a tail position
      return binding.scrutinee.kind === "AVar" && nameEq(binding.scrutinee.name, funcName);
  }
};

/**
 * Mark a lambda as tail-recursive.
 */
const markTailRecursive = (
  binding: IR.IRBLambda,
  funcName: Name,
  params: readonly Name[],
): IR.IRBLambda => ({
  ...binding,
  tailRecursive: { funcName, params },
});

/**
 * Transform expression, marking tail-recursive functions.
 */
const transformTCO = (expr: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBinding = transformBindingTCO(expr.binding);
      return IR.irlet(expr.name, newBinding, transformTCO(expr.body));
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => {
        if (b.binding.kind === "IRBLambda") {
          const params = collectLambdaParams(b.binding);
          const innerBody = getInnermostBody(b.binding);

          // Check if this is tail-recursive
          if (hasCallTo(innerBody, b.name) && isTailCallOnly(innerBody, b.name)) {
            const marked = markTailRecursive(b.binding, b.name, params);
            return { name: b.name, binding: transformBindingTCO(marked) };
          }
        }
        return { name: b.name, binding: transformBindingTCO(b.binding) };
      });
      return IR.irletrec(newBindings, transformTCO(expr.body));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? transformTCO(c.guard) : null,
        body: transformTCO(c.body),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const transformBindingTCO = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
    case "IRBMatch":
      return binding;

    case "IRBLambda":
      return {
        ...binding,
        body: transformTCO(binding.body),
      };
  }
};

// =============================================================================
// Inline Trivial Bindings
// =============================================================================

type InlineEnv = Map<string, IR.Atom>;

/** Inline trivial atom bindings (let x = y in ... → substitute y for x) */
const inlineExpr = (expr: IR.IRExpr, env: InlineEnv): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom": {
      if (expr.atom.kind === "AVar") {
        const inlined = env.get(nameKey(expr.atom.name));
        if (inlined) return IR.iratom(inlined);
      }
      return expr;
    }

    case "IRLet": {
      const newBinding = inlineBinding(expr.binding, env);

      // If binding is just an atom, add to inline env
      if (newBinding.kind === "IRBAtom") {
        const newEnv = new Map(env);
        newEnv.set(nameKey(expr.name), newBinding.atom);
        return inlineExpr(expr.body, newEnv);
      }

      // At this point, newBinding is not an atom (non-trivial binding)
      const newBody = inlineExpr(expr.body, env);
      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: inlineBinding(b.binding, env),
      }));
      return IR.irletrec(newBindings, inlineExpr(expr.body, env));
    }

    case "IRMatch": {
      const newScrutinee = inlineAtom(expr.scrutinee, env);
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineExpr(c.guard, env) : null,
        body: inlineExpr(c.body, env),
      }));
      return IR.irmatch(newScrutinee, cases, expr.type);
    }
  }
};

const inlineAtom = (atom: IR.Atom, env: InlineEnv): IR.Atom => {
  if (atom.kind === "AVar") {
    const inlined = env.get(nameKey(atom.name));
    if (inlined) return inlined;
  }
  return atom;
};

const inlineBinding = (binding: IR.IRBinding, env: InlineEnv): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
      return IR.irbatom(inlineAtom(binding.atom, env));

    case "IRBApp":
      return IR.irbapp(inlineAtom(binding.func, env), inlineAtom(binding.arg, env), binding.type);

    case "IRBBinOp":
      return IR.irbbinop(
        binding.op,
        inlineAtom(binding.left, env),
        inlineAtom(binding.right, env),
        binding.type,
      );

    case "IRBTuple":
      return IR.irbtuple(
        binding.elements.map((e) => inlineAtom(e, env)),
        binding.type,
      );

    case "IRBRecord":
      return IR.irbrecord(
        binding.fields.map((f) => ({ name: f.name, value: inlineAtom(f.value, env) })),
        binding.type,
      );

    case "IRBRecordUpdate":
      return IR.irbrecordupdate(
        inlineAtom(binding.record, env),
        binding.fields.map((f) => ({ name: f.name, value: inlineAtom(f.value, env) })),
        binding.type,
      );

    case "IRBField":
      return IR.irbfield(inlineAtom(binding.record, env), binding.field, binding.type);

    case "IRBLambda":
      return IR.irblambda(binding.param, inlineExpr(binding.body, env), binding.type);

    case "IRBForeign":
      return IR.irbforeign(
        binding.module,
        binding.name,
        binding.args.map((a) => inlineAtom(a, env)),
        binding.type,
      );

    case "IRBMatch": {
      const newScrutinee = inlineAtom(binding.scrutinee, env);
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineExpr(c.guard, env) : null,
        body: inlineExpr(c.body, env),
      }));
      return IR.irbmatch(newScrutinee, cases, binding.type);
    }
  }
};

// =============================================================================
// Optimization Pipeline
// =============================================================================

/**
 * Optimize a single IR expression.
 * @param expr The expression to optimize
 * @param initialEnv Optional initial environment with global values (for beta reduction)
 */
const optimizeExpr = (expr: IR.IRExpr, initialEnv?: ExtendedEnv): IR.IRExpr => {
  // 1. Constant folding and beta reduction
  let result = foldExpr(expr, initialEnv ?? new Map());

  // 2. Inline trivial bindings
  result = inlineExpr(result, new Map());

  // 3. Dead code elimination
  const uses = new Set<string>();
  collectUses(result, uses);
  result = removeUnused(result, uses);

  // 4. Tail call optimization
  result = transformTCO(result);

  return result;
};

/**
 * Optimize a declaration.
 */
const optimizeDecl = (decl: IR.IRDecl): IR.IRDecl => {
  switch (decl.kind) {
    case "IRDeclType":
      return decl;

    case "IRDeclLet":
      return IR.irdecllet(decl.name, optimizeBinding(decl.binding));

    case "IRDeclLetRec": {
      const newBindings = decl.bindings.map((b) => ({
        name: b.name,
        binding: optimizeBinding(b.binding),
      }));
      return IR.irdeclletrec(newBindings);
    }
  }
};

const optimizeBinding = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
    case "IRBMatch":
      return binding;

    case "IRBBinOp": {
      // Apply constant folding to binary operations
      const folded = tryFoldBinOp(binding.op, binding.left, binding.right, new Map());
      if (folded) {
        return IR.irbatom(folded);
      }
      return binding;
    }

    case "IRBLambda": {
      const optimizedBody = optimizeExpr(binding.body);

      // Try eta reduction: λx. f x → f (when x is not free in f)
      const etaReduced = tryEtaReduce(binding.param, optimizedBody);
      if (etaReduced) {
        return etaReduced;
      }

      return IR.irblambda(binding.param, optimizedBody, binding.type);
    }
  }
};

/**
 * Resolve transitive atom references in the inline environment.
 * If x -> y and y -> 7, then resolve x -> 7.
 */
const resolveTransitiveInlines = (env: InlineEnv): void => {
  // Keep resolving until no changes
  let changed = true;
  while (changed) {
    changed = false;
    for (const [key, atom] of env) {
      if (atom.kind === "AVar") {
        const resolved = env.get(nameKey(atom.name));
        if (resolved && resolved !== atom) {
          env.set(key, resolved);
          changed = true;
        }
      }
    }
  }
};

/**
 * Collect trivial atom bindings from declarations (for global inlining).
 */
const collectGlobalInlines = (decls: readonly IR.IRDecl[]): InlineEnv => {
  const env: InlineEnv = new Map();
  for (const decl of decls) {
    if (decl.kind === "IRDeclLet" && decl.binding.kind === "IRBAtom") {
      env.set(nameKey(decl.name), decl.binding.atom);
    }
  }
  // Resolve transitive references: if x -> y and y -> 7, then x -> 7
  resolveTransitiveInlines(env);
  return env;
};

/**
 * Collect extended values from declarations (for beta reduction).
 * This includes lambdas, constructors, and atoms.
 */
const collectGlobalExtended = (decls: readonly IR.IRDecl[]): ExtendedEnv => {
  const env: ExtendedEnv = new Map();
  for (const decl of decls) {
    if (decl.kind === "IRDeclLet") {
      const extValue = getExtendedValueFromBinding(decl.binding);
      if (extValue) {
        env.set(nameKey(decl.name), extValue);
      }
    }
  }
  return env;
};

/**
 * Filter out declarations that are trivial atom bindings.
 */
const filterTrivialDecls = (decls: readonly IR.IRDecl[], env: InlineEnv): IR.IRDecl[] => {
  return decls.filter((decl) => {
    if (decl.kind === "IRDeclLet") {
      return !env.has(nameKey(decl.name));
    }
    return true;
  });
};

/**
 * Apply global inlining to a declaration.
 */
const inlineDecl = (decl: IR.IRDecl, env: InlineEnv): IR.IRDecl => {
  switch (decl.kind) {
    case "IRDeclType":
      return decl;
    case "IRDeclLet":
      return IR.irdecllet(decl.name, inlineBinding(decl.binding, env));
    case "IRDeclLetRec": {
      const newBindings = decl.bindings.map((b) => ({
        name: b.name,
        binding: inlineBinding(b.binding, env),
      }));
      return IR.irdeclletrec(newBindings);
    }
  }
};

/**
 * Optimize an IR program.
 */
export const optimize = (program: IR.IRProgram): IR.IRProgram => {
  let decls = program.decls;
  let main = program.main;

  // Iterate until fixpoint: optimize -> inline -> optimize -> ...
  const MAX_ITERATIONS = 10;
  for (let i = 0; i < MAX_ITERATIONS; i++) {
    // Apply per-declaration optimizations
    decls = decls.map(optimizeDecl);

    // Collect global extended values (lambdas, constructors, etc.) for beta reduction
    const globalExtEnv = collectGlobalExtended(decls);

    // Apply main expression optimization with global environment
    main = main ? optimizeExpr(main, globalExtEnv) : null;

    // Collect and apply global inlining
    const globalEnv = collectGlobalInlines(decls);
    if (globalEnv.size === 0) break;

    const prevDeclCount = decls.length;
    decls = filterTrivialDecls(decls, globalEnv).map((d) => inlineDecl(d, globalEnv));
    if (main) {
      main = inlineExpr(main, globalEnv);
    }

    // If no declarations were removed, we've reached fixpoint
    if (decls.length === prevDeclCount) break;
  }

  return { decls, main };
};
