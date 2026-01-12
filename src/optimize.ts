/**
 * IR Optimization Passes (Section 11)
 *
 * Transforms the IR to improve performance:
 * - Constant Folding: evaluate literal expressions at compile time
 * - Beta Reduction: inline function applications (λx. body) arg → let x = arg in body
 * - Eta Reduction: simplify λx. f x → f (when x not free in f)
 * - Dead Code Elimination: remove unused bindings
 * - Tail Call Optimization: mark tail-recursive functions
 * - Case-of-Known-Constructor: simplify match when scrutinee is statically known
 * - Case-of-Case: push outer match into branches of inner match
 * - Let Floating Outward: hoist bindings out of lambdas for sharing
 * - Let Floating Inward: sink bindings into branches where they're used
 */

import * as IR from "./ir";
import { freshName, type Name } from "./core";
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
// Alpha Renaming (for safe beta reduction)
// =============================================================================

type RenameEnv = Map<string, Name>;

/** Freshen a name if it's in the rename environment, otherwise keep it */
const renameName = (name: Name, env: RenameEnv): Name => {
  const renamed = env.get(nameKey(name));
  return renamed ?? name;
};

/** Alpha-rename all bound variables in an expression with fresh names */
const alphaRenameExpr = (expr: IR.IRExpr, env: RenameEnv): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return IR.iratom(alphaRenameAtom(expr.atom, env));

    case "IRLet": {
      const newBinding = alphaRenameBinding(expr.binding, env);
      // Create fresh name for this binding
      const freshN = freshName(expr.name.original);
      const newEnv = new Map(env);
      newEnv.set(nameKey(expr.name), freshN);
      const newBody = alphaRenameExpr(expr.body, newEnv);
      return IR.irlet(freshN, newBinding, newBody);
    }

    case "IRLetRec": {
      // First, create fresh names for all bindings
      const newEnv = new Map(env);
      const freshNames: Name[] = [];
      for (const b of expr.bindings) {
        const freshN = freshName(b.name.original);
        freshNames.push(freshN);
        newEnv.set(nameKey(b.name), freshN);
      }
      // Then rename all bindings and body with the new env
      const newBindings = expr.bindings.map((b, i) => ({
        name: freshNames[i]!,
        binding: alphaRenameBinding(b.binding, newEnv),
      }));
      const newBody = alphaRenameExpr(expr.body, newEnv);
      return IR.irletrec(newBindings, newBody);
    }

    case "IRMatch": {
      const newScrutinee = alphaRenameAtom(expr.scrutinee, env);
      const newCases = expr.cases.map((c) => alphaRenameCase(c, env));
      return IR.irmatch(newScrutinee, newCases, expr.type);
    }
  }
};

const alphaRenameAtom = (atom: IR.Atom, env: RenameEnv): IR.Atom => {
  switch (atom.kind) {
    case "AVar":
      return IR.avar(renameName(atom.name, env), atom.type);
    case "ALit":
    case "ACon":
      return atom;
  }
};

const alphaRenameBinding = (binding: IR.IRBinding, env: RenameEnv): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
      return IR.irbatom(alphaRenameAtom(binding.atom, env));

    case "IRBApp":
      return IR.irbapp(
        alphaRenameAtom(binding.func, env),
        alphaRenameAtom(binding.arg, env),
        binding.type,
      );

    case "IRBBinOp":
      return IR.irbbinop(
        binding.op,
        alphaRenameAtom(binding.left, env),
        alphaRenameAtom(binding.right, env),
        binding.type,
      );

    case "IRBTuple":
      return IR.irbtuple(
        binding.elements.map((e) => alphaRenameAtom(e, env)),
        binding.type,
      );

    case "IRBRecord":
      return IR.irbrecord(
        binding.fields.map((f) => ({ name: f.name, value: alphaRenameAtom(f.value, env) })),
        binding.type,
      );

    case "IRBRecordUpdate":
      return IR.irbrecordupdate(
        alphaRenameAtom(binding.record, env),
        binding.fields.map((f) => ({ name: f.name, value: alphaRenameAtom(f.value, env) })),
        binding.type,
      );

    case "IRBField":
      return IR.irbfield(alphaRenameAtom(binding.record, env), binding.field, binding.type);

    case "IRBLambda": {
      // Create fresh name for lambda parameter
      const freshParam = freshName(binding.param.original);
      const newEnv = new Map(env);
      newEnv.set(nameKey(binding.param), freshParam);
      const newBody = alphaRenameExpr(binding.body, newEnv);
      return IR.irblambda(freshParam, newBody, binding.type);
    }

    case "IRBForeign":
      return IR.irbforeign(
        binding.module,
        binding.name,
        binding.args.map((a) => alphaRenameAtom(a, env)),
        binding.type,
      );

    case "IRBMatch": {
      const newScrutinee = alphaRenameAtom(binding.scrutinee, env);
      const newCases = binding.cases.map((c) => alphaRenameCase(c, env));
      return IR.irbmatch(newScrutinee, newCases, binding.type);
    }
  }
};

const alphaRenameCase = (c: IR.IRCase, env: RenameEnv): IR.IRCase => {
  // Pattern binds new variables - freshen them
  const patternEnv = new Map(env);
  const newPattern = alphaRenamePattern(c.pattern, patternEnv);
  const newGuard = c.guard ? alphaRenameExpr(c.guard, patternEnv) : null;
  const newBody = alphaRenameExpr(c.body, patternEnv);
  return { pattern: newPattern, guard: newGuard, body: newBody };
};

const alphaRenamePattern = (pattern: IR.IRPattern, env: RenameEnv): IR.IRPattern => {
  switch (pattern.kind) {
    case "IRPWild":
    case "IRPLit":
      return pattern;

    case "IRPVar": {
      const freshN = freshName(pattern.name.original);
      env.set(nameKey(pattern.name), freshN);
      return IR.irpvar(freshN, pattern.type);
    }

    case "IRPCon":
      return IR.irpcon(
        pattern.name,
        pattern.args.map((p) => alphaRenamePattern(p, env)),
        pattern.type,
      );

    case "IRPTuple":
      return IR.irptuple(pattern.elements.map((p) => alphaRenamePattern(p, env)));

    case "IRPRecord":
      return IR.irprecord(
        pattern.fields.map((f) => ({ name: f.name, pattern: alphaRenamePattern(f.pattern, env) })),
      );

    case "IRPAs": {
      const freshN = freshName(pattern.name.original);
      env.set(nameKey(pattern.name), freshN);
      const newSubPattern = alphaRenamePattern(pattern.pattern, env);
      return IR.irpas(freshN, newSubPattern, pattern.type);
    }
  }
};

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
          // Beta reduction with alpha-renaming to avoid name conflicts
          // when the same lambda is inlined multiple times.
          // We create a fresh name for the parameter and rename the body
          // so that the parameter references match.
          const freshParam = freshName(lambdaVal.param.original);
          const renameEnv: RenameEnv = new Map();
          renameEnv.set(nameKey(lambdaVal.param), freshParam);
          const renamedBody = alphaRenameExpr(lambdaVal.body, renameEnv);
          const paramBinding = IR.irlet(
            freshParam,
            IR.irbatom(expr.binding.arg),
            bindResultTo(renamedBody, expr.name, expr.body),
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
 * Check if a known value is fully resolved (no unknown variables).
 * We can only safely optimize matches when all nested values are known.
 */
const isFullyKnown = (value: KnownValue, env: ExtendedEnv): boolean => {
  switch (value.kind) {
    case "lit":
      return true;
    case "con": {
      // Check all constructor arguments
      for (const arg of value.args) {
        const argValue = getKnownValueExt(arg, env);
        if (!argValue || !isFullyKnown(argValue, env)) return false;
      }
      return true;
    }
    case "tuple": {
      // Check all tuple elements
      for (const elem of value.elements) {
        const elemValue = getKnownValueExt(elem, env);
        if (!elemValue || !isFullyKnown(elemValue, env)) return false;
      }
      return true;
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

  // Only optimize if the value is fully known (no unknown nested variables).
  // Otherwise, we might incorrectly pick a wildcard case when a constructor
  // pattern should match at runtime.
  if (!isFullyKnown(knownValue, env)) return null;

  // Find matching case
  for (const c of cases) {
    // Skip cases with guards - too complex for static analysis
    if (c.guard) continue;

    const matchResult = matchPattern(c.pattern, knownValue, scrutinee, env);
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
  env: ExtendedEnv,
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
        const argValue = getKnownValueExt(valArg, env);
        if (!argValue) {
          // Argument is unknown - can still match variable/wildcard patterns
          if (patArg.kind === "IRPVar") {
            bindings.push({ name: patArg.name, atom: valArg });
            continue;
          }
          if (patArg.kind === "IRPWild") {
            continue;
          }
          return null; // Can't statically analyze argument
        }
        const subMatch = matchPattern(patArg, argValue, valArg, env);
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
        const elemValue = getKnownValueExt(valElem, env);
        if (!elemValue) {
          // Element value is unknown (e.g., a variable) - can still match
          // variable patterns or wildcards, but not constructor patterns
          if (patElem.kind === "IRPVar") {
            tupleBindings.push({ name: patElem.name, atom: valElem });
            continue;
          }
          if (patElem.kind === "IRPWild") {
            continue;
          }
          // Can't match constructor/literal patterns against unknown value
          return null;
        }
        const subMatch = matchPattern(patElem, elemValue, valElem, env);
        if (!subMatch) return null;
        tupleBindings.push(...subMatch.bindings);
      }
      return { bindings: tupleBindings };
    }

    case "IRPRecord":
      // Record patterns are more complex, skip for now
      return null;

    case "IRPAs": {
      // As-pattern: bind the whole value and match sub-pattern
      const subMatch = matchPattern(pattern.pattern, value, scrutinee, env);
      if (!subMatch) return null;
      return { bindings: [{ name: pattern.name, atom: scrutinee }, ...subMatch.bindings] };
    }
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

      // Remove if not used AND doesn't have side effects
      // Applications are conservatively assumed to potentially have side effects
      // since they might call foreign functions
      if (!uses.has(nameKey(expr.name)) && !hasSideEffects(expr.binding)) {
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
// Function Inlining
// =============================================================================

/**
 * Function Inlining (Section 11.7)
 *
 * Inline function bodies when:
 * - Function is called exactly once, OR
 * - Function body is "small" (< threshold nodes)
 *
 * Do NOT inline:
 * - Recursive functions (would cause infinite expansion)
 * - Functions with side effects (foreign calls in body)
 */

/** Maximum body size for unconditional inlining */
const MAX_INLINE_SIZE = 15;

/** Counts for uses and call sites */
type UseCounts = {
  uses: Map<string, number>; // Total references
  calls: Map<string, number>; // References in function position of application
};

/** Count how many times each variable is used and called in an expression */
const countUses = (expr: IR.IRExpr): UseCounts => {
  const uses = new Map<string, number>();
  const calls = new Map<string, number>();
  countUsesExpr(expr, uses, calls);
  return { uses, calls };
};

const countUsesExpr = (
  expr: IR.IRExpr,
  uses: Map<string, number>,
  calls: Map<string, number>,
): void => {
  switch (expr.kind) {
    case "IRAtom":
      countUsesAtom(expr.atom, uses);
      break;

    case "IRLet":
      countUsesBinding(expr.binding, uses, calls);
      countUsesExpr(expr.body, uses, calls);
      break;

    case "IRLetRec":
      for (const b of expr.bindings) {
        countUsesBinding(b.binding, uses, calls);
      }
      countUsesExpr(expr.body, uses, calls);
      break;

    case "IRMatch":
      countUsesAtom(expr.scrutinee, uses);
      for (const c of expr.cases) {
        if (c.guard) countUsesExpr(c.guard, uses, calls);
        countUsesExpr(c.body, uses, calls);
      }
      break;
  }
};

const countUsesAtom = (atom: IR.Atom, uses: Map<string, number>): void => {
  if (atom.kind === "AVar") {
    const key = nameKey(atom.name);
    uses.set(key, (uses.get(key) ?? 0) + 1);
  }
};

const countUsesBinding = (
  binding: IR.IRBinding,
  uses: Map<string, number>,
  calls: Map<string, number>,
): void => {
  switch (binding.kind) {
    case "IRBAtom":
      countUsesAtom(binding.atom, uses);
      break;

    case "IRBApp":
      // Count the function in both uses and calls
      countUsesAtom(binding.func, uses);
      if (binding.func.kind === "AVar") {
        const key = nameKey(binding.func.name);
        calls.set(key, (calls.get(key) ?? 0) + 1);
      }
      countUsesAtom(binding.arg, uses);
      break;

    case "IRBBinOp":
      countUsesAtom(binding.left, uses);
      countUsesAtom(binding.right, uses);
      break;

    case "IRBTuple":
      for (const e of binding.elements) countUsesAtom(e, uses);
      break;

    case "IRBRecord":
      for (const f of binding.fields) countUsesAtom(f.value, uses);
      break;

    case "IRBRecordUpdate":
      countUsesAtom(binding.record, uses);
      for (const f of binding.fields) countUsesAtom(f.value, uses);
      break;

    case "IRBField":
      countUsesAtom(binding.record, uses);
      break;

    case "IRBLambda":
      countUsesExpr(binding.body, uses, calls);
      break;

    case "IRBForeign":
      for (const a of binding.args) countUsesAtom(a, uses);
      break;

    case "IRBMatch":
      countUsesAtom(binding.scrutinee, uses);
      for (const c of binding.cases) {
        if (c.guard) countUsesExpr(c.guard, uses, calls);
        countUsesExpr(c.body, uses, calls);
      }
      break;
  }
};

/** Check if a binding contains foreign calls (side effects) */
const hasSideEffects = (binding: IR.IRBinding): boolean => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
      return false;

    // Applications are conservatively assumed to potentially have side effects
    // since we can't statically determine if the function being called is a
    // foreign function with side effects (like IO.printLine)
    case "IRBApp":
    case "IRBForeign":
      return true;

    case "IRBLambda":
      return exprHasSideEffects(binding.body);

    case "IRBMatch":
      return binding.cases.some(
        (c) => (c.guard && exprHasSideEffects(c.guard)) || exprHasSideEffects(c.body),
      );
  }
};

const exprHasSideEffects = (expr: IR.IRExpr): boolean => {
  switch (expr.kind) {
    case "IRAtom":
      return false;

    case "IRLet":
      return hasSideEffects(expr.binding) || exprHasSideEffects(expr.body);

    case "IRLetRec":
      return expr.bindings.some((b) => hasSideEffects(b.binding)) || exprHasSideEffects(expr.body);

    case "IRMatch":
      return expr.cases.some(
        (c) => (c.guard && exprHasSideEffects(c.guard)) || exprHasSideEffects(c.body),
      );
  }
};

/** Check if a lambda body references its own name (recursive) */
const isRecursive = (funcName: Name, binding: IR.IRBinding): boolean => {
  if (binding.kind !== "IRBLambda") return false;
  const fv = new Set<string>();
  collectFreeVars(binding.body, new Set([nameKey(binding.param)]), fv);
  return fv.has(nameKey(funcName));
};

type FunctionEnv = Map<string, { binding: IR.IRBLambda }>;

/**
 * Inline functions that are used once or have small bodies.
 */
const inlineFunctions = (expr: IR.IRExpr): IR.IRExpr => {
  // First pass: count uses and calls
  const counts = countUses(expr);

  // Second pass: inline eligible functions
  return inlineFunctionsExpr(expr, new Map(), counts);
};

const inlineFunctionsExpr = (expr: IR.IRExpr, env: FunctionEnv, counts: UseCounts): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const key = nameKey(expr.name);
      const useCount = counts.uses.get(key) ?? 0;
      const callCount = counts.calls.get(key) ?? 0;

      // Check if this is an inlineable function
      if (expr.binding.kind === "IRBLambda") {
        const lambda = expr.binding;
        const size = bindingSizeForInline(lambda);
        const recursive = isRecursive(expr.name, lambda);
        const sideEffects = hasSideEffects(lambda);

        // Can only remove binding if ALL uses are calls (we can inline all of them)
        const allUsesAreCalls = useCount === callCount;

        // Inline if: (used once OR small) AND not recursive AND no side effects
        const shouldInline =
          !recursive && !sideEffects && (useCount === 1 || size <= MAX_INLINE_SIZE);

        if (shouldInline) {
          // Add to environment for inlining at call sites
          const newEnv = new Map(env);
          newEnv.set(key, { binding: lambda });
          const newBody = inlineFunctionsExpr(expr.body, newEnv, counts);

          // Only remove the binding if ALL uses are calls that we can inline
          // If there are non-call uses (e.g., returning the function as a value),
          // we must keep the binding
          if (useCount === 1 && allUsesAreCalls) {
            return newBody;
          }

          // Otherwise keep the binding but also inline at call sites
          return IR.irlet(expr.name, inlineFunctionsBinding(lambda, env, counts), newBody);
        }
      }

      // Not inlineable - process normally
      const newBinding = inlineFunctionsBinding(expr.binding, env, counts);
      const newBody = inlineFunctionsExpr(expr.body, env, counts);
      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      // Don't inline recursive bindings from letrec
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: inlineFunctionsBinding(b.binding, env, counts),
      }));
      const newBody = inlineFunctionsExpr(expr.body, env, counts);
      return IR.irletrec(newBindings, newBody);
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineFunctionsExpr(c.guard, env, counts) : null,
        body: inlineFunctionsExpr(c.body, env, counts),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const inlineFunctionsBinding = (
  binding: IR.IRBinding,
  env: FunctionEnv,
  counts: UseCounts,
): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return binding;

    case "IRBApp": {
      // Check if we're calling an inlineable function
      if (binding.func.kind === "AVar") {
        const funcInfo = env.get(nameKey(binding.func.name));
        if (funcInfo) {
          // Inline the function: (λx. body) arg → let x = arg in body
          // Use alpha-renaming to avoid name conflicts
          const lambda = funcInfo.binding;
          const freshParam = freshName(lambda.param.original);
          const renameEnv: RenameEnv = new Map();
          renameEnv.set(nameKey(lambda.param), freshParam);
          const renamedBody = alphaRenameExpr(lambda.body, renameEnv);

          // Return a match binding that evaluates to the inlined body
          // We wrap in a let to bind the argument
          // Since we're in a binding context, we return an IRBMatch that does the work
          return IR.irbmatch(
            binding.arg,
            [
              {
                pattern: IR.irpvar(freshParam, binding.arg.type),
                guard: null,
                body: inlineFunctionsExpr(renamedBody, env, counts),
              },
            ],
            binding.type,
          );
        }
      }
      return binding;
    }

    case "IRBLambda":
      return IR.irblambda(
        binding.param,
        inlineFunctionsExpr(binding.body, env, counts),
        binding.type,
      );

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? inlineFunctionsExpr(c.guard, env, counts) : null,
        body: inlineFunctionsExpr(c.body, env, counts),
      }));
      return IR.irbmatch(binding.scrutinee, cases, binding.type);
    }
  }
};

/** Size calculation for inlining decisions (slightly different from duplication heuristics) */
const bindingSizeForInline = (binding: IR.IRBinding): number => {
  switch (binding.kind) {
    case "IRBAtom":
      return 1;
    case "IRBApp":
    case "IRBBinOp":
    case "IRBField":
      return 2;
    case "IRBTuple":
      return binding.elements.length;
    case "IRBRecord":
    case "IRBRecordUpdate":
      return binding.fields.length;
    case "IRBLambda":
      return 1 + exprSizeForInline(binding.body);
    case "IRBForeign":
      return 5 + binding.args.length; // Foreign calls are "expensive"
    case "IRBMatch":
      return (
        3 +
        binding.cases.reduce(
          (acc, c) => acc + exprSizeForInline(c.body) + (c.guard ? exprSizeForInline(c.guard) : 0),
          0,
        )
      );
  }
};

const exprSizeForInline = (expr: IR.IRExpr): number => {
  switch (expr.kind) {
    case "IRAtom":
      return 1;
    case "IRLet":
      return 1 + bindingSizeForInline(expr.binding) + exprSizeForInline(expr.body);
    case "IRLetRec":
      return (
        1 +
        expr.bindings.reduce((acc, b) => acc + bindingSizeForInline(b.binding), 0) +
        exprSizeForInline(expr.body)
      );
    case "IRMatch":
      return (
        1 +
        expr.cases.reduce(
          (acc, c) => acc + exprSizeForInline(c.body) + (c.guard ? exprSizeForInline(c.guard) : 0),
          0,
        )
      );
  }
};

// =============================================================================
// Case-of-Case Transformation
// =============================================================================

/**
 * Case-of-case transformation.
 * Transforms:
 *   let inner = match x when P1 -> e1 when P2 -> e2 ...
 *   in match inner when Q1 -> body1 when Q2 -> body2 ...
 * Into:
 *   match x
 *     when P1 -> match e1 when Q1 -> body1 when Q2 -> body2 ...
 *     when P2 -> match e2 when Q1 -> body1 when Q2 -> body2 ...
 *
 * This exposes more opportunities for case-of-known-constructor optimization.
 * To avoid code explosion, we limit transformation based on size heuristics.
 */

/** Estimate the "size" of an expression for code duplication heuristics */
const exprSize = (expr: IR.IRExpr): number => {
  switch (expr.kind) {
    case "IRAtom":
      return 1;
    case "IRLet":
      return 1 + bindingSize(expr.binding) + exprSize(expr.body);
    case "IRLetRec":
      return (
        1 + expr.bindings.reduce((acc, b) => acc + bindingSize(b.binding), 0) + exprSize(expr.body)
      );
    case "IRMatch":
      return (
        1 +
        expr.cases.reduce((acc, c) => acc + exprSize(c.body) + (c.guard ? exprSize(c.guard) : 0), 0)
      );
  }
};

const bindingSize = (binding: IR.IRBinding): number => {
  switch (binding.kind) {
    case "IRBAtom":
      return 1;
    case "IRBApp":
    case "IRBBinOp":
    case "IRBField":
      return 2;
    case "IRBTuple":
      return binding.elements.length;
    case "IRBRecord":
    case "IRBRecordUpdate":
      return binding.fields.length;
    case "IRBLambda":
      return 1 + exprSize(binding.body);
    case "IRBForeign":
      return 1 + binding.args.length;
    case "IRBMatch":
      return (
        1 +
        binding.cases.reduce(
          (acc, c) => acc + exprSize(c.body) + (c.guard ? exprSize(c.guard) : 0),
          0,
        )
      );
  }
};

/** Maximum size of outer cases to allow duplication */
const MAX_OUTER_CASES_SIZE = 50;

/** Maximum number of inner branches to transform */
const MAX_INNER_BRANCHES = 8;

/**
 * Try to apply case-of-case transformation.
 * Returns the transformed expression if applicable, null otherwise.
 */
const tryCaseOfCase = (expr: IR.IRLet): IR.IRExpr | null => {
  // Pattern: let inner = match x ... in <expr using inner as match scrutinee>
  if (expr.binding.kind !== "IRBMatch") return null;

  const innerMatch = expr.binding;
  const innerName = expr.name;

  // Check if inner match has too many branches
  if (innerMatch.cases.length > MAX_INNER_BRANCHES) return null;

  // Find the outer match that uses innerName as scrutinee
  const outerMatchInfo = findOuterMatch(expr.body, innerName);
  if (!outerMatchInfo) return null;

  const { outerMatch, context } = outerMatchInfo;

  // Check size heuristic: outer cases shouldn't be too large
  const outerSize = outerMatch.cases.reduce(
    (acc, c) => acc + exprSize(c.body) + (c.guard ? exprSize(c.guard) : 0),
    0,
  );
  if (outerSize > MAX_OUTER_CASES_SIZE) return null;

  // Transform: push outer match into each inner branch
  const newCases: IR.IRCase[] = innerMatch.cases.map((innerCase) => {
    // The body of the inner case becomes the scrutinee context for the outer match
    // We need to substitute the inner case body for 'innerName' in the outer match
    const newBody = substituteInnerResult(innerCase.body, innerName, outerMatch, context);
    return {
      pattern: innerCase.pattern,
      guard: innerCase.guard,
      body: newBody,
    };
  });

  return IR.irmatch(innerMatch.scrutinee, newCases, outerMatch.type);
};

type OuterMatchInfo = {
  outerMatch: IR.IRMatch | IR.IRBMatch;
  context: (result: IR.IRExpr) => IR.IRExpr;
};

/**
 * Find an outer match expression that uses the given name as its scrutinee.
 * Returns the match and a context function to reconstruct the surrounding expression.
 */
const findOuterMatch = (expr: IR.IRExpr, targetName: Name): OuterMatchInfo | null => {
  switch (expr.kind) {
    case "IRAtom":
      return null;

    case "IRMatch":
      // Direct match on the target
      if (expr.scrutinee.kind === "AVar" && nameEq(expr.scrutinee.name, targetName)) {
        return {
          outerMatch: expr,
          context: (result) => result,
        };
      }
      return null;

    case "IRLet": {
      // Check if this let's binding is a match on the target
      if (expr.binding.kind === "IRBMatch") {
        if (
          expr.binding.scrutinee.kind === "AVar" &&
          nameEq(expr.binding.scrutinee.name, targetName)
        ) {
          return {
            outerMatch: expr.binding,
            context: (result) => {
              // The result replaces the match binding, need to extract result into the let body
              return bindResultTo(result, expr.name, expr.body);
            },
          };
        }
      }

      // Otherwise look in the body (only if binding doesn't shadow targetName)
      if (nameEq(expr.name, targetName)) return null;

      const innerResult = findOuterMatch(expr.body, targetName);
      if (innerResult) {
        return {
          outerMatch: innerResult.outerMatch,
          context: (result) => IR.irlet(expr.name, expr.binding, innerResult.context(result)),
        };
      }
      return null;
    }

    case "IRLetRec": {
      // Check if any binding shadows targetName
      if (expr.bindings.some((b) => nameEq(b.name, targetName))) return null;

      const innerResult = findOuterMatch(expr.body, targetName);
      if (innerResult) {
        return {
          outerMatch: innerResult.outerMatch,
          context: (result) => IR.irletrec(expr.bindings, innerResult.context(result)),
        };
      }
      return null;
    }
  }
};

/**
 * Substitute the result of an inner match case into the outer match.
 * The inner case body is evaluated, then the outer match is applied to it.
 */
const substituteInnerResult = (
  innerBody: IR.IRExpr,
  innerName: Name,
  outerMatch: IR.IRMatch | IR.IRBMatch,
  context: (result: IR.IRExpr) => IR.IRExpr,
): IR.IRExpr => {
  // We need to bind the result of innerBody to innerName, then apply the outer match
  // If innerBody is just an atom, we can substitute directly
  if (innerBody.kind === "IRAtom") {
    // Create the outer match with the atom as scrutinee
    const newOuterMatch =
      outerMatch.kind === "IRMatch"
        ? IR.irmatch(innerBody.atom, outerMatch.cases, outerMatch.type)
        : IR.irmatch(innerBody.atom, outerMatch.cases, outerMatch.type);
    return context(newOuterMatch);
  }

  // Otherwise, bind the inner body result to innerName and apply outer match
  return bindResultTo(innerBody, innerName, context(createMatchExpr(outerMatch, innerName)));
};

/** Create a match expression from an IRBMatch or IRMatch, using a variable as scrutinee */
const createMatchExpr = (match: IR.IRMatch | IR.IRBMatch, varName: Name): IR.IRExpr => {
  const scrutinee = IR.avar(varName, match.scrutinee.type);
  return IR.irmatch(scrutinee, match.cases, match.type);
};

/**
 * Apply case-of-case transformation throughout an expression.
 */
const applyCaseOfCase = (expr: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      // First, recursively transform children
      const newBinding = applyCaseOfCaseBinding(expr.binding);
      const newBody = applyCaseOfCase(expr.body);
      const newExpr = IR.irlet(expr.name, newBinding, newBody);

      // Then try case-of-case at this level
      const transformed = tryCaseOfCase(newExpr);
      if (transformed) {
        // Recursively apply to the result (may expose more opportunities)
        return applyCaseOfCase(transformed);
      }
      return newExpr;
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: applyCaseOfCaseBinding(b.binding),
      }));
      return IR.irletrec(newBindings, applyCaseOfCase(expr.body));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? applyCaseOfCase(c.guard) : null,
        body: applyCaseOfCase(c.body),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const applyCaseOfCaseBinding = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, applyCaseOfCase(binding.body), binding.type);

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? applyCaseOfCase(c.guard) : null,
        body: applyCaseOfCase(c.body),
      }));
      return IR.irbmatch(binding.scrutinee, cases, binding.type);
    }
  }
};

// =============================================================================
// Let Floating
// =============================================================================

/**
 * Let floating transformations:
 *
 * 1. Float-inward (let sinking): Move bindings closer to their use sites
 *    let x = e in match y when A -> ...x... when B -> ...
 *    → match y when A -> let x = e in ...x... when B -> ...
 *    (Only when x is used in one branch)
 *
 * 2. Float-outward (let hoisting): Move bindings out of lambdas
 *    let f = λp -> let x = e in body
 *    → let x = e in let f = λp -> body
 *    (Only when e doesn't reference p)
 */

/** Collect free variables in an expression */
const freeVars = (expr: IR.IRExpr): Set<string> => {
  const result = new Set<string>();
  collectFreeVars(expr, new Set(), result);
  return result;
};

const collectFreeVars = (expr: IR.IRExpr, bound: Set<string>, free: Set<string>): void => {
  switch (expr.kind) {
    case "IRAtom":
      collectFreeVarsAtom(expr.atom, bound, free);
      break;

    case "IRLet": {
      collectFreeVarsBinding(expr.binding, bound, free);
      const newBound = new Set(bound);
      newBound.add(nameKey(expr.name));
      collectFreeVars(expr.body, newBound, free);
      break;
    }

    case "IRLetRec": {
      const newBound = new Set(bound);
      for (const b of expr.bindings) {
        newBound.add(nameKey(b.name));
      }
      for (const b of expr.bindings) {
        collectFreeVarsBinding(b.binding, newBound, free);
      }
      collectFreeVars(expr.body, newBound, free);
      break;
    }

    case "IRMatch":
      collectFreeVarsAtom(expr.scrutinee, bound, free);
      for (const c of expr.cases) {
        const patternBound = new Set(bound);
        collectPatternBound(c.pattern, patternBound);
        if (c.guard) collectFreeVars(c.guard, patternBound, free);
        collectFreeVars(c.body, patternBound, free);
      }
      break;
  }
};

const collectFreeVarsAtom = (atom: IR.Atom, bound: Set<string>, free: Set<string>): void => {
  if (atom.kind === "AVar") {
    const key = nameKey(atom.name);
    if (!bound.has(key)) {
      free.add(key);
    }
  }
};

const collectFreeVarsBinding = (
  binding: IR.IRBinding,
  bound: Set<string>,
  free: Set<string>,
): void => {
  switch (binding.kind) {
    case "IRBAtom":
      collectFreeVarsAtom(binding.atom, bound, free);
      break;

    case "IRBApp":
      collectFreeVarsAtom(binding.func, bound, free);
      collectFreeVarsAtom(binding.arg, bound, free);
      break;

    case "IRBBinOp":
      collectFreeVarsAtom(binding.left, bound, free);
      collectFreeVarsAtom(binding.right, bound, free);
      break;

    case "IRBTuple":
      for (const e of binding.elements) {
        collectFreeVarsAtom(e, bound, free);
      }
      break;

    case "IRBRecord":
      for (const f of binding.fields) {
        collectFreeVarsAtom(f.value, bound, free);
      }
      break;

    case "IRBRecordUpdate":
      collectFreeVarsAtom(binding.record, bound, free);
      for (const f of binding.fields) {
        collectFreeVarsAtom(f.value, bound, free);
      }
      break;

    case "IRBField":
      collectFreeVarsAtom(binding.record, bound, free);
      break;

    case "IRBLambda": {
      const newBound = new Set(bound);
      newBound.add(nameKey(binding.param));
      collectFreeVars(binding.body, newBound, free);
      break;
    }

    case "IRBForeign":
      for (const a of binding.args) {
        collectFreeVarsAtom(a, bound, free);
      }
      break;

    case "IRBMatch":
      collectFreeVarsAtom(binding.scrutinee, bound, free);
      for (const c of binding.cases) {
        const patternBound = new Set(bound);
        collectPatternBound(c.pattern, patternBound);
        if (c.guard) collectFreeVars(c.guard, patternBound, free);
        collectFreeVars(c.body, patternBound, free);
      }
      break;
  }
};

const collectPatternBound = (pattern: IR.IRPattern, bound: Set<string>): void => {
  switch (pattern.kind) {
    case "IRPWild":
    case "IRPLit":
      break;
    case "IRPVar":
      bound.add(nameKey(pattern.name));
      break;
    case "IRPCon":
      for (const p of pattern.args) {
        collectPatternBound(p, bound);
      }
      break;
    case "IRPTuple":
      for (const p of pattern.elements) {
        collectPatternBound(p, bound);
      }
      break;
    case "IRPRecord":
      for (const f of pattern.fields) {
        collectPatternBound(f.pattern, bound);
      }
      break;
    case "IRPAs":
      bound.add(nameKey(pattern.name));
      collectPatternBound(pattern.pattern, bound);
      break;
  }
};

/** Check if a binding references a specific variable */
const bindingReferences = (binding: IR.IRBinding, varKey: string): boolean => {
  const free = new Set<string>();
  collectFreeVarsBinding(binding, new Set(), free);
  return free.has(varKey);
};

/**
 * Let floating outward: hoist bindings out of lambdas.
 * Transform: let f = λp -> let x = e in body  (where e doesn't use p)
 * Into: let x = e in let f = λp -> body
 */
const floatOutward = (expr: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBody = floatOutward(expr.body);

      // Check if binding is a lambda with hoistable inner lets
      if (expr.binding.kind === "IRBLambda") {
        const hoisted = tryHoistFromLambda(expr.binding);
        if (hoisted) {
          // Wrap the lambda binding with the hoisted lets
          let result: IR.IRExpr = IR.irlet(expr.name, hoisted.lambda, newBody);
          for (let i = hoisted.hoistedBindings.length - 1; i >= 0; i--) {
            const h = hoisted.hoistedBindings[i];
            if (h) {
              result = IR.irlet(h.name, h.binding, result);
            }
          }
          return floatOutward(result); // May expose more opportunities
        }
      }

      const newBinding = floatOutwardBinding(expr.binding);
      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: floatOutwardBinding(b.binding),
      }));
      return IR.irletrec(newBindings, floatOutward(expr.body));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? floatOutward(c.guard) : null,
        body: floatOutward(c.body),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const floatOutwardBinding = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, floatOutward(binding.body), binding.type);

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? floatOutward(c.guard) : null,
        body: floatOutward(c.body),
      }));
      return IR.irbmatch(binding.scrutinee, cases, binding.type);
    }
  }
};

type HoistResult = {
  hoistedBindings: { name: Name; binding: IR.IRBinding }[];
  lambda: IR.IRBLambda;
};

/**
 * Try to hoist let bindings out of a lambda.
 * Only hoists bindings that don't reference the lambda parameter.
 */
const tryHoistFromLambda = (lambda: IR.IRBLambda): HoistResult | null => {
  const paramKey = nameKey(lambda.param);
  const hoisted: { name: Name; binding: IR.IRBinding }[] = [];

  let currentBody = lambda.body;

  // Collect consecutive let bindings at the start of the lambda body
  // that don't reference the parameter
  while (currentBody.kind === "IRLet") {
    // Check if this binding references the lambda parameter
    if (bindingReferences(currentBody.binding, paramKey)) {
      break; // Can't hoist this or any following bindings
    }

    // Also check if this binding references any previously hoisted names
    // (those would be moved outside, so we can still reference them)
    hoisted.push({ name: currentBody.name, binding: currentBody.binding });
    currentBody = currentBody.body;
  }

  if (hoisted.length === 0) return null;

  // Create the new lambda with the remaining body
  const newLambda: IR.IRBLambda = {
    ...lambda,
    body: currentBody,
  };

  return { hoistedBindings: hoisted, lambda: newLambda };
};

/**
 * Let floating inward: sink bindings into branches where they're used.
 * Transform: let x = e in match y when A -> ...x... when B -> ... (no x)
 * Into: match y when A -> let x = e in ...x... when B -> ...
 */
const floatInward = (expr: IR.IRExpr): IR.IRExpr => {
  switch (expr.kind) {
    case "IRAtom":
      return expr;

    case "IRLet": {
      const newBody = floatInward(expr.body);

      // Check if body is a match where we can sink the binding
      if (newBody.kind === "IRMatch") {
        const sunk = trySinkIntoMatch(expr.name, expr.binding, newBody);
        if (sunk) {
          return floatInward(sunk); // May expose more opportunities
        }
      }

      const newBinding = floatInwardBinding(expr.binding);
      return IR.irlet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBindings = expr.bindings.map((b) => ({
        name: b.name,
        binding: floatInwardBinding(b.binding),
      }));
      return IR.irletrec(newBindings, floatInward(expr.body));
    }

    case "IRMatch": {
      const cases = expr.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? floatInward(c.guard) : null,
        body: floatInward(c.body),
      }));
      return IR.irmatch(expr.scrutinee, cases, expr.type);
    }
  }
};

const floatInwardBinding = (binding: IR.IRBinding): IR.IRBinding => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBApp":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
    case "IRBForeign":
      return binding;

    case "IRBLambda":
      return IR.irblambda(binding.param, floatInward(binding.body), binding.type);

    case "IRBMatch": {
      const cases = binding.cases.map((c) => ({
        pattern: c.pattern,
        guard: c.guard ? floatInward(c.guard) : null,
        body: floatInward(c.body),
      }));
      return IR.irbmatch(binding.scrutinee, cases, binding.type);
    }
  }
};

/**
 * Try to sink a binding into a match expression.
 * Only sinks if the binding is used in some but not all branches.
 */
const trySinkIntoMatch = (
  bindingName: Name,
  binding: IR.IRBinding,
  match: IR.IRMatch,
): IR.IRExpr | null => {
  const bindingKey = nameKey(bindingName);

  // Check which branches use the binding
  const branchUsage = match.cases.map((c) => {
    const fv = freeVars(c.body);
    const guardFv = c.guard ? freeVars(c.guard) : new Set<string>();
    return fv.has(bindingKey) || guardFv.has(bindingKey);
  });

  const usedCount = branchUsage.filter(Boolean).length;

  // Don't sink if:
  // - Used in all branches (no benefit)
  // - Not used anywhere (will be eliminated by DCE anyway)
  // - Binding is trivial (atom) - no benefit to sinking
  if (usedCount === 0 || usedCount === match.cases.length) return null;
  if (binding.kind === "IRBAtom") return null;

  // Sink the binding into branches where it's used
  const newCases = match.cases.map((c, i) => {
    if (branchUsage[i]) {
      return {
        pattern: c.pattern,
        guard: c.guard,
        body: IR.irlet(bindingName, binding, c.body),
      };
    }
    return c;
  });

  return IR.irmatch(match.scrutinee, newCases, match.type);
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

  // 3. Function inlining (inline small/single-use functions)
  result = inlineFunctions(result);

  // 4. Case-of-case transformation
  result = applyCaseOfCase(result);

  // 5. Let floating inward (sink bindings into branches)
  result = floatInward(result);

  // 6. Dead code elimination
  const uses = new Set<string>();
  collectUses(result, uses);
  result = removeUnused(result, uses);

  // 7. Tail call optimization
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
