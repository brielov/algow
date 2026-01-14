/**
 * Pattern Type Inference
 *
 * Type inference for patterns in match expressions.
 */

import type * as C from "../core";
import {
  type Type,
  type TypeEnv,
  type Subst,
  tInt,
  tFloat,
  tStr,
  tChar,
  tBool,
  tUnit,
  ttuple,
  trecord,
  freshTypeVar,
  instantiate,
  composeSubst,
  applySubst,
} from "../types";
import { type CheckContext, addError, recordType } from "./context";
import { unify } from "./unify";

// =============================================================================
// Pattern Inference
// =============================================================================

export type PatternBindings = Map<string, Type>;

export const inferPattern = (
  ctx: CheckContext,
  env: TypeEnv,
  expectedType: Type,
  pattern: C.CPattern,
): [Subst, PatternBindings] => {
  const bindings: PatternBindings = new Map();

  const infer = (expected: Type, pat: C.CPattern): Subst => {
    switch (pat.kind) {
      case "CPWild":
        return new Map();

      case "CPVar": {
        const key = `${pat.name.id}:${pat.name.text}`;
        bindings.set(key, expected);
        return new Map();
      }

      case "CPLit": {
        let litType: Type;
        switch (pat.value.kind) {
          case "int":
            litType = tInt;
            break;
          case "float":
            litType = tFloat;
            break;
          case "string":
            litType = tStr;
            break;
          case "char":
            litType = tChar;
            break;
          case "bool":
            litType = tBool;
            break;
        }
        return unify(ctx, expected, litType, pat.span);
      }

      case "CPCon": {
        // Look up constructor type in environment
        const conScheme = env.get(pat.name);
        if (!conScheme) {
          addError(ctx, `Unknown constructor: ${pat.name}`, pat.span);
          return new Map();
        }

        // Instantiate and decompose constructor type
        const conType = instantiate(conScheme);

        // Extract argument types and result type from the constructor function type
        const argTypes: Type[] = [];
        let resultType = conType;
        while (resultType.kind === "TFun") {
          argTypes.push(resultType.param);
          resultType = resultType.ret;
        }

        // Unify expected type with the result type of the constructor
        let subst = unify(ctx, expected, resultType, pat.span);

        // Check arity
        if (pat.args.length !== argTypes.length) {
          addError(
            ctx,
            `Constructor ${pat.name} expects ${argTypes.length} arguments but got ${pat.args.length}`,
            pat.span,
          );
          return subst;
        }

        // Infer patterns for each argument
        for (let i = 0; i < pat.args.length; i++) {
          const s = infer(applySubst(subst, argTypes[i]!), pat.args[i]!);
          subst = composeSubst(subst, s);
        }
        return subst;
      }

      case "CPTuple": {
        // Empty tuple pattern () matches unit
        if (pat.elements.length === 0) {
          return unify(ctx, expected, tUnit, pat.span);
        }
        if (expected.kind === "TTuple") {
          if (expected.elements.length !== pat.elements.length) {
            addError(ctx, `Tuple pattern arity mismatch`, pat.span);
            return new Map();
          }
          let subst: Subst = new Map();
          for (let i = 0; i < pat.elements.length; i++) {
            const s = infer(applySubst(subst, expected.elements[i]!), pat.elements[i]!);
            subst = composeSubst(subst, s);
          }
          return subst;
        }
        // Expected is a type variable - create tuple constraint
        const elemTypes = pat.elements.map(() => freshTypeVar());
        const tupleType = ttuple(elemTypes);
        let subst = unify(ctx, expected, tupleType, pat.span);
        for (let i = 0; i < pat.elements.length; i++) {
          const s = infer(applySubst(subst, elemTypes[i]!), pat.elements[i]!);
          subst = composeSubst(subst, s);
        }
        return subst;
      }

      case "CPRecord": {
        // Create field entries with fresh type variables paired with patterns
        const fieldEntries = pat.fields.map((f) => ({
          name: f.name,
          type: freshTypeVar(),
          pattern: f.pattern,
        }));
        const rowVar = freshTypeVar();
        const recordType = trecord(
          fieldEntries.map((e) => [e.name, e.type] as [string, Type]),
          rowVar,
        );

        // Unify with expected type
        let subst = unify(ctx, expected, recordType, pat.span);

        // Infer each field pattern (iterating over entries avoids lookup)
        for (const entry of fieldEntries) {
          const s = infer(applySubst(subst, entry.type), entry.pattern);
          subst = composeSubst(subst, s);
        }
        return subst;
      }

      case "CPAs": {
        const key = `${pat.name.id}:${pat.name.text}`;
        bindings.set(key, expected);
        return infer(expected, pat.pattern);
      }

      case "CPOr": {
        const s1 = infer(expected, pat.left);
        const s2 = infer(applySubst(s1, expected), pat.right);
        return composeSubst(s1, s2);
      }
    }
  };

  const subst = infer(expectedType, pattern);
  return [subst, bindings];
};

/** Record types for all variables in a pattern */
export const recordPatternTypes = (
  ctx: CheckContext,
  env: TypeEnv,
  subst: Subst,
  pattern: C.CPattern,
  scrutineeType: Type,
): void => {
  const recordInPattern = (pat: C.CPattern, expectedType: Type): void => {
    switch (pat.kind) {
      case "CPWild":
        break;

      case "CPVar":
        recordType(ctx, pat.name, applySubst(subst, expectedType));
        break;

      case "CPLit":
        break;

      case "CPCon": {
        // Get constructor type to find argument types
        const conScheme = env.get(pat.name);
        if (!conScheme) break;
        const conType = instantiate(conScheme);

        // Extract argument types
        const argTypes: Type[] = [];
        let resultType = conType;
        while (resultType.kind === "TFun") {
          argTypes.push(resultType.param);
          resultType = resultType.ret;
        }

        // Unify to get specialized argument types
        const conSubst = unify(ctx, expectedType, resultType);
        for (let i = 0; i < pat.args.length && i < argTypes.length; i++) {
          recordInPattern(pat.args[i]!, applySubst(conSubst, argTypes[i]!));
        }
        break;
      }

      case "CPTuple": {
        if (expectedType.kind === "TTuple") {
          for (let i = 0; i < pat.elements.length; i++) {
            const elemType = expectedType.elements[i];
            if (elemType) {
              recordInPattern(pat.elements[i]!, elemType);
            }
          }
        }
        break;
      }

      case "CPRecord": {
        if (expectedType.kind === "TRecord") {
          for (const f of pat.fields) {
            const fieldType = expectedType.fields.get(f.name);
            if (fieldType) {
              recordInPattern(f.pattern, fieldType);
            }
          }
        }
        break;
      }

      case "CPAs":
        recordType(ctx, pat.name, applySubst(subst, expectedType));
        recordInPattern(pat.pattern, expectedType);
        break;

      case "CPOr":
        // For or-patterns, all alternatives should bind the same variables
        // with the same types, so just process the left side
        recordInPattern(pat.left, expectedType);
        break;
    }
  };

  recordInPattern(pattern, scrutineeType);
};

// =============================================================================
// Or-Pattern Variable Binding Validation
// =============================================================================

/**
 * Collect all variable names bound by a pattern.
 */
const collectPatternVars = (pattern: C.CPattern): Set<string> => {
  const vars = new Set<string>();

  const collect = (p: C.CPattern): void => {
    switch (p.kind) {
      case "CPWild":
        break;
      case "CPVar":
        vars.add(p.name.text);
        break;
      case "CPLit":
        break;
      case "CPCon":
        for (const arg of p.args) {
          collect(arg);
        }
        break;
      case "CPTuple":
        for (const elem of p.elements) {
          collect(elem);
        }
        break;
      case "CPRecord":
        for (const field of p.fields) {
          collect(field.pattern);
        }
        break;
      case "CPAs":
        vars.add(p.name.text);
        collect(p.pattern);
        break;
      case "CPOr":
        // For or-patterns, collect from both sides (they should match)
        collect(p.left);
        collect(p.right);
        break;
    }
  };

  collect(pattern);
  return vars;
};

/**
 * Validate that an or-pattern binds the same variables in all alternatives.
 * Returns an error message if validation fails, null otherwise.
 */
export const validateOrPatternBindings = (ctx: CheckContext, pattern: C.CPattern): void => {
  const validate = (p: C.CPattern): void => {
    if (p.kind === "CPOr") {
      const leftVars = collectPatternVars(p.left);
      const rightVars = collectPatternVars(p.right);

      // Check that both sides bind the same variables
      const onlyLeft = [...leftVars].filter((v) => !rightVars.has(v));
      const onlyRight = [...rightVars].filter((v) => !leftVars.has(v));

      if (onlyLeft.length > 0 || onlyRight.length > 0) {
        const missing: string[] = [];
        if (onlyLeft.length > 0) {
          missing.push(`'${onlyLeft.join("', '")}' only in left alternative`);
        }
        if (onlyRight.length > 0) {
          missing.push(`'${onlyRight.join("', '")}' only in right alternative`);
        }
        addError(
          ctx,
          `Or-pattern alternatives must bind the same variables. ${missing.join("; ")}`,
          p.span,
        );
      }

      // Recursively validate nested patterns
      validate(p.left);
      validate(p.right);
    } else if (p.kind === "CPCon") {
      for (const arg of p.args) {
        validate(arg);
      }
    } else if (p.kind === "CPTuple") {
      for (const elem of p.elements) {
        validate(elem);
      }
    } else if (p.kind === "CPRecord") {
      for (const field of p.fields) {
        validate(field.pattern);
      }
    } else if (p.kind === "CPAs") {
      validate(p.pattern);
    }
  };

  validate(pattern);
};
