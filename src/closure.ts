/**
 * Closure Conversion Pass
 *
 * Transforms IR with nested lambdas into IRProgram with flat top-level functions.
 * Each lambda is lifted to a top-level function with an explicit environment parameter
 * containing captured variables.
 *
 * This pass is required for backends that don't support native closures (Go, WASM, C).
 *
 * Algorithm:
 * 1. Traverse IR to find all lambda expressions
 * 2. Compute free variables for each lambda
 * 3. Lift lambdas to top-level functions with environment parameter
 * 4. Replace lambda expressions with IRClosureBinding
 *
 * Example transformation:
 *   let x = 1 in (y -> x + y)
 * becomes:
 *   function $fn_1($env, y) { return $env[0] + y }
 *   let x = 1 in { $fn: "$fn_1", $env: [x] }
 */

import type { Type } from "./checker";
import * as ir from "./ir";

// =============================================================================
// FREE VARIABLE ANALYSIS
// =============================================================================

/** Compute free variables in an IR expression */
const freeVarsExpr = (expr: ir.IRExpr, bound: Set<string>): Set<string> => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return freeVarsAtom(expr.atom, bound);

    case "IRLet": {
      const bindingFree = freeVarsBinding(expr.binding, bound);
      const newBound = new Set(bound);
      newBound.add(expr.name);
      const bodyFree = freeVarsExpr(expr.body, newBound);
      return union(bindingFree, bodyFree);
    }

    case "IRLetRec": {
      const newBound = new Set(bound);
      for (const { name } of expr.bindings) {
        newBound.add(name);
      }
      let free = new Set<string>();
      for (const { binding } of expr.bindings) {
        free = union(free, freeVarsBinding(binding, newBound));
      }
      free = union(free, freeVarsExpr(expr.body, newBound));
      return free;
    }
  }
};

/** Compute free variables in an atomic value */
const freeVarsAtom = (atom: ir.IRAtom, bound: Set<string>): Set<string> => {
  switch (atom.kind) {
    case "IRLit":
      return new Set();
    case "IRVar":
      return bound.has(atom.name) ? new Set() : new Set([atom.name]);
    case "IRForeignVar":
      // Foreign variables are provided by runtime, not free variables
      return new Set();
  }
};

/** Compute free variables in a binding */
const freeVarsBinding = (binding: ir.IRBinding, bound: Set<string>): Set<string> => {
  switch (binding.kind) {
    case "IRAtomBinding":
      return freeVarsAtom(binding.atom, bound);

    case "IRAppBinding":
      return union(freeVarsAtom(binding.func, bound), freeVarsAtom(binding.arg, bound));

    case "IRBinOpBinding":
      return union(freeVarsAtom(binding.left, bound), freeVarsAtom(binding.right, bound));

    case "IRIfBinding":
      return union(
        freeVarsAtom(binding.cond, bound),
        union(freeVarsExpr(binding.thenBranch, bound), freeVarsExpr(binding.elseBranch, bound)),
      );

    case "IRTupleBinding": {
      let free = new Set<string>();
      for (const elem of binding.elements) {
        free = union(free, freeVarsAtom(elem, bound));
      }
      return free;
    }

    case "IRRecordBinding": {
      let free = new Set<string>();
      for (const field of binding.fields) {
        free = union(free, freeVarsAtom(field.value, bound));
      }
      return free;
    }

    case "IRRecordUpdateBinding": {
      let free = freeVarsAtom(binding.base, bound);
      for (const field of binding.fields) {
        free = union(free, freeVarsAtom(field.value, bound));
      }
      return free;
    }

    case "IRFieldAccessBinding":
      return freeVarsAtom(binding.record, bound);

    case "IRTupleIndexBinding":
      return freeVarsAtom(binding.tuple, bound);

    case "IRMatchBinding": {
      let free = freeVarsAtom(binding.scrutinee, bound);
      for (const case_ of binding.cases) {
        const patternBound = patternBindings(case_.pattern);
        const caseBound = new Set([...bound, ...patternBound]);
        if (case_.guard) {
          free = union(free, freeVarsExpr(case_.guard, caseBound));
        }
        free = union(free, freeVarsExpr(case_.body, caseBound));
      }
      return free;
    }

    case "IRLambdaBinding": {
      const newBound = new Set(bound);
      newBound.add(binding.param);
      return freeVarsExpr(binding.body, newBound);
    }

    case "IRClosureBinding": {
      let free = new Set<string>();
      for (const capture of binding.captures) {
        free = union(free, freeVarsAtom(capture, bound));
      }
      return free;
    }
  }
};

/** Get variable names bound by a pattern */
const patternBindings = (pattern: ir.IRPattern): Set<string> => {
  switch (pattern.kind) {
    case "IRPVar":
      return new Set([pattern.name]);
    case "IRPWildcard":
    case "IRPLit":
      return new Set();
    case "IRPCon": {
      let vars = new Set<string>();
      for (const arg of pattern.args) {
        vars = union(vars, patternBindings(arg));
      }
      return vars;
    }
    case "IRPTuple": {
      let vars = new Set<string>();
      for (const elem of pattern.elements) {
        vars = union(vars, patternBindings(elem));
      }
      return vars;
    }
    case "IRPRecord": {
      let vars = new Set<string>();
      for (const field of pattern.fields) {
        vars = union(vars, patternBindings(field.pattern));
      }
      return vars;
    }
    case "IRPAs":
      return union(new Set([pattern.name]), patternBindings(pattern.pattern));
    case "IRPOr":
      // All alternatives bind the same variables
      return pattern.alternatives.length > 0
        ? patternBindings(pattern.alternatives[0]!)
        : new Set();
  }
};

/** Set union helper */
const union = <T>(a: Set<T>, b: Set<T>): Set<T> => new Set([...a, ...b]);

// =============================================================================
// CLOSURE CONVERSION CONTEXT
// =============================================================================

type ConvertContext = {
  /** Counter for generating unique function IDs */
  funcCounter: number;
  /** Accumulated lifted functions */
  functions: ir.IRFunction[];
  /** Set of constructor names (treated as global) */
  constructors: Set<string>;
};

const createContext = (constructorNames: readonly string[]): ConvertContext => ({
  funcCounter: 0,
  functions: [],
  constructors: new Set(constructorNames),
});

/** Generate a fresh function ID */
const freshFuncId = (ctx: ConvertContext): string => {
  const id = `$fn_${ctx.funcCounter}`;
  ctx.funcCounter++;
  return id;
};

// =============================================================================
// CLOSURE CONVERSION
// =============================================================================

/** Convert an expression, lifting lambdas */
const convertExpr = (ctx: ConvertContext, expr: ir.IRExpr): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr;

    case "IRLet":
      return ir.irLet(expr.name, convertBinding(ctx, expr.binding), convertExpr(ctx, expr.body));

    case "IRLetRec": {
      const bindings = expr.bindings.map(({ name, binding }) =>
        ir.irRecBinding(name, convertBinding(ctx, binding)),
      );
      return ir.irLetRec(bindings, convertExpr(ctx, expr.body));
    }
  }
};

/** Convert a binding, lifting lambdas to top-level functions */
const convertBinding = (ctx: ConvertContext, binding: ir.IRBinding): ir.IRBinding => {
  switch (binding.kind) {
    case "IRAtomBinding":
    case "IRClosureBinding":
      return binding;

    case "IRAppBinding":
      return binding;

    case "IRBinOpBinding":
      return binding;

    case "IRIfBinding":
      return ir.irIfBinding(
        binding.cond,
        convertExpr(ctx, binding.thenBranch),
        convertExpr(ctx, binding.elseBranch),
        binding.type,
      );

    case "IRTupleBinding":
    case "IRRecordBinding":
    case "IRRecordUpdateBinding":
    case "IRFieldAccessBinding":
    case "IRTupleIndexBinding":
      return binding;

    case "IRMatchBinding": {
      const cases = binding.cases.map((case_) => ({
        pattern: case_.pattern,
        guard: case_.guard ? convertExpr(ctx, case_.guard) : undefined,
        body: convertExpr(ctx, case_.body),
      }));
      return ir.irMatchBinding(binding.scrutinee, cases, binding.type);
    }

    case "IRLambdaBinding": {
      // Compute free variables (excluding constructors which are global)
      const bound = new Set<string>([binding.param]);
      const allFree = freeVarsExpr(binding.body, bound);
      const freeVars = [...allFree].filter((v) => !ctx.constructors.has(v));

      if (freeVars.length === 0) {
        // No free variables - just convert the body
        return ir.irLambdaBinding(
          binding.param,
          binding.paramType,
          convertExpr(ctx, binding.body),
          binding.type,
          binding.tailRecursive,
        );
      }

      // Lift to top-level function with environment
      const funcId = freshFuncId(ctx);
      const envParam = "$env";

      // Convert body, and it will reference $env for captured variables
      const convertedBody = convertExprWithEnv(ctx, binding.body, freeVars, envParam);

      // Get types of captured variables
      const captureTypes: Type[] = freeVars.map(() => ({ kind: "TVar", name: "_" }) as Type);

      // Get return type from function type
      const returnType =
        binding.type.kind === "TFun" ? binding.type.ret : ({ kind: "TVar", name: "_" } as Type);

      // Create lifted function
      const fn = ir.irFunction(
        funcId,
        envParam,
        binding.param,
        binding.paramType,
        convertedBody,
        captureTypes,
        returnType,
      );
      ctx.functions.push(fn);

      // Create closure binding that captures free variables
      const captures: ir.IRAtom[] = freeVars.map((name) =>
        ir.irVar(name, { kind: "TVar", name: "_" }),
      );

      return ir.irClosureBinding(funcId, captures, binding.type);
    }
  }
};

/** Convert expression, replacing free variable references with environment access */
const convertExprWithEnv = (
  ctx: ConvertContext,
  expr: ir.IRExpr,
  freeVars: string[],
  envParam: string,
): ir.IRExpr => {
  const envIndex = new Map(freeVars.map((v, i) => [v, i]));

  const substituteAtom = (atom: ir.IRAtom): ir.IRAtom => {
    if (atom.kind === "IRVar" && envIndex.has(atom.name)) {
      // This should be replaced with environment access
      // For now, we'll generate a special variable name that the backend understands
      return ir.irVar(`${envParam}[${envIndex.get(atom.name)}]`, atom.type);
    }
    return atom;
  };

  const substituteExpr = (e: ir.IRExpr): ir.IRExpr => {
    switch (e.kind) {
      case "IRAtomExpr":
        return ir.irAtomExpr(substituteAtom(e.atom));

      case "IRLet":
        return ir.irLet(e.name, substituteBinding(e.binding), substituteExpr(e.body));

      case "IRLetRec": {
        const bindings = e.bindings.map(({ name, binding }) =>
          ir.irRecBinding(name, substituteBinding(binding)),
        );
        return ir.irLetRec(bindings, substituteExpr(e.body));
      }
    }
  };

  const substituteBinding = (b: ir.IRBinding): ir.IRBinding => {
    switch (b.kind) {
      case "IRAtomBinding":
        return ir.irAtomBinding(substituteAtom(b.atom));

      case "IRAppBinding":
        return ir.irAppBinding(substituteAtom(b.func), substituteAtom(b.arg), b.type);

      case "IRBinOpBinding":
        return ir.irBinOpBinding(
          b.op,
          substituteAtom(b.left),
          substituteAtom(b.right),
          b.operandType,
          b.type,
        );

      case "IRIfBinding":
        return ir.irIfBinding(
          substituteAtom(b.cond),
          substituteExpr(b.thenBranch),
          substituteExpr(b.elseBranch),
          b.type,
        );

      case "IRTupleBinding":
        return ir.irTupleBinding(b.elements.map(substituteAtom), b.type);

      case "IRRecordBinding":
        return ir.irRecordBinding(
          b.fields.map((f) => ir.irRecordField(f.name, substituteAtom(f.value))),
          b.type,
        );

      case "IRRecordUpdateBinding":
        return ir.irRecordUpdateBinding(
          substituteAtom(b.base),
          b.fields.map((f) => ir.irRecordField(f.name, substituteAtom(f.value))),
          b.type,
        );

      case "IRFieldAccessBinding":
        return ir.irFieldAccessBinding(substituteAtom(b.record), b.field, b.type);

      case "IRTupleIndexBinding":
        return ir.irTupleIndexBinding(substituteAtom(b.tuple), b.index, b.type);

      case "IRMatchBinding":
        return ir.irMatchBinding(
          substituteAtom(b.scrutinee),
          b.cases.map((c) => ({
            pattern: c.pattern,
            guard: c.guard ? substituteExpr(c.guard) : undefined,
            body: substituteExpr(c.body),
          })),
          b.type,
        );

      case "IRLambdaBinding":
        // Nested lambda - recursively convert
        return convertBinding(ctx, b);

      case "IRClosureBinding":
        return ir.irClosureBinding(b.funcId, b.captures.map(substituteAtom), b.type);
    }
  };

  return substituteExpr(expr);
};

// =============================================================================
// MAIN ENTRY POINT
// =============================================================================

/**
 * Convert IR expression to IRProgram with lifted functions.
 *
 * @param expr The IR expression to convert
 * @param constructorNames Names of data constructors (treated as global)
 * @returns IRProgram with top-level functions and main expression
 */
export const closureConvert = (
  expr: ir.IRExpr,
  constructorNames: readonly string[],
): ir.IRProgram => {
  const ctx = createContext(constructorNames);
  const main = convertExpr(ctx, expr);
  return ir.irProgram(ctx.functions, main);
};
