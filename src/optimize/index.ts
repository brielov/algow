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

import * as IR from "../ir";
import {
  type ExtendedEnv,
  type InlineEnv,
  nameKey,
  getExtendedValueFromBinding,
  tryEtaReduce,
} from "./types";
import { foldExpr, tryFoldBinOp } from "./fold";
import { collectUses, removeUnused } from "./dce";
import { transformTCO } from "./tco";
import { inlineExpr, inlineBinding, inlineFunctions } from "./inline";
import { applyCaseOfCase } from "./case-of-case";
import { floatInward } from "./float";

// =============================================================================
// Expression Optimization
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

// =============================================================================
// Binding Optimization
// =============================================================================

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

// =============================================================================
// Declaration Optimization
// =============================================================================

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

// =============================================================================
// Global Inlining
// =============================================================================

/**
 * Resolve transitive atom references in the inline environment.
 */
const resolveTransitiveInlines = (env: InlineEnv): void => {
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
 * Never inline the 'main' function as it's the program entry point.
 */
const collectGlobalInlines = (decls: readonly IR.IRDecl[]): InlineEnv => {
  const env: InlineEnv = new Map();
  for (const decl of decls) {
    if (decl.kind === "IRDeclLet" && decl.binding.kind === "IRBAtom") {
      if (decl.name.text === "main") continue;
      env.set(nameKey(decl.name), decl.binding.atom);
    }
  }
  resolveTransitiveInlines(env);
  return env;
};

/**
 * Collect extended values from declarations (for beta reduction).
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

// =============================================================================
// Main Optimization Function
// =============================================================================

/**
 * Optimize an IR program.
 */
export const optimize = (program: IR.IRProgram): IR.IRProgram => {
  let decls = program.decls;
  let main = program.main;

  // Iterate until fixpoint
  const MAX_ITERATIONS = 10;
  for (let i = 0; i < MAX_ITERATIONS; i++) {
    // Apply per-declaration optimizations
    decls = decls.map(optimizeDecl);

    // Collect global extended values for beta reduction
    const globalExtEnv = collectGlobalExtended(decls);

    // Optimize main expression with global environment
    main = main ? optimizeExpr(main, globalExtEnv) : null;

    // Collect and apply global inlining
    const globalEnv = collectGlobalInlines(decls);
    if (globalEnv.size === 0) break;

    const prevDeclCount = decls.length;
    decls = filterTrivialDecls(decls, globalEnv).map((d) => inlineDecl(d, globalEnv));
    if (main) {
      main = inlineExpr(main, globalEnv);
    }

    if (decls.length === prevDeclCount) break;
  }

  return { decls, main };
};
