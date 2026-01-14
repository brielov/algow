/**
 * Effect Analysis
 *
 * Analyzes which functions need to be async based on their transitive calls
 * to async foreign functions. This enables generating synchronous code for
 * pure functions that don't use IO.
 *
 * Algorithm:
 * 1. Build a call graph from the IR program
 * 2. Mark functions that directly call async foreign functions
 * 3. Propagate async-ness to callers until fixpoint
 */

import type * as IR from "./ir";
import type { Name } from "./core";

// =============================================================================
// Effect Analysis Result
// =============================================================================

export type EffectAnalysis = {
  /** Set of function IDs (by Name.id) that are async */
  readonly asyncFunctions: Set<number>;
  /** Set of foreign functions used (module.name format) */
  readonly usedForeign: Set<string>;
};

// =============================================================================
// Call Graph Building
// =============================================================================

type CallGraph = {
  /** Maps function id to set of function ids it calls */
  calls: Map<number, Set<number>>;
  /** Maps function id to whether it directly calls async foreign */
  directAsync: Set<number>;
  /** Set of all foreign functions used */
  usedForeign: Set<string>;
};

const createCallGraph = (): CallGraph => ({
  calls: new Map(),
  directAsync: new Set(),
  usedForeign: new Set(),
});

/** Get or create the call set for a function */
const getCallSet = (graph: CallGraph, funcId: number): Set<number> => {
  let calls = graph.calls.get(funcId);
  if (!calls) {
    calls = new Set();
    graph.calls.set(funcId, calls);
  }
  return calls;
};

/** Context for tracking current function during analysis */
type AnalysisContext = {
  graph: CallGraph;
  /** Current function being analyzed (null if at top level) */
  currentFunc: number | null;
};

// =============================================================================
// IR Traversal
// =============================================================================

const analyzeAtom = (ctx: AnalysisContext, atom: IR.Atom): void => {
  if (atom.kind === "AVar" && ctx.currentFunc !== null) {
    // Record that current function references this variable
    // (could be a function call target)
    getCallSet(ctx.graph, ctx.currentFunc).add(atom.name.id);
  }
};

const analyzeBinding = (ctx: AnalysisContext, binding: IR.IRBinding, boundName?: number): void => {
  switch (binding.kind) {
    case "IRBAtom":
      analyzeAtom(ctx, binding.atom);
      // If we're binding a variable to another variable, create an edge
      // so async-ness propagates through assignments like `main = asyncFn`
      if (binding.atom.kind === "AVar" && boundName !== undefined) {
        getCallSet(ctx.graph, boundName).add(binding.atom.name.id);
      }
      break;

    case "IRBApp":
      analyzeAtom(ctx, binding.func);
      analyzeAtom(ctx, binding.arg);
      // Record the function call
      if (binding.func.kind === "AVar" && ctx.currentFunc !== null) {
        getCallSet(ctx.graph, ctx.currentFunc).add(binding.func.name.id);
      }
      break;

    case "IRBBinOp":
      analyzeAtom(ctx, binding.left);
      analyzeAtom(ctx, binding.right);
      break;

    case "IRBTuple":
      for (const elem of binding.elements) {
        analyzeAtom(ctx, elem);
      }
      break;

    case "IRBRecord":
      for (const field of binding.fields) {
        analyzeAtom(ctx, field.value);
      }
      break;

    case "IRBRecordUpdate":
      analyzeAtom(ctx, binding.record);
      for (const field of binding.fields) {
        analyzeAtom(ctx, field.value);
      }
      break;

    case "IRBField":
      analyzeAtom(ctx, binding.record);
      break;

    case "IRBLambda": {
      // Lambda introduces a new function scope
      // Use boundName if provided (from declaration), otherwise use param.id
      const lambdaId = boundName ?? binding.param.id;
      const savedFunc = ctx.currentFunc;
      ctx.currentFunc = lambdaId;
      analyzeExpr(ctx, binding.body);
      ctx.currentFunc = savedFunc;
      break;
    }

    case "IRBForeign": {
      // Track foreign function usage
      const foreignName = `$${binding.module}_${binding.name}`;
      ctx.graph.usedForeign.add(foreignName);

      // If async, mark current function as directly async
      if (binding.isAsync && ctx.currentFunc !== null) {
        ctx.graph.directAsync.add(ctx.currentFunc);
      }

      for (const arg of binding.args) {
        analyzeAtom(ctx, arg);
      }
      break;
    }

    case "IRBMatch":
      analyzeAtom(ctx, binding.scrutinee);
      for (const case_ of binding.cases) {
        if (case_.guard) analyzeExpr(ctx, case_.guard);
        analyzeExpr(ctx, case_.body);
      }
      break;
  }
};

const analyzeExpr = (ctx: AnalysisContext, expr: IR.IRExpr): void => {
  switch (expr.kind) {
    case "IRAtom":
      analyzeAtom(ctx, expr.atom);
      break;

    case "IRLet":
      analyzeBinding(ctx, expr.binding);
      analyzeExpr(ctx, expr.body);
      break;

    case "IRLetRec":
      for (const { binding } of expr.bindings) {
        analyzeBinding(ctx, binding);
      }
      analyzeExpr(ctx, expr.body);
      break;

    case "IRMatch":
      analyzeAtom(ctx, expr.scrutinee);
      for (const case_ of expr.cases) {
        if (case_.guard) analyzeExpr(ctx, case_.guard);
        analyzeExpr(ctx, case_.body);
      }
      break;
  }
};

const analyzeDecl = (ctx: AnalysisContext, decl: IR.IRDecl): void => {
  switch (decl.kind) {
    case "IRDeclType":
      // Type declarations don't have runtime effects
      break;

    case "IRDeclLet": {
      // For let bindings with lambda bodies, the function id is the binding name
      if (decl.binding.kind === "IRBLambda") {
        ctx.currentFunc = decl.name.id;
        analyzeBinding(ctx, decl.binding, decl.name.id);
        ctx.currentFunc = null;
      } else if (decl.binding.kind === "IRBForeign" && decl.binding.isAsync) {
        // Async foreign function - mark this declaration as async
        ctx.graph.directAsync.add(decl.name.id);
        ctx.graph.usedForeign.add(`$${decl.binding.module}_${decl.binding.name}`);
      } else {
        analyzeBinding(ctx, decl.binding, decl.name.id);
      }
      break;
    }

    case "IRDeclLetRec": {
      for (const { name, binding } of decl.bindings) {
        if (binding.kind === "IRBLambda") {
          ctx.currentFunc = name.id;
          analyzeBinding(ctx, binding, name.id);
          ctx.currentFunc = null;
        } else if (binding.kind === "IRBForeign" && binding.isAsync) {
          // Async foreign function - mark this declaration as async
          ctx.graph.directAsync.add(name.id);
          ctx.graph.usedForeign.add(`$${binding.module}_${binding.name}`);
        } else {
          analyzeBinding(ctx, binding, name.id);
        }
      }
      break;
    }
  }
};

// =============================================================================
// Async Propagation
// =============================================================================

/**
 * Propagate async-ness through the call graph to fixpoint.
 * If function A calls function B and B is async, then A must be async too.
 */
const propagateAsync = (graph: CallGraph): Set<number> => {
  const asyncFuncs = new Set(graph.directAsync);

  // Iterate until no changes
  let changed = true;
  while (changed) {
    changed = false;
    for (const [funcId, callees] of graph.calls) {
      if (asyncFuncs.has(funcId)) continue; // Already async

      // Check if any callee is async
      for (const calleeId of callees) {
        if (asyncFuncs.has(calleeId)) {
          asyncFuncs.add(funcId);
          changed = true;
          break;
        }
      }
    }
  }

  return asyncFuncs;
};

// =============================================================================
// Main Analysis Function
// =============================================================================

/**
 * Analyze an IR program for async effects and foreign function usage.
 */
export const analyzeEffects = (program: IR.IRProgram): EffectAnalysis => {
  const graph = createCallGraph();
  const ctx: AnalysisContext = {
    graph,
    currentFunc: null,
  };

  // Analyze all declarations
  for (const decl of program.decls) {
    analyzeDecl(ctx, decl);
  }

  // Analyze main expression if present
  if (program.main) {
    analyzeExpr(ctx, program.main);
  }

  // Propagate async to fixpoint
  const asyncFunctions = propagateAsync(graph);

  return {
    asyncFunctions,
    usedForeign: graph.usedForeign,
  };
};

/**
 * Check if a specific function is async.
 */
export const isFunctionAsync = (analysis: EffectAnalysis, name: Name): boolean => {
  return analysis.asyncFunctions.has(name.id);
};

/**
 * Check if the main function needs to be async.
 */
export const isMainAsync = (program: IR.IRProgram, analysis: EffectAnalysis): boolean => {
  // Find main function
  for (const decl of program.decls) {
    if (decl.kind === "IRDeclLet" && decl.name.text === "main") {
      return analysis.asyncFunctions.has(decl.name.id);
    }
    if (decl.kind === "IRDeclLetRec") {
      for (const b of decl.bindings) {
        if (b.name.text === "main") {
          return analysis.asyncFunctions.has(b.name.id);
        }
      }
    }
  }
  return false;
};
