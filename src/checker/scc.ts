/**
 * Dependency Analysis and Strongly Connected Components
 *
 * Tarjan's algorithm for finding SCCs, used to determine
 * binding groups for let-rec analysis.
 */

import type * as C from "../core";
import type { Name } from "../core";

// =============================================================================
// Binding Information
// =============================================================================

export type BindingInfo = {
  name: Name;
  value: C.CExpr;
  span?: C.CExpr["span"];
};

// =============================================================================
// Free Variable Collection
// =============================================================================

/**
 * Collect all free variables referenced in an expression.
 * Returns the set of binding names (original names) that are referenced.
 */
export const collectFreeVars = (expr: C.CExpr, bound: Set<string> = new Set()): Set<string> => {
  const freeVars = new Set<string>();

  const collect = (e: C.CExpr, localBound: Set<string>): void => {
    switch (e.kind) {
      case "CVar":
        if (!localBound.has(e.name.text)) {
          freeVars.add(e.name.text);
        }
        break;

      case "CLit":
        break;

      case "CApp":
        collect(e.func, localBound);
        collect(e.arg, localBound);
        break;

      case "CAbs": {
        const newBound = new Set(localBound);
        newBound.add(e.param.text);
        collect(e.body, newBound);
        break;
      }

      case "CLet": {
        collect(e.value, localBound);
        const newBound = new Set(localBound);
        newBound.add(e.name.text);
        collect(e.body, newBound);
        break;
      }

      case "CLetRec": {
        const newBound = new Set(localBound);
        for (const b of e.bindings) {
          newBound.add(b.name.text);
        }
        for (const b of e.bindings) {
          collect(b.value, newBound);
        }
        collect(e.body, newBound);
        break;
      }

      case "CMatch":
        collect(e.scrutinee, localBound);
        for (const c of e.cases) {
          const patBound = new Set(localBound);
          collectPatternBindings(c.pattern, patBound);
          if (c.guard) collect(c.guard, patBound);
          collect(c.body, patBound);
        }
        break;

      case "CCon":
        break;

      case "CTuple":
        for (const elem of e.elements) {
          collect(elem, localBound);
        }
        break;

      case "CRecord":
        for (const f of e.fields) {
          collect(f.value, localBound);
        }
        break;

      case "CRecordUpdate":
        collect(e.record, localBound);
        for (const f of e.fields) {
          collect(f.value, localBound);
        }
        break;

      case "CField":
        collect(e.record, localBound);
        break;

      case "CForeign":
        break;

      case "CBinOp":
        collect(e.left, localBound);
        collect(e.right, localBound);
        break;
    }
  };

  collect(expr, bound);
  return freeVars;
};

/** Collect variable names bound by a pattern into the given set */
export const collectPatternBindings = (pattern: C.CPattern, bound: Set<string>): void => {
  switch (pattern.kind) {
    case "CPWild":
    case "CPLit":
      break;
    case "CPVar":
      bound.add(pattern.name.text);
      break;
    case "CPCon":
      for (const arg of pattern.args) {
        collectPatternBindings(arg, bound);
      }
      break;
    case "CPTuple":
      for (const elem of pattern.elements) {
        collectPatternBindings(elem, bound);
      }
      break;
    case "CPRecord":
      for (const f of pattern.fields) {
        collectPatternBindings(f.pattern, bound);
      }
      break;
    case "CPAs":
      bound.add(pattern.name.text);
      collectPatternBindings(pattern.pattern, bound);
      break;
    case "CPOr":
      collectPatternBindings(pattern.left, bound);
      break;
  }
};

// =============================================================================
// Dependency Graph
// =============================================================================

/**
 * Build a dependency graph for a set of bindings.
 * Returns a map from binding name to the set of binding names it depends on.
 */
export const buildDependencyGraph = (
  bindings: readonly BindingInfo[],
): Map<string, Set<string>> => {
  const bindingNames = new Set(bindings.map((b) => b.name.text));
  const graph = new Map<string, Set<string>>();

  for (const binding of bindings) {
    const freeVars = collectFreeVars(binding.value);
    // Only include dependencies on other bindings in this group
    const deps = new Set([...freeVars].filter((v) => bindingNames.has(v)));
    graph.set(binding.name.text, deps);
  }

  return graph;
};

// =============================================================================
// Tarjan's SCC Algorithm
// =============================================================================

/**
 * Tarjan's algorithm for finding strongly connected components.
 * Returns SCCs in reverse topological order (dependencies come later).
 */
export const findSCCs = (graph: Map<string, Set<string>>): string[][] => {
  const index = new Map<string, number>();
  const lowlink = new Map<string, number>();
  const onStack = new Set<string>();
  const stack: string[] = [];
  const sccs: string[][] = [];
  let indexCounter = 0;

  const strongconnect = (v: string): void => {
    index.set(v, indexCounter);
    lowlink.set(v, indexCounter);
    indexCounter++;
    stack.push(v);
    onStack.add(v);

    const successors = graph.get(v) ?? new Set();
    for (const w of successors) {
      if (!index.has(w)) {
        strongconnect(w);
        lowlink.set(v, Math.min(lowlink.get(v)!, lowlink.get(w)!));
      } else if (onStack.has(w)) {
        lowlink.set(v, Math.min(lowlink.get(v)!, index.get(w)!));
      }
    }

    if (lowlink.get(v) === index.get(v)) {
      const scc: string[] = [];
      let w: string;
      do {
        w = stack.pop()!;
        onStack.delete(w);
        scc.push(w);
      } while (w !== v);
      sccs.push(scc);
    }
  };

  for (const v of graph.keys()) {
    if (!index.has(v)) {
      strongconnect(v);
    }
  }

  // Tarjan's algorithm produces SCCs in topological order:
  // - When A depends on B, we recurse to B first
  // - B's SCC is formed before A's SCC
  // - So dependencies come before dependents in the output
  return sccs;
};

/**
 * Check if an SCC is self-recursive (single binding that references itself)
 */
export const isSelfRecursive = (scc: string[], graph: Map<string, Set<string>>): boolean => {
  if (scc.length !== 1) return false;
  const name = scc[0]!;
  const deps = graph.get(name) ?? new Set();
  return deps.has(name);
};
