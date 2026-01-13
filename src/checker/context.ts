/**
 * Type Checker Context
 *
 * Contains the CheckContext type and helper functions for managing
 * diagnostics, type recording, and tuple projections.
 */

import type { Diagnostic } from "../diagnostics";
import { error as diagError } from "../diagnostics";
import type { SymbolTableBuilder } from "../lsp/symbols";
import { setDefinitionScheme } from "../lsp/symbols";
import type { NodeId, Span } from "../surface";
import type { Name } from "../core";
import {
  type Type,
  type Subst,
  type Scheme,
  type ConstructorRegistry,
  type AliasRegistry,
  ttuple,
  freshTypeVar,
  applySubst,
  composeSubst,
} from "../types";

// =============================================================================
// Check Context
// =============================================================================

export type CheckContext = {
  readonly diagnostics: Diagnostic[];
  readonly registry: ConstructorRegistry;
  readonly aliasRegistry: AliasRegistry;
  // Symbol table builder for recording types on definitions
  readonly symbolTableBuilder: SymbolTableBuilder;
  // Unified type map: NodeId → Type (for LSP/lowering)
  readonly nodeTypeMap: Map<NodeId, Type>;
  // Span position to NodeId mapping (for LSP position lookup)
  readonly spanToNodeId: Map<number, NodeId>;
  // Legacy map from Name.id to inferred type (for bindings - will be removed)
  readonly typeMap: Map<number, Type>;
  // Deferred tuple projections: TVar name -> (index -> element type)
  // Used to collect all tuple index accesses before creating the tuple type
  readonly tupleProjections: Map<string, Map<number, Type>>;
};

export const createContext = (
  symbolTableBuilder: SymbolTableBuilder,
  registry: ConstructorRegistry = new Map(),
  aliasRegistry: AliasRegistry = new Map(),
): CheckContext => ({
  diagnostics: [],
  registry,
  aliasRegistry,
  symbolTableBuilder,
  nodeTypeMap: new Map(),
  spanToNodeId: new Map(),
  typeMap: new Map(),
  tupleProjections: new Map(),
});

export const addError = (ctx: CheckContext, message: string, span?: Span): void => {
  const start = span?.start ?? 0;
  const end = span?.end ?? start;
  ctx.diagnostics.push(diagError(start, end, message));
};

/** Record type for a named binding (legacy - used by lower.ts) */
export const recordType = (ctx: CheckContext, name: Name, type: Type): void => {
  ctx.typeMap.set(name.id, type);
};

/** Record scheme for a named binding in both typeMap and symbol table */
export const recordScheme = (ctx: CheckContext, name: Name, s: Scheme): void => {
  ctx.typeMap.set(name.id, s.type);
  setDefinitionScheme(ctx.symbolTableBuilder, name.id, s);
};

/** Record the type of an expression by its nodeId and span position */
export const recordNodeType = (ctx: CheckContext, nodeId: NodeId, span: Span, type: Type): void => {
  ctx.nodeTypeMap.set(nodeId, type);
  ctx.spanToNodeId.set(span.start, nodeId);
};

/**
 * Record a tuple projection constraint: typeVar.index should have type elemType
 * Returns the element type (existing if already recorded, or fresh)
 */
export const recordTupleProjection = (
  ctx: CheckContext,
  typeVarName: string,
  index: number,
): Type => {
  let projections = ctx.tupleProjections.get(typeVarName);
  if (!projections) {
    projections = new Map();
    ctx.tupleProjections.set(typeVarName, projections);
  }
  let elemType = projections.get(index);
  if (!elemType) {
    elemType = freshTypeVar();
    projections.set(index, elemType);
  }
  return elemType;
};

/**
 * Resolve all pending tuple projections for a type variable into a proper tuple type.
 * Returns a substitution binding the type variable to a tuple.
 *
 * Note: This function needs access to unify, which creates a circular dependency.
 * The unify function is passed as a parameter to break the cycle.
 */
export const resolveTupleProjections = (
  ctx: CheckContext,
  typeVarName: string,
  subst: Subst,
  unify: (ctx: CheckContext, t1: Type, t2: Type, span?: Span) => Subst,
): Subst => {
  const projections = ctx.tupleProjections.get(typeVarName);
  if (!projections || projections.size === 0) {
    return subst;
  }

  // Find the maximum index to determine tuple size
  const maxIndex = Math.max(...projections.keys());
  const elements: Type[] = [];
  for (let i = 0; i <= maxIndex; i++) {
    const elemType = projections.get(i);
    // Apply current substitution to the element type
    elements.push(elemType ? applySubst(subst, elemType) : freshTypeVar());
  }

  const tupleType = ttuple(elements);

  // Check if the type variable is already bound in the substitution
  const existingType = subst.get(typeVarName);
  if (existingType) {
    // The TVar is already bound - unify the existing type with our tuple
    const s = unify(ctx, applySubst(subst, existingType), tupleType, undefined);
    return composeSubst(subst, s);
  }

  // Bind the type variable to the tuple
  const s = new Map([[typeVarName, tupleType]]);
  return composeSubst(subst, s);
};
