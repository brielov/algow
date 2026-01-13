/**
 * Type Inference Engine implementing Algorithm W (Section 8)
 *
 * Works on Core AST with unique Name identifiers.
 */

import type { Diagnostic } from "../diagnostics";
import type { NodeId } from "../surface";
import type * as C from "../core";
import type { SymbolTableBuilder } from "../lsp/symbols";
import {
  type Type,
  type Subst,
  type TypeEnv,
  type Constraint,
  type ConstructorRegistry,
  tvar,
  tcon,
  tfun,
  tapp,
  ttuple,
  trecord,
  scheme,
  mono,
  freshTypeVar,
  applySubst,
  applySubstEnv,
  composeSubst,
  ftv,
  generalize,
  resetTypeVarCounter,
  instances,
  typeToString,
} from "../types";

// Re-export typeToString for use in compile.ts
export { typeToString } from "../types";

// Import from submodules
import { type CheckContext, createContext, addError, recordScheme } from "./context";
import { unify } from "./unify";
import { inferExpr, type InferResult } from "./infer";
import { type BindingInfo, buildDependencyGraph, findSCCs, isSelfRecursive } from "./scc";

// Re-export types that consumers need
export type { CheckContext } from "./context";
export type { InferResult } from "./infer";

// =============================================================================
// Type Conversion
// =============================================================================

const convertCoreType = (ctype: C.CType): Type => {
  switch (ctype.kind) {
    case "CTVar":
      return tvar(ctype.name);
    case "CTCon":
      return tcon(ctype.name);
    case "CTApp":
      return tapp(convertCoreType(ctype.func), convertCoreType(ctype.arg));
    case "CTFun":
      return tfun(convertCoreType(ctype.param), convertCoreType(ctype.result));
    case "CTTuple":
      return ttuple(ctype.elements.map(convertCoreType));
    case "CTRecord":
      return trecord(ctype.fields.map((f) => [f.name, convertCoreType(f.type)]));
  }
};

// =============================================================================
// Constraint Solving
// =============================================================================

const solveConstraints = (ctx: CheckContext, constraints: readonly Constraint[]): void => {
  for (const c of constraints) {
    const { className, type } = c;

    if (type.kind === "TVar") continue; // Defer

    if (type.kind === "TCon") {
      const classInstances = instances.get(className);
      if (!classInstances?.has(type.name)) {
        addError(ctx, `Type '${type.name}' does not satisfy ${className}`);
      }
      continue;
    }

    if (type.kind === "TTuple" && (className === "Eq" || className === "Ord")) {
      // Tuples satisfy Eq/Ord if all elements do
      continue;
    }

    addError(ctx, `Type '${typeToString(type)}' does not satisfy ${className}`);
  }
};

// =============================================================================
// Declaration Processing
// =============================================================================

export type CheckOutput = {
  readonly subst: Subst;
  readonly type: Type | null;
  readonly constraints: readonly Constraint[];
  readonly diagnostics: readonly Diagnostic[];
  readonly typeEnv: TypeEnv;
  readonly constructorRegistry: ConstructorRegistry;
  // Legacy: Map from Name.id to type (for bindings)
  readonly typeMap: ReadonlyMap<number, Type>;
  // Unified: Map from NodeId to instantiated type (for all expressions)
  readonly nodeTypeMap: ReadonlyMap<NodeId, Type>;
  // Maps span.start to NodeId for LSP position lookups
  readonly spanToNodeId: ReadonlyMap<number, NodeId>;
};

export const checkProgram = (
  program: C.CProgram,
  symbolTableBuilder: SymbolTableBuilder,
  initialEnv: TypeEnv = new Map(),
  initialRegistry: ConstructorRegistry = new Map(),
): CheckOutput => {
  resetTypeVarCounter();

  const ctx = createContext(symbolTableBuilder, initialRegistry);
  let env = new Map(initialEnv);
  let subst: Subst = new Map();
  const allConstraints: Constraint[] = [];

  // First pass: process all type declarations (register constructors)
  for (const decl of program.decls) {
    if (decl.kind === "CDeclType") {
      // Register constructors
      for (const con of decl.constructors) {
        // Build constructor type: field1 -> field2 -> ... -> ResultType
        let conType: Type = tcon(decl.name);
        for (const param of decl.params) {
          conType = tapp(conType, tvar(param));
        }
        for (let i = con.fields.length - 1; i >= 0; i--) {
          conType = tfun(convertCoreType(con.fields[i]!), conType);
        }
        const conScheme = scheme(decl.params, conType);
        env.set(con.name, conScheme);
      }
      // Register type in constructor registry
      ctx.registry.set(
        decl.name,
        decl.constructors.map((c) => c.name),
      );
    }
  }

  // Second pass: process foreign declarations first (they have explicit types)
  for (const decl of program.decls) {
    if (decl.kind === "CDeclForeign") {
      const type = convertCoreType(decl.type);
      const key = `${decl.name.id}:${decl.name.text}`;
      // Generalize foreign function types to allow polymorphic usage
      // e.g., `encode : a -> string` should instantiate fresh `a` each time
      const freeVars = [...ftv(type)];
      const foreignScheme = scheme(freeVars, type);
      env.set(key, foreignScheme);
      // Also register with module.name key for lookup
      env.set(`${decl.module}.${decl.jsName}`, foreignScheme);
      recordScheme(ctx, decl.name, foreignScheme);
    }
  }

  // Third pass: collect all let bindings for SCC analysis
  const allBindings: BindingInfo[] = [];
  const bindingByName = new Map<string, BindingInfo>();

  for (const decl of program.decls) {
    if (decl.kind === "CDeclLet") {
      const info: BindingInfo = { name: decl.name, value: decl.value, span: decl.span };
      allBindings.push(info);
      bindingByName.set(decl.name.text, info);
    } else if (decl.kind === "CDeclLetRec") {
      // Explicit let rec bindings are already known to be mutually recursive
      // We'll handle them separately to preserve their grouping
      for (const b of decl.bindings) {
        const info: BindingInfo = { name: b.name, value: b.value, span: decl.span };
        allBindings.push(info);
        bindingByName.set(b.name.text, info);
      }
    }
  }

  // Build dependency graph and find SCCs
  const depGraph = buildDependencyGraph(allBindings);
  const sccs = findSCCs(depGraph);

  // Process each SCC in topological order
  for (const scc of sccs) {
    if (scc.length === 1 && !isSelfRecursive(scc, depGraph)) {
      // Single non-recursive binding: infer and generalize immediately
      const binding = bindingByName.get(scc[0]!)!;
      const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), binding.value);
      subst = composeSubst(subst, s);
      allConstraints.push(...c);

      const key = `${binding.name.id}:${binding.name.text}`;
      const generalizedScheme = generalize(applySubstEnv(subst, env), t);
      env.set(key, generalizedScheme);
      recordScheme(ctx, binding.name, generalizedScheme);
    } else {
      // Mutually recursive bindings: treat as letrec
      const sccBindings = scc.map((name) => bindingByName.get(name)!);

      // Create fresh type variables for all bindings in the SCC
      const bindingTypes = new Map<string, Type>();
      for (const b of sccBindings) {
        const tv = freshTypeVar();
        const key = `${b.name.id}:${b.name.text}`;
        bindingTypes.set(key, tv);
        env.set(key, mono(tv));
      }

      // Infer types for all bindings
      for (const b of sccBindings) {
        const key = `${b.name.id}:${b.name.text}`;
        const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), b.value);
        subst = composeSubst(subst, s);
        allConstraints.push(...c);

        const expectedType = bindingTypes.get(key)!;
        const s2 = unify(ctx, applySubst(subst, expectedType), t, b.span);
        subst = composeSubst(subst, s2);
      }

      // Generalize: remove current bindings from env to allow proper generalization
      const outerEnv = applySubstEnv(subst, env);
      for (const b of sccBindings) {
        const key = `${b.name.id}:${b.name.text}`;
        outerEnv.delete(key);
      }
      for (const b of sccBindings) {
        const key = `${b.name.id}:${b.name.text}`;
        const t = applySubst(subst, bindingTypes.get(key)!);
        const generalizedScheme = generalize(outerEnv, t);
        env.set(key, generalizedScheme);
        recordScheme(ctx, b.name, generalizedScheme);
      }
    }
  }

  // Check main expression if present
  let resultType: Type | null = null;
  if (program.expr) {
    const [s, t, c] = inferExpr(ctx, applySubstEnv(subst, env), program.expr);
    subst = composeSubst(subst, s);
    allConstraints.push(...c);
    resultType = t;
  }

  // Solve constraints
  const finalConstraints = allConstraints.map((c) => ({
    className: c.className,
    type: applySubst(subst, c.type),
  }));
  solveConstraints(ctx, finalConstraints);

  return {
    subst,
    type: resultType ? applySubst(subst, resultType) : null,
    constraints: finalConstraints,
    diagnostics: ctx.diagnostics,
    typeEnv: env,
    constructorRegistry: ctx.registry,
    typeMap: ctx.typeMap,
    nodeTypeMap: ctx.nodeTypeMap,
    spanToNodeId: ctx.spanToNodeId,
  };
};

