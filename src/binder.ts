/**
 * Name Resolution (Binding Phase)
 *
 * Walks the AST to build a symbol table before type checking.
 * Resolves all variable references to their definitions.
 * Reports "unbound variable" errors.
 *
 * This phase produces an immutable SymbolTable that the checker
 * uses to associate types with definitions.
 */

import type * as ast from "./ast";
import { findSimilarNames, unboundVariable, type Diagnostic } from "./diagnostics";

// =============================================================================
// SYMBOL TYPES
// =============================================================================

/**
 * A symbol definition represents where a name is introduced.
 * Note: No type field - types are tracked separately by the checker.
 */
export type Definition = {
  readonly name: string;
  readonly span: ast.Span;
  readonly kind: DefinitionKind;
};

export type DefinitionKind =
  | "variable" // let binding
  | "parameter" // lambda parameter or pattern binding
  | "constructor" // data constructor (Just, Nothing, Cons)
  | "type"; // type name from data declaration

/**
 * A symbol reference represents a usage of a defined name.
 */
export type Reference = {
  readonly name: string;
  readonly span: ast.Span;
  readonly definition: Definition | null; // null if unresolved
};

/**
 * Immutable symbol table storing all definitions and references.
 */
export type SymbolTable = {
  readonly definitions: readonly Definition[];
  readonly references: readonly Reference[];
};

/**
 * Result of the binding phase.
 */
export type BindResult = {
  readonly symbols: SymbolTable;
  readonly diagnostics: readonly Diagnostic[];
};

// =============================================================================
// BINDING CONTEXT (internal, mutable during binding)
// =============================================================================

type BindContext = {
  readonly definitions: Definition[];
  readonly references: Reference[];
  readonly diagnostics: Diagnostic[];
  /** Maps name to stack of definitions (for scoping) */
  readonly scope: Map<string, Definition[]>;
};

const createContext = (): BindContext => ({
  definitions: [],
  references: [],
  diagnostics: [],
  scope: new Map(),
});

const addDefinition = (
  ctx: BindContext,
  name: string,
  span: ast.Span,
  kind: DefinitionKind,
): Definition => {
  const def: Definition = { name, span, kind };
  ctx.definitions.push(def);

  // Push onto scope stack
  const stack = ctx.scope.get(name);
  if (stack) {
    stack.push(def);
  } else {
    ctx.scope.set(name, [def]);
  }

  return def;
};

const addReference = (ctx: BindContext, name: string, span: ast.Span): Reference => {
  // Find current definition in scope
  const stack = ctx.scope.get(name);
  const definition = stack && stack.length > 0 ? stack[stack.length - 1]! : null;

  const ref: Reference = { name, span, definition };
  ctx.references.push(ref);

  // Report unbound variable error with suggestions
  if (!definition) {
    const suggestions = findSimilarNames(name, ctx.scope.keys());
    ctx.diagnostics.push(
      unboundVariable(span.start, span.end, name, suggestions.length > 0 ? suggestions : undefined),
    );
  }

  return ref;
};

const popScope = (ctx: BindContext, name: string): void => {
  const stack = ctx.scope.get(name);
  if (stack && stack.length > 0) {
    stack.pop();
  }
};

const finalize = (ctx: BindContext): SymbolTable => ({
  definitions: ctx.definitions,
  references: ctx.references,
});

// =============================================================================
// MAIN BIND FUNCTION
// =============================================================================

/**
 * Bind an expression - resolve all names and build symbol table.
 */
export const bind = (expr: ast.Expr): BindResult => {
  const ctx = createContext();
  bindExpr(ctx, expr);
  return {
    symbols: finalize(ctx),
    diagnostics: ctx.diagnostics,
  };
};

/**
 * Bind with constructor names pre-populated in scope.
 * Used when data declarations provide constructors.
 */
export const bindWithConstructors = (
  constructorNames: readonly string[],
  expr: ast.Expr,
): BindResult => {
  const ctx = createContext();

  // Add constructors to scope (they don't have spans, use dummy span)
  for (const name of constructorNames) {
    addDefinition(ctx, name, { start: 0, end: 0 }, "constructor");
  }

  bindExpr(ctx, expr);
  return {
    symbols: finalize(ctx),
    diagnostics: ctx.diagnostics,
  };
};

// =============================================================================
// EXPRESSION BINDING
// =============================================================================

const bindExpr = (ctx: BindContext, expr: ast.Expr): void => {
  switch (expr.kind) {
    case "Num":
    case "Bool":
    case "Str":
      // Literals have no bindings
      break;

    case "Var":
      bindVar(ctx, expr);
      break;

    case "Let":
      bindLet(ctx, expr);
      break;

    case "LetRec":
      bindLetRec(ctx, expr);
      break;

    case "Abs":
      bindAbs(ctx, expr);
      break;

    case "App":
      bindExpr(ctx, expr.func);
      bindExpr(ctx, expr.param);
      break;

    case "If":
      bindExpr(ctx, expr.cond);
      bindExpr(ctx, expr.then);
      bindExpr(ctx, expr.else);
      break;

    case "BinOp":
      bindExpr(ctx, expr.left);
      bindExpr(ctx, expr.right);
      break;

    case "Tuple":
      for (const element of expr.elements) {
        bindExpr(ctx, element);
      }
      break;

    case "Record":
      for (const field of expr.fields) {
        bindExpr(ctx, field.value);
      }
      break;

    case "FieldAccess":
      bindExpr(ctx, expr.record);
      break;

    case "TupleIndex":
      bindExpr(ctx, expr.tuple);
      break;

    case "QualifiedVar":
      // Qualified variables (Module.name) are resolved by the type checker
      // No local scope resolution needed
      break;

    case "Match":
      bindMatch(ctx, expr);
      break;
  }
};

const bindVar = (ctx: BindContext, expr: ast.Var): void => {
  // Use span or a dummy span for error reporting
  const span = expr.span ?? { start: 0, end: 0 };
  addReference(ctx, expr.name, span);
};

const bindLet = (ctx: BindContext, expr: ast.Let): void => {
  // First bind the value (name not yet in scope)
  bindExpr(ctx, expr.value);

  // Add definition (use dummy span if missing)
  const nameSpan = expr.nameSpan ?? expr.span ?? { start: 0, end: 0 };
  addDefinition(ctx, expr.name, nameSpan, "variable");

  // Bind body with name in scope
  bindExpr(ctx, expr.body);

  // Pop scope
  popScope(ctx, expr.name);
};

const bindLetRec = (ctx: BindContext, expr: ast.LetRec): void => {
  // For recursive/mutually recursive bindings, add ALL definitions BEFORE binding any values
  for (const binding of expr.bindings) {
    const nameSpan = binding.nameSpan ?? expr.span ?? { start: 0, end: 0 };
    addDefinition(ctx, binding.name, nameSpan, "variable");
  }

  // Bind all values (all names are in scope for mutual recursion)
  for (const binding of expr.bindings) {
    bindExpr(ctx, binding.value);
  }

  // Bind body
  bindExpr(ctx, expr.body);

  // Pop all scopes (in reverse order)
  for (let i = expr.bindings.length - 1; i >= 0; i--) {
    popScope(ctx, expr.bindings[i]!.name);
  }
};

const bindAbs = (ctx: BindContext, expr: ast.Abs): void => {
  // Add parameter definition (use dummy span if missing)
  const paramSpan = expr.paramSpan ?? expr.span ?? { start: 0, end: 0 };
  addDefinition(ctx, expr.param, paramSpan, "parameter");

  // Bind body with parameter in scope
  bindExpr(ctx, expr.body);

  // Pop scope
  popScope(ctx, expr.param);
};

const bindMatch = (ctx: BindContext, expr: ast.Match): void => {
  // Bind the scrutinee
  bindExpr(ctx, expr.expr);

  // Bind each case
  for (const case_ of expr.cases) {
    // Collect pattern bindings
    const bindings = bindPattern(ctx, case_.pattern);

    // Bind case body with pattern bindings in scope
    bindExpr(ctx, case_.body);

    // Pop all pattern bindings
    for (const name of bindings) {
      popScope(ctx, name);
    }
  }
};

// =============================================================================
// PATTERN BINDING
// =============================================================================

/**
 * Bind a pattern and return the names of variables bound.
 * These need to be popped from scope after the case body.
 */
const bindPattern = (ctx: BindContext, pattern: ast.Pattern): string[] => {
  switch (pattern.kind) {
    case "PVar": {
      // Always add to scope (use dummy span if missing)
      const span = pattern.span ?? { start: 0, end: 0 };
      addDefinition(ctx, pattern.name, span, "parameter");
      return [pattern.name];
    }

    case "PWildcard":
      return [];

    case "PCon": {
      // Constructor is a reference - use precise nameSpan if available
      const conSpan =
        pattern.nameSpan ??
        (pattern.span
          ? {
              start: pattern.span.start,
              end: pattern.span.start + pattern.name.length,
            }
          : null);
      if (conSpan) {
        addReference(ctx, pattern.name, conSpan);
      }

      // Bind nested patterns
      const bindings: string[] = [];
      for (const arg of pattern.args) {
        bindings.push(...bindPattern(ctx, arg));
      }
      return bindings;
    }

    case "QualifiedPCon": {
      // Qualified constructor patterns (Module.Constructor) are resolved by type checker
      // Just bind the nested pattern arguments
      const bindings: string[] = [];
      for (const arg of pattern.args) {
        bindings.push(...bindPattern(ctx, arg));
      }
      return bindings;
    }

    case "PLit":
    case "PChar":
      return [];

    case "PTuple": {
      const bindings: string[] = [];
      for (const element of pattern.elements) {
        bindings.push(...bindPattern(ctx, element));
      }
      return bindings;
    }

    case "PRecord": {
      const bindings: string[] = [];
      for (const field of pattern.fields) {
        bindings.push(...bindPattern(ctx, field.pattern));
      }
      return bindings;
    }

    case "PAs": {
      // Bind the as-name and inner pattern
      const span = pattern.nameSpan ?? pattern.span ?? { start: 0, end: 0 };
      addDefinition(ctx, pattern.name, span, "parameter");
      const innerBindings = bindPattern(ctx, pattern.pattern);
      return [pattern.name, ...innerBindings];
    }

    case "POr": {
      // Or-patterns: all alternatives bind the same variables (validated by type checker)
      // Just bind the first alternative since they all have the same bindings
      if (pattern.alternatives.length > 0) {
        return bindPattern(ctx, pattern.alternatives[0]!);
      }
      return [];
    }
  }
};

// =============================================================================
// SYMBOL TABLE QUERIES
// =============================================================================

/**
 * Find a definition at a given position.
 */
export const findDefinitionAt = (table: SymbolTable, position: number): Definition | null => {
  for (const def of table.definitions) {
    if (position >= def.span.start && position < def.span.end) {
      return def;
    }
  }
  return null;
};

/**
 * Find a reference at a given position.
 */
export const findReferenceAt = (table: SymbolTable, position: number): Reference | null => {
  for (const ref of table.references) {
    if (position >= ref.span.start && position < ref.span.end) {
      return ref;
    }
  }
  return null;
};

/**
 * Find all references to a definition.
 */
export const findReferences = (
  table: SymbolTable,
  definition: Definition,
): readonly Reference[] => {
  return table.references.filter((ref) => ref.definition === definition);
};

/**
 * Find the definition for a symbol at a position (go-to-definition).
 * Works whether the position is at a reference or definition.
 */
export const goToDefinition = (table: SymbolTable, position: number): Definition | null => {
  // Check if position is on a reference
  const ref = findReferenceAt(table, position);
  if (ref) {
    return ref.definition;
  }

  // Check if position is already on a definition
  const def = findDefinitionAt(table, position);
  if (def) {
    return def;
  }

  return null;
};

/**
 * Find all occurrences including the definition (for rename/highlight).
 */
export const findAllOccurrences = (
  table: SymbolTable,
  position: number,
): { definition: Definition | null; references: readonly Reference[] } => {
  const definition = goToDefinition(table, position);
  if (!definition) {
    return { definition: null, references: [] };
  }
  return { definition, references: findReferences(table, definition) };
};
