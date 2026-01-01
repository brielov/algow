/**
 * Symbol Table for LSP Support
 *
 * Tracks definitions and references for:
 * - Go-to-definition: given a usage, find where it's defined
 * - Find references: given a definition, find all usages
 * - Rename: change a symbol name everywhere it appears
 * - Hover: show type at a location
 */

import type { Span } from "./ast";
import type { Type } from "./infer";

// =============================================================================
// SYMBOL TYPES
// =============================================================================

/**
 * A symbol definition represents where a name is introduced.
 * This includes:
 * - Let bindings (let x = ...)
 * - Lambda parameters (fn x => ...)
 * - Constructor names from data declarations
 * - Type names from data declarations
 * - Pattern bindings (match ... | Cons x xs => ...)
 */
export type Definition = {
  readonly name: string;
  readonly span: Span;
  readonly kind: DefinitionKind;
  readonly type: Type | null;
};

export type DefinitionKind =
  | "variable" // let binding or lambda parameter
  | "parameter" // lambda parameter or pattern binding
  | "constructor" // data constructor (Just, Nothing, Cons)
  | "type"; // type name from data declaration

/**
 * A symbol reference represents a usage of a defined name.
 */
export type Reference = {
  readonly name: string;
  readonly span: Span;
  readonly definition: Definition | null; // null if unresolved
};

/**
 * Symbol table storing all definitions and references in a program.
 */
export type SymbolTable = {
  readonly definitions: Definition[];
  readonly references: Reference[];
};

// =============================================================================
// SYMBOL TABLE BUILDER
// =============================================================================

/**
 * Mutable builder for constructing a symbol table during type inference.
 */
export type SymbolTableBuilder = {
  readonly definitions: Definition[];
  readonly references: Reference[];
  /** Maps name to stack of definitions (for scoping) */
  readonly scope: Map<string, Definition[]>;
};

/**
 * Create an empty symbol table builder.
 */
export const createSymbolTableBuilder = (): SymbolTableBuilder => ({
  definitions: [],
  references: [],
  scope: new Map(),
});

/**
 * Add a definition to the symbol table.
 */
export const addDefinition = (
  builder: SymbolTableBuilder,
  name: string,
  span: Span,
  kind: DefinitionKind,
  type: Type | null = null,
): Definition => {
  const def: Definition = { name, span, kind, type };
  builder.definitions.push(def);

  // Push onto scope stack
  const stack = builder.scope.get(name);
  if (stack) {
    stack.push(def);
  } else {
    builder.scope.set(name, [def]);
  }

  return def;
};

/**
 * Add a reference to the symbol table.
 * Resolves to the current definition in scope, if any.
 */
export const addReference = (builder: SymbolTableBuilder, name: string, span: Span): Reference => {
  // Find current definition in scope
  const stack = builder.scope.get(name);
  const definition = stack && stack.length > 0 ? stack[stack.length - 1]! : null;

  const ref: Reference = { name, span, definition };
  builder.references.push(ref);

  return ref;
};

/**
 * Pop a definition from the scope stack (when leaving a scope).
 */
export const popScope = (builder: SymbolTableBuilder, name: string): void => {
  const stack = builder.scope.get(name);
  if (stack && stack.length > 0) {
    stack.pop();
  }
};

/**
 * Finalize the symbol table builder into an immutable symbol table.
 */
export const finalize = (builder: SymbolTableBuilder): SymbolTable => ({
  definitions: builder.definitions,
  references: builder.references,
});

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
export const findReferences = (table: SymbolTable, definition: Definition): Reference[] => {
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
 * Find all references including the definition (for rename/highlight).
 */
export const findAllOccurrences = (
  table: SymbolTable,
  position: number,
): { definition: Definition | null; references: Reference[] } => {
  const definition = goToDefinition(table, position);
  if (!definition) {
    return { definition: null, references: [] };
  }
  return { definition, references: findReferences(table, definition) };
};
