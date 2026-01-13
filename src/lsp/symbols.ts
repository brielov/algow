/**
 * LSP Symbol Table Types
 *
 * Core data structures for tracking symbol definitions, references,
 * and scope information for LSP features.
 */

import type { FileId, Span } from "../surface";
import { applySubst, type Scheme, type Subst, type Type } from "../types";

// Re-export for convenience
export type { FileId, Span } from "../surface";

// =============================================================================
// Location Types
// =============================================================================

/**
 * Location in source code.
 * Note: With file-aware spans, Location is essentially just Span
 * (since Span now includes fileId). We keep Location for API compatibility.
 */
export type Location = Span;

// =============================================================================
// Symbol Types
// =============================================================================

/** Kind of symbol for categorization */
export type SymbolKind =
  | "function"
  | "variable"
  | "parameter"
  | "pattern-binding"
  | "type"
  | "constructor"
  | "module"
  | "foreign";

/** Information about a symbol definition */
export type SymbolDefinition = {
  readonly nameId: number;
  readonly name: string;
  readonly kind: SymbolKind;
  readonly location: Location;
  readonly scheme?: Scheme;
};

/** A reference to a symbol */
export type SymbolReference = {
  readonly targetId: number;
  readonly location: Location;
};

// =============================================================================
// Constructor and Type Symbols
// =============================================================================

/** Constructor symbol information */
export type ConstructorSymbol = {
  readonly name: string;
  readonly qualifiedName: string;
  readonly typeName: string;
  readonly tag: number;
  readonly arity: number;
  readonly location?: Location;
};

/** Type symbol information */
export type TypeSymbol = {
  readonly name: string;
  readonly params: readonly string[];
  readonly constructors: readonly string[];
  readonly location?: Location;
};

// =============================================================================
// Scope Tracking for Autocomplete
// =============================================================================

/** Snapshot of scope at a position */
export type ScopeSnapshot = {
  readonly offset: number;
  readonly bindings: ReadonlyMap<string, number>; // name → nameId
  readonly constructors: ReadonlySet<string>;
};

// =============================================================================
// Symbol Table
// =============================================================================

/** Main symbol table structure */
export type SymbolTable = {
  /** All symbol definitions indexed by Name.id */
  readonly definitions: ReadonlyMap<number, SymbolDefinition>;

  /** All references indexed by Name.id of target */
  readonly references: ReadonlyMap<number, readonly SymbolReference[]>;

  /** Constructor symbols */
  readonly constructors: ReadonlyMap<string, ConstructorSymbol>;

  /** Type symbols */
  readonly types: ReadonlyMap<string, TypeSymbol>;

  /** Scope snapshots for autocomplete */
  readonly scopeSnapshots: readonly ScopeSnapshot[];
};

// =============================================================================
// Builder Types (mutable versions for construction)
// =============================================================================

/** Mutable builder for constructing SymbolTable */
export type SymbolTableBuilder = {
  definitions: Map<number, SymbolDefinition>;
  references: Map<number, SymbolReference[]>;
  constructors: Map<string, ConstructorSymbol>;
  types: Map<string, TypeSymbol>;
  scopeSnapshots: ScopeSnapshot[];
};

/** Create an empty symbol table builder */
export const createSymbolTableBuilder = (): SymbolTableBuilder => ({
  definitions: new Map(),
  references: new Map(),
  constructors: new Map(),
  types: new Map(),
  scopeSnapshots: [],
});

/** Add a definition to the builder */
export const addDefinition = (builder: SymbolTableBuilder, def: SymbolDefinition): void => {
  builder.definitions.set(def.nameId, def);
};

/** Add a reference to the builder */
export const addReference = (builder: SymbolTableBuilder, ref: SymbolReference): void => {
  const existing = builder.references.get(ref.targetId);
  if (existing) {
    existing.push(ref);
  } else {
    builder.references.set(ref.targetId, [ref]);
  }
};

/** Add a scope snapshot */
export const addScopeSnapshot = (builder: SymbolTableBuilder, snapshot: ScopeSnapshot): void => {
  builder.scopeSnapshots.push(snapshot);
};

/** Freeze builder into immutable SymbolTable */
export const freezeSymbolTable = (builder: SymbolTableBuilder): SymbolTable => ({
  definitions: builder.definitions,
  references: builder.references,
  constructors: builder.constructors,
  types: builder.types,
  scopeSnapshots: builder.scopeSnapshots,
});

// =============================================================================
// Enrichment with Type Information
// =============================================================================

/** Enrich symbol definitions with type schemes, applying final substitution */
export const enrichWithTypes = (
  table: SymbolTable,
  typeMap: ReadonlyMap<number, Type>,
  typeEnv: ReadonlyMap<string, Scheme>,
  subst: Subst,
): SymbolTable => {
  const enrichedDefs = new Map<number, SymbolDefinition>();

  for (const [nameId, def] of table.definitions) {
    // Try to find scheme in typeEnv (keyed as "{id}:{name}")
    const schemeKey = `${nameId}:${def.name}`;
    const envScheme = typeEnv.get(schemeKey);

    if (envScheme) {
      // Apply substitution to resolve type variables
      const resolvedType = applySubst(subst, envScheme.type);
      const scheme: Scheme = { ...envScheme, type: resolvedType };
      enrichedDefs.set(nameId, { ...def, scheme });
    } else {
      // Fallback: use typeMap if available
      const type = typeMap.get(nameId);
      if (type) {
        const resolvedType = applySubst(subst, type);
        enrichedDefs.set(nameId, {
          ...def,
          scheme: { vars: [], constraints: [], type: resolvedType },
        });
      } else {
        enrichedDefs.set(nameId, def);
      }
    }
  }

  return {
    ...table,
    definitions: enrichedDefs,
  };
};

// =============================================================================
// Lookup Utilities
// =============================================================================

/** Find symbol at a specific offset within a file */
export const findSymbolAt = (
  table: SymbolTable,
  fileId: FileId,
  offset: number,
):
  | { kind: "definition"; def: SymbolDefinition }
  | { kind: "reference"; ref: SymbolReference }
  | null => {
  // Check definitions
  for (const def of table.definitions.values()) {
    if (
      def.location.fileId === fileId &&
      offset >= def.location.start &&
      offset < def.location.end
    ) {
      return { kind: "definition", def };
    }
  }

  // Check references
  for (const refs of table.references.values()) {
    for (const ref of refs) {
      if (
        ref.location.fileId === fileId &&
        offset >= ref.location.start &&
        offset < ref.location.end
      ) {
        return { kind: "reference", ref };
      }
    }
  }

  return null;
};

/** Get scope at a specific offset (for autocomplete) */
export const getScopeAt = (table: SymbolTable, offset: number): ScopeSnapshot | null => {
  let best: ScopeSnapshot | null = null;

  for (const snapshot of table.scopeSnapshots) {
    if (snapshot.offset <= offset) {
      if (!best || snapshot.offset > best.offset) {
        best = snapshot;
      }
    }
  }

  return best;
};
