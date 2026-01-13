/**
 * LSP Analysis Module
 *
 * Wraps compilation for LSP use, managing analysis results
 * and providing access to symbol information.
 */

import { compileForLSP, type ModuleInfo, type SourceFile } from "../compile";
import type { CheckOutput } from "../checker";
import type { Diagnostic } from "../diagnostics";
import { typeToString } from "../checker";
import type { DocumentManager } from "./documents";
import { getSourceFiles } from "./documents";
import preludeContent from "../../lib/prelude.alg" with { type: "text" };

// =============================================================================
// Prelude Loading
// =============================================================================

/** The prelude source file (embedded at build time) */
const prelude: SourceFile = { path: "<prelude>", content: preludeContent };
import {
  buildLineIndex,
  offsetToPosition,
  positionToOffset,
  spanToRange,
  type LineIndex,
  type Position,
  type Range,
} from "./positions";
import { findSymbolAt, getScopeAt, type SymbolDefinition, type SymbolTable } from "./symbols";
import { getFileById, getFileByPath, type FileRegistry } from "./workspace";

// =============================================================================
// Types
// =============================================================================

/** Result of analysis */
export type AnalysisResult = {
  readonly success: boolean;
  readonly diagnostics: readonly Diagnostic[];
  readonly symbolTable: SymbolTable | null;
  readonly fileRegistry: FileRegistry;
  readonly lineIndices: ReadonlyMap<string, LineIndex>;
  readonly modules: ModuleInfo;
  readonly checkOutput: CheckOutput | null;
};

/** Analyzer that maintains analysis state */
export type Analyzer = {
  lastResult: AnalysisResult | null;
};

// =============================================================================
// Factory
// =============================================================================

/** Create a new analyzer */
export const createAnalyzer = (): Analyzer => ({
  lastResult: null,
});

// =============================================================================
// Analysis
// =============================================================================

/** Run analysis on all open documents */
export const analyze = (analyzer: Analyzer, documents: DocumentManager): AnalysisResult => {
  // Get prelude and user sources
  const userSources = getSourceFiles(documents);
  const sources = [prelude, ...userSources];

  // Build line indices for each file
  const lineIndices = new Map<string, LineIndex>();
  for (const source of sources) {
    lineIndices.set(source.path, buildLineIndex(source.content));
  }

  // Compile for LSP
  const compileResult = compileForLSP(sources as readonly SourceFile[]);

  const result: AnalysisResult = {
    success: compileResult.success,
    diagnostics: compileResult.diagnostics,
    symbolTable: compileResult.symbolTable,
    fileRegistry: compileResult.fileRegistry,
    lineIndices,
    modules: compileResult.modules,
    checkOutput: compileResult.checkOutput,
  };

  analyzer.lastResult = result;
  return result;
};

// =============================================================================
// Position Utilities
// =============================================================================

/** Get line index for a file path */
export const getLineIndex = (result: AnalysisResult, path: string): LineIndex | null => {
  return result.lineIndices.get(path) ?? null;
};

/** Convert LSP position to byte offset */
export const lspPositionToOffset = (
  result: AnalysisResult,
  path: string,
  position: Position,
): number | null => {
  const lineIndex = result.lineIndices.get(path);
  if (!lineIndex) return null;
  return positionToOffset(lineIndex, position);
};

/** Convert byte offset to LSP position */
export const offsetToLspPosition = (
  result: AnalysisResult,
  path: string,
  offset: number,
): Position | null => {
  const lineIndex = result.lineIndices.get(path);
  if (!lineIndex) return null;
  return offsetToPosition(lineIndex, offset);
};

// =============================================================================
// Symbol Lookup
// =============================================================================

/** Find symbol at a position in a file */
export const findSymbolAtPosition = (
  result: AnalysisResult,
  path: string,
  position: Position,
):
  | { kind: "definition"; def: SymbolDefinition }
  | { kind: "reference"; targetId: number }
  | null => {
  if (!result.symbolTable) return null;

  const file = getFileByPath(result.fileRegistry, path);
  if (!file) return null;

  const lineIndex = result.lineIndices.get(path);
  if (!lineIndex) return null;

  const offset = positionToOffset(lineIndex, position);
  const symbol = findSymbolAt(result.symbolTable, file.id, offset);

  if (!symbol) return null;

  if (symbol.kind === "definition") {
    return { kind: "definition", def: symbol.def };
  } else {
    return { kind: "reference", targetId: symbol.ref.targetId };
  }
};

/** Get definition for a symbol at position */
export const getDefinitionAtPosition = (
  result: AnalysisResult,
  path: string,
  position: Position,
): { path: string; range: Range } | null => {
  if (!result.symbolTable) return null;

  const symbol = findSymbolAtPosition(result, path, position);
  if (!symbol) return null;

  // Get the definition
  const defId = symbol.kind === "definition" ? symbol.def.nameId : symbol.targetId;
  const def = result.symbolTable.definitions.get(defId);
  if (!def) return null;

  // Get file for definition
  const defFile = getFileById(result.fileRegistry, def.location.fileId);
  if (!defFile) return null;

  // Get line index for definition file
  const defLineIndex = result.lineIndices.get(defFile.path);
  if (!defLineIndex) return null;

  // Convert location (which is a span) to range
  const range = spanToRange(defLineIndex, def.location);

  return { path: defFile.path, range };
};

/** Get all references to a symbol at position */
export const getReferencesAtPosition = (
  result: AnalysisResult,
  path: string,
  position: Position,
  includeDefinition: boolean = true,
): { path: string; range: Range }[] => {
  if (!result.symbolTable) return [];

  const symbol = findSymbolAtPosition(result, path, position);
  if (!symbol) return [];

  const defId = symbol.kind === "definition" ? symbol.def.nameId : symbol.targetId;
  const def = result.symbolTable.definitions.get(defId);
  const refs = result.symbolTable.references.get(defId) ?? [];

  const locations: { path: string; range: Range }[] = [];

  // Include definition if requested
  if (includeDefinition && def) {
    const defFile = getFileById(result.fileRegistry, def.location.fileId);
    const defLineIndex = defFile ? result.lineIndices.get(defFile.path) : null;
    if (defFile && defLineIndex) {
      locations.push({
        path: defFile.path,
        range: spanToRange(defLineIndex, def.location),
      });
    }
  }

  // Add all references
  for (const ref of refs) {
    const refFile = getFileById(result.fileRegistry, ref.location.fileId);
    const refLineIndex = refFile ? result.lineIndices.get(refFile.path) : null;
    if (refFile && refLineIndex) {
      locations.push({
        path: refFile.path,
        range: spanToRange(refLineIndex, ref.location),
      });
    }
  }

  return locations;
};

/** Get hover information at position */
export const getHoverAtPosition = (
  result: AnalysisResult,
  path: string,
  position: Position,
): { contents: string; range: Range } | null => {
  if (!result.symbolTable) return null;

  const file = getFileByPath(result.fileRegistry, path);
  if (!file) return null;

  const lineIndex = result.lineIndices.get(path);
  if (!lineIndex) return null;

  const offset = positionToOffset(lineIndex, position);
  const symbol = findSymbolAt(result.symbolTable, file.id, offset);
  if (!symbol) return null;

  // Get definition
  const defId = symbol.kind === "definition" ? symbol.def.nameId : symbol.ref.targetId;
  const def = result.symbolTable.definitions.get(defId);
  if (!def) return null;

  // Build hover content
  let contents = `**${def.name}**`;
  if (def.scheme) {
    contents += ` : ${typeToString(def.scheme.type)}`;
  }
  contents += `\n\n*${def.kind}*`;

  // Get range from the symbol location (location is a Span)
  const loc = symbol.kind === "definition" ? symbol.def.location : symbol.ref.location;
  const range = spanToRange(lineIndex, loc);

  return { contents, range };
};

/** Get the word before a position (for detecting Module. completions) */
const getWordBeforeDot = (content: string, offset: number): string | null => {
  // Look backwards for a dot
  if (offset <= 0 || content[offset - 1] !== ".") return null;

  // Find the start of the word before the dot
  let start = offset - 2;
  while (start >= 0 && /[a-zA-Z0-9_]/.test(content[start]!)) {
    start--;
  }
  start++;

  const word = content.slice(start, offset - 1);
  // Module names start with uppercase
  if (word.length > 0 && /^[A-Z]/.test(word)) {
    return word;
  }
  return null;
};

/** Check if we're in a pattern context (after 'when' in a match expression) */
const isInPatternContext = (content: string, offset: number): boolean => {
  // Look backwards for 'when' keyword, skipping whitespace
  let pos = offset - 1;

  // Skip any partial identifier being typed
  while (pos >= 0 && /[a-zA-Z0-9_]/.test(content[pos]!)) {
    pos--;
  }

  // Skip whitespace
  while (pos >= 0 && /\s/.test(content[pos]!)) {
    pos--;
  }

  // Check if we're right after 'when'
  if (pos >= 3) {
    const beforeCursor = content.slice(pos - 3, pos + 1);
    if (beforeCursor === "when") {
      return true;
    }
  }

  return false;
};

/** Find the position after 'match' keyword, returns the start of the scrutinee */
const findMatchScrutineeStart = (content: string, offset: number): number | null => {
  // Search backwards for 'match' keyword
  let pos = offset;
  while (pos >= 5) {
    // Check for 'match' followed by whitespace
    if (
      content.slice(pos - 5, pos) === "match" &&
      (pos === 5 || /\s/.test(content[pos - 6]!)) &&
      /\s/.test(content[pos]!)
    ) {
      // Skip whitespace after 'match'
      let scrutineeStart = pos;
      while (scrutineeStart < content.length && /\s/.test(content[scrutineeStart]!)) {
        scrutineeStart++;
      }
      return scrutineeStart;
    }
    pos--;
  }
  return null;
};

/** Extract the base type constructor name from a Type */
const getTypeConstructorName = (type: import("../types").Type): string | null => {
  switch (type.kind) {
    case "TCon":
      return type.name;
    case "TApp":
      return getTypeConstructorName(type.con);
    default:
      return null;
  }
};

/** Get completions at position */
export const getCompletionsAtPosition = (
  result: AnalysisResult,
  path: string,
  position: Position,
): { label: string; kind: string; detail?: string }[] => {
  const file = getFileByPath(result.fileRegistry, path);
  if (!file) return [];

  const lineIndex = result.lineIndices.get(path);
  if (!lineIndex) return [];

  const offset = positionToOffset(lineIndex, position);

  // Check if we're completing after "Module."
  const moduleName = getWordBeforeDot(file.content, offset);
  if (moduleName) {
    const moduleMembers = result.modules.get(moduleName);
    if (moduleMembers) {
      return Array.from(moduleMembers).map((member) => ({
        label: member,
        kind: "Function",
        detail: `${moduleName}.${member}`,
      }));
    }
    // Unknown module, return empty
    return [];
  }

  // Check if we're in a pattern context (after 'when')
  if (isInPatternContext(file.content, offset)) {
    const completions: { label: string; kind: string; detail?: string }[] = [];

    // Try to determine the scrutinee type for filtering
    let scrutineeTypeName: string | null = null;
    if (result.checkOutput) {
      const scrutineeStart = findMatchScrutineeStart(file.content, offset);
      if (scrutineeStart !== null) {
        const nodeId = result.checkOutput.spanToNodeId.get(scrutineeStart);
        if (nodeId !== undefined) {
          const scrutineeType = result.checkOutput.nodeTypeMap.get(nodeId);
          if (scrutineeType) {
            scrutineeTypeName = getTypeConstructorName(scrutineeType);
          }
        }
      }
    }

    // Add constructors, filtered by scrutinee type if known
    if (result.symbolTable) {
      for (const [name, ctor] of result.symbolTable.constructors) {
        if (scrutineeTypeName === null || ctor.typeName === scrutineeTypeName) {
          completions.push({
            label: name,
            kind: "Constructor",
            detail: ctor.typeName,
          });
        }
      }
    }

    return completions;
  }

  // Regular completions
  const completions: { label: string; kind: string; detail?: string }[] = [];

  // Add module names
  for (const moduleName of result.modules.keys()) {
    completions.push({
      label: moduleName,
      kind: "Module",
    });
  }

  // Add bindings from scope
  if (result.symbolTable) {
    const scope = getScopeAt(result.symbolTable, offset);
    if (scope) {
      for (const [name, nameId] of scope.bindings) {
        const def = result.symbolTable.definitions.get(nameId);
        const detail = def?.scheme ? typeToString(def.scheme.type) : undefined;
        completions.push({
          label: name,
          kind: def?.kind === "function" ? "Function" : "Variable",
          detail,
        });
      }
    }

    // Add constructors
    for (const [name, ctor] of result.symbolTable.constructors) {
      completions.push({
        label: name,
        kind: "Constructor",
        detail: ctor.typeName,
      });
    }
  }

  return completions;
};

// =============================================================================
// Document Symbols
// =============================================================================

/** Get document symbols for outline */
export const getDocumentSymbols = (
  result: AnalysisResult,
  path: string,
): { name: string; kind: string; range: Range; detail?: string }[] => {
  if (!result.symbolTable) return [];

  const file = getFileByPath(result.fileRegistry, path);
  if (!file) return [];

  const lineIndex = result.lineIndices.get(path);
  if (!lineIndex) return [];

  const symbols: { name: string; kind: string; range: Range; detail?: string }[] = [];

  // Find all definitions in this file
  for (const def of result.symbolTable.definitions.values()) {
    if (def.location.fileId !== file.id) continue;

    // Only include top-level symbols (functions, types)
    if (def.kind === "parameter" || def.kind === "pattern-binding") continue;

    const range = spanToRange(lineIndex, def.location);
    const detail = def.scheme ? typeToString(def.scheme.type) : undefined;

    symbols.push({
      name: def.name,
      kind: def.kind,
      range,
      detail,
    });
  }

  // Add type definitions
  for (const [name, typeInfo] of result.symbolTable.types) {
    if (typeInfo.location && typeInfo.location.fileId === file.id) {
      const range = spanToRange(lineIndex, typeInfo.location);
      symbols.push({
        name,
        kind: "type",
        range,
        detail: typeInfo.params.length > 0 ? typeInfo.params.join(" ") : undefined,
      });
    }
  }

  return symbols;
};
