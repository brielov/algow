/**
 * File Registry - Core Infrastructure
 *
 * Tracks file identity through the compilation pipeline.
 * Used by parser to embed file IDs in spans, eliminating the need
 * for span offset adjustment in multi-file compilation.
 */

import type { FileId, Span } from "./surface";

// Re-export for convenience
export type { FileId, Span } from "./surface";

// =============================================================================
// File Registry Types
// =============================================================================

/** Information about a source file */
export type FileInfo = {
  readonly id: FileId;
  readonly path: string;
  readonly content: string;
};

/** Registry of all files in compilation */
export type FileRegistry = {
  readonly files: ReadonlyMap<string, FileInfo>;
  readonly byId: ReadonlyMap<FileId, FileInfo>;
};

// =============================================================================
// Builder Types
// =============================================================================

/** Mutable builder for constructing FileRegistry */
export type FileRegistryBuilder = {
  files: Map<string, FileInfo>;
  byId: Map<FileId, FileInfo>;
  nextId: number;
};

/** Create an empty file registry builder */
export const createFileRegistryBuilder = (): FileRegistryBuilder => ({
  files: new Map(),
  byId: new Map(),
  nextId: 0,
});

/** Register a file and return its ID */
export const registerFile = (
  builder: FileRegistryBuilder,
  path: string,
  content: string,
): FileId => {
  // Check if already registered
  const existing = builder.files.get(path);
  if (existing) {
    return existing.id;
  }

  const id = builder.nextId++;
  const info: FileInfo = { id, path, content };

  builder.files.set(path, info);
  builder.byId.set(id, info);

  return id;
};

/** Freeze builder into immutable FileRegistry */
export const freezeFileRegistry = (builder: FileRegistryBuilder): FileRegistry => ({
  files: builder.files,
  byId: builder.byId,
});

// =============================================================================
// File Lookup
// =============================================================================

/** Get file info by path */
export const getFileByPath = (registry: FileRegistry, path: string): FileInfo | null => {
  return registry.files.get(path) ?? null;
};

/** Get file info by ID */
export const getFileById = (registry: FileRegistry, id: FileId): FileInfo | null => {
  return registry.byId.get(id) ?? null;
};

/** Get all file paths */
export const getAllPaths = (registry: FileRegistry): string[] => {
  return Array.from(registry.files.keys());
};

// =============================================================================
// Span Utilities
// =============================================================================

/** Create a span in a specific file */
export const span = (fileId: FileId, start: number, end: number): Span => ({
  fileId,
  start,
  end,
});

/** Get source text for a span */
export const getSpanText = (registry: FileRegistry, s: Span): string | null => {
  const file = registry.byId.get(s.fileId);
  if (!file) return null;
  return file.content.slice(s.start, s.end);
};
