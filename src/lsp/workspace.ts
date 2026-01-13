/**
 * LSP Workspace Management
 *
 * Handles file identity tracking through the compilation pipeline.
 * When multiple files are combined for compilation, we need to map
 * global byte offsets back to file-local positions.
 */

import type { FileId, Location, Span } from "./symbols";

// =============================================================================
// File Registry Types
// =============================================================================

/** Information about a source file */
export type FileInfo = {
  readonly id: FileId;
  readonly path: string;
  readonly content: string;
  /** Byte offset where this file's content starts in combined source */
  readonly globalOffset: number;
  /** Length of file content in bytes */
  readonly length: number;
};

/** Registry of all files in the workspace */
export type FileRegistry = {
  readonly files: ReadonlyMap<string, FileInfo>;
  readonly byId: ReadonlyMap<FileId, FileInfo>;
  readonly nextId: number;
};

// =============================================================================
// Builder Types
// =============================================================================

/** Mutable builder for constructing FileRegistry */
export type FileRegistryBuilder = {
  files: Map<string, FileInfo>;
  byId: Map<FileId, FileInfo>;
  nextId: number;
  currentOffset: number;
};

/** Create an empty file registry builder */
export const createFileRegistryBuilder = (): FileRegistryBuilder => ({
  files: new Map(),
  byId: new Map(),
  nextId: 0,
  currentOffset: 0,
});

/** Register a file and return its ID */
export const registerFile = (
  builder: FileRegistryBuilder,
  path: string,
  content: string,
): FileId => {
  const id = builder.nextId++;
  const info: FileInfo = {
    id,
    path,
    content,
    globalOffset: builder.currentOffset,
    length: content.length,
  };

  builder.files.set(path, info);
  builder.byId.set(id, info);
  builder.currentOffset += content.length;

  // Account for newline separator between files
  builder.currentOffset += 1;

  return id;
};

/** Freeze builder into immutable FileRegistry */
export const freezeFileRegistry = (builder: FileRegistryBuilder): FileRegistry => ({
  files: builder.files,
  byId: builder.byId,
  nextId: builder.nextId,
});

// =============================================================================
// Location Conversion
// =============================================================================

/** Convert a global byte offset to file-local location */
export const globalToLocal = (
  registry: FileRegistry,
  globalOffset: number,
): { fileId: FileId; localOffset: number } | null => {
  for (const file of registry.byId.values()) {
    const endOffset = file.globalOffset + file.length;
    if (globalOffset >= file.globalOffset && globalOffset < endOffset) {
      return {
        fileId: file.id,
        localOffset: globalOffset - file.globalOffset,
      };
    }
  }
  return null;
};

/** Convert a global span to a Location */
export const spanToLocation = (
  registry: FileRegistry,
  span: Span,
): Location | null => {
  const result = globalToLocal(registry, span.start);
  if (!result) return null;

  // Verify end is in the same file
  const file = registry.byId.get(result.fileId);
  if (!file) return null;

  const endOffset = file.globalOffset + file.length;
  if (span.end > endOffset) {
    // Span crosses file boundaries - shouldn't happen, but handle gracefully
    return null;
  }

  return {
    fileId: result.fileId,
    span: {
      start: span.start - file.globalOffset,
      end: span.end - file.globalOffset,
    },
  };
};

/** Convert a local offset back to global */
export const localToGlobal = (
  registry: FileRegistry,
  fileId: FileId,
  localOffset: number,
): number | null => {
  const file = registry.byId.get(fileId);
  if (!file) return null;
  return file.globalOffset + localOffset;
};

/** Convert a Location back to global span */
export const locationToGlobalSpan = (
  registry: FileRegistry,
  location: Location,
): Span | null => {
  const file = registry.byId.get(location.fileId);
  if (!file) return null;

  return {
    start: file.globalOffset + location.span.start,
    end: file.globalOffset + location.span.end,
  };
};

// =============================================================================
// File Lookup
// =============================================================================

/** Get file info by path */
export const getFileByPath = (
  registry: FileRegistry,
  path: string,
): FileInfo | null => {
  return registry.files.get(path) ?? null;
};

/** Get file info by ID */
export const getFileById = (
  registry: FileRegistry,
  id: FileId,
): FileInfo | null => {
  return registry.byId.get(id) ?? null;
};

/** Get all file paths */
export const getAllPaths = (registry: FileRegistry): string[] => {
  return Array.from(registry.files.keys());
};

// =============================================================================
// URI Utilities
// =============================================================================

/** Convert file path to URI */
export const pathToUri = (path: string): string => {
  // Ensure path is absolute and properly formatted
  if (path.startsWith("/")) {
    return `file://${path}`;
  }
  return `file:///${path}`;
};

/** Convert URI to file path */
export const uriToPath = (uri: string): string => {
  if (uri.startsWith("file:///")) {
    // Windows: file:///C:/path
    const path = uri.slice(8);
    // Check if it looks like a Windows path
    if (path.length > 1 && path[1] === ":") {
      return path;
    }
    // Unix: file:///path -> /path
    return "/" + path;
  }
  if (uri.startsWith("file://")) {
    return uri.slice(7);
  }
  return uri;
};
