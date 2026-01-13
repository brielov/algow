/**
 * LSP Document State Management
 *
 * Tracks open documents and their content.
 */

import { buildLineIndex, type LineIndex } from "./positions";
import { uriToPath } from "./workspace";

// =============================================================================
// Types
// =============================================================================

/** State of a single document */
export type DocumentState = {
  readonly uri: string;
  readonly path: string;
  readonly content: string;
  readonly version: number;
  readonly lineIndex: LineIndex;
};

/** Document manager */
export type DocumentManager = {
  readonly documents: Map<string, DocumentState>;
};

// =============================================================================
// Factory
// =============================================================================

/** Create a new document manager */
export const createDocumentManager = (): DocumentManager => ({
  documents: new Map(),
});

// =============================================================================
// Document Operations
// =============================================================================

/** Open a document */
export const openDocument = (
  manager: DocumentManager,
  uri: string,
  content: string,
  version: number,
): DocumentState => {
  const path = uriToPath(uri);
  const lineIndex = buildLineIndex(content);
  const state: DocumentState = { uri, path, content, version, lineIndex };
  manager.documents.set(uri, state);
  return state;
};

/** Update a document with new content */
export const updateDocument = (
  manager: DocumentManager,
  uri: string,
  content: string,
  version: number,
): DocumentState | null => {
  const existing = manager.documents.get(uri);
  if (!existing) return null;

  const lineIndex = buildLineIndex(content);
  const state: DocumentState = { ...existing, content, version, lineIndex };
  manager.documents.set(uri, state);
  return state;
};

/** Close a document */
export const closeDocument = (manager: DocumentManager, uri: string): boolean => {
  return manager.documents.delete(uri);
};

/** Get a document by URI */
export const getDocument = (manager: DocumentManager, uri: string): DocumentState | null => {
  return manager.documents.get(uri) ?? null;
};

/** Get all open documents */
export const getAllDocuments = (manager: DocumentManager): DocumentState[] => {
  return Array.from(manager.documents.values());
};

/** Check if a document is open */
export const isDocumentOpen = (manager: DocumentManager, uri: string): boolean => {
  return manager.documents.has(uri);
};

// =============================================================================
// Content Helpers
// =============================================================================

/** Get document content by URI */
export const getDocumentContent = (manager: DocumentManager, uri: string): string | null => {
  const doc = manager.documents.get(uri);
  return doc?.content ?? null;
};

/** Get all documents as SourceFile array for compilation */
export const getSourceFiles = (manager: DocumentManager): { path: string; content: string }[] => {
  return getAllDocuments(manager).map((doc) => ({
    path: doc.path,
    content: doc.content,
  }));
};
