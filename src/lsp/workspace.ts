/**
 * LSP Workspace Management
 *
 * Re-exports file registry types from core and adds URI utilities.
 * File-aware spans (spans include fileId) eliminate the need for
 * global offset tracking and conversion.
 */

// Re-export FileRegistry types from core
export {
  createFileRegistryBuilder,
  freezeFileRegistry,
  getAllPaths,
  getFileById,
  getFileByPath,
  registerFile,
  type FileInfo,
  type FileRegistry,
  type FileRegistryBuilder,
} from "../files";

// Re-export FileId from surface
export type { FileId } from "../surface";

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
