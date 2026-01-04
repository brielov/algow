/**
 * Position conversion utilities for LSP.
 *
 * LSP uses 0-based line:character positions (UTF-16 code units).
 * Algow uses byte offsets internally.
 *
 * For ASCII source files (typical for Algow), byte offset equals UTF-16 code units.
 * Full Unicode support would require proper UTF-16 surrogate pair handling.
 */

import type { Position, Range } from "vscode-languageserver-types";
import type { Span } from "../ast";

/**
 * Convert byte offset to 1-based line:col (for CLI error messages).
 */
export const offsetToLineCol = (source: string, offset: number): { line: number; col: number } => {
  let line = 1;
  let col = 1;
  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === "\n") {
      line++;
      col = 1;
    } else {
      col++;
    }
  }
  return { line, col };
};

/**
 * Convert byte offset to LSP Position (0-based line:character).
 */
export const offsetToPosition = (source: string, offset: number): Position => {
  let line = 0;
  let character = 0;

  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === "\n") {
      line++;
      character = 0;
    } else {
      character++;
    }
  }

  return { line, character };
};

/**
 * Convert LSP Position to byte offset.
 */
export const positionToOffset = (source: string, position: Position): number => {
  let currentLine = 0;
  let offset = 0;

  // Find start of target line
  while (currentLine < position.line && offset < source.length) {
    if (source[offset] === "\n") {
      currentLine++;
    }
    offset++;
  }

  // Add character offset within line
  return offset + position.character;
};

/**
 * Convert Algow Span (byte offsets) to LSP Range.
 */
export const spanToRange = (source: string, span: Span): Range => ({
  start: offsetToPosition(source, span.start),
  end: offsetToPosition(source, span.end),
});
