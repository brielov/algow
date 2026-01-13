/**
 * LSP Position Conversion Utilities
 *
 * Converts between byte offsets (used internally) and LSP positions
 * (line:character using UTF-16 code units).
 */

import type { Span } from "./symbols";

// =============================================================================
// LSP Types
// =============================================================================

/** LSP Position (0-based line and character) */
export type Position = {
  readonly line: number;
  readonly character: number;
};

/** LSP Range */
export type Range = {
  readonly start: Position;
  readonly end: Position;
};

// =============================================================================
// Line Index
// =============================================================================

/** Pre-computed line start offsets for fast position conversion */
export type LineIndex = {
  readonly source: string;
  /** Byte offset where each line starts */
  readonly lineStarts: readonly number[];
};

/** Build a line index from source text */
export const buildLineIndex = (source: string): LineIndex => {
  const lineStarts: number[] = [0];

  for (let i = 0; i < source.length; i++) {
    if (source[i] === "\n") {
      lineStarts.push(i + 1);
    }
  }

  return { source, lineStarts };
};

// =============================================================================
// Offset to Position
// =============================================================================

/** Convert byte offset to LSP Position */
export const offsetToPosition = (index: LineIndex, offset: number): Position => {
  // Binary search for line
  let low = 0;
  let high = index.lineStarts.length - 1;

  while (low < high) {
    const mid = Math.floor((low + high + 1) / 2);
    if (index.lineStarts[mid]! <= offset) {
      low = mid;
    } else {
      high = mid - 1;
    }
  }

  const line = low;
  const lineStart = index.lineStarts[line]!;

  // Character offset within line (UTF-16 code units)
  // For simplicity, treat as 1:1 with bytes (ASCII compatible)
  // Full Unicode support would require counting UTF-16 code units
  const character = offset - lineStart;

  return { line, character };
};

/** Convert span to LSP Range */
export const spanToRange = (index: LineIndex, span: Span): Range => ({
  start: offsetToPosition(index, span.start),
  end: offsetToPosition(index, span.end),
});

// =============================================================================
// Position to Offset
// =============================================================================

/** Convert LSP Position to byte offset */
export const positionToOffset = (index: LineIndex, position: Position): number => {
  const { line, character } = position;

  // Clamp line to valid range
  const clampedLine = Math.max(0, Math.min(line, index.lineStarts.length - 1));
  const lineStart = index.lineStarts[clampedLine]!;

  // Calculate line end
  const lineEnd =
    clampedLine + 1 < index.lineStarts.length
      ? index.lineStarts[clampedLine + 1]! - 1 // -1 to exclude newline
      : index.source.length;

  // Clamp character to line length
  const lineLength = lineEnd - lineStart;
  const clampedChar = Math.max(0, Math.min(character, lineLength));

  return lineStart + clampedChar;
};

/** Convert LSP Range to span */
export const rangeToSpan = (index: LineIndex, range: Range): Span => ({
  start: positionToOffset(index, range.start),
  end: positionToOffset(index, range.end),
});

// =============================================================================
// Line Utilities
// =============================================================================

/** Get line count */
export const getLineCount = (index: LineIndex): number => {
  return index.lineStarts.length;
};

/** Get line at position */
export const getLineAt = (index: LineIndex, line: number): string => {
  if (line < 0 || line >= index.lineStarts.length) {
    return "";
  }

  const start = index.lineStarts[line]!;
  const end =
    line + 1 < index.lineStarts.length
      ? index.lineStarts[line + 1]! - 1 // -1 to exclude newline
      : index.source.length;

  return index.source.slice(start, end);
};

/** Get offset at start of line */
export const getLineStartOffset = (index: LineIndex, line: number): number => {
  if (line < 0) return 0;
  if (line >= index.lineStarts.length) return index.source.length;
  return index.lineStarts[line]!;
};

/** Get offset at end of line */
export const getLineEndOffset = (index: LineIndex, line: number): number => {
  if (line < 0) return 0;
  if (line >= index.lineStarts.length - 1) return index.source.length;
  return index.lineStarts[line + 1]! - 1; // -1 to exclude newline
};

// =============================================================================
// Text Utilities
// =============================================================================

/** Get text in a span */
export const getTextInSpan = (index: LineIndex, span: Span): string => {
  return index.source.slice(span.start, span.end);
};

/** Get text in a range */
export const getTextInRange = (index: LineIndex, range: Range): string => {
  const span = rangeToSpan(index, range);
  return getTextInSpan(index, span);
};

/** Check if offset is at end of line (before newline or EOF) */
export const isAtEndOfLine = (index: LineIndex, offset: number): boolean => {
  if (offset >= index.source.length) return true;
  return index.source[offset] === "\n";
};

/** Check if offset is at start of line */
export const isAtStartOfLine = (index: LineIndex, offset: number): boolean => {
  return index.lineStarts.includes(offset);
};
