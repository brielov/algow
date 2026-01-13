/**
 * LSP JSON-RPC Protocol Handler
 *
 * Handles reading/writing LSP messages over stdio.
 * LSP uses JSON-RPC 2.0 with Content-Length headers.
 */

// =============================================================================
// Message Types
// =============================================================================

/** JSON-RPC request */
export type RequestMessage = {
  readonly jsonrpc: "2.0";
  readonly id: number | string;
  readonly method: string;
  readonly params?: unknown;
};

/** JSON-RPC response */
export type ResponseMessage = {
  readonly jsonrpc: "2.0";
  readonly id: number | string | null;
  readonly result?: unknown;
  readonly error?: ResponseError;
};

/** JSON-RPC notification (no id, no response expected) */
export type NotificationMessage = {
  readonly jsonrpc: "2.0";
  readonly method: string;
  readonly params?: unknown;
};

/** JSON-RPC error */
export type ResponseError = {
  readonly code: number;
  readonly message: string;
  readonly data?: unknown;
};

/** Any LSP message */
export type Message = RequestMessage | ResponseMessage | NotificationMessage;

// =============================================================================
// Error Codes
// =============================================================================

export const ErrorCodes = {
  ParseError: -32700,
  InvalidRequest: -32600,
  MethodNotFound: -32601,
  InvalidParams: -32602,
  InternalError: -32603,
  ServerNotInitialized: -32002,
  UnknownErrorCode: -32001,
  RequestCancelled: -32800,
  ContentModified: -32801,
} as const;

// =============================================================================
// Message Reader
// =============================================================================

/** Buffer for accumulating incoming data */
export type MessageReader = {
  buffer: Buffer;
  contentLength: number | null;
};

/** Create a new message reader */
export const createMessageReader = (): MessageReader => ({
  buffer: Buffer.alloc(0),
  contentLength: null,
});

/** Header separator */
const HEADER_SEPARATOR = "\r\n\r\n";
const CONTENT_LENGTH_HEADER = "Content-Length: ";

/**
 * Feed data into the reader and extract complete messages.
 * Returns an array of parsed messages (may be empty).
 */
export const feedData = (reader: MessageReader, chunk: Buffer): Message[] => {
  reader.buffer = Buffer.concat([reader.buffer, chunk]);
  const messages: Message[] = [];

  while (true) {
    // Try to parse header if we don't have content length yet
    if (reader.contentLength === null) {
      const headerEnd = reader.buffer.indexOf(HEADER_SEPARATOR);
      if (headerEnd === -1) break; // Need more data

      const header = reader.buffer.subarray(0, headerEnd).toString("utf8");
      const lines = header.split("\r\n");

      for (const line of lines) {
        if (line.startsWith(CONTENT_LENGTH_HEADER)) {
          reader.contentLength = parseInt(line.slice(CONTENT_LENGTH_HEADER.length), 10);
          break;
        }
      }

      if (reader.contentLength === null) {
        // Invalid header, skip it
        reader.buffer = reader.buffer.subarray(headerEnd + HEADER_SEPARATOR.length);
        continue;
      }

      // Remove header from buffer
      reader.buffer = reader.buffer.subarray(headerEnd + HEADER_SEPARATOR.length);
    }

    // Check if we have the complete content
    if (reader.buffer.length < reader.contentLength) break; // Need more data

    // Extract content
    const content = reader.buffer.subarray(0, reader.contentLength).toString("utf8");
    reader.buffer = reader.buffer.subarray(reader.contentLength);
    reader.contentLength = null;

    // Parse JSON
    try {
      const message = JSON.parse(content) as Message;
      messages.push(message);
    } catch {
      // Invalid JSON, skip
    }
  }

  return messages;
};

// =============================================================================
// Message Writer
// =============================================================================

/** Encode a message for transmission */
export const encodeMessage = (message: ResponseMessage | NotificationMessage): Buffer => {
  const content = JSON.stringify(message);
  const contentBytes = Buffer.from(content, "utf8");
  const header = `Content-Length: ${contentBytes.length}\r\n\r\n`;
  return Buffer.concat([Buffer.from(header, "utf8"), contentBytes]);
};

// =============================================================================
// Message Helpers
// =============================================================================

/** Check if message is a request (has id and method) */
export const isRequest = (msg: Message): msg is RequestMessage => {
  return "id" in msg && "method" in msg;
};

/** Check if message is a notification (has method but no id) */
export const isNotification = (msg: Message): msg is NotificationMessage => {
  return "method" in msg && !("id" in msg);
};

/** Check if message is a response (has id but no method) */
export const isResponse = (msg: Message): msg is ResponseMessage => {
  return "id" in msg && !("method" in msg);
};

/** Create a success response */
export const createResponse = (id: number | string, result: unknown): ResponseMessage => ({
  jsonrpc: "2.0",
  id,
  result,
});

/** Create an error response */
export const createErrorResponse = (
  id: number | string | null,
  code: number,
  message: string,
  data?: unknown,
): ResponseMessage => ({
  jsonrpc: "2.0",
  id,
  error: { code, message, data },
});

/** Create a notification */
export const createNotification = (method: string, params?: unknown): NotificationMessage => ({
  jsonrpc: "2.0",
  method,
  params,
});
