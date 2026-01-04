/**
 * Transport abstraction for LSP communication.
 *
 * The transport layer handles message serialization and delivery,
 * allowing the LSP server to work over different channels:
 * - stdio: for traditional editor integration
 * - postMessage: for web worker communication (Monaco playground)
 */

// =============================================================================
// JSON-RPC MESSAGE TYPES
// =============================================================================

/**
 * Base JSON-RPC 2.0 message types.
 * These wrap around the LSP protocol messages.
 */

export type JsonRpcMessage = JsonRpcRequest | JsonRpcResponse | JsonRpcNotification;

export type JsonRpcRequest = {
  readonly jsonrpc: "2.0";
  readonly id: number | string;
  readonly method: string;
  readonly params?: unknown;
};

export type JsonRpcResponse = {
  readonly jsonrpc: "2.0";
  readonly id: number | string | null;
  readonly result?: unknown;
  readonly error?: JsonRpcError;
};

export type JsonRpcNotification = {
  readonly jsonrpc: "2.0";
  readonly method: string;
  readonly params?: unknown;
};

export type JsonRpcError = {
  readonly code: number;
  readonly message: string;
  readonly data?: unknown;
};

// =============================================================================
// TYPE GUARDS
// =============================================================================

export const isRequest = (msg: JsonRpcMessage): msg is JsonRpcRequest =>
  "id" in msg && "method" in msg;

export const isResponse = (msg: JsonRpcMessage): msg is JsonRpcResponse =>
  "id" in msg && !("method" in msg);

export const isNotification = (msg: JsonRpcMessage): msg is JsonRpcNotification =>
  !("id" in msg) && "method" in msg;

// =============================================================================
// TRANSPORT INTERFACE
// =============================================================================

/**
 * Transport-agnostic message passing interface.
 * Transports implement this to connect the LSP server to different environments.
 */
export type Transport = {
  /** Send a message from server to client */
  send(message: JsonRpcMessage): void;

  /** Register handler for incoming messages */
  onMessage(handler: (message: JsonRpcMessage) => void): void;

  /** Called when transport should shut down */
  onClose(handler: () => void): void;
};

// =============================================================================
// RESPONSE HELPERS
// =============================================================================

export const successResponse = (id: number | string, result: unknown): JsonRpcResponse => ({
  jsonrpc: "2.0",
  id,
  result,
});

export const errorResponse = (
  id: number | string,
  code: number,
  message: string,
): JsonRpcResponse => ({
  jsonrpc: "2.0",
  id,
  error: { code, message },
});

export const notification = (method: string, params: unknown): JsonRpcNotification => ({
  jsonrpc: "2.0",
  method,
  params,
});
