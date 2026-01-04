/**
 * Web Worker Transport for LSP.
 *
 * Implements LSP communication over postMessage for use with Monaco editor
 * in web browsers.
 */

import type { JsonRpcMessage, Transport } from "./transport";

// Web Worker global scope type
declare const self: {
  onmessage: ((event: MessageEvent<JsonRpcMessage>) => void) | null;
  postMessage(message: JsonRpcMessage): void;
};

/**
 * Create a web worker transport for LSP communication.
 * Uses postMessage to communicate with the main thread.
 */
export const createWorkerTransport = (): Transport => {
  let messageHandler: ((msg: JsonRpcMessage) => void) | null = null;

  self.onmessage = (event: MessageEvent<JsonRpcMessage>) => {
    messageHandler?.(event.data);
  };

  return {
    send(message: JsonRpcMessage): void {
      self.postMessage(message);
    },

    onMessage(handler: (msg: JsonRpcMessage) => void): void {
      messageHandler = handler;
    },

    onClose(): void {
      // Web workers don't have a close event
    },
  };
};
