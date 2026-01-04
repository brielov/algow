/**
 * stdio Transport for LSP.
 *
 * Implements the LSP Content-Length header protocol for communication
 * over stdin/stdout with traditional editors (VS Code, Neovim, etc.).
 */

import type { JsonRpcMessage, Transport } from "./transport";

/**
 * Create a stdio transport for LSP communication.
 */
export const createStdioTransport = (): Transport => {
  let messageHandler: ((msg: JsonRpcMessage) => void) | null = null;
  let closeHandler: (() => void) | null = null;
  let buffer = "";

  // Read from stdin
  process.stdin.setEncoding("utf8");
  process.stdin.on("data", (chunk: string) => {
    buffer += chunk;
    processBuffer();
  });

  process.stdin.on("end", () => {
    closeHandler?.();
  });

  const processBuffer = (): void => {
    while (true) {
      // Look for Content-Length header
      const headerEnd = buffer.indexOf("\r\n\r\n");
      if (headerEnd === -1) return;

      const header = buffer.slice(0, headerEnd);
      const match = header.match(/Content-Length: (\d+)/);
      if (!match) {
        // Invalid header, skip to next potential header
        buffer = buffer.slice(headerEnd + 4);
        continue;
      }

      const contentLength = parseInt(match[1]!, 10);
      const contentStart = headerEnd + 4;
      const contentEnd = contentStart + contentLength;

      if (buffer.length < contentEnd) {
        // Not enough data yet
        return;
      }

      const content = buffer.slice(contentStart, contentEnd);
      buffer = buffer.slice(contentEnd);

      try {
        const message = JSON.parse(content) as JsonRpcMessage;
        messageHandler?.(message);
      } catch {
        // Invalid JSON, ignore
      }
    }
  };

  return {
    send(message: JsonRpcMessage): void {
      const content = JSON.stringify(message);
      const header = `Content-Length: ${Buffer.byteLength(content)}\r\n\r\n`;
      process.stdout.write(header + content);
    },

    onMessage(handler: (msg: JsonRpcMessage) => void): void {
      messageHandler = handler;
    },

    onClose(handler: () => void): void {
      closeHandler = handler;
    },
  };
};
