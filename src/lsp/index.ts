/**
 * Algow LSP Entry Point
 *
 * Sets up stdio transport and starts the LSP server.
 */

import { createMessageReader, feedData } from "./protocol";
import { createServer, handleMessage } from "./server";

// Create server with stdout writer
const server = createServer((data: Buffer) => {
  process.stdout.write(data);
});

// Create message reader
const reader = createMessageReader();

// Handle stdin data
process.stdin.on("data", (chunk: Buffer) => {
  const messages = feedData(reader, chunk);
  for (const message of messages) {
    handleMessage(server, message);
  }
});

// Handle stdin close
process.stdin.on("end", () => {
  process.exit(0);
});

// Prevent unhandled rejection crashes
process.on("unhandledRejection", (error) => {
  console.error("Unhandled rejection:", error);
});
