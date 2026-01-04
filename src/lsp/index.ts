#!/usr/bin/env bun
/**
 * LSP Server Entry Point (stdio).
 *
 * Run with: bun run src/lsp/index.ts
 * Or via package.json: bun run lsp
 */

import { createServer } from "./server";
import { createStdioTransport } from "./stdio";

const transport = createStdioTransport();
createServer(transport);
