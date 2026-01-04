/**
 * Playground Worker Entry Point.
 *
 * This file is bundled for the browser and runs the LSP server
 * in a web worker context.
 */

import { createServer } from "../src/lsp/server";
import { createWorkerTransport } from "../src/lsp/worker";

const transport = createWorkerTransport();
createServer(transport);
