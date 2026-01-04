/**
 * Playground Dev Server.
 *
 * Simple Bun server for developing the playground.
 * Serves static files and handles live reload.
 */

const MIME_TYPES: Record<string, string> = {
  ".html": "text/html",
  ".js": "application/javascript",
  ".ts": "application/javascript",
  ".css": "text/css",
  ".json": "application/json",
};

const getMimeType = (path: string): string => {
  const ext = path.slice(path.lastIndexOf("."));
  return MIME_TYPES[ext] ?? "application/octet-stream";
};

const server = Bun.serve({
  port: 3000,

  async fetch(req) {
    const url = new URL(req.url);
    let path = url.pathname;

    // Default to index.html
    if (path === "/") {
      path = "/index.html";
    }

    // Try to serve from playground directory
    const filePath = `./playground${path}`;
    const file = Bun.file(filePath);

    if (await file.exists()) {
      return new Response(file, {
        headers: { "Content-Type": getMimeType(path) },
      });
    }

    return new Response("Not found", { status: 404 });
  },
});

console.log(`Playground running at http://localhost:${server.port}`);
console.log("Press Ctrl+C to stop");
