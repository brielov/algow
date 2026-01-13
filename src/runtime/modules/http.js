// Http module - cross-platform (uses fetch API)
// HttpError tags: NetworkError=0, TimeoutError=1, InvalidUrl=2, HttpStatusError=3
// Either tags: Left=0, Right=1

// Convert Headers to Algow List (string, string)
const $headersToList = (headers) => {
  let list = null;
  const entries = [...headers.entries()].reverse();
  for (const [key, value] of entries) {
    list = { h: [key, value], t: list };
  }
  return list;
};

// Make HTTP request
const $httpRequest = async (url, options = {}) => {
  try {
    const response = await fetch(url, options);
    const body = await response.text();
    const headers = $headersToList(response.headers);
    if (!response.ok) {
      // Left HttpStatusError (tag 3)
      return [0, [3, response.status, body]];
    }
    // Right response record
    return [1, { status: response.status, body, headers }];
  } catch (err) {
    if (err.name === "TypeError" && err.message.includes("URL")) {
      // Left InvalidUrl (tag 2)
      return [0, [2, url]];
    }
    if (err.name === "TimeoutError" || err.message.includes("timeout")) {
      // Left TimeoutError (tag 1)
      return [0, [1]];
    }
    // Left NetworkError (tag 0)
    return [0, [0, err.message]];
  }
};

$foreign.Http = {
  get: (url) => $httpRequest(url),
  post: (url) => (body) =>
    $httpRequest(url, {
      method: "POST",
      body,
      headers: { "Content-Type": "text/plain" },
    }),
  put: (url) => (body) =>
    $httpRequest(url, {
      method: "PUT",
      body,
      headers: { "Content-Type": "text/plain" },
    }),
  delete: (url) => $httpRequest(url, { method: "DELETE" }),
};
