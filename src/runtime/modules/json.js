// Json module - cross-platform
// JsonValue tags: JsonNull=0, JsonBool=1, JsonNumber=2, JsonString=3, JsonArray=4, JsonObject=5
// Either tags: Left=0, Right=1

// Convert JS value to JsonValue
const $jsToJson = (v) => {
  if (v === null) return [0]; // JsonNull
  if (typeof v === "boolean") return [1, v]; // JsonBool
  if (typeof v === "number") return [2, v]; // JsonNumber
  if (typeof v === "string") return [3, v]; // JsonString
  if (Array.isArray(v)) {
    // JsonArray - convert JS array to Algow List of JsonValue
    let list = null;
    for (let i = v.length - 1; i >= 0; i--) {
      list = { h: $jsToJson(v[i]), t: list };
    }
    return [4, list];
  }
  if (typeof v === "object") {
    // JsonObject - convert JS object to Algow List of (string, JsonValue) tuples
    const entries = Object.entries(v);
    let list = null;
    for (let i = entries.length - 1; i >= 0; i--) {
      const [key, val] = entries[i];
      list = { h: [key, $jsToJson(val)], t: list };
    }
    return [5, list];
  }
  return [0]; // Fallback to JsonNull for undefined, functions, etc.
};

// Convert Algow value to JS value for JSON.stringify
const $algowToJs = (v) => {
  // null = Nil or Nothing
  if (v === null) return null;

  // Primitives
  if (typeof v !== "object") return v;

  // List (Cons cells with h/t fields)
  if ("h" in v && "t" in v) {
    const arr = [];
    let cur = v;
    while (cur !== null) {
      arr.push($algowToJs(cur.h));
      cur = cur.t;
    }
    return arr;
  }

  // ADT (array with numeric tag at index 0)
  if (Array.isArray(v)) {
    // Tuples and ADTs are both arrays
    // For ADTs, first element is numeric tag
    if (typeof v[0] === "number" && Number.isInteger(v[0])) {
      // ADT: encode as { _tag: number, ...fields }
      const obj = { _tag: v[0] };
      for (let i = 1; i < v.length; i++) {
        obj["_" + (i - 1)] = $algowToJs(v[i]);
      }
      return obj;
    }
    // Tuple: encode as array
    return v.map($algowToJs);
  }

  // Record: plain object
  const obj = {};
  for (const key of Object.keys(v)) {
    obj[key] = $algowToJs(v[key]);
  }
  return obj;
};

$foreign.Json = {
  decode: (s) => {
    try {
      const parsed = JSON.parse(s);
      return [1, $jsToJson(parsed)]; // Right JsonValue
    } catch (e) {
      return [0, e.message]; // Left string
    }
  },
  encode: (v) => JSON.stringify($algowToJs(v)),
};
