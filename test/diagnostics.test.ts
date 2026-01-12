import { describe, expect, it } from "bun:test";
import {
  error,
  warning,
  info,
  typeMismatch,
  unboundVariable,
  levenshteinDistance,
  findSimilarNames,
} from "../src/diagnostics";

describe("Diagnostics", () => {
  describe("error", () => {
    it("creates error diagnostic", () => {
      const diag = error(0, 10, "test error");
      expect(diag).toEqual({
        start: 0,
        end: 10,
        message: "test error",
        severity: "error",
      });
    });

    it("preserves position information", () => {
      const diag = error(42, 100, "message");
      expect(diag.start).toBe(42);
      expect(diag.end).toBe(100);
    });
  });

  describe("warning", () => {
    it("creates warning diagnostic", () => {
      const diag = warning(5, 15, "test warning");
      expect(diag).toEqual({
        start: 5,
        end: 15,
        message: "test warning",
        severity: "warning",
      });
    });

    it("has warning severity", () => {
      const diag = warning(0, 1, "msg");
      expect(diag.severity).toBe("warning");
    });
  });

  describe("info", () => {
    it("creates info diagnostic", () => {
      const diag = info(20, 30, "test info");
      expect(diag).toEqual({
        start: 20,
        end: 30,
        message: "test info",
        severity: "info",
      });
    });

    it("has info severity", () => {
      const diag = info(0, 1, "msg");
      expect(diag.severity).toBe("info");
    });
  });

  describe("typeMismatch", () => {
    it("creates type mismatch diagnostic", () => {
      const diag = typeMismatch(0, 10, "Int", "String");
      expect(diag.severity).toBe("error");
      expect(diag.kind).toBe("type-mismatch");
      expect(diag.expected).toBe("Int");
      expect(diag.actual).toBe("String");
      expect(diag.message).toBe("Type mismatch: expected 'Int', got 'String'");
    });

    it("includes context in message when provided", () => {
      const diag = typeMismatch(0, 10, "Int", "String", "function argument");
      expect(diag.message).toBe("Type mismatch in function argument: expected 'Int', got 'String'");
    });
  });

  describe("unboundVariable", () => {
    it("creates unbound variable diagnostic", () => {
      const diag = unboundVariable(0, 5, "foo");
      expect(diag.severity).toBe("error");
      expect(diag.kind).toBe("unbound-variable");
      expect(diag.message).toBe("Unknown variable: foo");
    });

    it("includes suggestions when provided", () => {
      const diag = unboundVariable(0, 5, "fo", ["foo", "for"]);
      expect(diag.suggestions).toEqual(["foo", "for"]);
    });
  });

  describe("levenshteinDistance", () => {
    it("returns 0 for identical strings", () => {
      expect(levenshteinDistance("hello", "hello")).toBe(0);
    });

    it("returns string length when other is empty", () => {
      expect(levenshteinDistance("hello", "")).toBe(5);
      expect(levenshteinDistance("", "world")).toBe(5);
    });

    it("returns 0 for two empty strings", () => {
      expect(levenshteinDistance("", "")).toBe(0);
    });

    it("calculates single character difference", () => {
      expect(levenshteinDistance("cat", "bat")).toBe(1);
      expect(levenshteinDistance("cat", "car")).toBe(1);
      expect(levenshteinDistance("cat", "cats")).toBe(1);
    });

    it("calculates multiple character differences", () => {
      expect(levenshteinDistance("kitten", "sitting")).toBe(3);
      expect(levenshteinDistance("saturday", "sunday")).toBe(3);
    });

    it("handles insertions", () => {
      expect(levenshteinDistance("abc", "abcd")).toBe(1);
      expect(levenshteinDistance("abc", "abcde")).toBe(2);
    });

    it("handles deletions", () => {
      expect(levenshteinDistance("abcd", "abc")).toBe(1);
      expect(levenshteinDistance("abcde", "abc")).toBe(2);
    });

    it("handles substitutions", () => {
      expect(levenshteinDistance("abc", "axc")).toBe(1);
      expect(levenshteinDistance("abc", "xyz")).toBe(3);
    });
  });

  describe("findSimilarNames", () => {
    it("finds similar names within edit distance", () => {
      const candidates = ["foo", "bar", "baz", "foobar"];
      const suggestions = findSimilarNames("fo", candidates);
      expect(suggestions).toContain("foo");
    });

    it("excludes exact matches", () => {
      const candidates = ["foo", "bar", "baz"];
      const suggestions = findSimilarNames("foo", candidates);
      expect(suggestions).not.toContain("foo");
    });

    it("returns empty array when no similar names", () => {
      const candidates = ["xyz", "abc", "def"];
      const suggestions = findSimilarNames("foo", candidates);
      expect(suggestions).toEqual([]);
    });

    it("limits number of suggestions", () => {
      const candidates = ["fo", "foo", "fooo", "foooo", "fooooo"];
      const suggestions = findSimilarNames("f", candidates, 5, 2);
      expect(suggestions.length).toBeLessThanOrEqual(2);
    });

    it("respects max distance parameter", () => {
      const candidates = ["foo", "foobar"];
      const nearSuggestions = findSimilarNames("fo", candidates, 1);
      const farSuggestions = findSimilarNames("fo", candidates, 5);
      expect(nearSuggestions).toContain("foo");
      expect(nearSuggestions).not.toContain("foobar");
      expect(farSuggestions).toContain("foobar");
    });

    it("sorts by distance (closest first)", () => {
      const candidates = ["foooo", "fooo", "foo"];
      const suggestions = findSimilarNames("fo", candidates, 3);
      expect(suggestions[0]).toBe("foo");
    });

    it("is case insensitive", () => {
      const candidates = ["FOO", "Bar", "BAZ"];
      const suggestions = findSimilarNames("foo", candidates, 1);
      expect(suggestions).toContain("FOO");
    });
  });
});
