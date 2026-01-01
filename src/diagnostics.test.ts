import { describe, expect, it } from "bun:test";
import { error, warning, info } from "./diagnostics";

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
});
