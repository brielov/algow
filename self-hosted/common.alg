-- =============================================================================
-- COMMON TYPES
-- =============================================================================
--
-- Shared types used across all compiler phases.
--
-- =============================================================================

module Common

  -- Source location (byte offsets)
  type Span = { start : int, stop : int }

  -- Wrap any value with source location
  type Located a = { value : a, span : Span }

  -- Literal values
  type Literal
    = LInt int
    | LFloat float
    | LString string
    | LChar char
    | LBool bool

  -- Standard result type
  type Result a e = Ok a | Err e

  -- Diagnostic severity
  type Severity = Error | Warning | Info

  -- Compiler diagnostic
  type Diagnostic = {
    message : string,
    span : Span,
    severity : Severity
  }

end
