-- Testing utilities

type TestResult
  = TestPass string
  | TestFail string string

module Test
  foreign equal : string -> a -> a -> TestResult
  foreign notEqual : string -> a -> a -> TestResult
  foreign ok : string -> bool -> TestResult
  foreign fail : string -> TestResult
  foreign run : List TestResult -> unit
end
