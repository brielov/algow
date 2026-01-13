-- Date and time utilities
module Duration
  foreign zero : Duration
  foreign millis : int -> Duration
  foreign seconds : int -> Duration
  foreign minutes : int -> Duration
  foreign hours : int -> Duration
  foreign days : int -> Duration
  foreign weeks : int -> Duration
  foreign add : Duration -> Duration -> Duration
  foreign negate : Duration -> Duration
  foreign toMillis : Duration -> int
  foreign toIso : Duration -> string
  foreign fromIso : string -> Maybe Duration
end

module Date
  foreign make : int -> int -> int -> Maybe Date
  foreign year : Date -> int
  foreign month : Date -> int
  foreign day : Date -> int
  foreign dayOfWeek : Date -> int
  foreign dayOfYear : Date -> int
  foreign isLeapYear : Date -> bool
  foreign addDays : int -> Date -> Date
  foreign addMonths : int -> Date -> Date
  foreign addYears : int -> Date -> Date
  foreign diffDays : Date -> Date -> int
  foreign startOfMonth : Date -> Date
  foreign endOfMonth : Date -> Date
  foreign startOfYear : Date -> Date
  foreign endOfYear : Date -> Date
  foreign toIso : Date -> string
  foreign fromIso : string -> Maybe Date
  foreign format : string -> Date -> string
  foreign formatMonth : string -> Date -> string
  foreign formatDay : string -> Date -> string
end

module Time
  foreign make : int -> int -> int -> Maybe Time
  foreign hour : Time -> int
  foreign minute : Time -> int
  foreign second : Time -> int
  foreign addSeconds : int -> Time -> Time
  foreign addMinutes : int -> Time -> Time
  foreign addHours : int -> Time -> Time
  foreign toIso : Time -> string
  foreign fromIso : string -> Maybe Time
  foreign format : string -> Time -> string
end

module DateTime
  foreign make : Date -> Time -> DateTime
  foreign date : DateTime -> Date
  foreign time : DateTime -> Time
  foreign now : string -> DateTime
  foreign toTimestamp : string -> DateTime -> int
  foreign fromTimestamp : string -> int -> DateTime
  foreign add : Duration -> DateTime -> DateTime
  foreign sub : Duration -> DateTime -> DateTime
  foreign diff : DateTime -> DateTime -> Duration
  foreign toIso : DateTime -> string
  foreign fromIso : string -> Maybe DateTime
  foreign format : string -> string -> DateTime -> string
end
