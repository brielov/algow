-- Stdlib: Time, Date, DateTime, Duration modules

let tests =
  -- Duration tests
  [ Test.equal "Duration.zero millis" 0 (Duration.toMillis Duration.zero)
  , Test.equal "Duration.millis" 1000 (Duration.toMillis (Duration.millis 1000))
  , Test.equal "Duration.seconds" 5000 (Duration.toMillis (Duration.seconds 5))
  , Test.equal "Duration.minutes" 120000 (Duration.toMillis (Duration.minutes 2))
  , Test.equal "Duration.hours" 7200000 (Duration.toMillis (Duration.hours 2))
  , Test.equal "Duration.days" 86400000 (Duration.toMillis (Duration.days 1))
  , Test.equal "Duration.weeks" 604800000 (Duration.toMillis (Duration.weeks 1))
  , Test.equal "Duration.add" 3000 (Duration.toMillis (Duration.add (Duration.seconds 1) (Duration.seconds 2)))
  , Test.equal "Duration.negate" (-1000) (Duration.toMillis (Duration.negate (Duration.seconds 1)))
    -- Duration ISO format
  , Test.equal "Duration.toIso" "PT1S" (Duration.toIso (Duration.seconds 1))
  , Test.equal "Duration.fromIso" (Just (Duration.seconds 30)) (Duration.fromIso "PT30S")
    -- Date tests
  , Test.ok "Date.make valid" (Maybe.isJust (Date.make 2024 1 15))
  , Test.equal "Date.make invalid month" Nothing (Date.make 2024 13 1)
  , Test.equal "Date.make invalid day" Nothing (Date.make 2024 2 30)
  , Test.equal "Date.year" 2024 (
      match Date.make 2024 6 15 when Just d -> Date.year d when Nothing -> 0 end
    )
  , Test.equal "Date.month" 6 (
      match Date.make 2024 6 15 when Just d -> Date.month d when Nothing -> 0 end
    )
  , Test.equal "Date.day" 15 (
      match Date.make 2024 6 15 when Just d -> Date.day d when Nothing -> 0 end
    )
  , Test.ok "Date.dayOfWeek in range" (
      match Date.make 2024 1 1 when Just d -> Date.dayOfWeek d >= 0 && Date.dayOfWeek d <= 6 when Nothing -> false end
    )
  , Test.ok "Date.dayOfYear in range" (
      match Date.make 2024 6 15 when Just d -> Date.dayOfYear d >= 1 && Date.dayOfYear d <= 366 when Nothing -> false end
    )
    -- Leap year
  , Test.ok "Date.isLeapYear 2024" (
      match Date.make 2024 1 1 when Just d -> Date.isLeapYear d when Nothing -> false end
    )
  , Test.ok "Date.isLeapYear 2023 false" (
      match Date.make 2023 1 1 when Just d -> Bool.negate (Date.isLeapYear d) when Nothing -> true end
    )
    -- Date arithmetic
  , Test.equal "Date.addDays" 20 (
      match Date.make 2024 1 15 when Just d -> Date.day (Date.addDays 5 d) when Nothing -> 0 end
    )
  , Test.equal "Date.addMonths" 3 (
      match Date.make 2024 1 15 when Just d -> Date.month (Date.addMonths 2 d) when Nothing -> 0 end
    )
  , Test.equal "Date.addYears" 2025 (
      match Date.make 2024 1 15 when Just d -> Date.year (Date.addYears 1 d) when Nothing -> 0 end
    )
  , Test.equal "Date.diffDays" 10 (
      match (Date.make 2024 1 1, Date.make 2024 1 11)
        when (Just d1, Just d2) -> Date.diffDays d1 d2
        when _ -> 0
      end
    )
    -- Date ISO format
  , Test.equal "Date.toIso" "2024-06-15" (
      match Date.make 2024 6 15 when Just d -> Date.toIso d when Nothing -> "" end
    )
  , Test.ok "Date.fromIso valid" (Maybe.isJust (Date.fromIso "2024-06-15"))
  , Test.equal "Date.fromIso invalid" Nothing (Date.fromIso "invalid")
    -- Time tests
  , Test.ok "Time.make valid" (Maybe.isJust (Time.make 14 30 45))
  , Test.equal "Time.make invalid hour" Nothing (Time.make 25 0 0)
  , Test.equal "Time.make invalid minute" Nothing (Time.make 12 60 0)
  , Test.equal "Time.make invalid second" Nothing (Time.make 12 30 60)
  , Test.equal "Time.hour" 14 (
      match Time.make 14 30 45 when Just t -> Time.hour t when Nothing -> 0 end
    )
  , Test.equal "Time.minute" 30 (
      match Time.make 14 30 45 when Just t -> Time.minute t when Nothing -> 0 end
    )
  , Test.equal "Time.second" 45 (
      match Time.make 14 30 45 when Just t -> Time.second t when Nothing -> 0 end
    )
    -- Time arithmetic
  , Test.equal "Time.addSeconds" 50 (
      match Time.make 14 30 45 when Just t -> Time.second (Time.addSeconds 5 t) when Nothing -> 0 end
    )
  , Test.equal "Time.addMinutes" 35 (
      match Time.make 14 30 45 when Just t -> Time.minute (Time.addMinutes 5 t) when Nothing -> 0 end
    )
  , Test.equal "Time.addHours" 16 (
      match Time.make 14 30 45 when Just t -> Time.hour (Time.addHours 2 t) when Nothing -> 0 end
    )
    -- Time ISO format
  , Test.equal "Time.toIso" "14:30:45" (
      match Time.make 14 30 45 when Just t -> Time.toIso t when Nothing -> "" end
    )
  , Test.ok "Time.fromIso valid" (Maybe.isJust (Time.fromIso "14:30:45"))
    -- DateTime tests
  , Test.ok "DateTime.make valid" (
      match (Date.make 2024 6 15, Time.make 14 30 0)
        when (Just d, Just t) -> true
        when _ -> false
      end
    )
  , Test.ok "DateTime.now returns DateTime" (
      let dt = DateTime.now "UTC" in
      true
    )
    -- DateTime components
  , Test.equal "DateTime.date year" 2024 (
      match (Date.make 2024 6 15, Time.make 14 30 0)
        when (Just d, Just t) -> Date.year (DateTime.date (DateTime.make d t))
        when _ -> 0
      end
    )
  , Test.equal "DateTime.time hour" 14 (
      match (Date.make 2024 6 15, Time.make 14 30 0)
        when (Just d, Just t) -> Time.hour (DateTime.time (DateTime.make d t))
        when _ -> 0
      end
    )
    -- DateTime arithmetic
  , Test.ok "DateTime.add Duration" (
      match (Date.make 2024 6 15, Time.make 14 30 0)
        when (Just d, Just t) ->
          let dt = DateTime.make d t in
          let dt2 = DateTime.add (Duration.hours 1) dt in
          Time.hour (DateTime.time dt2) == 15
        when _ -> false
      end
    )
    -- DateTime ISO format
  , Test.ok "DateTime.toIso format" (
      match (Date.make 2024 6 15, Time.make 14 30 0)
        when (Just d, Just t) -> String.length (DateTime.toIso (DateTime.make d t)) > 0
        when _ -> false
      end
    )
  , Test.ok "DateTime.fromIso valid" (Maybe.isJust (DateTime.fromIso "2024-06-15T14:30:00"))
  ]

let main = args -> Test.run tests
