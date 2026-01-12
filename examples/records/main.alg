-- =============================================================================
-- Records: Field Access, Updates, Row Polymorphism, Patterns
-- =============================================================================

-- =============================================================================
-- Basic Records
-- =============================================================================

let testBasicRecords =
  -- Simple record creation
  let point = { x = 10, y = 20 } in
  let a = point.x in                        -- 10
  let b = point.y in                        -- 20

  -- Record with different field types
  let person = { name = "Alice", age = 30, active = true } in
  let c = person.age in                     -- 30
  let d = String.length person.name in      -- 5

  -- Single field record
  let wrapper = { value = 100 } in
  let e = wrapper.value in                  -- 100

  a + b + c + d + e                         -- 165

-- =============================================================================
-- Record Updates
-- =============================================================================

let testRecordUpdates =
  let point = { x = 5, y = 10 } in

  -- Single field update
  let p1 = { point | x = 100 } in
  let a = p1.x + p1.y in                    -- 110

  -- Multiple field update
  let p2 = { point | x = 20, y = 30 } in
  let b = p2.x + p2.y in                    -- 50

  -- Chained updates
  let temp = { point | x = 15 } in
  let p3 = { temp | y = 25 } in
  let c = p3.x + p3.y in                    -- 40

  -- Update preserves other fields
  let person = { name = "Bob", age = 25, score = 100 } in
  let older = { person | age = 26 } in
  let d = older.age + older.score in        -- 126

  a + b + c + d                             -- 326

-- =============================================================================
-- Nested Records
-- =============================================================================

let testNestedRecords =
  -- Two levels of nesting
  let rect = { origin = { x = 0, y = 0 }, size = { w = 100, h = 50 } } in
  let a = rect.origin.x + rect.origin.y in  -- 0
  let b = rect.size.w + rect.size.h in      -- 150

  -- Three levels
  let nested = { a = { b = { c = 42 } } } in
  let c = nested.a.b.c in                   -- 42

  -- Update nested field
  let moved = { rect | origin = { x = 10, y = 20 } } in
  let d = moved.origin.x + moved.origin.y in  -- 30

  -- Mixed nesting
  let data = { info = { point = { x = 5, y = 5 }, label = "test" }, count = 3 } in
  let e = data.info.point.x + data.info.point.y + data.count in  -- 13

  a + b + c + d + e                         -- 235

-- =============================================================================
-- Records in Functions
-- =============================================================================

let testRecordFunctions =
  -- Function taking record
  let getX = p -> p.x in
  let a = getX { x = 42, y = 10 } in        -- 42

  -- Function returning record
  let makePoint = x y -> { x = x, y = y } in
  let p = makePoint 15 25 in
  let b = p.x + p.y in                      -- 40

  -- Record transformation
  let scale = factor p -> { x = p.x * factor, y = p.y * factor } in
  let scaled = scale 3 { x = 4, y = 5 } in
  let c = scaled.x + scaled.y in            -- 27

  -- Distance calculation (simplified)
  let distanceSquared = p1 p2 ->
    let dx = p2.x - p1.x in
    let dy = p2.y - p1.y in
    dx * dx + dy * dy
  in
  let d = distanceSquared { x = 0, y = 0 } { x = 3, y = 4 } in  -- 25

  -- Move point
  let move = dx dy p -> { x = p.x + dx, y = p.y + dy } in
  let moved = move 10 20 { x = 5, y = 5 } in
  let e = moved.x + moved.y in              -- 40

  a + b + c + d + e                         -- 174

-- =============================================================================
-- Row Polymorphism
-- =============================================================================

let testRowPolymorphism =
  -- Function works with any record having 'x' field
  let getX = r -> r.x in
  let a = getX { x = 10 } in                -- 10
  let b = getX { x = 20, y = 30 } in        -- 20
  let c = getX { x = 30, y = 40, z = 50 } in -- 30

  -- Function works with any record having 'x' and 'y' fields
  let sumXY = r -> r.x + r.y in
  let d = sumXY { x = 5, y = 10 } in        -- 15
  let e = sumXY { x = 1, y = 2, z = 3 } in  -- 3

  -- Record update (modify existing field)
  let updateX = val r -> { r | x = val } in
  let updated = updateX 100 { x = 1, y = 2 } in
  let f = updated.x in                      -- 100

  a + b + c + d + e + f                     -- 178

-- =============================================================================
-- Records with ADTs
-- =============================================================================

type Status = Active | Inactive | Pending int

let testRecordsWithADTs =
  -- Record containing ADT
  let user1 = { name = "Alice", status = Active } in
  let user2 = { name = "Bob", status = Pending 5 } in

  let statusValue = s ->
    match s
      when Active -> 10
      when Inactive -> 0
      when Pending n -> n
    end
  in
  let a = statusValue user1.status in       -- 10
  let b = statusValue user2.status in       -- 5

  -- ADT containing record
  let result1 = Just { value = 100, label = "ok" } in
  let c = match result1
    when Nothing -> 0
    when Just r -> r.value
  end in                                     -- 100

  -- List of records
  let users = [{ id = 1, score = 50 }, { id = 2, score = 75 }, { id = 3, score = 100 }] in
  let d = List.foldl (acc u -> acc + u.score) 0 users in  -- 225

  a + b + c + d                             -- 340

-- =============================================================================
-- Record Pattern Matching
-- =============================================================================

let testRecordPatterns =
  -- Simple record pattern
  let sumPoint = p ->
    match p
      when { x = x, y = y } -> x + y
    end
  in
  let a = sumPoint { x = 10, y = 20 } in    -- 30

  -- Partial record pattern
  let getX_ = p ->
    match p
      when { x = x } -> x
    end
  in
  let b = getX_ { x = 42, y = 100 } in      -- 42

  -- Nested record pattern
  let getInnerX = r ->
    match r
      when { inner = { x = x } } -> x
    end
  in
  let c = getInnerX { inner = { x = 99, y = 1 }, other = "test" } in  -- 99

  -- Record pattern in tuple
  let processData = data ->
    match data
      when (n, { value = v }) -> n + v
    end
  in
  let d = processData (10, { value = 50 }) in  -- 60

  a + b + c + d                             -- 231

-- =============================================================================
-- Records as Data Structures
-- =============================================================================

let testRecordDataStructures =
  -- Stack using record
  let emptyStack = { items = Nil, size = 0 } in
  let push = x s -> { items = x :: s.items, size = s.size + 1 } in
  let pop = s ->
    match s.items
      when Nil -> (Nothing, s)
      when Cons x rest -> (Just x, { items = rest, size = s.size - 1 })
    end
  in
  let s1 = push 10 emptyStack in
  let s2 = push 20 s1 in
  let s3 = push 30 s2 in
  let a = s3.size in                        -- 3
  let (top, _) = pop s3 in
  let b = match top when Just x -> x when Nothing -> 0 end in  -- 30

  -- Counter with increment/decrement
  let makeCounter = initial -> { count = initial } in
  let inc = c -> { count = c.count + 1 } in
  let dec = c -> { count = c.count - 1 } in
  let counter = makeCounter 0 in
  let c1 = inc counter in
  let c2 = inc c1 in
  let c3 = dec c2 in
  let c = c3.count in                       -- 1

  -- Config record
  let defaultConfig = { debug = false, maxItems = 100, timeout = 30 } in
  let customConfig = { defaultConfig | debug = true, maxItems = 50 } in
  let d = customConfig.maxItems + customConfig.timeout in  -- 80

  a + b + c + d                             -- 114

-- =============================================================================
-- Complex Record Operations
-- =============================================================================

let testComplexRecordOps =
  -- Merge two points (midpoint)
  let midpoint = p1 p2 -> { x = Int.div (p1.x + p2.x) 2, y = Int.div (p1.y + p2.y) 2 } in
  let mid = midpoint { x = 0, y = 0 } { x = 10, y = 20 } in
  let a = mid.x + mid.y in                  -- 15

  -- Rectangle operations
  let makeRect = x y w h -> { origin = { x = x, y = y }, size = { w = w, h = h } } in
  let area = rect -> rect.size.w * rect.size.h in
  let perimeter = rect -> 2 * (rect.size.w + rect.size.h) in

  let rect = makeRect 5 10 20 15 in
  let b = area rect in                      -- 300
  let c = perimeter rect in                 -- 70

  -- Contains point (simplified)
  let inRangeX = rect p -> p.x >= rect.origin.x && p.x < rect.origin.x + rect.size.w in
  let inRangeY = rect p -> p.y >= rect.origin.y && p.y < rect.origin.y + rect.size.h in
  let contains = rect p -> if inRangeX rect p then inRangeY rect p else false in
  let d = if contains rect { x = 10, y = 15 } then 1 else 0 in  -- 1
  let e = if contains rect { x = 0, y = 0 } then 1 else 0 in    -- 0

  a + b + c + d + e                         -- 386

-- =============================================================================
-- Final Result
-- =============================================================================

let result =
  testBasicRecords +                        -- 165
  testRecordUpdates +                       -- 326
  testNestedRecords +                       -- 235
  testRecordFunctions +                     -- 174
  testRowPolymorphism +                     -- 178
  testRecordsWithADTs +                     -- 340
  testRecordPatterns +                      -- 231
  testRecordDataStructures +                -- 114
  testComplexRecordOps                      -- 386
                                            -- Total: 2149

result
