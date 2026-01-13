-- Records: Field Access, Updates, Row Polymorphism, Patterns

let testBasicRecords =
  let point = { x = 10, y = 20 } in
  let a = Assert.equal 10 point.x "x" in
  let b = Assert.equal 20 point.y "y" in
  let person = { name = "Alice", age = 30, active = true } in
  let c = Assert.equal 30 person.age "age" in
  let d = Assert.equal 5 (String.length person.name) "name length" in
  let wrapper = { value = 100 } in
  let e = Assert.equal 100 wrapper.value "wrapper value" in
  ()

let testRecordUpdates =
  let point = { x = 5, y = 10 } in
  let p1 = { point | x = 100 } in
  let a = Assert.equal 110 (p1.x + p1.y) "single update" in
  let p2 = { point | x = 20, y = 30 } in
  let b = Assert.equal 50 (p2.x + p2.y) "multi update" in
  let person = { name = "Bob", age = 25, score = 100 } in
  let older = { person | age = 26 } in
  let c = Assert.equal 26 older.age "update preserves" in
  let d = Assert.equal 100 older.score "other fields" in
  ()

let testNestedRecords =
  let rect = { origin = { x = 0, y = 0 }, size = { w = 100, h = 50 } } in
  let a = Assert.equal 0 (rect.origin.x + rect.origin.y) "origin" in
  let b = Assert.equal 150 (rect.size.w + rect.size.h) "size" in
  let nested = { a = { b = { c = 42 } } } in
  let c = Assert.equal 42 nested.a.b.c "three levels" in
  let moved = { rect | origin = { x = 10, y = 20 } } in
  let d = Assert.equal 30 (moved.origin.x + moved.origin.y) "update nested" in
  ()

let testRecordFunctions =
  let getX = p -> p.x in
  let a = Assert.equal 42 (getX { x = 42, y = 10 }) "getX" in
  let makePoint = x y -> { x = x, y = y } in
  let p = makePoint 15 25 in
  let b = Assert.equal 40 (p.x + p.y) "makePoint" in
  let scale = factor p -> { x = p.x * factor, y = p.y * factor } in
  let scaled = scale 3 { x = 4, y = 5 } in
  let c = Assert.equal 27 (scaled.x + scaled.y) "scale" in
  let distanceSquared = p1 p2 -> let dx = p2.x - p1.x in let dy = p2.y - p1.y in dx * dx + dy * dy in
  let d = Assert.equal 25 (distanceSquared { x = 0, y = 0 } { x = 3, y = 4 }) "distance" in
  ()

let testRowPolymorphism =
  let getX = r -> r.x in
  let a = Assert.equal 10 (getX { x = 10 }) "one field" in
  let b = Assert.equal 20 (getX { x = 20, y = 30 }) "two fields" in
  let c = Assert.equal 30 (getX { x = 30, y = 40, z = 50 }) "three fields" in
  let sumXY = r -> r.x + r.y in
  let d = Assert.equal 15 (sumXY { x = 5, y = 10 }) "sumXY 2" in
  let e = Assert.equal 3 (sumXY { x = 1, y = 2, z = 3 }) "sumXY 3" in
  let updateX = val r -> { r | x = val } in
  let f = Assert.equal 100 ((updateX 100 { x = 1, y = 2 }).x) "updateX" in
  ()

type Status = Active | Inactive | Pending int

let testRecordsWithADTs =
  let user1 = { name = "Alice", status = Active } in
  let user2 = { name = "Bob", status = Pending 5 } in
  let statusValue = s -> match s when Active -> 10 when Inactive -> 0 when Pending n -> n end in
  let a = Assert.equal 10 (statusValue user1.status) "Active" in
  let b = Assert.equal 5 (statusValue user2.status) "Pending" in
  let result1 = Just { value = 100, label = "ok" } in
  let c = Assert.equal 100 (match result1 when Nothing -> 0 when Just r -> r.value end) "ADT with record" in
  let users = [{ id = 1, score = 50 }, { id = 2, score = 75 }, { id = 3, score = 100 }] in
  let d = Assert.equal 225 (List.foldl (acc u -> acc + u.score) 0 users) "list of records" in
  ()

let testRecordPatterns =
  let sumPoint = p -> match p when { x = x, y = y } -> x + y end in
  let a = Assert.equal 30 (sumPoint { x = 10, y = 20 }) "sumPoint" in
  let getX_ = p -> match p when { x = x } -> x end in
  let b = Assert.equal 42 (getX_ { x = 42, y = 100 }) "partial pattern" in
  let getInnerX = r -> match r when { inner = { x = x } } -> x end in
  let c = Assert.equal 99 (getInnerX { inner = { x = 99, y = 1 }, other = "test" }) "nested pattern" in
  let processData = data -> match data when (n, { value = v }) -> n + v end in
  let d = Assert.equal 60 (processData (10, { value = 50 })) "tuple with record" in
  ()

let testRecordDataStructures =
  let emptyStack = { items = Nil, size = 0 } in
  let push = x s -> { items = x :: s.items, size = s.size + 1 } in
  let pop = s -> match s.items when Nil -> (Nothing, s) when Cons x rest -> (Just x, { items = rest, size = s.size - 1 }) end in
  let s1 = push 10 emptyStack in
  let s2 = push 20 s1 in
  let s3 = push 30 s2 in
  let a = Assert.equal 3 s3.size "stack size" in
  let (top, _) = pop s3 in
  let b = Assert.equal (Just 30) top "stack pop" in
  let defaultConfig = { debug = false, maxItems = 100, timeout = 30 } in
  let customConfig = { defaultConfig | debug = true, maxItems = 50 } in
  let c = Assert.equal 80 (customConfig.maxItems + customConfig.timeout) "config" in
  ()

let testComplexRecordOps =
  let midpoint = p1 p2 -> { x = Int.div (p1.x + p2.x) 2, y = Int.div (p1.y + p2.y) 2 } in
  let mid = midpoint { x = 0, y = 0 } { x = 10, y = 20 } in
  let a = Assert.equal 15 (mid.x + mid.y) "midpoint" in
  let makeRect = x y w h -> { origin = { x = x, y = y }, size = { w = w, h = h } } in
  let area = rect -> rect.size.w * rect.size.h in
  let perimeter = rect -> 2 * (rect.size.w + rect.size.h) in
  let rect = makeRect 5 10 20 15 in
  let b = Assert.equal 300 (area rect) "area" in
  let c = Assert.equal 70 (perimeter rect) "perimeter" in
  ()

let main = args ->
  let a = testBasicRecords in
  let b = testRecordUpdates in
  let c = testNestedRecords in
  let d = testRecordFunctions in
  let e = testRowPolymorphism in
  let f = testRecordsWithADTs in
  let g = testRecordPatterns in
  let h = testRecordDataStructures in
  let i = testComplexRecordOps in
  IO.printLine "records: ok"
