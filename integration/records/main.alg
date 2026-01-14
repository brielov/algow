-- Records: Field Access, Updates, Row Polymorphism, Patterns
type Status
  = Active
  | Inactive
  | Pending int

let tests =
  let point = { x = 10, y = 20 } in
  let person = { name = "Alice", age = 30, active = true } in
  let wrapper = { value = 100 } in
  let p1 = { point | x = 100 } in
  let p2 = { point | x = 20, y = 30 } in
  let person2 = { name = "Bob", age = 25, score = 100 } in
  let older = { person2 | age = 26 } in
  let rect = { origin = { x = 0, y = 0 }, size = { w = 100, h = 50 } } in
  let nested = { a = { b = { c = 42 } } } in
  let moved = { rect | origin = { x = 10, y = 20 } } in
  let makePoint = x y -> { x = x, y = y } in
  let p = makePoint 15 25 in
  let scale = factor pt -> { x = pt.x * factor, y = pt.y * factor } in
  let scaled = scale 3 { x = 4, y = 5 } in
  let distanceSquared = p1 p2 -> let dx = p2.x - p1.x in
  let dy = p2.y - p1.y in
  dx * dx + dy * dy in
  let getX = r -> r.x in
  let sumXY = r -> r.x + r.y in
  let updateX = val r -> { r | x = val } in
  let user1 = { name = "Alice", status = Active } in
  let user2 = { name = "Bob", status = Pending 5 } in
  let statusValue = s ->
    match s
      when Active -> 10
      when Inactive -> 0
      when Pending n -> n
    end in
  let result1 = Just { value = 100, label = "ok" } in
  let users = [{ id = 1, score = 50 }, { id = 2, score = 75 }, { id = 3, score = 100 }] in
  let sumPoint = pt ->
    match pt
      when { x, y } -> x + y
    end in
  let getX_ = pt ->
    match pt
      when { x } -> x
    end in
  let getInnerX = r ->
    match r
      when { inner = { x } } -> x
    end in
  let processData = data ->
    match data
      when (n, { value = v }) -> n + v
    end in
  let emptyStack = { items = Nil, size = 0 } in
  let push = x s -> { items = x :: s.items, size = s.size + 1 } in
  let pop = s ->
    match s.items
      when Nil -> (Nothing, s)
      when Cons x rest -> (Just x, { items = rest, size = s.size - 1 })
    end in
  let s1 = push 10 emptyStack in
  let s2 = push 20 s1 in
  let s3 = push 30 s2 in
  match pop s3
    when (top, _) -> let defaultConfig = { debug = false, maxItems = 100, timeout = 30 } in
    let customConfig = { defaultConfig | debug = true, maxItems = 50 } in
    let midpoint = pt1 pt2 -> { x = Int.div (pt1.x + pt2.x) 2, y = Int.div (pt1.y + pt2.y) 2 } in
    let mid = midpoint { x = 0, y = 0 } { x = 10, y = 20 } in
    let makeRect = x y w h -> { origin = { x = x, y = y }, size = { w = w, h = h } } in
    let area = r -> (r.size).w * (r.size).h in
    let perimeter = r -> 2 * ((r.size).w + (r.size).h) in
    let rect2 = makeRect 5 10 20 15 in
    [ Test.equal "x" 10 point.x
      , Test.equal "y" 20 point.y
      , Test.equal "age" 30 person.age
      , Test.equal "name length" 5 (String.length person.name)
      , Test.equal "wrapper value" 100 wrapper.value
      , Test.equal "single update" 110 (p1.x + p1.y)
      , Test.equal "multi update" 50 (p2.x + p2.y)
      , Test.equal "update preserves" 26 older.age
      , Test.equal "other fields" 100 older.score
      , Test.equal "origin" 0 ((rect.origin).x + (rect.origin).y)
      , Test.equal "size" 150 ((rect.size).w + (rect.size).h)
      , Test.equal "three levels" 42 ((nested.a).b).c
      , Test.equal "update nested" 30 ((moved.origin).x + (moved.origin).y)
      , Test.equal "getX" 42 (getX { x = 42, y = 10 })
      , Test.equal "makePoint" 40 (p.x + p.y)
      , Test.equal "scale" 27 (scaled.x + scaled.y)
      , Test.equal "distance" 25 (distanceSquared { x = 0, y = 0 } { x = 3, y = 4 })
      , Test.equal "one field" 10 (getX { x = 10 })
      , Test.equal "two fields" 20 (getX { x = 20, y = 30 })
      , Test.equal "three fields" 30 (getX { x = 30, y = 40, z = 50 })
      , Test.equal "sumXY 2" 15 (sumXY { x = 5, y = 10 })
      , Test.equal "sumXY 3" 3 (sumXY { x = 1, y = 2, z = 3 })
      , Test.equal "updateX" 100 (updateX 100 { x = 1, y = 2 }).x
      , Test.equal "Active" 10 (statusValue user1.status)
      , Test.equal "Pending" 5 (statusValue user2.status)
      , Test.equal "ADT with record" 100 (match result1
        when Nothing -> 0
        when Just r -> r.value
      end)
      , Test.equal "list of records" 225 (List.foldl (acc u -> acc + u.score) 0 users)
      , Test.equal "sumPoint" 30 (sumPoint { x = 10, y = 20 })
      , Test.equal "partial pattern" 42 (getX_ { x = 42, y = 100 })
      , Test.equal "nested pattern" 99 (getInnerX { inner = { x = 99, y = 1 }, other = "test" })
      , Test.equal "tuple with record" 60 (processData (10, { value = 50 }))
      , Test.equal "stack size" 3 s3.size
      , Test.equal "stack pop" (Just 30) top
      , Test.equal "config" 80 (customConfig.maxItems + customConfig.timeout)
      , Test.equal "midpoint" 15 (mid.x + mid.y)
      , Test.equal "area" 300 (area rect2)
      , Test.equal "perimeter" 70 (perimeter rect2)
      ]
  end

let main =
  args -> Test.run tests
