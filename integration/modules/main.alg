-- Modules: Namespacing, Organization, Module Access
module Math
  let abs =
    n -> if n < 0
      then 0 - n
      else n
  let sign =
    n -> if n < 0
      then 0 - 1
      else if n > 0
        then 1
        else 0
  let min =
    a b -> if a < b
      then a
      else b
  let max =
    a b -> if a > b
      then a
      else b
  let clamp =
    lo hi x -> max lo (min hi x)
  let rec gcd = a b -> if b == 0
    then abs a
    else gcd b (Int.mod a b)
  let lcm =
    a b -> let g = gcd a b in
    if g == 0
      then 0
      else Int.div (abs (a * b)) g
  let rec pow = base exp -> if exp == 0
    then 1
    else base * pow base (exp - 1)
end

module Vec2
  type Vec2 = Vec2 int int
  let make =
    a b -> Vec2 a b
  let zero =
    Vec2 0 0
  let getX =
    v ->
      match v
        when Vec2 a b -> a
      end
  let getY =
    v ->
      match v
        when Vec2 a b -> b
      end
  let add =
    v1 v2 ->
      match (v1, v2)
        when (Vec2 x1 y1, Vec2 x2 y2) -> Vec2 (x1 + x2) (y1 + y2)
      end
  let scale =
    n v ->
      match v
        when Vec2 a b -> Vec2 (n * a) (n * b)
      end
  let dot =
    v1 v2 ->
      match (v1, v2)
        when (Vec2 x1 y1, Vec2 x2 y2) -> x1 * x2 + y1 * y2
      end
  let lengthSquared =
    v ->
      match v
        when Vec2 a b -> a * a + b * b
      end
end

module Stack
  type Stack a = Stack (List a)
  let empty =
    Stack Nil
  let isEmpty =
    s ->
      match s
        when Stack Nil -> true
        when _ -> false
      end
  let push =
    x s ->
      match s
        when Stack xs -> Stack (Cons x xs)
      end
  let pop =
    s ->
      match s
        when Stack Nil -> Nothing
        when Stack (Cons x rest) -> Just (x, Stack rest)
      end
  let peek =
    s ->
      match s
        when Stack Nil -> Nothing
        when Stack (Cons x rest) -> Just x
      end
  let size =
    s ->
      match s
        when Stack xs -> List.length xs
      end
end

module Queue
  type Queue a = Queue (List a) (List a)
  let empty =
    Queue Nil Nil
  let isEmpty =
    q ->
      match q
        when Queue Nil Nil -> true
        when _ -> false
      end
  let enqueue =
    x q ->
      match q
        when Queue front back -> Queue front (Cons x back)
      end
  let rec dequeue = q ->
    match q
      when Queue Nil Nil -> Nothing
      when Queue Nil back -> dequeue (Queue (List.reverse back) Nil)
      when Queue (Cons x rest) back -> Just (x, Queue rest back)
    end
  let size =
    q ->
      match q
        when Queue front back -> List.length front + List.length back
      end
end

module Counter
  type Counter = Counter int
  let make =
    n -> Counter n
  let zero =
    Counter 0
  let value =
    c ->
      match c
        when Counter n -> n
      end
  let increment =
    c ->
      match c
        when Counter n -> Counter (n + 1)
      end
  let decrement =
    c ->
      match c
        when Counter n -> Counter (n - 1)
      end
  let add =
    n c ->
      match c
        when Counter m -> Counter (m + n)
      end
end

let tests =
  let v1 = Vec2.make 3 4 in
  let v2 = Vec2.make 1 2 in
  let v3 = Vec2.add v1 v2 in
  let v4 = Vec2.scale 2 v1 in
  let s0 = Stack.empty in
  let s1 = Stack.push 1 s0 in
  let s2 = Stack.push 2 s1 in
  let s3 = Stack.push 3 s2 in
  let q0 = Queue.empty in
  let q1 = Queue.enqueue 1 q0 in
  let q2 = Queue.enqueue 2 q1 in
  let q3 = Queue.enqueue 3 q2 in
  let c0 = Counter.zero in
  let c1 = Counter.increment c0 in
  let c2 = Counter.increment c1 in
  let c3 = Counter.add 10 c2 in
  [ Test.equal "abs" 42 (Math.abs (0 - 42))
    , Test.equal "gcd" 6 (Math.gcd 48 18)
    , Test.equal "lcm" 12 (Math.lcm 4 6)
    , Test.equal "pow" 256 (Math.pow 2 8)
    , Test.equal "clamp" 100 (Math.clamp 0 100 150)
    , Test.equal "dot" 11 (Vec2.dot v1 v2)
    , Test.equal "lengthSquared" 25 (Vec2.lengthSquared v1)
    , Test.equal "add X" 4 (Vec2.getX v3)
    , Test.equal "scale Y" 8 (Vec2.getY v4)
    , Test.equal "stack size" 3 (Stack.size s3)
    , Test.equal "stack peek" (Just 3) (Stack.peek s3)
    , Test.ok "stack isEmpty" (Stack.isEmpty s0)
    , Test.ok "stack not isEmpty" (Bool.negate (Stack.isEmpty s3))
    , Test.equal "queue size" 3 (Queue.size q3)
    , Test.equal "queue dequeue first" (Just 1) (Maybe.map (p ->
      match p
        when (x, _) -> x
      end) (Queue.dequeue q3))
    , Test.ok "queue isEmpty" (Queue.isEmpty q0)
    , Test.equal "counter value after add" 12 (Counter.value c3)
    , Test.equal "counter zero value" 0 (Counter.value c0)
    ]

let main =
  args -> Test.run tests
