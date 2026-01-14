-- Type Inference: Let-polymorphism, Complex Expressions, Constraints

-- Identity function - most polymorphic
let id = x -> x

-- Const function - polymorphic in second argument
let const_ = x y -> x

-- Flip function - polymorphic
let flip = f x y -> f y x

-- Compose function - polymorphic
let compose = f g x -> f (g x)

-- Apply function
let apply = f x -> f x

-- Let-polymorphism: id can be used at different types in same expression
let polyId =
  let id = x -> x in
  (id 42, id "hello", id true)

-- Let-polymorphism with const
let polyConst =
  let const_ = x y -> x in
  (const_ 1 "ignored", const_ "hello" 42, const_ true Nothing)

-- Let-polymorphism with compose
let polyCompose =
  let compose = f g x -> f (g x) in
  let addOne = x -> x + 1 in
  let double = x -> x * 2 in
  let toUpper = String.toUpper in
  let append = s -> String.concat s "!" in
  (compose double addOne 5, compose append toUpper "hello")

-- Type inference through applications
let chainedApplication =
  let f = x -> x + 1 in
  let g = x -> x * 2 in
  let h = x -> x - 3 in
  h (g (f 10))

-- Type inference with higher-order functions
let higherOrderInference =
  let map_ = f xs -> List.map f xs in
  let filter_ = p xs -> List.filter p xs in
  let fold_ = f init xs -> List.foldl f init xs in
  let double = x -> x * 2 in
  let isEven = x -> Int.mod x 2 == 0 in
  let sum = acc x -> acc + x in
  fold_ sum 0 (filter_ isEven (map_ double [1, 2, 3, 4, 5]))

-- Inference with Maybe
let maybeInference =
  let safeHead = xs -> match xs when Nil -> Nothing when Cons x _ -> Just x end in
  let mapMaybe = f opt -> match opt when Nothing -> Nothing when Just x -> Just (f x) end in
  let flatMapMaybe = f opt -> match opt when Nothing -> Nothing when Just x -> f x end in
  let double = x -> x * 2 in
  let safeDivide = x -> if x == 0 then Nothing else Just (Int.div 100 x) in
  (mapMaybe double (safeHead [5, 10, 15]),
   flatMapMaybe safeDivide (safeHead [10, 20, 30]))

-- Inference with Either
let eitherInference =
  let mapRight = f e -> match e when Left err -> Left err when Right x -> Right (f x) end in
  let mapLeft = f e -> match e when Left err -> Left (f err) when Right x -> Right x end in
  let double = x -> x * 2 in
  let toUpper = String.toUpper in
  (mapRight double (Right 21),
   mapLeft toUpper (Left "error"))

-- Inference with records
let recordInference =
  let getX = r -> r.x in
  let getY = r -> r.y in
  let sumXY = r -> r.x + r.y in
  let point = { x = 10, y = 20 } in
  let point3d = { x = 1, y = 2, z = 3 } in
  (getX point, getX point3d, sumXY point, sumXY point3d)

-- Inference with tuples
let tupleInference =
  let fst_ = p -> match p when (a, _) -> a end in
  let snd_ = p -> match p when (_, b) -> b end in
  let swap = p -> match p when (a, b) -> (b, a) end in
  let pair1 = (42, "hello") in
  let pair2 = (true, [1, 2, 3]) in
  (fst_ pair1, snd_ pair1, fst_ pair2, swap pair1)

-- Inference with nested structures
let nestedInference =
  let nested = Just (Just (Just 42)) in
  let unwrap3 = opt -> match opt
    when Just (Just (Just x)) -> x
    when _ -> 0
  end in
  unwrap3 nested

-- Inference with mutual recursion
let rec isEven = n -> if n == 0 then true else isOdd (n - 1)
and isOdd = n -> if n == 0 then false else isEven (n - 1)

-- Inference with complex pattern matching
let complexPattern =
  let extract = data -> match data
    when (Nothing, _) -> 0
    when (Just x, Nil) -> x
    when (Just x, Cons y _) -> x + y
  end in
  (extract (Nothing, [1, 2, 3]),
   extract (Just 10, Nil),
   extract (Just 10, [20, 30]))

-- Inference through pipelines
let pipelineInference =
  [1, 2, 3, 4, 5]
    |> List.filter (x -> x > 2)
    |> List.map (x -> x * 2)
    |> List.foldl (acc x -> acc + x) 0

-- Inference with ADTs
type Box a = Box a
type Pair a b = MkPair a b

let adtInference =
  let unbox = b -> match b when Box x -> x end in
  let fstPair = p -> match p when MkPair a _ -> a end in
  let sndPair = p -> match p when MkPair _ b -> b end in
  let box1 = Box 42 in
  let box2 = Box "hello" in
  let pair = MkPair 10 "world" in
  (unbox box1, unbox box2, fstPair pair, sndPair pair)

-- Inference with recursive types
type MyList a = MyNil | MyCons a (MyList a)

let rec myLength = xs -> match xs
  when MyNil -> 0
  when MyCons _ rest -> 1 + myLength rest
end

let rec myMap = f xs -> match xs
  when MyNil -> MyNil
  when MyCons x rest -> MyCons (f x) (myMap f rest)
end

let customListInference =
  let xs = MyCons 1 (MyCons 2 (MyCons 3 MyNil)) in
  let doubled = myMap (x -> x * 2) xs in
  (myLength xs, myLength doubled)

-- Inference with do-notation
let doInference =
  do[Maybe]
    x <- Just 10
    y <- Just 20
    z <- Just 30
    Just (x + y + z)
  end

-- Constrained polymorphism
let constrainedPoly =
  let eq_ = a b -> a == b in
  let lt = a b -> a < b in
  let add = a b -> a + b in
  (eq_ 5 5, eq_ "hello" "hello", lt 5 10, lt "a" "b", add 10 20, add 1.5 2.5)

-- Inference with function returning function
let functionReturningFunction =
  let makeAdder = n -> (x -> x + n) in
  let add5 = makeAdder 5 in
  let add10 = makeAdder 10 in
  (add5 100, add10 100)

-- Inference with multiple type variables
let multiTypeVar =
  let pair_ = a b -> (a, b) in
  let first_ = p -> match p when (a, _) -> a end in
  let second_ = p -> match p when (_, b) -> b end in
  let p1 = pair_ 42 "hello" in
  let p2 = pair_ true [1, 2, 3] in
  (first_ p1, second_ p1, first_ p2, second_ p2)

let tests =
  [ Test.equal "identity int" 42 (id 42)
  , Test.equal "identity string" "hello" (id "hello")
  , Test.equal "identity bool" true (id true)
  , Test.equal "const" 10 (const_ 10 "ignored")
  , Test.equal "flip" 5 (flip (a b -> a - b) 5 10)
  , Test.equal "compose" 15 (compose (x -> x + 5) (x -> x * 2) 5)
  , Test.equal "apply" 10 (apply (x -> x * 2) 5)
  , Test.equal "polyId" (42, "hello", true) polyId
  , Test.equal "polyConst" (1, "hello", true) polyConst
  , Test.equal "polyCompose" (12, "HELLO!") polyCompose
  , Test.equal "chained" 19 chainedApplication
  , Test.equal "higher order" 40 higherOrderInference
  , Test.equal "maybe inference" (Just 10, Just 10) maybeInference
  , Test.equal "either inference" (Right 42, Left "ERROR") eitherInference
  , Test.equal "record inference" (10, 1, 30, 3) recordInference
  , Test.equal "tuple inference fst" 42 (match tupleInference when (a, _, _, _) -> a end)
  , Test.equal "tuple inference snd" "hello" (match tupleInference when (_, b, _, _) -> b end)
  , Test.equal "nested inference" 42 nestedInference
  , Test.ok "isEven 10" (isEven 10)
  , Test.ok "isOdd 11" (isOdd 11)
  , Test.ok "isEven 0" (isEven 0)
  , Test.equal "complex pattern" (0, 10, 30) complexPattern
  , Test.equal "pipeline" 18 pipelineInference
  , Test.equal "adt inference" (42, "hello", 10, "world") adtInference
  , Test.equal "custom list" (3, 3) customListInference
  , Test.equal "do inference" (Just 60) doInference
  , Test.equal "constrained poly" (true, true, true, true, 30, 4.0) constrainedPoly
  , Test.equal "function returning function" (105, 110) functionReturningFunction
  , Test.equal "multi type var first" 42 (match multiTypeVar when (a, _, _, _) -> a end)
  , Test.equal "multi type var second" "hello" (match multiTypeVar when (_, b, _, _) -> b end)
  ]

let main = args -> Test.run tests
