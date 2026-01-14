-- Stress Tests: Large structures, Deep recursion, Complex types

-- Large ADT with many constructors
type LargeEnum
  = E0 | E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8 | E9
  | E10 | E11 | E12 | E13 | E14 | E15 | E16 | E17 | E18 | E19
  | E20 | E21 | E22 | E23 | E24 | E25 | E26 | E27 | E28 | E29
  | E30 | E31 | E32 | E33 | E34 | E35 | E36 | E37 | E38 | E39
  | E40 | E41 | E42 | E43 | E44 | E45 | E46 | E47 | E48 | E49

let enumToInt = e -> match e
  when E0 -> 0 when E1 -> 1 when E2 -> 2 when E3 -> 3 when E4 -> 4
  when E5 -> 5 when E6 -> 6 when E7 -> 7 when E8 -> 8 when E9 -> 9
  when E10 -> 10 when E11 -> 11 when E12 -> 12 when E13 -> 13 when E14 -> 14
  when E15 -> 15 when E16 -> 16 when E17 -> 17 when E18 -> 18 when E19 -> 19
  when E20 -> 20 when E21 -> 21 when E22 -> 22 when E23 -> 23 when E24 -> 24
  when E25 -> 25 when E26 -> 26 when E27 -> 27 when E28 -> 28 when E29 -> 29
  when E30 -> 30 when E31 -> 31 when E32 -> 32 when E33 -> 33 when E34 -> 34
  when E35 -> 35 when E36 -> 36 when E37 -> 37 when E38 -> 38 when E39 -> 39
  when E40 -> 40 when E41 -> 41 when E42 -> 42 when E43 -> 43 when E44 -> 44
  when E45 -> 45 when E46 -> 46 when E47 -> 47 when E48 -> 48 when E49 -> 49
end

-- Generate a range of numbers
let rec range = start end_ ->
  if start >= end_ then []
  else start :: range (start + 1) end_

-- Tail-recursive sum for large lists
let rec sumTailRec = acc xs -> match xs
  when Nil -> acc
  when Cons x rest -> sumTailRec (acc + x) rest
end

-- Tail-recursive length for large lists
let rec lengthTailRec = acc xs -> match xs
  when Nil -> acc
  when Cons _ rest -> lengthTailRec (acc + 1) rest
end

-- Tail-recursive map for large lists
let rec mapTailRec = f acc xs -> match xs
  when Nil -> List.reverse acc
  when Cons x rest -> mapTailRec f (f x :: acc) rest
end

-- Tail-recursive filter for large lists
let rec filterTailRec = p acc xs -> match xs
  when Nil -> List.reverse acc
  when Cons x rest -> if p x then filterTailRec p (x :: acc) rest else filterTailRec p acc rest
end

-- Tail-recursive fibonacci (stress test)
let rec fibTailRec = n a b ->
  if n <= 0 then a
  else fibTailRec (n - 1) b (a + b)

-- Deep recursion test (tail-recursive countdown)
let rec countdown = n ->
  if n <= 0 then 0
  else countdown (n - 1)

-- Large record
let largeRecord =
  { f0 = 0, f1 = 1, f2 = 2, f3 = 3, f4 = 4
  , f5 = 5, f6 = 6, f7 = 7, f8 = 8, f9 = 9
  , f10 = 10, f11 = 11, f12 = 12, f13 = 13, f14 = 14
  , f15 = 15, f16 = 16, f17 = 17, f18 = 18, f19 = 19
  , f20 = 20, f21 = 21, f22 = 22, f23 = 23, f24 = 24
  , f25 = 25, f26 = 26, f27 = 27, f28 = 28, f29 = 29
  }

let sumLargeRecord = r ->
  r.f0 + r.f1 + r.f2 + r.f3 + r.f4 +
  r.f5 + r.f6 + r.f7 + r.f8 + r.f9 +
  r.f10 + r.f11 + r.f12 + r.f13 + r.f14 +
  r.f15 + r.f16 + r.f17 + r.f18 + r.f19 +
  r.f20 + r.f21 + r.f22 + r.f23 + r.f24 +
  r.f25 + r.f26 + r.f27 + r.f28 + r.f29

-- Large tuple
let largeTuple = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

let sumLargeTuple = t -> match t
  when (a, b, c, d, e, f, g, h, i, j) -> a + b + c + d + e + f + g + h + i + j
end

-- Deeply nested Maybe - test with specific depth
type Deep10 = D10 (Maybe (Maybe (Maybe (Maybe (Maybe (Maybe (Maybe (Maybe (Maybe (Maybe int))))))))))

let deepMaybe = Just (Just (Just (Just (Just (Just (Just (Just (Just (Just 42)))))))))

-- Unwrap 10 levels deep
let unwrapDeep10 = m -> match m
  when Just m1 -> match m1
    when Just m2 -> match m2
      when Just m3 -> match m3
        when Just m4 -> match m4
          when Just m5 -> match m5
            when Just m6 -> match m6
              when Just m7 -> match m7
                when Just m8 -> match m8
                  when Just m9 -> match m9
                    when Just x -> x
                    when Nothing -> (-1)
                  end
                  when Nothing -> (-1)
                end
                when Nothing -> (-1)
              end
              when Nothing -> (-1)
            end
            when Nothing -> (-1)
          end
          when Nothing -> (-1)
        end
        when Nothing -> (-1)
      end
      when Nothing -> (-1)
    end
    when Nothing -> (-1)
  end
  when Nothing -> (-1)
end

-- Complex nested type
type Tree a = Leaf | Node a (Tree a) (Tree a)

let rec buildTree = depth ->
  if depth <= 0 then Leaf
  else Node depth (buildTree (depth - 1)) (buildTree (depth - 1))

let rec treeSize = t -> match t
  when Leaf -> 0
  when Node _ l r -> 1 + treeSize l + treeSize r
end

let rec treeSumTailRec = acc stack -> match stack
  when Nil -> acc
  when Cons t rest -> match t
    when Leaf -> treeSumTailRec acc rest
    when Node v l r -> treeSumTailRec (acc + v) (l :: r :: rest)
  end
end

-- Long chain of function applications
let chain = x ->
  x |> (a -> a + 1)
    |> (a -> a * 2)
    |> (a -> a + 3)
    |> (a -> a * 2)
    |> (a -> a + 5)
    |> (a -> a * 2)
    |> (a -> a + 7)
    |> (a -> a * 2)
    |> (a -> a + 9)
    |> (a -> a * 2)

-- Large match expression
let largeMatch = n -> match n
  when 0 -> "zero"
  when 1 -> "one"
  when 2 -> "two"
  when 3 -> "three"
  when 4 -> "four"
  when 5 -> "five"
  when 6 -> "six"
  when 7 -> "seven"
  when 8 -> "eight"
  when 9 -> "nine"
  when 10 -> "ten"
  when 11 -> "eleven"
  when 12 -> "twelve"
  when 13 -> "thirteen"
  when 14 -> "fourteen"
  when 15 -> "fifteen"
  when 16 -> "sixteen"
  when 17 -> "seventeen"
  when 18 -> "eighteen"
  when 19 -> "nineteen"
  when 20 -> "twenty"
  when _ -> "large"
end

-- Stress type inference
let complexInference =
  let f = x -> x in
  let g = x -> f (f (f (f (f x)))) in
  let h = x -> g (g x) in
  h 42

-- Long list operations
let longListOps = xs ->
  xs
    |> List.map (x -> x * 2)
    |> List.filter (x -> Int.mod x 4 == 0)
    |> List.map (x -> x + 1)
    |> List.foldl (acc x -> acc + x) 0

let tests =
  let smallList = range 0 100 in
  let mediumList = range 0 1000 in
  let tree5 = buildTree 5 in
  [ -- Large enum tests
    Test.equal "enum E0" 0 (enumToInt E0)
  , Test.equal "enum E25" 25 (enumToInt E25)
  , Test.equal "enum E49" 49 (enumToInt E49)
    -- Large list operations
  , Test.equal "range 0-100 length" 100 (lengthTailRec 0 smallList)
  , Test.equal "range 0-100 sum" 4950 (sumTailRec 0 smallList)
  , Test.equal "range 0-1000 length" 1000 (lengthTailRec 0 mediumList)
  , Test.equal "range 0-1000 sum" 499500 (sumTailRec 0 mediumList)
    -- Large list map/filter
  , Test.equal "map large list" 9900 (sumTailRec 0 (mapTailRec (x -> x * 2) [] smallList))
  , Test.equal "filter large list" 2450 (sumTailRec 0 (filterTailRec (x -> Int.mod x 2 == 0) [] smallList))
    -- Deep recursion (tail-recursive)
  , Test.equal "countdown 10000" 0 (countdown 10000)
  , Test.equal "fib 20" 6765 (fibTailRec 20 0 1)
  , Test.equal "fib 30" 832040 (fibTailRec 30 0 1)
    -- Large record
  , Test.equal "large record sum" 435 (sumLargeRecord largeRecord)
  , Test.equal "large record field" 15 largeRecord.f15
  , Test.equal "large record update" 115 ((sumLargeRecord { largeRecord | f0 = 100 }) - 335)
    -- Large tuple
  , Test.equal "large tuple sum" 45 (sumLargeTuple largeTuple)
  , Test.equal "large tuple access" 5 largeTuple.5
    -- Deep Maybe unwrap
  , Test.equal "deep maybe" 42 (unwrapDeep10 deepMaybe)
    -- Tree operations
  , Test.equal "tree size depth 5" 31 (treeSize tree5)
  , Test.equal "tree sum tail rec" 57 (treeSumTailRec 0 [tree5])
    -- Long chain
  , Test.equal "long chain" 318 (chain 1)
    -- Large match
  , Test.equal "large match 0" "zero" (largeMatch 0)
  , Test.equal "large match 10" "ten" (largeMatch 10)
  , Test.equal "large match 20" "twenty" (largeMatch 20)
  , Test.equal "large match 100" "large" (largeMatch 100)
    -- Complex inference
  , Test.equal "complex inference" 42 complexInference
    -- Long list operations
  , Test.equal "long list ops" 2550 (longListOps smallList)
    -- Many consecutive operations
  , Test.equal "many ops" 20200 (
      range 0 100
        |> List.map (x -> x + 1)
        |> List.map (x -> x * 2)
        |> sumTailRec 0
    )
    -- Nested list operations
  , Test.equal "nested list ops" 4950 (
      range 0 100
        |> List.filter (x -> x >= 0)
        |> List.map (x -> x)
        |> sumTailRec 0
    )
    -- Large concat
  , Test.equal "concat many lists" 300 (
      lengthTailRec 0 (List.concat [range 0 100, range 0 100, range 0 100])
    )
  ]

let main = args -> Test.run tests
