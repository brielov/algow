-- =============================================================================
-- Recursion: All Recursive Patterns and Techniques
-- =============================================================================

-- =============================================================================
-- Basic Recursion
-- =============================================================================

let testBasicRecursion =
  -- Factorial
  let rec factorial = n ->
    if n <= 1 then 1 else n * factorial (n - 1)
  in
  let a = factorial 5 in                    -- 120
  let b = factorial 0 in                    -- 1

  -- Fibonacci
  let rec fib = n ->
    if n <= 1 then n else fib (n - 1) + fib (n - 2)
  in
  let c = fib 10 in                         -- 55

  -- Sum from 1 to n
  let rec sumTo = n ->
    if n <= 0 then 0 else n + sumTo (n - 1)
  in
  let d = sumTo 10 in                       -- 55

  -- Power function
  let rec pow = base exp_ ->
    if exp_ <= 0 then 1 else base * pow base (exp_ - 1)
  in
  let e = pow 2 10 in                       -- 1024

  a + b + c + d + e                         -- 1255

-- =============================================================================
-- Tail Recursion with Accumulators
-- =============================================================================

let testTailRecursion =
  -- Tail-recursive factorial
  let rec factTail = acc n ->
    if n <= 1 then acc else factTail (acc * n) (n - 1)
  in
  let factorial_ = factTail 1 in
  let a = factorial_ 5 in                   -- 120

  -- Tail-recursive fibonacci
  let rec fibTail = a b n ->
    if n <= 0 then a else fibTail b (a + b) (n - 1)
  in
  let fib_ = fibTail 0 1 in
  let b = fib_ 10 in                        -- 55

  -- Tail-recursive sum
  let rec sumTail = acc n ->
    if n <= 0 then acc else sumTail (acc + n) (n - 1)
  in
  let c = sumTail 0 100 in                  -- 5050

  -- Tail-recursive length
  let rec lengthTail = acc xs ->
    match xs
      when Nil -> acc
      when Cons _ rest -> lengthTail (acc + 1) rest
    end
  in
  let d = lengthTail 0 [1, 2, 3, 4, 5] in   -- 5

  -- Tail-recursive reverse
  let rec reverseTail = acc xs ->
    match xs
      when Nil -> acc
      when Cons x rest -> reverseTail (x :: acc) rest
    end
  in
  let reversed = reverseTail Nil [1, 2, 3] in
  let e = match reversed when Cons x _ -> x when Nil -> 0 end in  -- 3

  a + b + c + d + e                         -- 5233

-- =============================================================================
-- Mutual Recursion
-- =============================================================================

let testMutualRecursion =
  -- isEven and isOdd
  let rec isEven = n -> if n == 0 then true else isOdd (n - 1)
  and isOdd = n -> if n == 0 then false else isEven (n - 1)
  in
  let a = if isEven 10 then 1 else 0 in     -- 1
  let b = if isOdd 10 then 1 else 0 in      -- 0
  let c = if isEven 7 then 1 else 0 in      -- 0
  let d = if isOdd 7 then 1 else 0 in       -- 1

  -- Mutually recursive tree traversal
  let rec processOdd = n -> if n <= 0 then 0 else n + processEven (n - 1)
  and processEven = n -> if n <= 0 then 0 else processOdd (n - 1)
  in
  let e = processOdd 10 in                  -- 30 (10 + 8 + 6 + 4 + 2)
  let f = processEven 10 in                 -- 25 (9 + 7 + 5 + 3 + 1)

  a + b + c + d + e + f                     -- 57

-- =============================================================================
-- Recursion on Lists
-- =============================================================================

let testListRecursion =
  -- Sum of list
  let rec sumList = xs ->
    match xs
      when Nil -> 0
      when Cons x rest -> x + sumList rest
    end
  in
  let a = sumList [1, 2, 3, 4, 5] in        -- 15

  -- Product of list
  let rec prodList = xs ->
    match xs
      when Nil -> 1
      when Cons x rest -> x * prodList rest
    end
  in
  let b = prodList [1, 2, 3, 4] in          -- 24

  -- Maximum element
  let rec maxList = xs ->
    match xs
      when Nil -> 0
      when Cons x Nil -> x
      when Cons x rest ->
        let m = maxList rest in
        if x > m then x else m
    end
  in
  let c = maxList [3, 1, 4, 1, 5, 9, 2, 6] in  -- 9

  -- Count occurrences
  let rec countOccur = target xs ->
    match xs
      when Nil -> 0
      when Cons x rest ->
        let rest_ = countOccur target rest in
        if x == target then 1 + rest_ else rest_
    end
  in
  let d = countOccur 1 [1, 2, 1, 3, 1, 4, 1] in  -- 4

  -- Flatten nested lists
  let rec flatten = xss ->
    match xss
      when Nil -> Nil
      when Cons xs rest -> List.concat [xs, flatten rest]
    end
  in
  let flattened = flatten [[1, 2], [3], [4, 5, 6]] in
  let e = sumList flattened in              -- 21

  a + b + c + d + e                         -- 73

-- =============================================================================
-- Recursion on Trees
-- =============================================================================

type BinTree a = Empty | Branch a (BinTree a) (BinTree a)

let testTreeRecursion =
  -- Count nodes
  let rec countNodes = tree ->
    match tree
      when Empty -> 0
      when Branch _ left right -> 1 + countNodes left + countNodes right
    end
  in
  -- Sum all values
  let rec sumNodes = tree ->
    match tree
      when Empty -> 0
      when Branch x left right -> x + sumNodes left + sumNodes right
    end
  in
  -- Tree depth
  let rec treeDepth = tree ->
    match tree
      when Empty -> 0
      when Branch _ left right ->
        let l = treeDepth left in
        let r = treeDepth right in
        1 + (if l > r then l else r)
    end
  in
  -- Mirror tree
  let rec mirror = tree ->
    match tree
      when Empty -> Empty
      when Branch x left right -> Branch x (mirror right) (mirror left)
    end
  in

  -- Build a tree:     10
  --                  /  \
  --                 5    15
  --                / \     \
  --               3   7    20
  let tree = Branch 10 (Branch 5 (Branch 3 Empty Empty) (Branch 7 Empty Empty)) (Branch 15 Empty (Branch 20 Empty Empty)) in
  let a = countNodes tree in                -- 6
  let b = sumNodes tree in                  -- 60
  let c = treeDepth tree in                 -- 3

  -- Check mirror preserves sum
  let mirrored = mirror tree in
  let d = sumNodes mirrored in              -- 60

  a + b + c + d                             -- 129

-- =============================================================================
-- Higher-Order Recursive Functions
-- =============================================================================

let testHigherOrderRecursion =
  -- Generic fold
  let rec fold = f z xs ->
    match xs
      when Nil -> z
      when Cons x rest -> f x (fold f z rest)
    end
  in
  let a = fold (x acc -> x + acc) 0 [1, 2, 3, 4, 5] in  -- 15

  -- Generic map
  let rec map_ = f xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> f x :: map_ f rest
    end
  in
  let doubled = map_ (x -> x * 2) [1, 2, 3] in
  let b = fold (x acc -> x + acc) 0 doubled in  -- 12

  -- Generic filter
  let rec filter_ = p xs ->
    match xs
      when Nil -> Nil
      when Cons x rest ->
        if p x then x :: filter_ p rest else filter_ p rest
    end
  in
  let evens = filter_ (x -> Int.mod x 2 == 0) [1, 2, 3, 4, 5, 6] in
  let c = fold (x acc -> x + acc) 0 evens in    -- 12

  -- takeWhile
  let rec takeWhile = p xs ->
    match xs
      when Nil -> Nil
      when Cons x rest ->
        if p x then x :: takeWhile p rest else Nil
    end
  in
  let taken = takeWhile (x -> x < 5) [1, 2, 3, 4, 5, 6, 7] in
  let d = fold (x acc -> x + acc) 0 taken in    -- 10

  -- dropWhile
  let rec dropWhile = p xs ->
    match xs
      when Nil -> Nil
      when Cons x rest ->
        if p x then dropWhile p rest else xs
    end
  in
  let dropped = dropWhile (x -> x < 5) [1, 2, 3, 4, 5, 6, 7] in
  let e = fold (x acc -> x + acc) 0 dropped in  -- 18

  a + b + c + d + e                         -- 67

-- =============================================================================
-- Numeric Recursion Patterns
-- =============================================================================

let testNumericRecursion =
  -- Greatest Common Divisor (Euclidean algorithm)
  let rec gcd = a b ->
    if b == 0 then a else gcd b (Int.mod a b)
  in
  let a = gcd 48 18 in                      -- 6
  let b = gcd 100 35 in                     -- 5

  -- Integer square root (binary search style)
  let rec isqrt_ = n guess ->
    let next = Int.div (guess + Int.div n guess) 2 in
    if next >= guess then guess else isqrt_ n next
  in
  let isqrt = n -> if n <= 1 then n else isqrt_ n (Int.div n 2) in
  let c = isqrt 100 in                      -- 10
  let d = isqrt 144 in                      -- 12

  -- Digit sum
  let rec digitSum = n ->
    if n <= 0 then 0 else Int.mod n 10 + digitSum (Int.div n 10)
  in
  let e = digitSum 12345 in                 -- 15

  -- Count digits
  let rec countDigits = n ->
    if n < 10 then 1 else 1 + countDigits (Int.div n 10)
  in
  let f = countDigits 12345 in              -- 5

  -- Reverse integer
  let rec reverseInt = acc n ->
    if n <= 0 then acc else reverseInt (acc * 10 + Int.mod n 10) (Int.div n 10)
  in
  let g = reverseInt 0 12345 in             -- 54321

  a + b + c + d + e + f + g                 -- 54374

-- =============================================================================
-- Recursion with Data Structure Building
-- =============================================================================

let testBuildingRecursion =
  -- Generate range [start, end)
  let rec range = start end_ ->
    if start >= end_ then Nil else start :: range (start + 1) end_
  in
  let r = range 1 6 in
  let a = List.foldl (acc x -> acc + x) 0 r in  -- 15

  -- Replicate value n times
  let rec replicate = n x ->
    if n <= 0 then Nil else x :: replicate (n - 1) x
  in
  let fives = replicate 5 10 in
  let b = List.foldl (acc x -> acc + x) 0 fives in  -- 50

  -- Build balanced tree
  let rec buildTree = xs ->
    match xs
      when Nil -> Empty
      when Cons x rest ->
        let len = List.length rest in
        let half = Int.div len 2 in
        let left = List.take half rest in
        let right = List.drop half rest in
        Branch x (buildTree left) (buildTree right)
    end
  in
  let tree = buildTree [5, 3, 7, 1, 4, 6, 8] in
  let rec sumTree_ = t ->
    match t when Empty -> 0 when Branch x l r -> x + sumTree_ l + sumTree_ r end
  in
  let c = sumTree_ tree in                  -- 34

  -- Interleave two lists
  let rec interleave = xs ys ->
    match xs
      when Nil -> ys
      when Cons x rest -> x :: interleave ys rest
    end
  in
  let interleaved = interleave [1, 3, 5] [2, 4, 6] in
  let d = List.foldl (acc x -> acc + x) 0 interleaved in  -- 21

  a + b + c + d                             -- 120

-- =============================================================================
-- Divide and Conquer Recursion
-- =============================================================================

let testDivideAndConquer =
  -- Merge sort helper: merge two sorted lists
  let rec merge = xs ys ->
    match xs
      when Nil -> ys
      when Cons x xrest -> match ys
        when Nil -> xs
        when Cons y yrest ->
          if x <= y then x :: merge xrest ys
          else y :: merge xs yrest
      end
    end
  in

  -- Split list in half
  let rec splitAt = n xs ->
    if n <= 0 then (Nil, xs) else match xs
      when Nil -> (Nil, Nil)
      when Cons x rest ->
        let (left, right) = splitAt (n - 1) rest in
        (x :: left, right)
    end
  in

  -- Merge sort
  let rec mergeSort = xs ->
    match xs
      when Nil -> Nil
      when Cons _ Nil -> xs
      when Cons _ _ ->
        let len = List.length xs in
        let (left, right) = splitAt (Int.div len 2) xs in
        merge (mergeSort left) (mergeSort right)
    end
  in

  let sorted = mergeSort [5, 2, 8, 1, 9, 3, 7, 4, 6] in
  let rec isSorted = xs ->
    match xs
      when Nil -> true
      when Cons _ Nil -> true
      when Cons x (Cons y rest) -> if x <= y then isSorted (y :: rest) else false
    end
  in
  let a = if isSorted sorted then 1 else 0 in   -- 1

  -- Sum of sorted list
  let b = List.foldl (acc x -> acc + x) 0 sorted in  -- 45

  -- Binary search in sorted list
  let rec binarySearch = target xs ->
    match xs
      when Nil -> false
      when Cons _ _ ->
        let len = List.length xs in
        if len == 1 then
          match xs when Cons x _ -> x == target when Nil -> false end
        else
          let mid = Int.div len 2 in
          let (left, right) = splitAt mid xs in
          match right
            when Cons midVal _ ->
              if target == midVal then true
              else if target < midVal then binarySearch target left
              else binarySearch target right
            when Nil -> binarySearch target left
          end
    end
  in
  let c = if binarySearch 5 sorted then 1 else 0 in  -- 1
  let d = if binarySearch 10 sorted then 1 else 0 in -- 0

  a + b + c + d                             -- 47

-- =============================================================================
-- Final Result
-- =============================================================================

let result =
  testBasicRecursion +                      -- 1255
  testTailRecursion +                       -- 5233
  testMutualRecursion +                     -- 57
  testListRecursion +                       -- 73
  testTreeRecursion +                       -- 129
  testHigherOrderRecursion +                -- 67
  testNumericRecursion +                    -- 54374
  testBuildingRecursion +                   -- 120
  testDivideAndConquer                      -- 47
                                            -- Total: 61355

result
