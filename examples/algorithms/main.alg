-- =============================================================================
-- Algorithms and Data Structures
-- =============================================================================

-- =============================================================================
-- Sorting Algorithms
-- =============================================================================

let testSortingAlgorithms =
  -- Insertion Sort
  let rec insert = x sorted ->
    match sorted
      when Nil -> x :: Nil
      when Cons y rest -> if x <= y then x :: sorted else y :: insert x rest
    end
  in
  let rec insertionSort = xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> insert x (insertionSort rest)
    end
  in
  let sorted1 = insertionSort [5, 2, 8, 1, 9] in
  let a = List.foldl (acc x -> acc * 10 + x) 0 sorted1 in  -- 12589

  -- Quick Sort
  let rec partition = pivot xs ->
    match xs
      when Nil -> (Nil, Nil)
      when Cons y rest ->
        let (lt, ge) = partition pivot rest in
        if y < pivot then (y :: lt, ge) else (lt, y :: ge)
    end
  in
  let rec quickSort = xs ->
    match xs
      when Nil -> Nil
      when Cons pivot rest ->
        let (lt, ge) = partition pivot rest in
        List.concat [quickSort lt, pivot :: Nil, quickSort ge]
    end
  in
  let sorted2 = quickSort [3, 7, 1, 8, 2, 6] in
  let b = List.foldl (acc x -> acc * 10 + x) 0 sorted2 in  -- 123678

  a + b                                     -- 136267

-- =============================================================================
-- Searching Algorithms
-- =============================================================================

let testSearchingAlgorithms =
  -- Linear Search
  let rec linearSearch = target xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest ->
        if x == target then Just x
        else linearSearch target rest
    end
  in
  let a = match linearSearch 5 [1, 3, 5, 7, 9] when Just _ -> 1 when Nothing -> 0 end in  -- 1
  let b = match linearSearch 4 [1, 3, 5, 7, 9] when Just _ -> 1 when Nothing -> 0 end in  -- 0

  -- Find index
  let rec findIndex = target idx xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest ->
        if x == target then Just idx
        else findIndex target (idx + 1) rest
    end
  in
  let c = match findIndex 5 0 [1, 3, 5, 7, 9] when Just i -> i when Nothing -> -1 end in  -- 2
  let d = match findIndex 4 0 [1, 3, 5, 7, 9] when Just i -> i when Nothing -> -1 end in  -- -1

  -- Count occurrences
  let rec countOccurrences = target xs ->
    match xs
      when Nil -> 0
      when Cons x rest ->
        let rest_ = countOccurrences target rest in
        if x == target then 1 + rest_ else rest_
    end
  in
  let e = countOccurrences 2 [1, 2, 3, 2, 4, 2, 5] in  -- 3

  a + b + c + d + e                         -- 5

-- =============================================================================
-- Numeric Algorithms
-- =============================================================================

let testNumericAlgorithms =
  -- Greatest Common Divisor
  let rec gcd = a b -> if b == 0 then a else gcd b (Int.mod a b) in
  let a = gcd 48 18 in                      -- 6

  -- Least Common Multiple
  let lcm = a b -> Int.div (a * b) (gcd a b) in
  let b = lcm 12 18 in                      -- 36

  -- Prime check (simple trial division)
  let rec isPrimeHelper = n divisor ->
    if divisor * divisor > n then true
    else if Int.mod n divisor == 0 then false
    else isPrimeHelper n (divisor + 1)
  in
  let isPrime = n -> if n < 2 then false else isPrimeHelper n 2 in
  let c = if isPrime 17 then 1 else 0 in    -- 1
  let d = if isPrime 15 then 1 else 0 in    -- 0

  -- Sum of divisors
  let rec sumDivisors = n d ->
    if d > Int.div n 2 then 0
    else if Int.mod n d == 0 then d + sumDivisors n (d + 1)
    else sumDivisors n (d + 1)
  in
  let e = sumDivisors 12 1 in               -- 16 (1+2+3+4+6)

  -- Count primes up to n
  let rec countPrimes = n count ->
    if n < 2 then count
    else countPrimes (n - 1) (if isPrime n then count + 1 else count)
  in
  let f = countPrimes 20 0 in               -- 8 (2,3,5,7,11,13,17,19)

  a + b + c + d + e + f                     -- 67

-- =============================================================================
-- Tree Algorithms
-- =============================================================================

type BST a = Empty | Node a (BST a) (BST a)

let testTreeAlgorithms =
  -- Insert into BST
  let rec insert = x tree ->
    match tree
      when Empty -> Node x Empty Empty
      when Node v left right ->
        if x < v then Node v (insert x left) right
        else if x > v then Node v left (insert x right)
        else tree
    end
  in

  -- Build BST from list
  let rec buildBST = xs tree ->
    match xs
      when Nil -> tree
      when Cons x rest -> buildBST rest (insert x tree)
    end
  in

  -- In-order traversal
  let rec inOrder = tree ->
    match tree
      when Empty -> Nil
      when Node v left right -> List.concat [inOrder left, v :: Nil, inOrder right]
    end
  in

  -- Check if BST contains value
  let rec contains = x tree ->
    match tree
      when Empty -> false
      when Node v left right ->
        if x == v then true
        else if x < v then contains x left
        else contains x right
    end
  in

  -- Count nodes
  let rec countNodes = tree ->
    match tree
      when Empty -> 0
      when Node _ left right -> 1 + countNodes left + countNodes right
    end
  in

  -- Tree height
  let rec height = tree ->
    match tree
      when Empty -> 0
      when Node _ left right ->
        let lh = height left in
        let rh = height right in
        1 + (if lh > rh then lh else rh)
    end
  in

  let tree = buildBST [5, 3, 7, 1, 4, 6, 8] Empty in
  let sorted = inOrder tree in
  let a = List.foldl (acc x -> acc * 10 + x) 0 sorted in  -- 1345678

  let b = if contains 4 tree then 1 else 0 in   -- 1
  let c = if contains 9 tree then 1 else 0 in   -- 0
  let d = countNodes tree in                     -- 7
  let e = height tree in                         -- 3

  a + b + c + d + e                         -- 1345689

-- =============================================================================
-- Final Result
-- =============================================================================

let result =
  testSortingAlgorithms +                   -- 136267
  testSearchingAlgorithms +                 -- 5
  testNumericAlgorithms +                   -- 67
  testTreeAlgorithms                        -- 1345689
                                            -- Total: 1482028

let main = args -> IO.printLine (Int.toString result)
