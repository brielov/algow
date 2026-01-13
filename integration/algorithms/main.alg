-- Algorithms and Data Structures

let testSortingAlgorithms =
  let rec insert = x sorted -> match sorted
    when Nil -> x :: Nil
    when Cons y rest -> if x <= y then x :: sorted else y :: insert x rest
  end in
  let rec insertionSort = xs -> match xs when Nil -> Nil when Cons x rest -> insert x (insertionSort rest) end in
  let a = Assert.equal [1, 2, 5, 8, 9] (insertionSort [5, 2, 8, 1, 9]) "insertionSort" in
  let rec partition = pivot xs -> match xs
    when Nil -> (Nil, Nil)
    when Cons y rest -> let (lt, ge) = partition pivot rest in if y < pivot then (y :: lt, ge) else (lt, y :: ge)
  end in
  let rec quickSort = xs -> match xs
    when Nil -> Nil
    when Cons pivot rest -> let (lt, ge) = partition pivot rest in List.concat [quickSort lt, pivot :: Nil, quickSort ge]
  end in
  let b = Assert.equal [1, 2, 3, 6, 7, 8] (quickSort [3, 7, 1, 8, 2, 6]) "quickSort" in
  ()

let testSearchingAlgorithms =
  let rec linearSearch = target xs -> match xs
    when Nil -> Nothing
    when Cons x rest -> if x == target then Just x else linearSearch target rest
  end in
  let a = Assert.equal (Just 5) (linearSearch 5 [1, 3, 5, 7, 9]) "linearSearch found" in
  let b = Assert.equal Nothing (linearSearch 4 [1, 3, 5, 7, 9]) "linearSearch not found" in
  let rec findIndex = target idx xs -> match xs
    when Nil -> Nothing
    when Cons x rest -> if x == target then Just idx else findIndex target (idx + 1) rest
  end in
  let c = Assert.equal (Just 2) (findIndex 5 0 [1, 3, 5, 7, 9]) "findIndex found" in
  let d = Assert.equal Nothing (findIndex 4 0 [1, 3, 5, 7, 9]) "findIndex not found" in
  let rec countOccurrences = target xs -> match xs
    when Nil -> 0
    when Cons x rest -> (if x == target then 1 else 0) + countOccurrences target rest
  end in
  let e = Assert.equal 3 (countOccurrences 2 [1, 2, 3, 2, 4, 2, 5]) "countOccurrences" in
  ()

let testNumericAlgorithms =
  let rec gcd = a b -> if b == 0 then a else gcd b (Int.mod a b) in
  let a = Assert.equal 6 (gcd 48 18) "gcd" in
  let lcm = a b -> Int.div (a * b) (gcd a b) in
  let b = Assert.equal 36 (lcm 12 18) "lcm" in
  let rec isPrimeHelper = n divisor -> if divisor * divisor > n then true else if Int.mod n divisor == 0 then false else isPrimeHelper n (divisor + 1) in
  let isPrime = n -> if n < 2 then false else isPrimeHelper n 2 in
  let c = Assert.ok (isPrime 17) "17 is prime" in
  let d = Assert.ok (Bool.negate (isPrime 15)) "15 not prime" in
  let rec sumDivisors = n d -> if d > Int.div n 2 then 0 else if Int.mod n d == 0 then d + sumDivisors n (d + 1) else sumDivisors n (d + 1) in
  let e = Assert.equal 16 (sumDivisors 12 1) "sumDivisors 12" in
  let rec countPrimes = n count -> if n < 2 then count else countPrimes (n - 1) (if isPrime n then count + 1 else count) in
  let f = Assert.equal 8 (countPrimes 20 0) "primes up to 20" in
  ()

type BST a = BSTEmpty | BSTNode a (BST a) (BST a)

let testTreeAlgorithms =
  let rec insert = x tree -> match tree
    when BSTEmpty -> BSTNode x BSTEmpty BSTEmpty
    when BSTNode v left right -> if x < v then BSTNode v (insert x left) right else if x > v then BSTNode v left (insert x right) else tree
  end in
  let rec buildBST = xs tree -> match xs when Nil -> tree when Cons x rest -> buildBST rest (insert x tree) end in
  let rec inOrder = tree -> match tree
    when BSTEmpty -> Nil
    when BSTNode v left right -> List.concat [inOrder left, v :: Nil, inOrder right]
  end in
  let rec contains = x tree -> match tree
    when BSTEmpty -> false
    when BSTNode v left right -> if x == v then true else if x < v then contains x left else contains x right
  end in
  let rec countNodes = tree -> match tree when BSTEmpty -> 0 when BSTNode _ left right -> 1 + countNodes left + countNodes right end in
  let rec height = tree -> match tree
    when BSTEmpty -> 0
    when BSTNode _ left right -> 1 + (if height left > height right then height left else height right)
  end in
  let tree = buildBST [5, 3, 7, 1, 4, 6, 8] BSTEmpty in
  let a = Assert.equal [1, 3, 4, 5, 6, 7, 8] (inOrder tree) "inOrder" in
  let b = Assert.ok (contains 4 tree) "contains 4" in
  let c = Assert.ok (Bool.negate (contains 9 tree)) "not contains 9" in
  let d = Assert.equal 7 (countNodes tree) "countNodes" in
  let e = Assert.equal 3 (height tree) "height" in
  ()

let main = args ->
  let a = testSortingAlgorithms in
  let b = testSearchingAlgorithms in
  let c = testNumericAlgorithms in
  let d = testTreeAlgorithms in
  IO.printLine "algorithms: ok"
