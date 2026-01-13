-- Algorithms and Data Structures

let rec insert = x sorted -> match sorted
  when Nil -> x :: Nil
  when Cons y rest -> if x <= y then x :: sorted else y :: insert x rest
end

let rec insertionSort = xs -> match xs when Nil -> Nil when Cons x rest -> insert x (insertionSort rest) end

let rec partition = pivot xs -> match xs
  when Nil -> (Nil, Nil)
  when Cons y rest -> let (lt, ge) = partition pivot rest in if y < pivot then (y :: lt, ge) else (lt, y :: ge)
end

let rec quickSort = xs -> match xs
  when Nil -> Nil
  when Cons pivot rest -> let (lt, ge) = partition pivot rest in List.concat [quickSort lt, pivot :: Nil, quickSort ge]
end

let rec linearSearch = target xs -> match xs
  when Nil -> Nothing
  when Cons x rest -> if x == target then Just x else linearSearch target rest
end

let rec findIndex = target idx xs -> match xs
  when Nil -> Nothing
  when Cons x rest -> if x == target then Just idx else findIndex target (idx + 1) rest
end

let rec countOccurrences = target xs -> match xs
  when Nil -> 0
  when Cons x rest -> (if x == target then 1 else 0) + countOccurrences target rest
end

let rec gcd = a b -> if b == 0 then a else gcd b (Int.mod a b)

let lcm = a b -> Int.div (a * b) (gcd a b)

let rec isPrimeHelper = n divisor -> if divisor * divisor > n then true else if Int.mod n divisor == 0 then false else isPrimeHelper n (divisor + 1)

let isPrime = n -> if n < 2 then false else isPrimeHelper n 2

let rec sumDivisors = n d -> if d > Int.div n 2 then 0 else if Int.mod n d == 0 then d + sumDivisors n (d + 1) else sumDivisors n (d + 1)

let rec countPrimes = n count -> if n < 2 then count else countPrimes (n - 1) (if isPrime n then count + 1 else count)

type BST a = BSTEmpty | BSTNode a (BST a) (BST a)

let rec bstInsert = x tree -> match tree
  when BSTEmpty -> BSTNode x BSTEmpty BSTEmpty
  when BSTNode v left right -> if x < v then BSTNode v (bstInsert x left) right else if x > v then BSTNode v left (bstInsert x right) else tree
end

let rec buildBST = xs tree -> match xs when Nil -> tree when Cons x rest -> buildBST rest (bstInsert x tree) end

let rec inOrder = tree -> match tree
  when BSTEmpty -> Nil
  when BSTNode v left right -> List.concat [inOrder left, v :: Nil, inOrder right]
end

let rec contains = x tree -> match tree
  when BSTEmpty -> false
  when BSTNode v left right -> if x == v then true else if x < v then contains x left else contains x right
end

let rec countNodes = tree -> match tree when BSTEmpty -> 0 when BSTNode _ left right -> 1 + countNodes left + countNodes right end

let rec height = tree -> match tree
  when BSTEmpty -> 0
  when BSTNode _ left right -> 1 + (if height left > height right then height left else height right)
end

let tests =
  let tree = buildBST [5, 3, 7, 1, 4, 6, 8] BSTEmpty in
  [ Test.equal "insertionSort" [1, 2, 5, 8, 9] (insertionSort [5, 2, 8, 1, 9])
  , Test.equal "quickSort" [1, 2, 3, 6, 7, 8] (quickSort [3, 7, 1, 8, 2, 6])
  , Test.equal "linearSearch found" (Just 5) (linearSearch 5 [1, 3, 5, 7, 9])
  , Test.equal "linearSearch not found" Nothing (linearSearch 4 [1, 3, 5, 7, 9])
  , Test.equal "findIndex found" (Just 2) (findIndex 5 0 [1, 3, 5, 7, 9])
  , Test.equal "findIndex not found" Nothing (findIndex 4 0 [1, 3, 5, 7, 9])
  , Test.equal "countOccurrences" 3 (countOccurrences 2 [1, 2, 3, 2, 4, 2, 5])
  , Test.equal "gcd" 6 (gcd 48 18)
  , Test.equal "lcm" 36 (lcm 12 18)
  , Test.ok "17 is prime" (isPrime 17)
  , Test.ok "15 not prime" (Bool.negate (isPrime 15))
  , Test.equal "sumDivisors 12" 16 (sumDivisors 12 1)
  , Test.equal "primes up to 20" 8 (countPrimes 20 0)
  , Test.equal "inOrder" [1, 3, 4, 5, 6, 7, 8] (inOrder tree)
  , Test.ok "contains 4" (contains 4 tree)
  , Test.ok "not contains 9" (Bool.negate (contains 9 tree))
  , Test.equal "countNodes" 7 (countNodes tree)
  , Test.equal "height" 3 (height tree)
  ]

let main = args -> Test.run tests
