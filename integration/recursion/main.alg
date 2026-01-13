-- Recursion: All Recursive Patterns and Techniques

let testBasicRecursion =
  let rec factorial = n -> if n <= 1 then 1 else n * factorial (n - 1) in
  let a = Assert.equal 120 (factorial 5) "factorial 5" in
  let b = Assert.equal 1 (factorial 0) "factorial 0" in
  let rec fib = n -> if n <= 1 then n else fib (n - 1) + fib (n - 2) in
  let c = Assert.equal 55 (fib 10) "fib 10" in
  let rec sumTo = n -> if n <= 0 then 0 else n + sumTo (n - 1) in
  let d = Assert.equal 55 (sumTo 10) "sumTo 10" in
  let rec pow = base exp_ -> if exp_ <= 0 then 1 else base * pow base (exp_ - 1) in
  let e = Assert.equal 1024 (pow 2 10) "pow 2 10" in
  ()

let testTailRecursion =
  let rec factTail = acc n -> if n <= 1 then acc else factTail (acc * n) (n - 1) in
  let a = Assert.equal 120 (factTail 1 5) "tail factorial 5" in
  let rec fibTail = a b n -> if n <= 0 then a else fibTail b (a + b) (n - 1) in
  let b = Assert.equal 55 (fibTail 0 1 10) "tail fib 10" in
  let rec sumTail = acc n -> if n <= 0 then acc else sumTail (acc + n) (n - 1) in
  let c = Assert.equal 5050 (sumTail 0 100) "tail sum 100" in
  let rec lengthTail = acc xs -> match xs when Nil -> acc when Cons _ rest -> lengthTail (acc + 1) rest end in
  let d = Assert.equal 5 (lengthTail 0 [1, 2, 3, 4, 5]) "tail length" in
  let rec reverseTail = acc xs -> match xs when Nil -> acc when Cons x rest -> reverseTail (x :: acc) rest end in
  let e = Assert.equal [3, 2, 1] (reverseTail Nil [1, 2, 3]) "tail reverse" in
  ()

let testMutualRecursion =
  let rec isEven = n -> if n == 0 then true else isOdd (n - 1)
  and isOdd = n -> if n == 0 then false else isEven (n - 1) in
  let a = Assert.ok (isEven 10) "10 is even" in
  let b = Assert.ok (Bool.negate (isOdd 10)) "10 not odd" in
  let c = Assert.ok (Bool.negate (isEven 7)) "7 not even" in
  let d = Assert.ok (isOdd 7) "7 is odd" in
  let rec processOdd = n -> if n <= 0 then 0 else n + processEven (n - 1)
  and processEven = n -> if n <= 0 then 0 else processOdd (n - 1) in
  let e = Assert.equal 30 (processOdd 10) "processOdd 10" in
  let f = Assert.equal 25 (processEven 10) "processEven 10" in
  ()

let testListRecursion =
  let rec sumList = xs -> match xs when Nil -> 0 when Cons x rest -> x + sumList rest end in
  let a = Assert.equal 15 (sumList [1, 2, 3, 4, 5]) "sumList" in
  let rec prodList = xs -> match xs when Nil -> 1 when Cons x rest -> x * prodList rest end in
  let b = Assert.equal 24 (prodList [1, 2, 3, 4]) "prodList" in
  let rec maxList = xs -> match xs
    when Nil -> 0
    when Cons x Nil -> x
    when Cons x rest -> let m = maxList rest in if x > m then x else m
  end in
  let c = Assert.equal 9 (maxList [3, 1, 4, 1, 5, 9, 2, 6]) "maxList" in
  let rec countOccur = target xs -> match xs
    when Nil -> 0
    when Cons x rest -> (if x == target then 1 else 0) + countOccur target rest
  end in
  let d = Assert.equal 4 (countOccur 1 [1, 2, 1, 3, 1, 4, 1]) "countOccur" in
  ()

type BinTree a = Empty | Branch a (BinTree a) (BinTree a)

let testTreeRecursion =
  let rec countNodes = tree -> match tree when Empty -> 0 when Branch _ left right -> 1 + countNodes left + countNodes right end in
  let rec sumNodes = tree -> match tree when Empty -> 0 when Branch x left right -> x + sumNodes left + sumNodes right end in
  let rec treeDepth = tree -> match tree
    when Empty -> 0
    when Branch _ left right -> 1 + (if treeDepth left > treeDepth right then treeDepth left else treeDepth right)
  end in
  let tree = Branch 10 (Branch 5 (Branch 3 Empty Empty) (Branch 7 Empty Empty)) (Branch 15 Empty (Branch 20 Empty Empty)) in
  let a = Assert.equal 6 (countNodes tree) "countNodes" in
  let b = Assert.equal 60 (sumNodes tree) "sumNodes" in
  let c = Assert.equal 3 (treeDepth tree) "treeDepth" in
  ()

let testHigherOrderRecursion =
  let rec fold = f z xs -> match xs when Nil -> z when Cons x rest -> f x (fold f z rest) end in
  let a = Assert.equal 15 (fold (x acc -> x + acc) 0 [1, 2, 3, 4, 5]) "fold sum" in
  let rec map_ = f xs -> match xs when Nil -> Nil when Cons x rest -> f x :: map_ f rest end in
  let b = Assert.equal [2, 4, 6] (map_ (x -> x * 2) [1, 2, 3]) "map double" in
  let rec filter_ = p xs -> match xs when Nil -> Nil when Cons x rest -> if p x then x :: filter_ p rest else filter_ p rest end in
  let c = Assert.equal [2, 4, 6] (filter_ (x -> Int.mod x 2 == 0) [1, 2, 3, 4, 5, 6]) "filter even" in
  let rec takeWhile = p xs -> match xs when Nil -> Nil when Cons x rest -> if p x then x :: takeWhile p rest else Nil end in
  let d = Assert.equal [1, 2, 3, 4] (takeWhile (x -> x < 5) [1, 2, 3, 4, 5, 6, 7]) "takeWhile" in
  ()

let testNumericRecursion =
  let rec gcd = a b -> if b == 0 then a else gcd b (Int.mod a b) in
  let a = Assert.equal 6 (gcd 48 18) "gcd 48 18" in
  let b = Assert.equal 5 (gcd 100 35) "gcd 100 35" in
  let rec digitSum = n -> if n <= 0 then 0 else Int.mod n 10 + digitSum (Int.div n 10) in
  let c = Assert.equal 15 (digitSum 12345) "digitSum" in
  let rec countDigits = n -> if n < 10 then 1 else 1 + countDigits (Int.div n 10) in
  let d = Assert.equal 5 (countDigits 12345) "countDigits" in
  let rec reverseInt = acc n -> if n <= 0 then acc else reverseInt (acc * 10 + Int.mod n 10) (Int.div n 10) in
  let e = Assert.equal 54321 (reverseInt 0 12345) "reverseInt" in
  ()

let testBuildingRecursion =
  let rec range = start end_ -> if start >= end_ then Nil else start :: range (start + 1) end_ in
  let a = Assert.equal [1, 2, 3, 4, 5] (range 1 6) "range" in
  let rec replicate = n x -> if n <= 0 then Nil else x :: replicate (n - 1) x in
  let b = Assert.equal [10, 10, 10, 10, 10] (replicate 5 10) "replicate" in
  let rec interleave = xs ys -> match xs when Nil -> ys when Cons x rest -> x :: interleave ys rest end in
  let c = Assert.equal [1, 2, 3, 4, 5, 6] (interleave [1, 3, 5] [2, 4, 6]) "interleave" in
  ()

let testDivideAndConquer =
  let rec merge = xs ys -> match xs
    when Nil -> ys
    when Cons x xrest -> match ys
      when Nil -> xs
      when Cons y yrest -> if x <= y then x :: merge xrest ys else y :: merge xs yrest
    end
  end in
  let rec splitAt = n xs -> if n <= 0 then (Nil, xs) else match xs
    when Nil -> (Nil, Nil)
    when Cons x rest -> let (left, right) = splitAt (n - 1) rest in (x :: left, right)
  end in
  let rec mergeSort = xs -> match xs
    when Nil -> Nil
    when Cons _ Nil -> xs
    when _ -> let len = List.length xs in let (left, right) = splitAt (Int.div len 2) xs in merge (mergeSort left) (mergeSort right)
  end in
  let sorted = mergeSort [5, 2, 8, 1, 9, 3, 7, 4, 6] in
  let a = Assert.equal [1, 2, 3, 4, 5, 6, 7, 8, 9] sorted "mergeSort" in
  let rec isSorted = xs -> match xs
    when Nil -> true
    when Cons _ Nil -> true
    when Cons x (Cons y rest) -> if x <= y then isSorted (y :: rest) else false
  end in
  let b = Assert.ok (isSorted sorted) "isSorted" in
  ()

let main = args ->
  let a = testBasicRecursion in
  let b = testTailRecursion in
  let c = testMutualRecursion in
  let d = testListRecursion in
  let e = testTreeRecursion in
  let f = testHigherOrderRecursion in
  let g = testNumericRecursion in
  let h = testBuildingRecursion in
  let i = testDivideAndConquer in
  IO.printLine "recursion: ok"
