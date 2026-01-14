-- Recursion: All Recursive Patterns and Techniques
type BinTree a
  = Empty
  | Branch a (BinTree a) (BinTree a)

let rec factorial = n -> if n <= 1
  then 1
  else n * factorial (n - 1)

let rec fib = n -> if n <= 1
  then n
  else fib (n - 1) + fib (n - 2)

let rec sumTo = n -> if n <= 0
  then 0
  else n + sumTo (n - 1)

let rec pow = base exp_ -> if exp_ <= 0
  then 1
  else base * pow base (exp_ - 1)

let rec factTail = acc n -> if n <= 1
  then acc
  else factTail (acc * n) (n - 1)

let rec fibTail = a b n -> if n <= 0
  then a
  else fibTail b (a + b) (n - 1)

let rec sumTail = acc n -> if n <= 0
  then acc
  else sumTail (acc + n) (n - 1)

let rec lengthTail = acc xs ->
  match xs
    when Nil -> acc
    when Cons _ rest -> lengthTail (acc + 1) rest
  end

let rec reverseTail = acc xs ->
  match xs
    when Nil -> acc
    when Cons x rest -> reverseTail (x :: acc) rest
  end

let rec isEven = n -> if n == 0
  then true
  else isOdd (n - 1)
and isOdd = n -> if n == 0
  then false
  else isEven (n - 1)

let rec processOdd = n -> if n <= 0
  then 0
  else n + processEven (n - 1)
and processEven = n -> if n <= 0
  then 0
  else processOdd (n - 1)

let rec sumList = xs ->
  match xs
    when Nil -> 0
    when Cons x rest -> x + sumList rest
  end

let rec prodList = xs ->
  match xs
    when Nil -> 1
    when Cons x rest -> x * prodList rest
  end

let rec maxList = xs ->
  match xs
    when Nil -> 0
    when Cons x Nil -> x
    when Cons x rest -> let m = maxList rest in
    if x > m
      then x
      else m
  end

let rec countOccur = target xs ->
  match xs
    when Nil -> 0
    when Cons x rest -> (if x == target
      then 1
      else 0) + countOccur target rest
  end

let rec countNodes = tree ->
  match tree
    when Empty -> 0
    when Branch _ left right -> 1 + countNodes left + countNodes right
  end

let rec sumNodes = tree ->
  match tree
    when Empty -> 0
    when Branch x left right -> x + sumNodes left + sumNodes right
  end

let rec treeDepth = tree ->
  match tree
    when Empty -> 0
    when Branch _ left right -> 1 + (if treeDepth left > treeDepth right
      then treeDepth left
      else treeDepth right)
  end

let rec fold = f z xs ->
  match xs
    when Nil -> z
    when Cons x rest -> f x (fold f z rest)
  end

let rec map_ = f xs ->
  match xs
    when Nil -> Nil
    when Cons x rest -> f x :: map_ f rest
  end

let rec filter_ = p xs ->
  match xs
    when Nil -> Nil
    when Cons x rest -> if p x
      then x :: filter_ p rest
      else filter_ p rest
  end

let rec takeWhile = p xs ->
  match xs
    when Nil -> Nil
    when Cons x rest -> if p x
      then x :: takeWhile p rest
      else Nil
  end

let rec gcd = a b -> if b == 0
  then a
  else gcd b (Int.mod a b)

let rec digitSum = n -> if n <= 0
  then 0
  else Int.mod n 10 + digitSum (Int.div n 10)

let rec countDigits = n -> if n < 10
  then 1
  else 1 + countDigits (Int.div n 10)

let rec reverseInt = acc n -> if n <= 0
  then acc
  else reverseInt (acc * 10 + Int.mod n 10) (Int.div n 10)

let rec range = start end_ -> if start >= end_
  then Nil
  else start :: range (start + 1) end_

let rec replicate = n x -> if n <= 0
  then Nil
  else x :: replicate (n - 1) x

let rec interleave = xs ys ->
  match xs
    when Nil -> ys
    when Cons x rest -> x :: interleave ys rest
  end

let rec merge = xs ys ->
  match xs
    when Nil -> ys
    when Cons x xrest -> match ys
      when Nil -> xs
      when Cons y yrest -> if x <= y
        then x :: merge xrest ys
        else y :: merge xs yrest
    end
  end

let rec splitAt = n xs -> if n <= 0
  then (Nil, xs)
  else match xs
    when Nil -> (Nil, Nil)
    when Cons x rest -> match splitAt (n - 1) rest
      when (left, right) -> (x :: left, right)
    end
  end

let rec mergeSort = xs ->
  match xs
    when Nil -> Nil
    when Cons _ Nil -> xs
    when _ -> let len = List.length xs in
    match splitAt (Int.div len 2) xs
      when (left, right) -> merge (mergeSort left) (mergeSort right)
    end
  end

let rec isSorted = xs ->
  match xs
    when Nil -> true
    when Cons _ Nil -> true
    when Cons x (Cons y rest) -> if x <= y
      then isSorted (y :: rest)
      else false
  end

let tests =
  let tree = Branch 10 (Branch 5 (Branch 3 Empty Empty) (Branch 7 Empty Empty)) (Branch 15 Empty (Branch 20 Empty Empty)) in
  let sorted = mergeSort [5, 2, 8, 1, 9, 3, 7, 4, 6] in
  [ Test.equal "factorial 5" 120 (factorial 5)
    , Test.equal "factorial 0" 1 (factorial 0)
    , Test.equal "fib 10" 55 (fib 10)
    , Test.equal "sumTo 10" 55 (sumTo 10)
    , Test.equal "pow 2 10" 1024 (pow 2 10)
    , Test.equal "tail factorial 5" 120 (factTail 1 5)
    , Test.equal "tail fib 10" 55 (fibTail 0 1 10)
    , Test.equal "tail sum 100" 5050 (sumTail 0 100)
    , Test.equal "tail length" 5 (lengthTail 0 [1, 2, 3, 4, 5])
    , Test.equal "tail reverse" [3, 2, 1] (reverseTail Nil [1, 2, 3])
    , Test.ok "10 is even" (isEven 10)
    , Test.ok "10 not odd" (Bool.negate (isOdd 10))
    , Test.ok "7 not even" (Bool.negate (isEven 7))
    , Test.ok "7 is odd" (isOdd 7)
    , Test.equal "processOdd 10" 30 (processOdd 10)
    , Test.equal "processEven 10" 25 (processEven 10)
    , Test.equal "sumList" 15 (sumList [1, 2, 3, 4, 5])
    , Test.equal "prodList" 24 (prodList [1, 2, 3, 4])
    , Test.equal "maxList" 9 (maxList [3, 1, 4, 1, 5, 9, 2, 6])
    , Test.equal "countOccur" 4 (countOccur 1 [1, 2, 1, 3, 1, 4, 1])
    , Test.equal "countNodes" 6 (countNodes tree)
    , Test.equal "sumNodes" 60 (sumNodes tree)
    , Test.equal "treeDepth" 3 (treeDepth tree)
    , Test.equal "fold sum" 15 (fold (x acc -> x + acc) 0 [1, 2, 3, 4, 5])
    , Test.equal "map double" [2, 4, 6] (map_ (x -> x * 2) [1, 2, 3])
    , Test.equal "filter even" [2, 4, 6] (filter_ (x -> Int.mod x 2 == 0) [1, 2, 3, 4, 5, 6])
    , Test.equal "takeWhile" [1, 2, 3, 4] (takeWhile (x -> x < 5) [1, 2, 3, 4, 5, 6, 7])
    , Test.equal "gcd 48 18" 6 (gcd 48 18)
    , Test.equal "gcd 100 35" 5 (gcd 100 35)
    , Test.equal "digitSum" 15 (digitSum 12345)
    , Test.equal "countDigits" 5 (countDigits 12345)
    , Test.equal "reverseInt" 54321 (reverseInt 0 12345)
    , Test.equal "range" [1, 2, 3, 4, 5] (range 1 6)
    , Test.equal "replicate" [10, 10, 10, 10, 10] (replicate 5 10)
    , Test.equal "interleave" [1, 2, 3, 4, 5, 6] (interleave [1, 3, 5] [2, 4, 6])
    , Test.equal "mergeSort" [1, 2, 3, 4, 5, 6, 7, 8, 9] sorted
    , Test.ok "isSorted" (isSorted sorted)
    ]

let main =
  args -> Test.run tests
