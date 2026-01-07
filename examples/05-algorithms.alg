-- Classic Algorithms
-- Demonstrates well-known algorithms in functional style

-- =============================================================================
-- HELPER FUNCTIONS
-- =============================================================================

let rec append xs ys = match xs
  when Nil -> ys
  when Cons x rest -> Cons x (append rest ys)
end

let rec len xs = match xs
  when Nil -> 0
  when Cons h rest -> 1 + len rest
end

let rec nth n xs = match xs
  when Nil -> Nothing
  when Cons x rest ->
    if n == 0 then Just x
    else nth (n - 1) rest
end

let rec take n xs =
  if n <= 0 then Nil
  else match xs
    when Nil -> Nil
    when Cons x rest -> Cons x (take (n - 1) rest)
  end

let rec drop n xs =
  if n <= 0 then xs
  else match xs
    when Nil -> Nil
    when Cons h rest -> drop (n - 1) rest
  end

let rec zip xs ys = match xs
  when Nil -> Nil
  when Cons x xrest -> match ys
    when Nil -> Nil
    when Cons y yrest -> Cons (x, y) (zip xrest yrest)
  end
end

-- =============================================================================
-- BINARY SEARCH
-- =============================================================================

let binarySearch target xs =
  let rec go low high =
    if low > high then Nothing
    else
      let mid = (low + high) / 2 in
      match nth mid xs
        when Nothing -> Nothing
        when Just v ->
          if v == target then Just mid
          else if v < target then go (mid + 1) high
          else go low (mid - 1)
      end
  in go 0 (len xs - 1)

-- =============================================================================
-- FAST EXPONENTIATION
-- =============================================================================

-- Fast exponentiation O(log n)
let rec fastPow base exp =
  if exp == 0 then 1
  else if exp == 1 then base
  else
    let half = fastPow base (exp / 2) in
    let halfSquared = half * half in
    -- Check if exp is even using subtraction
    let doubled = (exp / 2) * 2 in
    if doubled == exp
    then halfSquared
    else halfSquared * base

-- =============================================================================
-- NEWTON-RAPHSON SQUARE ROOT
-- =============================================================================

let isqrt n =
  let rec improve guess =
    let nextGuess = (guess + n / guess) / 2 in
    if nextGuess >= guess then guess
    else improve nextGuess
  in
  if n <= 0 then 0
  else if n == 1 then 1
  else improve (n / 2)

-- =============================================================================
-- RUN-LENGTH ENCODING
-- =============================================================================

let rec rleEncode xs = match xs
  when Nil -> Nil
  when Cons x rest ->
    let rec countRun val count xs = match xs
      when Nil -> (count, Nil)
      when Cons y rest ->
        if y == val
        then countRun val (count + 1) rest
        else (count, xs)
    end in
    match countRun x 1 rest
      when (count, remaining) ->
        Cons (x, count) (rleEncode remaining)
    end
end

let rec rleDecode xs = match xs
  when Nil -> Nil
  when Cons pair rest -> match pair
    when (val, count) ->
      let rec replicate n x =
        if n <= 0 then Nil
        else Cons x (replicate (n - 1) x)
      in append (replicate count val) (rleDecode rest)
  end
end

-- =============================================================================
-- TESTING
-- =============================================================================

let sortedList = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]

in {
  -- Binary Search
  find7 = binarySearch 7 sortedList,
  find8 = binarySearch 8 sortedList,
  find1 = binarySearch 1 sortedList,
  find19 = binarySearch 19 sortedList,

  -- Helper functions
  takeTest = take 3 [1, 2, 3, 4, 5],
  dropTest = drop 2 [1, 2, 3, 4, 5],
  zipTest = zip [1, 2, 3] [4, 5, 6],

  -- Power
  pow2_8 = fastPow 2 8,
  pow3_4 = fastPow 3 4,

  -- Square root
  sqrt16 = isqrt 16,
  sqrt100 = isqrt 100,
  sqrt50 = isqrt 50,

  -- Run-length encoding
  rleEncoded = rleEncode [1, 1, 1, 2, 2, 3, 3, 3, 3, 1],
  rleDecoded = rleDecode [(1, 3), (2, 2), (3, 4)]
}
