-- Higher-Order Functions
-- Demonstrates functional programming patterns

-- =============================================================================
-- BASIC COMBINATORS
-- =============================================================================

-- Note: id, const, compose, flip are in prelude
-- Here we define aliases for demonstration

-- Apply function to value
let apply f x = f x

-- =============================================================================
-- LIST COMBINATORS
-- =============================================================================

-- Map with index
let rec mapIndexed f xs =
  let rec go i xs = match xs
    when Nil -> Nil
    when Cons x rest -> Cons (f i x) (go (i + 1) rest)
  end
  in go 0 xs

-- Flat map (bind for lists)
let rec flatMap f xs = match xs
  when Nil -> Nil
  when Cons x rest ->
    let rec append ys zs = match ys
      when Nil -> zs
      when Cons y rest -> Cons y (append rest zs)
    end
    in append (f x) (flatMap f rest)
end

-- Take while predicate holds
let rec takeWhile p xs = match xs
  when Nil -> Nil
  when Cons x rest ->
    if p x
    then Cons x (takeWhile p rest)
    else Nil
end

-- Drop while predicate holds
let rec dropWhile p xs = match xs
  when Nil -> Nil
  when Cons x rest ->
    if p x
    then dropWhile p rest
    else xs
end

-- Span: split at first element not satisfying predicate
let rec span p xs = match xs
  when Nil -> (Nil, Nil)
  when Cons x rest ->
    if p x
    then match span p rest
      when (yes, no) -> (Cons x yes, no)
    end
    else (Nil, xs)
end

-- Group consecutive equal elements
let rec groupBy eq xs = match xs
  when Nil -> Nil
  when Cons x rest ->
    match span (y -> eq x y) rest
      when (same, different) ->
        Cons (Cons x same) (groupBy eq different)
    end
end

-- Zip with function
let rec zipWith f xs ys = match xs
  when Nil -> Nil
  when Cons x xrest -> match ys
    when Nil -> Nil
    when Cons y yrest -> Cons (f x y) (zipWith f xrest yrest)
  end
end

-- Scan left (running fold)
let rec scanLeft f z xs = match xs
  when Nil -> [z]
  when Cons x rest ->
    let next = f z x in
    Cons z (scanLeft f next rest)
end

-- Unfold: generate list from seed
let rec unfold f seed = match f seed
  when Nothing -> Nil
  when Just pair -> match pair
    when (val, nextSeed) -> Cons val (unfold f nextSeed)
  end
end

-- =============================================================================
-- MAYBE COMBINATORS
-- =============================================================================

-- Maybe map
let mapMaybe f m = match m
  when Nothing -> Nothing
  when Just x -> Just (f x)
end

-- Maybe bind (flatMap)
let bindMaybe f m = match m
  when Nothing -> Nothing
  when Just x -> f x
end

-- Maybe apply
let applyMaybe mf mx = match mf
  when Nothing -> Nothing
  when Just f -> mapMaybe f mx
end

-- Sequence list of maybes
let rec sequenceMaybes xs = match xs
  when Nil -> Just Nil
  when Cons m rest ->
    bindMaybe (x ->
      mapMaybe (rs -> Cons x rs) (sequenceMaybes rest)
    ) m
end

-- Filter map (map with optional result)
let rec filterMap f xs = match xs
  when Nil -> Nil
  when Cons x rest -> match f x
    when Nothing -> filterMap f rest
    when Just y -> Cons y (filterMap f rest)
  end
end

-- =============================================================================
-- CURRYING AND PARTIAL APPLICATION
-- =============================================================================

-- Add (curried)
let add x y = x + y

-- Partially applied functions
let add5 = add 5
let add10 = add 10

-- Multiply (curried)
let mul x y = x * y
let double = mul 2
let triple = mul 3

-- =============================================================================
-- FUNCTION PIPELINES
-- =============================================================================

-- Process numbers: double, add 1, square
let pipeline xs =
  let doubled = map double xs in
  let incremented = map (x -> x + 1) doubled in
  let squared = map (x -> x * x) incremented in
  squared

-- Same using composition
let processNum = compose (x -> x * x) (compose (x -> x + 1) double)

-- =============================================================================
-- TESTING
-- =============================================================================

let nums = [1, 2, 3, 4, 5]

in {
  -- Basic combinators (using prelude)
  identity = id 42,
  composed = compose (x -> x * x) (x -> x + 1) 5,
  flipped = flip (x y -> x - y) 3 10,

  -- List combinators
  indexed = mapIndexed (i x -> (i, x)) nums,
  flatMapped = flatMap (x -> [x, x * 10]) [1, 2, 3],
  takenWhile = takeWhile (x -> x < 4) nums,
  droppedWhile = dropWhile (x -> x < 3) nums,
  spanned = span (x -> x < 3) nums,
  grouped = groupBy (x y -> x == y) [1, 1, 2, 2, 2, 3, 1, 1],
  zippedWith = zipWith add [1, 2, 3] [10, 20, 30],
  scanned = scanLeft add 0 [1, 2, 3, 4],

  -- Unfold: generate [1, 2, 3, 4, 5]
  unfolded = unfold (n -> if n > 5 then Nothing else Just (n, n + 1)) 1,

  -- Maybe combinators
  mapped = mapMaybe double (Just 5),
  bound = bindMaybe (x -> if x > 0 then Just (x * 2) else Nothing) (Just 5),
  sequenced = sequenceMaybes [Just 1, Just 2, Just 3],
  sequencedFail = sequenceMaybes [Just 1, Nothing, Just 3],
  filterMapped = filterMap (x -> if x > 2 then Just (x * 10) else Nothing) nums,

  -- Partial application
  added5 = add5 10,
  added10 = add10 5,
  doubled = map double nums,
  tripled = map triple nums,

  -- Pipelines
  pipelined = pipeline [1, 2, 3],
  processed = map processNum [1, 2, 3]
}
