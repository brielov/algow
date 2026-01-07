-- Sorting Algorithms
-- Demonstrates various sorting algorithms in a functional style

-- =============================================================================
-- HELPER FUNCTIONS
-- =============================================================================

-- Append two lists
let rec append xs ys = match xs
  when Nil -> ys
  when Cons x rest -> Cons x (append rest ys)
end

-- Filter list by predicate
let rec filterBy p xs = match xs
  when Nil -> Nil
  when Cons x rest ->
    if p x
    then Cons x (filterBy p rest)
    else filterBy p rest
end

-- =============================================================================
-- INSERTION SORT
-- =============================================================================

-- Insert element into sorted list
let rec insertSorted x xs = match xs
  when Nil -> Cons x Nil
  when Cons y rest ->
    if x <= y
    then Cons x xs
    else Cons y (insertSorted x rest)
end

-- Insertion sort
let rec insertionSort xs = match xs
  when Nil -> Nil
  when Cons x rest -> insertSorted x (insertionSort rest)
end

-- =============================================================================
-- QUICKSORT
-- =============================================================================

-- Quicksort using filter
let rec quicksort xs = match xs
  when Nil -> Nil
  when Cons pivot rest ->
    let smaller = filterBy (x -> x < pivot) rest in
    let larger = filterBy (x -> x >= pivot) rest in
    append (quicksort smaller) (Cons pivot (quicksort larger))
end

-- =============================================================================
-- MERGE SORT
-- =============================================================================

-- Merge two sorted lists
let rec merge xs ys = match xs
  when Nil -> ys
  when Cons x xrest -> match ys
    when Nil -> xs
    when Cons y yrest ->
      if x <= y
      then Cons x (merge xrest ys)
      else Cons y (merge xs yrest)
  end
end

-- Split list into two halves
let rec splitAt n xs =
  if n <= 0
  then (Nil, xs)
  else match xs
    when Nil -> (Nil, Nil)
    when Cons x rest ->
      let result = splitAt (n - 1) rest in
      match result
        when (left, right) -> (Cons x left, right)
      end
  end

-- Length of list
let rec len xs = match xs
  when Nil -> 0
  when Cons h rest -> 1 + len rest
end

-- Merge sort
let rec mergeSort xs = match xs
  when Nil -> Nil
  when Cons x Nil -> Cons x Nil
  when Cons x (Cons y rest) ->
    let n = len xs in
    let half = n / 2 in
    let parts = splitAt half xs in
    match parts
      when (left, right) ->
        merge (mergeSort left) (mergeSort right)
    end
end

-- =============================================================================
-- TESTING
-- =============================================================================

let unsorted = [64, 34, 25, 12, 22, 11, 90, 5, 77, 30]
let small = [3, 1, 4, 1, 5, 9, 2, 6]

in {
  original = unsorted,

  -- All sorting algorithms
  insertion = insertionSort unsorted,
  quick = quicksort unsorted,
  mergeSorted = mergeSort unsorted,

  -- Small list for verification
  smallOrig = small,
  smallSorted = quicksort small,

  -- Edge cases
  empty = quicksort Nil,
  single = quicksort [42],
  alreadySorted = quicksort [1, 2, 3, 4, 5],
  reversedList = quicksort [5, 4, 3, 2, 1],
  duplicates = quicksort [3, 1, 3, 1, 3, 1]
}
