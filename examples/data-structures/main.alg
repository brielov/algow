-- =============================================================================
-- Data Structures: Binary Search Tree, Stack, Queue
-- =============================================================================

-- =============================================================================
-- Binary Search Tree
-- =============================================================================

type BST a = Empty | BSTNode (BST a) a (BST a)

let rec bstInsert = x t ->
  match t
    when Empty -> BSTNode Empty x Empty
    when BSTNode left val right ->
      if x < val
        then BSTNode (bstInsert x left) val right
        else if x > val
          then BSTNode left val (bstInsert x right)
          else t
  end

let rec bstMember = x t ->
  match t
    when Empty -> false
    when BSTNode left val right ->
      if x == val then true
      else if x < val then bstMember x left
      else bstMember x right
  end

let rec bstSize = t ->
  match t
    when Empty -> 0
    when BSTNode left val right -> 1 + bstSize left + bstSize right
  end

let rec bstInorder = t ->
  match t
    when Empty -> Nil
    when BSTNode left val right ->
      List.append (bstInorder left) (Cons val (bstInorder right))
  end

let bstFromList = xs -> List.foldl (acc x -> bstInsert x acc) Empty xs

-- =============================================================================
-- Stack (LIFO)
-- =============================================================================

type Stack a = Stack (List a)

let stackEmpty = Stack Nil

let stackPush = x s ->
  match s when Stack xs -> Stack (Cons x xs) end

let stackPop = s ->
  match s
    when Stack Nil -> Nothing
    when Stack (Cons x rest) -> Just (x, Stack rest)
  end

let stackSize = s ->
  match s when Stack xs -> List.length xs end

-- =============================================================================
-- Queue (FIFO)
-- =============================================================================

type Queue a = Queue (List a) (List a)

let queueEmpty = Queue Nil Nil

let queueEnqueue = x q ->
  match q when Queue front back -> Queue front (Cons x back) end

let rec queueDequeue = q ->
  match q
    when Queue Nil Nil -> Nothing
    when Queue Nil back -> queueDequeue (Queue (List.reverse back) Nil)
    when Queue (Cons x rest) back -> Just (x, Queue rest back)
  end

let queueSize = q ->
  match q when Queue front back -> List.length front + List.length back end

-- =============================================================================
-- Association List
-- =============================================================================

type Assoc k v = Assoc (List (k, v))

let assocEmpty = Assoc Nil

let assocInsert = key val a ->
  match a when Assoc xs -> Assoc (Cons (key, val) xs) end

let rec assocLookup = key a ->
  match a
    when Assoc Nil -> Nothing
    when Assoc (Cons (k, v) rest) ->
      if k == key then Just v else assocLookup key (Assoc rest)
  end

let assocSize = a ->
  match a when Assoc xs -> List.length xs end

-- =============================================================================
-- Testing
-- =============================================================================

let testBST =
  let t = bstFromList [5, 3, 7, 1, 4, 6, 8] in
  let a = bstSize t in                                    -- 7
  let b = if bstMember 4 t then 1 else 0 in               -- 1
  let c = if bstMember 9 t then 0 else 1 in               -- 1
  let d = List.length (bstInorder t) in                   -- 7
  a + b + c + d                                           -- 16

let testStack =
  let s0 = stackEmpty in
  let s1 = stackPush 1 s0 in
  let s2 = stackPush 2 s1 in
  let s3 = stackPush 3 s2 in
  let a = stackSize s3 in                                 -- 3
  let b = match stackPop s3
    when Just (x, rest) -> x
    when Nothing -> 0
  end in                                                  -- 3
  a + b                                                   -- 6

let testQueue =
  let q0 = queueEmpty in
  let q1 = queueEnqueue 1 q0 in
  let q2 = queueEnqueue 2 q1 in
  let q3 = queueEnqueue 3 q2 in
  let a = queueSize q3 in                                 -- 3
  let b = match queueDequeue q3
    when Just (x, rest) -> x
    when Nothing -> 0
  end in                                                  -- 1
  a + b                                                   -- 4

let testAssoc =
  let a0 = assocEmpty in
  let a1 = assocInsert 1 "one" a0 in
  let a2 = assocInsert 2 "two" a1 in
  let a3 = assocInsert 3 "three" a2 in
  let a = assocSize a3 in                                 -- 3
  let b = match assocLookup 2 a3
    when Just v -> 1
    when Nothing -> 0
  end in                                                  -- 1
  a + b                                                   -- 4

let result = testBST + testStack + testQueue + testAssoc  -- 30

let main = args -> IO.printLine (Int.toString result)
