-- Data Structures
-- Demonstrates algebraic data types and operations on them

-- =============================================================================
-- BINARY TREE
-- =============================================================================

type Tree a = Leaf | Node a (Tree a) (Tree a)

-- Insert into binary search tree
let rec treeInsert x tree = match tree
  when Leaf -> Node x Leaf Leaf
  when Node v left right ->
    if x < v
    then Node v (treeInsert x left) right
    else if x > v
    then Node v left (treeInsert x right)
    else tree
end

-- Search in binary search tree
let rec treeContains x tree = match tree
  when Leaf -> false
  when Node v left right ->
    if x == v then true
    else if x < v then treeContains x left
    else treeContains x right
end

-- In-order traversal (returns sorted list for BST)
let rec treeInOrder tree = match tree
  when Leaf -> Nil
  when Node v left right ->
    let leftList = treeInOrder left in
    let rightList = treeInOrder right in
    let rec append xs ys = match xs
      when Nil -> ys
      when Cons x rest -> Cons x (append rest ys)
    end
    in append leftList (Cons v rightList)
end

-- Tree height
let rec treeHeight tree = match tree
  when Leaf -> 0
  when Node v left right ->
    let leftH = treeHeight left in
    let rightH = treeHeight right in
    1 + (if leftH > rightH then leftH else rightH)
end

-- Count nodes
let rec treeSize tree = match tree
  when Leaf -> 0
  when Node v left right ->
    1 + treeSize left + treeSize right
end

-- Build tree from list
let rec treeFromList xs = match xs
  when Nil -> Leaf
  when Cons x rest -> treeInsert x (treeFromList rest)
end

-- =============================================================================
-- NATURAL NUMBERS (Peano)
-- =============================================================================

type Nat = Zero | Succ Nat

-- Convert number to Nat
let rec toNat n =
  if n <= 0 then Zero
  else Succ (toNat (n - 1))

-- Convert Nat to number
let rec fromNat n = match n
  when Zero -> 0
  when Succ m -> 1 + fromNat m
end

-- Add two Nats
let rec natAdd a b = match a
  when Zero -> b
  when Succ n -> Succ (natAdd n b)
end

-- Multiply two Nats
let rec natMul a b = match a
  when Zero -> Zero
  when Succ n -> natAdd b (natMul n b)
end

-- =============================================================================
-- EXPRESSION TREE (Simple Calculator)
-- =============================================================================

type Expr = Num number | Add Expr Expr | Mul Expr Expr | Sub Expr Expr

-- Evaluate expression
let rec evalExpr expr = match expr
  when Num n -> n
  when Add a b -> evalExpr a + evalExpr b
  when Mul a b -> evalExpr a * evalExpr b
  when Sub a b -> evalExpr a - evalExpr b
end

-- Count operations in expression
let rec countOps expr = match expr
  when Num n -> 0
  when Add a b -> 1 + countOps a + countOps b
  when Mul a b -> 1 + countOps a + countOps b
  when Sub a b -> 1 + countOps a + countOps b
end

-- Example: (2 + 3) * (4 - 1) = 15
let exampleExpr = Mul (Add (Num 2) (Num 3)) (Sub (Num 4) (Num 1))

-- =============================================================================
-- TESTING
-- =============================================================================

let nums = [5, 3, 7, 1, 9, 2, 8, 4, 6]
let bst = treeFromList nums

in {
  -- Binary Tree
  inOrder = treeInOrder bst,
  height = treeHeight bst,
  size = treeSize bst,
  contains5 = treeContains 5 bst,
  contains10 = treeContains 10 bst,

  -- Peano Numbers
  nat3 = fromNat (toNat 3),
  nat5 = fromNat (toNat 5),
  natSum = fromNat (natAdd (toNat 3) (toNat 4)),
  natProd = fromNat (natMul (toNat 3) (toNat 4)),

  -- Expression Tree
  exprResult = evalExpr exampleExpr,
  exprOps = countOps exampleExpr
}
