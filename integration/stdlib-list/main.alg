-- Stdlib: List module
let tests =
  let xs = [1, 2, 3, 4, 5] in
  let ys = [6, 7, 8, 9, 10] in
  let empty = [] in
  [ Test.equal "map double" [2, 4, 6, 8, 10] (List.map (x -> x * 2) xs)
    , Test.equal "map identity" xs (List.map (x -> x) xs)
    , Test.equal "map empty" [] (List.map (x -> x) empty)
    , Test.equal "filter even" [2, 4] (List.filter (x -> Int.mod x 2 == 0) xs)
    , Test.equal "filter all" xs (List.filter (x -> true) xs)
    , Test.equal "filter none" [] (List.filter (x -> false) xs)
    , Test.equal "filter empty" [] (List.filter (x -> true) empty)
    , Test.equal "head" (Just 1) (List.head xs)
    , Test.equal "head empty" Nothing (List.head empty)
    , Test.equal "tail" (Just [2, 3, 4, 5]) (List.tail xs)
    , Test.equal "tail empty" Nothing (List.tail empty)
    , Test.equal "tail singleton" (Just []) (List.tail [42])
    , Test.ok "isEmpty empty" (List.isEmpty empty)
    , Test.ok "isEmpty not" (Bool.negate (List.isEmpty xs))
    , Test.equal "length" 5 (List.length xs)
    , Test.equal "length empty" 0 (List.length empty)
    , Test.equal "length singleton" 1 (List.length [42])
    , Test.equal "foldr sum" 15 (List.foldr (x acc -> x + acc) 0 xs)
    , Test.equal "foldl sum" 15 (List.foldl (acc x -> acc + x) 0 xs)
    , Test.equal "foldr concat" "12345" (List.foldr (x acc -> String.concat (Int.toString x) acc) "" xs)
    , Test.equal "foldl concat" "12345" (List.foldl (acc x -> String.concat acc (Int.toString x)) "" xs)
    , Test.equal "foldr empty" 0 (List.foldr (x acc -> x + acc) 0 empty)
    , Test.equal "reverse" [5, 4, 3, 2, 1] (List.reverse xs)
    , Test.equal "reverse empty" [] (List.reverse empty)
    , Test.equal "reverse singleton" [42] (List.reverse [42])
    , Test.equal "append" [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] (List.append xs ys)
    , Test.equal "append empty left" xs (List.append empty xs)
    , Test.equal "append empty right" xs (List.append xs empty)
    , Test.equal "concat" [1, 2, 3, 4] (List.concat [[1, 2], [3, 4]])
    , Test.equal "concat empty" [] (List.concat [])
    , Test.equal "concat with empties" [1, 2, 3] (List.concat [[], [1], [], [2, 3], []])
    , Test.equal "take 3" [1, 2, 3] (List.take 3 xs)
    , Test.equal "take 0" [] (List.take 0 xs)
    , Test.equal "take more" xs (List.take 100 xs)
    , Test.equal "take negative" [] (List.take (0 - 1) xs)
    , Test.equal "drop 2" [3, 4, 5] (List.drop 2 xs)
    , Test.equal "drop 0" xs (List.drop 0 xs)
    , Test.equal "drop more" [] (List.drop 100 xs)
    , Test.equal "drop negative" xs (List.drop (0 - 1) xs)
    , Test.equal "zip equal" [(1, 6), (2, 7), (3, 8), (4, 9), (5, 10)] (List.zip xs ys)
    , Test.equal "zip unequal" [(1, 6), (2, 7)] (List.zip [1, 2] [6, 7, 8])
    , Test.equal "zip empty" [] (List.zip empty xs)
    , Test.ok "any true" (List.any (x -> x > 3) xs)
    , Test.ok "any false" (Bool.negate (List.any (x -> x > 10) xs))
    , Test.ok "any empty" (Bool.negate (List.any (x -> true) empty))
    , Test.ok "all true" (List.all (x -> x > 0) xs)
    , Test.ok "all false" (Bool.negate (List.all (x -> x > 3) xs))
    , Test.ok "all empty" (List.all (x -> false) empty)
    , Test.equal "find exists" (Just 4) (List.find (x -> x > 3) xs)
    , Test.equal "find not exists" Nothing (List.find (x -> x > 10) xs)
    , Test.equal "find empty" Nothing (List.find (x -> true) empty)
    , Test.ok "elem exists" (List.elem 3 xs)
    , Test.ok "elem not exists" (Bool.negate (List.elem 10 xs))
    , Test.ok "elem empty" (Bool.negate (List.elem 1 empty))
    , Test.equal "filterMap" [4, 8] (List.filterMap (x -> if Int.mod x 2 == 0
      then Just (x * 2)
      else Nothing) xs)
    , Test.equal "filterMap all Nothing" [] (List.filterMap (x -> Nothing) xs)
    , Test.equal "filterMap all Just" [2, 4, 6, 8, 10] (List.filterMap (x -> Just (x * 2)) xs)
    , Test.equal "splitAt" ([1, 2], [3, 4, 5]) (List.splitAt 2 xs)
    , Test.equal "splitAt 0" ([], xs) (List.splitAt 0 xs)
    , Test.equal "splitAt all" (xs, []) (List.splitAt 10 xs)
    , Test.equal "replicate" [42, 42, 42] (List.replicate 3 42)
    , Test.equal "replicate 0" [] (List.replicate 0 42)
    , Test.equal "replicate 1" [42] (List.replicate 1 42)
    , Test.equal "nub" [1, 2, 3] (List.nub [1, 2, 2, 3, 1, 3])
    , Test.equal "nub no dups" xs (List.nub xs)
    , Test.equal "nub empty" [] (List.nub empty)
    , Test.equal "nth 0" (Just 1) (List.nth 0 xs)
    , Test.equal "nth 2" (Just 3) (List.nth 2 xs)
    , Test.equal "nth out of bounds" Nothing (List.nth 10 xs)
    , Test.equal "nth negative" Nothing (List.nth (0 - 1) xs)
    , Test.equal "range" [0, 1, 2, 3, 4] (List.range 0 5)
    , Test.equal "range empty" [] (List.range 5 5)
    , Test.equal "range negative" [] (List.range 5 0)
    , Test.equal "union" [1, 2, 3, 4, 5] (List.union [1, 2, 3] [3, 4, 5])
    , Test.equal "sortBy identity" [1, 2, 3, 4, 5] (List.sortBy (x -> x) [3, 1, 4, 1, 5, 2])
    , Test.equal "sortBy negate" [5, 4, 3, 2, 1] (List.sortBy (x -> 0 - x) [3, 1, 4, 1, 5, 2])
    , Test.equal "last" (Just 5) (List.last xs)
    , Test.equal "last empty" Nothing (List.last empty)
    , Test.equal "last singleton" (Just 42) (List.last [42])
    , Test.equal "init" (Just [1, 2, 3, 4]) (List.init xs)
    , Test.equal "init empty" Nothing (List.init empty)
    , Test.equal "init singleton" (Just []) (List.init [42])
    , Test.equal "zipWith add" [7, 9, 11] (List.zipWith (a b -> a + b) [1, 2, 3] [6, 7, 8])
    , Test.equal "zipWith empty" [] (List.zipWith (a b -> a + b) empty xs)
    , Test.equal "flatMap" [1, 1, 2, 2, 3, 3] (List.flatMap (x -> [x, x]) [1, 2, 3])
    , Test.equal "flatMap empty" [] (List.flatMap (x -> [x, x]) empty)
    , Test.equal "unzip" ([1, 2, 3], [4, 5, 6]) (List.unzip [(1, 4), (2, 5), (3, 6)])
    , Test.equal "unzip empty" ([], []) (List.unzip [])
    , Test.equal "partition" ([2, 4], [1, 3, 5]) (List.partition (x -> Int.mod x 2 == 0) xs)
    , Test.equal "partition all yes" (xs, []) (List.partition (x -> true) xs)
    , Test.equal "partition all no" ([], xs) (List.partition (x -> false) xs)
    , Test.equal "indexed" [(0, 1), (1, 2), (2, 3)] (List.indexed [1, 2, 3])
    , Test.equal "indexed empty" [] (List.indexed empty)
    , Test.equal "sum" 15 (List.sum xs)
    , Test.equal "sum empty" 0 (List.sum empty)
    , Test.equal "product" 120 (List.product xs)
    , Test.equal "product empty" 1 (List.product empty)
    , Test.equal "minimum" (Just 1) (List.minimum xs)
    , Test.equal "minimum empty" Nothing (List.minimum empty)
    , Test.equal "maximum" (Just 5) (List.maximum xs)
    , Test.equal "maximum empty" Nothing (List.maximum empty)
    , Test.equal "intersperse" [1, 0, 2, 0, 3] (List.intersperse 0 [1, 2, 3])
    , Test.equal "intersperse single" [1] (List.intersperse 0 [1])
    , Test.equal "intersperse empty" [] (List.intersperse 0 empty)
    , Test.equal "intercalate" [1, 2, 0, 3, 4, 0, 5, 6] (List.intercalate [0] [[1, 2], [3, 4], [5, 6]])
    ]

-- map
-- filter
-- head / tail
-- isEmpty
-- length
-- foldr / foldl
-- reverse
-- append
-- concat
-- take / drop
-- zip
-- any / all
-- find
-- elem
-- filterMap
-- splitAt
-- replicate
-- nub
-- nth
-- range
-- union
-- sortBy
-- last / init
-- zipWith
-- flatMap / concatMap
-- unzip
-- partition
-- indexed
-- sum / product
-- minimum / maximum
-- intersperse / intercalate
let main =
  args -> Test.run tests
