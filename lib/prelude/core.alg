-- Core data types
type Maybe a
  = Nothing
  | Just a

type Either a b
  = Left a
  | Right b

type List a
  = Nil
  | Cons a (List a)

-- Core functional programming utilities
module Function
  let id =
    x -> x
  let const =
    x y -> x
  let flip =
    f x y -> f y x
  let compose =
    f g x -> f (g x)
  let pipe =
    f g x -> g (f x)
end

module Bool
  foreign negate : bool -> bool
  foreign both : bool -> bool -> bool
  foreign either : bool -> bool -> bool
  foreign xor : bool -> bool -> bool
end

module Tuple
  let fst =
    pair ->
      match pair
        when (a, b) -> a
      end
  let snd =
    pair ->
      match pair
        when (a, b) -> b
      end
  let swap =
    pair ->
      match pair
        when (a, b) -> (b, a)
      end
  let mapFirst =
    f pair ->
      match pair
        when (a, b) -> (f a, b)
      end
  let mapSecond =
    f pair ->
      match pair
        when (a, b) -> (a, f b)
      end
  let mapBoth =
    f g pair ->
      match pair
        when (a, b) -> (f a, g b)
      end
  let curry =
    f a b -> f (a, b)
  let uncurry =
    f pair ->
      match pair
        when (a, b) -> f a b
      end
end

module Maybe
  let isJust =
    m ->
      match m
        when Nothing -> false
        when Just x -> true
      end
  let isNothing =
    m ->
      match m
        when Nothing -> true
        when Just x -> false
      end
  let map =
    f m ->
      match m
        when Nothing -> Nothing
        when Just x -> Just (f x)
      end
  let flatMap =
    f m ->
      match m
        when Nothing -> Nothing
        when Just x -> f x
      end
  let pure =
    x -> Just x
  let withDefault =
    def m ->
      match m
        when Nothing -> def
        when Just x -> x
      end
  let toList =
    m ->
      match m
        when Nothing -> Nil
        when Just x -> Cons x Nil
      end
  let toEither =
    err m ->
      match m
        when Nothing -> Left err
        when Just x -> Right x
      end
end

module Either
  let isLeft =
    e ->
      match e
        when Left a -> true
        when Right b -> false
      end
  let isRight =
    e ->
      match e
        when Left a -> false
        when Right b -> true
      end
  let map =
    f e ->
      match e
        when Left a -> Left a
        when Right b -> Right (f b)
      end
  let mapLeft =
    f e ->
      match e
        when Left a -> Left (f a)
        when Right b -> Right b
      end
  let flatMap =
    f e ->
      match e
        when Left a -> Left a
        when Right b -> f b
      end
  let pure =
    x -> Right x
  let withDefault =
    def e ->
      match e
        when Left a -> def
        when Right b -> b
      end
  let fromMaybe =
    err m ->
      match m
        when Nothing -> Left err
        when Just x -> Right x
      end
  let toMaybe =
    e ->
      match e
        when Left a -> Nothing
        when Right b -> Just b
      end
end

module List
  let rec map = f xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> Cons (f x) (map f rest)
    end
  let rec filter = p xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> if p x then Cons x (filter p rest) else filter p rest
    end
  let head =
    xs ->
      match xs
        when Nil -> Nothing
        when Cons x rest -> Just x
      end
  let tail =
    xs ->
      match xs
        when Nil -> Nothing
        when Cons x rest -> Just rest
      end
  let isEmpty =
    xs ->
      match xs
        when Nil -> true
        when Cons x rest -> false
      end
  let rec length = xs ->
    match xs
      when Nil -> 0
      when Cons x rest -> 1 + length rest
    end
  let rec foldr = f z xs ->
    match xs
      when Nil -> z
      when Cons x rest -> f x (foldr f z rest)
    end
  let rec foldl = f z xs ->
    match xs
      when Nil -> z
      when Cons x rest -> foldl f (f z x) rest
    end
  let reverse =
    xs -> foldl (acc x -> Cons x acc) Nil xs
  let append =
    xs ys -> foldr Cons ys xs
  let rec concat = xss ->
    match xss
      when Nil -> Nil
      when Cons xs rest -> foldr Cons (concat rest) xs
    end
  let rec take = n xs -> if n <= 0 then Nil else match xs
    when Nil -> Nil
    when Cons x rest -> Cons x (take (n - 1) rest)
  end
  let rec drop = n xs -> if n <= 0 then xs else match xs
    when Nil -> Nil
    when Cons x rest -> drop (n - 1) rest
  end
  let rec zip = xs ys ->
    match (xs, ys)
      when (Cons x xrest, Cons y yrest) -> Cons (x, y) (zip xrest yrest)
      when pair -> Nil
    end
  let rec any = p xs ->
    match xs
      when Nil -> false
      when Cons x rest -> if p x then true else any p rest
    end
  let rec all = p xs ->
    match xs
      when Nil -> true
      when Cons x rest -> if p x then all p rest else false
    end
  let rec find = p xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest -> if p x then Just x else find p rest
    end
  let rec elem = x xs ->
    match xs
      when Nil -> false
      when Cons y rest -> if x == y then true else elem x rest
    end
  let rec filterMap = f xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> match f x
        when Just y -> Cons y (filterMap f rest)
        when Nothing -> filterMap f rest
      end
    end
  let rec splitAt = n xs -> if n <= 0 then (Nil, xs) else match xs
    when Nil -> (Nil, Nil)
    when Cons x rest -> let pair = splitAt (n - 1) rest in
    match pair
      when (ys, zs) -> (Cons x ys, zs)
    end
  end
  let rec replicate = n x -> if n <= 0 then Nil else Cons x (replicate (n - 1) x)
  let rec nub = xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> Cons x (nub (filter (y -> y != x) rest))
    end
  let rec nth = n xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest -> if n == 0 then Just x else nth (n - 1) rest
    end
  let rec range = start end_ -> if start >= end_ then Nil else Cons start (range (start + 1) end_)
  let union =
    xs ys -> nub (append xs ys)
  let rec insertBy = f x xs ->
    match xs
      when Nil -> Cons x Nil
      when Cons y rest -> if f x <= f y then Cons x (Cons y rest) else Cons y (insertBy f x rest)
    end
  let rec sortBy = f xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> insertBy f x (sortBy f rest)
    end
  let rec last = xs ->
    match xs
      when Nil -> Nothing
      when Cons x Nil -> Just x
      when Cons x rest -> last rest
    end
  let rec init = xs ->
    match xs
      when Nil -> Nothing
      when Cons x Nil -> Just Nil
      when Cons x rest -> match init rest
        when Nothing -> Nothing
        when Just ys -> Just (Cons x ys)
      end
    end
  let rec zipWith = f xs ys ->
    match (xs, ys)
      when (Cons x xrest, Cons y yrest) -> Cons (f x y) (zipWith f xrest yrest)
      when pair -> Nil
    end
  let flatMap =
    f xs -> concat (map f xs)
  let concatMap =
    f xs -> concat (map f xs)
  let rec unzip = pairs ->
    match pairs
      when Nil -> (Nil, Nil)
      when Cons (a, b) rest -> match unzip rest
        when (as_, bs) -> (Cons a as_, Cons b bs)
      end
    end
  let rec partition = p xs ->
    match xs
      when Nil -> (Nil, Nil)
      when Cons x rest -> match partition p rest
        when (yes, no) -> if p x then (Cons x yes, no) else (yes, Cons x no)
      end
    end
  let rec indexed = xs -> let rec go = i ys ->
    match ys
      when Nil -> Nil
      when Cons y rest -> Cons (i, y) (go (i + 1) rest)
    end
  in
  go 0 xs
  let sum =
    xs -> foldl (acc x -> acc + x) 0 xs
  let product =
    xs -> foldl (acc x -> acc * x) 1 xs
  let rec minimum = xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest -> match minimum rest
        when Nothing -> Just x
        when Just y -> Just (if x < y then x else y)
      end
    end
  let rec maximum = xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest -> match maximum rest
        when Nothing -> Just x
        when Just y -> Just (if x > y then x else y)
      end
    end
  let rec intersperse = sep xs ->
    match xs
      when Nil -> Nil
      when Cons x Nil -> Cons x Nil
      when Cons x rest -> Cons x (Cons sep (intersperse sep rest))
    end
  let intercalate =
    sep xss -> concat (intersperse sep xss)
  let rec forEach = f xs ->
    match xs
      when Nil -> ()
      when Cons x rest ->
        let unused = f x in
        forEach f rest
    end
end
