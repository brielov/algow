-- =============================================================================
-- PRELUDE - STANDARD LIBRARY
-- =============================================================================
--
-- Core data types and functions included in every program.
--
-- Modules:
-- - Maybe: Optional values
-- - Either: Sum type for errors/results
-- - List: Linked list with common operations
-- - Core: Utility functions (id, const, compose, flip)
-- - Bool: Boolean operations
-- - String, Char, Int, Float: Primitive operations (foreign)
-- - IO: Input/output operations (foreign)
-- - Debug: Debugging utilities (foreign)
-- - Map, Set: Collection types (foreign)
--
-- =============================================================================

-- =============================================================================
-- MAYBE MODULE
-- =============================================================================

module Maybe

  type Maybe a = Nothing | Just a

  let isJust = m -> match m
    when Nothing -> false
    when Just _ -> true
  end

  let isNothing = m -> match m
    when Nothing -> true
    when Just _ -> false
  end

  let rec map = f m -> match m
    when Nothing -> Nothing
    when Just x -> Just (f x)
  end

  let rec flatMap = f m -> match m
    when Nothing -> Nothing
    when Just x -> f x
  end

  let pure = x -> Just x

  let withDefault = def m -> match m
    when Nothing -> def
    when Just x -> x
  end

  let maybe = def f m -> match m
    when Nothing -> def
    when Just x -> f x
  end

  let toList = m -> match m
    when Nothing -> Nil
    when Just x -> Cons x Nil
  end

end

-- =============================================================================
-- EITHER MODULE
-- =============================================================================

module Either

  type Either a b = Left a | Right b

  let isLeft = e -> match e
    when Left _ -> true
    when Right _ -> false
  end

  let isRight = e -> match e
    when Left _ -> false
    when Right _ -> true
  end

  let rec map = f e -> match e
    when Left a -> Left a
    when Right b -> Right (f b)
  end

  let rec mapLeft = f e -> match e
    when Left a -> Left (f a)
    when Right b -> Right b
  end

  let rec flatMap = f e -> match e
    when Left a -> Left a
    when Right b -> f b
  end

  let pure = x -> Right x

  let withDefault = def e -> match e
    when Left _ -> def
    when Right b -> b
  end

  let fromMaybe = err m -> match m
    when Nothing -> Left err
    when Just x -> Right x
  end

end

-- =============================================================================
-- LIST MODULE
-- =============================================================================

module List

  type List a = Nil | Cons a (List a)

  let rec map = f xs -> match xs
    when Nil -> Nil
    when Cons x rest -> Cons (f x) (map f rest)
  end

  let rec filter = p xs -> match xs
    when Nil -> Nil
    when Cons x rest ->
      if p x then Cons x (filter p rest)
      else filter p rest
  end

  let head = xs -> match xs
    when Nil -> Nothing
    when Cons x _ -> Just x
  end

  let tail = xs -> match xs
    when Nil -> Nothing
    when Cons _ rest -> Just rest
  end

  let isEmpty = xs -> match xs
    when Nil -> true
    when Cons _ _ -> false
  end

  let rec length = xs -> match xs
    when Nil -> 0
    when Cons _ rest -> 1 + length rest
  end

  let rec foldr = f z xs -> match xs
    when Nil -> z
    when Cons x rest -> f x (foldr f z rest)
  end

  let rec foldl = f z xs -> match xs
    when Nil -> z
    when Cons x rest -> foldl f (f z x) rest
  end

  let reverse = xs -> foldl (acc x -> Cons x acc) Nil xs

  let append = xs ys -> foldr Cons ys xs

  let rec concat = xss -> match xss
    when Nil -> Nil
    when Cons xs rest -> foldr Cons (concat rest) xs
  end

  let rec take = n xs ->
    if n <= 0 then Nil
    else match xs
      when Nil -> Nil
      when Cons x rest -> Cons x (take (n - 1) rest)
    end

  let rec drop = n xs ->
    if n <= 0 then xs
    else match xs
      when Nil -> Nil
      when Cons _ rest -> drop (n - 1) rest
    end

  let rec zip = xs ys -> match (xs, ys)
    when (Cons x xrest, Cons y yrest) -> Cons (x, y) (zip xrest yrest)
    when _ -> Nil
  end

  let rec any = p xs -> match xs
    when Nil -> false
    when Cons x rest -> if p x then true else any p rest
  end

  let rec all = p xs -> match xs
    when Nil -> true
    when Cons x rest -> if p x then all p rest else false
  end

  let rec find = p xs -> match xs
    when Nil -> Nothing
    when Cons x rest -> if p x then Just x else find p rest
  end

  let rec nth = n xs -> match xs
    when Nil -> Nothing
    when Cons x rest -> if n == 0 then Just x else nth (n - 1) rest
  end

  let rec splitAt = n xs ->
    if n <= 0 then (Nil, xs)
    else match xs
      when Nil -> (Nil, Nil)
      when Cons x rest ->
        let (before, after) = splitAt (n - 1) rest in
        (Cons x before, after)
    end

  let rec filterMap = f xs -> match xs
    when Nil -> Nil
    when Cons x rest -> match f x
      when Nothing -> filterMap f rest
      when Just y -> Cons y (filterMap f rest)
    end
  end

  let rec replicate = n x ->
    if n <= 0 then Nil
    else Cons x (replicate (n - 1) x)

  let rec range = start stop ->
    if start >= stop then Nil
    else Cons start (range (start + 1) stop)

  -- Remove duplicates (keeps first occurrence)
  let rec nub = xs -> nubHelper xs Nil

  and nubHelper = xs seen -> match xs
    when Nil -> Nil
    when Cons x rest ->
      if any (y -> y == x) seen then nubHelper rest seen
      else Cons x (nubHelper rest (Cons x seen))
  end

end

-- =============================================================================
-- CORE MODULE
-- =============================================================================

module Core

  let id = x -> x

  let always = x y -> x

  let compose = f g x -> f (g x)

  let flip = f a b -> f b a

  let fst = pair -> match pair when (a, _) -> a end

  let snd = pair -> match pair when (_, b) -> b end

end

-- =============================================================================
-- BOOL MODULE
-- =============================================================================

module Bool

  let not = b -> if b then false else true

  let eq = a b -> if a then b else not b

  let both = a b -> if a then b else false

  let either = a b -> if a then true else b

end

-- =============================================================================
-- UNIT MODULE
-- =============================================================================

module Unit

  type Unit = Unit

end

-- =============================================================================
-- STRING MODULE (foreign functions)
-- =============================================================================

module String

  foreign length : string -> int
  foreign concat : string -> string -> string
  foreign substring : int -> int -> string -> string
  foreign charAt : int -> string -> Maybe char
  foreign head : string -> Maybe char
  foreign tail : string -> string
  foreign drop : int -> string -> string
  foreign take : int -> string -> string
  foreign isEmpty : string -> boolean
  foreign toList : string -> List char
  foreign fromList : List char -> string
  foreign eq : string -> string -> boolean
  foreign lt : string -> string -> boolean
  foreign split : string -> string -> List string
  foreign join : string -> List string -> string
  foreign trim : string -> string
  foreign toUpper : string -> string
  foreign toLower : string -> string
  foreign contains : string -> string -> boolean
  foreign startsWith : string -> string -> boolean
  foreign endsWith : string -> string -> boolean
  foreign replace : string -> string -> string -> string
  foreign replicate : int -> string -> string

end

-- =============================================================================
-- CHAR MODULE (foreign functions)
-- =============================================================================

module Char

  foreign toInt : char -> int
  foreign fromInt : int -> Maybe char
  foreign toString : char -> string
  foreign eq : char -> char -> boolean
  foreign lt : char -> char -> boolean
  foreign isDigit : char -> boolean
  foreign isAlpha : char -> boolean
  foreign isAlphaNum : char -> boolean
  foreign isSpace : char -> boolean
  foreign isUpper : char -> boolean
  foreign isLower : char -> boolean
  foreign toUpper : char -> char
  foreign toLower : char -> char
  foreign isIdentStart : char -> boolean
  foreign isIdentChar : char -> boolean

end

-- =============================================================================
-- INT MODULE (foreign functions)
-- =============================================================================

module Int

  foreign add : int -> int -> int
  foreign sub : int -> int -> int
  foreign mul : int -> int -> int
  foreign div : int -> int -> int
  foreign mod : int -> int -> int
  foreign neg : int -> int
  foreign abs : int -> int
  foreign eq : int -> int -> boolean
  foreign lt : int -> int -> boolean
  foreign le : int -> int -> boolean
  foreign gt : int -> int -> boolean
  foreign ge : int -> int -> boolean
  foreign toFloat : int -> float
  foreign toString : int -> string
  foreign fromString : string -> Maybe int

end

-- =============================================================================
-- FLOAT MODULE (foreign functions)
-- =============================================================================

module Float

  foreign add : float -> float -> float
  foreign sub : float -> float -> float
  foreign mul : float -> float -> float
  foreign div : float -> float -> float
  foreign neg : float -> float
  foreign abs : float -> float
  foreign eq : float -> float -> boolean
  foreign lt : float -> float -> boolean
  foreign le : float -> float -> boolean
  foreign gt : float -> float -> boolean
  foreign ge : float -> float -> boolean
  foreign floor : float -> int
  foreign ceil : float -> int
  foreign round : float -> int
  foreign sqrt : float -> float
  foreign pow : float -> float -> float
  foreign sin : float -> float
  foreign cos : float -> float
  foreign tan : float -> float
  foreign log : float -> float
  foreign exp : float -> float
  foreign toString : float -> string
  foreign fromString : string -> Maybe float

end

-- =============================================================================
-- IO MODULE (foreign functions)
-- =============================================================================

module IO

  type IOError
    = FileNotFound string
    | PermissionDenied string
    | IsDirectory string
    | AlreadyExists string
    | UnknownError string

  foreign print : string -> Unit
  foreign printLine : string -> Unit
  foreign readLine : Unit -> Either IOError string
  foreign readFile : string -> Either IOError string
  foreign writeFile : string -> string -> Either IOError Unit
  foreign appendFile : string -> string -> Either IOError Unit
  foreign fileExists : string -> boolean
  foreign deleteFile : string -> Either IOError Unit
  foreign isDirectory : string -> boolean
  foreign readDir : string -> Either IOError (List string)
  foreign args : Unit -> List string
  foreign exit : int -> Unit
  foreign getEnv : string -> Maybe string

end

-- =============================================================================
-- DEBUG MODULE (foreign functions)
-- =============================================================================

module Debug

  foreign log : a -> a
  foreign trace : string -> a -> a
  foreign panic : string -> a

end

-- =============================================================================
-- MAP MODULE (foreign functions - String keys only)
-- =============================================================================

module Map

  foreign empty : Map v
  foreign singleton : string -> v -> Map v
  foreign insert : string -> v -> Map v -> Map v
  foreign lookup : string -> Map v -> Maybe v
  foreign delete : string -> Map v -> Map v
  foreign member : string -> Map v -> boolean
  foreign size : Map v -> int
  foreign keys : Map v -> List string
  foreign values : Map v -> List v
  foreign toList : Map v -> List (string, v)
  foreign fromList : List (string, v) -> Map v

end

-- =============================================================================
-- SET MODULE (foreign functions - String values only)
-- =============================================================================

module Set

  foreign empty : Set
  foreign singleton : string -> Set
  foreign insert : string -> Set -> Set
  foreign delete : string -> Set -> Set
  foreign member : string -> Set -> boolean
  foreign size : Set -> int
  foreign toList : Set -> List string
  foreign fromList : List string -> Set
  foreign union : Set -> Set -> Set
  foreign intersect : Set -> Set -> Set
  foreign difference : Set -> Set -> Set

end
