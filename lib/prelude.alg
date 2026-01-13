-- =============================================================================
-- Algow Prelude
-- Standard library types and functions included in every program.
-- =============================================================================

-- =============================================================================
-- Data Types
-- =============================================================================

type Maybe a = Nothing | Just a

type Either a b = Left a | Right b

type List a = Nil | Cons a (List a)

type IOError
  = FileNotFound string
  | PermissionDenied string
  | IsDirectory string
  | NotDirectory string
  | AlreadyExists string
  | NotEmpty string
  | UnknownError string

type JsonValue
  = JsonNull
  | JsonBool bool
  | JsonNumber float
  | JsonString string
  | JsonArray (List JsonValue)
  | JsonObject (List (string, JsonValue))

-- =============================================================================
-- Maybe Functions
-- =============================================================================

module Maybe
  let isJust = m ->
    match m
      when Nothing -> false
      when Just x -> true
    end

  let isNothing = m ->
    match m
      when Nothing -> true
      when Just x -> false
    end

  let map = f m ->
    match m
      when Nothing -> Nothing
      when Just x -> Just (f x)
    end

  let flatMap = f m ->
    match m
      when Nothing -> Nothing
      when Just x -> f x
    end

  let pure = x -> Just x

  let withDefault = def m ->
    match m
      when Nothing -> def
      when Just x -> x
    end

  let toList = m ->
    match m
      when Nothing -> Nil
      when Just x -> Cons x Nil
    end
end

-- =============================================================================
-- Either Functions
-- =============================================================================

module Either
  let isLeft = e ->
    match e
      when Left a -> true
      when Right b -> false
    end

  let isRight = e ->
    match e
      when Left a -> false
      when Right b -> true
    end

  let map = f e ->
    match e
      when Left a -> Left a
      when Right b -> Right (f b)
    end

  let mapLeft = f e ->
    match e
      when Left a -> Left (f a)
      when Right b -> Right b
    end

  let flatMap = f e ->
    match e
      when Left a -> Left a
      when Right b -> f b
    end

  let pure = x -> Right x

  let withDefault = def e ->
    match e
      when Left a -> def
      when Right b -> b
    end

  let fromMaybe = err m ->
    match m
      when Nothing -> Left err
      when Just x -> Right x
    end
end

-- =============================================================================
-- List Functions
-- =============================================================================

module List
  let rec map = f xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> Cons (f x) (map f rest)
    end

  let rec filter = p xs ->
    match xs
      when Nil -> Nil
      when Cons x rest ->
        if p x
          then Cons x (filter p rest)
          else filter p rest
    end

  let head = xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest -> Just x
    end

  let tail = xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest -> Just rest
    end

  let isEmpty = xs ->
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

  let reverse = xs -> foldl (acc x -> Cons x acc) Nil xs

  let append = xs ys -> foldr Cons ys xs

  let rec concat = xss ->
    match xss
      when Nil -> Nil
      when Cons xs rest -> foldr Cons (concat rest) xs
    end

  let rec take = n xs ->
    if n <= 0
      then Nil
      else match xs
        when Nil -> Nil
        when Cons x rest -> Cons x (take (n - 1) rest)
      end

  let rec drop = n xs ->
    if n <= 0
      then xs
      else match xs
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
      when Cons x rest ->
        match f x
          when Just y -> Cons y (filterMap f rest)
          when Nothing -> filterMap f rest
        end
    end

  let rec splitAt = n xs ->
    if n <= 0
      then (Nil, xs)
      else match xs
        when Nil -> (Nil, Nil)
        when Cons x rest ->
          let pair = splitAt (n - 1) rest in
          match pair
            when (ys, zs) -> (Cons x ys, zs)
          end
      end

  let rec replicate = n x ->
    if n <= 0
      then Nil
      else Cons x (replicate (n - 1) x)

  let rec nub = xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> Cons x (nub (filter (y -> y != x) rest))
    end

  let rec nth = n xs ->
    match xs
      when Nil -> Nothing
      when Cons x rest ->
        if n == 0
          then Just x
          else nth (n - 1) rest
    end

  let rec range = start end_ ->
    if start >= end_
      then Nil
      else Cons start (range (start + 1) end_)

  let union = xs ys -> nub (append xs ys)

  let rec insertBy = f x xs ->
    match xs
      when Nil -> Cons x Nil
      when Cons y rest ->
        if f x <= f y
          then Cons x (Cons y rest)
          else Cons y (insertBy f x rest)
    end

  let rec sortBy = f xs ->
    match xs
      when Nil -> Nil
      when Cons x rest -> insertBy f x (sortBy f rest)
    end
end

-- =============================================================================
-- String Functions (foreign)
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
  foreign isEmpty : string -> bool
  foreign toList : string -> List char
  foreign fromList : List char -> string
  foreign eq : string -> string -> bool
  foreign lt : string -> string -> bool
  foreign split : string -> string -> List string
  foreign join : string -> List string -> string
  foreign trim : string -> string
  foreign toUpper : string -> string
  foreign toLower : string -> string
  foreign contains : string -> string -> bool
  foreign startsWith : string -> string -> bool
  foreign endsWith : string -> string -> bool
  foreign replace : string -> string -> string -> string
end

-- =============================================================================
-- Char Functions (foreign)
-- =============================================================================

module Char
  foreign toInt : char -> int
  foreign fromInt : int -> Maybe char
  foreign toString : char -> string
  foreign eq : char -> char -> bool
  foreign lt : char -> char -> bool
  foreign isDigit : char -> bool
  foreign isAlpha : char -> bool
  foreign isAlphaNum : char -> bool
  foreign isSpace : char -> bool
  foreign isUpper : char -> bool
  foreign isLower : char -> bool
  foreign toUpper : char -> char
  foreign toLower : char -> char
  foreign isIdentStart : char -> bool
  foreign isIdentChar : char -> bool
end

-- =============================================================================
-- Int Functions (foreign)
-- =============================================================================

module Int
  foreign add : int -> int -> int
  foreign sub : int -> int -> int
  foreign mul : int -> int -> int
  foreign div : int -> int -> int
  foreign mod : int -> int -> int
  foreign neg : int -> int
  foreign abs : int -> int
  foreign eq : int -> int -> bool
  foreign lt : int -> int -> bool
  foreign le : int -> int -> bool
  foreign gt : int -> int -> bool
  foreign ge : int -> int -> bool
  foreign toFloat : int -> float
  foreign toString : int -> string
  foreign fromString : string -> Maybe int
end

-- =============================================================================
-- Float Functions (foreign)
-- =============================================================================

module Float
  foreign add : float -> float -> float
  foreign sub : float -> float -> float
  foreign mul : float -> float -> float
  foreign div : float -> float -> float
  foreign neg : float -> float
  foreign abs : float -> float
  foreign eq : float -> float -> bool
  foreign lt : float -> float -> bool
  foreign le : float -> float -> bool
  foreign gt : float -> float -> bool
  foreign ge : float -> float -> bool
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
-- IO Functions (foreign)
-- =============================================================================

module IO
  foreign print : string -> unit
  foreign printLine : string -> unit
  foreign exit : int -> unit
  foreign getEnv : string -> Maybe string
end

-- =============================================================================
-- Debug Functions (foreign)
-- =============================================================================

module Debug
  foreign log : a -> a
  foreign trace : string -> a -> a
  foreign panic : string -> a
end

-- =============================================================================
-- Map Functions (foreign)
-- =============================================================================

module Map
  foreign empty : Map v
  foreign singleton : string -> v -> Map v
  foreign insert : string -> v -> Map v -> Map v
  foreign lookup : string -> Map v -> Maybe v
  foreign delete : string -> Map v -> Map v
  foreign member : string -> Map v -> bool
  foreign size : Map v -> int
  foreign keys : Map v -> List string
  foreign values : Map v -> List v
  foreign toList : Map v -> List (string, v)
  foreign fromList : List (string, v) -> Map v
end

-- =============================================================================
-- Set Functions (foreign)
-- =============================================================================

module Set
  foreign empty : Set
  foreign singleton : string -> Set
  foreign insert : string -> Set -> Set
  foreign delete : string -> Set -> Set
  foreign member : string -> Set -> bool
  foreign size : Set -> int
  foreign toList : Set -> List string
  foreign fromList : List string -> Set
  foreign union : Set -> Set -> Set
  foreign intersect : Set -> Set -> Set
  foreign difference : Set -> Set -> Set
end

-- =============================================================================
-- File Functions (foreign)
-- =============================================================================

module File
  foreign read   : string -> Either IOError string
  foreign write  : string -> string -> Either IOError unit
  foreign append : string -> string -> Either IOError unit
  foreign remove : string -> Either IOError unit
  foreign copy   : string -> string -> Either IOError unit
  foreign rename : string -> string -> Either IOError unit
  foreign touch  : string -> Either IOError unit
  foreign exists : string -> bool
  foreign size   : string -> Either IOError int
end

-- =============================================================================
-- Dir Functions (foreign)
-- =============================================================================

module Dir
  foreign create : string -> Either IOError unit
  foreign remove : string -> Either IOError unit
  foreign list   : string -> Either IOError (List string)
  foreign exists : string -> bool
end

-- =============================================================================
-- Path Functions (foreign)
-- =============================================================================

module Path
  foreign join       : string -> string -> string
  foreign dirname    : string -> string
  foreign basename   : string -> string
  foreign extension  : string -> Maybe string
  foreign isAbsolute : string -> bool
end

-- =============================================================================
-- Json Functions (foreign)
-- =============================================================================

module Json
  foreign decode : string -> Either string JsonValue
  foreign encode : a -> string
end
