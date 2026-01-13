-- Primitive type operations
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
  foreign fromList : (List char) -> string
  foreign eq : string -> string -> bool
  foreign lt : string -> string -> bool
  foreign split : string -> string -> List string
  foreign join : string -> (List string) -> string
  foreign trim : string -> string
  foreign toUpper : string -> string
  foreign toLower : string -> string
  foreign contains : string -> string -> bool
  foreign startsWith : string -> string -> bool
  foreign endsWith : string -> string -> bool
  foreign replace : string -> string -> string -> string
  foreign reverse : string -> string
  foreign repeat : int -> string -> string
  foreign indexOf : string -> string -> Maybe int
  foreign lastIndexOf : string -> string -> Maybe int
  foreign padLeft : int -> char -> string -> string
  foreign padRight : int -> char -> string -> string
  foreign lines : string -> List string
  foreign unlines : (List string) -> string
  foreign words : string -> List string
  foreign unwords : (List string) -> string
end
