-- Random number generation
module Random
  foreign int : int -> int -> int
  foreign float : float
  foreign bool : bool
  foreign pick : (List a) -> Maybe a
  foreign shuffle : (List a) -> List a
end
