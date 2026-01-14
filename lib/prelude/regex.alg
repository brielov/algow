-- Regular expression operations
module Regex
  -- Test if pattern matches anywhere in the string
  foreign test : string -> string -> bool

  -- Find the first match
  foreign find : string -> string -> Maybe { value : string, index : int, groups : List string }

  -- Find all matches
  foreign findAll : string -> string -> List { value : string, index : int, groups : List string }

  -- Replace first occurrence
  foreign replace : string -> string -> string -> string

  -- Replace all occurrences
  foreign replaceAll : string -> string -> string -> string

  -- Split string by pattern
  foreign split : string -> string -> List string

  -- Split string by pattern with limit
  foreign splitLimit : string -> int -> string -> List string
end
