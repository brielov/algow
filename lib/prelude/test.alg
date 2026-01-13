-- Testing utilities

module Assert
  foreign equal : a -> a -> string -> unit
  foreign notEqual : a -> a -> string -> unit
  foreign ok : bool -> string -> unit
  foreign fail : string -> unit
end
