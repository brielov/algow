-- I/O, filesystem, and debugging
type IOError
  = FileNotFound string
  | PermissionDenied string
  | IsDirectory string
  | NotDirectory string
  | AlreadyExists string
  | NotEmpty string
  | UnknownError string

module IO
  foreign print : a -> unit
  foreign printLine : a -> unit
  foreign exit : int -> unit
  foreign getEnv : string -> Maybe string
  foreign getArgs : List string
end

module Debug
  foreign log : a -> a
  foreign trace : string -> a -> a
  foreign panic : string -> a
end

module File
  foreign async read : string -> Either IOError string
  foreign async write : string -> string -> Either IOError unit
  foreign async append : string -> string -> Either IOError unit
  foreign async remove : string -> Either IOError unit
  foreign async copy : string -> string -> Either IOError unit
  foreign async rename : string -> string -> Either IOError unit
  foreign async touch : string -> Either IOError unit
  foreign async exists : string -> bool
  foreign async size : string -> Either IOError int
end

module Dir
  foreign async create : string -> Either IOError unit
  foreign async remove : string -> Either IOError unit
  foreign async list : string -> Either IOError (List string)
  foreign async exists : string -> bool
end

module Path
  foreign join : string -> string -> string
  foreign dirname : string -> string
  foreign basename : string -> string
  foreign extension : string -> Maybe string
  foreign isAbsolute : string -> bool
end
