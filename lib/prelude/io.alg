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
end

module Debug
  foreign log : a -> a
  foreign trace : string -> a -> a
  foreign panic : string -> a
end

module File
  foreign read : string -> Either IOError string
  foreign write : string -> string -> Either IOError unit
  foreign append : string -> string -> Either IOError unit
  foreign remove : string -> Either IOError unit
  foreign copy : string -> string -> Either IOError unit
  foreign rename : string -> string -> Either IOError unit
  foreign touch : string -> Either IOError unit
  foreign exists : string -> bool
  foreign size : string -> Either IOError int
end

module Dir
  foreign create : string -> Either IOError unit
  foreign remove : string -> Either IOError unit
  foreign list : string -> Either IOError (List string)
  foreign exists : string -> bool
end

module Path
  foreign join : string -> string -> string
  foreign dirname : string -> string
  foreign basename : string -> string
  foreign extension : string -> Maybe string
  foreign isAbsolute : string -> bool
end
