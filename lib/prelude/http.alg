-- HTTP client
type HttpError
  = NetworkError string
  | TimeoutError
  | InvalidUrl string
  | HttpStatusError int string

module Http
  foreign async get : string -> Either HttpError { status : int, body : string, headers : List (string, string) }
  foreign async post : string -> string -> Either HttpError { status : int, body : string, headers : List (string, string) }
  foreign async put : string -> string -> Either HttpError { status : int, body : string, headers : List (string, string) }
  foreign async delete : string -> Either HttpError { status : int, body : string, headers : List (string, string) }
end
