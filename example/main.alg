let decodeTodo =
  json ->
    do[Maybe]
      id <- Json.at "id" Json.int json
      title <- Json.at "title" Json.string json
      completed <- Json.at "completed" Json.bool json
      Just { id = id, title = title, completed = completed }
    end

let decodeTodoList =
  json -> Json.list decodeTodo json

let httpErrorToString =
  err ->
    match err
      when _ -> "Network error"
    end

let main =
  () -> let result = do[Either]
    res <- Either.mapLeft httpErrorToString (Http.get "https://jsonplaceholder.typicode.com/todos?_limit=5")
    json <- Json.decode res.body
    todos <- Either.fromMaybe "Failed to decode" (decodeTodoList json)
    Right todos
  end in
  IO.printLine result
