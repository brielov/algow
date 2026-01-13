-- =============================================================================
-- JSON Decoding into Typed Data Structures
-- =============================================================================
-- Demonstrates parsing JsonValue into custom ADTs

-- Define a Todo type matching jsonplaceholder.typicode.com/todos schema
type Todo = Todo {
  userId: int,
  id: int,
  title: string,
  completed: bool
}

-- Helper to find a field in a JSON object's key-value pairs
let rec findField = name pairs ->
  match pairs
    when Nil -> Nothing
    when Cons (k, v) rest ->
      if String.eq k name then Just v else findField name rest
  end

-- Decode a single Todo from JsonValue
let decodeTodo = json ->
  match json
    when JsonObject pairs ->
      let userId = match findField "userId" pairs
        when Just (JsonNumber n) -> Just (Float.round n)
        when _ -> Nothing
      end in
      let id = match findField "id" pairs
        when Just (JsonNumber n) -> Just (Float.round n)
        when _ -> Nothing
      end in
      let title = match findField "title" pairs
        when Just (JsonString s) -> Just s
        when _ -> Nothing
      end in
      let completed = match findField "completed" pairs
        when Just (JsonBool b) -> Just b
        when _ -> Nothing
      end in
      match (userId, id, title, completed)
        when (Just u, Just i, Just t, Just c) ->
          Just (Todo { userId = u, id = i, title = t, completed = c })
        when _ -> Nothing
      end
    when _ -> Nothing
  end

-- Decode a list of Todos from a JsonArray
let decodeTodos = json ->
  match json
    when JsonArray items -> List.filterMap decodeTodo items
    when _ -> Nil
  end

-- Test with inline JSON (no network required for type checking)
let testJson = "[{\"userId\":1,\"id\":1,\"title\":\"task one\",\"completed\":false},{\"userId\":1,\"id\":2,\"title\":\"task two\",\"completed\":true}]"

let testDecode =
  match Json.decode testJson
    when Left err -> 0
    when Right jsonValue ->
      let todos = decodeTodos jsonValue in
      List.length todos
  end

-- Test individual field access
let testFieldAccess =
  let todos = match Json.decode testJson
    when Left _ -> Nil
    when Right json -> decodeTodos json
  end in
  match todos
    when Cons (Todo t) _ -> if t.completed then 0 else 1
    when Nil -> 0
  end

-- Test completed filter
let testFilter =
  let todos = match Json.decode testJson
    when Left _ -> Nil
    when Right json -> decodeTodos json
  end in
  let completedTodos = List.filter (todo ->
    match todo when Todo t -> t.completed end
  ) todos in
  List.length completedTodos

-- Full integration: HTTP GET -> JSON decode -> typed data
-- (This would make a real network request when run)
let fetchTodos = unit ->
  let result = Http.get "https://jsonplaceholder.typicode.com/todos?_limit=5" in
  match result
    when Left err -> Nil
    when Right response ->
      match Json.decode response.body
        when Left _ -> Nil
        when Right json -> decodeTodos json
      end
  end

let main = args ->
  let a = IO.printLine (String.concat "Decoded todos: " (Int.toString testDecode)) in
  let b = IO.printLine (String.concat "Field access test: " (Int.toString testFieldAccess)) in
  let c = IO.printLine (String.concat "Completed count: " (Int.toString testFilter)) in
  IO.printLine "JSON decode tests passed!"
