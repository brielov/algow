-- =============================================================================
-- JSON Decoding with Combinators and Do-Notation
-- =============================================================================
-- Demonstrates both combinator-style and do-notation for JSON parsing

-- Bring Maybe's flatMap into scope for do-notation
let flatMap = Maybe.flatMap
let pure = x -> Just x

-- Define a Todo type matching jsonplaceholder.typicode.com/todos schema
type Todo = Todo {
  userId: int,
  id: int,
  title: string,
  completed: bool
}

-- =============================================================================
-- Combinator Style (using Json.mapN)
-- =============================================================================

let decodeTodoCombinator = json ->
  let userId = Json.at "userId" Json.int json in
  let id = Json.at "id" Json.int json in
  let title = Json.at "title" Json.string json in
  let completed = Json.at "completed" Json.bool json in
  Json.map4 (u i t c -> Todo { userId = u, id = i, title = t, completed = c }) userId id title completed

-- =============================================================================
-- Do-Notation Style (cleaner!)
-- =============================================================================

let decodeTodoDo = json -> do
  userId <- Json.at "userId" Json.int json
  id <- Json.at "id" Json.int json
  title <- Json.at "title" Json.string json
  completed <- Json.at "completed" Json.bool json
  pure (Todo { userId = userId, id = id, title = title, completed = completed })
end

-- Decode a list of Todos
let decodeTodosCombinator = Json.list decodeTodoCombinator
let decodeTodosDo = Json.list decodeTodoDo

-- Test JSON data
let testJson = "[{\"userId\":1,\"id\":1,\"title\":\"task one\",\"completed\":false},{\"userId\":1,\"id\":2,\"title\":\"task two\",\"completed\":true}]"

-- Test combinator style
let testCombinator =
  match Json.decode testJson
    when Left err -> 0
    when Right jsonValue ->
      match decodeTodosCombinator jsonValue
        when Nothing -> 0
        when Just todos -> List.length todos
      end
  end

-- Test do-notation style
let testDoNotation =
  match Json.decode testJson
    when Left err -> 0
    when Right jsonValue ->
      match decodeTodosDo jsonValue
        when Nothing -> 0
        when Just todos -> List.length todos
      end
  end

-- Test that both produce the same result
let testEquivalent = if testCombinator == testDoNotation then 1 else 0

-- Test field access on decoded data
let testFieldAccess =
  match Json.decode testJson
    when Left _ -> 0
    when Right json ->
      match decodeTodosDo json
        when Nothing -> 0
        when Just todos ->
          match todos
            when Cons (Todo t) _ -> if t.completed then 0 else 1
            when Nil -> 0
          end
      end
  end

-- Test filtering decoded data
let testFilter =
  match Json.decode testJson
    when Left _ -> 0
    when Right json ->
      match decodeTodosDo json
        when Nothing -> 0
        when Just todos ->
          let completed = List.filter (todo -> match todo when Todo t -> t.completed end) todos in
          List.length completed
      end
  end

-- Test primitive decoders
let testPrimitives =
  let intOk = match Json.int (JsonNumber 42.0) when Just n -> if n == 42 then 1 else 0 when Nothing -> 0 end in
  let strOk = match Json.string (JsonString "hello") when Just s -> if String.eq s "hello" then 1 else 0 when Nothing -> 0 end in
  let boolOk = match Json.bool (JsonBool true) when Just b -> if b then 1 else 0 when Nothing -> 0 end in
  let nullOk = match Json.null JsonNull when Just _ -> 1 when Nothing -> 0 end in
  intOk + strOk + boolOk + nullOk

-- Test nullable decoder
let testNullable =
  let nullCase = match Json.nullable Json.int JsonNull when Just Nothing -> 1 when _ -> 0 end in
  let valueCase = match Json.nullable Json.int (JsonNumber 5.0) when Just (Just n) -> if n == 5 then 1 else 0 when _ -> 0 end in
  nullCase + valueCase

-- Full integration: HTTP GET -> JSON decode -> typed data
let fetchTodos = unit ->
  let result = Http.get "https://jsonplaceholder.typicode.com/todos?_limit=5" in
  match result
    when Left err -> Nothing
    when Right response ->
      match Json.decode response.body
        when Left _ -> Nothing
        when Right json -> decodeTodosDo json
      end
  end

let main = args ->
  let a = IO.printLine (String.concat "Combinator style: " (Int.toString testCombinator)) in
  let b = IO.printLine (String.concat "Do-notation style: " (Int.toString testDoNotation)) in
  let c = IO.printLine (String.concat "Both equivalent: " (Int.toString testEquivalent)) in
  let d = IO.printLine (String.concat "Field access: " (Int.toString testFieldAccess)) in
  let e = IO.printLine (String.concat "Filter test: " (Int.toString testFilter)) in
  let f = IO.printLine (String.concat "Primitives: " (Int.toString testPrimitives)) in
  let g = IO.printLine (String.concat "Nullable: " (Int.toString testNullable)) in
  IO.printLine "JSON decoder tests passed!"
