-- Data structures and serialization
type JsonValue
  = JsonNull
  | JsonBool bool
  | JsonNumber float
  | JsonString string
  | JsonArray (List JsonValue)
  | JsonObject (List (string, JsonValue))

module Map
  foreign empty : Map v
  foreign singleton : string -> v -> Map v
  foreign insert : string -> v -> (Map v) -> Map v
  foreign lookup : string -> (Map v) -> Maybe v
  foreign delete : string -> (Map v) -> Map v
  foreign member : string -> (Map v) -> bool
  foreign size : (Map v) -> int
  foreign keys : (Map v) -> List string
  foreign values : (Map v) -> List v
  foreign toList : (Map v) -> List (string, v)
  foreign fromList : (List (string, v)) -> Map v
end

module Set
  foreign empty : Set
  foreign singleton : string -> Set
  foreign insert : string -> Set -> Set
  foreign delete : string -> Set -> Set
  foreign member : string -> Set -> bool
  foreign size : Set -> int
  foreign toList : Set -> List string
  foreign fromList : (List string) -> Set
  foreign union : Set -> Set -> Set
  foreign intersect : Set -> Set -> Set
  foreign difference : Set -> Set -> Set
end

module Json
  foreign decode : string -> Either string JsonValue
  foreign encode : a -> string
  let int =
    json ->
      match json
        when JsonNumber n -> Just (Float.round n)
        when _ -> Nothing
      end
  let float =
    json ->
      match json
        when JsonNumber n -> Just n
        when _ -> Nothing
      end
  let string =
    json ->
      match json
        when JsonString s -> Just s
        when _ -> Nothing
      end
  let bool =
    json ->
      match json
        when JsonBool b -> Just b
        when _ -> Nothing
      end
  let null =
    json ->
      match json
        when JsonNull -> Just ()
        when _ -> Nothing
      end
  let rec field = name json ->
    match json
      when JsonObject pairs -> match pairs
        when Nil -> Nothing
        when Cons (k, v) rest -> if String.eq k name then Just v else field name (JsonObject rest)
      end
      when _ -> Nothing
    end
  let at =
    name decoder json -> Maybe.flatMap decoder (field name json)
  let list =
    decoder json ->
      match json
        when JsonArray items -> let rec go = xs acc ->
          match xs
            when Nil -> Just (List.reverse acc)
            when Cons x rest -> match decoder x
              when Nothing -> Nothing
              when Just v -> go rest (Cons v acc)
            end
          end
        in
        go items Nil
        when _ -> Nothing
      end
  let nullable =
    decoder json ->
      match json
        when JsonNull -> Just Nothing
        when _ -> Maybe.map (x -> Just x) (decoder json)
      end
  let withDefault =
    default maybe ->
      match maybe
        when Nothing -> default
        when Just x -> x
      end
  let map =
    f ma ->
      match ma
        when Nothing -> Nothing
        when Just a -> Just (f a)
      end
  let map2 =
    f ma mb ->
      match ma
        when Nothing -> Nothing
        when Just a -> match mb
          when Nothing -> Nothing
          when Just b -> Just (f a b)
        end
      end
  let map3 =
    f ma mb mc ->
      match ma
        when Nothing -> Nothing
        when Just a -> match mb
          when Nothing -> Nothing
          when Just b -> match mc
            when Nothing -> Nothing
            when Just c -> Just (f a b c)
          end
        end
      end
  let map4 =
    f ma mb mc md ->
      match ma
        when Nothing -> Nothing
        when Just a -> match mb
          when Nothing -> Nothing
          when Just b -> match mc
            when Nothing -> Nothing
            when Just c -> match md
              when Nothing -> Nothing
              when Just d -> Just (f a b c d)
            end
          end
        end
      end
  let map5 =
    f ma mb mc md me ->
      match ma
        when Nothing -> Nothing
        when Just a -> match mb
          when Nothing -> Nothing
          when Just b -> match mc
            when Nothing -> Nothing
            when Just c -> match md
              when Nothing -> Nothing
              when Just d -> match me
                when Nothing -> Nothing
                when Just e -> Just (f a b c d e)
              end
            end
          end
        end
      end
end
