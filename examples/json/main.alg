-- =============================================================================
-- JSON Module Examples
-- =============================================================================

-- Test decoding various JSON values
let testDecode =
  let arr = Json.decode "[1, 2, 3]" in
  let arrLen = match arr
    when Left x -> 0
    when Right (JsonArray xs) -> List.length xs
    when Right x -> 0
  end in

  let obj = Json.decode "{\"x\": 1, \"y\": 2}" in
  let objOk = match obj
    when Left x -> 0
    when Right (JsonObject pairs) -> List.length pairs
    when Right x -> 0
  end in

  let num = Json.decode "42" in
  let numOk = match num
    when Right (JsonNumber n) -> if Float.eq n 42.0 then 1 else 0
    when x -> 0
  end in

  let str = Json.decode "\"hello\"" in
  let strOk = match str
    when Right (JsonString s) -> if String.eq s "hello" then 1 else 0
    when x -> 0
  end in

  let bool = Json.decode "true" in
  let boolOk = match bool
    when Right (JsonBool b) -> if b then 1 else 0
    when x -> 0
  end in

  let nullVal = Json.decode "null" in
  let nullOk = match nullVal
    when Right JsonNull -> 1
    when x -> 0
  end in

  arrLen + objOk + numOk + strOk + boolOk + nullOk

-- Test encoding various types (polymorphic encode now works!)
let encodeList = Json.encode [1, 2, 3]
let encodeRecord = Json.encode { x = 10, y = 20 }
let encodeStr = Json.encode "hello"
let encodeNum = Json.encode 42

let testEncode =
  let listOk = if String.eq encodeList "[1,2,3]" then 1 else 0 in
  let recordOk = if String.contains "\"x\"" encodeRecord then 1 else 0 in
  let strOk = if String.eq encodeStr "\"hello\"" then 1 else 0 in
  let numOk = if String.eq encodeNum "42" then 1 else 0 in
  listOk + recordOk + strOk + numOk

let result = testDecode + testEncode

let main = args ->
  let a = IO.printLine (String.concat "Decode tests: " (Int.toString testDecode)) in
  let b = IO.printLine (String.concat "Encode tests: " (Int.toString testEncode)) in
  IO.printLine (String.concat "Total: " (Int.toString result))
