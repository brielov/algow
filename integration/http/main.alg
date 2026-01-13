-- Test HTTP module
let main =
  args -> let a = IO.printLine "Testing HTTP GET..." in
  -- Test GET request
  let getResult = Http.get "https://httpbin.org/get" in
  let b = match getResult
    when Left (NetworkError msg) -> IO.printLine (String.concat "Network error: " msg)
    when Left TimeoutError -> IO.printLine "Timeout error"
    when Left (InvalidUrl url) -> IO.printLine (String.concat "Invalid URL: " url)
    when Left (HttpStatusError code body) -> IO.printLine (String.concat "HTTP error: " (Int.toString code))
    when Right response -> let x = IO.printLine (String.concat "GET Status: " (Int.toString response.status)) in
    IO.printLine "GET succeeded!"
  end in
  -- Test POST request
  let c = IO.printLine "Testing HTTP POST..." in
  let postResult = Http.post "https://httpbin.org/post" "Hello from Algow!" in
  let d = match postResult
    when Left (NetworkError msg) -> IO.printLine (String.concat "Network error: " msg)
    when Left TimeoutError -> IO.printLine "Timeout error"
    when Left (InvalidUrl url) -> IO.printLine (String.concat "Invalid URL: " url)
    when Left (HttpStatusError code body) -> IO.printLine (String.concat "HTTP error: " (Int.toString code))
    when Right response -> let x = IO.printLine (String.concat "POST Status: " (Int.toString response.status)) in
    IO.printLine "POST succeeded!"
  end in
  IO.printLine "HTTP tests complete!"
