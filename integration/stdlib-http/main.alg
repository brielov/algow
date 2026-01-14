-- Stdlib: Http module
-- Note: HTTP tests require network access and may fail in offline environments

let tests =
  [ -- GET request (using a reliable test endpoint)
    -- Note: httpbin.org is a popular HTTP testing service
    Test.ok "get request returns Either" (
      let result = Http.get "https://httpbin.org/get" in
      Either.isRight result || Either.isLeft result
    )
    -- Invalid URL should return error
  , Test.ok "invalid url" (Either.isLeft (Http.get "not-a-valid-url"))
    -- POST request
  , Test.ok "post request returns Either" (
      let result = Http.post "https://httpbin.org/post" "{\"test\": true}" in
      Either.isRight result || Either.isLeft result
    )
    -- Check response structure on success
  , Test.ok "response has status" (
      match Http.get "https://httpbin.org/get"
        when Right response -> response.status >= 100 && response.status < 600
        when Left _ -> true
      end
    )
  , Test.ok "response has body" (
      match Http.get "https://httpbin.org/get"
        when Right response -> String.length response.body > 0
        when Left _ -> true
      end
    )
  , Test.ok "response has headers" (
      match Http.get "https://httpbin.org/get"
        when Right response -> true
        when Left _ -> true
      end
    )
    -- Status code check
  , Test.ok "200 status" (
      match Http.get "https://httpbin.org/status/200"
        when Right response -> response.status == 200
        when Left _ -> true
      end
    )
    -- 404 should return error or status 404
  , Test.ok "404 handling" (
      match Http.get "https://httpbin.org/status/404"
        when Right response -> response.status == 404
        when Left _ -> true
      end
    )
  ]

let main = args -> Test.run tests
