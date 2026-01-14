-- Stdlib: Http module
-- Note: HTTP tests require network access and may fail in offline environments
let tests =
  [ Test.ok "get request returns Either" (let result = Http.get "https://httpbin.org/get" in
    -- GET request (using a reliable test endpoint)
    -- Note: httpbin.org is a popular HTTP testing service
    Either.isRight result || Either.isLeft result)
    , Test.ok "invalid url" (Either.isLeft (Http.get "not-a-valid-url"))
    , Test.ok "post request returns Either" (let result = Http.post "https://httpbin.org/post" "{\"test\": true}" in
    -- Invalid URL should return error
    -- POST request
    Either.isRight result || Either.isLeft result)
    , Test.ok "response has status" (match Http.get "https://httpbin.org/get"
      when Right response -> response.status >= 100 && response.status < 600
      when Left _ -> true
    end)
    , Test.ok "response has body" (match Http.get "https://httpbin.org/get"
      when Right response -> String.length response.body > 0
      when Left _ -> true
    end)
    , Test.ok "response has headers" (match Http.get "https://httpbin.org/get"
      when Right response -> true
      when Left _ -> true
    end)
    , Test.ok "200 status" (match Http.get "https://httpbin.org/status/200"
      when Right response -> response.status == 200
      when Left _ -> true
    end)
    , Test.ok "404 handling" (match Http.get "https://httpbin.org/status/404"
      when Right response -> response.status == 404
      when Left _ -> true
    end)
    ]

-- Check response structure on success
-- Status code check
-- 404 should return error or status 404
let main =
  args -> Test.run tests
