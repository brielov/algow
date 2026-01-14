-- Stdlib: File module (testing file operations in temp directory)

let tempFile = "/tmp/algow_test_file.txt"
let tempFile2 = "/tmp/algow_test_file2.txt"

-- Clean up any leftover test files
let cleanup = dummy ->
  let r1 = File.remove tempFile in
  let r2 = File.remove tempFile2 in
  true

let tests =
  let ready = cleanup () in
  [ -- write and read
    Test.ok "write file" (Either.isRight (File.write tempFile "hello world"))
  , Test.ok "read file" (match File.read tempFile when Right "hello world" -> true when _ -> false end)
    -- exists
  , Test.ok "exists after write" (File.exists tempFile)
  , Test.ok "not exists before create" (Bool.negate (File.exists "/tmp/algow_nonexistent_12345.txt"))
    -- append
  , Test.ok "append" (Either.isRight (File.append tempFile "\nline 2"))
  , Test.ok "read after append" (match File.read tempFile when Right "hello world\nline 2" -> true when _ -> false end)
    -- size
  , Test.ok "size is Right" (Either.isRight (File.size tempFile))
  , Test.ok "size positive" (match File.size tempFile when Right n -> n > 0 when Left e -> false end)
    -- copy
  , Test.ok "copy" (Either.isRight (File.copy tempFile tempFile2))
  , Test.ok "copy exists" (File.exists tempFile2)
    -- rename (using temp file 2)
  , Test.ok "rename" (Either.isRight (File.rename tempFile2 "/tmp/algow_test_renamed.txt"))
  , Test.ok "renamed exists" (File.exists "/tmp/algow_test_renamed.txt")
  , Test.ok "original not exists after rename" (Bool.negate (File.exists tempFile2))
    -- touch
  , Test.ok "touch new file" (Either.isRight (File.touch tempFile2))
  , Test.ok "touched exists" (File.exists tempFile2)
    -- remove
  , Test.ok "remove" (Either.isRight (File.remove tempFile2))
  , Test.ok "not exists after remove" (Bool.negate (File.exists tempFile2))
    -- remove renamed file
  , Test.ok "remove renamed" (Either.isRight (File.remove "/tmp/algow_test_renamed.txt"))
    -- Error cases
  , Test.ok "read nonexistent" (Either.isLeft (File.read "/tmp/algow_nonexistent_12345.txt"))
  , Test.ok "remove nonexistent" (Either.isLeft (File.remove "/tmp/algow_nonexistent_12345.txt"))
    -- Cleanup
  , Test.ok "cleanup" (Either.isRight (File.remove tempFile))
  ]

let main = args -> Test.run tests
