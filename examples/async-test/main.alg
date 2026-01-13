-- Test async file operations

let main = args ->
  -- Write to a file
  let writeResult = File.write "/tmp/algow-test.txt" "Hello from Algow!" in
  let a = match writeResult
    when Left err -> IO.printLine "Write failed"
    when Right unit -> IO.printLine "Write succeeded"
  end in

  -- Read from the file
  let readResult = File.read "/tmp/algow-test.txt" in
  let b = match readResult
    when Left err -> IO.printLine "Read failed"
    when Right content -> IO.printLine (String.concat "Read: " content)
  end in

  -- Check if file exists
  let existsResult = File.exists "/tmp/algow-test.txt" in
  let c = if existsResult
    then IO.printLine "File exists: true"
    else IO.printLine "File exists: false"
  in

  -- Clean up
  let removeResult = File.remove "/tmp/algow-test.txt" in
  match removeResult
    when Left err -> IO.printLine "Remove failed"
    when Right unit -> IO.printLine "Remove succeeded"
  end
