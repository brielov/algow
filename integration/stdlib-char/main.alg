-- Stdlib: Char module

let tests =
  [ -- toInt / fromInt
    Test.equal "toInt a" 97 (Char.toInt 'a')
  , Test.equal "toInt A" 65 (Char.toInt 'A')
  , Test.equal "toInt 0" 48 (Char.toInt '0')
  , Test.equal "toInt space" 32 (Char.toInt ' ')
  , Test.equal "fromInt 97" (Just 'a') (Char.fromInt 97)
  , Test.equal "fromInt 65" (Just 'A') (Char.fromInt 65)
    -- toString
  , Test.equal "toString a" "a" (Char.toString 'a')
  , Test.equal "toString space" " " (Char.toString ' ')
    -- eq / lt
  , Test.ok "eq true" (Char.eq 'a' 'a')
  , Test.ok "eq false" (Bool.negate (Char.eq 'a' 'b'))
  , Test.ok "lt true" (Char.lt 'a' 'b')
  , Test.ok "lt false" (Bool.negate (Char.lt 'b' 'a'))
  , Test.ok "lt equal" (Bool.negate (Char.lt 'a' 'a'))
    -- isDigit
  , Test.ok "isDigit 0" (Char.isDigit '0')
  , Test.ok "isDigit 9" (Char.isDigit '9')
  , Test.ok "isDigit 5" (Char.isDigit '5')
  , Test.ok "isDigit a false" (Bool.negate (Char.isDigit 'a'))
  , Test.ok "isDigit space false" (Bool.negate (Char.isDigit ' '))
    -- isAlpha
  , Test.ok "isAlpha a" (Char.isAlpha 'a')
  , Test.ok "isAlpha Z" (Char.isAlpha 'Z')
  , Test.ok "isAlpha m" (Char.isAlpha 'm')
  , Test.ok "isAlpha 5 false" (Bool.negate (Char.isAlpha '5'))
  , Test.ok "isAlpha space false" (Bool.negate (Char.isAlpha ' '))
    -- isAlphaNum
  , Test.ok "isAlphaNum a" (Char.isAlphaNum 'a')
  , Test.ok "isAlphaNum 5" (Char.isAlphaNum '5')
  , Test.ok "isAlphaNum space false" (Bool.negate (Char.isAlphaNum ' '))
  , Test.ok "isAlphaNum - false" (Bool.negate (Char.isAlphaNum '-'))
    -- isSpace
  , Test.ok "isSpace space" (Char.isSpace ' ')
  , Test.ok "isSpace tab" (Char.isSpace '\t')
  , Test.ok "isSpace newline" (Char.isSpace '\n')
  , Test.ok "isSpace a false" (Bool.negate (Char.isSpace 'a'))
    -- isUpper
  , Test.ok "isUpper A" (Char.isUpper 'A')
  , Test.ok "isUpper Z" (Char.isUpper 'Z')
  , Test.ok "isUpper a false" (Bool.negate (Char.isUpper 'a'))
  , Test.ok "isUpper 5 false" (Bool.negate (Char.isUpper '5'))
    -- isLower
  , Test.ok "isLower a" (Char.isLower 'a')
  , Test.ok "isLower z" (Char.isLower 'z')
  , Test.ok "isLower A false" (Bool.negate (Char.isLower 'A'))
  , Test.ok "isLower 5 false" (Bool.negate (Char.isLower '5'))
    -- toUpper / toLower
  , Test.equal "toUpper a" 'A' (Char.toUpper 'a')
  , Test.equal "toUpper z" 'Z' (Char.toUpper 'z')
  , Test.equal "toUpper A unchanged" 'A' (Char.toUpper 'A')
  , Test.equal "toUpper 5 unchanged" '5' (Char.toUpper '5')
  , Test.equal "toLower A" 'a' (Char.toLower 'A')
  , Test.equal "toLower Z" 'z' (Char.toLower 'Z')
  , Test.equal "toLower a unchanged" 'a' (Char.toLower 'a')
  , Test.equal "toLower 5 unchanged" '5' (Char.toLower '5')
    -- isIdentStart
  , Test.ok "isIdentStart a" (Char.isIdentStart 'a')
  , Test.ok "isIdentStart A" (Char.isIdentStart 'A')
  , Test.ok "isIdentStart _" (Char.isIdentStart '_')
  , Test.ok "isIdentStart 5 false" (Bool.negate (Char.isIdentStart '5'))
    -- isIdentChar
  , Test.ok "isIdentChar a" (Char.isIdentChar 'a')
  , Test.ok "isIdentChar 5" (Char.isIdentChar '5')
  , Test.ok "isIdentChar _" (Char.isIdentChar '_')
  , Test.ok "isIdentChar space false" (Bool.negate (Char.isIdentChar ' '))
    -- Edge cases
  , Test.ok "digit boundary 0" (Char.isDigit '0')
  , Test.ok "digit boundary 9" (Char.isDigit '9')
  , Test.ok "alpha boundary a" (Char.isAlpha 'a')
  , Test.ok "alpha boundary z" (Char.isAlpha 'z')
  , Test.ok "alpha boundary A" (Char.isAlpha 'A')
  , Test.ok "alpha boundary Z" (Char.isAlpha 'Z')
  ]

let main = args -> Test.run tests
