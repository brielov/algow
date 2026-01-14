-- Project: Regular Expression Matcher
-- An NFA-based regex matcher supporting core regex features

-- Regex AST
type Regex
  = REmpty
  | REpsilon
  | RChar char
  | RAny
  | RSeq Regex Regex
  | RAlt Regex Regex
  | RStar Regex
  | RPlus Regex
  | ROptional Regex
  | RClass (List char) bool

-- NFA state representation
type StateId = StateId int

type NFATransition
  = TEpsilon StateId
  | TChar char StateId
  | TAny StateId
  | TClass (List char) bool StateId

type NFAState = NFAState StateId (List NFATransition)

type NFA = NFA StateId StateId (List NFAState)

-- State counter for NFA construction
type Counter = Counter int

let freshState = counter -> match counter when Counter n ->
  (StateId n, Counter (n + 1))
end

-- Parse regex string to AST
let rec parseRegex = input ->
  match parseAlt input
    when Left e -> Left e
    when Right (regex, rest) ->
      if String.isEmpty rest then Right regex
      else Left "unexpected characters after regex"
  end

and parseAlt = input ->
  match parseSeq input
    when Left e -> Left e
    when Right (left, rest) -> parseAltCont left rest
  end

and parseAltCont = left input ->
  if String.isEmpty input then Right (left, input)
  else match String.head input
    when Nothing -> Right (left, input)
    when Just '|' ->
      match parseSeq (String.tail input)
        when Left e -> Left e
        when Right (right, rest) -> parseAltCont (RAlt left right) rest
      end
    when Just _ -> Right (left, input)
  end

and parseSeq = input ->
  match parseQuantified input
    when Left e -> Left e
    when Right (left, rest) -> parseSeqCont left rest
  end

and parseSeqCont = left input ->
  if String.isEmpty input then Right (left, input)
  else match String.head input
    when Nothing -> Right (left, input)
    when Just '|' -> Right (left, input)
    when Just ')' -> Right (left, input)
    when Just _ ->
      match parseQuantified input
        when Left e -> Left e
        when Right (right, rest) -> parseSeqCont (RSeq left right) rest
      end
  end

and parseQuantified = input ->
  match parseAtom input
    when Left e -> Left e
    when Right (atom, rest) ->
      if String.isEmpty rest then Right (atom, rest)
      else match String.head rest
        when Nothing -> Right (atom, rest)
        when Just '*' -> Right (RStar atom, String.tail rest)
        when Just '+' -> Right (RPlus atom, String.tail rest)
        when Just '?' -> Right (ROptional atom, String.tail rest)
        when Just _ -> Right (atom, rest)
      end
  end

and parseAtom = input ->
  if String.isEmpty input then Right (REpsilon, input)
  else match String.head input
    when Nothing -> Right (REpsilon, input)
    when Just '(' ->
      match parseAlt (String.tail input)
        when Left e -> Left e
        when Right (inner, rest) ->
          if String.isEmpty rest then Left "unclosed group"
          else match String.head rest
            when Just ')' -> Right (inner, String.tail rest)
            when _ -> Left "expected ')'"
          end
      end
    when Just '[' -> parseCharClass (String.tail input)
    when Just '.' -> Right (RAny, String.tail input)
    when Just '\\' ->
      let rest = String.tail input in
      if String.isEmpty rest then Left "incomplete escape"
      else match String.head rest
        when Nothing -> Left "incomplete escape"
        when Just c -> Right (RChar c, String.tail rest)
      end
    when Just '|' -> Right (REpsilon, input)
    when Just ')' -> Right (REpsilon, input)
    when Just '*' -> Left "unexpected *"
    when Just '+' -> Left "unexpected +"
    when Just '?' -> Left "unexpected ?"
    when Just c -> Right (RChar c, String.tail input)
  end

and parseCharClass = input ->
  let (negated, rest) = match String.head input
    when Just '^' -> (true, String.tail input)
    when _ -> (false, input)
  end in
  match parseCharClassContent [] rest
    when Left e -> Left e
    when Right (chars, remaining) -> Right (RClass chars negated, remaining)
  end

and parseCharClassContent = acc input ->
  if String.isEmpty input then Left "unclosed character class"
  else match String.head input
    when Nothing -> Left "unclosed character class"
    when Just ']' -> Right (List.reverse acc, String.tail input)
    when Just '\\' ->
      let rest = String.tail input in
      if String.isEmpty rest then Left "incomplete escape in class"
      else match String.head rest
        when Nothing -> Left "incomplete escape in class"
        when Just c -> parseCharClassContent (c :: acc) (String.tail rest)
      end
    when Just c -> parseCharClassContent (c :: acc) (String.tail input)
  end

-- Build NFA from regex
let rec buildNFA = regex counter ->
  match regex
    when REmpty ->
      let (start, c1) = freshState counter in
      let (accept, c2) = freshState c1 in
      (NFA start accept [NFAState start [], NFAState accept []], c2)

    when REpsilon ->
      let (start, c1) = freshState counter in
      let (accept, c2) = freshState c1 in
      (NFA start accept [NFAState start [TEpsilon accept], NFAState accept []], c2)

    when RChar ch ->
      let (start, c1) = freshState counter in
      let (accept, c2) = freshState c1 in
      (NFA start accept [NFAState start [TChar ch accept], NFAState accept []], c2)

    when RAny ->
      let (start, c1) = freshState counter in
      let (accept, c2) = freshState c1 in
      (NFA start accept [NFAState start [TAny accept], NFAState accept []], c2)

    when RClass chars negated ->
      let (start, c1) = freshState counter in
      let (accept, c2) = freshState c1 in
      (NFA start accept [NFAState start [TClass chars negated accept], NFAState accept []], c2)

    when RSeq r1 r2 ->
      let (nfa1, c1) = buildNFA r1 counter in
      let (nfa2, c2) = buildNFA r2 c1 in
      match (nfa1, nfa2) when (NFA s1 a1 states1, NFA s2 a2 states2) ->
        let newStates1 = List.map (s -> match s when NFAState id trans ->
          if stateEq id a1
          then NFAState id (TEpsilon s2 :: trans)
          else s
        end) states1 in
        (NFA s1 a2 (List.append newStates1 states2), c2)
      end

    when RAlt r1 r2 ->
      let (nfa1, c1) = buildNFA r1 counter in
      let (nfa2, c2) = buildNFA r2 c1 in
      let (start, c3) = freshState c2 in
      let (accept, c4) = freshState c3 in
      match (nfa1, nfa2) when (NFA s1 a1 states1, NFA s2 a2 states2) ->
        let newStates1 = List.map (s -> match s when NFAState id trans ->
          if stateEq id a1
          then NFAState id (TEpsilon accept :: trans)
          else s
        end) states1 in
        let newStates2 = List.map (s -> match s when NFAState id trans ->
          if stateEq id a2
          then NFAState id (TEpsilon accept :: trans)
          else s
        end) states2 in
        let startState = NFAState start [TEpsilon s1, TEpsilon s2] in
        let acceptState = NFAState accept [] in
        (NFA start accept (startState :: acceptState :: List.append newStates1 newStates2), c4)
      end

    when RStar r ->
      let (inner, c1) = buildNFA r counter in
      let (start, c2) = freshState c1 in
      let (accept, c3) = freshState c2 in
      match inner when NFA s1 a1 states ->
        let newStates = List.map (s -> match s when NFAState id trans ->
          if stateEq id a1
          then NFAState id (TEpsilon s1 :: TEpsilon accept :: trans)
          else s
        end) states in
        let startState = NFAState start [TEpsilon s1, TEpsilon accept] in
        let acceptState = NFAState accept [] in
        (NFA start accept (startState :: acceptState :: newStates), c3)
      end

    when RPlus r ->
      let (inner, c1) = buildNFA r counter in
      let (start, c2) = freshState c1 in
      let (accept, c3) = freshState c2 in
      match inner when NFA s1 a1 states ->
        let newStates = List.map (s -> match s when NFAState id trans ->
          if stateEq id a1
          then NFAState id (TEpsilon s1 :: TEpsilon accept :: trans)
          else s
        end) states in
        let startState = NFAState start [TEpsilon s1] in
        let acceptState = NFAState accept [] in
        (NFA start accept (startState :: acceptState :: newStates), c3)
      end

    when ROptional r ->
      let (inner, c1) = buildNFA r counter in
      let (start, c2) = freshState c1 in
      let (accept, c3) = freshState c2 in
      match inner when NFA s1 a1 states ->
        let newStates = List.map (s -> match s when NFAState id trans ->
          if stateEq id a1
          then NFAState id (TEpsilon accept :: trans)
          else s
        end) states in
        let startState = NFAState start [TEpsilon s1, TEpsilon accept] in
        let acceptState = NFAState accept [] in
        (NFA start accept (startState :: acceptState :: newStates), c3)
      end
  end

and stateEq = s1 s2 -> match (s1, s2) when (StateId a, StateId b) -> a == b end

-- Find state by ID
let rec findState = id states -> match states
  when Nil -> Nothing
  when Cons (NFAState sid trans) rest ->
    if stateEq sid id then Just (NFAState sid trans) else findState id rest
end

-- Compute epsilon closure of a set of states
let rec epsilonClosure = stateIds nfa visited ->
  match stateIds
    when Nil -> visited
    when Cons id rest ->
      if List.any (v -> stateEq v id) visited
      then epsilonClosure rest nfa visited
      else match nfa when NFA _ _ states ->
        match findState id states
          when Nothing -> epsilonClosure rest nfa visited
          when Just (NFAState _ trans) ->
            let epsilonTargets = List.filterMap (t -> match t
              when TEpsilon target -> Just target
              when _ -> Nothing
            end) trans in
            epsilonClosure (List.append epsilonTargets rest) nfa (id :: visited)
        end
      end
  end

-- Step NFA with a character
let stepNFA = currentStates ch nfa ->
  match nfa when NFA _ _ states ->
    let nextStates = List.concat (List.map (sid ->
      match findState sid states
        when Nothing -> []
        when Just (NFAState _ trans) ->
          List.filterMap (t -> match t
            when TChar c target -> if c == ch then Just target else Nothing
            when TAny target -> Just target
            when TClass chars negated target ->
              let inClass = List.any (c -> c == ch) chars in
              if (inClass && Bool.negate negated) || (Bool.negate inClass && negated)
              then Just target
              else Nothing
            when TEpsilon _ -> Nothing
          end) trans
      end
    ) currentStates) in
    epsilonClosure nextStates nfa []
  end

-- Check if any state is the accept state
let isAccepting = currentStates acceptState ->
  List.any (s -> stateEq s acceptState) currentStates

-- Run NFA on input string
let runNFA = nfa input ->
  match nfa when NFA start accept _ ->
    let initialStates = epsilonClosure [start] nfa [] in
    let rec go = states chars -> match chars
      when Nil -> isAccepting states accept
      when Cons c rest ->
        let nextStates = stepNFA states c nfa in
        if List.isEmpty nextStates then false
        else go nextStates rest
    end in
    go initialStates (String.toList input)
  end

-- High-level match function
let matches = pattern input ->
  match parseRegex pattern
    when Left _ -> false
    when Right regex ->
      let (nfa, _) = buildNFA regex (Counter 0) in
      runNFA nfa input
  end

-- Compile a regex pattern
let compile = pattern ->
  match parseRegex pattern
    when Left e -> Left e
    when Right regex ->
      let (nfa, _) = buildNFA regex (Counter 0) in
      Right nfa
  end

-- Match with compiled NFA
let matchCompiled = nfa input -> runNFA nfa input

-- Find first match in string (returns start index)
let rec findFirst = pattern input ->
  match compile pattern
    when Left _ -> Nothing
    when Right nfa -> findFirstFrom nfa input 0
  end

and findFirstFrom = nfa input idx ->
  if idx >= String.length input then Nothing
  else
    let suffix = String.drop idx input in
    if matchPrefix nfa suffix then Just idx
    else findFirstFrom nfa input (idx + 1)

and matchPrefix = nfa input ->
  match nfa when NFA start accept _ ->
    let initialStates = epsilonClosure [start] nfa [] in
    let rec go = states chars -> match chars
      when Nil -> isAccepting states accept
      when Cons c rest ->
        if isAccepting states accept then true
        else
          let nextStates = stepNFA states c nfa in
          if List.isEmpty nextStates then isAccepting states accept
          else go nextStates rest
    end in
    go initialStates (String.toList input)
  end

-- Tests
let tests =
  [ -- Basic characters
    Test.ok "char match" (matches "a" "a")
  , Test.ok "char no match" (Bool.negate (matches "a" "b"))
  , Test.ok "string match" (matches "abc" "abc")
  , Test.ok "string no match" (Bool.negate (matches "abc" "abd"))

    -- Any character
  , Test.ok "dot matches any" (matches "a.c" "abc")
  , Test.ok "dot matches digit" (matches "a.c" "a1c")
  , Test.ok "dot no match empty" (Bool.negate (matches "a.c" "ac"))
  , Test.ok "multiple dots" (matches "..." "abc")

    -- Alternation
  , Test.ok "alt first" (matches "a|b" "a")
  , Test.ok "alt second" (matches "a|b" "b")
  , Test.ok "alt no match" (Bool.negate (matches "a|b" "c"))
  , Test.ok "alt three" (matches "a|b|c" "c")
  , Test.ok "alt words" (matches "cat|dog" "dog")

    -- Kleene star
  , Test.ok "star empty" (matches "a*" "")
  , Test.ok "star one" (matches "a*" "a")
  , Test.ok "star many" (matches "a*" "aaaa")
  , Test.ok "star in seq" (matches "a*b" "aaab")
  , Test.ok "star in seq empty" (matches "a*b" "b")

    -- Plus (one or more)
  , Test.ok "plus one" (matches "a+" "a")
  , Test.ok "plus many" (matches "a+" "aaaa")
  , Test.ok "plus no empty" (Bool.negate (matches "a+" ""))
  , Test.ok "plus in seq" (matches "a+b" "aab")

    -- Optional
  , Test.ok "optional present" (matches "a?" "a")
  , Test.ok "optional absent" (matches "a?" "")
  , Test.ok "optional in seq" (matches "ab?c" "ac")
  , Test.ok "optional in seq present" (matches "ab?c" "abc")

    -- Character classes
  , Test.ok "class match first" (matches "[abc]" "a")
  , Test.ok "class match middle" (matches "[abc]" "b")
  , Test.ok "class match last" (matches "[abc]" "c")
  , Test.ok "class no match" (Bool.negate (matches "[abc]" "d"))
  , Test.ok "negated class match" (matches "[^abc]" "d")
  , Test.ok "negated class no match" (Bool.negate (matches "[^abc]" "a"))

    -- Groups
  , Test.ok "group simple" (matches "(ab)" "ab")
  , Test.ok "group star" (matches "(ab)*" "abab")
  , Test.ok "group alt" (matches "(a|b)c" "ac")
  , Test.ok "group alt 2" (matches "(a|b)c" "bc")
  , Test.ok "nested groups" (matches "((a))" "a")

    -- Escapes
  , Test.ok "escape star" (matches "a\\*b" "a*b")
  , Test.ok "escape dot" (matches "a\\.b" "a.b")
  , Test.ok "escape backslash" (matches "a\\\\b" "a\\b")

    -- Complex patterns
  , Test.ok "email-like" (matches "[a-z]+@[a-z]+\\.[a-z]+" "test@example.com")
  , Test.ok "repeated groups" (matches "(ab)+" "ababab")
  , Test.ok "complex alt" (matches "(cat|dog)s?" "cats")
  , Test.ok "complex alt 2" (matches "(cat|dog)s?" "dog")
  , Test.ok "nested star" (matches "(a*b)*" "aabab")
  , Test.ok "complex 1" (matches "a(b|c)*d" "abcbd")
  , Test.ok "complex 2" (matches "(a|b)*abb" "aabb")
  , Test.ok "complex 3" (matches "(a|b)*abb" "babb")

    -- Empty and epsilon
  , Test.ok "empty pattern" (matches "" "")
  , Test.ok "empty pattern no match" (Bool.negate (matches "" "a"))

    -- Prefix matching
  , Test.equal "find first simple" (Just 0) (findFirst "abc" "abcdef")
  , Test.equal "find first offset" (Just 3) (findFirst "def" "abcdef")
  , Test.equal "find first pattern" (Just 2) (findFirst "a+" "xxaaax")
  , Test.equal "find first not found" Nothing (findFirst "xyz" "abcdef")

    -- Compile and match
  , Test.ok "compile and match" (match compile "a+b*"
      when Left _ -> false
      when Right nfa -> matchCompiled nfa "aaab"
    end)

    -- Edge cases
  , Test.ok "single char" (matches "x" "x")
  , Test.ok "all dots" (matches "....." "abcde")
  , Test.ok "star at start" (matches ".*abc" "xyzabc")
  , Test.ok "star at end" (matches "abc.*" "abcxyz")
  , Test.ok "plus with class" (matches "[ab]+" "abba")
  , Test.ok "optional group" (matches "(ab)?" "")
  , Test.ok "optional group present" (matches "(ab)?c" "abc")
  , Test.ok "alternation priority" (matches "ab|abc" "ab")

    -- Longer patterns
  , Test.ok "longer pattern 1" (matches "hello.*world" "hello beautiful world")
  , Test.ok "longer pattern 2" (matches "(foo|bar)+(baz)?" "foobarfoobaz")
  , Test.ok "word pattern" (matches "[a-z][a-z]*" "hello")
  ]

let main = args -> Test.run tests
