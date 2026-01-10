-- =============================================================================
-- Types Module
-- Core type definitions for the Algow compiler
-- Reference: SPEC.md Sections 3, 4, 5
-- =============================================================================

module Types

  -- ===========================================================================
  -- Section 3.1: Token Categories
  -- ===========================================================================

  type Token
    -- Literals
    = TokInt int
    | TokFloat float
    | TokString string
    | TokChar char

    -- Identifiers
    | TokLower string        -- foo, _bar (variables, type variables)
    | TokUpper string        -- Just, Nothing (constructors, type constructors)

    -- Keywords
    | TokLet                 -- let
    | TokRec                 -- rec
    | TokAnd                 -- and
    | TokIn                  -- in
    | TokIf                  -- if
    | TokThen                -- then
    | TokElse                -- else
    | TokMatch               -- match
    | TokWhen                -- when
    | TokEnd                 -- end
    | TokType                -- type
    | TokModule              -- module
    | TokUse                 -- use
    | TokForeign             -- foreign
    | TokAs                  -- as
    | TokDo                  -- do
    | TokTrue                -- true
    | TokFalse               -- false

    -- Operators & Punctuation
    | TokPlus                -- +
    | TokMinus               -- -
    | TokStar                -- *
    | TokSlash               -- /
    | TokEq                  -- =
    | TokEqEq                -- ==
    | TokNe                  -- !=
    | TokLt                  -- <
    | TokLe                  -- <=
    | TokGt                  -- >
    | TokGe                  -- >=
    | TokAnd2                -- &&
    | TokOr2                 -- ||
    | TokArrow               -- ->
    | TokLeftArrow           -- <-
    | TokPipe                -- |>
    | TokBar                 -- |
    | TokColon               -- :
    | TokColon2              -- ::
    | TokComma               -- ,
    | TokDot                 -- .
    | TokUnderscore          -- _
    | TokLParen              -- (
    | TokRParen              -- )
    | TokLBracket            -- [
    | TokRBracket            -- ]
    | TokLBrace              -- {
    | TokRBrace              -- }

    -- Special
    | TokEof
    | TokError string

  -- ===========================================================================
  -- Section 3.2: Trivia (Comments & Whitespace)
  -- ===========================================================================

  type Comment
    = LineComment string     -- -- comment
    | BlockComment string    -- {- comment -}

  type Trivia = Trivia (List Comment) (List Comment)  -- leading, trailing

  type Span = Span int int   -- start, end

  type Located a = Located a Span Trivia  -- value, span, trivia

  -- ===========================================================================
  -- Section 5.2: Literals
  -- ===========================================================================

  type Literal
    = LInt int
    | LFloat float
    | LString string
    | LChar char
    | LBool bool

  -- ===========================================================================
  -- Section 4.1: Surface Expressions
  -- ===========================================================================

  type SExpr
    = SVar string                                    -- x
    | SLit Literal                                   -- 42, "hello"
    | SApp SExpr SExpr                               -- f x
    | SAbs (List string) SExpr                       -- x y -> e (multi-param)
    | SLet string SExpr SExpr                        -- let x = e1 in e2
    | SLetRec (List SBinding) SExpr                  -- let rec f = e1 and g = e2 in e
    | SIf SExpr SExpr SExpr                          -- if c then t else f
    | SMatch SExpr (List SCase)                      -- match e when ... end
    | SCon string                                    -- Just, Cons
    | STuple (List SExpr)                            -- (a, b, c)
    | SRecord (List SField)                          -- { x = 1, y = 2 }
    | SRecordUpdate SExpr (List SField)              -- { r | x = 1 }
    | SFieldAccess SExpr string                      -- e.field
    | SList (List SExpr)                             -- [1, 2, 3]
    | SPipeOp SExpr SExpr                            -- e |> f
    | SConsOp SExpr SExpr                            -- x :: xs
    | SBinOp string SExpr SExpr                      -- a + b
    | SUnaryMinus SExpr                              -- -x
    | SDo (List SDoStmt)                             -- do { x <- e; ... } end
    | SAnnot SExpr SType                             -- (e : T)

  type SBinding = SBinding string SExpr              -- name, body

  type SField = SField string SExpr                  -- field name, value

  -- ===========================================================================
  -- Section 4.2: Surface Patterns
  -- ===========================================================================

  type SPattern
    = SPWild                                         -- _
    | SPVar string                                   -- x
    | SPLit Literal                                  -- 42, "hello"
    | SPCon string (List SPattern)                   -- Just x
    | SPTuple (List SPattern)                        -- (a, b)
    | SPRecord (List SPatternField)                  -- { x, y }
    | SPAs string SPattern                           -- x as p
    | SPOr SPattern SPattern                         -- p1 | p2
    | SPCons SPattern SPattern                       -- x :: xs
    | SPList (List SPattern)                         -- [a, b, c]

  type SPatternField = SPatternField string SPattern -- field name, pattern

  -- ===========================================================================
  -- Section 4.3: Surface Cases and Do-Statements
  -- ===========================================================================

  type SCase = SCase SPattern (Maybe SExpr) SExpr    -- pattern, guard, body

  type SDoStmt
    = DoBindPattern SPattern SExpr                   -- (a, b) <- e
    | DoLet SPattern SExpr                           -- let x = e
    | DoExpr SExpr                                   -- e

  -- ===========================================================================
  -- Section 4.4: Surface Types
  -- ===========================================================================

  type SType
    = STVar string                                   -- a (type variable)
    | STCon string                                   -- Int, String
    | STApp SType SType                              -- Maybe Int
    | STFun SType SType                              -- a -> b
    | STTuple (List SType)                           -- (Int, String)
    | STRecord (List STypeField)                     -- { x : Int, y : String }

  type STypeField = STypeField string SType          -- field name, type

  -- ===========================================================================
  -- Section 4.5: Surface Declarations
  -- ===========================================================================

  type SDecl
    = SDeclType string (List string) (List SConDecl) -- type Maybe a = ...
    | SDeclLet string SExpr                          -- let x = e
    | SDeclLetRec (List SBinding)                    -- let rec f = ... and g = ...
    | SDeclForeign string SType                      -- foreign foo : Int -> Int
    | SDeclModule string (List SUse) (List SDecl)    -- module Foo use ... end
    | SDeclUse SUse                                  -- top-level use statement

  type SConDecl = SConDecl string (List SType)       -- constructor name, field types

  type SUse
    = SUseQualified string                           -- use Module
    | SUseAll string                                 -- use Module (..)
    | SUseItems string (List string)                 -- use Module (a, b, c)

  type SProgram = SProgram (List SDecl) (Maybe SExpr) -- declarations, optional main expression

  -- ===========================================================================
  -- Section 5.1: Names (for resolved AST)
  -- ===========================================================================

  type Name = Name int string                        -- unique id, original name

  -- ===========================================================================
  -- Section 5.2: Core Expressions
  -- ===========================================================================

  type CExpr
    = CVar Name                                      -- variable
    | CLit Literal                                   -- literal
    | CApp CExpr CExpr                               -- application (f x)
    | CAbs Name CExpr                                -- lambda (\x -> e), single param
    | CLet Name CExpr CExpr                          -- let x = e1 in e2
    | CLetRec (List CBinding) CExpr                  -- let rec (mutual recursion)
    | CMatch CExpr (List CCase)                      -- pattern match
    | CCon string                                    -- constructor
    | CTuple (List CExpr)                            -- (a, b, c)
    | CRecord (List CField)                          -- { x = 1, y = 2 }
    | CRecordUpdate CExpr (List CField)              -- { r | x = 1 }
    | CFieldAccess CExpr string                      -- e.field
    | CForeign string string                         -- foreign call (module, name)

  type CBinding = CBinding Name CExpr                -- name, body

  type CField = CField string CExpr                  -- field name, value

  -- ===========================================================================
  -- Section 5.3: Core Patterns
  -- ===========================================================================

  type CPattern
    = CPWild                                         -- _
    | CPVar Name                                     -- x (binds)
    | CPLit Literal                                  -- 42, "hello"
    | CPCon string (List CPattern)                   -- Just x
    | CPTuple (List CPattern)                        -- (a, b)
    | CPRecord (List CPatternField)                  -- { x, y }
    | CPAs Name CPattern                             -- x as p
    | CPOr CPattern CPattern                         -- p1 | p2

  type CPatternField = CPatternField string CPattern -- field name, pattern

  -- ===========================================================================
  -- Section 5.4: Core Cases
  -- ===========================================================================

  type CCase = CCase CPattern (Maybe CExpr) CExpr    -- pattern, guard, body

  -- ===========================================================================
  -- Section 5.5: Core Declarations
  -- ===========================================================================

  type CDecl
    = CDeclType string (List string) (List CConDecl) -- type declaration
    | CDeclLet Name CExpr                            -- let binding
    | CDeclLetRec (List CBinding)                    -- recursive bindings
    | CDeclForeign Name string string CType          -- foreign (name, module, jsName, type)
    | CDeclUse SUse                                  -- top-level use (for name resolution)

  type CConDecl = CConDecl string (List CType)       -- constructor name, field types

  -- ===========================================================================
  -- Section 5.6: Core Types (for type annotations and inference)
  -- ===========================================================================

  type CType
    = CTVar string                                   -- type variable
    | CTCon string                                   -- type constructor
    | CTApp CType CType                              -- type application
    | CTFun CType CType                              -- function type
    | CTTuple (List CType)                           -- tuple type
    | CTRecord (List CTypeField)                     -- record type

  type CTypeField = CTypeField string CType          -- field name, type

  -- ===========================================================================
  -- Section 8.1: Types for Type Inference
  -- ===========================================================================

  type Type
    = TVar string                                    -- Type variable (α, β, ...)
    | TCon string                                    -- Type constructor (Int, String, ...)
    | TApp Type Type                                 -- Type application (Maybe Int)
    | TFun Type Type                                 -- Function type (a -> b)
    | TTuple (List Type)                             -- Tuple type (Int, String)
    | TRecord (List TField)                          -- Record type { x : Int }

  type TField = TField string Type                   -- field name, type

  type Scheme = Forall (List string) Type            -- Polymorphic type scheme

  -- ===========================================================================
  -- Utility Types
  -- ===========================================================================

  type ParseResult a
    = ParseOk a (List Token)
    | ParseErr string int                            -- message, position

  type LexResult = LexResult Token string int        -- token, remaining source, position

end
