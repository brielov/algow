-- Algorithm W - Type Definitions
-- Shared types for the type inference implementation

-- Types in simply-typed lambda calculus with let-polymorphism
type Type
  = TVar string
  | TCon string
  | TFun Type Type

-- Type scheme: forall a1 a2 ... an. Type
type Scheme = Forall (List string) Type

-- Expressions
type Expr
  = EVar string
  | EInt int
  | EBool bool
  | EAbs string Expr
  | EApp Expr Expr
  | ELet string Expr Expr
  | EIf Expr Expr Expr

-- Type environment: variable name -> type scheme
type Env = Env (List (string, Scheme))

-- Substitution: type variable -> type
type Subst = Subst (List (string, Type))

-- Inference result
type Result a
  = Err string
  | Ok a

-- State for fresh variable generation
type State = State int
