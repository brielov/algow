-- =============================================================================
-- Algorithm W - Type Definitions
-- =============================================================================
-- This module defines the core types used in the type inference algorithm.

-- =============================================================================
-- Type Representation
-- =============================================================================

-- Types in the lambda calculus with let-polymorphism
type Type
  = TVar string                    -- Type variable: a, b, c, etc.
  | TCon string                    -- Type constructor: Int, Bool, etc.
  | TFun Type Type                 -- Function type: a -> b

-- Type schemes for let-polymorphism: forall a b c. Type
-- The list contains the quantified type variables
type Scheme = Forall (List string) Type

-- =============================================================================
-- Expressions (Simple Lambda Calculus)
-- =============================================================================

type Expr
  = EVar string                    -- Variable: x
  | ELit Lit                       -- Literal: 42, true
  | EAbs string Expr               -- Lambda: \x -> e
  | EApp Expr Expr                 -- Application: e1 e2
  | ELet string Expr Expr          -- Let binding: let x = e1 in e2

type Lit
  = LInt int
  | LBool bool

-- =============================================================================
-- Type Environment
-- =============================================================================

-- Maps variable names to their type schemes
type TypeEnv = TypeEnv (List (string, Scheme))

module TypeEnv
  let empty = TypeEnv Nil

  let extend = name scheme env ->
    match env
      when TypeEnv bindings -> TypeEnv (Cons (name, scheme) bindings)
    end

  let rec lookup = name env ->
    match env
      when TypeEnv Nil -> Nothing
      when TypeEnv (Cons (n, s) rest) ->
        if String.eq name n
          then Just s
          else lookup name (TypeEnv rest)
    end

  let rec remove = name env ->
    match env
      when TypeEnv Nil -> TypeEnv Nil
      when TypeEnv (Cons (n, s) rest) ->
        if String.eq name n
          then remove name (TypeEnv rest)
          else
            match remove name (TypeEnv rest)
              when TypeEnv remaining -> TypeEnv (Cons (n, s) remaining)
            end
    end

  let toList = env ->
    match env
      when TypeEnv bindings -> bindings
    end
end

-- =============================================================================
-- Substitution
-- =============================================================================

-- Maps type variables to types
type Subst = Subst (List (string, Type))

module Subst
  let empty = Subst Nil

  let singleton = var ty -> Subst (Cons (var, ty) Nil)

  let rec lookup = var subst ->
    match subst
      when Subst Nil -> Nothing
      when Subst (Cons (v, t) rest) ->
        if String.eq var v
          then Just t
          else lookup var (Subst rest)
    end

  let toList = s ->
    match s
      when Subst bindings -> bindings
    end

  -- Compose two substitutions: (s2 `compose` s1) means apply s1 first, then s2
  let compose = s2 s1 ->
    match (s1, s2)
      when (Subst bindings1, Subst bindings2) ->
        let applied = List.map (p ->
          match p
            when (v, t) -> (v, applyType s2 t)
          end
        ) bindings1 in
        Subst (List.append bindings2 applied)
    end

  -- Apply substitution to a type
  and applyType = subst ty ->
    match ty
      when TVar v ->
        match lookup v subst
          when Nothing -> TVar v
          when Just t -> t
        end
      when TCon c -> TCon c
      when TFun t1 t2 -> TFun (applyType subst t1) (applyType subst t2)
    end

  -- Apply substitution to a scheme
  let applyScheme = subst scheme ->
    match scheme
      when Forall vars ty ->
        -- Remove bound variables from substitution
        let filteredSubst = List.foldl (acc var ->
          match acc
            when Subst bindings ->
              Subst (List.filter (p ->
                match p
                  when (v, t) -> String.eq v var == false
                end
              ) bindings)
          end
        ) subst vars in
        Forall vars (applyType filteredSubst ty)
    end

  -- Apply substitution to an environment
  let applyEnv = subst env ->
    match env
      when TypeEnv bindings ->
        TypeEnv (List.map (p ->
          match p
            when (name, scheme) -> (name, applyScheme subst scheme)
          end
        ) bindings)
    end
end

-- =============================================================================
-- Free Type Variables
-- =============================================================================

module FreeVars
  -- Get free type variables in a type
  let rec freeInType = ty ->
    match ty
      when TVar v -> Cons v Nil
      when TCon c -> Nil
      when TFun t1 t2 -> List.append (freeInType t1) (freeInType t2)
    end

  -- Get free type variables in a scheme
  let freeInScheme = scheme ->
    match scheme
      when Forall vars ty ->
        List.filter (v -> List.elem v vars == false) (freeInType ty)
    end

  -- Get free type variables in an environment
  let freeInEnv = env ->
    match env
      when TypeEnv bindings ->
        List.concat (List.map (p ->
          match p
            when (name, scheme) -> freeInScheme scheme
          end
        ) bindings)
    end
end

-- Types exported for other modules
