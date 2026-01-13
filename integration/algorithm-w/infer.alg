-- Algorithm W - Type Inference Implementation
-- Uses types from types.alg

-- Utility Functions
let strEq = String.eq

let rec listMember = x xs ->
  match xs
    when Nil -> false
    when Cons y rest -> if strEq x y then true else listMember x rest
  end

let listUnion = xs ys ->
  List.foldl (acc x -> if listMember x acc then acc else Cons x acc) ys xs

let listDiff = xs ys ->
  List.filter (x -> listMember x ys == false) xs

let rec listNub = xs ->
  match xs
    when Nil -> Nil
    when Cons x rest ->
      if listMember x rest then listNub rest else Cons x (listNub rest)
  end

-- Environment Operations
module Env
  let empty = Env Nil

  let extend = name scheme env ->
    match env when Env bindings -> Env (Cons (name, scheme) bindings) end

  let rec lookup = name env ->
    match env
      when Env Nil -> Nothing
      when Env (Cons (n, s) rest) ->
        if strEq name n then Just s else lookup name (Env rest)
    end

  let remove = name env ->
    match env
      when Env bindings ->
        Env (List.filter (p -> match p when (n, s) -> strEq n name == false end) bindings)
    end
end

-- Substitution Operations
module Subst
  let empty = Subst Nil

  let single = var ty -> Subst (Cons (var, ty) Nil)

  let rec lookup = var s ->
    match s
      when Subst Nil -> Nothing
      when Subst (Cons (v, t) rest) ->
        if strEq var v then Just t else lookup var (Subst rest)
    end

  let rec applyType = s ty ->
    match ty
      when TVar v ->
        match lookup v s
          when Nothing -> TVar v
          when Just t -> t
        end
      when TCon c -> TCon c
      when TFun t1 t2 -> TFun (applyType s t1) (applyType s t2)
    end

  let applyScheme = s scheme ->
    match scheme
      when Forall vars ty ->
        let filtered = match s
          when Subst bindings ->
            Subst (List.filter (p ->
              match p when (v, t) -> listMember v vars == false end
            ) bindings)
        end in
        Forall vars (applyType filtered ty)
    end

  let applyEnv = s env ->
    match env
      when Env bindings ->
        Env (List.map (p ->
          match p when (name, scheme) -> (name, applyScheme s scheme) end
        ) bindings)
    end

  let compose = s2 s1 ->
    match s1
      when Subst bindings1 ->
        match s2
          when Subst bindings2 ->
            let applied = List.map (p ->
              match p when (v, t) -> (v, applyType s2 t) end
            ) bindings1 in
            Subst (List.append bindings2 applied)
        end
    end
end

-- Free Type Variables
module FTV
  let rec inType = ty ->
    match ty
      when TVar v -> Cons v Nil
      when TCon c -> Nil
      when TFun t1 t2 -> listUnion (inType t1) (inType t2)
    end

  let inScheme = scheme ->
    match scheme
      when Forall vars ty -> listDiff (inType ty) vars
    end

  let inEnv = env ->
    match env
      when Env bindings ->
        listNub (List.concat (List.map (p ->
          match p when (name, scheme) -> inScheme scheme end
        ) bindings))
    end
end

-- Occurs Check
let rec occurs = var ty ->
  match ty
    when TVar v -> strEq var v
    when TCon c -> false
    when TFun t1 t2 -> occurs var t1 || occurs var t2
  end

-- Unification
let rec unify = t1 t2 ->
  match (t1, t2)
    when (TVar v1, TVar v2) ->
      if strEq v1 v2 then Ok Subst.empty else Ok (Subst.single v1 t2)

    when (TVar v, t) ->
      if occurs v t
        then Err "infinite type (occurs check)"
        else Ok (Subst.single v t)

    when (t, TVar v) ->
      if occurs v t
        then Err "infinite type (occurs check)"
        else Ok (Subst.single v t)

    when (TCon c1, TCon c2) ->
      if strEq c1 c2 then Ok Subst.empty else Err "type constructor mismatch"

    when (TFun l1 r1, TFun l2 r2) ->
      match unify l1 l2
        when Err e -> Err e
        when Ok s1 ->
          match unify (Subst.applyType s1 r1) (Subst.applyType s1 r2)
            when Err e -> Err e
            when Ok s2 -> Ok (Subst.compose s2 s1)
          end
      end

    when _ -> Err "cannot unify types"
  end

-- Fresh Variables & Instantiation
let freshVar = state ->
  match state
    when State n ->
      (TVar (String.concat "t" (Int.toString n)), State (n + 1))
  end

let instantiate = scheme state ->
  match scheme
    when Forall vars ty ->
      let rec go = vs s st ->
        match vs
          when Nil -> (Subst.applyType s ty, st)
          when Cons v rest ->
            match freshVar st
              when (tv, st2) -> go rest (Subst.compose (Subst.single v tv) s) st2
            end
        end
      in go vars Subst.empty state
  end

let generalize = env ty ->
  let envFree = FTV.inEnv env in
  let tyFree = FTV.inType ty in
  let vars = listNub (listDiff tyFree envFree) in
  Forall vars ty

-- Algorithm W
let rec infer = env expr state ->
  match expr
    when EVar x ->
      match Env.lookup x env
        when Nothing -> (Err (String.concat "unbound: " x), state)
        when Just scheme ->
          match instantiate scheme state
            when (ty, st) -> (Ok (Subst.empty, ty), st)
          end
      end

    when EInt n -> (Ok (Subst.empty, TCon "Int"), state)

    when EBool b -> (Ok (Subst.empty, TCon "Bool"), state)

    when EAbs x body ->
      match freshVar state
        when (tv, st1) ->
          let env2 = Env.extend x (Forall Nil tv) env in
          match infer env2 body st1
            when (Err e, st2) -> (Err e, st2)
            when (Ok (s, t), st2) ->
              (Ok (s, TFun (Subst.applyType s tv) t), st2)
          end
      end

    when EApp e1 e2 ->
      match freshVar state
        when (tv, st1) ->
          match infer env e1 st1
            when (Err e, st2) -> (Err e, st2)
            when (Ok (s1, t1), st2) ->
              match infer (Subst.applyEnv s1 env) e2 st2
                when (Err e, st3) -> (Err e, st3)
                when (Ok (s2, t2), st3) ->
                  match unify (Subst.applyType s2 t1) (TFun t2 (Subst.applyType s2 tv))
                    when Err e -> (Err e, st3)
                    when Ok s3 ->
                      let s = Subst.compose s3 (Subst.compose s2 s1) in
                      (Ok (s, Subst.applyType s3 (Subst.applyType s2 tv)), st3)
                  end
              end
          end
      end

    when ELet x e1 e2 ->
      match infer env e1 state
        when (Err e, st1) -> (Err e, st1)
        when (Ok (s1, t1), st1) ->
          let env2 = Subst.applyEnv s1 env in
          let scheme = generalize env2 t1 in
          match infer (Env.extend x scheme env2) e2 st1
            when (Err e, st2) -> (Err e, st2)
            when (Ok (s2, t2), st2) ->
              (Ok (Subst.compose s2 s1, t2), st2)
          end
      end

    when EIf cond then_ else_ ->
      match infer env cond state
        when (Err e, st1) -> (Err e, st1)
        when (Ok (s1, t1), st1) ->
          match unify t1 (TCon "Bool")
            when Err e -> (Err e, st1)
            when Ok s2 ->
              let env2 = Subst.applyEnv (Subst.compose s2 s1) env in
              match infer env2 then_ st1
                when (Err e, st2) -> (Err e, st2)
                when (Ok (s3, t2), st2) ->
                  let env3 = Subst.applyEnv s3 env2 in
                  match infer env3 else_ st2
                    when (Err e, st3) -> (Err e, st3)
                    when (Ok (s4, t3), st3) ->
                      match unify (Subst.applyType s4 t2) t3
                        when Err e -> (Err e, st3)
                        when Ok s5 ->
                          let s = Subst.compose s5 (Subst.compose s4 (Subst.compose s3 (Subst.compose s2 s1))) in
                          (Ok (s, Subst.applyType s5 t3), st3)
                      end
                  end
              end
          end
      end
  end

-- Top-Level Interface
let typeOf = expr ->
  match infer Env.empty expr (State 0)
    when (Err e, _) -> Left e
    when (Ok (s, ty), _) -> Right ty
  end

-- Pretty print type
let rec showType = ty ->
  match ty
    when TVar v -> v
    when TCon c -> c
    when TFun t1 t2 ->
      let l = match t1
        when TFun _ _ -> String.concat "(" (String.concat (showType t1) ")")
        when _ -> showType t1
      end in
      String.concat l (String.concat " -> " (showType t2))
  end
