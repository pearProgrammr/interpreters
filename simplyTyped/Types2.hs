module Types2 where

type Id = String

data Type = TVar TyVar
          | TInt
          | TBool
          | TFun Type Type
          deriving (Eq, Show)

data TyVar = TyVar Id Int
           deriving (Eq, Show)

type Subst = [(TyVar, Type)]
nullSubst :: Subst
nullSubst = []

class Types t where
  applySubst :: Subst -> t -> t
  tv    :: t -> [TyVar]

instance Types Type where
  applySubst s (TVar v) = case lookup v s of
                          Just t -> t
                          Nothing -> TVar v
  applySubst s (TFun l r) = TFun (applySubst s l) (applySubst s r)
  applySubst s t          = t

  tv (TVar u) = [u]
  tv (TFun l r) = unique (tv l ++ tv r)
                  where unique (x:xs) = if x `elem` xs then xs else (x:xs)
                        unique []     = []
  tv _          = []

-- composition of two substitutions
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, applySubst s1 t) | (u, t) <- s2] ++ s1

--unification algorithm
unify TInt TInt = []
unify TBool TBool = []
unify (TVar v) (TVar w) = [(v, TVar w) | v /= w]
unify (TVar v) t = varBind v t
unify t (TVar v) = varBind v t
unify (TFun t1 t2) (TFun t1' t2')
  = let s1 = unify t1 t1'
        s2 = unify (applySubst s1 t2) (applySubst s1 t2')
      in (s2 @@ s1)
unify t1 t2 = error ("Cannot unify "++ show t1 ++ " with " ++ show t2)

-- if we have types that refer to each other in some circular fashion,
-- that's an error!!!
varBind v t = if (v `elem` vars t)
                 then error ("Occurs check fails!")
                 else [(v, t)]
              where vars (TVar v)   = [v]
                    vars TInt       = []
                    vars TBool      = []
                    vars (TFun l r) = vars l ++ vars r

type Name = String
data Term = Var Name
          | Ap Term Term
          | Lam Name Term
          | Num Int
          | Boolean Bool

type Env = [(Name, Type)]

extend env (n,t) = (n,t):env

envLookup :: Name -> Env -> Type
envLookup n env = case lookup n env of
                       Just t -> t
                       Nothing -> error ("unbound variable " ++ n)

newVar n = (TVar (TyVar "none" n), n+1)

infer :: Env -> Term -> Int -> (Subst, Type, Int)
infer env (Var v) n
  = let t = envLookup v env
        in ([], t, n)
infer env (Num num) n
  = ([], TInt, n)

infer env (Boolean b) n
  = ([], TBool, n)

infer env (Lam v e) n
  = let (u, n1) = newVar n
        (s, t, n2) = infer ((v, u):env) e n1
    in (s, TFun (applySubst s u) t, n2)


infer env (Ap l r) n
  = let (s1, t1, n1) = infer env l n
        (s2, t2, n2) = infer (applySubstToEnv s1 env) r n1
        (v, n3) = newVar n2
        s3 = unify (applySubst s2 t1) (TFun t2 v)
    in (s3 @@ s2 @@ s1, applySubst s3 v, n3)


applySubstToEnv :: Subst -> Env -> Env
applySubstToEnv s env = [(n, applySubst s t) | (n, t) <- env]
