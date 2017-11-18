module Type where

import Data.List
import Data.Char


type Name = String
type Id = String
data Type = TVar TyVar
          | TInt
          | TBool
          | TFun Type Type
          | TGen Int
          deriving (Eq, Show)

data TyVar = TyVar Id
           deriving (Eq, Show)

enumId :: Int -> Id
enumId n = "v" ++ show n

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

instance Types a => Types [a] where
  applySubst s = map (applySubst s)
  tv = nub . concat . map tv



-- Type schemes for polymorphism
data Scheme = Forall [TyVar] Type
            deriving Eq

instance Types Scheme where
  applySubst s (Forall tvs t) = Forall tvs (applySubst s t)
  tv (Forall tvs t) = tv t

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TFun t1 t2) = TFun (inst ts t1) (inst ts t2)
  inst ts (TGen n) = ts !! n
  inst ts t = t

{-
-- Instantiation of a type scheme
inst :: [Type] -> Scheme -> Type
inst ts (Forall vs t) = applySubst (zip vs ts) t
-}

-- adapted from quantify in THIH
gen :: Env -> Type -> Scheme
gen env t = Forall vars (applySubst s t)
              where vars = (tv t \\ tv env)
                    s = zip vars (map TGen [0..])
{-
gen :: [TyVar] -> Type -> Scheme
gen vars t = Forall vars' (applySubst s t)
                    where vars' = [v | v <- tv t, v `elem` vars]
                          s     = zip vars' (map TGen [0..])
-}

-- taken from THIH
toScheme :: Type -> Scheme
toScheme t = Forall [] t

-- Refined version of generalization
{- WORK IN PROGRESS
gen :: Env -> Type -> Scheme
gen env t = Forall (tv t \\ tvInEnv env) t
  where tvInEnv :: Env -> [TyVar]
        tvInEnv env = concat [tv t | (id, t) <- env]
-}


type Assump = (Id, Scheme)

-- Substitutions
type Subst = [(TyVar, Type)]
nullSubst :: Subst
nullSubst = []


-- composition of two substitutions
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, applySubst s1 t) | (u, t) <- s2] ++ s1

data Env = Env [Assump]
emptyEnv::Env
emptyEnv = Env []

extendEnv :: Env -> Assump -> Env
extendEnv (Env as) elt = Env (elt:as)

envLookup :: Id -> Env -> Maybe Scheme
envLookup x (Env as) = lookup x as

instance Types Env where
  applySubst s as = undefined
  tv as = nub (concat (map tv (getSchemes as)))
          where getSchemes (Env as) = map snd as


-- pretty print
prettyType :: Type -> String
prettyType TInt = "Int"
prettyType TBool = "Bool"
prettyType (TFun x y) = "(" ++ prettyType x ++ " -> " ++ prettyType y ++ ")"
prettyType (TVar (TyVar id)) = id
prettyType (TGen n) = varNumToType n


varNumToType :: Int -> String
varNumToType n
    | n > 26 = chr totalAscii : varNumToType (n `mod` 26)
    | otherwise = [chr totalAscii]
    where baseAscii = 97 -- 97 is ascii decimal for 'a'
          totalAscii = n + baseAscii

