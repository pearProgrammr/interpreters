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


-- Instantiation of a type scheme
inst :: Scheme -> [Type] -> Type
inst (Forall vs t) ts = applySubst (zip vs ts) t

-- Refined version of generalization
gen :: Env -> Type -> Scheme
gen env t = Forall (tv t \\ tvInEnv env) t
  where tvInEnv :: Env -> [TyVar]
        tvInEnv env = concat [tv t | (id, t) <- env]


-- Substitutions
type Subst = [(TyVar, Type)]
nullSubst :: Subst
nullSubst = []

-- composition of two substitutions
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, applySubst s1 t) | (u, t) <- s2] ++ s1

type Env = [(Name, Type)]
emptyEnv::Env
emptyEnv=[]

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

