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
  apply :: Subst -> t -> t
  tv    :: t -> [TyVar]

instance Types Type where
  apply s (TVar v) = case lookup v s of
                          Just t -> t
                          Nothing -> TVar v
  apply s (TFun l r) = TFun (apply s l) (apply s r)
  apply s t          = t

  tv (TVar u) = [u]
  tv (TFun l r) = unique (tv l ++ tv r)
                  where unique (x:xs) = if x `elem` xs then xs else (x:xs)
                        unique []     = []
  tv _          = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv = nub . concat . map tv


-- Type schemes for polymorphism
data Scheme = Forall [TyVar] Type
            deriving Eq

instance Types Scheme where
  apply s (Forall tvs t) = Forall tvs (apply s t)
  tv (Forall tvs t) = tv t

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TFun t1 t2) = TFun (inst ts t1) (inst ts t2)
  inst ts (TGen n) = ts !! n
  inst ts t = t

-- adapted from quantify in THIH
gen :: TyEnv -> Type -> Scheme
gen tyEnv t = Forall vars (apply s t)
              where vars = (tv t \\ tv tyEnv)
                    s = zip vars (map TGen [0..])
-- taken from THIH
toScheme :: Type -> Scheme
toScheme t = Forall [] t


type Assump = (Id, Scheme)

-- Substitutions
type Subst = [(TyVar, Type)]
nullSubst :: Subst
nullSubst = []


-- composition of two substitutions
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

data TyEnv = TyEnv [Assump]
emptyTyEnv::TyEnv
emptyTyEnv = TyEnv []

extendTyEnv :: TyEnv -> Assump -> TyEnv
extendTyEnv (TyEnv as) elt = TyEnv (elt:as)

tyEnvLookup :: Id -> TyEnv -> Maybe Scheme
tyEnvLookup x (TyEnv as) = lookup x as

instance Types TyEnv where
  apply s as = undefined
  tv as = nub (concat (map tv (getSchemes as)))
          where getSchemes (TyEnv as) = map snd as


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

