module Type where

import Data.List
import Data.Char


type Name = String
type Id = String
data Type = TVar TyVar
          | TCon TyCon
          | TFun Type Type
          | TGen Int
          deriving (Eq, Show)

data TyVar = TyVar Id
           deriving (Eq, Show)

data TyCon = PrimTy Id
           | DataTy Id [Constr]
           deriving Eq 

instance Show TyCon where
   show (PrimTy i)    = "primitive " ++ i
   show (DataTy i cs) = "data " ++ i ++ " where\n" ++ unlines [ "  " ++ showConstr c | c <- cs ]

nameTyCon              :: TyCon -> Id
nameTyCon (PrimTy i)    = i
nameTyCon (DataTy i cs) = i

findTyCon           :: Id -> CEnv -> TyCon
findTyCon i env      = case [ tc | tc <- env, nameTyCon tc == i ] of
                         []     -> error ("undefined type constructor " ++ i)
                         (tc:_) -> tc


-- Constructor Functions:

type Constr = (Id, Type)

nameConstr        :: Constr -> Id
nameConstr (i, ts) = i

findConstr        :: Id -> CEnv -> Constr
findConstr i cEnv   = head [ c | DataTy i cs <- cEnv, c <- cs, nameConstr c == i ]

showConstr        :: Constr -> String
showConstr (i, t)  = i ++ " :: " ++ show t


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

type Assump = (Id, Scheme)

-- adapted from quantify in THIH
gen :: TyEnv -> Type -> Scheme
gen tyEnv t = Forall vars (apply s t)
              where vars = (tv t \\ tv tyEnv)
                    s = zip vars (map TGen [0..])
-- taken from THIH
toScheme :: Type -> Scheme
toScheme t = Forall [] t


-- Substitutions

type Subst = [(TyVar, Type)]
nullSubst :: Subst
nullSubst = []


-- composition of two substitutions
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1


-- Type Environment

data TyEnv = TyEnv [Assump]
emptyTyEnv :: TyEnv
emptyTyEnv = TyEnv []

extendTyEnv :: TyEnv -> Assump -> TyEnv
extendTyEnv (TyEnv as) elt = TyEnv (elt:as)

tyEnvLookup :: Id -> TyEnv -> Maybe Scheme
tyEnvLookup x (TyEnv as) = lookup x as

instance Types TyEnv where
  apply s as = undefined
  tv as = nub (concat (map tv (getSchemes as)))
          where getSchemes (TyEnv as) = map snd as

-- Abstract syntax for "parsed" programs:

type Prog    = [Defn]

data Defn    = Data Id [(Id, [TypeExp])]

data TypeExp = TEFunc TypeExp TypeExp
             | TECon  Id

-- Constructor Environments: for now, these just hold lists of types that have
-- either been specified as primitives or defined as an algebraic
-- datatype.

type CEnv  = [TyCon]

prelude  :: CEnv
prelude   = [PrimTy "Int"]

printEnv :: CEnv -> IO ()
printEnv  = mapM_ (putStrLn . show)


-- Calculating the type corresponding to a given type expression:

toType                   :: CEnv -> TypeExp -> Type
toType cEnv (TEFunc dt rt) = TFun (toType cEnv dt) (toType cEnv rt)
toType cEnv (TECon i)      = TCon (findTyCon i cEnv)

-- Calculating the new environment that is produced by adding a new
-- group of type definitions to an existing environment:
elaborate       :: Prog -> CEnv -> CEnv
elaborate ds cEnv = newenv
 where 
       -- An extended environment that adds the types defined in ds:
       newenv :: CEnv
       newenv  = [ tc | Data i ces  <- ds,
                        let tc = DataTy i (map (constr tc) ces) ] ++ cEnv

       -- If a constructor called ci of a type constructor tc has fields
       -- described by type expressions tes = [te1, ..., ten], and if the
       -- corresponding types are t1, ..., tn, then the constructor function
       -- is described by:  ci :: t1 -> ... -> tn -> tc
       constr             :: TyCon -> (Id, [TypeExp]) -> Constr
       constr tc (ci, tes) = (ci, foldr TFun (TCon tc) (map (toType newenv) tes))




-- pretty print
prettyType :: Type -> String
--prettyType TInt = "Int"
--prettyType TBool = "Bool"
prettyType (TFun x y) = "(" ++ prettyType x ++ " -> " ++ prettyType y ++ ")"
prettyType (TVar (TyVar id)) = id
prettyType (TGen n) = varNumToType n


varNumToType :: Int -> String
varNumToType n
    | n > 26 = chr totalAscii : varNumToType (n `mod` 26)
    | otherwise = [chr totalAscii]
    where baseAscii = 97 -- 97 is ascii decimal for 'a'
          totalAscii = n + baseAscii

