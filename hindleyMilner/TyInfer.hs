module TyInfer where

import TyInfM
import Type
import Terms


unify :: Type -> Type -> TyInf ()
unify t1 t2 = TyInf (\c s n -> case unite (apply s t1) (apply s t2) of
                               Err msg -> Err msg
                               Answer u -> Answer ((), c, u @@ s, n))

-- is there a way to chain these things??
newVar :: TyInf Type
newVar = TyInf (\c s n -> Answer (TVar (TyVar (enumId n)), c, s, n+1))


-- There has to be a better way to do this... it all ends up looking similar
enum :: Int -> TyInf Type
enum idx = newVar


-- adapted from freshInst in THIH
newInst :: Scheme -> TyInf Type
newInst (Forall tvs t)
  = do
    ts <- mapM enum [0..(length tvs)] -- There has to be a better way to do this
    return (inst ts t)

retErrn :: String -> TyInf a
retErrn str = TyInf (\c s n -> Err str)

check tyEnv e t
  = do
    t' <- infer tyEnv e
    unify t t'

infer :: TyEnv -> Term -> TyInf Type

-- core language constructs

infer tyEnv (Var v)
  = case tyEnvLookup v tyEnv of
      Nothing -> retErrn ("Unbound variable " ++ v)
      Just tSch -> newInst tSch

infer tyEnv (Lambda v e)
  = do
    u <- newVar
    t <- infer (extendTyEnv tyEnv (v, toScheme u)) e
    return (TFun u t)

infer tyEnv (Apl l r)
  = do
    t1 <- infer tyEnv l
    t2 <- infer tyEnv r
    v <- newVar
    unify t1 (TFun t2 v)
    return v

infer tyEnv (Let v x e)
  = do
    t1 <- infer tyEnv x
    t2 <- infer (extendTyEnv tyEnv (v, gen tyEnv t1)) e -- generalize the type of x
    return t2

-- create a type called dat and store this in the CEnv
-- the type of this will be the type that we associate with dat in CEnv
-- for now, I will associate this with a type variable...
infer tyEnv (Data datName [cs])
  = do
    v <- newVar
    return v

{-
-- Integer operations

infer tyEnv (IntConst num)
  = return TInt

infer tyEnv (MathOp op n1 n2)
  = do
    check tyEnv n1 TInt
    check tyEnv n2 TInt
    return TInt


-- Boolean-related operations

infer tyEnv (BoolConst b)
  = return TBool

infer tyEnv (Equals x y)
  = do
    t1 <- infer tyEnv x
    t2 <- infer tyEnv y
    unify t2 t1
    return TBool

infer tyEnv (If c e1 e2)
  = do
    check tyEnv c TBool
    t2 <- infer tyEnv e1
    t3 <- infer tyEnv e2
    unify t3 t2
    return t3

-}
infer tyEnv (Assign v e)
  = infer tyEnv e

