module TyInfer where

import TyInfM
import Type
import Terms


unify :: Type -> Type -> TyInf ()
unify t1 t2 = TyInf (\s n -> case unite (applySubst s t1) (applySubst s t2) of
                               Err msg -> Err msg
                               Answer u -> Answer ((), u @@ s, n))

-- is there a way to chain these things??
newVar :: TyInf Type
newVar = TyInf (\s n -> Answer (TVar (TyVar (enumId n)), s, n+1))

-- There has to be a better way to do this...
enum :: Int -> TyInf Type
enum idx = newVar


-- adapted from freshInst in THIH
newInst :: Scheme -> TyInf Type
newInst (Forall tvs t)
  = do
    ts <- mapM enum [0..(length tvs)] -- There has to be a better way to do this
    return (inst ts t)


retErrn :: String -> TyInf a
retErrn str = TyInf (\ s n -> Err str)

check env e t
  = do
    t' <- infer env e
    unify t t'

infer :: Env -> Term -> TyInf Type

infer env (Var v)
  = case lookup v env of
      Nothing -> retErrn ("Unbound variable " ++ v)
      Just tSch -> newInst tSch

infer env (IntConst num)
  = return TInt

infer env (BoolConst b)
  = return TBool

infer env (Lambda v e)
  = do
    u <- newVar
    t <- infer ((v, toScheme u):env) e
    return (TFun u t)

infer env (Apl l r)
  = do
    t1 <- infer env l
    t2 <- infer env r
    v <- newVar
    unify t1 (TFun t2 v)
    return v

infer env (MathOp op n1 n2)
  = do
    check env n1 TInt
    check env n2 TInt
    return TInt

infer env (Equals x y)
  = do
    t1 <- infer env x
    t2 <- infer env y
    unify t2 t1
    return TBool

infer env (If c e1 e2)
  = do
    check env c TBool
    t2 <- infer env e1
    t3 <- infer env e2
    unify t3 t2
    return t3

infer env (Assign v e)
  = infer env e

infer env (Let v x e) -- it seems like let is the crux of all of this.. why was it even included? 
  = do
    t1 <- infer env x
    t2 <- infer ((v, gen (tv t1) t1):env) e -- generalize the type of x
    return t2

