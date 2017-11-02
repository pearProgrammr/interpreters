module TyInfer where

import TyInfM
import Type
import Terms


unify :: Type -> Type -> TyInf ()
unify t1 t2 = TyInf (\s n -> case unite (applySubst s t1) (applySubst s t2) of
                               Err msg -> Err msg
                               Answer u -> Answer ((), u @@ s, n))

newVar :: TyInf Type
newVar = TyInf (\s n -> Answer (TVar (TyVar n), s, n+1))

retErrn :: String -> TyInf a
retErrn str = TyInf (\ s n -> Err str)

check env e t
  = do
    t' <- infer env e
    unify t t'

infer :: Env -> Term -> TyInf Type

infer env (Var v)
  = case lookup v env of
      Just t -> return t
      Nothing -> retErrn ("Unbound variable " ++ v)

infer env (IntConst num)
  = return TInt

infer env (BoolConst b)
  = return TBool

infer env (Lambda v e)
  = do
    u <- newVar
    t <- infer ((v, u):env) e
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

infer env (Let v x e)
  = do
    t1 <- infer env x
    t2 <- infer ((v,t1):env) e
    return t2

