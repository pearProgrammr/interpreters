module TypeCheck where

import Tokens
import Grammar
import Terms

data TypeVal = TyNum | TyBool | TyFunc Name Term TyEnv
             deriving (Show, Eq)

type TyEnv = [(String, TypeVal)]

checkType :: Term -> TyEnv -> TypeVal

checkType (Var n) tyEnv =
    case lookup n tyEnv of
        Just val -> val
        Nothing  -> error ("unbound variable " ++ n)

checkType (Assign n expr) tyEnv =
    case lookup n tyEnv of
        Just val -> error (n ++ " already exists")
        Nothing  -> checkType expr tyEnv


checkType (Lambda n b) tyEnv = TyFunc n b tyEnv

checkType (Apl e1 e2) tyEnv =
    case checkType e2 tyEnv of
        TyFunc n b tyEnv' -> checkType b ((n,checkType e1 tyEnv):tyEnv')
        _                 -> error ("Applications require function type")

checkType (IntConst num) tyEnv = TyNum

checkType (MathOp op e1 e2) tyEnv =
    case (checkType e1 tyEnv, checkType e2 tyEnv) of
        (TyNum, TyNum) -> TyNum
        _              -> error ("arithmetic operators must be applied to integer type")

checkType (Let n e b) tyEnv =
     checkType b ((n, checkType e tyEnv):tyEnv)

checkType (BoolConst True)  tyEnv = TyBool
checkType (BoolConst False) tyEnv = TyBool

checkType (Equals e1 e2) tyEnv =
    case (checkType e1 tyEnv, checkType e2 tyEnv) of
         (tv1, tv2) -> if tv1 == tv2 
                          then tv1
                          else error (show tv1 ++ " and " ++ show tv2 ++ " are not equivalent")

checkType (If pred thenExpr elseExpr) tyEnv =
    case checkType pred tyEnv of
         TyBool -> case (checkType thenExpr tyEnv, checkType elseExpr tyEnv) of
                        (tv1, tv2) -> if tv1 == tv2
                                         then tv1
                                         else error ("then and else clause have differing types")
