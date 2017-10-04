module Eval where

import Grammar
import Tokens

evalUntyped input = show $ eval input []

data Val = Num Int
         | Func Name Term Env
         deriving Show

type Env = [(String, Val)]

eval :: Term -> Env -> Val
eval (Variable n) env = case lookup n env of
                            Just val -> val
                            Nothing  -> error ("Unbound variable " ++ n)

eval (Lambda n body) env = Func n body env

eval (Let n expr body) env = eval body ((n, eval expr env):env)

eval (App body param) env =
    case eval body env of
         Func n body env' -> eval body ((n, eval param env):env')
         _                -> error ("Function required at the left-side of application")

eval (Const numval) env = Num numval

eval (MathOp op e1 e2) env = 
    case (eval e1 env, eval e2 env) of
         (Num val1, Num val2) -> Num (applyOp op val1 val2)
         _                    -> error ("bad math")

applyOp Add = (+) 
applyOp Sub = (-)
applyOp Mul = (*)
applyOp Div = quot


printTerm :: Term -> String
printTerm (Variable n)       = "Var " ++ n ++ " "
printTerm (Lambda n t)       = "Lambda " ++ n ++ " " ++ printTerm t ++ " "
printTerm (App t1 t2)        = "App " ++ printTerm t1 ++ printTerm t2 ++ " "
printTerm (Brack t)          =  "( " ++ printTerm t ++ " ) "
printTerm (Const val)        = "Constant Value " ++ show val
printTerm (MathOp Add e1 e2) = printTerm e1 ++ " + " ++ printTerm e2 ++ " "
printTerm (MathOp Sub e1 e2) = printTerm e1 ++ " - " ++ printTerm e2 ++ " "
printTerm (MathOp Mul e1 e2) = printTerm e1 ++ " * " ++ printTerm e2 ++ " "
printTerm (MathOp Div e1 e2) = printTerm e1 ++ " / " ++ printTerm e2 ++ " "
printTerm (Let n e b)        = "Let " ++ n ++ " = " ++ printTerm e ++ " in " ++ printTerm b
