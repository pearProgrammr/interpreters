module Eval where

import Grammar
import Tokens

evalUntyped input = show $ eval input []

-- TODO: clean this up. This can be simplified with the show function
printTerm :: Term -> String
printTerm (Variable name)    = "Var " ++ name ++ " "
printTerm (Lambda name term) = "Lambda " ++ name ++ " " ++ (printTerm term) ++ " "
printTerm (App t1 t2)        = "App " ++ (printTerm t1) ++ (printTerm t2) ++ " "
printTerm (Brack term)       =  "( " ++ (printTerm term) ++ " ) "
printTerm (Const val)        = "Constant Value " ++ (show val)
printTerm (Add e1 e2)        = (printTerm e1) ++ " + " ++ (printTerm e2) ++ " "
printTerm (Sub e1 e2)        = (printTerm e1) ++ " - " ++ (printTerm e2) ++ " "
printTerm (Let n e b)        = "Let " ++ n ++ " = " ++ printTerm e ++ " in " ++ printTerm b

data Val = Num Int
         | Func Name Term Env
         deriving Show

type Env = [(String, Val)]

eval :: Term -> Env -> Val
eval (Variable n) env = case lookup n env of
                            Just val -> val
                            Nothing  -> error ("Unbound variable " ++ n)

eval (Lambda name body) env = Func name body env

eval (Let n expr body) env = eval body ((n, eval expr env):env)

eval (App body param) env =
    case eval body env of
         Func n body env' -> eval body ((n, eval param env):env')
         _                -> error ("Function required at the left-side of application")

eval (Const numval) env = Num numval
eval (Add e1 e2) env = evalMathOp e1 e2 env (+)
eval (Sub e1 e2) env = evalMathOp e1 e2 env (-)
eval (Mul e1 e2) env = evalMathOp e1 e2 env (*)
eval (Div e1 e2) env = evalMathOp e1 e2 env quot

evalMathOp :: Term -> Term -> Env -> (Int -> Int -> Int) -> Val
evalMathOp e1 e2 env op =
    case (eval e1 env, eval e2 env) of
         (Num val1, Num val2) -> Num (val1 `op` val2)
         _                    -> error ("bad math")
