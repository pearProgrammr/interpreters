module Eval where

import Grammar
import Tokens
import Terms

data Val = Num Int
         | Boolean Bool
         | Func Name Term Env
         deriving Show

type Env = [(String, Val)]

eval :: Term -> Env -> (Val, Env)
eval (Var n) env =
    case lookup n env of
         Just val -> (val, env)
         Nothing  -> error ("Unbound variable " ++ n)

eval (Assign n expr) env =
    case lookup n env of
         Just val -> error (n ++" already exists")
         _        -> case eval expr env of
                          (v, e) -> (v, (n, v):env)

eval (Lambda n body) env = (Func n body env, env)

eval (Let n expr body) env = (fst (eval body ((n, fst(eval expr env)):env)), env)

eval (Apl body param) env =
    case eval body env of
         (Func n body env', env'') -> (fst (eval body ((n, fst(eval param env)):env')), env)
         _                         -> error ("Function required at the left-side of application")


-- Built-in integers
eval (IntConst numval) env = (Num numval, env)

eval (MathOp op e1 e2) env =
    case (eval e1 env, eval e2 env) of
         ((Num val1, env1), (Num val2, env2)) -> (Num (applyOp op val1 val2), env)
         _                                    -> error ("bad math")
         where applyOp Add = (+)
               applyOp Sub = (-)
               applyOp Mul = (*)
               applyOp Div = quot


-- Built-in booleans
eval (BoolConst False) env = (Boolean False, env)
eval (BoolConst True ) env = (Boolean True, env)

eval (Equals e1 e2) env =
    case (eval e1 env, eval e2 env) of
         ((Num val1, env1),     (Num val2, env2))     -> (Boolean (val1 == val2), env)
         ((Boolean val1, env1), (Boolean val2, env2)) -> (Boolean (val1 == val2), env)
         _                                            -> error ("both terms must have equal type when using ==")

eval (If e1 e2 e3) env =
    case eval e1 env of
         (Boolean True, env')  -> eval e2 env
         (Boolean False, env') -> eval e3 env
         _                     -> error ("if predeicate is not a boolean value")


