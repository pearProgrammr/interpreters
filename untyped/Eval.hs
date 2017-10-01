module Eval where

import Grammar
import Tokens

evalUntyped input = eval input

eval :: Term -> String
eval (Variable name)    = "Var " ++ name ++ " "
eval (Lambda name term) = "Lambda " ++ name ++ " " ++ (eval term) ++ " "
eval (App t1 t2)        = "App " ++ (eval t1) ++ (eval t2) ++ " "
eval (Brack term)       =  "( " ++ (eval term) ++ " ) "

