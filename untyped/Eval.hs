module Eval where

import Grammar
import Tokens

evalUntyped input = printTerm input

printTerm :: Term -> String
printTerm (Variable name)    = "Var " ++ name ++ " "
printTerm (Lambda name term) = "Lambda " ++ name ++ " " ++ (printTerm term) ++ " "
printTerm (App t1 t2)        = "App " ++ (printTerm t1) ++ (printTerm t2) ++ " "
printTerm (Brack term)       =  "( " ++ (printTerm term) ++ " ) "

