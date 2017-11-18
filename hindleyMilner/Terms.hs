module Terms where

type Name = String
data Term = Var Name
          | IntConst Int
          | Lambda Name Term
          | Apl Term Term
          | MathOp Op Term Term
          | Let Name Term Term
          | BoolConst Bool
          | Equals Term Term
          | If Term Term Term
          | Assign String Term
          deriving (Show, Eq)

data Op = Add | Sub | Mul | Div
        deriving (Show, Eq)

-- use this for debugging
printTerm :: Term -> String
printTerm (Var n)            = "Var " ++ n ++ " "
printTerm (Lambda n t)       = "Lambda " ++ n ++ " " ++ printTerm t ++ " "
printTerm (Apl t1 t2)        = "Apl " ++ printTerm t1 ++ printTerm t2 ++ " "
printTerm (IntConst val)     = "Constant Value " ++ show val
printTerm (MathOp Add e1 e2) = printTerm e1 ++ " + " ++ printTerm e2 ++ " "
printTerm (MathOp Sub e1 e2) = printTerm e1 ++ " - " ++ printTerm e2 ++ " "
printTerm (MathOp Mul e1 e2) = printTerm e1 ++ " * " ++ printTerm e2 ++ " "
printTerm (MathOp Div e1 e2) = printTerm e1 ++ " / " ++ printTerm e2 ++ " "
printTerm (Let n e b)        = "Let " ++ n ++ " = " ++ printTerm e ++ " in " ++ printTerm b
