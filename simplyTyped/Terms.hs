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

