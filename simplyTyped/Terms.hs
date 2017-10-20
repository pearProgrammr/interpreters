module Terms where

type Name = String
data Term = Variable Name
          | Const Int 
          | Lambda Name Term
          | App Term Term
          | MathOp Op Term Term
          | Let Name Term Term
          | ConstTrue
          | ConstFalse
          | Equals Term Term
          | If Term Term Term
          | Assign String Term
          deriving (Show, Eq)

data Op = Add | Sub | Mul | Div
        deriving (Show, Eq)

