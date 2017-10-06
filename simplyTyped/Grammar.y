{
module Grammar where
import Tokens
}

%name parseSimplyTyped
%tokentype { Token }
%error { parseError }

%token 
      -- lambda calculus symbols --
      '\\'            { TokenLambda }
      '->'            { TokenRArrow }
      var             { TokenVariable $$ }
      '('             { TokenOB }
      ')'             { TokenCB }

      -- integer and integer operands --
      constInt        { TokenInt $$ }
      '+'             { TokenAdd }
      '-'             { TokenSub }
      '*'             { TokenMul }
      '/'             { TokenDiv }

      -- logic and logical operands
      true            { TokenTrue }
      false           { TokenFalse }
      '=='            { TokenEq }

      -- scope-altering tokens --
      let             { TokenLet }
      '='             { TokenAssign }
      in              { TokenIn }

      -- control-flow --
      if              { TokenIf }
      then            { TokenThen }
      else            { TokenElse }


%right '='

%left '+' '-'
%left '*' '/'

%%

Term  : var                         { Variable $1 }
      | '\\' var '->' Term          { Lambda $2 $4 }
      | let var '=' Term in Term    { Let $2 $4 $6 }
      | Term Term                   { App $1 $2 }
      | '(' Term ')'                { $2 }
      | constInt                    { Const $1 }
      | Term '+' Term               { MathOp Add $1 $3 }
      | Term '-' Term               { MathOp Sub $1 $3 }
      | Term '*' Term               { MathOp Mul $1 $3 }
      | Term '/' Term               { MathOp Div $1 $3 }
      | true                        { ConstTrue }
      | false                       { ConstFalse }
      | Term '==' Term              { Equals $1 $3 }
      | if Term then Term else Term { If $2 $4 $6}


{
parseError :: [Token] -> a
parseError = error "Parse error"



type Name = String
data Term = Variable Name
          | Const Int 
          | Lambda Name Term
          | App Term Term
          | Brack Term
          | MathOp Op Term Term
          | Let Name Term Term
          | ConstTrue
          | ConstFalse
          | Equals Term Term
          | If Term Term Term
          deriving (Show, Eq)

data Op = Add | Sub | Mul | Div
        deriving (Show, Eq)
}
