{
module Grammar where
import Tokens
}

%name parseUntyped
%tokentype { Token }
%error { parseError }

%token 
      lambda          { TokenLambda }
      var             { TokenVariable $$ }
      constInt        { TokenInt $$ }
      '->'            { TokenRArrow }
      '('             { TokenOB }
      ')'             { TokenCB }
      '+'             { TokenAdd }
      '-'             { TokenSub }
      '*'             { TokenMul }
      '/'             { TokenDiv }


%left '+' '-'
%left '*' '/'

%%

Term  : var                     { Variable $1 }
      | lambda var '->' Term    { Lambda $2 $4 }
      | Term Term               { App $1 $2 }
      | '(' Term ')'            { $2 }
      | constInt                { Const $1 }
      | Term '+' Term           { Add $1 $3 }
      | Term '-' Term           { Sub $1 $3 }
      | Term '*' Term           { Mul $1 $3 }
      | Term '/' Term           { Div $1 $3 }


{
parseError :: [Token] -> a
parseError = error "Parse error"

type Name = String
data Term = Variable Name
          | Const Int 
          | Lambda Name Term
          | App Term Term
          | Brack Term
          | Add Term Term
          | Sub Term Term
          | Mul Term Term
          | Div Term Term
          deriving Show
}
