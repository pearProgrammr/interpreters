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
      '.'             { TokenDot }
      '('             { TokenOB }
      ')'             { TokenCB }

%%

Term  : var                     { Variable $1 }
      | lambda var '.' Term     { Lambda $2 $4 }
      | Term Term               { App $1 $2 }
      | '(' Term ')'            { $2 }

{

parseError :: [Token] -> a
parseError = error "Parse error"

type Name = String
data Term = Variable Name
          | Lambda Name Term
          | App Term Term
          | Brack Term
          deriving Show
}
