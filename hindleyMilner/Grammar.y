{
module Grammar where
import Tokens
import Terms
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

      -- data types --
      Data            { TokenData }
      '|'             { TokenBar }

%right '='

%left '|'
%left '+' '-'
%left '*' '/'

%%

Term  : var                         { Var $1 }
      | var '=' Term                { Assign $1 $3 }
      | '\\' var '->' Term          { Lambda $2 $4 }
      | let var '=' Term in Term    { Let $2 $4 $6 }
      | Term Term                   { Apl $1 $2 }
      | '(' Term ')'                { $2 }
      | constInt                    { IntConst $1 }
      | Term '+' Term               { MathOp Add $1 $3 }
      | Term '-' Term               { MathOp Sub $1 $3 }
      | Term '*' Term               { MathOp Mul $1 $3 }
      | Term '/' Term               { MathOp Div $1 $3 }
      | true                        { BoolConst True }
      | false                       { BoolConst False }
      | Term '==' Term              { Equals $1 $3 }
      | if Term then Term else Term { If $2 $4 $6 }
      | Data var '=' cList          { Data $2 $4 }

cList : pList                       { [Constr $1] }
      | pList '|' cList             { (Constr $1) : $3}

pList : var                         { [$1] }
      | var pList                   { $1 : $2 }

{
parseError :: [Token] -> a
parseError = error "Parse error"
}
