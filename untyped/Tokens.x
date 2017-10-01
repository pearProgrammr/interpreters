{
module Tokens where
}

%wrapper "basic"

$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
        $white+ ;
        lambda  { \s -> TokenLambda }
        $alpha+ { \s -> TokenVariable s }
        .       { \s -> TokenDot }
        \(      { \s -> TokenOB }
        \)      { \s -> TokenCB }


{
-- each action has type :: String -> Token
data Token = TokenLambda
           | TokenVariable String
           | TokenDot
           | TokenOB
           | TokenCB
           deriving (Eq, Show)

scanTokens = alexScanTokens
}
