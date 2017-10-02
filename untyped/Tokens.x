{
module Tokens where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-
        $white+ ;
        lambda  { \s -> TokenLambda }
        $alpha+ { \s -> TokenVariable s }
        $digit+ { \s -> TokenInt (read s) }
        \.      { \s -> TokenDot }
        \(      { \s -> TokenOB }
        \)      { \s -> TokenCB }
        \+      { \s -> TokenAdd }
        \-      { \s -> TokenSub }
--        \*      { \s -> TokenMul }
--        \/      { \s -> TokenDiv }


{
-- each action has type :: String -> Token
data Token = TokenLambda
           | TokenVariable String
           | TokenInt Int
           | TokenDot
           | TokenOB
           | TokenCB
           | TokenAdd
           | TokenSub
--           | TokenMul
--           | TokenDiv
           deriving (Eq, Show)

scanTokens = alexScanTokens
}
