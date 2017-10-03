{
module Tokens where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-
        $white+ ;
        lambda  { \s -> TokenLambda }
        '->'    { \s -> TokenRArrow }
        let     { \s -> TokenLet }
        =       { \s -> TokenAssign }
        in      { \s -> TokenIn }
        $alpha+ { \s -> TokenVariable s }
        $digit+ { \s -> TokenInt (read s) }
        \(      { \s -> TokenOB }
        \)      { \s -> TokenCB }
        \+      { \s -> TokenAdd }
        \-      { \s -> TokenSub }
        \*      { \s -> TokenMul }
        \/      { \s -> TokenDiv }


{
-- each action has type :: String -> Token
data Token = TokenLambda
           | TokenVariable String
           | TokenInt Int
           | TokenRArrow
           | TokenOB
           | TokenCB
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenDiv
           | TokenLet
           | TokenAssign
           | TokenIn
           deriving (Eq, Show)

scanTokens = alexScanTokens
}
