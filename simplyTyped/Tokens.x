{
module Tokens where
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-
        -- integer and integer operands --
        $digit+ { \s -> TokenInt (read s) }
        \(      { \s -> TokenOB }
        \)      { \s -> TokenCB }
        \+      { \s -> TokenAdd }
        \-      { \s -> TokenSub }
        \*      { \s -> TokenMul }
        \/      { \s -> TokenDiv }

        -- logic and logical operands --
        ==      { \s -> TokenEq }
        true    { \s -> TokenTrue }
        false   { \s -> TokenFalse }
        if      { \s -> TokenIf }
        then    { \s -> TokenThen }
        else    { \s -> TokenElse }

        -- general symbols --
        $white+ ;
        lambda  { \s -> TokenLambda }
        '->'    { \s -> TokenRArrow }
        let     { \s -> TokenLet }
        =       { \s -> TokenAssign }
        in      { \s -> TokenIn }
        $alpha+ { \s -> TokenVariable s }




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
           | TokenEq
           | TokenTrue
           | TokenFalse
           | TokenIf
           | TokenThen
           | TokenElse
           deriving (Eq, Show)

scanTokens = alexScanTokens
}
