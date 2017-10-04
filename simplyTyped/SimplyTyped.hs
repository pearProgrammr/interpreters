module Main where
import System.IO
import Eval
import Grammar
import Tokens
import TypeCheck


evalLoop = do
         flushBuff "> "
         input <- getLine
         if input == ":q"
             then do
                 putStrLn "Leaving SimplyTyped"
                 return ()
             else do
                 --flushBuff input
                 let tokens = scanTokens input
                 --putStr "tokens"
                 --print tokens
                 let ast = parseSimplyTyped tokens
                 let exprType = checkType ast []
                 print exprType
                 --print ast
                 let val = evalSimplyTyped ast
                 print val
                 evalLoop

flushBuff input = do
                putStr input 
                hFlush stdout

eval = evalSimplyTyped
main :: IO()
main = do
     evalLoop

