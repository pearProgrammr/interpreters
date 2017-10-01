module Main where
import System.IO
import Eval
import Grammar
import Tokens


evalLoop = do
         flushBuff "> "
         input <- getLine
         if input == ":q"
             then do
                 putStrLn "Leaving Untyped"
                 return ()
             else do
                 flushBuff input
                 let tokens = scanTokens input
                 putStr "tokens"
                 print tokens
                 let ast = parseUntyped tokens
                 print ast
                 let val = evalUntyped ast
                 print val
                 evalLoop

flushBuff input = do
                putStr input 
                hFlush stdout

eval = evalUntyped
main :: IO()
main = do
     evalLoop

