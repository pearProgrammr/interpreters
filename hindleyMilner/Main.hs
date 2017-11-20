module Main where
import System.IO
import Eval
import Grammar
import Tokens
import Type
import TyInfer
import TyInfM

mainEnv :: Env
mainEnv = []


evalLoop env = do
         flushBuff "> "
         input <- getLine
         if input == ":q"
             then do
                 putStrLn "Exiting"
                 return ()
             else do
                 let tokens = scanTokens input
                 print tokens
                 let ast = parseSimplyTyped tokens
                 print ast
                 run (infer (TyEnv []) ast) [] [] 0
                 let val = eval ast env
                 print $ show val
                 evalLoop (snd val)

flushBuff input = do
                putStr input 
                hFlush stdout

main :: IO()
main = do
     evalLoop mainEnv

