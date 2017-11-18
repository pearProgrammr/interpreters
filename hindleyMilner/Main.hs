module Main where
import System.IO
import Eval
import Grammar
import Tokens
import Type
import TyInfer
import TyInfM

mainEnv :: Eval.Env
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
                 let ast = parseSimplyTyped tokens
                 run (infer (Type.Env []) ast) [] 0
                 let val = eval ast env
                 print $ show val
                 evalLoop (snd val)

flushBuff input = do
                putStr input 
                hFlush stdout

main :: IO()
main = do
     evalLoop mainEnv

