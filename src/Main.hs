module Main where

import Control.Exception
import Data.Map.Lazy as M

import Exceptions
import Eval
import Grammar
import Lexer

evalStr :: Varmap -> String -> (Varmap, Float)
evalStr vm0 str = evalExprs vm0 $ parz $ alex str


handler :: HalcException -> IO ()
handler e =  putStrLn $ "Exception caught: " ++ show e

psl = putStrLn
repl :: Varmap -> IO ()
repl vmap = do
  putStr "> "
  exp <- getLine
  if exp == "q" then psl "Bye" else do
    let (vmap', f) = evalStr vmap exp
    --print f
    handle handler  (print f)
    repl vmap'

  
main = do
  psl "Type q to quit"
  repl  M.empty --([]::Varmap)

  
alex = alexScanTokens
rung str = parz $ alexScanTokens str
runLex str = print $ show $ alexScanTokens str

tlex1 = runLex "myvar = 12 + 13\nmyvar + 16"
--tgram = runGram "foo = 12"

g2 = rung "z = 12  y = 13 + 1 x = 5 + y"
g3 = rung "z = 12 + 13 + 14"


