module Main where

import Eval
import Grammar
import Lexer

g4 = evalExprs m0 $ parz $ alex "x = 5 + 6 z = x + 7 * x"
evalStr vm0 str = evalExprs vm0 $ parz $ alex str

psl = putStrLn
repl vmap = do
  putStr "> "
  exp <- getLine
  if exp == "q" then psl "Bye" else do
    let (vmap', f) = evalStr vmap exp
    print f
    repl vmap'

  
main = do
  psl "Type q to quit"
  repl m0
  --repl
  --psl "Bye"
  
alex = alexScanTokens
rung str = parz $ alexScanTokens str
runLex str = print $ show $ alexScanTokens str

tlex1 = runLex "myvar = 12 + 13\nmyvar + 16"
--tgram = runGram "foo = 12"

g2 = rung "z = 12  y = 13 + 1 x = 5 + y"
g3 = rung "z = 12 + 13 + 14"


