module TestLexer where

import Lexer

lexTest str = do
  let ts = alexScanTokens str
  print $ show ts
  
lt01 = lexTest "myvar = 12 + 13"

lt02 = lexTest "myvar = 12 + "

lt03 = lexTest "mayvar ^" 
