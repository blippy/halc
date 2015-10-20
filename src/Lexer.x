{
module Lexer where

}

%wrapper "basic"

$digit = 0-9
$white = [\ \t \r \n]
$alpha = [a-z]
$op = [\+\-\*\/]

tokens :-

  $white+     ;
  "#".*    ;
  $alpha+  { \s -> TkVar s }
  $digit+  { \s -> TkNum (read s::Float) }
  $op      { \s -> TkOp s }
  "="      { \s -> TkEq }
{

data Token =
     TkVar String
     | TkNum Float
     | TkOp String
     | TkEq
     deriving (Eq, Show)

lexTest = do
  let ts = alexScanTokens "myvar = 12 + 13"
  print $ show ts
}
