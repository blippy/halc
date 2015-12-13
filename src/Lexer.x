{
module Lexer where

}

%wrapper "posn"

$digit = 0-9
$white = [\ \t \r \n]
$alpha = [a-z]
$op = [\+\-\*\/]

tokens :-

  $white+     ;
  "#".*    ;
  $alpha+  { \pos s -> TkVar (pos, s) }
  $digit+  { \pos s -> TkNum (read s::Float) }
  $op      { \pos s -> TkOp s }
  "="      { \pos s -> TkEq }
  .        { \pos s  -> TkUnknown (pos, s) }
{

data Token =
     TkVar (AlexPosn, String)
     | TkNum Float
     | TkOp String
     | TkEq
     | TkUnknown (AlexPosn, String)
     deriving (Eq, Show)


}
