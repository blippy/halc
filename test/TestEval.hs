module TestEval where

import Data.Map.Lazy as M

import Expr
import Eval
import Lexer -- for AlexPn

pos0 = AlexPn 0 0 0

e1 = evalBinop m0 "*" (Var (pos0, "foo")) (Num 10)


m0 = (M.fromList [("foo", 12)])
e2 = evalTerm m0  (Var (pos0, "foo"))

g4 = evalExprs m0 $ parz $ alex "x = 5 + 6 z = x + 7 * x"

