module Expr where

import Lexer

--data Expr = Assign (TkVar String) (TkVar Float) deriving (Eq, Show)

data Term = Num Float
          | Var (AlexPosn, String)
          | Binop String Term Term
          deriving (Eq, Show)

data Expr = Assign  String Term
          | ExpTerm Term
          deriving (Eq, Show)


--data Term = Float | String 


