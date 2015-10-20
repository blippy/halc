module Eval where

import Data.Map.Lazy as M

import Expr

type Varmap = M.Map String Float
type Partial = (Varmap, Float)
type Output = Either String Partial
-- eval :: [Expr.Expr]
evalTerm ::  Varmap -> Term -> Float
evalTerm vmap term = do
  -- let et x = evalTerm varMap x
  case term of
   Num f -> f
   Var s -> case M.lookup s vmap of
             Just v -> v
             Nothing -> error $ "No variable: " ++ s
   Binop op t1 t2 -> evalBinop vmap op t1 t2


m0 = (M.fromList [("foo", 12)])
e1 = evalTerm m0  (Var "foo")


evalBinop:: Varmap -> String -> Term -> Term -> Float
evalBinop vmap op t1 t2 =
  case op of
   "*" -> v (*) 
   "/" -> v (/)
   "+" -> v (+)
   "-" -> v (-)
  where
    et x = evalTerm vmap x
    v op = (et t1) `op` (et t2)

--  return $ Right (Num 666.0)




e2 = evalBinop m0 "*" (Var "foo") (Num 10)

evalExpr :: Varmap -> Expr -> (Varmap, Float)
evalExpr vmap e =
  case e of
   Assign vname term -> assign vname term
   ExpTerm t -> (vmap, evalTerm vmap t)
   where
     assign vname term =
       (M.insert vname val vmap, val)
       where val = evalTerm vmap term
     

evalExprs':: (Varmap, Float) -> [Expr] -> (Varmap, Float)
evalExprs' (v,f)  (e:es) =
  if Prelude.null es then (v',f') else evalExprs' (v', f') es
  where
    (v', f') = evalExpr v e

evalExprs :: Varmap -> [Expr] -> (Varmap, Float)
evalExprs vmap0 es =
  if Prelude.null es then (vmap0, 0.0) else evalExprs' (vmap0, 0.0) es

--  M.foldl evalExpr m0 es

{-

evalBinop :: Varmap -> Term -> Either String Float
evalBinop vmap bop =
  res
  where
    et x = evalTerm vmap x
    Binop op t1 t2 = bop
    res = case op of
      "*" -> (\x y -> (vmap, x*y)) <$> (et t1) <*> (et t2)
      --"/" -> (/) <$> (et t1) <*> (et t2)
      --"+" -> (+) <$> (et t1) <*> (et t2)
      --"-" -> (-) <$> (et t1) <*> (et t2)
-}


{-
evalBinop vmap bop = do
  let et x = evalTerm vmap x
  let Binop op t1 t2 = bop
  res <- (et t1) * (et t2)
  (et t1) >>= 
  return res
-}

--e2 = evalTerm (M.fromList [("foo", 12)]) $ Binop "+" (Var "foo") (Num 11)

{-
evalExpr :: Varmap -> Expr -> Either String (Varmap, Float)
evalExpr vmap expr = do
  (vmap', res) <- case expr of
                   ExpTerm x -> (vmap, evalTerm vmap x)
                   _ -> Left "Unprogrammed expression in evalExpr"
  return (vmap', res)
-}  
