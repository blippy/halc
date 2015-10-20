module Scratch where

import Data.Map.Lazy as M

import Expr

type Varmap = M.Map String Float
type Partial = (Varmap, Float)
type Output = Either String Partial

--s1 = ((\x y -> (fst x, (snd x) * y)) <$> (Right (M.empty, 2)))
--s2 = s1 <$> (Right (M.empty, 3))
r1:: Output
r1 = Right (M.empty, 3)
r2 = Right (M.empty, 4)

mult2 :: (Varmap, Float) -> Output
mult2 (v, f) = Right (v, f*2)

--s1 :: (Varmap, Float) -> Output
s1 :: Output
s1 = r1 >>= mult2

s2 :: (Float -> Float -> Float) -> (Varmap, Float) -> (Varmap, Float) -> Output
s2 op (v1, f1) (v2, f2) = Right (v1, f1 `op` f2)

--s3 :: Output
s3 = (s2 (/)) <$> r1 <*> r2

--s4 = s3 <*> r1

-- s4 = Right 

--s1 = (Right (M.empty, 2)) >>= (*)
