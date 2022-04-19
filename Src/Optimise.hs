module Optimise where

import Matlab
import Debug.Trace
import Data.Function ((&))

type Program = [Expression]

optimise' :: [Program -> Program] -> Program -> Program
optimise' ops p =
  let p' = foldl (&) p ops
  in trace (show p') $ if p == p' then p' else optimise' ops p'

optimise = optimise' [fuseLoop, fuseOffset, constantfold]

{-
 i := 1 to 3
 i := 3 to 5
-}

{-
 i := 1 to 3
 ...
 offset = offset + 3;
 ...
 i := 1 to 3
-}

constantfold :: Program -> Program
constantfold = fmap go where
  go (t1 := t2) = t1 := cfold t2
  go (For t1 t2 t3 b1) = For (cfold t1) (cfold t2) (cfold t3) (\ i -> constantfold (b1 i))
  go (If cs bs) = If (map (\ (t, bs) -> (cfold t, constantfold bs)) cs) (constantfold bs)
  go (ReadOffset t1 t2) = ReadOffset (cfold t1) (cfold t2)

  cfold (IntLit i) = IntLit i
  cfold (Neg t) = case cfold t of
    IntLit i -> IntLit (-i)
    x -> Neg x
  cfold (Bin op t1 t2) = case (cfold t1, cfold t2) of
    (IntLit i1, IntLit i2) -> case op of
      Plus -> IntLit (i1 + i2)
      Mul -> IntLit (i1 * i2)
      Exp -> IntLit (i1 ^ i2)
      LessThan -> BoolLit (i1 < i2)
    (x, y) -> Bin op x y
  cfold (Index t1 t2) = Index (cfold t1) (cfold t2)
  cfold (Var x) = Var x
  cfold (Emb f) = Emb f

fuseLoop :: Program -> Program
--fuse ps = traverse
fuseLoop [] = []
fuseLoop ((For i f1 t1 b1) : (ReadOffset c1 off1) : (For _ f2 t2 b2) : ps)
  | f1 == f2 && t1 == t2 =
    let bd i = b1 i ++ b2 (i `addT` off1) 
        newLoop = [For i f1 t1 bd, ReadOffset c1 off1]
    in fuseLoop (newLoop ++ ps)
fuseLoop (p : ps) = p : fuseLoop ps

fuseOffset :: Program -> Program
fuseOffset [] = []
fuseOffset ((ReadOffset c1 o1) : (ReadOffset c2 o2) : ps)
  | c1 == c2 = fuseOffset (ReadOffset c1 (o1 `addT` o2) : ps)
fuseOffset (p : ps) = p : fuseOffset ps
