module Optimise where

import Expression
import Data.Function ((&))
import Data.Bifunctor (bimap, second)

optimise :: Program -> Program
optimise = optimise' [fuseLoop, fuseOffset, constantfold, singleLoop]

optimise' :: [Program -> Program] -> Program -> Program
optimise' ops p =
  let p' = foldl (&) p ops
  in {-trace (show p') $-} if p == p' then p' else optimise' ops p'

constantfold :: Program -> Program
constantfold = fmap go where
  go (Statement t1) = Statement $ cfold t1
  go (t1 := t2) = t1 := cfold t2
  go (For t1 t2 t3 b1) = For (cfold t1) (cfold t2) (cfold t3) (constantfold . b1)
  go (If cs es) = If (map (bimap cfold constantfold) cs) (constantfold es)
  go (ReadOffset t1 t2) = ReadOffset (cfold t1) (cfold t2)

  cfold (IntLit i) = IntLit i
  cfold (BoolLit b) = BoolLit b
  cfold (Neg t) = case cfold t of
    IntLit i -> IntLit (-i)
    x -> Neg x
  cfold (Bin Plus t (IntLit 0)) = cfold t
  cfold (Bin Plus (IntLit 0) t) = cfold t
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
  cfold (CellIndex t1 t2) = CellIndex (cfold t1) (cfold t2)
  cfold (App f args) = App (cfold f) (map cfold args)
  cfold (Cast t ty) = Cast (cfold t) ty

singleLoop :: Program -> Program
singleLoop = concatMap $ \case
  For i f1 t1 b1 | f1 == t1  -> b1 f1
                 | otherwise -> [For i f1 t1 (singleLoop . b1)]
  If cs es -> [If (map (second singleLoop) cs) (singleLoop es)]
  x -> [x]

fuseLoop :: Program -> Program
fuseLoop [] = []
fuseLoop ((For i f1 t1 b1) : (ReadOffset c1 off1) : (For _ f2 t2 b2) : ps)
  | f1 == f2 && t1 == t2 =
    let bd j = b1 j ++ b2 j
        newLoop = [For i f1 t1 bd, ReadOffset c1 off1]
    in fuseLoop (newLoop ++ ps)
fuseLoop (p : ps) = p : fuseLoop ps

fuseOffset :: Program -> Program
fuseOffset [] = []
fuseOffset ((ReadOffset c1 o1) : (ReadOffset c2 o2) : ps)
  | c1 == c2 = fuseOffset (ReadOffset c1 (o1 <++> o2) : ps)
fuseOffset (p : ps) = p : fuseOffset ps

