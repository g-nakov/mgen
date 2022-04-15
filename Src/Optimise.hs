module Optimise where

import Matlab
import Debug.Trace
import Data.Function ((&))

type Program = [Expression]

optimise' :: [Program -> Program] -> Program -> Program
optimise' ops p = trace (show p) $ 
  let p' = foldl (&) p ops
  in p' -- if p == p' then p' else optimise ops p'

optimise = optimise' [fuseLoop, fuseOffset]

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
