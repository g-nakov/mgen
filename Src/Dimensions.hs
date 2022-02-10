{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Dimensions where

import qualified Data.Map as M
import Control.Monad.State 
import Data.Foldable (traverse_)
import Data.List (sortOn, intercalate)

data BaseUnit
  = One   -- dimensionless unit
  | Meter -- length
  | Gram  -- mass
  | Sec   -- time
  | Amp   -- electric current
  | Kel   -- temp.
  | Mol   -- substance
  | Cd    -- luminous intensity

  deriving (Ord,Eq)

instance Show BaseUnit where
  show One   = ""
  show Meter = "m"
  show Gram  = "g"
  show Sec   = "s"
  show Amp   = "A"
  show Kel   = "K"
  show Mol   = "mol"
  show Cd    = "cd"
  
-- (10^prefix, base unit, exponent)
type DerivedUnit = [(Int, BaseUnit, Int)]

newtype DerivedUnitNF = DUNF (Int, [(BaseUnit, Int)]) deriving (Eq, Show)

normalise :: DerivedUnit -> DerivedUnitNF
normalise us =
  let initS = (0, M.empty)
      f  = \(pr,u,exp) -> modify (\(io,m) -> (pr * exp + io,
                                       M.alter (Just . maybe exp (\exp' -> (exp + exp'))) u m))
      (io, mus) = execState (traverse_ f us) initS
      g = M.foldrWithKey (\u exp l -> if (exp == 0 || u == One) then l else (u, exp) : l) []
  in  DUNF (io , sortOn fst . g $ mus)
 
printUnits :: DerivedUnit -> String
printUnits u =  let (DUNF (i, us)) = normalise u
                    i'  =  if i == 0 then [] else ["10^" ++ show i]  
                in intercalate " " $ i' ++ map print' us
  where
    print' (u, e) = show u ++ "^" ++ show e 
    printPrefix 0 = ""
    printPrefix 1 = "da"
    printPrefix 2 = "h"
    printPrefix 3 = "k"
    printPrefix 6 = "M"
    printPrefix 9 = "G"
    printPrefix 12 = "T"
    printPrefix 15 = "P"
    printPrefix 18 = "E"
    printPrefix 21 = "Z"
    printPrefix 24 = "Y"
    printPrefix (-1) = "d"
    printPrefix (-2) = "c"
    printPrefix (-3) = "m"
    printPrefix (-6) = "μ"
    printPrefix (-9) = "n"
    printPrefix (-12) = "p"
    printPrefix (-15) = "f"
    printPrefix (-18) = "a"
    printPrefix (-21) = "z"
    printPrefix (-24) = "y"
    printPrefix s     = "10^" ++ show s ++ " "

