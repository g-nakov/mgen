-- {-# LANGUAGE FlexibleInstances, TypeSynonymInstances, TypeApplications #-}
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

normalise :: DerivedUnit -> DerivedUnit
normalise us =
  let f  = \(pr,u,exp) -> modify $ M.alter (Just . maybe (pr, exp) (\(pr',exp') -> (pr + pr', exp + exp'))) u
      mus = execState (traverse_ f us) M.empty
      h = \m -> M.insert One ((M.foldrWithKey (\_ (pr, exp) -> (pr * exp +)) 0 m), 1) m
      g = M.foldrWithKey (\u (pr,exp) -> ((if u /= One then 1 else pr, u, exp) :)) []
  in  sortOn (\( _, u, _) -> u) . g . h $ mus

printUnits :: DerivedUnit -> String
printUnits =  intercalate " " . map print'. normalise
  where
    print' (pr, One, e) = show "10^" ++ show (pr^e)
    print' (1 , u,   e) = show u ++ "^" ++ show e
    print' (pr, u,   e) = printPrefix pr ++  show u ++ "^" ++ show e
   
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

