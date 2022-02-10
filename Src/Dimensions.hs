module Dimensions where

import qualified Data.Map as M
import Control.Monad.State 
import Data.Foldable (traverse_)
import Data.List (sortOn, intercalate)

{-
length 	meter 	m
mass 	kilogram       	kg
time 	second 	s
electric current 	ampere 	A
thermodynamic temperature       	kelvin 	K
amount of substance 	mole 	mol
luminous intensity 
-}
-- data Quantity = Length | Mass | Time | Current | Temp | Subst | Lum  
data BaseUnit
  = Meter -- length
  | Gram  -- mass
  | Sec   -- time
  | Amp   -- el. current
  | Kel   -- temp.
  | Mol   -- substance
  | Cd    -- luminous int.
  | One   -- dimensionless units
  deriving (Ord,Eq)

instance Show BaseUnit where
  show Meter = "m"
  show Gram  = "g"
  show Sec   = "s"
  show Amp   = "A"
  show Kel   = "K"
  show Mol   = "mol"
  show Cd    = "cd"
  show One   = ""

  -- mm / (s^2)
type DerivedUnit = [(Int, BaseUnit, Int)] -- (10^prefix, base unit, exponent)

-- [(One, Meter, ) (Micro, Meter, -2)]
-- m / (mm^2) ~> 1 / (mm^2) ~> 100 / mm ~> 10^4 / m ~>
normalise :: DerivedUnit -> DerivedUnit
normalise us =
  let f = (\(pr,u,exp) -> modify $ M.alter (Just . maybe (pr, exp) (\(pr',exp') -> (pr + pr', exp + exp'))) u)
  in  sortOn (\( _, u, _) -> u) $ M.foldrWithKey (\u (pr,exp) -> ((pr, u, exp) :)) [] $ execState (traverse_ f us) M.empty   

printUnits :: DerivedUnit -> String
printUnits =  intercalate " " . map (\(pr, u, e) -> printPrefix pr ++  show u ++ "^" ++ show e) . normalise
  where
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

    
data Dimension
  = Base 
  | Dimension :^: Int
  | Dimension :*: Dimension
  deriving (Show)

(5,m,2) -> 
