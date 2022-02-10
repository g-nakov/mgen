module Dimensions where

import Data.Map as M
import Data.Foldable (traverse_)

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
  | Kg    -- mass
  | Sec   -- time
  | Amp   -- el. current
  | Kel   -- temp.
  | Mol   -- substance
  | Cd    -- luminous int.

  -- mm / (s^2)
type DerivedUnit = [(Int, BaseUnit, Int)] -- (prefix, base unit, exponent)

 -- [(One, Meter, ) (Micro, Meter, -2)]
 -- m / (mm^2) ~> 1 / (mm^2) ~> 100 / mm ~> 10^4 / m ~>
normalise :: DerivedUnit -> DerivedUnit
normalise us =
  let v = traverse_
            (\(pr,u,exp) -> modify $ alter (Just . maybe (pr, exp) (\(pr',ex') -> (pr*pr', ex + ex') )  u) 
          us
  in _ 
 
    
data Dimension
  = Base 
  | Dimension :^: Int
  | Dimension :*: Dimension
  deriving (Show)

