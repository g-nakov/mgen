module Dimension
  (DerivedUnitNF(DerivedUnitNF), pDerivedUnit, metricPrefix, printUnit) where

import Parser

import qualified Data.Map as M
import Control.Applicative
import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Bifunctor
import Data.Functor
import Data.Maybe

data BaseUnit
  = One
  | Meter -- length
  | Gram  -- mass
  | Sec   -- time
  | Amp   -- electric current
  | Kel   -- temp.
  | Mol   -- substance
  | Cd    -- luminous intensity
  deriving (Ord,Eq)

type Exponent = Int
type MetricPrefix = Int

baseUnitNames :: [(BaseUnit, String)]
baseUnitNames =
  [(Meter, "m")
  ,(Gram, "g")
  ,(Sec, "s")
  ,(Amp, "A")
  ,(Kel, "K")
  ,(Mol, "mol")
  ,(Cd, "cd")
  ]

unitNames :: [(DerivedUnit, String)]
unitNames = map (\ (b, n) -> ([(0, b, 1)], n)) baseUnitNames
            ++
            [ ([(0, One, 1)], "rad")
            , ([(0, One, 1)], "sr")
            , ([(0, Sec, -1)], "Hz")
            , ([(3, Gram, 1), (0, Meter, 1), (0, Sec, -2)], "N")
            , ([(3, Gram, 1), (0, Meter, -1), (0, Sec, -2)], "Pa")
            , ([(3, Gram, 1), (0, Meter, 2), (0, Sec, -2)], "J")
            , ([(3, Gram, 1), (0, Meter, 2), (0, Sec, -3)], "W")
            , ([(0, Sec, 1), (0, Amp, 1)], "C")
            , ([(3, Gram, 1), (0, Meter, 2), (0, Sec, -3), (0, Amp, -1)], "V")
            , ([(3, Gram, -1), (0, Meter, -2), (0, Sec, 4), (0, Amp, 2)], "F")
            , ([(3, Gram, 1), (0, Meter, 2), (0, Sec, -3), (0, Amp, -2)], "Ω")
            , ([(3, Gram, -1), (0, Meter, -2), (0, Sec, 3), (0, Amp, 2)], "S")
            , ([(3, Gram, 1), (0, Meter, 2), (0, Sec, -2), (0, Amp, -1)], "Wb")
            , ([(3, Gram, 1), (0, Sec, -2), (0, Amp, -1)], "T")
            , ([(3, Gram, 1), (0, Meter, 2), (0, Sec, -2), (0, Amp, -2)], "H")
            , ([(0, Cd, 1), (0, One, 1)], "lm")
            , ([(0, Cd, 1), (0, One, 1), (0, Meter, -2)], "lx")
            , ([(0, Sec, -1)], "Bq")
            , ([(0, Meter, 2), (0, Sec, -2)], "Gy")
            , ([(0, Meter, 2), (0, Sec, -2)], "Sv")
            , ([(0, Mol, 1), (0, Sec, -1)], "kat")
            ]

prefixNames :: [(String, MetricPrefix)]
prefixNames =
  [("da", 1), ("h",  2), ("k",  3)
  ,("M",  6), ("G",  9), ("T", 12)
  ,("P", 15), ("E", 18), ("Z", 21)
  ,("Y", 24)
  ,("d",  -1), ("c",  -2), ("m",  -3)
  ,("μ",  -6), ("n",  -9), ("p", -12)
  ,("f", -15), ("a", -18), ("z", -21)
  ,("y", -24)
  ]

instance Show BaseUnit where
  show n = fromMaybe "impossible" $ lookup n baseUnitNames

-- (10^prefix, base unit, exponent)
type DerivedUnit = [(MetricPrefix, BaseUnit, Exponent)]

newtype DerivedUnitNF = DerivedUnitNF (Int, [(BaseUnit, Exponent)])
  deriving (Eq, Ord)

instance Show DerivedUnitNF where
  show (DerivedUnitNF (i, us)) = unwords $ prefix i ++ map print' us
    where
      prefix 0 = []
      prefix u = ["10^"++ show u]
      print' (u,1) = show u
      print' (u,e) = show u ++ "^" ++ show e

normalise :: DerivedUnit -> DerivedUnitNF
normalise us =
  let initS' = (0, M.empty)
      f = \(pr, u, e) -> modify $
            bimap ((pr * e) +) (M.alter (Just . maybe e (e +)) u)
      (ones, mus) = execState (traverse_ f us) initS'
      keep (u, e) = u /= One && e /= 0
      g = filter keep . M.foldrWithKey (curry (:)) []
  in  DerivedUnitNF (ones , sortOn fst . g $ mus)

metricPrefix :: DerivedUnitNF -> MetricPrefix
metricPrefix (DerivedUnitNF (m , _)) = m

printUnit :: DerivedUnit -> String
printUnit =  show . normalise

pprefix :: Parser MetricPrefix
pprefix = asum [plit pr Data.Functor.$> v | (pr,v) <- prefixNames]

punitname :: Parser DerivedUnit
punitname = asum [plit n $> u | (u,n) <- unitNames]

pexp :: Parser Exponent
pexp = pch (=='^') *> pspace *> (pnumber  <|> (pch (=='(') *> pspace *> pnumber <* pspace <* pch (== ')')))

pderivedunit :: Parser DerivedUnit
pderivedunit = mkUnit <$>
               ((,) <$> pprefix <*> punitname
           <|> (0,) <$> punitname)
           <*> optional pexp
  where
    mkUnit (pre, unit) exp = let
      exp' = maybe 1 id exp
      in map (\ (p, u, e) -> (pre + p, u, exp' * e)) unit

pDerivedUnit :: Parser DerivedUnitNF
pDerivedUnit = normalise . fold <$> psep RequireOne s pderivedunit
  where s = () <$ (pspace *> pch (== '*') <* pspace) <|> pspace
