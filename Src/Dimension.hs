module Dimension
  (DerivedUnitNF(DerivedUnitNF), pDerivedUnit, metricPrefix) where

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
  ,(Cd, "cd")]

prefixNames :: [(String, MetricPrefix)]
prefixNames =
  [("da", 1), ("h",  2), ("k",  3)
  ,("M",  6), ("G",  9), ("T", 12)
  ,("P", 15), ("E", 18), ("Z", 21)
  ,("Y", 24)
  ,("d",  -1), ("c",  -2), ("m",  -3)
  ,("μ",  -6), ("n",  -9), ("p", -12)
  ,("f", -15), ("a", -18), ("z", -21)
  ,("y", -24)]

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

pexponent :: Parser a -> Parser (a, Exponent)
pexponent p = (,) <$> p <* (pspace *> pch (=='^') <* pspace) <*> pn
  where pn = pnumber  <|> (pch (=='(') *> pspace *> pnumber <* pspace <* pch (== ')'))


pWithPrefix :: Parser BaseUnit -> Parser (MetricPrefix , BaseUnit)
pWithPrefix p = (,) <$> pmp <*> p
                <|> (0,) <$> p
  where
    pmp =  asum [plit pr Data.Functor.$> v | (pr,v) <- prefixNames]


pUnit :: Parser (MetricPrefix, BaseUnit, Exponent)
pUnit = f <$> pexponent (pWithPrefix pbu)
        <|> g <$> pWithPrefix pbu
        <|> (,One,1) <$> pn
  where
    f ((p,u),e) = (p, u, e)
    g (p, u)    = (p, u, 1)
    pbu = asum [plit n $> u | (u,n) <- baseUnitNames]
    pn = (snd <$> pexponent (plit "10")) <|> (1 <$ plit "10") <|> (0 <$ plit "1")

pDerivedUnit :: Parser DerivedUnitNF
pDerivedUnit = normalise <$> psep RequireOne s pUnit
  where s = () <$ (pspace *> pch (== '*') <* pspace) <|> pspace
