{-# LANGUAGE  DeriveTraversable #-}
module Description where

import Dimension
import Parser
import Control.Applicative
import Control.Monad (void)
import Data.Foldable

data FieldType
  = Nat
  | Quantity DerivedUnitNF
  deriving (Show, Eq)

data IndexExpr
  = Lit Int
  | FieldAccess [String]
  deriving (Show, Eq)

data Description a
  = Object a [Description a]
  | Field a FieldType
  | Array a IndexExpr [FieldType]
  | ArrayLit a [FieldType]
  deriving (Show, Functor, Foldable, Traversable)

type Name = String
type QualifiedName = [Name]

pFieldType :: Parser FieldType
pFieldType = pkey $ Nat <$ pn <|> Quantity <$> pDerivedUnit
  where
    pn = asum $ plit <$> ["number", "Number", "Int", "int"]

pHeader :: Parser String
pHeader = pident <* pspace <* pch (== ':')

pField :: Parser (Description Name)
pField = Field <$> pHeader <*> (pcomment *> pFieldType <* pcomment)

pArrayLit :: Parser (Description Name)
pArrayLit = f <$> pHeader <*> (pcomment *> pFieldType <* pcomment) <*> pblock sItem
            <|> ArrayLit <$> (pHeader <* pcomment) <*> pblock tItem
  where
    f name ty items = ArrayLit name (ty <$ items)
    sItem = pch (== '-') *> pcomment
    tItem = pch (== '-') *> pcomment *> pFieldType <* pcomment

pArray :: Parser (Description Name)
pArray = Array <$> pHeader <*> (pcomment *> pindex <* pcomment)
                           <*> ptypes <* pcomment
  where ptypes = psep RequireOne (void (pspace *> pch (== ',') <* pspace)) pFieldType
        pindex = pkey $ Lit <$> pnumber
                      <|> FieldAccess <$> psep RequireOne (plit ".") pident

pObject :: Parser (Description Name)
pObject = Object <$> pHeader <* pcomment <*> pblock pDescription

pDescription :: Parser (Description Name)
pDescription =  pArray <|> pArrayLit <|> pField <|> pObject 

pDesc :: Parser (Description Name)
pDesc = ((many (pcomment' RequireOne *> peol) *> peol *> pObject) <|> pObject)
        <* optional (many (pcomment *> peol))
