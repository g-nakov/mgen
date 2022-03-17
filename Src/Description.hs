{-# LANGUAGE  DeriveTraversable #-}
module Description where

import Dimension
import Parser
import Control.Applicative

data FieldType
  = Nat
  | Quantity DerivedUnitNF
  deriving (Show, Eq)

data IndexExpr
  = Lit Int
  | FieldAccess [String]
  deriving (Show, Eq)

data DescriptionF a
  = Object a [DescriptionF a]
  | Field a FieldType
  | Array a IndexExpr [FieldType]
  | ArrayLit a [FieldType]
  deriving (Show, Functor, Foldable, Traversable)

type Description = DescriptionF String
type QualifiedDescription = DescriptionF [String]

pFieldType :: Parser FieldType
pFieldType = pkey $  Nat <$ ln
                 <|> Quantity <$> pDerivedUnit
  where
    ln = plit "number" <|> plit "Number" <|> plit "Int" <|> plit "int"

pHeader :: Parser String
pHeader = pident <* pspace <* pch (== ':')

pField :: Parser Description
pField = Field <$> pHeader <*> (pcomment *> pFieldType <* pcomment)

pArrayLit :: Parser Description
pArrayLit = f <$> pHeader <*> (pcomment *> pFieldType <* pcomment) <*> pblock sItem
            <|> ArrayLit <$> (pHeader <* pcomment) <*> pblock tItem
  where
    f name ty items = ArrayLit name (ty <$ items)
    sItem = pch (== '-') *> pcomment
    tItem = pch (== '-') *> pcomment *> pFieldType <* pcomment

pArray :: Parser Description
pArray = Array <$> pHeader <*> (pcomment *> pindex <* pcomment)
                           <*> ptypes <* pcomment
  where ptypes = psep RequireOne (() <$ (pspace *> pch (== ',') <* pspace)) pFieldType
        pindex = pkey $   Lit <$> pnumber
                      <|> FieldAccess <$> psep RequireOne (plit ".") pident

pObject :: Parser Description
pObject = Object <$> pHeader <* pcomment <*> pblock pDescription

pDescription :: Parser Description
pDescription =  pArray <|> pArrayLit <|> pField <|> pObject

pDesc :: Parser Description
pDesc = ((many (pcomment' RequireOne *> peol) *> peol *> pObject) <|> pObject)
        <* optional (many (pcomment *> peol))
