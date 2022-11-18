{-# LANGUAGE InstanceSigs #-}
module Python where

import Expression
import Description
import Output
import TranslateContext
import qualified Matlab
import Utils
import Data.Functor
import Data.List ( intercalate )

data PythonExp = Common Expression
               | WithOpen Term Term [PythonExp]
               | ReadLines Term Term
               -- classname allfields arrays objects
               | Class Name [Name] [Name] [Name]
  deriving (Show)

newtype PythonProgram = PythonProgram { getProgram :: [PythonExp] }

pythonStart :: StartIndex
pythonStart = 0

preamble :: Description QualifiedName -> Translated [PythonExp]
preamble (Object _ _) = do
  filename <- freshVar "fname"
  fileh    <- freshVar "f1"
  src      <- freshVar "src"
  readPtr  <- freshVar "readPtr"
  let body = [WithOpen filename fileh
               [ ReadLines src fileh
               , Common $ readPtr := IntLit 0]] 
  pure body
preamble _ = pure []


makeStructs :: Description QualifiedName -> Translated [PythonExp]
makeStructs o@(Object name _) =
  let clsVar = Var $ concat name
      clsName = Emb $ concatMap toTitle name ++ "()"
  in go o <&> (++ [Common (clsVar := clsName)])
  where
    go (Object name fields) = do
      let clsname = concatMap toTitle name
      breakdown <- breakdownForObject name
      case breakdown of
       Nothing -> pure []
       Just b  -> (concat <$> traverse go fields) <&> (++ mkClass clsname b)
    go _ = pure []
    
    mkClass cn b =
      let allFields = objects b ++ arrays b ++ rest b
      in [Class cn allFields (arrays b) (objects b)]             
makeStructs _ = pure []

reverseClassName :: Name -> Name -> Name
reverseClassName cls field = cls ++ toTitle field

display :: Term -> String
display (Neg t1) = concat ["( -", display t1, ")"]
display (Bin Exp t1 t2) = concat [display t1, " ** ", display t2]
display (Bin op t1 t2) = concat [display t1, " ", show op, " ", display t2]
display (BoolLit x) = show x
display (Cast t Nat) = concat ["int(", display t, ")"]
display (Cast t (Quantity _)) = concat ["float(", display t, ")"]
display (Index t1 t2) = concat [display t1, "[", display t2, "]"]
display (CellIndex t1 t2) = concat [display t1, "{", display t2, "}"]
display (App f args) = concat [display f ,
                               "(", intercalate ", " (map display args), ")"]
display x = Matlab.display x

displayExp :: Int -> PythonExp -> String
displayExp n = unlines . go n
  where
    go n = map (indent n ++) . \case
      (WithOpen fname fhandle bd) ->
         concat ["with open", "(", display fname, ") as ", display fhandle, ":"] 
         : concatMap (go $ n + 2) bd
      (ReadLines into fileh) -> go 0 $ Common $ into := Emb (display fileh ++ ".readlines()")
      (Common (Statement t)) -> [display t]
      (Common (t1 := t2))    -> [concat [display t1, " = ", display t2]]
      (Common (For i from to body)) ->
        let ctrl = [concat ["for ", display i, " in range(", display from, ", ", display to, "):"]]
            bd = body i
        in  ctrl ++ indentBody n bd
      (Common (If bds elseB)) ->
        let ifchain = makeIfs "if " n bds
        in case elseB of
          [] -> ifchain
          bd -> concat [ifchain, ["else:"], indentBody n bd]
      (Common (ReadOffset t1 t2)) -> go 0 $ Common $ t1 := t1 <++> t2
      (Class name slots arrays objects) ->
        let printConstructor x = if arrays /= [] || objects /= [] then x else []
        in concat [
          concat ["class ", name,"():"]
          : []
          : concat [indent $ n + 2
                   , "__slots__ = ("
                   , intercalate ", " $ map (\x -> "\"" ++ x ++"\"") slots
                   , ")"]
          :  printConstructor (
              [] :
              (indent (n + 2) ++ "def __init__(self):")
              : map (\o -> concat [indent (n + 4), "self.", o, " = ", reverseClassName name o, "()"]) objects
              ++ map (\a -> concat [indent (n + 4), "self.", a, " = {}"]) arrays)
          , [[]]
          , [[]]]
        
    indentBody n = concatMap (go (n + 2) . Common)
   
    makeIfs _ _ [] = []
    makeIfs key n ((c, bd) : xs) = concat [key, " ", display c, ":"] : indentBody n bd
                                 ++ makeIfs "elif " n xs
 
instance Output PythonProgram where
  addPreamble _ desc pr = do
    cls <- makeStructs desc
    p <- preamble desc
    let p' = case p of
          [WithOpen t1 t2 bd] -> [WithOpen t1 t2 $ bd ++ map Common pr]
          _ -> []
    pure $ PythonProgram $ cls ++ p'
    
  render = concatMap (displayExp 0). getProgram
