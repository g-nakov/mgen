module Matlab where

import Expression
import Data.List (intercalate)
import Control.Monad.State

class Display t where
  display :: t -> String
  displayIndent :: Int -> t -> String
  
  display = displayIndent 0
  displayIndent n t = replicate n ' ' ++ display t
    
data Term 
  = IntLit Int
  | FloatLit Double
  | BoolLit Bool
  | StringLit String
  | Add Term Term
  | Mul Term Term
  | Neg Term
  | Var String
  | Prim String
  | App (Term) [Term]
  | Index Term Term
  | CellIndex Term Term
  deriving (Show)

data Expression
  = NOP
  | Expression :- Expression
  | Term := Term
  | For Term Term Term Expression
  deriving (Show)

instance Display Term where
  display (IntLit n) = show n
  display (FloatLit f) = show f
  display (BoolLit b) = show b
  display (StringLit s) = s 
  display (Add t1 t2) = "(" ++ display t1 ++ " + " ++ display t2 ++ ")"
  display (Mul t1 t2) = display t1 ++ " * " ++ display t2
  display (Neg t1) = "-" ++ display t1 
  display (Var s) = s
  display (Prim s) = s
  display (App t1 args) = display t1 ++ "(" ++ intercalate "," (map display args) ++ ")"
  display (Index t1 t2) = display t1 ++ "[" ++ display t2 ++ "]"
  display (CellIndex t1 t2) = display t1 ++ "{" ++ display t2 ++ "}"
  
instance Display Expression where  
  displayIndent n = \case
    NOP        -> ""
    (e1 :- e2) -> displayIndent n e1 ++ displayIndent n e2
    (t1 := t2) -> displayIndent n t1 ++ " = " ++ display t2 ++ ";\n"
    (For i from to body) -> let ctrl = display i ++ " = " ++ display from ++ ":" ++ display to 
                            in  indent ++ "for "  ++ ctrl ++ "\n" ++
                                displayIndent (n + 4) body ++ 
                                indent ++ "end\n"
    where
      indent = replicate n ' '

exp1 = (Var "x") := (Index (Var "y") $ (Add (Var "x") (IntLit 5)))
f = For (Var "i") (IntLit 1) (IntLit 5)

data Env = Env 
  { preamble :: Maybe Description
  , postamble :: Maybe Description
  , readPtr :: Term
  , source  :: Term
  , offset  :: Maybe Term
  }

  
initEnv :: Env 
initEnv = undefined 


translate :: Description -> State Env Expression
translate = tr . qualifyNames
  where
    tr :: QualifiedDescription -> State Env Expression
    tr (Object _ fields) = do
      exps <- mapM tr fields
      pure $ foldl (:-) NOP exps
    tr (Field name ty) = do
      undefined
    tr _ = undefined

{-translateIn (Struct name fields) = do
  f <- (mapM translateIn $ go name fields) 
  pure $ Seq f
 where
    go :: Name -> [Expression] -> [Expression]
    go n fs = map (((n ++ ".") ++) `fmap`)  fs
translateIn (Field name ty) = do
  account for ty to generate 10^x * 
  TrEnv{idx = idx, contents = c} <- get
  let asn = assignWithIdx (Var name) c idx 
  let inc = Assignment idx (Add idx $ IntT 1)  
  pure $ Seq [asn, inc]

translateIn (Vector name varE ty) = do
  Env'{idx = idx, contents = c} <- get
  let cmd = forAssign "i" (IntT 1) (Var $ getExprName varE) (Var name) c idx
  pure cmd
 
translateIn (VectorLit name tys)  =  do
  Env'{idx = idx, contents = c} <- get
  let cmd = forAssign "i" (IntT 1) (IntT $ length tys) (Var name) c idx
  pure cmd
-}
