module Matlab where

import Data.List (intercalate)

data Op 
  = LessThan | Plus | Mul | Exp
  deriving (Eq)

instance Show Op where
  show LessThan = " <= "
  show Plus     = " + "
  show Mul      = " * "
  show Exp      = " ^ "

addT :: Term -> Term -> Term
t1 `addT` t2 = Infix Plus t1 t2

minusT :: Term -> Term -> Term
t1 `minusT` t2 = Infix Plus t1 (Neg t2)

mulT :: Term -> Term -> Term
t1 `mulT` t2 = Infix Mul t1 t2

expT :: Term -> Term -> Term
t1 `expT` t2 = Infix Exp t1 t2

scientficExp :: Int -> Term
scientficExp n =  Prim $ "1e" ++ show n

data Term
  = IntLit Int
  | FloatLit Double
  | Neg Term
  | BoolLit Bool
  | StringLit String
  | Var String
  | App Term [Term]
  | Infix Op Term Term
  | Prim String
  | Index Term Term
  | CellIndex Term Term
  deriving (Show)

data Expression
  = Term := Term
  | For Term Term Term Expression
  | If Term Expression
  deriving (Show)

ifLt :: Term -> Term -> Expression -> Expression
ifLt lht rht = If (Infix LessThan lht rht)

indent :: Int -> String
indent n = replicate n ' '

class Display t where
  display :: t -> String
  displayIndent :: Int -> t -> String
    
  display = displayIndent 0
  displayIndent n t = indent n ++ display t
  
  
instance Display Term where
  display (IntLit n) 
    | n > 0  = show n
    | otherwise = concat ["(", show n, ")"]
  display (FloatLit f) = show f
  display (Neg t1)    = concat ["(", "-", display t1, ")"]
  display (BoolLit b) = show b
  display (StringLit s) = show s
  display (Var s) = s
  display (Prim s) = s
  display (Infix op t1 t2) = concat [display t1, show op, display t2]
  display (App t1 args) = concat [display t1, "(", intercalate "," (map display args), ")"]
  display (Index t1 t2) = concat [display t1, "[", display t2, "]"]
  display (CellIndex t1 t2) = concat [display t1, "{", display t2, "}"]
  

instance Display Expression where
   displayIndent n = \case    
    (t1 := t2) -> displayIndent n t1 ++ " = " ++ display t2 ++ ";\n"
    (For i from to body) -> 
      let ctrl = display i ++ " = " ++ display from ++ ":" ++ display to 
      in  indent n ++ "for "  ++ ctrl ++ "\n" ++
          displayIndent (n + 4) body ++ 
          indent n ++ "end\n"
    (If c body) -> indent n ++ concat ["if ",  display c , "\n", displayIndent (n + 4) body, "end\n"]
