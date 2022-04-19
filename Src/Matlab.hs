module Matlab where

import Data.List (intercalate)

data BinaryOp 
  = LessThan | Plus | Mul | Exp
  deriving (Eq)

instance Show BinaryOp where
  show LessThan = " <= "
  show Plus     = " + "
  show Mul      = " * "
  show Exp      = " ^ "

data Term
  = IntLit Int
  | BoolLit Bool
  | Neg Term
  | Bin BinaryOp Term Term
  | Var String
  | Emb String
  | Index Term Term
  deriving (Show, Eq)

addT :: Term -> Term -> Term
t1 `addT` t2 = Bin Plus t1 t2

minusT :: Term -> Term -> Term
t1 `minusT` t2 = Bin Plus t1 (Neg t2)

mulT :: Term -> Term -> Term
t1 `mulT` t2 = Bin Mul t1 t2

expT :: Term -> Term -> Term
t1 `expT` t2 = Bin Exp t1 t2

scientficExp :: Int -> Term
scientficExp n = Emb $ "1e" ++ show n

data Expression
  = Term := Term
  | For Term Term Term (Term -> [Expression])
  | If [(Term, [Expression])] [Expression]
  | ReadOffset Term Term

instance Show Expression where
  show (t1 := t2) = show t1 ++ " := " ++ show t2 ++ "\n"
  show (For i f t b) = "For " ++ intercalate ", " (map show [i, f, t]) ++ "\n"
  show (If _ _) = "If _ _\n"
  show (ReadOffset t1 t2) = "ReadOffset " ++ show (t1 := (t1 `addT` t2))

instance Eq Expression where
  (t1 := t2) == (t1' := t2') = t1 == t1' && t2 == t2'
  (For t1 t2 t3 b1) ==   (For t1' t2' t3' b1') =
    t1 == t1' && t2 == t2' && t3 == t3' && (b1 (Var "__i") == b1' (Var "__i"))
  (If cs es) == (If cs' es') = cs == cs' && es == es'
  (ReadOffset t1 t2) == (ReadOffset t1' t2') = t1 == t1' && t2 == t2'
  _ == _ = False

ifLt :: Term -> Term -> Expression -> Expression
ifLt lht rht bd = If [(Bin LessThan lht rht, [bd])] []

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
  display (BoolLit True) = "true"
  display (BoolLit False) = "false"
  display (Neg t1)    = concat ["( -", display t1, ")"]
  display (Var s) = s
  display (Emb s) = s
  display (Bin op t1 t2) = concat [display t1, show op, display t2]
  display (Index t1 t2) = concat [display t1, "[", display t2, "]"]
  
instance Display Expression where
  displayIndent n = go where
    go :: Expression -> String
    go (t1 := t2) =  concat [displayIndent n t1, " = ", display t2, ";\n"]
    go (For i from to body) =
      let ctrl = concat [display i, " = ", display from, ":", display to]
          bd = body i
      in  concat $ [indent n, "for ", ctrl, "\n"] ++ map (displayIndent $ n + 4) bd ++ [indent n, "end\n"]
    go (If [] _) = ""
    go (If bds elseB) = 
      let ifs = [ concat $ display c : "\n" : map (displayIndent $ n + 4) bs |(c, bs) <- bds]
          ifchain = "if " ++ intercalate (indent n ++ "elseif ") ifs
          f x = concat [indent n, x, "\n"]
      in case elseB of
        [] -> ifchain ++ f "end"
        bs -> ifchain ++ f "else" ++ concatMap (displayIndent $ n + 4) bs ++ f "end"
    go (ReadOffset t1 t2) = go (t1 := (t1 `addT` t2))
