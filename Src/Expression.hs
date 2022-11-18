module Expression where

import Description (FieldType)
import Data.List (intercalate)

data BinaryOp
  = LessThan | Plus | Mul | Exp
  deriving (Eq)

instance Show BinaryOp where
  show LessThan = "<="
  show Plus     = "+"
  show Mul      = "*"
  show Exp      = "^"

data Term where
  IntLit :: Int -> Term
  BoolLit :: Bool -> Term
  Neg :: Term -> Term
  Bin :: BinaryOp -> Term -> Term -> Term
  Var :: String -> Term
  Emb :: String -> Term
  Index :: Term -> Term -> Term
  CellIndex :: Term -> Term -> Term
  App :: Term -> [Term] -> Term
  Cast :: Term -> FieldType -> Term
  deriving (Show, Eq)

data Expression
  = Statement Term
  | Term := Term 
  | For Term Term Term (Term -> Program)
  | If [(Term, Program)] Program
  | ReadOffset Term Term

infix 1 :=

type Program = [ Expression ]

(<++>) :: Term -> Term -> Term
t1 <++> t2 = Bin Plus t1 t2

(<-->) :: Term -> Term -> Term
t1 <--> t2 = Bin Plus t1 (Neg t2)

(<**>) :: Term -> Term -> Term
t1 <**> t2 = Bin Mul t1 t2

(<^^>) :: Term -> Term -> Term
t1 <^^> t2 = Bin Exp t1 t2

scexp :: Int -> Term
scexp n = Emb $ "1e" ++ show n

instance Show Expression where
  show (Statement t) = show t ++ "\n"
  show (t1 := t2) = show t1 ++ " := " ++ show t2 ++ "\n"
  show (For i f t _) = "For " ++ intercalate ", " (map show [i, f, t]) ++ "\n"
  show (If _ _) = "If _ _\n"
  show (ReadOffset t1 t2) = "ReadOffset " ++ show (t1 := t1 <++> t2)

instance Eq Expression where
  (Statement t1) == (Statement t2) = t1 == t2
  (t1 := t2) == (t1' := t2') = t1 == t1' && t2 == t2'
  (For t1 t2 t3 b1) ==   (For t1' t2' t3' b1') =
    t1 == t1' && t2 == t2' && t3 == t3' && (b1 (Var "__i") == b1' (Var "__i"))
  (If cs es) == (If cs' es') = cs == cs' && es == es'
  (ReadOffset t1 t2) == (ReadOffset t1' t2') = t1 == t1' && t2 == t2'
  _ == _ = False

ifLt :: Term -> Term -> Expression -> Expression
ifLt lht rht bd = If [(Bin LessThan lht rht, [bd])] []
