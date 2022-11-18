module Matlab where

import Expression
import Description
import TranslateContext
import Output
import Data.List (intercalate)
import Utils

newtype MatlabProgram = MatlabProgram { getProgram :: Program }

matlabStart :: StartIndex
matlabStart = 1

preamble :: Description QualifiedName -> Translated Program
preamble (Object ns _) = do
  filename <- freshVar "fname"
  fileh    <- freshVar "f1"
  contents <- freshVar "c1"
  src      <- freshVar "src"
  readPtr  <- freshVar "readPtr"
  let retRes = "function " ++ intercalate  "." ns
  let body = [ Emb retRes := App (Emb "getinputsfromfile") [filename],
              fileh := App (Emb "fopen") [filename],
              contents := App (Emb "textscan") [fileh, Emb "`%f`"],
              src := CellIndex contents (IntLit  1),
              Statement $ App (Emb "fclose") [fileh],
              readPtr := IntLit 1]
  pure body
preamble _ = pure []

display :: Term -> String
display (IntLit n)
  | n >= 0  = show n
  | otherwise = concat ["(", show n, ")"]
display (BoolLit True) = "true"
display (BoolLit False) = "false"
display (Neg t1) = concat ["( -", display t1, ")"]
display (Var s) = s
display (Emb s) = s
display (Bin op t1 t2) = concat [display t1, " ", show op, " ", display t2]
display (Index t1 t2) = concat [display t1, "[", display t2, "]"]
display (CellIndex t1 t2) = concat [display t1, "{", display t2, "}"]
display (App f args) = concat [display f ,
                               "(", intercalate ", " (map display args), ")"]
display (Cast t _) = display t
                        
displayExp :: Int -> Expression -> String
displayExp n = (indent n ++) . \case
    (Statement t) -> display t ++ ";\n"
    (t1 := t2) -> concat [display t1, " = ", display t2, ";\n"]
    (For i from to body) ->
      let ctrl = concat [display i, " = ", display from, ":", display to]
          bd = body i
      in  concat $ ["for ", ctrl, "\n"]
                   ++ map (displayExp $ n + 4) bd
                   ++ [indent n, "end\n"]
    (If [] _) -> ""
    (If bds elseB) ->
      let ifs = [concat $ display c : "\n" : map (displayExp $ n + 4) bs |(c, bs) <- bds]
          ifchain = "if " ++ intercalate (indent n ++ "elseif ") ifs
          f x = concat [indent n, x, "\n"]
      in case elseB of
        [] -> ifchain ++ f "end"
        bs -> ifchain ++ f "else" ++ concatMap (displayExp $ n + 4) bs ++ f "end"
    (ReadOffset t1 t2) -> displayExp 0 (t1 := t1 <++> t2)
    
instance Output MatlabProgram where
  addPreamble _ desc pr  = MatlabProgram . (++ pr) <$> preamble desc
  render = concatMap (displayExp 0) . getProgram
