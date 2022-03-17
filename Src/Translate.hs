{-# LANGUAGE  FlexibleContexts #-}
module Translate where

import Matlab
import Description
import Dimension
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import Data.Map (Map)
import Control.Arrow ( (>>>) )
import qualified Data.Map as M

data Ty
  = O | A | F FieldType
  deriving (Show, Eq)

newtype CheckState = CheckState
  { fields :: Map [String] Ty
  }

data TrState = TrState
  { offset :: Maybe Term
  , locals :: [[String]]
  }

fromCheckState :: CheckState -> TrState
fromCheckState cs = TrState Nothing (M.keys $ fields cs)

type Checked a = StateT CheckState (Except String) a
type Translated a = State TrState a

resolve :: IndexExpr -> [String] -> Checked IndexExpr
resolve (FieldAccess ns) sc = do
   CheckState{fields = fs} <- get
   let n =  map (++ ns) (inits sc)
   case asum $ map (\k -> (k,) <$> k `M.lookup` fs) n of
     Nothing ->  throwError "Unknown Index field"
     Just (qname, F Nat) -> pure $ FieldAccess qname
     Just _ -> throwError "Wrong field type"
  where
    inits :: [a] -> [[a]]
    inits [] = [[]]
    inits xs = xs : inits (init xs)
resolve ie _ = pure ie

check :: Description -> Checked (DescriptionF [String])
check = go [] where
  go :: [String] -> Description -> Checked (DescriptionF [String])
  go sc (Object name fs) = do
    qname <- checkName sc name O
    mfields <- traverse (go $ sc ++ [name]) fs
    pure $ Object qname mfields
  go sc (Field name ty) = do
    qname <- checkName sc name $ F ty
    pure $ Field qname ty
  go sc (Array name ind tys) = do
    qname <- checkName sc name A
    qind  <- resolve ind sc
    pure $ Array qname qind tys
  go sc (ArrayLit name tys) = do
    qname <- checkName sc name A
    pure $ ArrayLit qname tys

  checkName :: [String] -> String -> Ty -> Checked [String]
  checkName sc name ty = do
    CheckState{fields = fs} <- get
    let qname = sc ++ [name]
    case qname `M.lookup` fs of
      Just _ -> throwError $ "Field already defined " ++ name
      Nothing -> do
        writeField qname ty
        pure qname

  writeField f t =  modify $
      \st -> st{fields = M.insert f t $ fields st}



freshVar :: String -> Translated Term
freshVar s = do
  TrState{locals = ls} <- get
  if [s] `elem` ls
    then freshVar (s++"0")
    else pure $ Var s

adjustTerm :: Term -> FieldType -> Term
adjustTerm tm Nat = tm
adjustTerm tm (Quantity q) =
  case metricPrefix q of
    0 -> tm
    n -> scientficExp n `mulT` tm

split :: Eq a => Bool -> [a] -> ([a], Maybe a)
split collapse = reverse >>> \case
   []         -> ([], Nothing)
   l@(x : xs) ->
    let (suffix, rprefix) = span (==x) xs
    in case length suffix of
        0 | collapse  -> (reverse l, Nothing)
          | otherwise -> (reverse rprefix, Just x)
        _ -> (reverse rprefix, Just x)

pad :: Int -> [a] -> [a]
pad _ [] = []
pad 1 (x : _) = [x]
pad n (x : xs) = x : pad (n - 1) xs

translateArray :: Term -> IndexExpr -> [FieldType] -> Translated (Maybe Term, [Expression])
translateArray _ _ [] = pure (Nothing , [])
translateArray t (Lit n) tys = do
  readPtr <- freshVar "readPtr"
  src     <- freshVar "src"
  let (separate, inLoop) = split False $ pad n tys
      len  = length separate
      sepStms = [Index t i := adjustTerm (Index src (readPtr `addT` i)) ty
                | (ty,i) <- zip separate $ map IntLit [1..]]
  case inLoop of
    Nothing -> pure (Just (IntLit n) , sepStms)
    Just ty  -> do
      i <- freshVar "i"
      let from = IntLit $ len + 1
          to   = IntLit n
          body = Index t i := adjustTerm (Index src (readPtr `addT` i)) ty
      pure (Just to , sepStms ++ [For i from to body])

translateArray t (FieldAccess ns) tys = do
  readPtr <- freshVar "readPtr"
  src     <- freshVar "src"
  let var = Var $ intercalate "." ns
      (separate, inLoop) = split False tys
      len  = length separate
      sepStms = [ifLt i var (Index t i := adjustTerm (Index src (readPtr `addT` i)) ty)
                | (ty,i) <- zip separate $ map IntLit [1..]]
  case inLoop of
    Nothing -> pure (Just var , sepStms)
    Just ty  -> do
      i <- freshVar "i"
      let from = IntLit $ len + 1
          to   = var
          body = Index t i := adjustTerm (Index src (readPtr `addT` i)) ty
      pure (Just to , sepStms ++ [For i from to body])

translate' :: DescriptionF [String] -> Translated [Expression]
translate' desc = do
  oe   <- getOffsetAssignment
  (off, exps) <- go desc
  writeOffset off
  pure $ oe ++ exps
  where
    go (Object _ fs) = do
      xs <- traverse translate' fs
      pure (Nothing, concat xs)
    go (Field ns ty) = do
      let var = Var $ intercalate "." ns
      readPtr <- freshVar "readPtr"
      src     <- freshVar "src"
      let tm = adjustTerm (Index src readPtr) ty
      pure (Just $ IntLit 1 , [var := tm])
    go (Array ns ind tys) =
      let var = Var $ intercalate "." ns
      in translateArray var ind tys
    go (ArrayLit ns tys) =
      let var = Var $ intercalate "." ns
          len = length tys
      in translateArray var (Lit len) tys

    writeOffset t = modify $ \st -> st{offset = t}

    getOffsetAssignment = do
      TrState{offset = of'} <- get
      readPtr <- freshVar "readPtr"
      pure $ case of' of
        Nothing  -> []
        (Just t) -> [readPtr := (readPtr `addT` t)]

translate :: Description -> Either String String
translate = fmap (concatMap display .
                 (\(d, st)-> evalState (translate' d) $ fromCheckState st))
            . runExcept . (`runStateT` CheckState M.empty)
            . check
