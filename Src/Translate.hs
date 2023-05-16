{-# LANGUAGE FlexibleContexts #-}
module Translate where

import Description
import Dimension
import Expression
import Matlab ( MatlabProgram(MatlabProgram), matlabStart )
import Python ( PythonProgram(PythonProgram), pythonStart )
import Parser
import Optimise
import Output
import TranslateContext
import Control.Monad.State
import Control.Monad.Except
import Utils

import Data.Foldable
import Data.List (intercalate)
import qualified Data.Map as M 

resolve :: IndexExpr -> [String] -> Translated IndexExpr
resolve (FieldAccess ns) sc = do
   fs <- gets fields
   let n =  map (++ ns) (inits sc)
   case asum [(k,) <$> k `M.lookup` fs | k <- n] of
     Nothing ->  throwError $ UnknownField ns
     Just (qname, F Nat) -> pure $ FieldAccess qname
     Just (qname, ty) -> throwError $ WrongType qname ty
  where
    inits [] = [[]]
    inits xs = xs : inits (init xs)
resolve ie _ = pure ie

check :: Description Name -> Translated (Description QualifiedName)
check = go [] where
  go :: [Name] -> Description Name -> Translated (Description QualifiedName)
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

  checkName :: [Name] -> Name -> Ty -> Translated QualifiedName
  checkName scope name ty = do
    fs <- gets fields
    let qname = scope ++ [name]
    case qname `M.lookup` fs of
      Just _ -> throwError $ ExistingField qname
      Nothing -> do
        writeField qname ty
        updateBreakdown scope name ty
        pure qname

  writeField qname ty =
    modify $ \st -> st{fields = M.insert qname ty $ fields st}
 
  updateBreakdown scope name O =
    let insertEmpty = M.insert (scope ++ [name]) emptyBreakdown
        action = case scope of
          [] -> id
          _  -> M.adjust (\b -> b{objects = name : objects b}) scope 
    in modify
       $ \st -> st{breakdowns = action $ insertEmpty $ breakdowns st}
  updateBreakdown scope name ty = 
    let action = case ty of
        { A -> M.adjust (\b -> b{arrays = name : arrays b}) scope
        ; _ -> M.adjust (\b -> b{rest = name : rest b}) scope}
    in modify $ \st -> st{breakdowns = action $ breakdowns st}
    
                    
adjustTerm :: Term -> FieldType -> Term
adjustTerm tm Nat = Cast tm Nat
adjustTerm tm ty@(Quantity q)
  | metricPrefix q == 0 = Cast tm ty
  | otherwise = scexp (metricPrefix q) <**> Cast tm ty

translateArray :: Term -> IndexExpr -> [FieldType] -> Translated (Maybe Term, [Expression])
translateArray _ _ [] = pure (Nothing , [])
translateArray t (Lit n) tys = do
  readPtr <- freshVar "readPtr"
  src     <- freshVar "src"
  start   <- startIndex
  let (separate, inLoop) = split False $ pad n tys
      len  = length separate
      sepStms = [Index t i := adjustTerm (Index src (readPtr <++> i)) ty
                | (ty,i) <- zip separate $ map IntLit [start..]]
  case inLoop of
    Nothing -> pure (Just (IntLit n) , sepStms)
    Just ty  -> do
      i <- freshVar "i"
      let from = IntLit $ len + start
          to   = IntLit n
          body j = [Index t j := adjustTerm (Index src (readPtr <++> j)) ty]
      pure (Just to , sepStms ++ [For i from to body])

translateArray t (FieldAccess ns) tys = do
  readPtr <- freshVar "readPtr"
  src     <- freshVar "src"
  start   <- startIndex
  let var = Var $ intercalate "." ns
      (separate, inLoop) = split False tys
      len  = length separate
      sepStms = [ifLt i var (Index t i := adjustTerm (Index src (readPtr <++> i)) ty)
                | (ty,i) <- zip separate $ map IntLit [start..]]
  case inLoop of
    Nothing -> pure (Just var , sepStms)
    Just ty  -> do
      i <- freshVar "i"
      let from   = IntLit $ len + start
          to     = var
          body j = [Index t j := adjustTerm (Index src (readPtr <++> j)) ty]
      pure (Just to , sepStms ++ [For i from to body])

translate' :: Description QualifiedName -> Translated [Expression]
translate' desc = do
  readPtr <- freshVar "readPtr"
  oe   <- getOffsetAssignment readPtr
  (off, exps) <- go readPtr desc
  writeOffset off
  pure $ oe ++ exps
  where
    go _ (Object _ fs) = do
      xs <- traverse translate' fs
      pure (Nothing, concat xs)
    go readPtr (Field ns ty) = do
      let var = Var $ intercalate "." ns
      src     <- freshVar "src"
      let tm = adjustTerm (Index src readPtr) ty
      pure (Just $ IntLit 1 , [var := tm])
    go _ (Array ns ind tys) =
      let var = Var $ intercalate "." ns
      in translateArray var ind tys
    go _ (ArrayLit ns tys) =
      let var = Var $ intercalate "." ns
          len = length tys
      in translateArray var (Lit len) tys

    writeOffset t = modify $ \st -> st{offset = t}

    getOffsetAssignment readPtr = do
      off' <- gets offset
      pure $ case off' of
        Nothing  -> []
        (Just t) -> [ReadOffset readPtr t]

mkDocument :: TargetLang -> Document
mkDocument Matlab = MkDocument $ MatlabProgram []
mkDocument Python = MkDocument $ PythonProgram []

putStartIndex :: TargetLang -> Translated ()
putStartIndex l =
  let index = case l of { Matlab -> matlabStart ; Python -> pythonStart}
  in modify $ \st -> st{startIdx = index}

translate :: TargetLang -> String -> Description Name -> Either TrError String
translate l f x = let d = mkDocument l
  in runExcept
  $ fmap render
  $ flip evalStateT TranslateContext.initState
  $ check x >>=
    \y -> putStartIndex l >> translate' y >>= addPreamble d f y . optimise

translateString :: TargetLang -> String -> String -> Either TrError String
translateString l f s = case parse pDesc s Parser.initState of
  Right d -> translate l f d
  Left e -> Left (ParseError e)
