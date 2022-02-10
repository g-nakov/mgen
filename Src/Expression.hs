module Expression where 

type Desc = String

data Dimension
  = Base Desc String
  | Dimension :^: Int
  | Dimension :*: Dimension
  deriving (Show)

data DType 
  = Nat
  | Quantity Dimension
  deriving (Show)

data IndexExpr
  = Lit Int
  | FieldAccess [String]
  deriving (Show) 

data DescriptionF a
  = Object a [DescriptionF a]
  | Field a DType
  | Array a IndexExpr DType -- Array "lengths" (Field "items" Nat) (Quantity "length" "M")
  | ArrayLit a [DType]
  -- what about tensors ? | Tensor a IndexExpr [IndexExpr] DType
  deriving (Show, Functor)
  
getName :: DescriptionF a -> a
getName (Object s _)    = s
getName (Field s _)     = s
getName (Array s _ _) = s
getName (ArrayLit s _) = s
  
type Description = DescriptionF String
type QualifiedDescription = DescriptionF [String]

qualifyNames :: DescriptionF String -> DescriptionF [String]
qualifyNames (Object s fs) = Object [s] $ (fmap (s:) . qualifyNames) <$> fs
qualifyNames obj = (:[]) <$> obj

{-
data Env = Env
  { prefixes :: [String]
  , fields :: [(Name,DType)]
  , readPtr :: String
  }
  deriving (Show)

assignWithIdx :: MatlabT -> MatlabT -> MatlabT -> MatlabC
assignWithIdx lhs rhsC rhsIdx =
  Assignment lhs $ Index rhsC rhsIdx

forAssign :: Name -> MatlabT -> MatlabT -> MatlabT -> MatlabT -> MatlabT -> MatlabC
forAssign i from to lhs rhs idx =
  let vi = Var i in
    For vi from to $
       assignWithIdx (Index lhs vi) rhs (Add idx vi)

preamble :: MatlabC
preamble = undefined

data Env' = Env'
  { idx :: MatlabT
  , contents :: MatlabT
  } deriving (Show)

initEnv' :: Env'
initEnv' = Env' (Var "readPtr") (Var "contents")

translateIn :: Expression -> State Env' MatlabC
translateIn (Struct name fields) = do
  f <- (mapM translateIn $ go name fields) 
  pure $ Seq f
 where
    go :: Name -> [Expression] -> [Expression]
    go n fs = map (((n ++ ".") ++) `fmap`)  fs
translateIn (Field name ty) = do
  -- account for ty to generate 10^x * 
  Env'{idx = idx, contents = c} <- get
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

writeField :: [Name] -> (Name, DType) -> [String]
writeField [] (name, _) = [name ++ " = " ] 

output :: Expression -> State Env [String]
output (Struct pr exps)  = do
   env <- get
   let ps' = prefixes env
   let fs' = fields env
   put $ env {prefixes = ps' ++ [pr]}
   res <- forM exps output
   put $ env {prefixes = ps', fields = fs'}
   pure $ concat res
   
output _ = undefined

expr1 :: Expression
expr1 = Field "test" Scalar

expr2 :: Expression
expr2 = Vector "vec" expr1 Scalar
-}
