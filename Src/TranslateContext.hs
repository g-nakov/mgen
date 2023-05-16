module TranslateContext where

import Expression
import Description
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Data.Functor ( (<&>) ) 

data Ty = O | A | F FieldType
  deriving (Eq, Show)

data ObjectBreakdown = ObjectBreakdown
  { objects :: [Name]
  , arrays  :: [Name]
  , rest :: [Name]
  }
  deriving (Show)

type StartIndex = Int

data TrState = TrState
  { offset :: Maybe Term
  , fields :: Map QualifiedName Ty
  , breakdowns :: Map QualifiedName ObjectBreakdown
  , startIdx :: StartIndex 
  } 

data TrError
  = UnknownField QualifiedName
  | ExistingField QualifiedName
  | WrongType QualifiedName Ty
  | ParseError String
  deriving (Eq, Show)

type Translated a = StateT TrState (Except TrError) a

initState :: TrState
initState = TrState Nothing M.empty M.empty (-1)

locals :: Translated (Set QualifiedName)
locals = gets fields <&> S.fromList . M.keys

breakdownForObject :: QualifiedName -> Translated (Maybe ObjectBreakdown)
breakdownForObject key = gets breakdowns <&> M.lookup key

startIndex :: Translated StartIndex
startIndex = gets startIdx

emptyBreakdown :: ObjectBreakdown
emptyBreakdown = ObjectBreakdown [] [] []

freshVar :: Name -> Translated Term
freshVar s = do
  ls <- locals
  if [s] `elem` ls
    then freshVar (s++"0")
    else pure $ Var s


data TargetLang = Python | Matlab
 deriving (Show)

instance Read TargetLang where
  readsPrec _ "matlab" = [(Matlab, "")]
  readsPrec _ "python" = [(Python, "")]
  readsPrec _ _ = []
