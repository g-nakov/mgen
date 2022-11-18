module Parser where 

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Char


type IndentLvl = Int

newtype Parser a = Parser (String -> IndentLvl -> Maybe (a, String, IndentLvl))

getLvl :: Parser IndentLvl
getLvl = Parser $ \s il -> Just (il, s, il)

putLvl :: IndentLvl -> Parser ()
putLvl l = Parser $ \s _ -> Just ((), s, l)

parse :: Parser a -> String -> IndentLvl -> a
parse (Parser f) s l = case f s l of
  (Just (a, [], _)) -> a
  _          -> error "Error parsing"

initState :: IndentLvl
initState = 0

instance Monad Parser where
  return = pure
  (Parser f) >>= k = Parser $ \s is -> do
    (a, s', is') <- f s is
    let (Parser g) = k a
    g s' is'
    
instance Applicative Parser where
  pure a = Parser $ \s is -> Just (a, s, is)
  (<*>) = ap

instance Functor Parser where
  fmap = ap . return
  
instance Alternative Parser where
  empty = Parser $ \_ _ -> Nothing
  (Parser f) <|> (Parser g) = Parser $ \s is -> f s is <|> g s is

data EmptySequence 
  = AllowZero
  | RequireOne
  | Require Int -- exactly n
  deriving (Eq,Show)  
  
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
p <&&> q = (&&) <$> p <*> q

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
p <||> q = (||) <$> p <*> q

pch :: (Char -> Bool) -> Parser Char
pch p = Parser $ \s is -> case s of
    (c:cs) | p c -> Just (c, cs, is)
    _ -> Nothing
    
plit :: String -> Parser ()
plit = mapM_ (pch . (==))

pblock :: Parser a -> Parser [a]
pblock p = do
  oldLvl <- getLvl
  peol
  sps <- many (pch $ isSpace <&&> (/='\n'))
  let i = length sps
  guard (i > oldLvl)
  putLvl i
  x  <- p
  xs <- many $ peol *> pspace' (Require i) *> p
  putLvl oldLvl
  return (x : xs)

pseq :: Parser a -> EmptySequence -> Parser [a]
pseq p = \case
  (Require n) -> do 
    sps <- many p
    guard (length sps == n)
    return sps
  AllowZero   -> many p
  RequireOne  -> some p

pspace' :: EmptySequence -> Parser ()
pspace' = (() <$) . pseq (pch $ isSpace <&&> (/='\n')) 
  
pspace :: Parser ()
pspace = pspace' AllowZero

psep :: EmptySequence -> Parser () -> Parser a -> Parser [a]
psep e s p = (:) <$> p <*> many (id <$ s <*> p)
 <|> (if e == AllowZero then pure [] else empty)

peol ::Parser ()
peol = pch (=='\n') $> ()

pkey :: Parser a -> Parser a
pkey = (pch (=='@') *>)

pcomment' :: EmptySequence -> Parser ()
pcomment' = (() <$). pseq (pch $ (/= '@') <&&> (/='\n'))  

pcomment :: Parser ()
pcomment = pcomment' AllowZero

pident :: Parser String
pident =  (:) <$> pch isLower <*> many (pch $ isAlphaNum <||> (=='_'))

pnumber :: Parser Int
pnumber = optional psign >>= \x -> maybe id (const negate) x <$> ppos
  where
    psign = pch (== '-') <* pspace
    ppos = foldl (\n -> (10*n +) . digitToInt) 0 <$> some (pch isDigit)
