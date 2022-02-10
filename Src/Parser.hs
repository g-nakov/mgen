module Parser where 

import Expression
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Char (isSpace, isAlphaNum)

newtype Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (Parser f) = f

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  (Parser f) >>= k = Parser $ \s -> do
    (a, s') <- f s
    parse (k a) s'
    
instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Functor Parser where
  fmap = ap . return
  
instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser f) <|> (Parser g) = Parser $ \s -> f s <|> g s

(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
p <&&> q = pure (&&) <*> p <*> q

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
p <||> q = pure (||) <*> p <*> q

pch :: (Char -> Bool) -> Parser Char
pch p = Parser $ \case 
    (c:cs) | p c -> Just (c, cs)
    _ -> Nothing

plit :: String -> Parser ()
plit = mapM_ (pch . (==))

pspace :: Parser ()
pspace = many (pch $ isSpace <&&> (/='\n')) $> ()
               
around :: Parser a -> Parser b -> Parser b
around s p = s *> p <* s

peol ::Parser ()
peol = pch (=='\n') $> ()

pkeyword :: Parser a -> Parser a
pkeyword = (pch (=='@') *>)

pcomment :: Parser ()
pcomment = many (pch $ (/= '@') <&&> (/= '\n')) $> ()

pident :: Parser String
pident = pure (:) <*> pch isAlphaNum <*> many (pch $ isAlphaNum <||> (=='_')) 

pTypedField :: Parser Description
pTypedField =  pure Field <*> (pFieldHeader
                               -- proper type
                               <* pcomment `around` pkeyword (plit "number" <|> plit "Number") 
                               <* peol)
                          <*> pure Nat

pFieldHeader :: Parser String
pFieldHeader = pspace `around` pident <* pch (== ':')

pArrayLitItem :: Parser () -- proper type
pArrayLitItem = pspace `around` pch (== '-') *> pcomment *> peol

-- pArrayLit :: Parser Description
-- pArrayLit = pure 

parseDescription :: String -> Maybe Description
parseDescription = undefined
