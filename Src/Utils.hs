module Utils where

import Control.Arrow ( (>>>) )
import Data.Char (toUpper)

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

indent :: Int -> String
indent n = replicate n ' '

toTitle :: String -> String
toTitle [] = []
toTitle (c:cs) = toUpper c : cs
