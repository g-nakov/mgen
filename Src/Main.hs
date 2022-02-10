module Main where

import Parser
import Expression

main :: IO ()
main = do
  args <- getArgs
  let fp = case args of 
    (fp :_) -> fp; 
    _ -> error "No file supplied"
  contents <- readFile fp
  let cs = parseDescription contents
  _
