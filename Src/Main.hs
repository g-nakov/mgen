module Main where

import System.Environment
import Description
import Parser
import Translate (translate)

main :: IO ()
main = do
  args <- getArgs
  let fp = case args of {(x :_) -> x; _ -> error "No file supplied"}
  contents <- readFile fp
  case translate $ parse pDesc contents initS of
    Left err -> error err
    Right res -> putStrLn res
