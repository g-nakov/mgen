module Main where

import Description
import Parser
import Translate (translate)
import TranslateContext( TargetLang(Matlab) )
import Options.Applicative

data Conf = Conf
  { srcFile :: String 
  , tgtLang :: TargetLang
  }

main :: IO ()
main = do
  conf <- execParser pConf
  contents <- readFile $ srcFile conf
  case translate (tgtLang conf) (parse pDesc contents initState) of
    Left err -> error $ show err
    Right res -> putStrLn res
 where
  pConf = info
          (Conf <$> argument str ( help "input description FILE" <> metavar "FILE")
                  <*> option auto (long "lang" <> help "Target language"
                                   <> showDefault <> value Matlab <> metavar "LANG")
          <**> helper)
          (fullDesc <> progDesc "Generate code for data input from using an mgen description file")  
  

