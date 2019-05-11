module Main where

import           Control.Monad                  ( forM_ )
import           System.Environment             ( getArgs )
import           Text.Megaparsec                ( runParser )
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           Text.Pretty.Simple             ( pPrint )

import           Lang.Surface
import           Lang.Surface.Balance
import           Lang.Surface.Parser            ( parser )
import           ParserHelper

-- printMany = mapM_ $ (>> putStrLn "==================") . pPrint

main :: IO ()
main = do
  [srcFile] <- getArgs
  srcContent <- readFile srcFile
  case runParser parser srcFile srcContent of
    Left e -> putStrLn $ errorBundlePretty e
    Right p -> do
      let ids = infixDefs p
      pPrint p
      putStrLn "===================================="
      pPrint $ mapE (balance ids) p

