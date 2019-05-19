module Main where

import           Text.Pretty.Simple
import           Control.Monad
import           System.Environment
import           Pipes.Parser
import           Pipes.Balancer
import           Pipes.Desugar
import           Pipes.PatternMatchingCompiler
import           Pipes.CaseExpander
import           Pipes.Grouper
import           Pipes.Translator
import           Pipes.ConstrCollector
import           Pipes.TypeChecker
import           Lang.Surface
import           Lang.Core
import           Lang.Type
import           Pipe
import           Misc

import Debug.Trace

progromPipes :: Pipe ErrorMessage Source Program
progromPipes = parse
           >=> balance
           >=> desugar
           >=> trace "compilepm"  compilePatternMatching
           >=> trace "expandcase" expand

corePipes :: Pipe ErrorMessage Program [[CoreCombinator]]
corePipes = trace "regroup"   regroup
        >=> trace "translate" mapM translate

constrPipes :: Pipe ErrorMessage Program ConstrsTable
constrPipes = collectConstrs . dataTypeDefs

pipe :: Pipe ErrorMessage Source (ConstrsTable, [[CoreCombinator]], Program)
pipe s = do p <- progromPipes s
            ts <- constrPipes p
            cs <- corePipes p
            return (ts, cs, p)

tiDemoPipe :: Pipe ErrorMessage Source (Program, [[CoreCombinator]],  [(Type, Subst)])
tiDemoPipe s = do p <- progromPipes s
                  trace ("p: " <> show p) $ return ()
                  ct <- constrPipes p
                  trace ("=================") $ return ()
                  trace (show ct) $ return ()
                  grs <- regroup p
                  trace ("grs=================") $ return ()
                  trace (unlines $ show <$> grs) $ return ()
                  css <- mapM translate grs
                  trace ("=================") $ return ()
                  trace (unlines $ show <$> css) $ return ()
                  let cs = head <$> filter (not . null) css
                  tys <- mapM (infer ct) cs
                  return (p, css, tys)

main :: IO ()
main = do
  [srcFile] <- getArgs
  srcContent <- readFile srcFile
  case tiDemoPipe (Source srcFile srcContent) of
    Left e -> putStrLn e
    Right a -> pPrint a
                         -- mapM_ pPrint tys

