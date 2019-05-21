module Main where

import           Control.Monad
import           Data.Text                      ( unpack )
import           Data.Text.Lazy                 ( toStrict )
import           System.Environment
import           Text.Pretty.Simple

import           Pipes.Parser
import           Pipes.PreludeInjector
import           Pipes.Balancer
import           Pipes.Desugar
import           Pipes.PatternMatchingCompiler
import           Pipes.CaseExpander
import           Pipes.Grouper
import           Pipes.Translator
import           Pipes.ConstrCollector
import           Pipes.TypeChecker
import           Pipes.Lifter
import           Pipes.GCodeCompiler
import           Pipes.GCodePrinter

import           Lang.Surface
import           Lang.Core
import           Lang.SC
import           Lang.GCode
import           Lang.Type
import           Lang.Builtins

import           Pipe
import           Misc

import           Debug.Trace

progromPipes :: Pipe ErrorMessage Source Program
progromPipes = parse
           >=> injectPrelude
           >=> balance
           >=> desugar
           >=> compilePatternMatching
           >=> expand

corePipes :: Pipe ErrorMessage Program [[CoreCombinator]]
corePipes = regroup
        >=> mapM translate

constrPipes :: Pipe ErrorMessage Program ConstrsTable
constrPipes = collectConstrs . dataTypeDefs

pipe :: Pipe ErrorMessage Source (ConstrsTable, [[CoreCombinator]], Program)
pipe s = do p  <- progromPipes s
            ts <- constrPipes p
            cs <- corePipes p
            return (ts, cs, p)

builtins :: [(Name, Int)]
builtins = fmap (\(a, b, _) -> (a, b)) builtinCombinatorSignatures

liftPipe :: Pipe ErrorMessage Source ([SC], Name)
liftPipe s = do prog <- progromPipes s
                cs   <- mconcat <$> corePipes prog
                ct   <- constrPipes prog
                liftCombinators builtins ct cs

compileSCPipe :: Pipe ErrorMessage Source [GInstr]
compileSCPipe s = do prog         <- progromPipes s
                     ct           <- constrPipes prog
                     cs           <- mconcat <$> corePipes prog
                     infer ct cs
                     (scs, entry) <- liftCombinators builtins ct cs
                     instrs       <- compileProgram scs
                     let header = [ "Compiled from " <> sourceFileName s
                                  , "------ original content: ------" ]
                                  <> lines (sourceFileContent s)
                                  <>
                                  [ "------- ------- ------- -------" ]
                     let more   = GComment <$> header
                     return $ more <> [GEntry entry] <> instrs

main :: IO ()
main = do
  [srcFile]  <- getArgs
  srcContent <- readFile srcFile
  case compileSCPipe (Source srcFile srcContent) of
    Left  e -> putStrLn e
    Right a -> mapM_ putStrLn $ printGCode a

