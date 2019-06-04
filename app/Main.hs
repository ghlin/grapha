module Main where

import           Control.Monad
import           Data.List                      ( intersperse )
import           Data.Text                      ( unpack )
import           Data.Text.Lazy                 ( toStrict )
import           Options.Applicative
import           System.Environment
import           System.IO
import           System.Exit
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

data Options
  = Options
    { inputFile        :: String
    , outputFile       :: Maybe String
    , skipTypeChecking :: Bool
    , noPrelude        :: Bool
    , dumpCore         :: Bool
    , dumpSC           :: Bool
    }
    deriving (Show)

getopts :: Parser Options
getopts = Options
  <$> argument str (metavar "FILE")
  <*> optional (strOption (  long  "output-file"
                          <> short 'o'
                          <> help  "output file name"))
  <*> switch (  long  "skip-typecheck"
             <> short 't'
             <> help  "don't run type checker")
  <*> switch (  long  "no-prelude"
             <> short 'p'
             <> help  "don't inject the prelude")
  <*> switch (  long  "dump-core"
             <> short 'c'
             <> help  "instead of generating GCode, dump corecombinators")
  <*> switch (  long  "dump-sc"
             <> short  's'
             <> help   "instead of generating GCode, dump supercombinators")

progromPipes :: Options -> Pipe ErrorMessage Source Program
progromPipes o = parse
             >=> injectDataTypes
             >=> (if noPrelude o then return else injectPrelude)
             >=> balance
             >=> desugar
             >=> compilePatternMatching
             >=> expand

constrPipes :: Pipe ErrorMessage Program ConstrsTable
constrPipes = collectConstrs . dataTypeDefs

data InternalReps
  = InternalReps
    { irProgram   :: Program
    , irCore      :: [CoreCombinator]
    , irSCProgram :: SCProgram
    }
    deriving (Show)

compileSCPipe :: Options -> Pipe ErrorMessage Source InternalReps
compileSCPipe o s = do prog   <- progromPipes o s
                       ct     <- constrPipes prog
                       ccs    <- translate $ combinatorDefs prog
                       unless (skipTypeChecking o) $ void $ infer ct ccs -- this is really slow...
                       scProg <- liftCombinators builtins ct ccs
                       return $ InternalReps prog ccs scProg
                         where builtins = fmap (\(a, b, _, _) -> (a, b)) builtinCombinatorSignatures

compileGCodePipe :: Source -> Pipe ErrorMessage SCProgram [GInstr]
compileGCodePipe s (scs, entry) = do instrs       <- compileProgram (scs, builtins)
                                     let header = [ "Compiled from " <> sourceFileName s
                                                  , "------ original content: ------" ]
                                                  <> lines (sourceFileContent s)
                                                  <>
                                                  [ "------- ------- ------- -------" ]
                                     let more   = GComment <$> header
                                     return $ more <> [GEntry entry] <> instrs
                                       where builtins = fmap (\(a, _, _, b) -> (a, b)) builtinCombinatorSignatures

withEither :: Either ErrorMessage b -> (b -> IO ()) -> IO ()
withEither (Right e) f = f e
withEither (Left  e) f = do hPutStrLn stderr e
                            hPutStrLn stderr "Can't compile, good luck then..."
                            exitWith $ ExitFailure 1

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (getopts <**> helper) $ fullDesc
                                     <> progDesc "The Grapha compiler"
                                     <> header   "grc - Grapha compiler"
    run o = do
      let srcFile = inputFile o
      srcContent <- readFile srcFile
      let source = Source srcFile srcContent
      withEither (compileSCPipe o source) (start o source)
    writeOutput' Nothing lines = putStrLn lines
    writeOutput' (Just file) lines = writeFile file lines
    writeOutput = writeOutput' . outputFile
    start o s irs
      | dumpCore o || dumpSC o = dump         o s irs
      | otherwise              = compileGCode o s irs
    doDumpSC   prog  = putStrLn "--- dump sc ---"   >> (mapM_ putStrLn $ prettySC             <$> fst prog)
    doDumpCore cores = putStrLn "--- dump core ---" >> (mapM_ putStrLn $ prettyCoreCombinator <$> cores)
    dump o s irs
      | dumpCore o = doDumpCore (irCore      irs) >> dump (o { dumpCore = False }) s irs
      | dumpSC   o = doDumpSC   (irSCProgram irs) >> dump (o { dumpSC   = False }) s irs
      | otherwise  = return ()
    compileGCode o s irs = withEither (compileGCodePipe s $ irSCProgram irs) $ \instrs ->
        writeOutput o $ unlines $ printGCode instrs


