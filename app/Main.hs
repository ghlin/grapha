{-# LANGUAGE TupleSections #-}
module Main where

import           Debug.Trace
import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( forM_
                                                , (>=>)
                                                , foldM
                                                )
import           System.Environment             ( getArgs )
import           Text.Megaparsec                ( runParser )
import           Text.Megaparsec.Error          ( errorBundlePretty )
import           Text.Pretty.Simple             ( pPrint )

import qualified Lang.Typing                   as Ty
import qualified Lang.TypingEnv                as TE
import qualified Lang.Surface                  as S
import           Lang.Simpl.Translator
import           Lang.Core
import           Lang.Kind
import           Lang.Surface.Balancer
import           Lang.Surface.Renamer
import           Lang.Surface.Grouper
import           Lang.Surface.Desugar
import           Lang.Surface.PatternMatchCompiler
import           Lang.Surface.CaseLineariser
import           Lang.Surface.Parser            ( parser )
import           ParserHelper
import           Misc

printMany :: Show a => [a] -> IO ()
printMany = mapM_ $ (>> putStrLn "==================") . pPrint

kinds :: [S.DataTypeDef] -> [S.TypeClassDef] -> Either String TE.KindSubst
kinds ds is = do
  ds' <- TE.topoDatatypeDefs ds
  s1  <- foldM TE.dataTypeKinds TE.initialSubsts ds'
  is' <- TE.topoClassDefs is
  foldM TE.classKinds s1 is'

classes :: TE.KindSubst -> [S.TypeClassDef] -> Either String [TE.ClassDef]
classes = mapM . TE.translateClassDef

insts :: TE.KindSubst -> [S.InstanceDef] -> Either String [TE.InstDef]
insts = mapM . TE.translateInstDef

types :: TE.KindSubst -> [S.DataTypeDef] -> Either String [(Name, Kind)]
types ks = mapM k
  where k (S.DataTypeDef name _ _) = (name, ) <$> TE.lookupKind name ks

printAll :: S.Program -> IO ()
printAll p = case result of
               Left err -> print err
               Right (cls, ins, tys) -> do
                 putStrLn "==========classes============"
                 printMany cls
                 putStrLn "==========insts=============="
                 printMany ins
                 putStrLn "==========insts=============="
                 printMany tys
             where result = do let ds = S.dataTypeDefs p
                               let is = S.instanceDefs p
                               let cs = S.typeClassDefs p
                               k <- kinds ds cs
                               cls <- classes k cs
                               ins <- insts k is
                               tys <- types k ds
                               return (cls, ins, tys)

compile :: S.Program -> S.Program
compile p = let ixs = S.infixDefs p
             in linearise $ S.mapE regroupE $ compileProgram $ desugar $ S.mapE (balance ixs) p

main :: IO ()
main = do
  [srcFile] <- getArgs
  srcContent <- readFile srcFile
  case runParser parser srcFile srcContent of
    Left e -> putStrLn $ errorBundlePretty e
    Right p -> do
      let p' = compile p
      let cds = S.combinatorDefs p'
      pPrint $ translateC <$> cds
      -- pPrint p'
      -- pPrint p'
      -- printAll p'

