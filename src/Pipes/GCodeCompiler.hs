module Pipes.GCodeCompiler
  ( compileProgram
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class      ( lift )
import qualified Control.Monad.Trans.State     as T
import qualified Control.Monad.Trans.Writer    as W
import qualified Control.Monad.Trans.Except    as E
import           Data.List                      ( union )
import           Data.Char                      ( ord )
import           Text.Printf                    ( printf )
import           Lang.SC
import           Lang.GCode
import           Lang.Literal
import           Misc
import           Pipe

compileProgram :: Pipe ErrorMessage ([SC], [(Name, [Bool])]) [GInstr]
compileProgram (scs, bs) = runM (CState 0 bs) $ compileProgram' scs

compileProgram' :: [SC] -> M ()
compileProgram' scs = do let builtins = unions $ extractBuiltins <$> scs
                         let picks    = unions $ extractPicks    <$> scs
                         let packs    = unions $ extractPacks    <$> scs
                         let tests    = unions $ extractTests    <$> scs
                         mapM_ compileSC      scs
                         mapM_ compileBuiltin builtins
                         mapM_ compilePick    picks
                         mapM_ compilePack    packs
                         mapM_ compileTest    tests

extract :: Eq a => (SCExpr -> [a]) -> SCExpr -> [a]
extract f = ex
  where ex (SCApp l r    ) = ex l `union` ex r
        ex (SCIf  c t e  ) = ex c `union` ex t `union` ex e
        ex (SCLet _ e b  ) = ex e `union` ex b
        ex (SCLetRec bs e) = unions (ex . snd <$> bs) `union` ex e
        ex e               = f e

extractBuiltins :: SC -> [(Name, Int)]
extractBuiltins (SC _ _ e) = extract ex e
  where ex (SCBuiltin n a) = [(n, a)]
        ex _               = []

extractPacks :: SC -> [(Name, Int)]
extractPacks (SC _ _ e) = extract ex e
  where ex (SCPack n a) = [(n, a)]
        ex _            = []

extractPicks :: SC -> [Int]
extractPicks (SC _ _ e) = extract ex e
  where ex (SCPick f) = [f]
        ex _          = []

extractTests :: SC -> [Name]
extractTests (SC _ _ e) = extract ex e
  where ex (SCTest n) = [n]
        ex _          = []

unions :: Eq a => [[a]] -> [a]
unions = foldr union []

data CState
  = CState
    { supply   :: Int
    , builtins :: [(Name, [Bool])]
    }

type M a = E.ExceptT ErrorMessage
         ( W.WriterT [GInstr]
         ( T.State   CState
         )) a

runM :: CState -> M a -> Either ErrorMessage [GInstr]
runM s m =
  let uw1 = E.runExceptT m
      uw2 = W.runWriterT uw1
      (r, instrs) = T.evalState uw2 s
   in const instrs <$> r

-- | name -> stack ref
type Context = [(Name, Int)]

lookupRef :: Name -> Context -> M Int
lookupRef v p = case lookup v p of
                  Just r  -> return r
                  Nothing -> E.throwE $ printf "Lookup ref failed for symbol: %s" v

type Compiler a = Context -> Int -> a

acquireLabel :: M Int
acquireLabel = do i <- lift $ lift $ T.gets $ (+ 1) . supply
                  lift $ lift $ T.modify $ \s -> s { supply = i }
                  return i

instr :: GInstr -> M ()
instr = lift . W.tell . singleton

pack :: Name -> Name
pack = printf "$pack{%s}"

pick :: Int -> Name
pick = printf "$pick{%d}"

test :: Name -> Name
test = printf "$test{%s}"

builtin :: Name -> Name
builtin = printf "$builtin{%s}"

compileSC :: SC -> M ()
compileSC (SC name args body) = do let d = length args
                                   let p = args `zip` ((d - ) <$> [0 ..])
                                   instr $ GGlobalStart name d
                                   compileBody p d body
                                   instr $ GGlobalEnd

compileBody :: Compiler (SCExpr -> M ())
compileBody p d e = do compileE p d e
                       instr $ GUpdate $ d + 1
                       instr $ GPop d
                       instr $ GUnwind

compileE :: Compiler (SCExpr -> M ())
compileE p d = r
  where
    r (SCLit (LInteger i))      = instr $ GPushPrimI i
    r (SCLit (LChar c))         = instr $ GPushPrimI $ ord c
    r (SCLit l)                 = E.throwE $ "TODO [GCodeCompiler.compileE]: unimplemented"
    r (SCVar v) | head v == '$' = instr $ GPushGlobal v
                | otherwise     = do ref <- lookupRef v p
                                     instr $ GPush $ d - ref
    r (SCBuiltin b arity)       = instr $ GPushGlobal $ builtin b
    r (SCPack tag _)            = instr $ GPushGlobal $ pack    tag
    r (SCPick c)                = instr $ GPushGlobal $ pick    c
    r (SCTest c)                = instr $ GPushGlobal $ test    c
    r (SCApp lhs rhs)           = do compileE p  d      rhs
                                     compileE p (d + 1) lhs
                                     instr GMkApp
    r (SCLet name expr body)    = do compileE p  d  expr
                                     let d' = d + 1
                                     let p' = (name, d'):p
                                     compileE p' d' body
                                     instr $ GSlide 1
    r (SCIf c t e)              = do l1 <- acquireLabel
                                     l2 <- acquireLabel
                                     compileE p d c
                                     instr   GEval
                                     instr $ GJumpFalse l1
                                     compileE p d t
                                     instr $ GJump l2
                                     instr $ GLabel l1
                                     compileE p d e
                                     instr $ GLabel l2
    r (SCLetRec bindings body)  = do let p' = foldr (:) p $ (fst <$> bindings) `zip` ((+ d) <$> [1..])
                                     let d' = d + length bindings
                                     compileLetRec p' d' $ snd <$> bindings
                                     compileE p' d' body
                                     instr $ GSlide $ d' - d

compileLetRec :: Compiler ([SCExpr] -> M ())
compileLetRec p d rhss = do let n = length rhss
                            instr $ GAlloc n -- alloc n holes
                            forM_ (rhss `zip` [0 ..]) $ \(e, r) -> do
                              compileE p d e
                              instr $ GUpdate $ n - r

lookupStrictness :: Name -> M [Bool]
lookupStrictness k = do bs <- lift $ lift $ T.gets builtins
                        case lookup k bs of
                          Nothing -> E.throwE $ "Unknown builtin: " <> k
                          Just st -> return st

compileBuiltin :: (Name, Int) -> M ()
compileBuiltin (name, arity) = do strictness <- lookupStrictness name
                                  instr $ GGlobalStart (builtin name) arity
                                  let strictArgs = fst <$> filter snd ([0 .. arity - 1] `zip` strictness)
                                  forM_ strictArgs $ \n -> do
                                    instr $ GPush n
                                    instr   GEval
                                    instr $ GUpdate $ n + 1
                                  instr $ GBuiltin name arity
                                  instr $ GUpdate 1
                                  instr   GGlobalEnd

compilePack :: (Name, Int) -> M ()
compilePack (tag, arity) = do instr $ GGlobalStart (pack tag) arity
                              instr $ GPack   tag arity
                              instr $ GUpdate 1
                              instr   GGlobalEnd

compilePick :: Int -> M ()
compilePick f = do instr $ GGlobalStart (pick f) 1
                   instr   GEval
                   instr $ GPick   f
                   instr $ GUpdate 1
                   instr   GUnwind
                   instr   GGlobalEnd

compileTest :: Name -> M ()
compileTest tag = do instr $ GGlobalStart (test tag) 1
                     instr   GEval
                     instr $ GTest   tag
                     instr $ GUpdate 1
                     instr   GGlobalEnd

