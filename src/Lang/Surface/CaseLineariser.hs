module Lang.Surface.CaseLineariser
  ( linearise
  ) where


import Debug.Trace
import           Control.Monad                  ( replicateM
                                                , foldM
                                                )
import qualified Control.Monad.Trans.State     as T
import           Lang.Surface
import           Lang.Surface.Subst
import           Lang.Builtins
import           Misc

linearise :: Program -> Program
linearise prog = T.evalState (linearise' prog) (LState 0)

linearise' :: Program -> M Program
linearise' prog = do let cds = combinatorDefs prog
                     let ins = instanceDefs   prog
                     cds' <- mapM lrC cds
                     ins' <- mapM lrI ins
                     return $ prog { instanceDefs = ins, combinatorDefs = cds' }

newtype LState
  = LState
    { supply :: Int
    }
    deriving (Show, Eq)

type M a = T.State LState a

acquireId :: M Name
acquireId = do
  i <- (+ 1) <$> T.gets supply
  T.modify $ \s -> s { supply = i }
  return $ "u{" <> show i <> "}"

simpleEnought :: Expression -> Bool
simpleEnought EVar{} = True
simpleEnought ELit{} = True
simpleEnought _      = False

simpleEnoughtP :: Pattern -> Bool
simpleEnoughtP PCon{} = False
simpleEnoughtP _      = True

lr :: Expression -> Pattern -> Expression -> M [CaseAlternative]
lr fall p    body | simpleEnoughtP p = do body' <- lrE body
                                          return [CaseAlternative p body', CaseAlternative PWildcard fall]
lr fall (PCon c ps) body = do let reuse p = if simpleEnoughtP p
                                               then return (p, True)
                                               else do { u <- acquireId; return (PVar u, False) }
                              us <- mapM reuse ps
                              let bs = ps `zip` us
                              let folder b (p, (u, simple)) = if simple
                                                                then return b
                                                                else do let exam = let PVar x = u in EVar x
                                                                        lrA exam fall (CaseAlternative p b)
                              e <- foldM folder body bs
                              return [ CaseAlternative (PCon c (fst . snd <$> bs)) e
                                     , CaseAlternative PWildcard fall ]

lrA :: Expression -> Expression -> CaseAlternative -> M Expression
lrA exam fall (CaseAlternative p body) = do alts <- lr fall p body
                                            return $ ECase exam alts

lrE :: Expression -> M Expression
lrE (ECase e alts) | simpleEnought e = foldM (lrA e) (EVar missingCaseVarName) $ reverse alts
lrE (ECase e alts) = do u <- acquireId
                        body <- lrE $ ECase (EVar u) alts
                        return $ ELet [LetBinding (CombinatorBinding u []) e] body
lrE (ELet bs e) = do bs' <- mapM lrB bs
                     e'  <- lrE e
                     return $ ELet bs' e'
lrE (EApp l r)     = EApp <$> lrE l <*> lrE r
lrE (ELam ps body) = ELam ps <$> lrE body
lrE (EIf c t e)    = EIf <$> lrE c <*> lrE t <*> lrE e
lrE e              = return e

lrB :: LetBinding -> M LetBinding
lrB (LetBinding f e) = LetBinding f <$> lrE e

lrC :: CombinatorDef -> M CombinatorDef
lrC (CombinatorDef name pats body) = CombinatorDef name pats <$> lrE body

lrI :: InstanceDef -> M InstanceDef
lrI (InstanceDef name preds ty c) = InstanceDef name preds ty <$> mapM lrC c

