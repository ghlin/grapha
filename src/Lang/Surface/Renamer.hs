module Lang.Surface.Renamer
  ( renameProgram
  ) where

import qualified Control.Monad.Trans.State     as T
import           Control.Monad                  ( foldM, mapM )
import           Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Lang.Surface
import           Misc

type Subst = M.Map Name Name

data State
  = State
    { supply :: Int -- for fresh name
    }

type R a = T.State State a

-- | acquire a fresh name (with given prefix)
acquire :: String -> R Name
acquire prefix = do
  supply' <- T.gets $ (+ 1) . supply
  T.modify $ \s -> s { supply = supply' }
  return $ prefix <> show supply'

decorate :: String -> String
decorate = ("{" <>) . (<> "}")

flatten' :: [Pattern] -> [Name]
flatten' = mconcat . fmap flatten

flatten :: Pattern -> [Name]
flatten = f []
  where f ns (PVar n) = n:ns
        f ns (PCon _ cs) = flatten' cs <> ns
        f ns _ = ns

-- | assign a fresh name, return the updated Subst mapping
assign :: String -> Subst -> Name -> R Subst
assign prefix subst name = do
  name' <- (name <>) . decorate <$> acquire prefix
  return $ M.insert name name' subst

-- | assign many names..
assignMany :: String -> Subst -> [Name] -> R Subst
assignMany = foldM . assign

pat :: Subst -> Pattern -> Pattern
pat s (PVar n)    = PVar $ var s n
pat s (PCon c cs) = PCon c $ pat s <$> cs
pat _ p           = p

var :: Subst -> String -> String
var s name = fromMaybe name $ M.lookup name s

rename' :: Subst -> Expression -> R Expression
rename' s = r
  where
    v = var s
    r (EVar name)          = return $ EVar $ v name
    r (EApp e1 e2)         = EApp <$> r e1 <*> r e2
    r (EIf c t e)          = EIf <$> r c <*> r t <*> r e
    r (EUnary op e)        = EUnary (v op) <$> r e
    r (EBinary op lhs rhs) = EBinary (v op) <$> r lhs <*> r rhs
    r (EListLiteral es)    = EListLiteral <$> mapM r es
    r (ETupleLiteral es)   = ETupleLiteral <$> mapM r es
    -- where subst starts...
    r (ELam ps e) = do
      s' <- assignMany "lam" s $ flatten' ps
      ELam (pat s' <$> ps) <$> rename' s' e
    r (ECase e as) = do
      e' <- r e
      as' <- renameAlts s as
      return $ ECase e' as'
      where renameAlts = mapM . rA
            rA subs (CaseAlternative p e) = do
              s' <- assignMany "alt" subs $ flatten p
              e' <- rename' s' e
              return $ CaseAlternative (pat s' p) e'
    r (ELet bs e) = do
      let names = [name | LetBinding (CombinatorBinding name _) _ <- bs]
               <> [name | LetBinding (PatternBinding p) _ <- bs, name <- flatten p]
      s' <- assignMany "let" s names
      bs' <- renameBindings s' bs
      e' <- rename' s' e
      return $ ELet bs' e'
        where renameBindings = mapM . rB
              rB s' (LetBinding (PatternBinding p) e) = do
                e' <- rename' s' e
                return $ LetBinding (PatternBinding (pat s' p)) e'
              rB s' (LetBinding (CombinatorBinding comb ps) e) = do
                s'' <- assignMany "pat" s' $ flatten' ps
                e' <- rename' s'' e
                return $ LetBinding (CombinatorBinding (var s' comb) (pat s'' <$> ps)) e'
              rB s' (BindingAnnotation (Annotation name sc)) =
                return $ BindingAnnotation $ Annotation (var s' name) sc
    r (EDo stmts) = do
      let names = [name | DoBind (Just pattern) _ <- stmts, name <- flatten pattern]
               <> [name | DoLetBinding (PatternBinding pattern) _ <- stmts, name <- flatten pattern]
               <> [name | DoLetBinding (CombinatorBinding name _) _ <- stmts]
      s' <- assignMany "do" s names
      stmts' <- renameStmts s' stmts
      return $ EDo stmts'
        where renameStmts = mapM . rS
              rS s' (DoBind mp e) =
                DoBind (pat s' <$> mp) <$> rename' s' e
              rS s' (DoLetBinding (PatternBinding p) e) = do
                DoLetBinding (PatternBinding $ pat s' p) <$> rename' s' e
              rS s' (DoLetBinding (CombinatorBinding comb ps) e) = do
                s'' <- assignMany "pat" s' $ flatten' ps
                DoLetBinding (CombinatorBinding (var s' comb) $ pat s'' <$> ps) <$> rename' s'' e
    -- trivial cases
    r x = return x

renameCombinatorDef' :: Subst -> CombinatorDef -> R CombinatorDef
renameCombinatorDef' s (CombinatorDef name ps e) = do
  s' <- assignMany "pat" s $ flatten' ps
  CombinatorDef name (pat s' <$> ps) <$> rename' s' e

renameInstanceDef' :: Subst -> InstanceDef -> R InstanceDef
renameInstanceDef' s (InstanceDef name preds typ defs) =
  InstanceDef name preds typ <$> mapM (renameCombinatorDef' s) defs

renameProgram' :: Subst -> Program -> R Program
renameProgram' s prog = do
  let instances   = instanceDefs prog
  let combinators = combinatorDefs prog
  instances'   <- mapM (renameInstanceDef' s) instances
  combinators' <- mapM (renameCombinatorDef' s) combinators
  return $ prog { instanceDefs = instances', combinatorDefs = combinators' }

-- | 为局部(local)符号选择无重复的名字
-- e.g.
-- \x y z -> (\x y z -> (x, y, z)) x y z
-- ==>
-- \x{pat1} y{pat2} z{pat3} -> (\x{pat4} y{pat5} z{pat6} -> (x{pat4}, y{pat5}, z{pat6})) x{pat1} y{pat2} z{pat3}
renameProgram :: Program -> Program
renameProgram prog = T.evalState (renameProgram' M.empty prog) (State 0)
