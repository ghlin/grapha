module Lang.Surface.PatternMatchCompiler
  where

import qualified Control.Monad.Trans.State     as T
import           Control.Monad
import           Lang.Surface
import           Lang.Surface.Subst
import           Misc

import Debug.Trace

groupToplevelBindings :: [CombinatorDef] -> [[CombinatorDef]]
groupToplevelBindings = g [] [] Nothing
  where g s c _ [] = s <> [c]
        g s _ Nothing   ((d@(CombinatorDef n _ _)):ds) = g s [d] (Just n) ds
        g s c (Just n') ((d@(CombinatorDef n _ _)):ds) = if n == n'
                                                          then g s (c <> [d]) (Just n) ds
                                                          else g (s <> [c]) [d] (Just n) ds

-- | FIXME: 暂时未考虑annotation...
groupLetBindings :: [LetBinding] -> [[LetBinding]]
groupLetBindings = g [] [] Nothing
  where g s c _ []    = s <> [c]
        g s c l        ((d@(LetBinding (PatternBinding {}) _)):ds)     = g ([d]:s) c l ds
        g s c Nothing  ((d@(LetBinding (CombinatorBinding k _) _)):ds) = g s [d] (Just k) ds
        g s c (Just n) ((d@(LetBinding (CombinatorBinding k _) _)):ds) = if n == k
                                                                           then g s (c <> [d])   (Just n) ds
                                                                           else g (s <> [c]) [d] (Just n) ds

fromLetBinding :: LetBinding -> CombinatorDef
fromLetBinding (LetBinding (CombinatorBinding f ps) e) = CombinatorDef f ps e

fromCombinatorDef :: CombinatorDef -> LetBinding
fromCombinatorDef (CombinatorDef f ps e) = LetBinding (CombinatorBinding f ps) e

pmFail :: Expression
pmFail = EVar "undefined"

type MatchClause = ([Pattern], Expression)
data ClauseType = CTVar | CTCon | CTLit deriving (Show, Eq)

ofClause :: Pattern -> ClauseType
ofClause PVar {}   = CTVar
ofClause PWildcard = CTVar
ofClause PCon {}   = CTCon
ofClause PLit {}   = CTLit

toMatchClause :: CombinatorDef -> MatchClause
toMatchClause (CombinatorDef _ ps e) = (ps, e)

-- | 分组, 结果形式为CTCon和CTVar交替出现
partition :: [MatchClause] -> [(ClauseType, [MatchClause])]
-- partition ms | trace ("PARTITION:\n CLAUSE= " <> unlines (show <$> ms)) False = undefined
partition ms = p Nothing [] [] ms
  where
    p Nothing   q _ ((m@((p1:_), _)):ms) = p (Just $ ofClause p1) q [m] ms
    p (Just ct) q c []                   = q <> [(ct, c)]
    p (Just ct) q c ((m@((p1:_), _)):ms) | ct == ofClause p1 = p (Just ct) q (c <> [m]) ms
                                         | otherwise = p (Just $ ofClause p1) (q <> [(ct, c)]) [m] ms

data PMCState
  = PMCState
    { supply :: Int
    , env    :: [DataTypeDef]
    }
    deriving (Show, Eq)

type M a = T.State PMCState a

acquireId :: M Name
acquireId = do
  i <- (+ 1) <$> T.gets supply
  T.modify $ \s -> s { supply = i }
  return $ "u{mp_" <> show i <> "}"

lookupProd :: Name -> M Int
lookupProd p = do dts <- T.gets env
                  let topair (ProductDef nam ts) = (nam, length ts)
                  let prod (DataTypeDef _ _ ps) = topair <$> ps
                  let prods = mconcat $ prod <$> dts
                  case lookup p prods of
                    Just arity -> return arity
                    Nothing    -> fail $ "Missing value constructor: " <> p

match :: [Name] -> Expression -> [MatchClause] -> M Expression
-- match us e ms | trace ("MATCH " <> show us <> ":\n exp = " <> show e <> "\n" <> unlines (show <$> ms)) False = undefined
match _  _ [([], e)]  = compileE e
match [] e []         = compileE e
match ns e cs         = foldM (runMatch ns) e $ reverse $ partition cs

preprocessPat :: MatchClause -> M MatchClause
preprocessPat (PWildcard:ps, e) = do s <- acquireId
                                     return (PVar s:ps, e)
preprocessPat x                 = return x

matchPVar :: Name -> MatchClause -> MatchClause
matchPVar u (PVar n:ps, e) = (ps, subst n u e)

propagateCon :: MatchClause -> MatchClause
propagateCon (PCon _ ps:rs, e) = (ps <> rs, e)

matchPCons :: [Name] -> Expression -> (Name, [MatchClause]) -> M CaseAlternative
matchPCons us e (con, cs) = do arity <- lookupProd con
                               us'   <- replicateM arity acquireId
                               let cs' = propagateCon <$> cs
                               e'    <- match (us' <> us) e cs'
                               return $ CaseAlternative (PCon con $ PVar <$> us') e'

matchPLit :: [Name] -> Expression -> MatchClause -> M CaseAlternative
matchPLit us fallbackE (PLit l:ps, e) = do e' <- match us fallbackE [(ps, e)]
                                           return $ CaseAlternative (PLit l) e'

runMatch :: [Name] -> Expression -> (ClauseType, [MatchClause]) -> M Expression
runMatch (n:ns) e (CTVar, cs) = do cs'  <- mapM preprocessPat cs
                                   let cs'' = matchPVar n <$> cs'
                                   match ns e cs''
runMatch (n:ns) e (CTCon, cs) = do alts <- mapM (matchPCons ns e) $ groupCon cs
                                   let defA = CaseAlternative (PVar n) e
                                   return $ ECase (EVar n) (alts <> [defA])
runMatch (n:ns) e (CTLit, cs) = do alts <- mapM (matchPLit ns e) cs
                                   let defA = CaseAlternative (PVar n) e
                                   return $ ECase (EVar n) (alts <> [defA])

groupCon :: [MatchClause] -> [(Name, [MatchClause])]
groupCon = g []
  where
    g q []      = q
    g q (m:ms)  = g (insert m q) ms
    insert m@(PCon c _:_, _) [] = [(c, [m])]
    insert m@(PCon c _:_, _) ((k, ms):rs) | k == c = (k, ms <> [m]):rs
                                          | otherwise = (k, ms):insert m rs


compileBindingGroup :: [CombinatorDef] -> M CombinatorDef
compileBindingGroup cs = do let combA (CombinatorDef _ ps _) = length ps
                            let combN (CombinatorDef n _ _)  = n
                            let name  = combN $ head cs
                            let arity = combA $ head cs
                            let mcs   = toMatchClause <$> cs
                            us <- replicateM arity acquireId
                            e  <- match us pmFail mcs
                            return $ CombinatorDef name (PVar <$> us) e

compileLetBindingGroup :: [LetBinding] -> M LetBinding
compileLetBindingGroup ls = fromCombinatorDef <$> (compileBindingGroup $ fromLetBinding <$> ls)

compileC :: [CombinatorDef] -> M [CombinatorDef]
compileC cds = mapM compileBindingGroup $ groupToplevelBindings cds

compileE :: Expression -> M Expression
compileE (ELet bs e) = do let groups = groupLetBindings bs
                          bs' <- mapM compileLetBindingGroup groups
                          e'  <- compileE e
                          return $ ELet bs' e'
compileE (EApp l r)   = EApp <$> compileE l <*> compileE r
compileE (ELam ps e)  = ELam ps <$> compileE e
compileE (ECase e as) = ECase <$> compileE e <*> mapM compileA as
compileE (EIf c t e)  = EIf <$> compileE c <*> compileE t <*> compileE e
compileE v            = return v

compileA :: CaseAlternative -> M CaseAlternative
compileA (CaseAlternative p e) = CaseAlternative p <$> compileE e

compileI :: InstanceDef -> M InstanceDef
compileI (InstanceDef name ps ty ds) = InstanceDef name ps ty <$> compileC ds

compileP :: Program -> M Program
compileP p = do let ids = instanceDefs  p
                let cds = combinatorDefs p
                cds' <- compileC cds
                ids' <- mapM compileI ids
                return $ p { instanceDefs = ids', combinatorDefs = cds' }

compileProgram :: Program -> Program
compileProgram p = T.evalState (compileP p) $ PMCState 0 (dataTypeDefs p)

