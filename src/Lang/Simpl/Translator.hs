module Lang.Simpl.Translator where

import           Control.Monad.Trans.State     as T
import           Control.Monad                  ( mapM
                                                , replicateM
                                                , foldM
                                                )
import           Lang.Surface.Subst
import           Lang.Surface                  as U
import           Lang.Simpl                    as S
import           Lang.Builtins
import           Misc

translate :: Program -> Program
translate = undefined

translateC :: CombinatorDef -> SimplCombinatorDef
translateC c = T.evalState (trC c) (TState 0)

newtype TState
  = TState
    { supply :: Int
    }
    deriving (Show, Eq)

type M a = T.State TState a

acquireId :: M Name
acquireId = do
  i <- (+ 1) <$> T.gets supply
  T.modify $ \s -> s { supply = i }
  return $ "u{" <> show i <> "}"

unPVar :: Pattern -> Name
unPVar (PVar v) = v
unPVar _ = error "not a PVar"

unEVar :: Expression -> Name
unEVar (U.EVar v) = v
unEVar _ = error "not a EVar"


mkBinding :: SimplExpr -> (Name, Int) -> SimplCombinatorDef
mkBinding e (v, f) = SimplCombinatorDef v [] (EPick f e)

trA :: Name -> [CaseAlternative] -> M SimplExpr
trA _    []                                = return $ S.EVar missingCaseVarName
trA exam (CaseAlternative (PVar v) e : _)  = trE $ subst v exam e
trA _    (CaseAlternative PWildcard e : _) = trE e
trA exam (CaseAlternative (PCon con ps) e:rs) = do let vs = unPVar <$> ps
                                                   let arity = length vs
                                                   let examE = S.EVar exam
                                                   let bindings = mkBinding examE <$> (vs `zip` [0..arity])
                                                   e' <- trE e
                                                   let passE = if null bindings
                                                                  then e'
                                                                  else S.ELet bindings e'
                                                   let checkE = ETest con examE
                                                   failE <- trA exam rs
                                                   return $ S.EIf checkE passE failE

trP :: [(Pattern, SimplExpr)] -> M [SimplCombinatorDef]
trP ((PWildcard, e):ps) = trP ps
trP ((PVar p, e):ps)    = (:) (SimplCombinatorDef p [] e) <$> trP ps
trP ((PCon c ps, e):rs) = do let vs = unPVar <$> ps
                             let arity = length ps
                             vs <- replicateM arity acquireId
                             let picks = flip EPick e  <$> [0 .. arity]
                             let mkB (v, p) = SimplCombinatorDef v [] p
                             let bindings = mkB <$> (vs `zip` picks)
                             let more = ps `zip` picks
                             rs' <- trP $ more <> rs
                             return $ bindings <> rs'

trB :: [LetBinding] -> M [SimplCombinatorDef]
trB (LetBinding (CombinatorBinding n ps) e:bs) = do b   <- SimplCombinatorDef n (unPVar <$> ps) <$> trE e
                                                    bs' <- trB bs
                                                    return $ b:bs'
trB (LetBinding (PatternBinding p) e:bs) = do e' <- trE e
                                              bs' <- trB bs
                                              pbs <- trP [(p, e')]
                                              return $ pbs <> bs'

trE :: Expression -> M SimplExpr
trE (U.ECase exam alts) = trA (unEVar exam) alts
trE (U.ELet  bs   e   ) = S.ELet <$> trB bs <*> trE e
trE (U.EIf c t e      ) = S.EIf <$> trE c <*> trE t <*> trE e
trE (U.EApp l  r      ) = S.EApp <$> trE l <*> trE r
trE (U.ELam ps e      ) = S.ELam (unPVar <$> ps) <$> trE e
trE (U.EVar v         ) = return $ S.EVar v
trE (U.ELit l         ) = return $ S.ELit l
trE e                   = error "trE : Unexpected expression"

trC :: CombinatorDef -> M SimplCombinatorDef
trC (CombinatorDef f ps e) = SimplCombinatorDef f (unPVar <$> ps) <$> trE e


