module Pipes.Translator
  ( translate
  ) where

import           Control.Monad.Trans.State     as T
import           Control.Monad                  ( mapM
                                                , replicateM
                                                , foldM
                                                )
import           Lang.Surface.Subst
import           Lang.Surface                  as S
import           Lang.Core                     as C
import           Lang.Builtins
import           Misc
import           Pipe

import        Debug.Trace

translate :: Pipe ErrorMessage [CombinatorDef] [CoreCombinator]
translate = Right . fmap translateC

translateC :: CombinatorDef -> CoreCombinator
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
unEVar (S.EVar v) = v
unEVar _ = error "not a EVar"


mkBinding :: CoreExpr -> Name -> (Name, Int) -> CoreCombinator
mkBinding e c (v, f) = CoreCombinator v [] (EPick c f e)

trA :: Name -> [CaseAlternative] -> M CoreExpr
trA _    []                              = return $ C.EVar missingCaseVarName
trA exam (CaseAlternative (PVar v) e:_)  = trE $ subst v exam e
trA _    (CaseAlternative PWildcard e:_) = trE e
trA exam (CaseAlternative (PLit l) e:rs) = do let examE = C.EVar exam
                                              let checkE = C.EApp (C.EApp (C.EVar "==") (C.ELit l)) examE
                                              passE  <- trE e
                                              failE <- trA exam rs
                                              return $ C.EIf checkE passE failE
trA exam (CaseAlternative (PCon con ps) e:rs) = do let vs = unPVar <$> ps
                                                   let arity = length vs
                                                   let examE = C.EVar exam
                                                   let bindings = mkBinding examE con <$> (vs `zip` [0..arity])
                                                   e' <- trE e
                                                   let passE = if null bindings
                                                                  then e'
                                                                  else C.ELet bindings e'
                                                   let checkE = ETest con examE
                                                   failE <- trA exam rs
                                                   return $ C.EIf checkE passE failE

trP :: [(Pattern, CoreExpr)] -> M [CoreCombinator]
trP []                  = return []
trP ((PWildcard, e):ps) = trP ps
trP ((PVar p, e):ps)    = (:) (CoreCombinator p [] e) <$> trP ps
trP ((PCon c ps, e):rs) = do let vs = unPVar <$> ps
                             let arity = length ps
                             vs <- replicateM arity acquireId
                             let picks = flip (EPick c) e  <$> [0 .. arity]
                             let mkB (v, p) = CoreCombinator v [] p
                             let bindings = mkB <$> (vs `zip` picks)
                             let more = ps `zip` picks
                             rs' <- trP $ more <> rs
                             return $ bindings <> rs'

trB :: [LetBinding] -> M [CoreCombinator]
trB []                                         = return []
trB (LetBinding (CombinatorBinding n ps) e:bs) = do b   <- CoreCombinator n (unPVar <$> ps) <$> trE e
                                                    bs' <- trB bs
                                                    return $ b:bs'
trB (LetBinding (PatternBinding p) e:bs) = do e' <- trE e
                                              bs' <- trB bs
                                              pbs <- trP [(p, e')]
                                              return $ pbs <> bs'

trE :: Expression -> M CoreExpr
trE (S.ECase exam alts) = trA (unEVar exam) alts
trE (S.ELet  []   f   ) = trE f
trE (S.ELet  bs   e   ) = C.ELet <$> trB bs <*> trE e
trE (S.EIf c t e      ) = C.EIf <$> trE c <*> trE t <*> trE e
trE (S.EApp l  r      ) = C.EApp <$> trE l <*> trE r
trE (S.ELam ps e      ) = C.ELam (unPVar <$> ps) <$> trE e
trE (S.EVar v         ) = return $ C.EVar v
trE (S.ELit l         ) = return $ C.ELit l
trE e                   = error "trE : Unexpected expression"

trC :: CombinatorDef -> M CoreCombinator
trC (CombinatorDef f ps e) = CoreCombinator f (unPVar <$> ps) <$> trE e

