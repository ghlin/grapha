module Lang.Surface.Subst
  ( fvs
  , fvsP
  , fvsA
  , fvsB
  , fvsBF
  , subst
  ) where

import           Data.List                      ( union
                                                , intersect
                                                , nub
                                                , (\\)
                                                )
import           Misc
import           Lang.Surface

fvs :: Expression -> [Name]
fvs (ELit _)       = []
fvs (EVar v)       = [v]
fvs (EApp l r)     = fvs l `union` fvs r
fvs (EIf c t e)    = fvs c `union` fvs t `union` fvs e
fvs (ELam pts e)   = fvs e \\ mconcat (fvsP <$> pts)
fvs (ECase e alts) = fvs e `union` mconcat (fvsA <$> alts)
fvs (ELet bs e)    = (mconcat (fvsB <$> bs) `union` fvs e) \\ fvsBFs bs
fvs e              = error $ "Unexpected exp type: " <> show e

fvsP :: Pattern -> [Name]
fvsP (PCon _ ps) = mconcat $ fvsP <$> ps
fvsP (PVar v)    = [v]
fvsP _           = []

fvsA :: CaseAlternative -> [Name]
fvsA (CaseAlternative p e) = fvs e \\ fvsP p

fvsBFs :: [LetBinding] -> [Name]
fvsBFs = mconcat . fmap fvsBF

fvsBF :: LetBinding -> [Name]
fvsBF (LetBinding f _) = fvsBF' f

fvsBF' :: LetBindingForm -> [Name]
fvsBF' (PatternBinding p)      = fvsP p
fvsBF' (CombinatorBinding n _) = [n]

fvsB :: LetBinding -> [Name]
fvsB (LetBinding (PatternBinding _) e)       = fvs e
fvsB (LetBinding (CombinatorBinding n ps) e) = fvs e \\ multi fvsP ps

multi :: (a -> [b]) -> [a] -> [b]
multi f = mconcat . fmap f

subst :: Name -> Name -> Expression -> Expression
subst n t (EVar v)    | n == v                    = EVar t
subst n t (ELam ps e) | n `notElem` multi fvsP ps = ELam ps $ subst n t e
subst n t (EApp l r)                              = EApp (subst n t l) (subst n t r)
subst n t (ELet bs e) | n `notElem` fvsBFs bs     = ELet (substBs n t bs) (subst n t e)
subst n t (ECase e alts)                          = ECase (subst n t e) (substA n t <$> alts)
subst n t (EIf c h e   )                          = EIf (subst n t c) (subst n t h) (subst n t e)
subst _ _ e                                       = e

substA :: Name -> Name -> CaseAlternative -> CaseAlternative
substA n t (CaseAlternative p e) | n `notElem` fvsP p = CaseAlternative p $ subst n t e
substA _ _ e                                          = e

substBs :: Name -> Name -> [LetBinding] -> [LetBinding]
substBs n t bs | n `notElem` fvsBFs bs = substB n t <$> bs
substBs _ _ bs                         = bs

substB :: Name -> Name -> LetBinding -> LetBinding
substB n t (LetBinding (PatternBinding p) e) = LetBinding (PatternBinding p) $ subst n t e
substB n t (LetBinding (CombinatorBinding c ps) e) | n `notElem` multi fvsP ps =
  LetBinding (CombinatorBinding c ps) $ subst n t e
substB _ _ b = b

