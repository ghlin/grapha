module Pipes.Desugar
  ( desugar
  ) where

import           Lang.Surface
import           Lang.Literal
import           Misc
import           Pipe

desugarC :: CombinatorDef -> CombinatorDef
desugarC (CombinatorDef name ps body) = CombinatorDef name (desugarP <$> ps) (desugarE body)

desugarB :: LetBinding -> LetBinding
desugarB (LetBinding f e) = LetBinding (desugarF f) (desugarE e)

desugarF :: LetBindingForm -> LetBindingForm
desugarF (CombinatorBinding name ps) = CombinatorBinding name $ desugarP <$> ps
desugarF (PatternBinding p) = PatternBinding $ desugarP p

desugarE :: Expression -> Expression
desugarE = r
  where
    r (ELit (LString s)    ) = foldr cons (EVar "[]") $ ELit . LChar <$> s
    r (ELam ps body        ) = ELam ps $ r body
    r (EIf c t e           ) = EIf (r c) (r t) (r e)
    r (EApp f e            ) = EApp (r f) (r e)
    r (ECase e  alts       ) = ECase (r e) $ desugarA <$> alts
    r (ELet  bs body       ) = ELet (desugarB <$> bs) $ r body
    r (EListLiteral  ts    ) = desugarL $ r <$> ts
    r (ETupleLiteral ts    ) = desugarT $ r <$> ts
    r (EUnary name e       ) = EApp (EVar name) $ r e
    r (EBinary name lhs rhs) = EApp (EApp (EVar name) (r lhs)) (r rhs)
    r v                      = v

desugarA :: CaseAlternative -> CaseAlternative
desugarA (CaseAlternative p e) = CaseAlternative (desugarP p) (desugarE e)

desugarP :: Pattern -> Pattern
desugarP (PCon "[]" xs) = foldr consP (PCon "[]" []) $ desugarP <$> xs
desugarP (PCon p ps)    = PCon p $ desugarP <$> ps
desugarP p              = p

cons :: Expression -> Expression -> Expression
cons a b = EApp (EApp (EVar "::") a) b

consP :: Pattern -> Pattern -> Pattern
consP x y = PCon "::" [x, y]

desugarT :: [Expression] -> Expression
desugarT es = foldl EApp (EVar $ tupleCon $ length es) es

desugarL :: [Expression] -> Expression
desugarL = foldr cons $ EVar "[]"

desugar :: Pipe ErrorMessage Program Program
desugar prog = return $ prog { combinatorDefs = desugarC <$> combinatorDefs prog }


