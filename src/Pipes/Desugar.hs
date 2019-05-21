module Pipes.Desugar
  ( desugar
  ) where

import           Lang.Surface
import           Misc
import           Pipe

desugarE :: Expression -> Expression
desugarE = r
  where
    r (ELam ps body        ) = ELam ps $ r body
    r (EIf c t e           ) = EIf (r c) (r t) (r e)
    r (EApp f e            ) = EApp (r f) (r e)
    r (ECase e  alts       ) = ECase (r e) $ mapE r <$> alts
    r (ELet  bs body       ) = ELet (mapE r <$> bs) $ r body
    r (EListLiteral  ts    ) = desugarL $ r <$> ts
    r (ETupleLiteral ts    ) = desugarT $ r <$> ts
    r (EUnary name e       ) = EApp (EVar name) $ r e
    r (EBinary name lhs rhs) = EApp (EApp (EVar name) (r lhs)) (r rhs)
    r v                      = v

desugarT :: [Expression] -> Expression
desugarT es = foldl EApp (EVar $ tupleCon $ length es) es

desugarL :: [Expression] -> Expression
desugarL = foldr cons $ EVar "[]"
  where cons a b = EApp (EApp (EVar "::") a) b

desugar :: HasExpression b => Pipe ErrorMessage b b
desugar = Right . mapE desugarE


