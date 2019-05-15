module Lang.Surface.Desugar
  ( desugar
  , desugarE
  ) where

import           Lang.Surface
import           Misc                           ( tupleCon )

desugarS :: [DoStmt] -> Expression
desugarS [DoBind Nothing exp]         = desugarE exp
desugarS (DoBind Nothing exp:rest)    = EBinary ">>" exp (desugarS rest)
desugarS (DoBind (Just pat) exp:rest) = EBinary ">>=" exp $ ELam [pat] $ desugarS rest
desugarS (DoLetBinding frm exp:rest)  = ELet [LetBinding frm exp] $ desugarS rest
desugarS _                            = error "empty do"

desugarE :: Expression -> Expression
desugarE = r
  where
    r (ELam ps body        ) = ELam ps $ r body
    r (EIf c t e           ) = EIf (r c) (r t) (r e)
    r (ECase e  alts       ) = ECase (r e) $ mapE r <$> alts
    r (ELet  bs body       ) = ELet (mapE r <$> bs) $ r body
    r (EListLiteral  ts    ) = desugarL $ r <$> ts
    r (ETupleLiteral ts    ) = desugarT $ r <$> ts
    r (EUnary name e       ) = EApp (EVar name) $ r e
    r (EBinary name lhs rhs) = EApp (EApp (EVar name) (r lhs)) (r rhs)
    r (EDo stmts           ) = r $ desugarS stmts
    r v                      = v

desugarT :: [Expression] -> Expression
desugarT es = foldl EApp (EVar $ tupleCon $ length es) es

desugarL :: [Expression] -> Expression
desugarL = foldr cons $ EVar "[]"
  where cons a b = EApp (EApp (EVar "::") a) b

desugar :: HasExpression b => b -> b
desugar = mapE desugarE
